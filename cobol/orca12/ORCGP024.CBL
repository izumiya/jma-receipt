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
       PROGRAM-ID.             ORCGP024.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 所得者情報（Ｐ０２４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/05/10  日付変換後ゼロ表示変更
      *  01.00.02    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  01.00.04    MCC-太田     01/05/10  日付変換後ゼロ表示変更
      *  01.00.05    MCC-多々納   01/07/02  日付入力チェックサブ追加
      *  02.00.00    NACL-多々納  02/07/30  インターフェース変更
      *  02.00.01    NACL-多々納  03/02/13  食事標準負担減額日追加
      *  02.00.02    NACL-多々納  03/03/24  公費負担額追加
      *  02.00.03    NACL-多々納  03/10/14  公費負担額初期編集追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  PAR-KEY
      *                            FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PAR-KEY.
      *        05  PAR-FILEMEI         PIC X(08).
      *        05  PAR-EDANUM          PIC 9(04).
      *    03  PAR-DATA-REC            PIC X(3000).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "P01PARA".
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA            PIC 9(01).
           03  FLG-END             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-IN-TNKSELNUM    PIC 9(01).
           03  FLG-IN-TSYSELNUM    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
     *
           03  IDP-1               PIC 9(04).
           03  IDP-2               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-RENNUMZ         PIC ZZZZ.
           03  WRK-NAIBU-IDX-AREA.
               05  WRK-G-IDX       PIC 9(04).
               05  WRK-N-IDX       PIC 9(04).
      *        1:所得テーブル /2:年金テーブル
               05  WRK-W-TBL       PIC 9(01).
           03  WRK-TSY-SMAX        PIC 9(04).
           03  WRK-TNK-SMAX        PIC 9(04).
           03  WRK-TSY-MAX         PIC 9(04).
           03  WRK-TNK-MAX         PIC 9(04).
      *----(01.00.01) LINE ADD START --------------------------------
           03  WRK-WWYMD           PIC X(10).
      *----(01.00.01) LINE ADD END   --------------------------------
           03  WRK-SELNUM          PIC 9(04).
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-Z7                  PIC Z,ZZZ,ZZZ.
      *
           03  WRK-IDX                 PIC 9(04).
           03  WRK-KOHID               PIC 9(10).
           03  WRK-PARA-EDANUM         PIC 9(03).
           03  WRK-FTN1-IDX            PIC 9(04).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
       01  HZN-PARA-REC.
           COPY    "CPPARA.INC"
                              REPLACING  //PARA-// BY //HZN-PARA-//.
      *
      *   低所得者履歴
      *
       01  LNK-TSY-AREA.
           02  LNK-TSY-REC           OCCURS  20.
           COPY    "CPTSYRRK.INC"
                            REPLACING  //TSYRRK-// BY //LNK-TSY-//.
      *
      *   年金履歴
      *
       01  LNK-TNK-AREA.
           02  LNK-TNK-REC           OCCURS  10.
           COPY    "CPTNKRRK.INC"
                              REPLACING  //TNKRRK-// BY //LNK-TNK-//.
      *
      *    公費負担保存
       01  LNK-PTKOHFTN-AREA.
           02  LNK-PTKOHFTN-REC           OCCURS  50.
           COPY    "CPPTKOHFTN.INC"
                       REPLACING  //PTKOHFTN-// BY //LNK-PTKOHFTN-//.
      *    公費負担
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *    公費負担更新保存
       01  HZN-PTKOHFTN-AREA.
           03  HZN-PTKOHFTN-OCC           OCCURS  10.
               05  HZN-PTKOHFTN-OCC2          OCCURS  50.
                   07  HZN-PTKOHFTN-REC       PIC X(200).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   数字変換領域
           COPY    "CPORCSNUM.INC".
      *----(01.00.04) LINE ADD START ----------------------------------
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *----(01.00.04) LINE ADD END   ----------------------------------
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
      *
       01  SCRAREA.
           COPY    "P02.INC".
           COPY    "P02A.INC".
      *****COPY    "P02B.INC".
      *****COPY    "P02C.INC".
           COPY    "P02D.INC".
           COPY    "P02E.INC".
           COPY    "P02F.INC".
           COPY    "P014.INC".
           COPY    "P98.INC".
           COPY    "P981.INC".
           COPY    "P99.INC".
           COPY    "P0201.INC".
           COPY    "P982.INC".
           COPY    "P100.INC".
           COPY    "P02X.INC".
           COPY    "PERR.INC".
           COPY    "PID1.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
           IF      SPA-FLG-END         =   ZERO
               PERFORM 5001-MAPCUR-SEC
      *********PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
      *
           .
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
      *
      *    初期画面編集
           PERFORM 300-SCREEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    パラメタファイル検索処理
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
      *    初期画面編集
           PERFORM 310-SPASET-SEC
      *
           MOVE    1       TO  SPA-GMN24-CUR
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル検索処理
      *****************************************************************
       320-PARAFILE-KENSAKU-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-TNK-MAX
           MOVE    ZERO                TO  SPA-TSY-MAX
           INITIALIZE                  LNK-TSY-AREA
           INITIALIZE                  LNK-TNK-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TNKRRK"            TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
               IF      PARA-EDANUM         <=  10
                   MOVE    PARA-DATA-REC   TO  LNK-TNK-REC
                                                         (PARA-EDANUM)
                   MOVE    PARA-EDANUM     TO  SPA-TNK-MAX
               END-IF
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TSYRRK"            TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
               IF      PARA-EDANUM         <=  20
                   MOVE    PARA-DATA-REC   TO  LNK-TSY-REC
                                                         (PARA-EDANUM)
                   MOVE    PARA-EDANUM     TO  SPA-TSY-MAX
               END-IF
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           .
       320-PARAFILE-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    低所得者編集
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   20 )
                   OR        ( IDX     >   SPA-TSY-MAX )
      *
               MOVE    LNK-TSY-NINSTYMD (IDX)
                                         TO  SPA-GMN24-TSYDPSTYMD (IDX)
               MOVE    LNK-TSY-NINSTYMD (IDX)  TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD        TO  SPA-GMN24-TSYDPSTYMDW(IDX)
      *
               MOVE    LNK-TSY-NINEDYMD (IDX)
                                         TO  SPA-GMN24-TSYDPEDYMD (IDX)
               MOVE    LNK-TSY-NINEDYMD (IDX)  TO  WRK-SYMD
               IF  WRK-SYMD              =   ALL "9"
                   MOVE    ALL "9"  TO  SPA-GMN24-TSYDPEDYMDW(IDX)(1:7)
               ELSE
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD    TO  SPA-GMN24-TSYDPEDYMDW(IDX)
               END-IF
      *
               MOVE    LNK-TSY-SKJGNSTYMD (IDX)
                                         TO  SPA-GMN24-TSYLSKJGNSTYMD
                                                              (IDX)
               MOVE    LNK-TSY-SKJGNSTYMD (IDX)  TO  WRK-SYMD
               IF      WRK-SYMD            =   SPACE
                   MOVE    SPACE       TO  SPA-GMN24-TSYLSKJGNSTYMDW
                                                           (IDX)(1:7)
               ELSE
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD    TO  SPA-GMN24-TSYLSKJGNSTYMDW
                                                                (IDX)
               END-IF
           END-PERFORM
      *
      *    年金編集
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   10 )
                   OR        ( IDX     >   SPA-TNK-MAX )
      *
               MOVE    LNK-TNK-STYMD (IDX)
                                       TO  SPA-GMN24-TNKDPSTYMD (IDX)
               MOVE    LNK-TNK-STYMD (IDX) TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD      TO  SPA-GMN24-TNKDPSTYMDW(IDX)
      *
               MOVE    LNK-TNK-EDYMD (IDX)
                                       TO  SPA-GMN24-TNKDPEDYMD (IDX)
               MOVE    LNK-TNK-EDYMD (IDX) TO  WRK-SYMD
               IF  WRK-SYMD            =    ALL "9"
                   MOVE    ALL "9" TO  SPA-GMN24-TNKDPEDYMDW(IDX)(1:7)
               ELSE
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD  TO  SPA-GMN24-TNKDPEDYMDW(IDX)
               END-IF
           END-PERFORM
      *
      *TEST
      *    前回の選択番号は反映させない
           MOVE    ZERO                TO  SPA-MAE24-FTNSELNUM
           INITIALIZE                  SPA-GMN24-FTN-AREA
           INITIALIZE                  SPA-GMN24-FTN-AREA2
      *    公費負担の最初を選択とする
           IF      (SPA-GMN24-FTNSELNUM    =   ZERO )  AND
                   (SPA-MAE24-FTNSELNUM    =   ZERO )  AND
                   (SPA-FTN1-MAX           >   ZERO )
               PERFORM 3102-KOHFTN-HEN-SEC
           END-IF
           IF       SPA-GMN24-FTNSELNUM    =   ZERO
               INITIALIZE                  SPA-GMN24-KOHFTNTBL2
           END-IF
      *TEST
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担初期編集処理
      *****************************************************************
       3102-KOHFTN-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-FTN1-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      IDX     >   SPA-FTN1-MAX
               IF      SPA-NAI24-FTNDELKBN (IDX)   =   SPACE
                   MOVE    IDX             TO  WRK-FTN1-IDX
                   MOVE    IDX             TO  SPA-NAI24-FTNSELNUM
                   MOVE    SPA-FTN1-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-FTN1-IDX        =   ZERO
               GO      TO                  3102-KOHFTN-HEN-EXT
           END-IF
           MOVE    1                   TO  SPA-GMN24-FTNSELNUM
           MOVE    SPA-GMN24-FTNSELNUM TO  SPA-MAE24-FTNSELNUM
      *
      *    対象のデータをパラメタファイルより編集する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-PARA-EDANUM
           MOVE    SPACE               TO  LNK-PTKOHFTN-AREA
           INITIALIZE                  SPA-GMN24-KOHFTNTBL2
           PERFORM
               UNTIL   FLG-PARA            =   1
               MOVE    PARA-DATA-REC           TO  PTKOHFTN-REC
      *        公費ＩＤの同じものを対象とする
               IF      PTKOHFTN-KOHID      =   SPA-NAI24-KOHID
                                                        (WRK-FTN1-IDX)
                   ADD     1                   TO  IDX
                   IF      IDX                 <=  50
                       MOVE    PARA-DATA-REC   TO  LNK-PTKOHFTN-REC
                                                             (IDX)
                       MOVE    IDX             TO  SPA-FTN2-MAX
                   END-IF
               END-IF
      *
               MOVE    PARA-EDANUM         TO  WRK-PARA-EDANUM
      *
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *    画面テーブルへ編集する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN2-MAX
               MOVE    LNK-PTKOHFTN-REC (IDX)  TO  PTKOHFTN-REC
      *
               MOVE    PTKOHFTN-RENNUM     TO  SPA-NAI24-TRENNUM  (IDX)
               MOVE    PTKOHFTN-TEKSTYMD   TO  SPA-NAI24-TFTNSTYMD(IDX)
               MOVE    PTKOHFTN-TEKEDYMD   TO  SPA-NAI24-TFTNEDYMD (IDX)
               MOVE    PTKOHFTN-GAIJGNGAK  TO  SPA-GMN24-TGAIJGNGAK(IDX)
               MOVE    PTKOHFTN-NYUJGNGAK  TO  SPA-GMN24-TNYUJGNGAK(IDX)
      *
               MOVE    PTKOHFTN-TEKSTYMD   TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNSTYMD(IDX)
               MOVE    PTKOHFTN-TEKEDYMD   TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNEDYMD(IDX)
      *
           END-PERFORM
      *
           IF      SPA-FTN2-MAX        =   ZERO
               MOVE    ZERO                TO  SPA-GMN24-FTNSELNUM
               MOVE    ZERO                TO  SPA-MAE24-FTNSELNUM
           END-IF
      *
           .
       3102-KOHFTN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN24-CUR       =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN24-CUR
               WHEN    01
                   MOVE    "P024_TSRSELNUM"     TO  MCP-WIDGET
               WHEN    02
                   MOVE    "P024_TSYSTYMD"      TO  MCP-WIDGET
               WHEN    03
                   MOVE    "P024_TSYEDYMD"      TO  MCP-WIDGET
               WHEN   12
                   MOVE    "P024_TSYSKJGNSTYMD"      TO  MCP-WIDGET
               WHEN    04
                   MOVE    "BT1"                TO  MCP-WIDGET
               WHEN    05
                   MOVE    "BT2"                TO  MCP-WIDGET
               WHEN    06
                   MOVE    "P024_TNKSELNUM"     TO  MCP-WIDGET
               WHEN    07
                   MOVE    "P024_TNKSTYMD"      TO  MCP-WIDGET
               WHEN    08
                   MOVE    "P024_TNKEDYMD"      TO  MCP-WIDGET
               WHEN    09
                   MOVE    "BN1"                TO  MCP-WIDGET
               WHEN    10
                   MOVE    "BN2"                TO  MCP-WIDGET
      *        公費負担
               WHEN    30
                   MOVE    "P024_FTNSELNUM"     TO  MCP-WIDGET
               WHEN    31
                   MOVE    "P024_FTNSELNUM2"    TO  MCP-WIDGET
               WHEN    32
                   MOVE    "P024_FTNTEKSTYMD"   TO  MCP-WIDGET
               WHEN    33
                   MOVE    "P024_FTNTEKEDYMD"   TO  MCP-WIDGET
               WHEN    34
                   MOVE    "P024_GAIJGNGAK"     TO  MCP-WIDGET
               WHEN    35
                   MOVE    "P024_NYUJGNGAK"     TO  MCP-WIDGET
               WHEN    36
                   MOVE    "BF1"                TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "P024_TSYLIST"
                   MOVE    2           TO  SPA-GMN24-CUR
               WHEN    "P024_TSRSELNUM"
                   MOVE    2           TO  SPA-GMN24-CUR
               WHEN    "P024_TSYSTYMD"
                   MOVE    3           TO  SPA-GMN24-CUR
               WHEN    "P024_TSYEDYMD"
                   MOVE    12          TO  SPA-GMN24-CUR
               WHEN    "P024_TSYSKJGNSTYMD"
                   MOVE    4            TO  SPA-GMN24-CUR
      *
               WHEN    "P024_TNKLIST"
                   MOVE    7           TO  SPA-GMN24-CUR
               WHEN    "P024_TNKSELNUM"
                   MOVE    7           TO  SPA-GMN24-CUR
               WHEN    "P024_TNKSTYMD"
                   MOVE    8           TO  SPA-GMN24-CUR
               WHEN    "P024_TNKEDYMD"
                   MOVE    9           TO  SPA-GMN24-CUR
      *        公費負担
               WHEN    "P024_FTNSELNUM"
                   MOVE    31          TO  SPA-GMN24-CUR
               WHEN    "P024_FTNSELNUM2"
                   MOVE    32          TO  SPA-GMN24-CUR
               WHEN    "P024_FTNTEKSTYMD"
                   MOVE    33          TO  SPA-GMN24-CUR
               WHEN    "P024_FTNTEKEDYMD"
                   MOVE    34          TO  SPA-GMN24-CUR
               WHEN    "P024_GAIJGNGAK"
                   MOVE    35          TO  SPA-GMN24-CUR
               WHEN    "P024_NYUJGNGAK"
                   MOVE    36          TO  SPA-GMN24-CUR
      *
           END-EVALUATE
      *
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    登録処理
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    低所得更新
               WHEN    "CLICKED"       ALSO    "BT1"
                   PERFORM 411-TSKRRK-KOUSIN-SEC
      *    低所得削除
               WHEN    "CLICKED"       ALSO    "BT2"
                   PERFORM 412-TSKRREK-DELETE-SEC
      *    年金更新
               WHEN    "CLICKED"       ALSO    "BN1"
                   PERFORM 413-TNKRRK-KOUSIN-SEC
      *    年金削除
               WHEN    "CLICKED"       ALSO    "BN2"
                   PERFORM 414-TNKRRK-DELETE-SEC
      *    公費負担登録
               WHEN    "CLICKED"       ALSO    "BF1"
                   PERFORM 415-KOHFTN-KOUSIN-SEC
      *    公費負担削除
               WHEN    "CLICKED"       ALSO    "BF2"
                   PERFORM 416-KOHFTN-DELETE-SEC
      *
      *     以外はエラー
               WHEN    "CLICKED"       ALSO     ANY
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN24-CUR
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／更新
      *****************************************************************
       411-TSKRRK-KOUSIN-SEC           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF  SPA-ERRCD           NOT =   SPACE
               GO      TO              411-TSKRRK-KOUSIN-EXT
           END-IF
      *
      *
           IF      SPA-GMN24-TSYSELNUM =   ZERO  OR  SPACE
               COMPUTE SPA-NAI24-TSYSELNUM =   SPA-TSY-MAX   +   1
               ADD     1               TO  SPA-TSY-MAX
               IF      SPA-TSY-MAX         >   20
                   MOVE   "0008"       TO  SPA-ERRCD
      *----(01.00.01) LINE ADD START --------------------------------
                   MOVE    2           TO  SPA-GMN24-CUR
      *----(01.00.01) LINE ADD END   --------------------------------
                   COMPUTE SPA-TSY-MAX =   SPA-TSY-MAX   -   1
                   GO     TO           411-TSKRRK-KOUSIN-EXT
               END-IF
               MOVE    SPA-TSY-MAX     TO  WRK-N-IDX
           END-IF
           MOVE    WRK-N-IDX           TO  IDX
           MOVE    SPA-GMN24-TSYSTYMDW TO  SPA-GMN24-TSYDPSTYMDW(IDX)
           MOVE    SPA-GMN24-TSYEDYMDW TO  SPA-GMN24-TSYDPEDYMDW(IDX)
           MOVE    SPA-GMN24-TSYSTYMD  TO  SPA-GMN24-TSYDPSTYMD (IDX)
           MOVE    SPA-GMN24-TSYEDYMD  TO  SPA-GMN24-TSYDPEDYMD (IDX)
           MOVE    SPA-GMN24-TSYSKJGNSTYMDW
                                       TO  SPA-GMN24-TSYLSKJGNSTYMDW
                                                                (IDX)
           MOVE    SPA-GMN24-TSYSKJGNSTYMD
                                       TO  SPA-GMN24-TSYLSKJGNSTYMD
                                                                (IDX)
      *
           INITIALIZE                  SPA-GMN24-TSYS-AREA
      *
           MOVE    2                   TO  SPA-GMN24-CUR
      *
           .
       411-TSKRRK-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／削除
      *****************************************************************
       412-TSKRREK-DELETE-SEC          SECTION.
      *
           IF   SPA-NAI24-TSYSELNUM    =   ZERO  OR  SPACE
                GO      TO             412-TSKRREK-DELETE-EXT
           END-IF
      *
           INITIALIZE                  WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TSYSELNUM TO  WRK-G-IDX
           MOVE    1                   TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           IF  WRK-N-IDX               =   ZERO
               MOVE    "2401"          TO  SPA-ERRCD
      *----(01.00.01) LINE ADD START --------------------------------
               MOVE    1               TO  SPA-GMN24-CUR
      *----(01.00.01) LINE ADD END   --------------------------------
               GO      TO              412-TSKRREK-DELETE-EXT
           END-IF
           MOVE    "1"                 TO  SPA-GMN24-TSYDELKBN
                                                     (WRK-N-IDX)
      *
           INITIALIZE                  SPA-GMN24-TSYS-AREA
      *
           MOVE    2                   TO  SPA-GMN24-CUR
      *
           .
       412-TSKRREK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／更新
      *****************************************************************
       413-TNKRRK-KOUSIN-SEC           SECTION.
      *
           MOVE    2                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF  SPA-ERRCD           NOT =   SPACE
               GO      TO              413-TNKRRK-KOUSIN-EXT
           END-IF
      *
      *
           IF      SPA-GMN24-TNKSELNUM =   ZERO  OR  SPACE
               COMPUTE SPA-NAI24-TNKSELNUM =   SPA-TNK-MAX   +   1
               ADD     1               TO  SPA-TNK-MAX
               IF  SPA-TNK-MAX         >   10
                   MOVE   "0008"       TO  SPA-ERRCD
      *----(01.00.01) LINE ADD START --------------------------------
                   MOVE    6           TO  SPA-GMN24-CUR
      *----(01.00.01) LINE ADD END   --------------------------------
                   COMPUTE SPA-TNK-MAX =   SPA-TNK-MAX   -   1
                   GO     TO           413-TNKRRK-KOUSIN-EXT
               END-IF
               MOVE    SPA-TNK-MAX     TO  WRK-N-IDX
           END-IF
           MOVE    WRK-N-IDX           TO  IDX
           MOVE    SPA-GMN24-TNKSTYMDW TO  SPA-GMN24-TNKDPSTYMDW(IDX)
           MOVE    SPA-GMN24-TNKEDYMDW TO  SPA-GMN24-TNKDPEDYMDW(IDX)
           MOVE    SPA-GMN24-TNKSTYMD  TO  SPA-GMN24-TNKDPSTYMD (IDX)
           MOVE    SPA-GMN24-TNKEDYMD  TO  SPA-GMN24-TNKDPEDYMD (IDX)
      *
           INITIALIZE                  SPA-GMN24-TNK-AREA
      *
           MOVE    7                   TO  SPA-GMN24-CUR
           .
       413-TNKRRK-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／削除
      *****************************************************************
       414-TNKRRK-DELETE-SEC           SECTION.
      *
           IF   SPA-NAI24-TNKSELNUM    =   ZERO  OR  SPACE
                GO      TO             414-TNKRRK-DELETE-EXT
           END-IF
      *
           INITIALIZE                  WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TNKSELNUM TO  WRK-G-IDX
           MOVE    2                   TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           IF  WRK-N-IDX               =   ZERO
               MOVE    "2406"          TO  SPA-ERRCD
               MOVE    6               TO  SPA-GMN24-CUR
               GO      TO              414-TNKRRK-DELETE-EXT
           END-IF
           MOVE    "1"                 TO  SPA-GMN24-TNKDELKBN
                                                     (WRK-N-IDX)
      *
           INITIALIZE                  SPA-GMN24-TNK-AREA
      *
           MOVE    7                   TO  SPA-GMN24-CUR
           .
       414-TNKRRK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担更新処理
      *****************************************************************
       415-KOHFTN-KOUSIN-SEC           SECTION.
      *
           MOVE    3                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO              415-KOHFTN-KOUSIN-EXT
           END-IF
      *
           IF      SPA-NAI24-FTNSELNUM2    =   ZERO
      *        追加
               ADD     1                       TO  SPA-FTN2-MAX
               MOVE    SPA-FTN2-MAX            TO  IDX
           ELSE
               MOVE    SPA-NAI24-FTNSELNUM2    TO  IDX
           END-IF
           MOVE    SPA-GMN24-FTNSTYMD  TO  SPA-GMN24-TFTNSTYMD (IDX)
           MOVE    SPA-GMN24-FTNEDYMD  TO  SPA-GMN24-TFTNEDYMD (IDX)
           MOVE    SPA-NAI24-GAIJGNGAK TO  SPA-GMN24-TGAIJGNGAK (IDX)
           MOVE    SPA-NAI24-NYUJGNGAK TO  SPA-GMN24-TNYUJGNGAK (IDX)
           MOVE    SPA-NAI24-FTNSTYMD  TO  SPA-NAI24-TFTNSTYMD (IDX)
           MOVE    SPA-NAI24-FTNEDYMD  TO  SPA-NAI24-TFTNEDYMD (IDX)
      *
      *
           INITIALIZE                  SPA-GMN24-FTN-AREA2
      *
           MOVE    31                  TO  SPA-GMN24-CUR
           .
       415-KOHFTN-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担／削除
      *****************************************************************
       416-KOHFTN-DELETE-SEC          SECTION.
      *
           IF   SPA-NAI24-FTNSELNUM2   =   ZERO
                GO      TO             416-KOHFTN-DELETE-EXT
           END-IF
      *
           MOVE    "1"                 TO  SPA-NAI24-FTNDELKBN2
                                                (SPA-NAI24-FTNSELNUM2)
      *
           INITIALIZE                  SPA-GMN24-FTN-AREA2
      *
           MOVE    31                  TO  SPA-GMN24-CUR
      *
           .
       416-KOHFTN-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      *    所得・年金レコード数を一時退避
           MOVE    SPA-TSY-MAX     TO  WRK-TSY-SMAX
           MOVE    SPA-TNK-MAX     TO  WRK-TNK-SMAX
      *    パラメタファイル再検索処理
           INITIALIZE                  LNK-TNK-AREA
           INITIALIZE                  LNK-TSY-AREA
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *    初期表示時の所得・年金レコード数を保存
           MOVE    SPA-TSY-MAX     TO  WRK-TSY-MAX
           MOVE    SPA-TNK-MAX     TO  WRK-TNK-MAX
           MOVE    WRK-TSY-SMAX    TO  SPA-TSY-MAX
           MOVE    WRK-TNK-SMAX    TO  SPA-TNK-MAX
      *
      *    画面内容をパラメタ出力処理
           PERFORM 500-SPA-PARA-TSYRRK-SEC
           PERFORM 500-SPA-PARA-TNKRRK-SEC
      *    公費負担
           PERFORM 410131-KOHFTN-PARA-SEC
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    低所得者画面の内容をパラメタへ保存
      *****************************************************************
       500-SPA-PARA-TSYRRK-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX         >   20          )
                   OR        ( IDX         >   SPA-TSY-MAX )
      *********MOVE    SPACE               TO  LNK-TSY-KEY-O   (IDX)
      *        初期表示時の所得レコード数以内の場合
               IF      IDX                 <=  WRK-TSY-MAX
      *            削除データの場合
                   IF  SPA-GMN24-TSYDELKBN(IDX)   =   "1"
                       MOVE    ALL "9"     TO  LNK-TSY-UPYMD      (IDX)
                   ELSE
      **************   変更前キー転送
      *                MOVE    LNK-TSY-HOSPID (IDX)
      *                                    TO  LNK-TSY-HOSPID-O   (IDX)
      *                MOVE    LNK-TSY-PTID   (IDX)
      *                                    TO  LNK-TSY-PTID-O     (IDX)
      *                MOVE    LNK-TSY-NINSTYMD (IDX)
      **************                       TO  LNK-TSY-NINSTYMD-O (IDX)
      *                認定期間変更時、変更年月日・更新年月日転送
                       IF  ( SPA-GMN24-TSYDPSTYMD(IDX)
                                   NOT =   LNK-TSY-NINSTYMD(IDX)) OR
                           ( SPA-GMN24-TSYDPEDYMD(IDX)
                                   NOT =   LNK-TSY-NINEDYMD(IDX)) OR
                           ( SPA-GMN24-TSYLSKJGNSTYMD(IDX)
                                   NOT =   LNK-TSY-SKJGNSTYMD(IDX))
                           MOVE    SPA-SYSYMD  TO  LNK-TSY-UPYMD  (IDX)
                           MOVE    SPA-GMN24-TSYDPSTYMD (IDX)
                                           TO  LNK-TSY-NINSTYMD   (IDX)
                           MOVE    SPA-GMN24-TSYDPEDYMD (IDX)
                                           TO  LNK-TSY-NINEDYMD   (IDX)
                           MOVE    SPA-GMN24-TSYLSKJGNSTYMD (IDX)
                                           TO  LNK-TSY-SKJGNSTYMD (IDX)
                       END-IF
                   END-IF
                   MOVE    IDX             TO  IDY
                   PERFORM 510-PARA-TSYRRK-OUT-SEC
               ELSE
      *        初期表示時の所得レコード数を超える場合
                   IF  SPA-GMN24-TSYDELKBN (IDX)   =   SPACE
                       ADD     1               TO  IDY
      *
                       INITIALIZE                  LNK-TSY-REC (IDY)
                       MOVE    SPA-HOSPID      TO  LNK-TSY-HOSPID (IDY)
                       MOVE    SPA-GMN-PTID    TO  LNK-TSY-PTID (IDY)
                       MOVE    SPA-GMN24-TSYDPSTYMD (IDX)
                                           TO  LNK-TSY-NINSTYMD   (IDY)
                       MOVE    SPA-GMN24-TSYDPEDYMD (IDX)
                                           TO  LNK-TSY-NINEDYMD   (IDY)
                       MOVE    SPA-GMN24-TSYLSKJGNSTYMD (IDX)
                                           TO  LNK-TSY-SKJGNSTYMD
                                                                 (IDY)
                       MOVE    SPA-TERMID      TO  LNK-TSY-TERMID (IDY)
                       MOVE    SPA-SYSYMD      TO  LNK-TSY-CREYMD (IDY)
                       PERFORM 510-PARA-TSYRRK-OUT-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       500-SPA-PARA-TSYRRK-EXT.
           EXIT.
           .
      *****************************************************************
      *    低所得者画面の内容をパラメタへ保存
      *****************************************************************
       510-PARA-TSYRRK-OUT-SEC         SECTION.
      *
      *    パラメタ登録
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TSYRRK"            TO  PARA-FILEMEI
           MOVE    IDY                 TO  PARA-EDANUM
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           IF      FLG-PARA            =   1
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "TSYRRK"            TO  PARA-FILEMEI
               MOVE    IDY                 TO  PARA-EDANUM
      *
               MOVE    LNK-TSY-REC(IDY)    TO  PARA-DATA-REC
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           ELSE
               MOVE    LNK-TSY-REC(IDY)    TO  PARA-DATA-REC
      *
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           END-IF
      *
           .
       510-PARA-TSYRRK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金画面の内容をパラメタへ保存
      *****************************************************************
       500-SPA-PARA-TNKRRK-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX         >   10          )
                   OR        ( IDX         >   SPA-TNK-MAX )
      *********MOVE    SPACE               TO  LNK-TNK-KEY-O(IDX)
      *        初期表示時の所得レコード数以内の場合
               IF  IDX                     <=  WRK-TNK-MAX
      *            削除データの場合
                   IF  SPA-GMN24-TNKDELKBN(IDX)   =   "1"
                       MOVE    ALL "9"     TO  LNK-TNK-UPYMD   (IDX)
                   ELSE
      ************     変更前キー転送
      *                MOVE    LNK-TNK-HOSPID (IDX)
      *                                    TO  LNK-TNK-HOSPID-O(IDX)
      *                MOVE    LNK-TNK-PTID (IDX)
      *                                    TO  LNK-TNK-PTID-O  (IDX)
      *                MOVE    LNK-TNK-STYMD (IDX)
      ************                         TO  LNK-TNK-STYMD-O (IDX)
      *                認定期間変更時、変更年月日・更新年月日転送
                       IF  (SPA-GMN24-TNKDPSTYMD(IDX)
                                   NOT =   LNK-TNK-STYMD(IDX))  OR
                           (SPA-GMN24-TNKDPEDYMD(IDX)
                                   NOT =   LNK-TNK-EDYMD(IDX))
                         MOVE    SPA-SYSYMD  TO  LNK-TNK-UPYMD (IDX)
                         MOVE    SPA-GMN24-TNKDPSTYMD (IDX)
                                           TO  LNK-TNK-STYMD   (IDX)
                         MOVE    SPA-GMN24-TNKDPEDYMD (IDX)
                                           TO  LNK-TNK-EDYMD   (IDX)
                       END-IF
                   END-IF
                   MOVE    IDX             TO  IDY
                   PERFORM 510-PARA-TNKRRK-OUT-SEC
               ELSE
      *        初期表示時の所得レコード数を超える場合
                   IF  SPA-GMN24-TNKDELKBN (IDX)   =   SPACE
                       ADD     1               TO  IDY
      *
                       INITIALIZE                  LNK-TNK-REC (IDY)
                       MOVE    SPA-HOSPID      TO  LNK-TNK-HOSPID (IDY)
                       MOVE    SPA-GMN-PTID    TO  LNK-TNK-PTID   (IDY)
                       MOVE    SPA-GMN24-TNKDPSTYMD (IDX)
                                               TO  LNK-TNK-STYMD  (IDY)
                       MOVE    SPA-GMN24-TNKDPEDYMD (IDX)
                                               TO  LNK-TNK-EDYMD  (IDY)
                       MOVE    SPA-TERMID      TO  LNK-TNK-TERMID (IDY)
                       MOVE    SPA-SYSYMD      TO  LNK-TNK-CREYMD (IDY)
                       PERFORM 510-PARA-TNKRRK-OUT-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       500-SPA-PARA-TNKRRK-EXT.
           EXIT.
           .
      *****************************************************************
      *    低所得者画面の内容をパラメタへ保存
      *****************************************************************
       510-PARA-TNKRRK-OUT-SEC         SECTION.
      *
      *    パラメタ登録
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TNKRRK"            TO  PARA-FILEMEI
           MOVE    IDY                 TO  PARA-EDANUM
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           IF      FLG-PARA            =   1
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "TNKRRK"            TO  PARA-FILEMEI
               MOVE    IDY                 TO  PARA-EDANUM
      *
               MOVE    LNK-TNK-REC(IDY)    TO  PARA-DATA-REC
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           ELSE
               MOVE    LNK-TNK-REC(IDY)    TO  PARA-DATA-REC
      *
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           END-IF
      *
      *
           .
       510-PARA-TNKRRK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    低所得　選択番号選択
               WHEN    ANY             ALSO    "P024_TSYLIST"
                   PERFORM 41012-TSYLIST-SEC
      *    低年金　選択番号選択
               WHEN    ANY             ALSO    "P024_TNKLIST"
                   PERFORM 41012-TNKLIST-SEC
      *    公費負担額番号選択
               WHEN    "ACTIVATE"      ALSO    "P024_FTNSELNUM"
                   PERFORM 41013-FTNSELNUM-SEC
                   GO      TO      200-ENTRY-EXT
      *    公費負担額番号選択
               WHEN    "ACTIVATE"      ALSO    "P024_FTNSELNUM2"
                   PERFORM 41014-FTNSELNUM2-SEC
                   GO      TO      200-ENTRY-EXT
      *    公費負担額番号選択
               WHEN    ANY             ALSO    "P024_PTKOHFTN"
                   PERFORM 41015-KOHFTNLIST-SEC
                   GO      TO      200-ENTRY-EXT
      *    公費負担額番号選択
               WHEN    ANY             ALSO    "P024_PTKOHFTN2"
                   PERFORM 41016-KOHFTNLIST2-SEC
                   GO      TO      200-ENTRY-EXT
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN24-CUR
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    低所得者選択
      *****************************************************************
       41012-TSYLIST-SEC           SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX       >   SPA-TSY-MAX
               IF      P02-P024-TSYLIST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-NAI24-TSYSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI24-TSYSELNUM
           END-IF
      *
           IF      SPA-NAI24-TSYSELNUM NOT =   ZERO
               MOVE     SPA-NAI24-TSYSELNUM    TO   WRK-RENNUMZ
               MOVE     WRK-RENNUMZ        TO   P02-P024-TSRSELNUM
               MOVE    2                   TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41012-TSYLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／選択番号選択
      *****************************************************************
       41012-TNKLIST-SEC           SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX       >   SPA-TNK-MAX
               IF      P02-P024-TNKLIST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-NAI24-TNKSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI24-TNKSELNUM
           END-IF
      *
           IF      SPA-NAI24-TNKSELNUM   NOT =   ZERO
               MOVE     SPA-NAI24-TNKSELNUM    TO   WRK-RENNUMZ
               MOVE     WRK-RENNUMZ        TO  P02-P024-TNKSELNUM
               MOVE    7                   TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41012-TNKLIST-EXT.
           EXIT.
      *****************************************************************
      *    公費負担／公費選択番号選択
      *****************************************************************
       41015-KOHFTNLIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX       >   SPA-FTN1-MAX
               IF      P02-P024-HFTNLST-SELECT (IDX)  =   "T"
                   IF      IDX           =   SPA-GMN24-FTNSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN24-FTNSELNUM
           END-IF
      *
           IF      SPA-GMN24-FTNSELNUM     NOT =   ZERO
               MOVE    SPA-GMN24-FTNSELNUM     TO  P02-P024-FTNSELNUM
               PERFORM 41013-FTNSELNUM-SEC
           END-IF
      *
           .
       41015-KOHFTNLIST-EXT.
           EXIT.
      *****************************************************************
      *    公費負担／公費選択番号選択
      *****************************************************************
       41013-FTNSELNUM-SEC           SECTION.
      *
           MOVE    P02-P024-FTNSELNUM  TO  SPA-GMN24-FTNSELNUM
      *    変更がある時、現在の更新分をパラメタへ
           IF      SPA-GMN24-FTNSELNUM NOT =   SPA-MAE24-FTNSELNUM
               PERFORM 410131-KOHFTN-PARA-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      41013-FTNSELNUM-EXT
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN24-KOHFTNTBL2
           INITIALIZE                  SPA-GMN24-FTN-AREA2
      *
           MOVE    ZERO                TO  WRK-FTN1-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN1-MAX
               IF      SPA-GMN24-FTNSELNUM =   SPA-GMN24-TGMNNUM (IDX)
                   MOVE    IDX             TO  WRK-FTN1-IDX
                   MOVE    IDX             TO  SPA-NAI24-FTNSELNUM
                   MOVE    SPA-FTN1-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-FTN1-IDX        =   ZERO
               MOVE    "2430"              TO  SPA-ERRCD
               MOVE    30                  TO  SPA-GMN24-CUR
               GO      TO                  41013-FTNSELNUM-EXT
           END-IF
           MOVE    SPA-GMN24-FTNSELNUM TO  SPA-MAE24-FTNSELNUM
      *
      *    対象のデータをパラメタファイルより編集する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-PARA-EDANUM
           MOVE    SPACE               TO  LNK-PTKOHFTN-AREA
           PERFORM
               UNTIL   FLG-PARA            =   1
               MOVE    PARA-DATA-REC           TO  PTKOHFTN-REC
      *        公費ＩＤの同じものを対象とする
               IF      PTKOHFTN-KOHID      =   SPA-NAI24-KOHID
                                                        (WRK-FTN1-IDX)
                   ADD     1                   TO  IDX
                   IF      IDX                 <=  50
                       MOVE    PARA-DATA-REC   TO  LNK-PTKOHFTN-REC
                                                             (IDX)
                       MOVE    IDX             TO  SPA-FTN2-MAX
                   END-IF
               END-IF
      *
               MOVE    PARA-EDANUM         TO  WRK-PARA-EDANUM
      *
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *    画面テーブルへ編集する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN2-MAX
               MOVE    LNK-PTKOHFTN-REC (IDX)  TO  PTKOHFTN-REC
      *
               MOVE    PTKOHFTN-RENNUM     TO  SPA-NAI24-TRENNUM  (IDX)
               MOVE    PTKOHFTN-TEKSTYMD   TO  SPA-NAI24-TFTNSTYMD(IDX)
               MOVE    PTKOHFTN-TEKEDYMD   TO  SPA-NAI24-TFTNEDYMD (IDX)
               MOVE    PTKOHFTN-GAIJGNGAK  TO  SPA-GMN24-TGAIJGNGAK(IDX)
               MOVE    PTKOHFTN-NYUJGNGAK  TO  SPA-GMN24-TNYUJGNGAK(IDX)
      *
               MOVE    PTKOHFTN-TEKSTYMD   TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNSTYMD(IDX)
               MOVE    PTKOHFTN-TEKEDYMD   TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNEDYMD(IDX)
      *
           END-PERFORM
      *    １件もないとき、公費の内容を初期表示
           IF      SPA-FTN2-MAX        =   ZERO
               MOVE    SPA-GMN24-TKOHSTYMD (WRK-FTN1-IDX)
                                          TO  SPA-GMN24-FTNSTYMD
               MOVE    SPA-NAI24-TKOHSTYMD (WRK-FTN1-IDX)
                                          TO  SPA-NAI24-FTNSTYMD
               MOVE    SPA-GMN24-TKOHEDYMD (WRK-FTN1-IDX)
                                          TO  SPA-GMN24-FTNEDYMD
               MOVE    SPA-NAI24-TKOHEDYMD (WRK-FTN1-IDX)
                                          TO  SPA-NAI24-FTNEDYMD
               MOVE    ZERO               TO  SPA-NAI24-GAIJGNGAK
               MOVE    ZERO               TO  SPA-NAI24-NYUJGNGAK
               MOVE    SPACE              TO  SPA-GMN24-GAIJGNGAK
               MOVE    SPACE              TO  SPA-GMN24-NYUJGNGAK
               MOVE    32                 TO  SPA-GMN24-CUR
           ELSE
               MOVE    31                 TO  SPA-GMN24-CUR
           END-IF
           .
       41013-FTNSELNUM-EXT.
           EXIT.
      *****************************************************************
      *    公費負担パラメタ変更処理
      *****************************************************************
       410131-KOHFTN-PARA-SEC           SECTION.
      *
      *    公費負担更新保存
           INITIALIZE                  HZN-PTKOHFTN-AREA
           MOVE    ZERO                TO  IDP-1
           MOVE    ZERO                TO  IDP-2
      *
      *    対象のデータをパラメタファイルより編集する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-PARA-EDANUM
           MOVE    ZERO                TO  WRK-KOHID
           MOVE    ZERO                TO  WRK-IDX
           PERFORM
               UNTIL   FLG-PARA            =   1
               MOVE    PARA-DATA-REC           TO  PTKOHFTN-REC
      *        公費ＩＤがテーブルにあること（ないものは削除とする）
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   SPA-FTN1-MAX
                   IF      PTKOHFTN-KOHID      =   SPA-NAI24-KOHID(IDX2)
                       IF      SPA-NAI24-FTNDELKBN (IDX2)  =   SPACE
                           MOVE    1               TO  FLG-OK
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                       MOVE    SPA-FTN1-MAX        TO  IDX2
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   1
      *
      *        公費ＩＤ毎にテーブル保存
               IF      PTKOHFTN-KOHID  NOT =   WRK-KOHID
                   ADD     1                   TO  IDP-1
                   IF      IDP-1               >   10
                       MOVE    10              TO  IDP-1
                   END-IF
                   MOVE    PTKOHFTN-KOHID      TO  WRK-KOHID
                   MOVE    ZERO                TO  IDP-2
               END-IF
               IF     (SPA-NAI24-FTNSELNUM >   ZERO   )  AND
                      (PTKOHFTN-KOHID      =   SPA-NAI24-KOHID
                                                 (SPA-NAI24-FTNSELNUM))
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDP-2
                   IF      IDP-2               <=  50
                       MOVE    PARA-DATA-REC   TO  HZN-PTKOHFTN-REC
                                                        (IDP-1 IDP-2)
                   END-IF
               END-IF
               END-IF
      *
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
      *    現在の更新分をパラメタへ
           IF      SPA-FTN2-MAX        =   ZERO
      *        パラメタ更新
               PERFORM 410132-KOHFTN-PARA-INS-SEC
               GO      TO      410131-KOHFTN-PARA-EXT
           END-IF
      *
           ADD     1                   TO  IDP-1
           MOVE    ZERO                TO  IDP-2
           IF      IDP-1               >   10
               DISPLAY "GP024 KOHFTN PARA 10 OVER"
               MOVE    "9900"          TO  SPA-ERRCD
               GO      TO      410131-KOHFTN-PARA-EXT
           END-IF
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   SPA-FTN2-MAX
      *
               IF      SPA-NAI24-FTNDELKBN2 (IDX)  =   SPACE
                   PERFORM 410131-KOHFTN-TBL-INS-SEC
               END-IF
           END-PERFORM
      *
      *    パラメタ追加
           PERFORM 410132-KOHFTN-PARA-INS-SEC
           .
       410131-KOHFTN-PARA-EXT.
           EXIT.
      *****************************************************************
      *    公費負担テーブル追加処理
      *****************************************************************
       410131-KOHFTN-TBL-INS-SEC           SECTION.
      *
      *    公費負担編集
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPID          TO  PTKOHFTN-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTKOHFTN-PTID
           MOVE    SPA-NAI24-KOHID  (SPA-NAI24-FTNSELNUM)
                                       TO  PTKOHFTN-KOHID
      *    連番を再番
           ADD     1                   TO  IDP-2
           MOVE    IDP-2               TO  PTKOHFTN-RENNUM
      *    適用開始年月日
           MOVE    SPA-NAI24-TFTNSTYMD(IDX)
                                       TO  PTKOHFTN-TEKSTYMD
           MOVE    SPA-NAI24-TFTNEDYMD(IDX)
                                       TO  PTKOHFTN-TEKEDYMD
           MOVE    SPA-NAI24-KOHNUM  (SPA-NAI24-FTNSELNUM)
                                       TO  PTKOHFTN-KOHNUM
           MOVE    SPA-NAI24-PAYKBN  (SPA-NAI24-FTNSELNUM)
                                       TO  PTKOHFTN-PAYKBN
           MOVE    SPA-GMN24-TGAIJGNGAK(IDX)
                                       TO  PTKOHFTN-GAIJGNGAK
           MOVE    SPA-GMN24-TNYUJGNGAK(IDX)
                                       TO  PTKOHFTN-NYUJGNGAK
      *
           MOVE    PTKOHFTN-REC        TO  HZN-PTKOHFTN-REC
                                                        (IDP-1 IDP-2)
           .
       410131-KOHFTN-TBL-INS-EXT.
           EXIT.
      *****************************************************************
      *    公費負担パラメタ追加処理
      *****************************************************************
       410132-KOHFTN-PARA-INS-SEC           SECTION.
      *
      *    削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    ZERO                TO  WRK-PARA-EDANUM
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   IDP-1
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   50  )  OR
                              (HZN-PTKOHFTN-REC(IDX IDY) =   SPACE)
                   ADD     1                   TO  WRK-PARA-EDANUM
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "P02"               TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
                   MOVE    WRK-PARA-EDANUM     TO  PARA-EDANUM
                   MOVE    HZN-PTKOHFTN-REC(IDX IDY)
                                               TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
               END-PERFORM
           END-PERFORM
      *
           .
       410132-KOHFTN-PARA-INS-EXT.
           EXIT.
      *****************************************************************
      *    公費負担／公費選択番号選択２
      *****************************************************************
       41016-KOHFTNLIST2-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX       >   SPA-FTN2-MAX
               IF      P02-P024-HFTNLST2-SELECT (IDX)  =   "T"
                   IF      IDX           =   SPA-GMN24-FTNSELNUM2
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN24-FTNSELNUM2
           END-IF
      *
           IF      SPA-GMN24-FTNSELNUM2    NOT =   ZERO
               MOVE    SPA-GMN24-FTNSELNUM2    TO  P02-P024-FTNSELNUM2
               PERFORM 41014-FTNSELNUM2-SEC
               MOVE    31                  TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41016-KOHFTNLIST2-EXT.
           EXIT.
      *****************************************************************
      *    公費負担／公費選択番号選択
      *****************************************************************
       41014-FTNSELNUM2-SEC           SECTION.
      *
           MOVE    P02-P024-FTNSELNUM2     TO  SPA-GMN24-FTNSELNUM2
      *
           IF      SPA-GMN24-FTNSELNUM2    =   ZERO
               GO     TO      41014-FTNSELNUM2-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-FTN1-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN2-MAX
               IF      SPA-GMN24-FTNSELNUM2    =   SPA-GMN24-TGMNNUM2
                                                                (IDX)
                   MOVE    IDX             TO  WRK-FTN1-IDX
                   MOVE    IDX             TO  SPA-NAI24-FTNSELNUM2
                   MOVE    SPA-FTN2-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-FTN1-IDX        =   ZERO
               MOVE    "2431"              TO  SPA-ERRCD
               MOVE    31                  TO  SPA-GMN24-CUR
               GO      TO                  41014-FTNSELNUM2-EXT
           END-IF
      *
      *    対象のデータを編集する
           MOVE    WRK-FTN1-IDX        TO  IDX
           MOVE    SPA-NAI24-TFTNSTYMD(IDX)    TO  SPA-NAI24-FTNSTYMD
           MOVE    SPA-NAI24-TFTNEDYMD(IDX)    TO  SPA-NAI24-FTNEDYMD
           MOVE    SPA-GMN24-TGAIJGNGAK(IDX)   TO  SPA-NAI24-GAIJGNGAK
           MOVE    SPA-GMN24-TNYUJGNGAK(IDX)   TO  SPA-NAI24-NYUJGNGAK
      *
           MOVE    SPA-GMN24-TFTNSTYMD(IDX)    TO  SPA-GMN24-FTNSTYMD
           MOVE    SPA-GMN24-TFTNEDYMD(IDX)    TO  SPA-GMN24-FTNEDYMD
           MOVE    SPA-NAI24-GAIJGNGAK         TO  WRK-Z7
           MOVE    WRK-Z7                      TO  SPA-GMN24-GAIJGNGAK
           MOVE    SPA-NAI24-NYUJGNGAK         TO  WRK-Z7
           MOVE    WRK-Z7                      TO  SPA-GMN24-NYUJGNGAK
      *
           MOVE    32                  TO  SPA-GMN24-CUR
           .
       41014-FTNSELNUM2-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU     NOT =   ZERO
               PERFORM 4109-TOUROKU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      410-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      *    低所得　選択番号
           IF      P02-P024-TSRSELNUM  =   SPA-MAE24-TSYSELNUM
               MOVE    ZERO                TO  FLG-IN-TSYSELNUM
           ELSE
               MOVE    1                   TO  FLG-IN-TSYSELNUM
           END-IF
           MOVE    P02-P024-TSRSELNUM  TO  SPA-GMN24-TSYSELNUM
           MOVE    P02-P024-TSYSTYMD   TO  SPA-GMN24-TSYSTYMDW
           MOVE    P02-P024-TSYEDYMD   TO  SPA-GMN24-TSYEDYMDW
           MOVE    P02-P024-TSYSKJGNSTYMD  TO  SPA-GMN24-TSYSKJGNSTYMDW
      *
      *    低年金　選択番号
           IF      P02-P024-TNKSELNUM  =   SPA-MAE24-TNKSELNUM
               MOVE    ZERO                TO  FLG-IN-TNKSELNUM
           ELSE
               MOVE    1                   TO  FLG-IN-TNKSELNUM
           END-IF
           MOVE    P02-P024-TNKSELNUM  TO  SPA-GMN24-TNKSELNUM
           MOVE    P02-P024-TNKSTYMD   TO  SPA-GMN24-TNKSTYMDW
           MOVE    P02-P024-TNKEDYMD   TO  SPA-GMN24-TNKEDYMDW
      *
      *    公費負担金　選択番号
           MOVE    P02-P024-FTNSELNUM2 TO  SPA-GMN24-FTNSELNUM2
           MOVE    P02-P024-FTNTEKSTYMD    TO  SPA-GMN24-FTNSTYMD
           MOVE    P02-P024-FTNTEKEDYMD    TO  SPA-GMN24-FTNEDYMD
           MOVE    P02-P024-GAIJGNGAK  TO  SPA-GMN24-GAIJGNGAK
           MOVE    P02-P024-NYUJGNGAK  TO  SPA-GMN24-NYUJGNGAK
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    低所得　選択番号
           PERFORM 41012-TSYSELNUM-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    低所得開始年月日
           PERFORM 41012-TSYSTYMD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    低所得終了年月日
           PERFORM 41012-TSYEDYMD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    食事標準負担減額開始日
           PERFORM 41012-TSYSKJGNSTYMD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    低年金　選択番号
           PERFORM 41012-NENINSELNUM-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    低年金開始年月日
           PERFORM 41012-TNKSTYMD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    低年金終了年月日
           PERFORM 41012-TNKEDYMD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    公費負担 開始日
           PERFORM 41031-FTNSTYMD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    公費負担 終了日
           PERFORM 41031-FTNEDYMD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    公費負担 外来上限額
           PERFORM 41031-GAIJGNGAK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    公費負担 入院上限額
           PERFORM 41032-NYUJGNGAK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／選択番号 
      *****************************************************************
       41012-TSYSELNUM-SEC           SECTION.
      *
           IF      SPA-GMN24-TSYSELNUM =   SPACE
               MOVE    SPA-GMN24-TSYSELNUM TO  SPA-MAE24-TSYSELNUM
               MOVE    ZERO                TO  SPA-NAI24-TSYSELNUM
               GO      TO               41012-TSYSELNUM-EXT
           END-IF
      *
           INITIALIZE                       ORCSNUMAREA
           MOVE    SPA-GMN24-TSYSELNUM  TO  SNUM-INX
           CALL    "ORCSNUM"                USING
                                            ORCSNUMAREA
           IF     (SNUM-RC          NOT =   ZERO  )   OR
                  (SNUM-MINKBN      NOT =   SPACE )   OR
                  (SNUM-SYOKBN      NOT =   SPACE )
               MOVE    "2401"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN24-CUR
               GO      TO                  41012-TSYSELNUM-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI24-TSYSELNUM
               MOVE    SNUM-OUTNUM         TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ         TO  SPA-GMN24-TSYSELNUM
           END-IF
      *
           MOVE    SPA-GMN24-TSYSELNUM     TO  SPA-MAE24-TSYSELNUM
           IF      FLG-IN-TSYSELNUM    =   ZERO
               GO      TO      41012-TSYSELNUM-EXT
           END-IF
      *
      *    削除データを除く内部インデックスを取得
           INITIALIZE                   WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TSYSELNUM  TO  WRK-G-IDX
           MOVE    1                    TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           IF      WRK-N-IDX            =   ZERO
               MOVE    "2401"           TO  SPA-ERRCD
               MOVE    SPACE            TO  SPA-GMN24-TSYSTYMDW
               MOVE    SPACE            TO  SPA-GMN24-TSYSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TSYEDYMDW
               MOVE    SPACE            TO  SPA-GMN24-TSYEDYMD
               MOVE    SPACE            TO  SPA-GMN24-TSYSKJGNSTYMDW
               MOVE    SPACE            TO  SPA-GMN24-TSYSKJGNSTYMD
           ELSE
               MOVE  SPA-GMN24-TSYDPSTYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYSTYMDW
               MOVE  SPA-GMN24-TSYDPEDYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYEDYMDW
               MOVE  SPA-GMN24-TSYDPSTYMD  (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYSTYMD
               MOVE  SPA-GMN24-TSYDPEDYMD  (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYEDYMD
               MOVE  SPA-GMN24-TSYLSKJGNSTYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYSKJGNSTYMDW
               MOVE  SPA-GMN24-TSYLSKJGNSTYMD (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYSKJGNSTYMD
           END-IF
           .
       41012-TSYSELNUM-EXT.
           EXIT.
      *****************************************************************
      *    低所得者／開始年月日 4121120--->H12.11.20
      *****************************************************************
       41012-TSYSTYMD-SEC           SECTION.
      *
           IF      SPA-GMN24-TSYSTYMDW =   SPACE
               MOVE    SPACE               TO  SPA-GMN24-TSYSTYMDW
               MOVE    SPACE               TO  SPA-GMN24-TSYSTYMD
               GO  TO                   41012-TSYSTYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TSYSTYMDW     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TSYSTYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TSYSTYMD
           ELSE
               MOVE    "2402"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN24-CUR
           END-IF
           .
       41012-TSYSTYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／終了年月日 4121120--->H12.11.20
      *****************************************************************
       41012-TSYEDYMD-SEC           SECTION.
      *
           IF    ( SPA-GMN24-TSYEDYMDW     =   SPACE )  AND
                 ( SPA-GMN24-TSYSTYMDW     =   SPACE )
               MOVE    SPACE               TO  SPA-GMN24-TSYEDYMDW
               MOVE    SPACE               TO  SPA-GMN24-TSYEDYMD
               GO  TO                     41012-TSYEDYMD-EXT
           END-IF
      *----(01.00.04) LINE ADD START --------------------------------
           IF     ((SPA-GMN24-TSYEDYMDW(1:7)   =   ALL "9" ) AND
                   (SPA-GMN24-TSYEDYMDW(8:)    =   SPACE   ))  OR
                  ( SPA-GMN24-TSYEDYMDW        =   SPACE    )
               MOVE    SPACE               TO  SPA-GMN24-TSYEDYMDW(1:7)
               MOVE    ALL "9"             TO  SPA-GMN24-TSYEDYMDW(1:7)
               MOVE    ALL "9"             TO  SPA-GMN24-TSYEDYMD
               GO  TO                      41012-TSYEDYMD-EXT
           END-IF
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TSYEDYMDW     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TSYEDYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TSYEDYMD
           ELSE
               MOVE    "2403"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN24-CUR
               GO  TO                   41012-TSYEDYMD-EXT
           END-IF
      *
           IF  SPA-GMN24-TSYSTYMD       <  SPA-GMN24-TSYEDYMD
               CONTINUE
           ELSE
               MOVE    "2404"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41012-TSYEDYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／食事標準負担減額開始日
      *****************************************************************
       41012-TSYSKJGNSTYMD-SEC           SECTION.
      *
           IF      SPA-GMN24-TSYSKJGNSTYMDW    =   SPACE
               MOVE    SPACE               TO  SPA-GMN24-TSYSKJGNSTYMD
               GO      TO      41012-TSYSKJGNSTYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TSYSKJGNSTYMDW
                                       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TSYSKJGNSTYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TSYSKJGNSTYMD
           ELSE
               MOVE    "2413"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN24-CUR
               GO  TO                   41012-TSYSKJGNSTYMD-EXT
           END-IF
      *
           IF     (SPA-GMN24-TSYSTYMD  <=  SPA-GMN24-TSYSKJGNSTYMD) AND
                  (SPA-GMN24-TSYEDYMD  >=  SPA-GMN24-TSYSKJGNSTYMD)
               CONTINUE
           ELSE
               MOVE    "2414"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41012-TSYSKJGNSTYMD-EXT.
           EXIT.
      *****************************************************************
      *    年金／選択番号 
      *****************************************************************
       41012-NENINSELNUM-SEC           SECTION.
      *
           IF  SPA-GMN24-TNKSELNUM     =   SPACE
               MOVE    SPA-GMN24-TNKSELNUM TO  SPA-MAE24-TNKSELNUM
               MOVE    ZERO                TO  SPA-NAI24-TNKSELNUM
               GO      TO               41012-NENINSELNUM-EXT
           END-IF
      *
           INITIALIZE                       ORCSNUMAREA
           MOVE    SPA-GMN24-TNKSELNUM  TO  SNUM-INX
           CALL    "ORCSNUM"                USING
                                            ORCSNUMAREA
           IF     (SNUM-RC          NOT =   ZERO  )   OR
                  (SNUM-MINKBN      NOT =   SPACE )   OR
                  (SNUM-SYOKBN      NOT =   SPACE )
               MOVE    "2406"           TO  SPA-ERRCD
               MOVE    6                TO  SPA-GMN24-CUR
               GO      TO               41012-NENINSELNUM-EXT
           ELSE
               MOVE    SNUM-OUTNUM      TO  SPA-NAI24-TNKSELNUM
               MOVE    SNUM-OUTNUM      TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ      TO  SPA-GMN24-TNKSELNUM
           END-IF
      *
           MOVE    SPA-GMN24-TNKSELNUM TO  SPA-MAE24-TNKSELNUM
           IF      FLG-IN-TNKSELNUM    =   ZERO
               GO      TO               41012-NENINSELNUM-EXT
           END-IF
      *
      *    削除データを除く内部インデックスを取得
           INITIALIZE                   WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TNKSELNUM  TO  WRK-G-IDX
           MOVE    2                    TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           IF  WRK-N-IDX                =   ZERO
               MOVE    "2406"           TO  SPA-ERRCD
               MOVE    6                TO  SPA-GMN24-CUR
               MOVE    SPACE            TO  SPA-GMN24-TNKSTYMDW
               MOVE    SPACE            TO  SPA-GMN24-TNKEDYMDW
               MOVE    SPACE            TO  SPA-GMN24-TNKSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TNKEDYMD
           ELSE
               MOVE  SPA-GMN24-TNKDPSTYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKSTYMDW
               MOVE  SPA-GMN24-TNKDPEDYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKEDYMDW
               MOVE  SPA-GMN24-TNKDPSTYMD  (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKSTYMD
               MOVE  SPA-GMN24-TNKDPEDYMD  (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKEDYMD
           END-IF
           .
       41012-NENINSELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／開始年月日 4121120--->H12.11.20
      *****************************************************************
       41012-TNKSTYMD-SEC       SECTION.
      *
           IF      SPA-GMN24-TNKSTYMDW =   SPACE
               MOVE    SPACE               TO  SPA-GMN24-TNKSTYMDW
               MOVE    SPACE               TO  SPA-GMN24-TNKSTYMD
               GO  TO                   41012-TNKSTYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TNKSTYMDW     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TNKSTYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TNKSTYMD
           ELSE
               MOVE    7                   TO  SPA-GMN24-CUR
               MOVE    "2407"              TO  SPA-ERRCD
           END-IF
            .
       41012-TNKSTYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／終了年月日 4121120--->H12.11.20
      *****************************************************************
       41012-TNKEDYMD-SEC           SECTION.
      *
           IF     (SPA-GMN24-TNKEDYMDW    =   SPACE )  AND
                  (SPA-GMN24-TNKSTYMDW    =   SPACE )
               MOVE    SPACE              TO  SPA-GMN24-TNKEDYMDW
               MOVE    SPACE              TO  SPA-GMN24-TNKEDYMD
               GO  TO                     41012-TNKEDYMD-EXT
           END-IF
      *----(01.00.04) LINE ADD START --------------------------------
           IF    ((SPA-GMN24-TNKEDYMDW(1:7)   =   ALL "9" )  AND
                  (SPA-GMN24-TNKEDYMDW(8:)    =   SPACE   ) )  OR
                 ( SPA-GMN24-TNKEDYMDW        =   SPACE     )
               MOVE    ALL "9"             TO  SPA-GMN24-TNKEDYMDW (1:7)
               MOVE    ALL "9"             TO  SPA-GMN24-TNKEDYMD
               GO  TO                      41012-TNKEDYMD-EXT
           END-IF
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TNKEDYMDW     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TNKEDYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TNKEDYMD
           ELSE
               MOVE    8                   TO  SPA-GMN24-CUR
               MOVE    "2408"              TO  SPA-ERRCD
               GO  TO                   41012-TNKEDYMD-EXT
           END-IF
      *
           IF      SPA-GMN24-TNKSTYMD  <  SPA-GMN24-TNKEDYMD
               CONTINUE
           ELSE
               MOVE    "2409"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN24-CUR
           END-IF
           .
       41012-TNKEDYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担／開始年月日
      *****************************************************************
       41031-FTNSTYMD-SEC           SECTION.
      *
           IF      SPA-GMN24-FTNSTYMD  =   SPACE
               MOVE    SPACE               TO  SPA-GMN24-FTNSTYMD
               MOVE    SPACE               TO  SPA-NAI24-FTNSTYMD
               GO      TO      41031-FTNSTYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-FTNSTYMD  TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-FTNSTYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI24-FTNSTYMD
           ELSE
               MOVE    "2432"              TO  SPA-ERRCD
               MOVE    32                  TO  SPA-GMN24-CUR
           END-IF
      *    対象公費の期間であること
           IF      SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >   SPA-NAI24-FTNSTYMD
               MOVE    "2432"              TO  SPA-ERRCD
               MOVE    32                  TO  SPA-GMN24-CUR
               GO      TO      41031-FTNSTYMD-EXT
           END-IF
           .
       41031-FTNSTYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担／終了年月
      *****************************************************************
       41031-FTNEDYMD-SEC           SECTION.
      *
           IF    ( SPA-GMN24-FTNSTYMD  =   SPACE )  AND
                 ( SPA-GMN24-FTNEDYMD  =   SPACE )
               MOVE    SPACE           TO  SPA-GMN24-FTNEDYMD
               MOVE    SPACE           TO  SPA-NAI24-FTNEDYMD
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
           IF    ( SPA-GMN24-FTNEDYMD  =   SPACE )  AND
                 ((FLG-TOUROKU     NOT =   ZERO  )  OR
                  (WRK-MCP-WIDGET      =   "P024_FTNTEKEDYMD"))
      *        公費の終了日
               MOVE    SPA-GMN24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)
                                           TO  SPA-GMN24-FTNEDYMD
               MOVE    SPA-NAI24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)
                                           TO  SPA-NAI24-FTNEDYMD
           END-IF
      *
           IF      SPA-GMN24-FTNEDYMD  =   SPACE
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
           IF      SPA-GMN24-FTNEDYMD(1:7) =   ALL "9"
               MOVE    SPACE               TO  SPA-GMN24-FTNEDYMD
               MOVE    ALL "9"             TO  SPA-GMN24-FTNEDYMD(1:7)
               MOVE    ALL "9"             TO  SPA-NAI24-FTNEDYMD
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-FTNEDYMD  TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-FTNEDYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI24-FTNEDYMD
           ELSE
               MOVE    "2433"              TO  SPA-ERRCD
               MOVE    33                  TO  SPA-GMN24-CUR
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *    対象公費の期間であること
           IF      SPA-NAI24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)
                                           <   SPA-NAI24-FTNEDYMD
               MOVE    "2433"              TO  SPA-ERRCD
               MOVE    33                  TO  SPA-GMN24-CUR
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
           IF      SPA-GMN24-FTNSTYMD  <=  SPA-GMN24-FTNEDYMD
               CONTINUE
           ELSE
               MOVE    "2438"              TO  SPA-ERRCD
               MOVE    33                  TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41031-FTNEDYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担 外来上限額
      *****************************************************************
       41031-GAIJGNGAK-SEC           SECTION.
      *
           INITIALIZE                       ORCSNUMAREA
           MOVE    SPA-GMN24-GAIJGNGAK TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC          NOT =   ZERO  )   OR
                  (SNUM-MINKBN      NOT =   SPACE )   OR
                  (SNUM-SYOKBN      NOT =   SPACE )   OR
                  (SNUM-SEISUU          >   7     )
               MOVE    "2435"           TO  SPA-ERRCD
               MOVE    34               TO  SPA-GMN24-CUR
               GO      TO               41031-GAIJGNGAK-EXT
           ELSE
               MOVE    SNUM-OUTNUM      TO  SPA-NAI24-GAIJGNGAK
               MOVE    SNUM-OUTNUM      TO  WRK-Z7
               MOVE    WRK-Z7           TO  SPA-GMN24-GAIJGNGAK
           END-IF
      *
      *
           .
       41031-GAIJGNGAK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担 入院上限額
      *****************************************************************
       41032-NYUJGNGAK-SEC           SECTION.
      *
           INITIALIZE                       ORCSNUMAREA
           MOVE    SPA-GMN24-NYUJGNGAK TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC          NOT =   ZERO  )   OR
                  (SNUM-MINKBN      NOT =   SPACE )   OR
                  (SNUM-SYOKBN      NOT =   SPACE )   OR
                  (SNUM-SEISUU          >   7     )
               MOVE    "2436"           TO  SPA-ERRCD
               MOVE    35               TO  SPA-GMN24-CUR
               GO      TO               41032-NYUJGNGAK-EXT
           ELSE
               MOVE    SNUM-OUTNUM      TO  SPA-NAI24-NYUJGNGAK
               MOVE    SNUM-OUTNUM      TO  WRK-Z7
               MOVE    WRK-Z7           TO  SPA-GMN24-NYUJGNGAK
           END-IF
      *
           .
       41032-NYUJGNGAK-EXT.
           EXIT.
      *
      ****************************************************************
      **    登録前関連チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *    所得履歴登録
           IF      FLG-TOUROKU         =   1
               PERFORM 41091-SYOTOKU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *    老年福祉年金履歴登録
           IF      FLG-TOUROKU         =   2
               PERFORM 41092-RFUKUSI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *    公費負担額登録
           IF      FLG-TOUROKU         =   3
               PERFORM 41093-KOHFTN-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    所得履歴登録前チェック処理
      ****************************************************************
       41091-SYOTOKU-CHK-SEC             SECTION.
      *
           IF     (SPA-GMN24-TSYSTYMD  =   SPACE )  OR
                  (SPA-GMN24-TSYEDYMD  =   SPACE )
               MOVE    "2411"          TO  SPA-ERRCD
               MOVE     2              TO  SPA-GMN24-CUR
               GO      TO      41091-SYOTOKU-CHK-EXT
           END-IF
      *
           INITIALIZE                  WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TSYSELNUM TO  WRK-G-IDX
           MOVE    1                   TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-TSY-MAX
               IF  ( SPA-GMN24-TSYDELKBN(IDX)   =   SPACE )  AND
                   ( WRK-N-IDX              NOT =   IDX   )
      *        表示明細中に入力開始・終了日を含むデータが存在しないこと
                   IF  ( SPA-GMN24-TSYEDYMD
                                       <   SPA-GMN24-TSYDPSTYMD(IDX)) OR
                       ( SPA-GMN24-TSYSTYMD
                                       >   SPA-GMN24-TSYDPEDYMD(IDX))
                       CONTINUE
                   ELSE
                       MOVE   "2405"       TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN24-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       41091-SYOTOKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    所得履歴登録前チェック処理
      ****************************************************************
       41092-RFUKUSI-CHK-SEC             SECTION.
      *
      *
           IF     (SPA-GMN24-TNKSTYMD  =   SPACE )  OR
                  (SPA-GMN24-TNKEDYMD  =   SPACE )
               MOVE    "2412"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN24-CUR
               GO      TO      41092-RFUKUSI-CHK-EXT
           END-IF
      *
      *    表示明細中に同開始日が存在しないこと
           INITIALIZE                  WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TNKSELNUM TO  WRK-G-IDX
           MOVE    2                   TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-TNK-MAX
             IF  ( SPA-GMN24-TNKDELKBN(IDX)   =   SPACE )  AND
                 ( WRK-N-IDX              NOT =   IDX   )
      *        表示明細中に入力開始・終了日を含むデータが存在しないこと
               IF  ( SPA-GMN24-TNKEDYMD < SPA-GMN24-TNKDPSTYMD(IDX)) OR
                   ( SPA-GMN24-TNKSTYMD > SPA-GMN24-TNKDPEDYMD(IDX))
                   CONTINUE
               ELSE
                   MOVE   "2410"       TO  SPA-ERRCD
      *----(01.00.01) LINE ADD START --------------------------------
                   MOVE    7           TO  SPA-GMN24-CUR
      *----(01.00.01) LINE ADD END   --------------------------------
                   GO      TO          41092-RFUKUSI-CHK-EXT
               END-IF
             END-IF
           END-PERFORM
      *
           .
       41092-RFUKUSI-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    内部インデックス取得処理　（削除データ除く）
      ****************************************************************
       4501-NAIBU-IDX-GET-SEC             SECTION.
      **
      * / WRK-G-IDX...画面上インデックス
      *   WRK-N-IDX...削除データ除く内部的なインデックス
      *
           MOVE    ZERO                   TO  WRK-N-IDX
           MOVE    ZERO                   TO  IDY
           EVALUATE    WRK-W-TBL
      *    所得テーブルより取得
           WHEN    1
               PERFORM   VARYING   IDX    FROM    1   BY  1
                         UNTIL    (IDX            >   20         )
                         OR       (IDX            >   SPA-TSY-MAX)
                         OR       (WRK-N-IDX      >   ZERO       )
                 IF  SPA-GMN24-TSYDELKBN (IDX)    =   SPACE
                     ADD     1                    TO  IDY
                     IF      IDY                  =   WRK-G-IDX
                         MOVE    IDX              TO  WRK-N-IDX
                     END-IF
                 END-IF
               END-PERFORM
      *
      *    年金テーブルより取得
           WHEN    2
               PERFORM   VARYING   IDX    FROM    1   BY  1
                         UNTIL    (IDX            >   10         )
                         OR       (IDX            >   SPA-TNK-MAX)
                         OR       (WRK-N-IDX      >   ZERO       )
                 IF  SPA-GMN24-TNKDELKBN (IDX)    =   SPACE
                     ADD     1                    TO  IDY
                     IF      IDY                  =   WRK-G-IDX
                         MOVE    IDX              TO  WRK-N-IDX
                     END-IF
                 END-IF
               END-PERFORM
           END-EVALUATE
           .
       4501-NAIBU-IDX-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担更新前チェック処理
      *****************************************************************
       41093-KOHFTN-CHK-SEC             SECTION.
      *
           IF     (SPA-NAI24-FTNSTYMD  =   SPACE )  OR
                  (SPA-NAI24-FTNEDYMD  =   SPACE )
               MOVE    "2437"          TO  SPA-ERRCD
               MOVE    32              TO  SPA-GMN24-CUR
               GO      TO      41093-KOHFTN-CHK-EXT
           END-IF
      *
      *    期間が重複しないこと
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   SPA-FTN2-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-NAI24-FTNSELNUM2    NOT =   IDX )  AND
                      (SPA-NAI24-FTNDELKBN2(IDX)   =   SPACE )
                   IF     (SPA-NAI24-FTNSTYMD  <   SPA-NAI24-TFTNSTYMD
                                                              (IDX)) OR
                          (SPA-NAI24-FTNEDYMD  >   SPA-NAI24-TFTNEDYMD
                                                              (IDX))
                       CONTINUE
                   ELSE
                       MOVE    "2434"          TO  SPA-ERRCD
                       MOVE    32              TO  SPA-GMN24-CUR
                   END-IF
               END-IF
           END-PERFORM
           .
       41093-KOHFTN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       410112-SE-WAFORMAT-YMDEDIT-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"
               MOVE    WRK-SYMD            TO  WRK-HENYMD
               GO      TO      410112-SE-WAFORMAT-YMDEDIT-EXT
           END-IF
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"        USING  STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF    STS-DAY-RC1            =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMD
           END-IF
           .
       410112-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦→和暦FORMAT日付編集
      *****************************************************************
       410113-WA-FOMAT-YMDEDIT-SEC     SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "22"                TO  LNK-DAY2-IRAI
           MOVE    WRK-WYMD            TO  LNK-DAY2-YMD (2:7)
           CALL    "ORCSDAY"    USING   STS-AREA-DAY  LNK-DAY2-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMD
           END-IF
           .
       410113-WA-FOMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦→西暦日付編集
      *****************************************************************
       410114-WA-SE-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD (2:7)
           CALL    "ORCSDAY"    USING   STS-AREA-DAY  LNK-DAY1-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
           ELSE
               MOVE    SPACE               TO  WRK-SYMD
           END-IF
           .
       410114-WA-SE-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  HZN-PARA-REC
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "PARA-KEY"          TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
           MOVE    HZN-PARA-REC        TO  PARA-REC
           IF      FLG-PARA            =   1
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *!       IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
      *!       END-IF
           ELSE
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *!       IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
      *!       END-IF
           END-IF
      *
           .
       400-PARA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル検索処理
      *****************************************************************
      *980-PARA-READ-SEC             SECTION.
      *
      *    READ    PARA-FILE
      *        INVALID
      *            MOVE    1           TO  FLG-PARA
      *        NOT INVALID
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *990-PARA-NEXT-SEC           SECTION.
      *
      *    READ    PARA-FILE       NEXT
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *990-PARA-NEXT-EXT.
      *    EXIT.
      *
