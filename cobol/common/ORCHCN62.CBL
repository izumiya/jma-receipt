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
       PROGRAM-ID.             ORCHCN62.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : お薬手帳印刷
      *  コンポーネント名  : 入院お薬手帳印刷（ＨＣＮ６２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/09/09    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 04.06.00     NACL-多々納  11/02/22  用法コードコメント対応
      * 04.06.00     NACL-森脇    11/03/10  用量割合コード対応
      * 04.06.00     NACL-多々納  12/09/XX  ＱＲコード対応
      * 04.07.00     NACL-多々納  12/09/XX  商品名コード対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(50000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    帳票領域
           COPY    "HCM62.INC".
      *
      *    ＱＲ帳票領域
           COPY    "HCM62Q.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-KBN             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-SOURYO          PIC 9(01).
           03  FLG-HEN-END         PIC 9(01).
      *
           03  FLG-QROK            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(02).
           03  CNT-ZAI             PIC 9(03).
           03  CNT-SRY             PIC 9(02).
           03  CNT-KAISU           PIC 9(01).
      *H24.9
           03  IDX-QR1             PIC 9(04).
           03  IDX-QR2             PIC 9(04).
           03  IDX-QRB             PIC 9(04).
           03  IDX-QY              PIC 9(04).
           03  IDQ                 PIC 9(04).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX-STR             PIC 9(04).
           03  IDX-STR1            PIC 9(04).
           03  IDX-KET             PIC 9(04).
           03  IDX-KET1            PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(03).
           03  IDX-SRY             PIC 9(02).
           03  MAX-GYO             PIC 9(04).
           03  IDX-KBN             PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-HENSURYO        PIC X(11).
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z9              PIC Z9.
      *
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-SRYSYUMEI           PIC X(20).
      *
           03  WRK-HEN-AREA.
               05  WRK-HENLINE.
                   07  WRK-HEN-CNT     PIC X(03).
                   07  WRK-HEN-DATA    PIC X(50).
                   07  WRK-HEN-Y1      PIC X(01).
               05  WRK-HENNAIYO    PIC X(26).
               05  WRK-YKZNAME     PIC X(100).
               05  WRK-YOUHOU      PIC X(100).
               05  WRK-SURYOU      PIC X(11).
               05  WRK-TANI        PIC X(12).
               05  WRK-KAISUU      PIC X(03).
      *
           03  WRK-HENSYUDATA      PIC X(100).
      *
           03  WRK-SSTKIJUNCD      PIC X(05).
           03  WRK-HENKBN-G.
               05  WRK-HENKBN      PIC 9(08)V9(05)
                                               OCCURS  5.
      *
           03  WRK-YOURYO          PIC 9(05)V9(05).
           03  WRK-KAISU           PIC 9(01).
      *
      *H24.9
      *    ＱＲ編集
           03  WRK-TNS-NAME        PIC X(100).
           03  WRK-YOH-NAME-G.
               05  WRK-YOH-NAME        OCCURS   5.
                   07  WRK-YOH-NAME-1  PIC X(02).
                   07  WRK-YOH-NAME-2  PIC X(98).
           03  WRK-FILE-CNT        PIC 9(02).
           03  WRK-SYS-TIME        PIC 9(08).
      *ver.4.7
           03  WRK-SHO-NAME        PIC X(80).
           03  WRK-SHO-TANINAME    PIC X(20).
      *
       01  WRK-MEISAI-AREA.
           03  WRK-M-KETA              PIC 9(03).
      *
      *    剤内容領域
       01  WRK-SRY-AREA.
           03  WRK-ZAIKAIKEI       PIC 9(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRY-TBL         OCCURS  50.
               05  WRK-SRYSURYO    PIC 9(05)V9(05).
               05  WRK-SRYKAISU    PIC 9(03).
               05  WRK-INPUTCOMENT PIC X(80).
               05  WRK-YOURYOU-TBL.
                   07  WRK-YOURYOU     PIC 9(05)V9(05)
                                       OCCURS   5.
      *
      *    印刷編集領域
       01  WRK-LINE-AREA.
           03  WRK-LINE-1.
               05  WRK-PTNAME      PIC X(54).
           03  WRK-LINE-2.
               05  FILLER          PIC X(06)   VALUE   "病棟".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-BTUNAME     PIC X(20).
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-ROOMNO      PIC X(06).
           03  WRK-LINE-3.
               05  FILLER          PIC X(06)   VALUE   "交付日".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-SRYYMDG     PIC X(04).
               05  WRK-SRYYMDY     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "年".
               05  WRK-SRYYMDM     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "月".
               05  WRK-SRYYMDD     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "日".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  FILLER          PIC X(08)   VALUE   "保険医名".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-DRNAME      PIC X(18).
           03  WRK-LINE-4.
               05  WRK-HOSPNAME    PIC X(54).
           03  WRK-LINE-5.
               05  WRK-HOSPTEL     PIC X(15).
           03  WRK-LINE-SEN.
               05  FILLER          PIC X(54)   VALUE
               "＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿".
      *
      *
      *    最大行数
       01  MAX-LINE                PIC 9(03)   VALUE    68.
       01  MAX-LINE2               PIC 9(03)   VALUE    34.
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *H24.9
      *    ＱＲ位置
       01  QR-FILE-NAME.
           03  FILLER              PIC X(05)   VALUE   "/tmp/".
           03  QR-FILE-HOSPNUM     PIC 9(02)   VALUE   ZERO.
           03  QR-FILE-FORM-ID     PIC X(05)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  QR-FILE-TERMID      PIC X(32)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  QR-FILE-SYOHMS      PIC 9(08)   VALUE   ZERO.
           03  FILLER              PIC X(04)   VALUE   ".qr_".
           03  QR-FILE-ID          PIC 9(02)   VALUE   ZERO.
           03  FILLER              PIC X(04)   VALUE   ".png".
      *
       01  KUHAKU-FILE-NAME        PIC X(512).
      *    空白の画像
       01  KUHAKU-NAME.
           03  FILLER          PIC X(05)   VALUE   "data/".
           03  FILLER          PIC X(15)   VALUE   "transparent.jpg".
      *　ＱＲ名称
       01  CONS-FORM-ID        PIC X(05)   VALUE   "HC62Q".
      *H24.9
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-COMMENT2           PIC X(01).
      *H24.9
           03  PARA-QRKBN              PIC X(01).
           03  PARA-SHOHIN             PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1034.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数付加情報マスタ
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *ＱＲ対応
      *    ＱＲデータ作成サブパラメタ
           COPY    "CPORCSQRCSV62.INC".
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHCN62.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ORCHCN62AREA.
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *H24.9
           INITIALIZE                  ORCSQRCSV62AREA
           MOVE    ZERO                TO  IDX-QR1
           MOVE    ZERO                TO  IDX-QRB
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *
      *    出力先プリンタ名割り当て情報
           INITIALIZE                     SYS-1034-REC
           INITIALIZE                     ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1034-REC
           END-IF
      *
      *    医療機関情報−基本情報
           PERFORM 900-1001-READ-SEC
      *
      *    医療機関情報−所在地、連絡先
           PERFORM 900-1002-READ-SEC
      *
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    保険医名称検索
           PERFORM 1001-DRCDNAME-SEC
      *
      *    病棟・病室編集
           MOVE    ORCHCN62-BTUNAME    TO  WRK-BTUNAME
           MOVE    ORCHCN62-ROOMNO     TO  WRK-ROOMNO
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
      *    プログラムオプション設定
           INITIALIZE                  PARA-AREA
      *    コメント２
           MOVE    "COMMENT2"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-COMMENT2
      *H24.9
      *    ＱＲ印刷
           MOVE    "QRKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-QRKBN
      *    商品名記載
           MOVE    "SHOHIN"        TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-SHOHIN
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険医名編集処理
      *****************************************************************
       1001-DRCDNAME-SEC           SECTION.
      *
      *    保険医名編集
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    ORCHCN62-DRCD       TO  SYS-1010-KBNCD
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
           MOVE    SYS-1010-NAME       TO  WRK-DRNAME
      *
           .
       1001-DRCDNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE               TO  SYS-1900-REC
           INITIALIZE                      SYS-1900-REC
           MOVE    "1900"              TO  SYS-1900-KANRICD
           MOVE    "*"                 TO  SYS-1900-KBNCD
           MOVE    SYS-1900-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(18) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC        SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHCN62"      TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME   TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING
                                       SPA-AREA
                                       ORCSPRGOPTIONAREA
           MOVE    SPACE           TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
           END-IF
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票ＨＥＡＤ情報編集処理
           PERFORM 210-HEAD-HENSYU-SEC
      *
      *    データがなくなるまで
           PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                   UNTIL ( IDX-ZAI     >   ORCHCN62-ZAI-MAX  )
      *        帳票ＢＯＤＹ情報編集処理
               PERFORM 220-BODY-HENSYU-SEC
           END-PERFORM
      *
           IF      IDX-ZAI             >   ZERO
      *
      *        帳票ＦＯＯＴ情報編集処理
               PERFORM 430-FOOT-SET-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRT-WRITE-SEC
           END-IF
      *
      *H24.9
      *    ＱＲ印刷
           IF      PARA-QRKBN          =   "1"
      *        ドクターなしは対象外
               PERFORM 500-QR-SYORI-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤ情報編集処理
      *****************************************************************
       210-HEAD-HENSYU-SEC         SECTION.
      *
      *    診療日
           MOVE    ORCHCN62-SRYYMD     TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3-G  TO  WRK-SRYYMDG
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  WRK-SRYYMDY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  WRK-SRYYMDM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-SRYYMDD
      *
      *    患者氏名
           MOVE    SPACE               TO  WRK-PTNAME
           IF      SPA-NAME        NOT =   SPACE
               STRING  SPA-NAME        DELIMITED  BY  SPACE
                       "　様"          DELIMITED  BY  SIZE
                       INTO            WRK-PTNAME
               END-STRING
           ELSE
               STRING  SPA-KANANAME    DELIMITED  BY  SPACE
                       "　様"          DELIMITED  BY  SIZE
                       INTO            WRK-PTNAME
               END-STRING
           END-IF
           INSPECT WRK-PTNAME          REPLACING  ALL "  "  BY  "　"
      *
      *    帳票ＨＥＡＤセット処理
           INITIALIZE                  HCM62
           MOVE    ZERO            TO  CNT-LINE
           PERFORM 410-HEAD-SET-SEC
      *
           .
       210-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ情報編集処理
      *****************************************************************
       220-BODY-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SOURYO
      *
      *    剤情報編集領域にセット
           INITIALIZE                  WRK-SRY-AREA
      *
           MOVE    ORCHCN62-ZAIKAIKEI(IDX-ZAI)
                                   TO  WRK-ZAIKAIKEI
           MOVE    ORCHCN62-SRYSYUKBN(IDX-ZAI)
                                   TO  WRK-SRYSYUKBN
           MOVE    ORCHCN62-SRYKBN(IDX-ZAI)
                                   TO  WRK-SRYKBN
      *
           MOVE    SPACE           TO  WRK-SSTKIJUNCD
      *
           IF      ORCHCN62-YSRYCD(IDX-ZAI) NOT =   SPACE
               MOVE    ORCHCN62-YSRYCD(IDX-ZAI) TO  WRK-SRYCD
      *        点数検索
               PERFORM 900-TENSU-READ-SEC
      *        回数カウント
               MOVE    SPACE           TO  WRK-SSTKIJUNCD
               MOVE    ZERO            TO  CNT-KAISU
               PERFORM VARYING IDX-KBN     FROM    1   BY  1
                       UNTIL   IDX-KBN     >   5
                   IF      TNS-SSTKIJUNCD(IDX-KBN) =   1
                       COMPUTE CNT-KAISU   =   CNT-KAISU   +   1
                       MOVE    "1"         TO  WRK-SSTKIJUNCD(IDX-KBN:1)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    ZERO            TO  CNT-SRY
      *H24.9
      *    ＱＲ対応
           MOVE    ZERO                TO  IDX-QR2
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHCN62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
      *        明細編集
               PERFORM 230-NAIYO-HENSYU-SEC
           END-PERFORM
      *
      *    剤情報があれば処理する
           IF      CNT-SRY             >   ZERO
      *        回数編集
               PERFORM 240-KAISUU-HENSYU-SEC
      *
               MOVE    WRK-LINE-SEN      TO  HCM62-SEN(CNT-LINE)
           END-IF
      *
           .
       220-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       230-NAIYO-HENSYU-SEC        SECTION.
      *
      *ver.4.7
      *    商品名コード
           MOVE    SPACE               TO  WRK-SHO-TANINAME
           MOVE    SPACE               TO  WRK-SHO-NAME
           IF     (PARA-SHOHIN         =   "1"  )
             AND  (ORCHCN62-SHO-SRYCD(IDX-ZAI IDX-SRY)
                                   NOT =   SPACE )
               MOVE    ORCHCN62-SHO-SRYCD (IDX-ZAI IDX-SRY)
                                           TO  WRK-SRYCD
      *        点数検索
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TANINAME        TO  WRK-SHO-TANINAME
               MOVE    TNS-NAME            TO  WRK-SHO-NAME
           END-IF
      *
      *
      *    点数検索
           MOVE    ORCHCN62-SRYCD(IDX-ZAI IDX-SRY)
                                       TO  WRK-SRYCD
      *    減の文字は対象外
           IF      WRK-SRYCD   =   "820000047"
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
           PERFORM 900-TENSU-READ-SEC
      *
      *ver.4.7
      *    商品名コード
           IF      ORCHCN62-SHO-SRYCD(IDX-ZAI IDX-SRY)  NOT =   SPACE
      *        名称と単位は商品名コードの内容とする
               IF      WRK-SHO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SHO-TANINAME    TO  TNS-TANINAME
               END-IF
               IF      WRK-SHO-NAME        NOT =   SPACE
                   MOVE    WRK-SHO-NAME        TO  TNS-NAME
               END-IF
           END-IF
      *!
      *
           COMPUTE CNT-SRY             =   CNT-SRY     +   1
      *
           MOVE    ORCHCN62-SRYSURYO(IDX-ZAI IDX-SRY)
                                       TO  WRK-SRYSURYO(CNT-SRY)
           MOVE    ORCHCN62-SRYKAISU(IDX-ZAI IDX-SRY)
                                       TO  WRK-SRYKAISU(CNT-SRY)
           MOVE    ORCHCN62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                       TO  WRK-INPUTCOMENT(CNT-SRY)
           MOVE    ORCHCN62-YOURYOU-TBL(IDX-ZAI IDX-SRY)
                                   TO  WRK-YOURYOU-TBL(CNT-SRY)
      *
           INITIALIZE                  WRK-HEN-AREA
      *
      *    ＱＲ用
           MOVE    SPACE               TO  WRK-TNS-NAME
           MOVE    SPACE               TO  WRK-YOH-NAME-G
           MOVE    ZERO                TO  IDX-QY
      *
      *    １件目だけ番号表示
           IF      CNT-SRY             =   1
               MOVE    IDX-ZAI         TO  WRK-Z9
               MOVE    WRK-Z9          TO  WRK-HEN-CNT(1:2)
               MOVE    ")"             TO  WRK-HEN-CNT(3:1)
      *        注射は診療名称編集
               IF      WRK-SRYKBN      =   "31"
                                       OR  "32"
                                       OR  "33"
                   PERFORM 2301-SRYKBN-HENSYU-SEC
               END-IF
           END-IF
      *
           IF      TNS-SRYCD(1:3)  =   "001"
      *        用法編集
               PERFORM 2302-YOUHOU-HENSYU-SEC
           ELSE
      *        名称編集
               PERFORM 2303-YKZNAME-HENSYU-SEC
      *        薬剤、その他材料のみ数量あり
               IF    ( WRK-SRYCD(1:1)      =   "6"   ) OR
                     ( WRK-SRYCD(1:1)      =   "7"   ) OR
                     ( WRK-SRYCD(1:3)      =   "059" )
      *            数量編集
                   PERFORM 2304-SURYOU-HENSYU-SEC
                   IF    ( PARA-COMMENT2       =   "1"   )
      *               薬剤のみ
                    AND  ( WRK-SRYCD(1:1)      =   "6"   )
      *                用量編集
                       PERFORM 240-YOURYOU-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *H24.9
      *    ＱＲ名称編集
           PERFORM 5001-QRMEISAI-HENSYU-SEC
      *
           .
       230-NAIYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別名編集
      *****************************************************************
       2301-SRYKBN-HENSYU-SEC       SECTION.
      *
           IF      WRK-SRYSYUKBN       =   SPACE
               MOVE    WRK-SRYKBN          TO  WRK-SRYSYUKBN(1:2)
               MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           END-IF
           IF      WRK-SRYSYUKBN(3:1)  NOT =   "0"
               MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           END-IF
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
               AT   END
                   MOVE    SPACE           TO  WRK-SRYSYUMEI
               WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN(MSYU-IDX)
                   MOVE    MSYU-SRYMEI (MSYU-IDX)
                                           TO  WRK-SRYSYUMEI
           END-SEARCH
      *
           MOVE    WRK-SRYSYUMEI       TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
           .
       2301-SRYKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法編集処理
      *****************************************************************
       2302-YOUHOU-HENSYU-SEC       SECTION.
      *
      *    コメント名称判定
           IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
               STRING  "【"            DELIMITED   BY  SIZE
                       TNS-NAME        DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
                       INTO            WRK-YOUHOU
               END-STRING
           ELSE
               STRING  "【"            DELIMITED   BY  SIZE
                       WRK-INPUTCOMENT(CNT-SRY)
                                       DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
                       INTO            WRK-YOUHOU
               END-STRING
           END-IF
      *    ＱＲ用
           MOVE    WRK-YOUHOU      TO  WRK-TNS-NAME
      *
           MOVE    WRK-YOUHOU      TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       2302-YOUHOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称編集処理
      *****************************************************************
       2303-YKZNAME-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-YKZNAME
      *
      *    薬剤編集
           IF      TNS-SRYCD(1:1)  =   "6"
      *        内用薬の総量編集の判定
               IF      TNSP-SOURYOHENKBN   =   1
                   MOVE    1               TO  FLG-SOURYO
               END-IF
               MOVE    TNS-NAME            TO  WRK-YKZNAME
           ELSE
      *        コメントあればセット、なければ名称セット
               IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
                   MOVE    TNS-NAME        TO  WRK-YKZNAME
               ELSE
                   MOVE    WRK-INPUTCOMENT(CNT-SRY)
                                           TO  WRK-YKZNAME
               END-IF
           END-IF
      *    ＱＲ用
           MOVE    WRK-YKZNAME     TO  WRK-TNS-NAME
      *
           MOVE    WRK-YKZNAME     TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       2303-YKZNAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       2304-SURYOU-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
           MOVE    WRK-SRYSURYO(CNT-SRY)   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  WRK-SURYOU
           END-IF
      *
           STRING  " "             DELIMITED  BY  SIZE
                   WRK-SURYOU      DELIMITED  BY  SPACE
                   " "             DELIMITED  BY  SIZE
                   TNS-TANINAME    DELIMITED  BY  SPACE
                   INTO            WRK-HENNAIYO
           END-STRING
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       2304-SURYOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集処理
      *****************************************************************
       240-KAISUU-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF    ( WRK-SRYKBN      =   "21" )  AND
                 ( FLG-SOURYO      =   1    )
               MOVE    "23"            TO  WRK-SRYKBN
           END-IF
      *
           MOVE    WRK-ZAIKAIKEI   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-KAISUU
           END-IF
      *
           EVALUATE    WRK-SRYKBN
      *        内服
               WHEN    "21"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "日分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
      *        頓服
               WHEN    "22"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "回分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "回"            DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
      *       外用・コメント
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-HENNAIYO
           END-EVALUATE
      *
           IF      WRK-HENNAIYO    NOT =   SPACE
               PERFORM 250-MHEN-SYORI-SET-SEC
           END-IF
      *
           .
       240-KAISUU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用量編集処理
      *****************************************************************
       240-YOURYOU-HENSYU-SEC       SECTION.
      *
      *    用量コード設定分セット
           MOVE    WRK-SRYSURYO(CNT-SRY)   TO  WRK-SURYO
           MOVE    CNT-KAISU       TO  WRK-KAISU
           MOVE    ZERO            TO  WRK-HENKBN-G
           MOVE    ZERO            TO  FLG-KBN
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL ( IDX-KBN     >   5 )
                   OR    ( FLG-KBN     =   1 )
      *        用量コードがあった場合
               IF      WRK-YOURYOU(CNT-SRY IDX-KBN)     >   ZERO
      *            指定してない服用時点ならエラー
                   IF      WRK-SSTKIJUNCD(IDX-KBN:1)   NOT  =   "1"
                       MOVE    1               TO  FLG-KBN
                   ELSE
      *                用量の方が多い場合エラー
                       IF      WRK-SURYO      <
                                   WRK-YOURYOU(CNT-SRY IDX-KBN) 
                           MOVE    1               TO  FLG-KBN
                       ELSE
                           MOVE    WRK-YOURYOU(CNT-SRY IDX-KBN) 
                                               TO  WRK-HENKBN(IDX-KBN)
      *                    用量コード使用分は計算にいれない
                           COMPUTE WRK-SURYO  =   WRK-SURYO
                               -   WRK-YOURYOU(CNT-SRY IDX-KBN) 
                           COMPUTE WRK-KAISU   =   WRK-KAISU   -   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    エラーなら処理抜ける
           IF      FLG-KBN  =   1
               GO  TO  240-YOURYOU-HENSYU-EXT
           END-IF
      *
      *    入力分をセット
           COMPUTE WRK-YOURYO   =   WRK-SURYO  /   WRK-KAISU
      *    割切れない場合処理抜ける
           IF      WRK-SURYO    =   WRK-YOURYO *   WRK-KAISU
               PERFORM VARYING  IDX-KBN    FROM    1   BY  1
                       UNTIL  ( IDX-KBN    >   5 )
      *            服用時点コードで設定していない場合は割ったものをセット
                   IF      WRK-SSTKIJUNCD(IDX-KBN:1)   =   "1"
                       IF      WRK-HENKBN(IDX-KBN)     =   0
                           MOVE    WRK-YOURYO  TO  WRK-HENKBN(IDX-KBN)
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
               GO  TO  240-YOURYOU-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL ( IDX-KBN     >   5 )
      *        用量コードがあった場合
               IF      WRK-HENKBN(IDX-KBN)    >   ZERO
                   MOVE    SPACE       TO  WRK-HENSYUDATA
      *
                   EVALUATE    IDX-KBN
                       WHEN    1
                           MOVE    "　朝："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    2
                           MOVE    "　昼："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    3
                           MOVE    "　夕："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    4
                           MOVE    "　寝："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    5
                           MOVE    "　起："    TO  WRK-HENSYUDATA(1:6)
                   END-EVALUATE
      *
                   MOVE    WRK-HENKBN(IDX-KBN)
                                               TO  WRK-SURYO
                   PERFORM 250-ZHEN-SURYO-SET-SEC
                   MOVE    WRK-HENSURYO        TO  WRK-SURYOU
      *
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-SURYOU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           TNS-TANINAME    DELIMITED  BY  SPACE
                           INTO            WRK-HENSYUDATA(7:)
                   END-STRING
      *H24.9
      *            ＱＲ　飲み方コメント
                   ADD     1                   TO  IDX-QY
                   MOVE    WRK-HENSYUDATA      TO  WRK-YOH-NAME(IDX-QY)
      *
                   PERFORM 250-HENSYU-SYORI-SEC
               END-IF
           END-PERFORM
      *
           .
       240-YOURYOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁チェック　処理
      *****************************************************************
       250-PAGE-CHK-SEC            SECTION.
      *
           EVALUATE    TRUE
      *        ページ件数を超えていたら印刷
               WHEN    CNT-LINE            =    MAX-LINE
                   PERFORM 400-PRT-WRITE-SEC
                   INITIALIZE                  HCM62
                   MOVE    ZERO            TO  CNT-LINE
                   PERFORM 410-HEAD-SET-SEC
               WHEN    CNT-LINE            =   MAX-LINE2
      *        半ページ件数を超えていたら改ページ
                   PERFORM 410-HEAD-SET-SEC
           END-EVALUATE
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           .
       250-PAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票Ｚ編集処理
      *****************************************************************
       250-ZHEN-SURYO-SET-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
      *    数量小数点以下編集
           MOVE    ZERO            TO  FLG-SP
           PERFORM VARYING IDX-STR     FROM    5   BY  -1
                   UNTIL   IDX-STR     <   1
               IF      WRK-SURYO-S2(IDX-STR:1) NOT =   ZERO
                   MOVE    1           TO  FLG-SP
               END-IF
               IF      FLG-SP      =   1
                   MOVE    WRK-SURYO-S2(IDX-STR:1)
                                        TO  WRK-SURYO-Z3(IDX-STR:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."             TO  WRK-SURYO-Z2
               IF  WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  IDX-STR1
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL   IDX-STR     >   11
               IF      WRK-SURYO-X(IDX-STR:1)  NOT =   SPACE
                   COMPUTE IDX-STR1    =   IDX-STR1    +   1
                   MOVE  WRK-SURYO-X(IDX-STR:1)
                                           TO  WRK-HENSURYO(IDX-STR1:1)
               END-IF
           END-PERFORM
      *
           .
       250-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-MHEN-SYORI-SET-SEC      SECTION.
      *
      *    桁数
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HENNAIYO        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-M-KETA
      *
           COMPUTE IDX-KET1            =   54  -   WRK-M-KETA
      *
      *    文字が重なる場合は１行下に表示する
           IF    ( HCM62-NAIYO1(CNT-LINE)(IDX-KET1:)   NOT =   SPACE )
           OR    ( HCM62-NAIYO2(CNT-LINE)              NOT =   SPACE )
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-HENNAIYO    TO  HCM62-NAIYO2(CNT-LINE)
           ELSE
               MOVE    WRK-HENNAIYO    TO  HCM62-NAIYO2(CNT-LINE)
           END-IF
      *
           .
       250-MHEN-SYORI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-HENSYU-SYORI-SEC        SECTION.
      *
      *    桁数
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HENSYUDATA      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-M-KETA
      *
           MOVE    1                   TO  IDX-STR
           MOVE    50                  TO  MAX-GYO
           MOVE    ZERO                TO  FLG-HEN-END
           PERFORM
                   UNTIL   FLG-HEN-END     =   1
               MOVE    WRK-HENSYUDATA(IDX-STR:MAX-GYO)
                                           TO  WRK-HEN-DATA
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-HENLINE         TO  HCM62-NAIYO1(CNT-LINE)
               MOVE    SPACE               TO  WRK-HENLINE
               COMPUTE IDX-STR             =   IDX-STR
                                           +   MAX-GYO
               IF      IDX-STR             >   WRK-M-KETA
                   MOVE    1               TO  FLG-HEN-END
               END-IF
           END-PERFORM
      *
           .
       250-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       400-PRT-WRITE-SEC           SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC62"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ORCHCN62-CNT    TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE        TO  HC01PARA-CNT(4:3)
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCM62           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCM62.red"     TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
           MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    SYS-1034-PRTNM(11)
                                   TO  MKPRT-PRTNM
           MOVE   "お薬手帳"       TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       410-HEAD-SET-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    CNT-PAGE        TO  WRK-ZZ9
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-1      TO  HCM62-NAIYO1(CNT-LINE)
      *
           MOVE    SPACE           TO  HCM62-NAIYO2(CNT-LINE)
           STRING  WRK-ZZ9         DELIMITED  BY  SIZE
                   " 頁"           DELIMITED  BY  SIZE
                   INTO            HCM62-NAIYO2(CNT-LINE)
           END-STRING
      *
      *    病棟・病室
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-2      TO  HCM62-NAIYO1(CNT-LINE)
      *
      *    処方日・保険医
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-3      TO  HCM62-NAIYO1(CNT-LINE)
      *
           MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
      *
           .
       410-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＦＯＯＴセット処理
      *****************************************************************
       430-FOOT-SET-SEC            SECTION.
      *
      *    医療機関名称セット
           PERFORM 250-PAGE-CHK-SEC
           MOVE    WRK-LINE-4      TO  HCM62-NAIYO1(CNT-LINE)
      *
      *    医療機関電話番号セット
           PERFORM 250-PAGE-CHK-SEC
           MOVE    WRK-LINE-5      TO  HCM62-NAIYO1(CNT-LINE)
      *
           .
       430-FOOT-SET-EXT.
           EXIT.
      *
      *H24.9
      *****************************************************************
      *    ＱＲ剤編集処理
      *****************************************************************
       5001-QRMEISAI-HENSYU-SEC            SECTION.
      *
           IF      ORCHCN62-SRYKBN (IDX-ZAI)    =   "98"
      *        備考コメント
               PERFORM 50012-QR-BIKOU-HEN-SEC
           ELSE
      *        ＱＲ剤編集
               PERFORM 50011-QR-MEISAI-HEN-SEC
           END-IF
      *
           .
       5001-QRMEISAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ剤編集処理
      *****************************************************************
       50011-QR-MEISAI-HEN-SEC            SECTION.
      *
           IF      IDX-QR2             =   ZERO
               ADD     1                   TO  IDX-QR1
      *
      *        剤回数
               MOVE    ORCHCN62-ZAIKAIKEI (IDX-ZAI)
                                       TO  ORCSQRCSV62-KAISU (IDX-QR1)
      *        診療区分
               MOVE    ORCHCN62-SRYKBN (IDX-ZAI)
                                       TO  ORCSQRCSV62-SRYKBN (IDX-QR1)
      *        診療種別
               MOVE    ORCHCN62-SRYSYUKBN (IDX-ZAI)
                                       TO  ORCSQRCSV62-SRYSYUKBN
                                                              (IDX-QR1)
           END-IF
      *
           ADD     1                   TO  IDX-QR2
           MOVE    ORCHCN62-SRYCD(IDX-ZAI IDX-SRY)
                                       TO  ORCSQRCSV62-SRYCD
                                                       (IDX-QR1 IDX-QR2)
           MOVE    WRK-TNS-NAME        TO  ORCSQRCSV62-TNS-NAME
                                                       (IDX-QR1 IDX-QR2)
           MOVE    TNS-TANINAME        TO  ORCSQRCSV62-TANINAME
                                                       (IDX-QR1 IDX-QR2)
           MOVE    ORCHCN62-SRYSURYO(IDX-ZAI IDX-SRY)
                                       TO  ORCSQRCSV62-SURYOU
                                                       (IDX-QR1 IDX-QR2)
           MOVE    TNS-YKZKBN          TO  ORCSQRCSV62-YKZKBN
                                                       (IDX-QR1 IDX-QR2)
           IF      TNSP-SOURYOHENKBN   =   1
      *        内服点剤
               MOVE    "1"             TO  ORCSQRCSV62-NAIHENKBN
                                                       (IDX-QR1 IDX-QR2)
           END-IF
           IF      ORCHCN62-SRYCD(IDX-ZAI IDX-SRY)(1:3) =   "001"
      *        用法コメント区分
               MOVE    TNS-KENTAICOMMENT   TO  ORCSQRCSV62-YCOMMNTKBN
                                                  (IDX-QR1 IDX-QR2)
           END-IF
      *
      *    飲み方コメント
           PERFORM VARYING     IDX-QY  FROM    1   BY  1
                   UNTIL      (IDX-QY      >   5   )
                           OR (WRK-YOH-NAME (IDX-QY)   =   SPACE )
                           OR (IDX-QR2     >=   100  )
               ADD     1                   TO  IDX-QR2
               MOVE    "810000001"         TO  ORCSQRCSV62-SRYCD
                                                       (IDX-QR1 IDX-QR2)
               MOVE    WRK-YOH-NAME-2(IDX-QY)
                                           TO  ORCSQRCSV62-TNS-NAME
                                                       (IDX-QR1 IDX-QR2)
           END-PERFORM
      *
           .
       50011-QR-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ備考コメント処理
      *****************************************************************
       50012-QR-BIKOU-HEN-SEC            SECTION.
      *
           ADD     1                   TO  IDX-QRB
           IF      IDX-QRB              <=  10
               MOVE    WRK-TNS-NAME        TO  ORCSQRCSV62-BIKOU
                                                       (IDX-QRB)
           END-IF
           .
       50012-QR-BIKOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ印刷処理
      *****************************************************************
       500-QR-SYORI-SEC            SECTION.
      *
           MOVE    ORCHCN62-SRYYMD     TO  ORCSQRCSV62-SRYYMD
      *    医療機関情報
           MOVE    SYS-1001-PREFNUM    TO  ORCSQRCSV62-PREFNUM
           MOVE    SYS-1001-HOSPCD     TO  ORCSQRCSV62-HOSPCD
           MOVE    SYS-1001-HOSPSBT    TO  ORCSQRCSV62-HOSPSBT
           MOVE    WRK-HOSPNAME        TO  ORCSQRCSV62-HOSPNAME
      *    ドクター
           MOVE    SYS-1010-KANANAME   TO  ORCSQRCSV62-DR-KANANAME
           MOVE    SYS-1010-NAME       TO  ORCSQRCSV62-DR-NAME
      *    患者情報
           MOVE    SPA-NAME            TO  ORCSQRCSV62-NAME
           MOVE    SPA-KANANAME        TO  ORCSQRCSV62-KANANAME
           MOVE    SPA-SEX             TO  ORCSQRCSV62-SEX
           MOVE    SPA-BIRTHDAY        TO  ORCSQRCSV62-BIRTHDAY
      *    時間
           COMPUTE WRK-SYS-TIME        =   SYS-TIME
                                       +   ORCHCN62-CNT
      *    ファイル名
           MOVE    CONS-FORM-ID        TO  ORCSQRCSV62-FORM-ID
      *    ファイル名組み立て項目
           MOVE    WRK-SYS-TIME        TO  ORCSQRCSV62-SYSTIME
      *
           CALL    "ORCSQRCSV62"       USING
                                       SPA-AREA
                                       ORCSQRCSV62AREA
      *
      *    ＱＲイメージ印刷
           IF     (ORCSQRCSV62-RET-COUNT   >   ZERO)
             AND  (ORCSQRCSV62-RET-COUNT   <=  12  )
               PERFORM 5003-QRIMAGE-HEN-SEC
           END-IF
           .
       500-QR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *        ＱＲイメージ印刷処理
      *****************************************************************
       5003-QRIMAGE-HEN-SEC      SECTION.
      *
      *    ＱＲ空白画像位置
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    KUHAKU-NAME         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  KUHAKU-FILE-NAME
      *
           MOVE    ZERO                TO  FLG-QROK
           MOVE    SPACE               TO  HCM62Q
           MOVE    ZERO                TO  CNT-LINE
      *
           MOVE    ZERO                TO  WRK-FILE-CNT
           PERFORM VARYING     IDQ     FROM    1  BY  1
                   UNTIL       IDQ     >   12
               MOVE    KUHAKU-FILE-NAME    TO  HCM62Q-IMAGE-1 (IDQ)
               ADD     1                   TO  WRK-FILE-CNT
               IF      WRK-FILE-CNT        <=  ORCSQRCSV62-RET-COUNT 
                   MOVE    SPA-HOSPNUM         TO  QR-FILE-HOSPNUM
                   MOVE    CONS-FORM-ID        TO  QR-FILE-FORM-ID
                   MOVE    SPA-TERMID          TO  QR-FILE-TERMID
                   MOVE    WRK-SYS-TIME        TO  QR-FILE-SYOHMS
                   MOVE    WRK-FILE-CNT        TO  QR-FILE-ID
      *            ファイル存在チェック
                   INITIALIZE                      ORCSFILECHKAREA
                   MOVE    QR-FILE-NAME        TO  FILECHK-NAME
                   CALL    "ORCSFILECHK"       USING
                                               ORCSFILECHKAREA
                   IF      FILECHK-RC          =   ZERO OR 2
                       MOVE   QR-FILE-NAME     TO  HCM62Q-IMAGE-1 (IDQ)
                       MOVE   1                TO  FLG-QROK
                   END-IF
      *
                   IF      WRK-FILE-CNT        =   1
      *                帳票ＨＥＡＤセット処理
                       PERFORM 50010-HEAD-SET-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-QROK            =   1
      *        帳票出力処理
               PERFORM 50031-QRPRT-WRITE-SEC
           END-IF
           .
       5003-QRIMAGE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       50010-HEAD-SET-SEC            SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
           MOVE    CNT-PAGE        TO  WRK-ZZ9
      *
           COMPUTE CNT-LINE        =   CNT-LINE    +   1
           MOVE    WRK-LINE-1      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
           MOVE    SPACE           TO  HCM62Q-NAIYO2(CNT-LINE)
           STRING  WRK-ZZ9         DELIMITED  BY  SIZE
                   " 頁"           DELIMITED  BY  SIZE
                   INTO            HCM62Q-NAIYO2(CNT-LINE)
           END-STRING
      *
      *    病棟・病室
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-2      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-3      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
           MOVE    WRK-LINE-SEN    TO  HCM62Q-SEN(CNT-LINE)
      *
           MOVE    4               TO  CNT-LINE
           MOVE    WRK-LINE-SEN    TO  HCM62Q-SEN(CNT-LINE)
      *    医療機関名称セット
           MOVE    WRK-LINE-4      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
      *    医療機関電話番号セット
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-5      TO  HCM62Q-NAIYO1(CNT-LINE)
           .
       50010-HEAD-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       50031-QRPRT-WRITE-SEC           SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC62"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ORCHCN62-CNT    TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE        TO  HC01PARA-CNT(4:3)
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCM62Q          TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCM62Q.red"    TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
           MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    SYS-1034-PRTNM(11)
                                   TO  MKPRT-PRTNM
           MOVE   "お薬手帳"       TO  MKPRT-INFO
           CALL   "ORCSMKPRT"          USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       50031-QRPRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
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
      *     MOVE    LNK-DAY2-EDTYMD3
      *                             TO  WRK-HENYMDG
      *     INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    ORCHCN62-SRYYMD     TO  TNS-YUKOSTYMD
           MOVE    ORCHCN62-SRYYMD     TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
      *    点数附加情報検索
           IF    ( TNS-SRYCD(1:1)      =   "6"  )  AND
                 ( FLG-TENSU           =   ZERO )
               PERFORM 900-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE              TENSUPLUS-REC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加情報読み込み
      *****************************************************************
       900-TENSUPLUS-READ-SEC           SECTION.
      *
           INITIALIZE                  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNSP-SRYCD
           MOVE    ORCHCN62-SRYYMD     TO  TNSP-YUKOSTYMD
           MOVE    ORCHCN62-SRYYMD     TO  TNSP-YUKOEDYMD
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSUPLUS
                   MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               ELSE
                   MOVE    1               TO  FLG-TENSUPLUS
                   INITIALIZE              TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS
               INITIALIZE              TENSUPLUS-REC
           END-IF
      *
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
                   INITIALIZE                      SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−基本情報読み込み
      *****************************************************************
       900-1001-READ-SEC           SECTION.
      *
      *    医療機関情報−基本情報読み込み
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SYS-1001-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *
           .
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−所在地、連絡先読み込み
      *****************************************************************
       900-1002-READ-SEC           SECTION.
      *
      *    医療機関情報−所在地、連絡先読み込み
           MOVE    SPACE               TO  SYS-1002-REC
           INITIALIZE                      SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SYS-1002-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           MOVE    SYS-1002-TEL    TO  WRK-HOSPTEL
      *
           .
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                       TO  SYS-1901-KBNCD
           MOVE    SYS-1901-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                       TO  SYS-1902-KBNCD
           MOVE    SYS-1902-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1902-REC
               MOVE    SYS-1902-TEL        TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
      *
           .
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込開始処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
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
      *
      *****************************************************************
      *    ＤＢ読込検索処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込終了処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *
