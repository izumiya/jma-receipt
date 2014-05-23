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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGI0API01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院登録
      *  管理者            :
      *  作成日付    作業者        記述
      *  14/04/09    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-NYUINCDCHG          PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX8                    PIC 9(05).
           03  IDX9                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXE                    PIC 9(05).
           03  IDX-ITM                 PIC 9(05).
           03  IDXS                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-SAGAKU              PIC 9(08).
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-KJNYMD              PIC X(08).
      *
       01  REG-AREA.
           03  REG-SAVEREQ             PIC X(01).
           03  REG-REQNUM              PIC X(02).
           03  REG-NYUINYMD            PIC X(08).
           03  REG-IDOYMD              PIC X(08).
           03  REG-BTUNUM              PIC X(02).
           03  REG-BRMNUM              PIC X(06).
           03  REG-SRYKA               PIC X(02).
           03  REG-SAGAKU              PIC X(07).
           03  REG-SAGAKUCD            PIC X(02).
           03  REG-DR                  PIC X(05)
                                       OCCURS   3.
           03  REG-SHOKAIYMD           PIC X(08).
           03  REG-KAIGOKBN            PIC X(01).
           03  REG-HKNCOMBI            PIC X(04).
           03  REG-KHNCD               PIC X(09).
           03  REG-NYUINRYOCD          PIC X(03).
           03  REG-TEIKIKBN            PIC X(01).
           03  REG-TAIINYMD            PIC X(08).
           03  REG-TENNYUYMD           PIC X(08).
           03  REG-TAIINCD             PIC X(02).
           03  REG-ERR-MAX             PIC 9(03).
           03  REG-ERR-OCC             OCCURS  10.
               05  REG-ERRCD           PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-H240401           PIC X(08)   VALUE "20120401".
           03  CONST-ERR-MAX           PIC 9(03)   VALUE 10.
           03  CONST-ODRITEM-MAX       PIC 9(05)   VALUE 30.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *    オーダエラーコード
           COPY    "CPNCLAIMERRCD.INC".
      *
      *    オーダ項目ＩＤ
           COPY    "CPNCLAIMID.INC".
      *
      *    オーダＩＤ
           COPY    "CPNORDERID.INC".
      *
      *    患者入院履歴退避
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //TRRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    算定可能入院料取得
           COPY    "CPORCSNYUINRYO.INC".
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
      *
      *    入院基本情報取得サブ
           COPY    "CPORCSNINF01.INC".
           COPY    "CPORCSNINF02.INC".
      *
      *    ＵＩＤ取得用エリア
       01  SUID-AREA.
           03  SUID-RET                PIC X(2).
           03  SUID-UID                PIC X(36).
       01  SUID-ST                     PIC 9(2).
      *
      *    API XML読み込み用定義体
           COPY    "CPHSPTINFMV2REQ.INC".
      *
      *    API XML書き込み用定義体
      *    入院履歴
           COPY    "CPHSPTINFM01V2RES.INC".
      *    請求情報
           COPY    "CPHSPTINFM02V2RES.INC".
      *
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK5113.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院コード置換
       01  NYUINCDCHG-REC.
           COPY    "CPNYUINCDCHG.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA31SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  REG-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  TRRK-REC
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( REG-ERR-MAX         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-HSPTINFM01RES
                                       XML-HSPTINFM02RES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
           INITIALIZE                  SUID-AREA
           CALL    "orcuidset" USING   SUID-ST
                                       SUID-AREA
      *
      *    医療機関識別番号セット 
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "8099"       TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
      *
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "8901"      TO  WRK-ERRCD
               WHEN    "1002"
                   MOVE    "8902"      TO  WRK-ERRCD
               WHEN    "1003"
                   MOVE    "8903"      TO  WRK-ERRCD
               WHEN    "1005"
                   MOVE    "8905"      TO  WRK-ERRCD
               WHEN    "1015"
                   MOVE    "8915"      TO  WRK-ERRCD
               WHEN    OTHER
                   MOVE    "8900"      TO  WRK-ERRCD
               END-EVALUATE
      *
               PERFORM 990-ERROR-EDIT-SEC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-ERROR-EDIT-SEC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           EVALUATE    HSPTINFMREQ-REQNUM
           WHEN    "01"
               PERFORM 1001-NYUIN-SEC
           WHEN    "02"
               PERFORM 1001-TAIIN-SEC
           WHEN    "03"
               PERFORM 1001-HENKO-SEC
           WHEN    "05"
           WHEN    "06"
               PERFORM 1001-NYUIN-TORIKESHI-SEC
           WHEN    "07"
               PERFORM 1001-TAIIN-TORIKESHI-SEC
           WHEN    "08"
               PERFORM 1001-IDO-SEC
           WHEN    "09"
               PERFORM 1001-IDO-TORIKESHI-SEC
           WHEN    OTHER
               MOVE    "8003"          TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC         SECTION.
      *
           MOVE    HSPTINFMREQ-SAVEREQ  TO  REG-SAVEREQ
      *
           MOVE   HSPTINFMREQ-REQNUM   TO  REG-REQNUM
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           PERFORM 10011-NYUINYMD-CHECK-SEC
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( HSPTINFMREQ-PTNUM    =   SPACE )
               MOVE    "8004"           TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    HSPTINFMREQ-PTNUM   TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "8004"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "8004"          TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-IF
      *
           .
       10011-PTNUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       10011-NYUINYMD-CHECK-SEC       SECTION.
      *
           IF    ( HSPTINFMREQ-NYUINYMD =   SPACE )
               MOVE    "8001"           TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           ELSE
               MOVE    HSPTINFMREQ-NYUINYMD
                                      TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1    =   SPACE )
                   MOVE    "8001"     TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               ELSE
                   MOVE    WRK-SYMD   TO  REG-NYUINYMD
               END-IF
      *
               IF    (  HSPTINFMREQ-REQNUM  NOT =   "01" )
                   IF    (  REG-NYUINYMD    NOT =   SPACE )
                    AND  (  REG-NYUINYMD  <   CONST-H240401 )
                       MOVE    "8002"     TO  WRK-ERRCD
                       PERFORM 990-ERROR-EDIT-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
               IF    ( HSPTINFMREQ-REQNUM  NOT =   "01" )
      *
                   PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
                   MOVE    PTNYUINRRK-REC  TO  TRRK-REC
      *
                   MOVE    TRRK-NYUINYMD   TO  WRK-YMD
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   MOVE    -1              TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD   TO  WRK-SRYYMD
      *
                   PERFORM 900-PTNYUINRRK-KEY39-SEL-SEC
                   IF    ( FLG-PTNYUINRRK      =   ZERO )
                       IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" )
                        AND  ( PTNYUINRRK-JTIKBN   NOT =   "6" )
                        AND  ( PTNYUINRRK-NYUINYMD =   REG-NYUINYMD )
                           IF    ( PTNYUINRRK-RRKNUM
                                               NOT =   TRRK-RRKNUM )
                               MOVE    "8006"  TO  WRK-ERRCD
                               PERFORM 990-ERROR-EDIT-SEC
                           END-IF
                       ELSE
                           IF    ( PTNYUINRRK-JTIKBN   =   "6" )
                            AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                            AND  ( PTNYUINRRK-TAIINYMD =   WRK-SRYYMD )
                               CONTINUE
                           ELSE
                               MOVE    "8005"  TO  WRK-ERRCD
                               PERFORM 990-ERROR-EDIT-SEC
                           END-IF
                       END-IF
                   ELSE
                           MOVE    "8005"      TO  WRK-ERRCD
                           PERFORM 990-ERROR-EDIT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       10011-NYUINYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（入院登録時）
      *****************************************************************
       1001-NYUIN-SEC                  SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           MOVE    HSPTINFMREQ-BTUNUM  TO  REG-BTUNUM
      *
           MOVE    HSPTINFMREQ-BRMNUM  TO  REG-BRMNUM
      *
           PERFORM 10011-SAGAKU-CHECK-SEC
      *
           PERFORM VARYING  IDX0    FROM   1   BY  1
                  UNTIL  ( IDX0    >   3 )
               MOVE    HSPTINFMREQ-DR (IDX0)
                                       TO  REG-DR (IDX0)
           END-PERFORM
      *
           MOVE    HSPTINFMREQ-SRYKA   TO  REG-SRYKA
      *
           MOVE    HSPTINFMREQ-SHOKAIYMD
                                       TO  WRK-DATE
           PERFORM 800-SYMD-SEC
           MOVE    WRK-SYMD            TO  REG-SHOKAIYMD
      *
           MOVE    HSPTINFMREQ-KAIGOKBN TO REG-KAIGOKBN
      *
           PERFORM 10011-HOKEN-CHECK-SEC
      *
           IF    ( HSPTINFMREQ-KHNCD   NOT =   SPACE )
               MOVE    REG-NYUINYMD    TO  WRK-KJNYMD
               PERFORM 10011-KHNCD-CHECK-SEC
           END-IF
      *
           MOVE    HSPTINFMREQ-TEIKIKBN TO REG-TEIKIKBN
      *
           .
       1001-NYUIN-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（退院登録時）
      *****************************************************************
       1001-TAIIN-SEC                  SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           PERFORM 10011-TAIINYMD-CHECK-SEC
      *
           MOVE    HSPTINFMREQ-TAIINCD TO  REG-TAIINCD
      *
           .
       1001-TAIIN-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（変更時）
      *****************************************************************
       1001-HENKO-SEC                  SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           PERFORM VARYING  IDX0    FROM   1   BY  1
                  UNTIL  ( IDX0    >   3 )
               MOVE    HSPTINFMREQ-DR (IDX0)
                                       TO  REG-DR (IDX0)
           END-PERFORM
      *
           MOVE    HSPTINFMREQ-TEIKIKBN TO REG-TEIKIKBN
      *
           .
       1001-HENKO-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（入院取消）
      *****************************************************************
       1001-NYUIN-TORIKESHI-SEC        SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       1001-NYUIN-TORIKESHI-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（退院取消）
      *****************************************************************
       1001-TAIIN-TORIKESHI-SEC        SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       1001-TAIIN-TORIKESHI-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（転科転棟転室時）
      *****************************************************************
       1001-IDO-SEC                    SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           MOVE    HSPTINFMREQ-SAVEREQ  TO  REG-SAVEREQ
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           PERFORM 10011-IDOYMD-CHECK-SEC
      *
           MOVE    HSPTINFMREQ-BTUNUM   TO  REG-BTUNUM
      *
           MOVE    HSPTINFMREQ-BRMNUM   TO  REG-BRMNUM
      *
           PERFORM 10011-SAGAKU-CHECK-SEC
      *
           PERFORM VARYING  IDX0    FROM   1   BY  1
                  UNTIL  ( IDX0    >   3 )
               MOVE    HSPTINFMREQ-DR (IDX0)
                                       TO  REG-DR (IDX0)
           END-PERFORM
      *
           MOVE    HSPTINFMREQ-SRYKA    TO  REG-SRYKA
      *
           PERFORM 10011-HOKEN-CHECK-SEC
      *
           IF    ( HSPTINFMREQ-KHNCD   NOT =   SPACE )
               MOVE    REG-IDOYMD      TO  WRK-KJNYMD
               PERFORM 10011-KHNCD-CHECK-SEC
           END-IF
      *
           .
       1001-IDO-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（異動取消時）
      *****************************************************************
       1001-IDO-TORIKESHI-SEC          SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       1001-IDO-TORIKESHI-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       10011-SAGAKU-CHECK-SEC          SECTION.
      *
           MOVE    HSPTINFMREQ-SAGAKU   TO  REG-SAGAKU
      *
           MOVE    HSPTINFMREQ-SAGAKU   TO  WRK-SNUM-INX
           PERFORM 800-ORCSNUM-SEC
           IF    ( FLG-ORCSNUM     =   ZERO )
               MOVE    WRK-SNUM-OUTNUM TO  WRK-SAGAKU
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5113"          TO  SYS-KANRICD
               MOVE    REG-NYUINYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
      *
               PERFORM 900-SYSKANRI-KEY2-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
               PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                        OR   ( REG-SAGAKUCD    NOT =   SPACE )
      *
                   IF    ( WRK-SAGAKU  =   SYS-5113-BRM-SAGAKU-NM )
                       MOVE    SYS-5113-KBNCD
                                       TO  REG-SAGAKUCD
                   END-IF
      *
                   PERFORM 900-SYSKANRI-KEY2-FET-SEC
                   MOVE    SYSKANRI-REC TO SYS-5113-REC
      *
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       10011-SAGAKU-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    保険情報チェック処理
      *****************************************************************
       10011-HOKEN-CHECK-SEC           SECTION.
      *
           INITIALIZE                      ORCSAPIHKNAREA
      *
      *    保険存在チェック
           MOVE    "2"                 TO  ORCSAPIHKN-KBN
      *    保険種類
           MOVE    HSPTINFMREQ-HKNNUM   TO  ORCSAPIHKN-IN-HKNNUM
      *    保険者番号
           MOVE    HSPTINFMREQ-HKNJANUM
                                       TO  ORCSAPIHKN-IN-HKNJANUM
      *    保険種類
           MOVE    HSPTINFMREQ-HKNJANAME
                                       TO  ORCSAPIHKN-IN-HKNNUM-NAME
      *    記号
           MOVE    HSPTINFMREQ-HKIGO    TO  ORCSAPIHKN-IN-MARK
      *    番号
           MOVE    HSPTINFMREQ-HNUM     TO  ORCSAPIHKN-IN-NUMBER
      *    有効期間
           MOVE    HSPTINFMREQ-HTEKSTYMD
                                       TO  WRK-DATE
           PERFORM 800-DATE-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKSTYMD
      *
           MOVE    HSPTINFMREQ-HTEKEDYMD
                                       TO  WRK-DATE
           PERFORM 800-DATE-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKEDYMD
      *
      *    公費情報
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   4 )
      *
      *        公費種類
               MOVE    HSPTINFMREQ-KOHNUM  (IDX0)
                                       TO  ORCSAPIHKN-IN-KOHNUM  (IDX0)
      *        公費名称
               MOVE    HSPTINFMREQ-KOHNAME (IDX0)
                                       TO  ORCSAPIHKN-IN-KOHNUM-NAME
                                                                 (IDX0)
      *        負担者番号
               MOVE    HSPTINFMREQ-FTNJANUM(IDX0)
                                       TO  ORCSAPIHKN-IN-FTNJANUM(IDX0)
      *        受給者番号
               MOVE    HSPTINFMREQ-JKYSNUM (IDX0)
                                       TO  ORCSAPIHKN-IN-JKYSNUM (IDX0)
           END-PERFORM
      *
           MOVE    SPA-AREA            TO  TSPA-AREA
           MOVE    REG-NYUINYMD        TO  TSPA-SYSYMD
      *
           CALL    "ORCSAPIHKN"        USING
                                       TSPA-AREA
                                       ORCSAPIHKNAREA
      *
           MOVE    ORCSAPIHKN-OT-HKNCOMBI
                                       TO  REG-HKNCOMBI
      *
           .
       10011-HOKEN-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院料チェック処理
      *****************************************************************
       10011-KHNCD-CHECK-SEC           SECTION.
      *
           PERFORM 900-NYUINCDCHG-KEY2-SEL-SEC
      *
           INITIALIZE                      SNYUINRYO-AREA
           MOVE    WRK-KJNYMD          TO  SNYUINRYO-SRYYMD
           MOVE    "01"                TO  SNYUINRYO-SYORIKBN
           MOVE    REG-BTUNUM          TO  SNYUINRYO-BTUNUM
           MOVE    REG-BRMNUM          TO  SNYUINRYO-BRMNUM
           MOVE    REG-HKNCOMBI        TO  SNYUINRYO-HKNCOMBI
           CALL    "ORCSNYUINRYO"      USING
                                       SNYUINRYO-AREA
                                       SPA-AREA
           IF    ( NYUINCDCHG-NYUINRYOCD
                                       NOT =   SPACE )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SNYUINRYO-MAX )
                        OR   ( REG-NYUINRYOCD
                                   NOT =   SPACE )
                   IF    ( SNYUINRYO-NYUINRYO-G (IDX0)
                                       =   NYUINCDCHG-NYUINRYOCD )
                       MOVE    NYUINCDCHG-NYUINRYOCD
                                       TO  REG-NYUINRYOCD
                       MOVE    NYUINCDCHG-SRYCD-AF
                                       TO  REG-KHNCD
                   END-IF
               END-PERFORM
      *
      *    ELSE
      *        IF    ( NYUINCDCHG-SRYCD-AF NOT =   SPACE )
      *            MOVE    NYUINCDCHG-SRYCD-AF
      *                                TO  REG-KHNCD
      *        END-IF
           END-IF
      *
           IF    ( SNYUINRYO-BTU-KHNSRYCD  NOT =   SPACE )
            AND  ( SNYUINRYO-BTU-KHNSRYCD      =   REG-KHNCD )
               MOVE    SPACE           TO  REG-NYUINRYOCD
               MOVE    SPACE           TO  REG-KHNCD
           END-IF
      *
           .
       10011-KHNCD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    退院日チェック処理
      *****************************************************************
       10011-TAIINYMD-CHECK-SEC       SECTION.
      *
           IF    ( HSPTINFMREQ-TAIINYMD =   SPACE )
               MOVE    "8009"           TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           ELSE
               MOVE    HSPTINFMREQ-TAIINYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "8009"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               ELSE
                   MOVE    WRK-SYMD    TO  REG-TAIINYMD
               END-IF
           END-IF
      *
           .
       10011-TAIINYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    異動日チェック処理
      *****************************************************************
       10011-IDOYMD-CHECK-SEC       SECTION.
      *
           IF    ( HSPTINFMREQ-IDOYMD =   SPACE )
               MOVE    "8007"         TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           ELSE
               MOVE    HSPTINFMREQ-IDOYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "8007"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               ELSE
                   MOVE    WRK-SYMD    TO  REG-IDOYMD
               END-IF
      *
               IF    (  REG-IDOYMD    NOT =   SPACE )
                AND  (  REG-IDOYMD   <   CONST-H240401 )
                   MOVE    "8008"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               END-IF
      *
           END-IF
      *
           .
       10011-IDOYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           EVALUATE    HSPTINFMREQ-REQNUM
           WHEN    "01"
               PERFORM 2001-NYUIN-SEC
           WHEN    "02"
               PERFORM 2001-TAIIN-SEC
           WHEN    "03"
               PERFORM 2001-HENKO-SEC
           WHEN    "05"
           WHEN    "06"
               PERFORM 2001-NYUIN-TORIKESHI-SEC
           WHEN    "07"
               PERFORM 2001-TAIIN-TORIKESHI-SEC
           WHEN    "08"
               PERFORM 2001-IDO-SEC
           WHEN    "09"
               PERFORM 2001-IDO-TORIKESHI-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院登録処理
      *****************************************************************
       2001-NYUIN-SEC                 SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    REG-NYUINYMD        TO  SODR-SRYYMD
           MOVE    NORDERID-NYUIN2ID   TO  SODR-ORDERID
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SHOKAI-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BTUNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BRMNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               IF    ( REG-SAGAKU        NOT =   SPACE )
                   PERFORM 2300-SAGAKU-SET-SEC
               END-IF
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               IF    ( REG-NYUINRYOCD    NOT =   SPACE )
                   PERFORM 2300-TOKNYUIN-SET-SEC
               END-IF
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               IF    ( REG-TEIKIKBN      NOT =   SPACE )
                   PERFORM 2300-TEIKIKBN-SET-SEC
               END-IF
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               IF    ( REG-KAIGOKBN      NOT =   SPACE )
                   PERFORM 2300-KAIGOKBN-SET-SEC
               END-IF
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR01"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2001-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-NYUIN-EXT.
           EXIT.
      *****************************************************************
      *    退院登録時処理
      *****************************************************************
       2001-TAIIN-SEC                  SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    REG-TAIINYMD        TO  SODR-SRYYMD
           MOVE    NORDERID-TAIIN2ID   TO  SODR-ORDERID
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TAIINCD-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR02"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2002-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-TAIIN-EXT.
           EXIT.
      *****************************************************************
      *    変更時処理
      *****************************************************************
       2001-HENKO-SEC                  SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    TRRK-TENNYUYMD      TO  SODR-SRYYMD
           MOVE    NORDERID-HENKOID    TO  SODR-ORDERID
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               IF    ( REG-TEIKIKBN      NOT =   SPACE )
                   PERFORM 2300-TEIKIKBN-SET-SEC
               END-IF
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR05"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2001-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-HENKO-EXT.
           EXIT.
      *****************************************************************
      *    入院取消処理
      *****************************************************************
       2001-NYUIN-TORIKESHI-SEC        SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    SPA-SYSYMD          TO  SODR-SRYYMD
           IF    ( REG-REQNUM          =   "05" )
               MOVE    NORDERID-CANNYUINID
                                       TO  SODR-ORDERID
           ELSE
               MOVE    NORDERID-ALLCANNYUINID
                                       TO  SODR-ORDERID
           END-IF
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR07"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2003-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-NYUIN-TORIKESHI-EXT.
           EXIT.
      *****************************************************************
      *    退院取消処理
      *****************************************************************
       2001-TAIIN-TORIKESHI-SEC        SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    SPA-SYSYMD          TO  SODR-SRYYMD
           MOVE    NORDERID-CANTAIINID
                                       TO  SODR-ORDERID
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR08"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2003-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-TAIIN-TORIKESHI-EXT.
           EXIT.
      *****************************************************************
      *    転科転棟転室処理
      *****************************************************************
       2001-IDO-SEC                    SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    REG-IDOYMD          TO  SODR-SRYYMD
           MOVE    NORDERID-IDOALLID   TO  SODR-ORDERID
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BTUNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BRMNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               IF    ( REG-SAGAKU        NOT =   SPACE )
                   PERFORM 2300-SAGAKU-SET-SEC
               END-IF
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               IF    ( REG-NYUINRYOCD    NOT =   SPACE )
                   PERFORM 2300-TOKNYUIN-SET-SEC
               END-IF
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR06"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2001-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-IDO-EXT.
           EXIT.
      *****************************************************************
      *    異動取消処理
      *****************************************************************
       2001-IDO-TORIKESHI-SEC          SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    SPA-SYSYMD          TO  SODR-SRYYMD
           MOVE    NORDERID-CANIDOID   TO  SODR-ORDERID
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR09"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2001-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-IDO-TORIKESHI-EXT.
           EXIT.
      *****************************************************************
      *    初回設定処理
      *****************************************************************
       2300-SHOKAI-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-SHOKAIJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
           IF    ( REG-SHOKAIYMD   NOT =   SPACE )
               MOVE    "01"        TO  LNK-ODRITEM-ITEM (IDX-ITM)
               MOVE    REG-SHOKAIYMD
                                   TO  LNK-ODRITEM-MEMO (IDX-ITM)
           END-IF
      *
           .
       2300-SHOKAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号設定処理
      *****************************************************************
       2300-BTUNUM-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-BTUNUMJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-BTUNUM          TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-BTUNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    病室番号設定処理
      *****************************************************************
       2300-BRMNUM-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-BRMNUMJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-BRMNUM          TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-BRMNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額設定処理
      *****************************************************************
       2300-SAGAKU-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-SAGAKUJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-SAGAKUCD        TO  LNK-ODRITEM-ITEM (IDX-ITM)
           MOVE    REG-SAGAKU          TO  LNK-ODRITEM-MEMO (IDX-ITM)
      *
           .
       2300-SAGAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    担当医設定処理
      *****************************************************************
       2300-DR-SET-SEC                 SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-DRJMA      TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
           IF    ( REG-DR (IDX2)       NOT =   SPACE )
               IF    ( REG-DR (IDX2)(5:1)  =   SPACE )
                   MOVE    "1"     TO  LNK-ODRITEM-ITEM(IDX-ITM)(1:1)
                   MOVE    REG-DR (IDX2)
                                   TO  LNK-ODRITEM-ITEM(IDX-ITM)(2:)
               ELSE
                   MOVE    REG-DR (IDX2)
                                   TO  LNK-ODRITEM-ITEM(IDX-ITM)
               END-IF
           END-IF
      *
           .
       2300-DR-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料設定処理
      *****************************************************************
       2300-TOKNYUIN-SET-SEC           SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-TOKNYUINJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-NYUINRYOCD       TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-TOKNYUIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院科設定処理
      *****************************************************************
       2300-NYUINKA-SET-SEC            SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-NYUINKAJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-SRYKA            TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-NYUINKA-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       2300-HKNCOMBI-SET-SEC           SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-HKNCOMBIJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-HKNCOMBI    TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分設定処理
      *****************************************************************
       2300-TEIKIKBN-SET-SEC           SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-TEIKIKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-TEIKIKBN    TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-TEIKIKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    介護区分区分設定処理
      *****************************************************************
       2300-KAIGOKBN-SET-SEC           SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-KAIGOKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-KAIGOKBN    TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-KAIGOKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院事由設定処理
      *****************************************************************
       2300-TAIINCD-SET-SEC            SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-TAIINCDJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    REG-TAIINCD          TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-TAIINCD-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-SODR-COMMON-SET-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SODR-HOSPNUM
           MOVE    SUID-UID            TO  SODR-KARTE-KEY
           MOVE    1                   TO  SODR-ORDERNUM
           MOVE    "1"                 TO  SODR-NYUGAIKBN
           MOVE    SPA-PTNUM           TO  SODR-PTNUM
           MOVE    SPA-PTID            TO  SODR-PTID
           MOVE    REG-SRYKA           TO  SODR-SRYKA
           MOVE    REG-HKNCOMBI        TO  SODR-HKNCOMBI
           MOVE    SMCNDATE-YMD        TO  SODR-UKEYMD
           MOVE    SMCNDATE-HMS        TO  SODR-UKEHMS
           IF    ( REG-SAVEREQ         =   "1" )
               MOVE    "0"             TO  SODR-PROCKBN
           ELSE
               MOVE    "2"             TO  SODR-PROCKBN
           END-IF
           MOVE    1                   TO  SODR-KEYGENERATE
           MOVE    99                  TO  SODR-INF-DIALOG
           MOVE    REG-KHNCD           TO  SODR-KHNSRYCD
           MOVE    "1"                 TO  SODR-SRCKBN
      *
           MOVE    "api_"              TO  SODR-OPID
           MOVE    SPA-OPID            TO  SODR-OPID(5:)
           MOVE    SMCNDATE-YMD        TO  SODR-UPYMD
           MOVE    SMCNDATE-HMS        TO  SODR-UPHMS
      *
           .
       2300-SODR-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-ODRITEM-COMMON-SET-SEC       SECTION.
      *
           COMPUTE    IDX-ITM  =   IDX-ITM +   1
      *
           MOVE    SPA-HOSPNUM     TO  LNK-ODRITEM-HOSPNUM  (IDX-ITM)
           MOVE    SUID-UID        TO  LNK-ODRITEM-KARTE-KEY(IDX-ITM)
           MOVE    1               TO  LNK-ODRITEM-ORDERNUM(IDX-ITM)
           MOVE    IDX-ITM         TO  LNK-ODRITEM-ITEMNUM (IDX-ITM)
      *
           .
       2300-ODRITEM-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダーエラー編集処理
      *****************************************************************
       2400-ORDER-ERROR-EDIT-SEC         SECTION.
      *
           IF    ( SODR-ERRCD   NOT =  SPACE )
               MOVE    SODR-ERRCD       TO WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-IF
      *
           PERFORM VARYING IDX9    FROM    1   BY  1
                   UNTIL ( IDX9    >   LNK-ODRITEM-MAX )
      *
               MOVE    LNK-ODRITEM-OCC (IDX9)  TO  ODRITEM-REC
               IF    ( ODRITEM-ERRCD   NOT =   SPACE )
                   MOVE    ODRITEM-ERRCD
                                        TO WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
                END-IF
      *
           END-PERFORM
      *
           .
       2400-ORDER-ERROR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理（入院履歴情報）
      *****************************************************************
       2001-RESPONSE-EDIT-SEC          SECTION.
      *
           INITIALIZE                  SNINF01-AREA
           MOVE    SODR-RRKNUM     TO  SNINF01-IN-RRKNUM
           IF    ( REG-REQNUM      =   "09" )
               MOVE    TRRK-TENNYUYMD
                                   TO  SNINF01-IN-KJNYMD
           END-IF
           CALL    "ORCSNINF01"      USING
                                   SNINF01-AREA
                                   SPA-AREA
      *
           MOVE    SNINF01-PTNUM          TO  HSPTINFM01RES-PTNUM
           MOVE    SNINF01-NAME           TO  HSPTINFM01RES-NAME
           MOVE    SNINF01-KANANAME       TO  HSPTINFM01RES-KANANAME
           MOVE    SNINF01-BIRTHDAY       TO  HSPTINFM01RES-BIRTHDAY
           MOVE    SNINF01-SEX            TO  HSPTINFM01RES-SEX
      *
           MOVE    1                    TO  IDXA
      *
           MOVE    SNINF01-RRKNUM (IDXA)  TO  HSPTINFM01RES-RRKNUM
           MOVE    SNINF01-CREATEKBN-LBL (IDXA)
                                   TO  HSPTINFM01RES-CREATEKBN-LBL
           MOVE    SNINF01-CREATEKBN-DATA (IDXA)
                                   TO  HSPTINFM01RES-CREATEKBN-DATA
           MOVE    SNINF01-CREATEKBN-NAME (IDXA)
                                   TO  HSPTINFM01RES-CREATEKBN-NAME
      *
           MOVE    SNINF01-NYUINYMD (IDXA)
                                   TO  HSPTINFM01RES-NYUINYMD
      *
           IF    ( REG-REQNUM      =  "03" OR "08" OR "09" )
               MOVE    SNINF01-IDOYMD (IDXA)
                                   TO  HSPTINFM01RES-IDOYMD
           END-IF
      *
           MOVE    SNINF01-BTUNUM-LBL (IDXA)
                                   TO  HSPTINFM01RES-BTUNUM-LBL
           MOVE    SNINF01-BTUNUM-DATA (IDXA)
                                   TO  HSPTINFM01RES-BTUNUM-DATA
      *
           MOVE    SNINF01-BTUNAME-LBL (IDXA)
                                   TO  HSPTINFM01RES-BTUNAME-LBL
           MOVE    SNINF01-BTUNAME-DATA (IDXA)
                                   TO  HSPTINFM01RES-BTUNAME-DATA
      *
           MOVE    SNINF01-BRMNUM-LBL (IDXA)
                                   TO  HSPTINFM01RES-BRMNUM-LBL
           MOVE    SNINF01-BRMNUM-DATA (IDXA)
                                   TO  HSPTINFM01RES-BRMNUM-DATA
      *
           MOVE    SNINF01-SRYKA-LBL (IDXA)
                                   TO  HSPTINFM01RES-SRYKA-LBL
           MOVE    SNINF01-SRYKA-DATA (IDXA)
                                   TO  HSPTINFM01RES-SRYKA-DATA
           MOVE    SNINF01-SRYKA-NAME (IDXA)
                                   TO  HSPTINFM01RES-SRYKA-NAME
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   3 )
      *
               MOVE    SNINF01-DR-LBL  (IDXA IDXD)
                                   TO  HSPTINFM01RES-DR-LBL  (IDXD)
               MOVE    SNINF01-DR-DATA (IDXA IDXD)
                                   TO  HSPTINFM01RES-DR-DATA (IDXD)
               MOVE    SNINF01-DR-NAME (IDXA IDXD)
                                   TO  HSPTINFM01RES-DR-NAME (IDXD)
      *
           END-PERFORM
      *
           MOVE    SNINF01-HKNCOMBI (IDXA)
                                   TO  HSPTINFM01RES-HKNCOMBI
           MOVE    SNINF01-HKNNUM (IDXA)
                                   TO  HSPTINFM01RES-HKNNUM
           MOVE    SNINF01-HKNJANUM (IDXA)
                                   TO  HSPTINFM01RES-HKNJANUM
           MOVE    SNINF01-HKNJANAME (IDXA)
                                   TO  HSPTINFM01RES-HKNJANAME
           MOVE    SNINF01-HKIGO (IDXA)
                                   TO  HSPTINFM01RES-HKIGO
           MOVE    SNINF01-HNUM (IDXA)
                                   TO  HSPTINFM01RES-HNUM
      *
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   4 )
               MOVE    SNINF01-KOHNUM (IDXA IDXK)
                                   TO  HSPTINFM01RES-KOHNUM (IDXK)
               MOVE    SNINF01-KOHNAME (IDXA IDXK)
                                   TO  HSPTINFM01RES-KOHNAME (IDXK)
               MOVE    SNINF01-FTNJANUM (IDXA IDXK)
                                   TO  HSPTINFM01RES-FTNJANUM (IDXK)
               MOVE    SNINF01-JKYSNUM (IDXA IDXK)
                                   TO  HSPTINFM01RES-JKYSNUM (IDXK)
           END-PERFORM
      *
           MOVE    SNINF01-SHOKAIYMD (IDXA)
                                   TO HSPTINFM01RES-SHOKAIYMD
           MOVE    SNINF01-KAIGOKBN-LBL (IDXA)
                                   TO HSPTINFM01RES-KAIGOKBN-LBL
           MOVE    SNINF01-KAIGOKBN-DATA (IDXA)
                                   TO HSPTINFM01RES-KAIGOKBN-DATA
           MOVE    SNINF01-KAIGOKBN-NAME (IDXA)
                                   TO HSPTINFM01RES-KAIGOKBN-NAME
           MOVE    SNINF01-SAGAKU-LBL (IDXA)
                                   TO HSPTINFM01RES-SAGAKU-LBL
           MOVE    SNINF01-SAGAKU-DATA (IDXA)
                                   TO HSPTINFM01RES-SAGAKU-DATA
           MOVE    SNINF01-SAGAKU-NAME (IDXA)
                                   TO HSPTINFM01RES-SAGAKU-NAME
           MOVE    SNINF01-SENTEIKBN-LBL (IDXA)
                                   TO HSPTINFM01RES-SENTEIKBN-LBL
           MOVE    SNINF01-SENTEIKBN-DATA (IDXA)
                                   TO HSPTINFM01RES-SENTEIKBN-DATA
           MOVE    SNINF01-SENTEIKBN-NAME (IDXA)
                                   TO HSPTINFM01RES-SENTEIKBN-NAME
           MOVE    SNINF01-NYUINRYO-LBL (IDXA)
                                   TO HSPTINFM01RES-NYUINRYO-LBL
           MOVE    SNINF01-NYUINRYO-DATA (IDXA)
                                   TO HSPTINFM01RES-NYUINRYO-DATA
           MOVE    SNINF01-NYUINRYO-NAME (IDXA)
                                   TO HSPTINFM01RES-NYUINRYO-NAME
           IF    ( REG-REQNUM      =  "03" OR "08" OR "09" )
               MOVE    SNINF01-IDONYUINRYO-LBL (IDXA)
                                   TO HSPTINFM01RES-IDONYUINRYO-LBL
               MOVE    SNINF01-IDONYUINRYO-DATA (IDXA)
                                   TO HSPTINFM01RES-IDONYUINRYO-DATA
               MOVE    SNINF01-IDONYUINRYO-NAME (IDXA)
                                   TO HSPTINFM01RES-IDONYUINRYO-NAME
           END-IF
           MOVE    SNINF01-NACCTKBN-LBL (IDXA)
                                   TO HSPTINFM01RES-NACCTKBN-LBL
           MOVE    SNINF01-NACCTKBN-DATA (IDXA)
                                   TO HSPTINFM01RES-NACCTKBN-DATA
           MOVE    SNINF01-NACCTKBN-NAME (IDXA)
                                   TO HSPTINFM01RES-NACCTKBN-NAME
           MOVE    SNINF01-TEIKIKBN-LBL (IDXA)
                                   TO HSPTINFM01RES-TEIKIKBN-LBL
           MOVE    SNINF01-TEIKIKBN-DATA (IDXA)
                                   TO HSPTINFM01RES-TEIKIKBN-DATA
           MOVE    SNINF01-TEIKIKBN-NAME (IDXA)
                                   TO HSPTINFM01RES-TEIKIKBN-NAME
      *
           .
       2001-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理（請求情報）
      *****************************************************************
       2002-RESPONSE-EDIT-SEC          SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
      *
           MOVE    SODR-SKYSTYMD   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFM02RES-SKYSTYMD
      *
           MOVE    REG-TAIINYMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFM02RES-SKYEDYMD
      *
           MOVE    SPA-PTNUM       TO  HSPTINFM02RES-PTNUM
           MOVE    PTINF-NAME      TO  HSPTINFM02RES-NAME
           MOVE    PTINF-KANANAME  TO  HSPTINFM02RES-KANANAME
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFM02RES-BIRTHDAY
           MOVE    PTINF-SEX       TO  HSPTINFM02RES-SEX
      *
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  HSPTINFM02RES-RRKNUM
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFM02RES-NYUINYMD
      *
           INITIALIZE                  SNINF02-AREA
      *
           MOVE    SODR-SKYSTYMD   TO  SNINF02-IN-SKYSTYMD
           MOVE    REG-TAIINYMD    TO  SNINF02-IN-SKYEDYMD
      *
           PERFORM VARYING IDXS    FROM    1   BY  1
                   UNTIL ( IDXS    >   SODR-DENPNUM-MAX )
      *
               MOVE    SODR-DENPNUM (IDXS)
                                   TO  WRK-DENPNUM
               PERFORM 900-SYUNOU-KEY-SEL-SEC
               MOVE    SYUNOU-REC  TO  LNK-SYUNOU-REC (IDXS)
      *
           END-PERFORM
      *
           CALL    "ORCSNINF02"    USING
                                   SNINF02-AREA
                                   LNK-SYUNOU-REC-T
                                   SPA-AREA
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SNINF02-SKYINF-MAX )
      *
               MOVE    SNINF02-SKYINF-KBN-LBL (IDXA)
                               TO  HSPTINFM02RES-SKYINF-KBN-LBL (IDXA)
               MOVE    SNINF02-SKYINF-KBN-DATA (IDXA)
                               TO  HSPTINFM02RES-SKYINF-KBN-DATA (IDXA)
               MOVE    SNINF02-SKYINF-KBN-NAME (IDXA)
                               TO  HSPTINFM02RES-SKYINF-KBN-NAME (IDXA)
               MOVE    SNINF02-SRYYM (IDXA)
                               TO  HSPTINFM02RES-SRYYM (IDXA)
               MOVE    SNINF02-DENPNUM (IDXA)
                               TO  HSPTINFM02RES-DENPNUM (IDXA)
               MOVE    SNINF02-SRYKA (IDXA)
                               TO  HSPTINFM02RES-SRYKA (IDXA)
               MOVE    SNINF02-SRYKANAME (IDXA)
                               TO  HSPTINFM02RES-SRYKANAME (IDXA)
      *
               MOVE    SNINF02-HKNCOMBI (IDXA)
                               TO  HSPTINFM02RES-HKNCOMBI (IDXA)
               MOVE    SNINF02-HKNNUM (IDXA)
                               TO  HSPTINFM02RES-HKNNUM (IDXA)
               MOVE    SNINF02-HKNJANUM (IDXA)
                               TO  HSPTINFM02RES-HKNJANUM (IDXA)
               MOVE    SNINF02-HKNJANAME (IDXA)
                               TO  HSPTINFM02RES-HKNJANAME (IDXA)
               MOVE    SNINF02-HKIGO (IDXA)
                               TO  HSPTINFM02RES-HKIGO (IDXA)
               MOVE    SNINF02-HNUM (IDXA)
                               TO  HSPTINFM02RES-HNUM (IDXA)
      *
               PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   4 )
                   MOVE    SNINF02-KOHNUM (IDXA IDXK)
                               TO  HSPTINFM02RES-KOHNUM (IDXA IDXK)
                   MOVE    SNINF02-KOHNAME (IDXA IDXK)
                               TO  HSPTINFM02RES-KOHNAME (IDXA IDXK)
                   MOVE    SNINF02-FTNJANUM (IDXA IDXK)
                               TO  HSPTINFM02RES-FTNJANUM (IDXA IDXK)
                   MOVE    SNINF02-JKYSNUM (IDXA IDXK)
                               TO  HSPTINFM02RES-JKYSNUM (IDXA IDXK)
               END-PERFORM
      *
               MOVE    SNINF02-SKYMONEY (IDXA)
                               TO  HSPTINFM02RES-SKYMONEY (IDXA)
               MOVE    SNINF02-HKNMONEY (IDXA)
                               TO  HSPTINFM02RES-HKNMONEY (IDXA)
               MOVE    SNINF02-JIHIMONEY (IDXA)
                               TO  HSPTINFM02RES-JIHIMONEY (IDXA)
               MOVE    SNINF02-RJNMONEY (IDXA)
                               TO  HSPTINFM02RES-RJNMONEY (IDXA)
               MOVE    SNINF02-KOHMONEY (IDXA)
                               TO  HSPTINFM02RES-KOHMONEY (IDXA)
               MOVE    SNINF02-SKJMONEY (IDXA)
                               TO  HSPTINFM02RES-SKJMONEY (IDXA)
               MOVE    SNINF02-RSIMONEY (IDXA)
                               TO  HSPTINFM02RES-RSIMONEY (IDXA)
               MOVE    SNINF02-GENMEN (IDXA)
                               TO  HSPTINFM02RES-GENMEN (IDXA)
               MOVE    SNINF02-TOTAL-TEN (IDXA)
                               TO  HSPTINFM02RES-TOTAL-TEN (IDXA)
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   16 )
                   MOVE    SNINF02-SRYKBN-NAME (IDXA IDX0)
                               TO  HSPTINFM02RES-SRYKBN-NAME (IDXA IDX0)
                   MOVE    SNINF02-SRYKBN-TENSU (IDXA IDX0)
                               TO  HSPTINFM02RES-SRYKBN-TENSU(IDXA IDX0)
               END-PERFORM
      *
               MOVE    SNINF02-SKJRYOYO (IDXA)
                                   TO  HSPTINFM02RES-SKJRYOYO (IDXA)
      *
           END-PERFORM
      *
           .
       2002-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理（患者情報）
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPA-PTNUM       TO  HSPTINFM01RES-PTNUM
           MOVE    PTINF-NAME      TO  HSPTINFM01RES-NAME
           MOVE    PTINF-KANANAME  TO  HSPTINFM01RES-KANANAME
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFM01RES-BIRTHDAY
           MOVE    PTINF-SEX       TO  HSPTINFM01RES-SEX
           MOVE    TRRK-RRKNUM     TO  HSPTINFM01RES-RRKNUM
           MOVE    TRRK-NYUINYMD   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFM01RES-NYUINYMD
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFM01RES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  HSPTINFM01RES-INFORMATION-TIME
      *
           IF    ( REG-ERR-MAX     =   ZERO )
               MOVE    1           TO  REG-ERR-MAX
               MOVE    "0000"      TO  REG-ERRCD (1)
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   REG-ERR-MAX )
      *
               MOVE    REG-ERRCD  (IDX1)
                                       TO  WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD
                               TO  HSPTINFM01RES-API-RESULT     (IDX1)
               MOVE    WRK-ERRMSG
                               TO  HSPTINFM01RES-API-RESULT-MSG (IDX1)
      *
           END-PERFORM
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           PERFORM VARYING IDXE    FROM   1   BY  1
                   UNTIL ( IDXE    >   NCLAIMERRCD-MAX )
                    OR   ( WRK-ERRMSG  NOT =   SPACE )
      *
               IF    ( NCLAIMERRCDL (IDXE) =   WRK-ERRCD )
                   MOVE    NCLAIMERRCDLMEI2  (IDXE)
                                       TO  WRK-ERRMSG
               END-IF
      *
           END-PERFORM
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( I0API01-BODY     NOT =   LOW-VALUE )
               DISPLAY "hsptinfmodv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "xml_hsptinfmodv2req"   TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           MOVE    I0API01-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               MOVE    "xml_hsptinfmodv2req"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "private_objects"   TO  APIREQ-RECORDNAME
               MOVE    "XMLREAD"       TO  MCP-FUNC
               PERFORM 910-XMLREAD-SEC
               IF    ( MCP-RC              =   ZERO  OR  1 )
                   PERFORM 9001-XML-FILTER-SEC
               ELSE
                   DISPLAY "XMLREAD failure = " MCP-RC
                   MOVE   "8098"           TO  WRK-ERRCD
               END-IF
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "8097"                TO  WRK-ERRCD
           END-IF
      *
           MOVE    "xml_hsptinfmodv2req"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  HSPTINFMREQ-PRIVATE-OBJECTS
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    HSPTINFM01RES-INFORMATION-DATE
                               TO  HSPTINFM02RES-INFORMATION-DATE
      *
           MOVE    HSPTINFM01RES-INFORMATION-TIME
                               TO  HSPTINFM02RES-INFORMATION-TIME
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   REG-ERR-MAX )
      *
               MOVE    HSPTINFM01RES-API-RESULT     (IDX1)
                               TO  HSPTINFM02RES-API-RESULT (IDX1)
               MOVE    HSPTINFM01RES-API-RESULT-MSG (IDX1)
                               TO  HSPTINFM02RES-API-RESULT-MSG (IDX1)
      *
           END-PERFORM
      *
           MOVE    "リクエスト番号"
                               TO  HSPTINFM01RES-REQNUM-LBL
      *
           MOVE    HSPTINFMREQ-REQNUM
                               TO  HSPTINFM01RES-REQNUM-DATA
      *
           EVALUATE    HSPTINFMREQ-REQNUM
           WHEN    "01"
               MOVE    "入院登録"
                               TO  HSPTINFM01RES-REQNUM-NAME
           WHEN    "02"
               MOVE    "退院登録"
                               TO  HSPTINFM01RES-REQNUM-NAME
           WHEN    "03"
               MOVE    "変更"
                               TO  HSPTINFM01RES-REQNUM-NAME
           WHEN    "05"
               MOVE    "入院取消"
                               TO  HSPTINFM01RES-REQNUM-NAME
           WHEN    "06"
               MOVE    "入院取消（会計含む）"
                               TO  HSPTINFM01RES-REQNUM-NAME
           WHEN    "07"
               MOVE    "退院取消"
                               TO  HSPTINFM01RES-REQNUM-NAME
           WHEN    "08"
               MOVE    "転科転棟転室"
                               TO  HSPTINFM01RES-REQNUM-NAME
           WHEN    "09"
               MOVE    "異動取消"
                               TO  HSPTINFM01RES-REQNUM-NAME
           END-EVALUATE
      *
           MOVE    HSPTINFM01RES-REQNUM-LBL
                               TO  HSPTINFM02RES-REQNUM-LBL
           MOVE    HSPTINFM01RES-REQNUM-DATA
                               TO  HSPTINFM02RES-REQNUM-DATA
           MOVE    HSPTINFM01RES-REQNUM-NAME
                               TO  HSPTINFM02RES-REQNUM-NAME
      *
           EVALUATE    HSPTINFMREQ-REQNUM
           WHEN    "02"
               PERFORM 9002-XML-WRITE-SEC
           WHEN    OTHER
               PERFORM 9001-XML-WRITE-SEC
           END-EVALUATE
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       9001-XML-WRITE-SEC             SECTION.
      *
           IF    ( I0API01-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsptinfmodv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsptinfmod01v2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    HSPTINFM01RES-PRIVATE-OBJECTS
                                           TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_hsptinfmod01v2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "xml_hsptinfmod01v2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  I0API01-BODY
               MOVE    "application/xml"   TO  I0API01-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
           END-IF
      *
           .
       9001-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       9002-XML-WRITE-SEC             SECTION.
      *
           IF    ( I0API01-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsptinfmodv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsptinfmod02v2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    HSPTINFM02RES-PRIVATE-OBJECTS
                                           TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_hsptinfmod02v2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "xml_hsptinfmod02v2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  I0API01-BODY
               MOVE    "application/xml"   TO  I0API01-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
           END-IF
      *
           .
       9002-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY3-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY39-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    REG-NYUINYMD        TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY39-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院料置換検索処理
      *****************************************************************
       900-NYUINCDCHG-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINCDCHG
      *
           INITIALIZE                      NYUINCDCHG-REC
           MOVE    HSPTINFMREQ-KHNCD   TO  NYUINCDCHG-SRYCD-BF
           MOVE    REG-NYUINYMD        TO  NYUINCDCHG-YUKOSTYMD
                                           NYUINCDCHG-YUKOEDYMD
           MOVE    NYUINCDCHG-REC      TO  MCPDATA-REC
           MOVE    "tbl_nyuincdchg"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINCDCHG-REC
           ELSE
               MOVE    1               TO  FLG-NYUINCDCHG
               INITIALIZE                  NYUINCDCHG-REC
           END-IF
      *
           MOVE    "tbl_nyuincdchg"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-NYUINCDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
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
       910-DBFETCH-SEC                 SECTION.
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
      *    ＸＭＬオープン処理
      *****************************************************************
       910-XMLOPEN-SEC                 SECTION.
      *
           MOVE    "XMLOPEN"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬクローズ処理
      *****************************************************************
       910-XMLCLOSE-SEC                SECTION.
      *
           MOVE    "XMLCLOSE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー編集処理
      *****************************************************************
       990-ERROR-EDIT-SEC              SECTION.
      *
           IF    ( REG-ERR-MAX     <   CONST-ERR-MAX )
      *
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   CONST-ERR-MAX )
                        OR   ( REG-ERRCD  (IDX8)
                                       =   WRK-ERRCD )
      *
                   IF    ( REG-ERRCD  (IDX8)   =   SPACE )
                       MOVE    WRK-ERRCD       TO  REG-ERRCD  (IDX8)
                       MOVE    IDX8            TO  REG-ERR-MAX
                       MOVE    CONST-ERR-MAX   TO  IDX8
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
           .
       990-ERROR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFM01RES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  HSPTINFM01RES-INFORMATION-TIME
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   REG-ERR-MAX )
      *
               MOVE    REG-ERRCD  (IDX1)
                                       TO  WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD
                               TO  HSPTINFM01RES-API-RESULT     (IDX1)
               MOVE    WRK-ERRMSG
                               TO  HSPTINFM01RES-API-RESULT-MSG (IDX1)
      *
           END-PERFORM
      *
           PERFORM 900-XML-WRITE-SEC
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
