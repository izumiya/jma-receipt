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
       PROGRAM-ID.         ORAPI031R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院患者基本取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/10/15    NACL−太田　新規作成
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
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-NYUINACT            PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SENTEICDCHG         PIC 9(01).
           03  FLG-SENTEI              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDXK                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
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
           03  WRK-DRCD                PIC X(05)
                                       OCCURS 3.
           03  WRK-NUM7Z               PIC ZZZZZZ9.
           03  WRK-KJNYMD              PIC X(08).
      *
           03  WRK-NENREI              PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1            PIC 9(04).
               05  WRK-MMS1            PIC 9(02).
               05  WRK-DDS1            PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2            PIC 9(04).
               05  WRK-MMS2            PIC 9(02).
               05  WRK-DDS2            PIC 9(02).
           03  WRK-NYUINRYO-G.
               05  WRK-NYUINRYO-CD     PIC X(09).
               05  WRK-NYUINRYO-NAME   PIC X(100).
      *
       01  KEY-AREA.
           03  KEY-NYUINYMD            PIC X(08).
           03  KEY-SHONUM              PIC 9(03).
           03  KEY-SRYCD               PIC X(09).
      *
      *
       01  CONST-AREA.
           03  CONST-H211001           PIC X(08)   VALUE "20091001".
           03  CONST-RRK-MAX           PIC 9(03)   VALUE 100.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //TRRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    特定入院料コード取得パラメタ
           COPY    "CPORCSTOKNYUIN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
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
      *    API XML読み込み用定義体
           COPY    "CPHSPTINFV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSPTINFV2RES.INC".
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
      *    職員情報
           COPY    "CPSK1010.INC".
      *    病棟
           COPY    "CPSK5001.INC".
      *    病室
           COPY    "CPSK5002.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    室料差額
           COPY    "CPSK5113.INC".
      *    退院事由
           COPY    "CPSK5013.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    選定入院料コード置換
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
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
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
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
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( WRK-ERRCD          =   SPACE )
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
           INITIALIZE                  XML-HSPTINFRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット 
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "99"         TO  WRK-ERRCD
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
               MOVE   "89"         TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           INITIALIZE                      ORCSNTYMDAREA
           MOVE    SPA-HOSPNUM         TO  SNTYMD-HOSPNUM
           MOVE    SPA-PTID            TO  SNTYMD-PTID
           MOVE    "1"                 TO  SNTYMD-KBN
           CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                           SPA-AREA
           IF  ( SNTYMD-RC         NOT =   ZERO )
               MOVE    "03"            TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( KEY-NYUINYMD    NOT =   SPACE )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0        >   SNTYMD-MAX )
                        OR   ( KEY-SHONUM  NOT =   ZERO )
      *
                   IF    ( SNTYMD-NYUINYMD (IDX0)  =   KEY-NYUINYMD )
                       IF    ( KEY-SHONUM  =   ZERO )
                               MOVE    SNTYMD-SHONUM (IDX0)
                                           TO  KEY-SHONUM
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( KEY-SHONUM      =   ZERO )
                   MOVE    "04"            TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
           END-IF
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
           PERFORM 10011-NYUINYMD-CHECK-SEC
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       10011-NYUINYMD-CHECK-SEC       SECTION.
      *
           IF    ( HSPTINFREQ-NYUINYMD =   SPACE )
               CONTINUE
           ELSE
               MOVE    HSPTINFREQ-NYUINYMD
                                           TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    WRK-SYMD            TO  KEY-NYUINYMD
      *
           END-IF
      *
           .
       10011-NYUINYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( HSPTINFREQ-PTNUM   =   SPACE )
               MOVE    "02"            TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    HSPTINFREQ-PTNUM   TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "02"        TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "02"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       10011-PTNUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-PTINF-EDIT-SEC
      *
           PERFORM 2001-NYUINRRK-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者報編集処理
      *****************************************************************
       2001-PTINF-EDIT-SEC                 SECTION.
      *
           MOVE    SPA-PTNUM           TO  HSPTINFRES-PTNUM
           MOVE    PTINF-NAME          TO  HSPTINFRES-NAME
           MOVE    PTINF-KANANAME      TO  HSPTINFRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSPTINFRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  HSPTINFRES-SEX
      *
          .
       2001-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報編集処理
      *****************************************************************
       2001-NYUINRRK-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDXA
      *
           PERFORM 900-PTNYUINRRK-KEY59-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( IDXA               >=   CONST-RRK-MAX )
      *
               IF    ( PTNYUINRRK-JTIKBN       =   "5" OR "6" )
                   IF   ( PTNYUINRRK-JTIKBN       =   "6" )
                    AND ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                       IF    ( IDXA        >=  1 )
                           MOVE    PTNYUINRRK-NYUINYMD
                                               TO  WRK-SYMD
                           PERFORM 800-DATE-SEC
                           MOVE    WRK-DATE    TO  HSPTINFRES-NYUINYMD
                                                              (IDXA)
                       END-IF
                   ELSE
                       PERFORM 20012-NYUINRRK-EDIT-SEC
                   END-IF
               ELSE
                   PERFORM 20011-NYUINRRK-EDIT-SEC
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY59-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
          .
       2001-NYUINRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴情報編集処理（通常登録分）
      *****************************************************************
       20011-NYUINRRK-EDIT-SEC                SECTION.
      *
           COMPUTE IDXA    =   IDXA    +   1
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5002"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BRMNUM   TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5002-REC
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-NYUINKA  TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           MOVE    "入院歴作成区分"
                               TO  HSPTINFRES-CREATEKBN-LBL (IDXA)
           MOVE    "0"         TO  HSPTINFRES-CREATEKBN-DATA(IDXA)
           MOVE    "通常登録"  TO  HSPTINFRES-CREATEKBN-NAME(IDXA)
      *
           MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE    TO  HSPTINFRES-NYUINYMD (IDXA)
      *
           MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE    TO  HSPTINFRES-TAIINYMD (IDXA)
      *
           IF    ( PTNYUINRRK-TAIINCD  NOT =   SPACE )
               MOVE    "退院事由"      TO  HSPTINFRES-TAIINCD-LBL (IDXA)
               MOVE    PTNYUINRRK-TAIINCD
                                       TO  HSPTINFRES-TAIINCD-DATA(IDXA)
      *
               INITIALIZE                          SYSKANRI-REC
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    "5013"                  TO  SYS-KANRICD
               MOVE    PTNYUINRRK-TAIINCD      TO  SYS-KBNCD
               MOVE    PTNYUINRRK-TENNYUYMD    TO  SYS-STYUKYMD
                                                   SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5013-REC
               MOVE    SYS-5013-NAME   TO  HSPTINFRES-TAIINCD-NAME(IDXA)
           END-IF
      *
           MOVE    "病棟番号"          TO  HSPTINFRES-BTUNUM-LBL (IDXA)
           MOVE    PTNYUINRRK-BTUNUM   TO  HSPTINFRES-BTUNUM-DATA(IDXA)
      *
           MOVE    "病棟名"            TO  HSPTINFRES-BTUNAME-LBL(IDXA)
           MOVE    SYS-5001-BTU-NAME
                                   TO  HSPTINFRES-BTUNAME-DATA (IDXA)
      *
           MOVE    "病室番号"      TO  HSPTINFRES-BRMNUM-LBL   (IDXA)
           MOVE    SYS-5002-BRM-NUM
                                   TO  HSPTINFRES-BRMNUM-DATA(IDXA)
      *
           MOVE    "診療科"        TO  HSPTINFRES-SRYKA-LBL  (IDXA)
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  HSPTINFRES-SRYKA-DATA (IDXA)
           MOVE    SYS-1005-SRYKANAME
                                   TO  HSPTINFRES-SRYKA-NAME (IDXA)
      *
           MOVE    PTNYUINRRK-DRCD1    TO  WRK-DRCD (1)
           MOVE    PTNYUINRRK-DRCD2    TO  WRK-DRCD (2)
           MOVE    PTNYUINRRK-DRCD3    TO  WRK-DRCD (3)
      *
           MOVE    ZERO            TO  IDXB
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   3 )
      *
               IF    ( WRK-DRCD (IDX0) NOT =   SPACE )
                   COMPUTE IDXB    =   IDXB    +   1
                   MOVE    "担当医"    TO  HSPTINFRES-DR-LBL (IDXA IDXB)
                   MOVE    WRK-DRCD (IDX0) 
                                       TO  HSPTINFRES-DR-DATA(IDXA IDXB)
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM TO  SYS-HOSPNUM
                   MOVE    "1010"      TO  SYS-KANRICD
                   MOVE    WRK-DRCD (IDX0)
                                       TO  SYS-KBNCD
                   MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC
                                       TO  SYS-1010-REC
                   MOVE    SYS-1010-NAME
                                       TO  HSPTINFRES-DR-NAME(IDXA IDXB)
               END-IF
      *
           END-PERFORM
      *
      *    保険組合せ
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  HSPTINFRES-HKNCOMBI (IDXA)
      *
           MOVE    SPA-AREA            TO  TSPA-AREA
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SPA-SYSYMD
           INITIALIZE                  ORCSAPIHKNAREA
           MOVE    "2"             TO  ORCSAPIHKN-KBN2
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           MOVE    TSPA-AREA   TO      SPA-AREA
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   20 )
                     OR  ( ORCSAPIHKN-THKNCOMBI (IDX1)
                                       =   SPACE  )
               IF    ( ORCSAPIHKN-THKNCOMBI (IDX1)
                                       =   PTNYUINRRK-HKNCOMBINUM )
                   PERFORM 200111-HKN-EDIT-SEC
                   MOVE    20      TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           PERFORM 200111-SHOKAIYMD-EDIT-SEC
      *
           MOVE    "介護からの異動"
                                   TO  HSPTINFRES-KAIGOKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-KAIGOKBN
                                   TO  HSPTINFRES-KAIGOKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-KAIGOKBN
           WHEN    "1"
               MOVE    "対象外"
                                   TO  HSPTINFRES-KAIGOKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "急性増悪により"
                                   TO  HSPTINFRES-KAIGOKBN-NAME(IDXA)
           END-EVALUATE
      *
           IF    ( PTNYUINRRK-RM-SAGAKU    NOT =   SPACE )
               INITIALIZE              SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5113"          TO  SYS-KANRICD
               MOVE    PTNYUINRRK-RM-SAGAKU
                                       TO  SYS-KBNCD
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    "室料差額"  TO  HSPTINFRES-SAGAKU-LBL  (IDXA)
                   MOVE    "円"        TO  HSPTINFRES-SAGAKU-NAME (IDXA)
                   MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  WRK-NUM7Z
                   MOVE    WRK-NUM7Z   TO  HSPTINFRES-SAGAKU-DATA (IDXA)
               END-IF
           END-IF
      *
           MOVE    "選定入院"  TO  HSPTINFRES-SENTEIKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-SENTEIKBN
                               TO  HSPTINFRES-SENTEIKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-SENTEIKBN
           WHEN    "1"
               MOVE    "選定対象"
                               TO  HSPTINFRES-SENTEIKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "選定対象外"
                               TO  HSPTINFRES-SENTEIKBN-NAME(IDXA)
           END-EVALUATE
      *
      *    MOVE    "入院料"    TO  HSPTINFRES-NYUINRYO-LBL (IDXA)
      *
           PERFORM 200111-NYUINRYO-EDIT-SEC
           IF    ( WRK-NYUINRYO-CD NOT =   SPACE )
               MOVE    "入院日の入院料"
                               TO  HSPTINFRES-NYUINRYO-LBL (IDXA)
               MOVE    WRK-NYUINRYO-CD 
                               TO  HSPTINFRES-NYUINRYO-DATA(IDXA)
               MOVE    WRK-NYUINRYO-NAME
                               TO  HSPTINFRES-NYUINRYO-NAME(IDXA)
           END-IF
      *    IF    ( PTNYUINRRK-TOKUTEI-KBN  NOT =   SPACE )
      *        INITIALIZE                  ORCSTOKNYUIN-AREA
      *        MOVE    PTNYUINRRK-TOKUTEI-KBN
      *                                 TO  LNK-TOKNYUIN-KBN
      *        MOVE    PTNYUINRRK-TOKUTEI-NYUIN
      *                                TO  LNK-TOKNYUIN-NYUIN
      *        MOVE    PTNYUINRRK-TENNYUYMD(1:6)
      *                                TO  LNK-TOKNYUIN-SANTEIYM
      *        MOVE    PTNYUINRRK-BTUNUM
      *                                TO  LNK-TOKNYUIN-BTUNUM
      *        CALL    "ORCSTOKNYUIN"  USING
      *                                ORCSTOKNYUIN-AREA
      *                                SPA-AREA
      *        MOVE    LNK-TOKNYUIN-SRYCD
      *                            TO  HSPTINFRES-NYUINRYO-DATA (IDXA)
      *        MOVE    LNK-TOKNYUIN-SRYCD
      *                            TO  KEY-SRYCD
      *        MOVE    PTNYUINRRK-TENNYUYMD
      *                            TO  WRK-KJNYMD
      *        PERFORM 900-TENSU-KEY-SEL-SEC
      *        MOVE    TNS-NAME    TO  HSPTINFRES-NYUINRYO-NAME (IDXA)
      *    ELSE
      *        EVALUATE    SYS-5001-BTU-KHNSRYCD
      *        WHEN    "099999950"
      *            MOVE    "190097010" TO  SYS-5001-BTU-KHNSRYCD
      *        WHEN    "099999951"
      *            MOVE    "190097210" TO  SYS-5001-BTU-KHNSRYCD
      *        WHEN    "099999952"
      *            MOVE    "190135110" TO  SYS-5001-BTU-KHNSRYCD
      *        END-EVALUATE
      *        MOVE    SYS-5001-BTU-KHNSRYCD
      *                            TO  HSPTINFRES-NYUINRYO-DATA (IDXA)
      *        MOVE    SYS-5001-BTU-KHNSRYCD
      *                            TO  KEY-SRYCD
      *        MOVE    PTNYUINRRK-TENNYUYMD
      *                            TO  WRK-KJNYMD
      *        PERFORM 900-TENSU-KEY-SEL-SEC
      *        MOVE    TNS-NAME    TO  HSPTINFRES-NYUINRYO-NAME (IDXA)
      *    END-IF
      *
           MOVE    "入院会計"      TO  HSPTINFRES-NACCTKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-NYUCALKBN
                                   TO  HSPTINFRES-NACCTKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-NYUCALKBN
           WHEN    "1"
               MOVE    "入院料を算定しない"
                                   TO  HSPTINFRES-NACCTKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "入院料を算定する"
                                   TO  HSPTINFRES-NACCTKBN-NAME(IDXA)
           END-EVALUATE
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN      =   ZERO )
      *
               MOVE    "分娩区分"  TO  HSPTINFRES-BUNBENKBN-LBL(IDXA)
               MOVE    BUNBEN-BUNBENKBN
                                   TO  HSPTINFRES-BUNBENKBN-DATA(IDXA)
               EVALUATE    BUNBEN-BUNBENKBN
               WHEN    "0"
                   MOVE    "分娩入院でない"
                                   TO  HSPTINFRES-BUNBENKBN-NAME(IDXA)
               WHEN    "1"
                   MOVE    "正常分娩"
                                   TO  HSPTINFRES-BUNBENKBN-NAME(IDXA)
               WHEN    "2"
                   MOVE    "異常分娩"
                                   TO  HSPTINFRES-BUNBENKBN-NAME(IDXA)
               END-EVALUATE
      *
               IF    ( BUNBEN-BUNBENKBN    =   "1" OR "2" )
                AND  ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
                AND  ( PTNYUINRRK-TAIINYMD    >=   CONST-H211001 )
                   MOVE    "直接支払制度"
                                   TO  HSPTINFRES-RIYOUKBN-LBL (IDXA)
                   MOVE    BUNBEN-RIYOUKBN
                                   TO  HSPTINFRES-RIYOUKBN-DATA(IDXA)
                   EVALUATE    BUNBEN-RIYOUKBN
                   WHEN    "0"
                       MOVE    "利用しない"
                                   TO  HSPTINFRES-RIYOUKBN-NAME(IDXA)
                   WHEN    "1"
                       MOVE    "利用する"
                                   TO  HSPTINFRES-RIYOUKBN-NAME(IDXA)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    "定期請求"      TO  HSPTINFRES-TEIKIKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-TEIKI-SEIKYUKBN
                                   TO  HSPTINFRES-TEIKIKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-TEIKI-SEIKYUKBN
           WHEN    "1"
               MOVE    "医療機関での設定"
                                   TO  HSPTINFRES-TEIKIKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "月末時のみ請求"
                                   TO  HSPTINFRES-TEIKIKBN-NAME(IDXA)
           WHEN    "3"
               MOVE    "定期請求しない"
                                   TO  HSPTINFRES-TEIKIKBN-NAME(IDXA)
           END-EVALUATE
      *
          .
       20011-NYUINRRK-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       200111-HKN-EDIT-SEC              SECTION.
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX1)
                                   TO  HSPTINFRES-HKNNUM   (IDXA)
      *
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX1)
                                   TO  HSPTINFRES-HKNJANUM (IDXA)
      *
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX1)
                                   TO  HSPTINFRES-HKNJANAME(IDXA)
      *
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX1)
                                   TO  HSPTINFRES-HKIGO (IDXA)
      *
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX1)
                                   TO  HSPTINFRES-HNUM  (IDXA)
      *
      *    公費情報
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   4 )
      *
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX1 IDXK)
                                   TO  HSPTINFRES-KOHNUM  (IDXA IDXK)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX1 IDXK)
                                   TO  HSPTINFRES-KOHNAME (IDXA IDXK)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX1 IDXK)
                                   TO  HSPTINFRES-FTNJANUM(IDXA IDXK)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX1 IDXK)
                                   TO  HSPTINFRES-JKYSNUM (IDXA IDXK)
           END-PERFORM
      *
          .
       200111-HKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    初回入院日編集処理
      *****************************************************************
       200111-SHOKAIYMD-EDIT-SEC       SECTION.
      *
           IF    ( PTNYUINRRK-SHOKAIKBN    =   "1" )
               MOVE    PTNYUINRRK-NYUINYMD
                                       TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  HSPTINFRES-SHOKAIYMD (IDXA)
           ELSE
               MOVE    PTNYUINRRK-REC  TO  TRRK-REC
               PERFORM 900-PTNYUINRRK-KEY30-SEL-SEC
               MOVE    TRRK-NYUINYMD   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  HSPTINFRES-SHOKAIYMD (IDXA)
           END-IF
      *
          .
       200111-SHOKAIYMD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       200111-NYUINRYO-EDIT-SEC            SECTION.
      *
           MOVE    PTNYUINRRK-NYUINYMD     TO  WRK-SRYYMD
           MOVE    ZERO                    TO  FLG-SENTEI
      *
           INITIALIZE                      WRK-NYUINRYO-G
      *
           PERFORM 900-NYUINACCT-KEY7-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-ZAISKBKBN     =   "1" )
                AND  ( NACCT-DAY (WRK-SRYYMD-DD)
                                           >   ZERO )
                   IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                       MOVE    1           TO  FLG-SENTEI
                   ELSE
                       PERFORM 2001111-NYUINRYO-EDIT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY7-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-NYUINRYO-CD NOT =   SPACE )
               IF    ( FLG-SENTEI      =   1 )
                   MOVE    WRK-NYUINRYO-CD 
                                       TO  KEY-SRYCD
                   PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
                   IF    ( FLG-SENTEICDCHG =   ZERO )
                       MOVE    SENCHG-SENTEISRYCD
                                           TO  KEY-SRYCD
                       MOVE    WRK-SRYYMD  TO  WRK-KJNYMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-SRYCD   TO  WRK-NYUINRYO-CD
                       MOVE    TNS-NAME    TO  WRK-NYUINRYO-NAME
                   END-IF
               END-IF
           END-IF
      *
          .
       200111-NYUINRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       2001111-NYUINRYO-EDIT-SEC            SECTION.
      *
           PERFORM 900-NYUINACT-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACT    NOT =   ZERO )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   5 )
      *
                   IF    ( NSRY-SRYCD (IDX0)   NOT =   SPACE )
                       MOVE    NSRY-SRYCD (IDX0)
                                           TO  KEY-SRYCD
                       MOVE    WRK-SRYYMD  TO  WRK-KJNYMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( TNS-DATAKBN =   1 )
                           MOVE    TNS-SRYCD
                                           TO  WRK-NYUINRYO-CD
                           MOVE    TNS-NAME
                                           TO  WRK-NYUINRYO-NAME
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACT-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinact"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
          .
       2001111-NYUINRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴情報編集処理（歴作成分）
      *****************************************************************
       20012-NYUINRRK-EDIT-SEC                SECTION.
      *
           COMPUTE IDXA    =   IDXA    +   1
      *
           MOVE    "入院歴作成区分"
                                   TO  HSPTINFRES-CREATEKBN-LBL (IDXA)
           IF    ( PTNYUINRRK-JTIKBN   =   "5" )
               IF    ( PTNYUINRRK-TAINRELKBN   =   "1" )
                   MOVE    "3"     TO  HSPTINFRES-CREATEKBN-DATA(IDXA)
                   MOVE    "他院歴（特別な関係にある医療機関）"
                                   TO  HSPTINFRES-CREATEKBN-NAME(IDXA)
               ELSE
                   MOVE    "2"     TO  HSPTINFRES-CREATEKBN-DATA(IDXA)
                   MOVE    "他院歴"
                                   TO  HSPTINFRES-CREATEKBN-NAME(IDXA)
               END-IF
           ELSE
                   MOVE    "1"     TO  HSPTINFRES-CREATEKBN-DATA(IDXA)
                   MOVE    "自院歴"
                                   TO  HSPTINFRES-CREATEKBN-NAME(IDXA)
           END-IF
      *
           MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFRES-NYUINYMD (IDXA)
      *
           MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSPTINFRES-TAIINYMD (IDXA)
      *
           MOVE    "病棟名"        TO  HSPTINFRES-BTUNAME-LBL (IDXA)
           MOVE    PTNYUINRRK-BTUNAME
                                   TO  HSPTINFRES-BTUNAME-DATA(IDXA)
      *
           PERFORM 200111-SHOKAIYMD-EDIT-SEC
      *
          .
       20012-NYUINRRK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSPTINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSPTINFRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "00"            TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSPTINFRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSPTINFRES-API-RESULT-MSG
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "00"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "01"
               MOVE    "診療年月の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "患者番号の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "入退院情報の取得に失敗しました"
                                       TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "指定された入院日の入院情報が存在しません"
                                       TO  WRK-ERRMSG
      *共通エラー
           WHEN    "89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                                               DELIMITED  BY  SIZE
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                   INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                       TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "92"
               STRING  "診療年月は平成２０年（２００８年）"
                                       DELIMITED BY SIZE
                       "４月以降"
                                       DELIMITED BY SIZE
                       "を指定してください"
                                       DELIMITED BY SIZE
               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "97"
               MOVE   "送信内容に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "送信内容の読込ができませんでした"
                                       TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤが未登録です"
                                       TO  WRK-ERRMSG
           WHEN    OTHER
               MOVE    "返却情報の編集でエラーが発生しました"
                                       TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST17-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsptinfv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "xml_hsptinfv2req"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST17-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               MOVE    "xml_hsptinfv2req"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "private_objects"   TO  APIREQ-RECORDNAME
               MOVE    "XMLREAD"       TO  MCP-FUNC
               PERFORM 910-XMLREAD-SEC
               IF    ( MCP-RC              =   ZERO  OR  1 )
                   PERFORM 9001-XML-FILTER-SEC
               ELSE
                   DISPLAY "XMLREAD failure = " MCP-RC
                   MOVE   "98"             TO  WRK-ERRCD
               END-IF
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"                 TO  WRK-ERRCD
           END-IF
      *
           MOVE    "xml_hsptinfv2req"   TO  MCP-TABLE
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
                                       TO  HSPTINFREQ-PRIVATE-OBJECTS
      *
           IF    ( HSPTINFREQ-PTNUM    =   LOW-VALUE )
               MOVE    SPACE           TO  HSPTINFREQ-PTNUM
           END-IF
      *
           IF    ( HSPTINFREQ-NYUINYMD =   LOW-VALUE )
               MOVE    SPACE           TO  HSPTINFREQ-NYUINYMD
           END-IF
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           IF    ( APILST17-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsptinfv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsptinfv2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    HSPTINFRES-PRIVATE-OBJECTS TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_hsptinfv2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "xml_hsptinfv2res"       TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST17-BODY
               MOVE    "application/xml"   TO  APILST17-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
           END-IF
      *
           .
       900-XML-WRITE-EXT.
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
       900-PTNYUINRRK-KEY30-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    TRRK-REC            TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               MOVE    "00000000"      TO  TRRK-NYUINYMD
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY30-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY59-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
      *
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-PTNYUINRRK-KEY59-FET-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY59-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY59-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
                   IF    ( KEY-SHONUM  NOT =   ZERO )
                       IF    ( PTNYUINRRK-SHONUM
                                           =   KEY-SHONUM )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
                   INITIALIZE                  PTNYUINRRK-REC
               END-IF
      *
           END-PERFORM
      *
           .
       900-PTNYUINRRK-KEY59-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    KEY-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-KJNYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY7-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    WRK-SRYYMD-YM       TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       900-NYUINACCT-KEY7-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為検索処理
      *****************************************************************
       900-NYUINACT-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
           INITIALIZE                      NYUINACT-REC
      *
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為検索処理
      *****************************************************************
       900-NYUINACT-KEY2-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    選定療養コード置換検索処理
      *****************************************************************
       900-SENTEICDCHG-KEY2-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SENTEICDCHG
      *
           INITIALIZE                      SENTEICDCHG-REC
           MOVE    KEY-SRYCD           TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
           ELSE
               MOVE    1               TO  FLG-SENTEICDCHG
               INITIALIZE                  SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SENTEICDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-PTID
                                   TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
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
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSPTINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSPTINFRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSPTINFRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSPTINFRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
