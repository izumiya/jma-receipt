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
       PROGRAM-ID.         ORAPI031RFDV2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院食事情報取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/09/03    NACL−太田　新規作成
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
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXH                    PIC 9(05).
           03  IDXN                    PIC 9(05).
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
           03  WRK-ZAISKBKBN           PIC X(01).
           03  WRK-NYUIN-DAY-G.
               05  WRK-NYUIN-DAY       PIC 9(01)
                                       OCCURS  31.
           03  WRK-NYUINRYO-DAY-G.
               05  WRK-NYUINRYO-DAY    PIC 9(01)
                                       OCCURS  31.
           03  WRK-PTSTATUS-DATA.
               05  WRK-PTSTATUS-N      PIC 9(02).
           03  WRK-DAY                 PIC 9(03).
           03  WRK-SKJ.
               05  WRK-SKJ-N           PIC 9(02).
           03  WRK-SKJ-NAME            PIC X(30).
           03  WRK-NUM7Z               PIC ZZZZZZ9.
           03  WRK-CALENDAR-MAX        PIC 9(03).
      *
       01  KEY-AREA.
           03  KEY-SRYYM               PIC X(06).
      *
       01  TCOMB-AREA.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT//
                                       BY          //TCOMB//.
      *
       01  TNACCT-AREA.
         02    TNACCT-OCC                OCCURS    4.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT//
                                       BY          //TNACCT//.
      *
       01  TGAIHAKU-AREA.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT//
                                       BY          //TGAIHAKU//.
      *
       01  CONST-AREA.
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
           03  CONST-ASA               PIC 9(03)   VALUE 1.
           03  CONST-HIRU              PIC 9(03)   VALUE 2.
           03  CONST-YU                PIC 9(03)   VALUE 3.
           03  CONST-GAIHAKU           PIC 9(03)   VALUE 4.
           03  CONST-HKN-MAX           PIC 9(03)   VALUE 10.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
      *
      *    科別・保険別入院会計編集サブ
           COPY    "CPORCSS009.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPHSFOODV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSFOODV2RES.INC".
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
      *    入院基本
           COPY    "CPSK5000.INC".
      *    病棟
           COPY    "CPSK5001.INC".
      *    病室
           COPY    "CPSK5002.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    室料差額
           COPY    "CPSK5113.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院診療会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
           INITIALIZE                  TCOMB-AREA
           INITIALIZE                  TNACCT-AREA
           INITIALIZE                  TGAIHAKU-AREA
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
           INITIALIZE                  XML-HSFOODRES
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
           END-IF
      *
           IF    ( WRK-ERRCD          =   SPACE )
      *        ＳＰＡ共通設定
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
               END-IF
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           IF    ( WRK-ERRCD           =   SPACE )
               INITIALIZE                      ORCSNTYMDAREA
               MOVE    SPA-HOSPNUM         TO  SNTYMD-HOSPNUM
               MOVE    PTINF-PTID          TO  SNTYMD-PTID
               MOVE    "2"                 TO  SNTYMD-KBN
               CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                               SPA-AREA
               IF  ( SNTYMD-RC             =   ZERO )
                   IF  ( SNTYMD-MAX        =   ZERO )
                       MOVE    "50"        TO  WRK-ERRCD
                   END-IF
               ELSE
                   MOVE    "03"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
      *
               MOVE    KEY-SRYYM   TO  WRK-SRYYMD-YM
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    IDXD    TO  WRK-SRYYMD-DD
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   SNTYMD-MAX )
                       IF    ( WRK-SRYYMD  >=  SNTYMD-NYUINYMD (IDX0))
                        AND  ( WRK-SRYYMD  <=  SNTYMD-TAIINYMD (IDX0))
                           MOVE    1   TO  WRK-NYUIN-DAY (IDXD)
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
               MOVE    "5"                 TO  WRK-ZAISKBKBN
               PERFORM 900-NYUINACCT-KEY17-SEL-SEC
               IF    ( FLG-NYUINACCT   =   ZERO )
                   MOVE    NYUINACCT-REC   TO  TCOMB-AREA
               ELSE
                   MOVE    "04"            TO  WRK-ERRCD
               END-IF
      *
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
      *
               MOVE    "1"                 TO  WRK-ZAISKBKBN
               PERFORM 901-NYUINACCT-KEY17-SEL-SEC
      *
               PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
                   IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                       CONTINUE
                   ELSE
                       PERFORM VARYING     IDX1    FROM    1   BY  1
                               UNTIL     ( IDX1    >   31 )
      *
                           IF   ( NACCT-DAY(IDX1)  >   ZERO )
                               MOVE    1   TO  WRK-NYUINRYO-DAY (IDX1)
                           END-IF
      *
                       END-PERFORM
                   END-IF
      *
                   PERFORM 901-NYUINACCT-KEY17-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               INITIALIZE                      ORCSNYUINDAY-AREA
               MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
               MOVE    SPA-PTID            TO  LNK-NYUINDAY-PTID
               MOVE    KEY-SRYYM           TO  LNK-NYUINDAY-SANTEIYM
               CALL    "ORCSNYUINDAY"          USING
                                               ORCSNYUINDAY-AREA
                                               SPA-AREA
               IF    ( LNK-NYUINDAY-RC     NOT =   ZERO )
                   INITIALIZE                  ORCSNYUINDAY-AREA
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
           PERFORM 10011-SRYYM-CHECK-SEC
           MOVE    KEY-SRYYM       TO  SPA-SRYYMD
           MOVE    "01"            TO  SPA-SRYYMD (7:2)
      *
           IF    ( WRK-ERRCD       =   SPACE )
               PERFORM 10011-PTNUM-CHECK-SEC
           END-IF
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       10011-SRYYM-CHECK-SEC       SECTION.
      *
           IF    ( HSFDREQ-SRYYM   =   SPACE )
               MOVE    SPA-SYSYMD (1:6)
                                   TO  KEY-SRYYM
           ELSE
               MOVE    HSFDREQ-SRYYM
                                   TO  WRK-DATE
               MOVE    "-01"       TO  WRK-DATE (8:3)
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "01"    TO  WRK-ERRCD
               ELSE
                   MOVE    WRK-SYMD (1:6)
                                   TO  KEY-SRYYM
               END-IF
           END-IF
      *
           IF    ( WRK-ERRCD       =   SPACE )
               IF    ( KEY-SRYYM   <   CONST-H200401 (1:6))
                   MOVE    "92"    TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       10011-SRYYM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( HSFDREQ-PTNUM   =   SPACE )
               MOVE    "02"            TO  WRK-ERRCD
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    HSFDREQ-PTNUM   TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "02"        TO  WRK-ERRCD
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF       NOT =   ZERO )
                   MOVE    "02"        TO  WRK-ERRCD
               END-IF
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
           IF    ( WRK-ERRCD       =   SPACE )
               PERFORM 2001-NACCT-EDIT-SEC
           END-IF
      *
           IF    ( WRK-ERRCD       =   SPACE )
               PERFORM 2001-HKN-EDIT-SEC
           END-IF
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
           MOVE    SPA-PTNUM           TO  HSFDRES-PTNUM
           MOVE    PTINF-NAME          TO  HSFDRES-NAME
           MOVE    PTINF-KANANAME      TO  HSFDRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSFDRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  HSFDRES-SEX
      *
          .
       2001-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報編集処理
      *****************************************************************
       2001-NACCT-EDIT-SEC                 SECTION.
      *
           MOVE    ZERO            TO      IDXA
           PERFORM VARYING IDX0    FROM    SNTYMD-MAX   BY  -1
                   UNTIL ( IDXA    >   5 )
                    OR   ( IDX0    <   1)
      *
               IF    ( KEY-SRYYM   >=  SNTYMD-NYUINYMD (IDX0)(1:6))
                AND  ( KEY-SRYYM   <=  SNTYMD-TAIINYMD (IDX0)(1:6))
                   COMPUTE IDXA    =   IDXA    +   1
                   MOVE    SNTYMD-NYUINYMD (IDX0)
                                   TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  HSFDRES-NYUINYMD (IDXA)
      *
                   MOVE    SNTYMD-TAIINYMD (IDX0)
                                       TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  HSFDRES-TAIINYMD (IDXA)
               END-IF
      *
           END-PERFORM
      *
           MOVE    KEY-SRYYM           TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE (1:7)      TO  HSFDRES-SRYYM
      *
           PERFORM 2001-SKJ-GAIHAKU-EDIT-SEC
      *
           MOVE    ZERO            TO  IDXA
      *
           INITIALIZE                  SS009-AREA
           MOVE    "1"             TO  SS009-EXGAIHAKU
           CALL    "ORCSS009"  USING   SS009-AREA
                                       TCOMB-AREA
                                       SPA-AREA
           IF    ( SS009-RC        =   ZERO )
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   IF    ( WRK-NYUIN-DAY (IDXD)    >   ZERO )
                    AND  ( TCOMB-DAY     (IDXD)    >   ZERO )
                       PERFORM 20011-NACCT-EDIT-SEC
                   END-IF
               END-PERFORM
           ELSE
               MOVE    "05"       TO  WRK-ERRCD
           END-IF
      *
          .
       2001-NACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報編集処理
      *****************************************************************
       20011-NACCT-EDIT-SEC                SECTION.
      *
           COMPUTE IDXA    =   IDXA    +   1
      *
           MOVE    IDXA            TO  WRK-CALENDAR-MAX
      *
           MOVE    KEY-SRYYM       TO  WRK-SRYYMD-YM
           MOVE    IDXD            TO  WRK-SRYYMD-DD
      *
           MOVE    WRK-SRYYMD      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSFDRES-SRYYMD (IDXA)
      *
           MOVE    "診療科"        TO  HSFDRES-SRYKA-LBL  (IDXA)
           MOVE    SS009-SRYKA-DAY (IDXD)
                                   TO  HSFDRES-SRYKA-DATA (IDXA)
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    SS009-SRYKA-DAY (IDXD)
                                   TO  SYS-KBNCD
           MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME
                                   TO  HSFDRES-SRYKA-NAME (IDXA)
      *
           MOVE    "病棟番号"      TO  HSFDRES-BTUNUM-LBL (IDXA)
           MOVE    SS009-BRMNUM-DAY(IDXD)(1:2)
                                   TO  HSFDRES-BTUNUM-DATA(IDXA)
      *
           MOVE    "病棟名"        TO  HSFDRES-BTUNAME-LBL(IDXA)
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SS009-BRMNUM-DAY(IDXD)(1:2)
                                   TO  SYS-KBNCD
           MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
           MOVE    SYS-5001-BTU-NAME
                                   TO  HSFDRES-BTUNAME-DATA (IDXA)
      *
           MOVE    "病室番号"      TO  HSFDRES-BRMNUM-LBL   (IDXA)
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    SS009-BRMNUM-DAY(IDXD)
                                   TO  SYS-KBNCD
           MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5002-REC
           MOVE    SYS-5002-BRM-NUM
                                   TO  HSFDRES-BRMNUM-DATA  (IDXA)
      *
           MOVE    SS009-HKNCOMBI-DAY (IDXD)
                                   TO  HSFDRES-HKNCOMBI (IDXA)
      *
           MOVE    "外泊・他医療機関受診情報等"
                                   TO  HSFDRES-PTSTATUS-LBL (IDXA)
           MOVE    TNACCT-DAY (CONST-GAIHAKU IDXD)
                                   TO  WRK-PTSTATUS-N
           MOVE    WRK-PTSTATUS-N  TO  HSFDRES-PTSTATUS-DATA (IDXA)
           EVALUATE    TNACCT-DAY (CONST-GAIHAKU IDXD)
           WHEN    ZERO
               MOVE    "入院中"        TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    1
               MOVE    "外泊"          TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    2
               MOVE    "治療の為の外泊"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    3
               MOVE    "他医療機関受診７０％減"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    4
               MOVE    "他医療機関受診７０％減"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    5
               MOVE    "他医療機関受診３０％減"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    6
               MOVE    "他医療機関受診５５％減"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    7
               MOVE    "他医療機関受診１５％減"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    8
               MOVE    "特定時間退院減算"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    9
               MOVE    "特定曜日入退院減算"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           WHEN    13
               MOVE    "特定曜日入退院減算＋他医療機関受診３０％減"
                                       TO  HSFDRES-PTSTATUS-NAME (IDXA)
           END-EVALUATE
      *
           MOVE    "朝食"              TO  HSFDRES-SKJ-ASA-LBL   (IDXA)
           MOVE    TNACCT-DAY (CONST-ASA IDXD)
                                       TO  WRK-SKJ-N
           MOVE    WRK-SKJ-N           TO  HSFDRES-SKJ-ASA-DATA  (IDXA)
           MOVE    TNACCT-DAY (CONST-ASA IDXD)
                                       TO  WRK-DAY
           PERFORM 200111-SKJ-EDIT-SEC
           MOVE    WRK-SKJ-NAME        TO  HSFDRES-SKJ-ASA-NAME  (IDXA)
      *
           MOVE    "昼食"              TO  HSFDRES-SKJ-HIRU-LBL  (IDXA)
           MOVE    TNACCT-DAY (CONST-HIRU IDXD)
                                       TO  WRK-SKJ-N
           MOVE    WRK-SKJ-N           TO  HSFDRES-SKJ-HIRU-DATA (IDXA)
           MOVE    TNACCT-DAY (CONST-HIRU IDXD)
                                       TO  WRK-DAY
           PERFORM 200111-SKJ-EDIT-SEC
           MOVE    WRK-SKJ-NAME        TO  HSFDRES-SKJ-HIRU-NAME (IDXA)
      *
           MOVE    "夕食"              TO  HSFDRES-SKJ-YU-LBL   (IDXA)
           MOVE    TNACCT-DAY (CONST-YU IDXD)
                                       TO  WRK-SKJ-N
           MOVE    WRK-SKJ-N           TO  HSFDRES-SKJ-YU-DATA  (IDXA)
           MOVE    TNACCT-DAY (CONST-YU IDXD)
                                       TO  WRK-DAY
           PERFORM 200111-SKJ-EDIT-SEC
           MOVE    WRK-SKJ-NAME        TO  HSFDRES-SKJ-YU-NAME  (IDXA)
      *
           MOVE    "室料差額"          TO  HSFDRES-SAGAKU-LBL   (IDXA)
           MOVE    "円"                TO  HSFDRES-SAGAKU-NAME  (IDXA)
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5113"              TO  SYS-KANRICD
           MOVE    SS009-SAGAKUCD-DAY (IDXD)
                                       TO  SYS-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5113-REC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z       TO  HSFDRES-SAGAKU-DATA (IDXA)
           ELSE
               MOVE    ZERO            TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z       TO  HSFDRES-SAGAKU-DATA (IDXA)
           END-IF
      *
          .
       20011-NACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    食事外泊情報編集処理
      *****************************************************************
       2001-SKJ-GAIHAKU-EDIT-SEC       SECTION.
      *
           PERFORM 900-NYUINACCT-KEY51-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               MOVE    ZERO                TO  IDXN
      *
               EVALUATE    NACCT-ZAISKBKBN
               WHEN    "2"
                   MOVE    CONST-GAIHAKU   TO  IDXN
               WHEN    "4"
                   EVALUATE    NACCT-SRYCDTOTAL
                   WHEN    099999918
                       MOVE    CONST-ASA   TO  IDXN
                   WHEN    099999919
                       MOVE    CONST-HIRU  TO  IDXN
                   WHEN    099999920
                       MOVE    CONST-YU    TO  IDXN
                   END-EVALUATE
               END-EVALUATE
      *
               IF    ( IDXN            >   ZERO )
                   MOVE    NYUINACCT-REC   TO  TNACCT-OCC (IDXN)
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY51-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    TNACCT-OCC (CONST-GAIHAKU)
                                   TO  TGAIHAKU-AREA
           MOVE    ZERO            TO  TNACCT-ZAIKAISU (CONST-ASA)
                                       TNACCT-ZAIKAISU (CONST-HIRU)
                                       TNACCT-ZAIKAISU (CONST-YU)
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    CONST-ASA   TO  IDXN
               PERFORM 20011-SKJ-GAIHAKU-EDIT-SEC
      *
               MOVE    CONST-HIRU  TO  IDXN
               PERFORM 20011-SKJ-GAIHAKU-EDIT-SEC
      *
               MOVE    CONST-YU    TO  IDXN
               PERFORM 20011-SKJ-GAIHAKU-EDIT-SEC
      *
           END-PERFORM
      *
          .
       2001-SKJ-GAIHAKU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    食事外泊情報編集処理
      *****************************************************************
       20011-SKJ-GAIHAKU-EDIT-SEC          SECTION.
      *
           IF    ( TGAIHAKU-DAY (IDXD)     =   ZERO )
             OR  ( TGAIHAKU-DAY (IDXD)    >=   4 )
               IF    ( TNACCT-DAY (IDXN IDXD)      >   ZERO )
                   COMPUTE TNACCT-ZAIKAISU (IDXN)
                       =   TNACCT-ZAIKAISU (IDXN)  +   1
               END-IF
           ELSE
               MOVE    ZERO    TO  TNACCT-DAY (IDXN IDXD)
           END-IF
      *
          .
       20011-SKJ-GAIHAKU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    食事情報編集処理
      *****************************************************************
       200111-SKJ-EDIT-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-SKJ-NAME
      *
           EVALUATE    WRK-DAY
           WHEN    ZERO
               MOVE    "食事なし"      TO  WRK-SKJ-NAME
           WHEN    1
               MOVE    "食事あり"      TO  WRK-SKJ-NAME
           WHEN    2
               MOVE    "食事あり（特別食）"
                                       TO  WRK-SKJ-NAME
           END-EVALUATE
      *
          .
       200111-SKJ-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       2001-HKN-EDIT-SEC               SECTION.
      *
           MOVE    ZERO                TO  IDXH
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   WRK-CALENDAR-MAX )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-HKN-MAX )
                        OR   ( IDXH    >=  CONST-HKN-MAX )
      *
                   IF    ( IDX0        >   IDXH )
                       COMPUTE IDXH    =   IDXH    +   1
                       MOVE    HSFDRES-HKNCOMBI (IDXA)
                                       TO  HSFDRES-HKNCOMBI-KEY (IDXH)
                       MOVE    CONST-HKN-MAX
                                       TO  IDX0
                   ELSE
                       IF   (  HSFDRES-HKNCOMBI (IDXA)
                                        =  HSFDRES-HKNCOMBI-KEY (IDX0))
                           MOVE    CONST-HKN-MAX
                                       TO  IDX0
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    SPA-AREA    TO  TSPA-AREA
           MOVE    SPA-SRYYMD  TO  SPA-SYSYMD
           INITIALIZE              ORCSAPIHKNAREA
           MOVE    "2"         TO  ORCSAPIHKN-KBN2
           CALL    "ORCSAPIHKN"    USING
                                   SPA-AREA
                                   ORCSAPIHKNAREA
           MOVE    TSPA-AREA   TO  SPA-AREA
      *
           PERFORM VARYING IDXH    FROM    1   BY  1
                   UNTIL ( IDXH    >   CONST-HKN-MAX )
                    OR   ( HSFDRES-HKNCOMBI-KEY (IDXH)
                                   =   SPACE )
               PERFORM VARYING IDXA    FROM    1   BY  1
                       UNTIL ( IDXA    >   20 )
                         OR  ( ORCSAPIHKN-THKNCOMBI (IDXA)
                                       =   SPACE  )
                   IF    ( HSFDRES-HKNCOMBI-KEY (IDXH)
                                       =   ORCSAPIHKN-THKNCOMBI (IDXA))
                       PERFORM 20011-HKN-EDIT-SEC
                       MOVE    20      TO  IDXA
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
          .
       2001-HKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       20011-HKN-EDIT-SEC              SECTION.
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDXA)
                                   TO  HSFDRES-HKNNUM   (IDXH)
      *
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDXA)
                                   TO  HSFDRES-HKNJANUM (IDXH)
      *
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDXA)
                                   TO  HSFDRES-HKNJANAME(IDXH)
      *
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDXA)
                                   TO  HSFDRES-HKIGO (IDXH)
      *
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDXA)
                                   TO  HSFDRES-HNUM  (IDXH)
      *
      *    公費情報
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   4 )
      *
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDXA IDXK)
                                   TO  HSFDRES-KOHNUM  (IDXH IDXK)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDXA IDXK)
                                   TO  HSFDRES-KOHNAME (IDXH IDXK)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDXA IDXK)
                                   TO  HSFDRES-FTNJANUM(IDXH IDXK)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDXA IDXK)
                                   TO  HSFDRES-JKYSNUM (IDXH IDXK)
           END-PERFORM
      *
          .
       20011-HKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSFDRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSFDRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "00"            TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSFDRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSFDRES-API-RESULT-MSG
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
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
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
           WHEN    "05"
               MOVE    "入院会計情報の取得に失敗しました"
                                       TO  WRK-ERRMSG
           WHEN    "50"
               MOVE    "診療年月にかかる入退院情報はありません"
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
           IF    ( APILST14-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsfoodv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "xml_hsfoodv2req"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST14-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               MOVE    "xml_hsfoodv2req"
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
           MOVE    "xml_hsfoodv2req"   TO  MCP-TABLE
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
                                       TO  HSFDREQ-PRIVATE-OBJECTS
      *
           IF    ( HSFDREQ-PTNUM   =   LOW-VALUE )
               MOVE    SPACE           TO  HSFDREQ-PTNUM
           END-IF
      *
           IF    ( HSFDREQ-SRYYM   =   LOW-VALUE )
               MOVE    SPACE           TO  HSFDREQ-SRYYM
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
           IF    ( APILST14-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsfoodv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsfoodv2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    HSFDRES-PRIVATE-OBJECTS TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_hsfoodv2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "xml_hsfoodv2res"       TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST14-BODY
               MOVE    "application/xml"   TO  APILST14-CONTENT-TYPE
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
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    KEY-SRYYM           TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    WRK-ZAISKBKBN       TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       901-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    KEY-SRYYM           TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    WRK-ZAISKBKBN       TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       901-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       901-NYUINACCT-KEY17-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       901-NYUINACCT-KEY17-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY51-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    KEY-SRYYM           TO  NACCT-SRYYM
                                           NACCT-CREYMD
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY51-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       900-NYUINACCT-KEY51-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY51-FET-EXT.
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
