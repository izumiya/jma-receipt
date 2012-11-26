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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSCHKTALL.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 入院料包括診療行為チェック処理（月）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/12/18    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-HKTSANTEI           PIC 9(01).
           03  FLG-HKTSRYCD            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
           03  FLG-NYUINACT           PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
      *
           03  FLG-CHK                 PIC 9(01).
           03  FLG-CHK2                PIC 9(01).
           03  FLG-CHK3                PIC 9(01).
           03  FLG-HKTCHK              PIC 9(01).
           03  FLG-HOUKATU             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-N                   PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-S                   PIC 9(04).
           03  IDX-H                   PIC 9(04).
           03  IDX-T                   PIC 9(04).
           03  IDX-SRY                 PIC 9(04).
      *
           03  IDX-D                   PIC 9(02).
           03  IDD                     PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *
           03  WRK-IDX-N               PIC 9(04).
      *
           03  WRK-K-SRYCD             PIC X(09).
      *    包括
           03  WRK-NYUHKT-AREA.
               05  WRK-NYUHKT-OCCURS   OCCURS   31.
                   07  WRK-NYUHKTCD        PIC X(03).
                   07  WRK-NYUSRYCD        PIC X(09).
      *    チェック用
       01  WRK-TBL-AREA.
           03  WRK-KANRICD             PIC  X(03).
      *
      *入院料
       01  TBL-NYUAREA.
           03  TBL-NYUDATA-G              OCCURS   100.
               05  TBL-N-KANRICD             PIC  X(03).
               05  TBL-N-SANTEI-HOU          PIC  X(01).
               05  TBL-N-SANTEI-KBN          PIC  X(01).
               05  TBL-N-NYUGAIKBN           PIC  X(01).
               05  TBL-N-HKNCOMBIKBN         PIC  X(01).
               05  TBL-N-SRYKBN-G.
                   07  TBL-N-SRYKBN-KBN      PIC X(01)
                                             OCCURS   50.
               05  TBL-N-SRYCD-G.
                   07  TBL-N-SRYCD           PIC X(09)
                                           OCCURS   99.
           03  TBL-N-MAX                 PIC 9(04).
      *
           03  TBL-HKTCHKFLG-G.
               05  TBL-HKTCHKFLG-OCC        OCCURS   31.
                   07  TBL-HKTCHKFLG        PIC 9(01).
      *
       01  TBL-HKTSANTEI-AREA.
           03  TBL-HKTSANTEI-DATA-G        OCCURS   600.
               05  TBL-HKTSRYCD            PIC X(09).
               05  TBL-SANTEIKBN           PIC X(01).
               05  TBL-KENTAIKBN           PIC X(01).
      *
      *    出来高算定日
       01  TBL-DEKIDAKA-AREA.
           03  TBL-DEKIDAKA-G              OCCURS   31.
               05  TBL-DEKIDAKA            PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1014.INC".
      *    包括チェックマスタ
       01  HKTSANTEI-REC.
           COPY    "CPHKTSANTEI.INC".
      *    包括診療コードマスタ
       01  HKTSRYCD-REC.
           COPY    "CPHKTSRYCD.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院行為マスタ−
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    電子点数表マスタチェックパラメタ
           COPY    "CPORCSCETNSCHK.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTALL.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCHKTALLAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
      *
           INITIALIZE                  ORCSCHKTALL-OUTAREA
      *
           MOVE    ORCSCHKTALL-SRYYMD  TO  WRK-SRYYMD
           IF     (ORCSCHKTALL-SRYDD   =   SPACE
                                       OR  "00" )
               MOVE    "01"            TO  WRK-SRYDD
           END-IF
      *
      *    包括対象有無
           PERFORM 100-INIT-SYORI-SEC
      *    入院料チェック処理
           PERFORM 200-NYUIN-CHKSYORI-SEC
           EVALUATE    ORCSCHKTALL-KBN
               WHEN    "1"
      *            入院料包括設定有無のみ
                   CONTINUE
               WHEN    OTHER
      *            入院料包括チェック
                   PERFORM 300-HKTSRY-CHEK-SEC
           END-EVALUATE
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SYORI-SEC            SECTION.
      *
      *    システム管理検索
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                  SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    WRK-SRYYMD          TO  SYS-1014-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1014-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-N
           INITIALIZE                  TBL-NYUAREA
           PERFORM UNTIL  (FLG-SYSKANRI    =   1  )
                       OR (IDX             >=  100 )
               MOVE    MCPDATA-REC         TO  SYS-1014-REC
               IF     (SYS-1014-SANTEI-HOU     =   "1"
                                               OR  "2"  )
               AND    (SYS-1014-KBNCD (1:3)    >=  "500" )
      *            入院料の包括算定方法あり
                   PERFORM 1002-KBNCD-TBLHEN-SEC
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療コード編集処理
      *****************************************************************
       1002-KBNCD-TBLHEN-SEC            SECTION.
      *
      *    入院料
           ADD     1                   TO  IDX-N
           MOVE    SYS-1014-KBNCD (1:3)
                                       TO  TBL-N-KANRICD (IDX-N)
      *    包括算定方法
           MOVE    SYS-1014-SANTEI-HOU TO  TBL-N-SANTEI-HOU (IDX-N)
           MOVE    SYS-1014-SANTEI-KBN TO  TBL-N-SANTEI-KBN (IDX-N)
           MOVE    SYS-1014-NYUGAIKBN  TO  TBL-N-NYUGAIKBN (IDX-N)
           MOVE    SYS-1014-HKNCOMBI-KBN
                                       TO  TBL-N-HKNCOMBIKBN (IDX-N)
           MOVE    SYS-1014-SRYKBN-G   TO  TBL-N-SRYKBN-G (IDX-N)
           PERFORM 1001-HKTSRYCD-HEN-SEC
           MOVE    IDX-N               TO  TBL-N-MAX
           .
       1002-KBNCD-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療コード編集処理
      *****************************************************************
       1001-HKTSRYCD-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  HKTSRYCD-REC
           INITIALIZE                  HKTSRYCD-REC
           MOVE    SYS-1014-KBNCD (1:3)
                                       TO  HKTSRYCD-KANRICD
           MOVE    WRK-SRYYMD          TO  HKTSRYCD-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  HKTSRYCD-YUKOEDYMD
           MOVE    HKTSRYCD-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSRYCD-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL  (FLG-HKTSRYCD    =   1  )
                      OR  (IDY             >=  99 )
               ADD     1                   TO  IDY
               MOVE    HKTSRYCD-SRYCD      TO  TBL-N-SRYCD
                                                           (IDX-N IDY)
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSRYCD-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-HKTSRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料算定判定処理
      *****************************************************************
       200-NYUIN-CHKSYORI-SEC         SECTION.
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    ORCSCHKTALL-PTID    TO  NACCT-PTID
           MOVE    ORCSCHKTALL-SRYYM   TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *    入院料算定なし
           IF      FLG-NYUINACCT       =   1
               MOVE    1                   TO  ORCSCHKTALL-NYUERR
           ELSE
               MOVE    ZERO                TO  ORCSCHKTALL-NYUERR
           END-IF
      *
           PERFORM UNTIL      (FLG-NYUINACCT     =   1)
      *
               PERFORM 5001-NYUINACT-SYORI-SEC
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       500-NYUIN-CHKSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括入院料の判定処理
      *****************************************************************
       5001-NYUINACT-SYORI-SEC         SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  WRK-IDX-N
           PERFORM UNTIL     ( FLG-NYUINACT    =   1 )
               PERFORM VARYING     IDZ     FROM    1  BY  1
                       UNTIL      (IDZ         >   5  )  OR
                                  (NSRY-SRYCD (IDZ) =   SPACE)
                   PERFORM 5001-SRYCD-CHK-SEC
               END-PERFORM
      *
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      NACCT-DAY (IDX-D)   NOT =   ZERO
                   IF      WRK-IDX-N       >   ZERO
                       MOVE    TBL-N-KANRICD (WRK-IDX-N)
                                           TO  WRK-NYUHKTCD (IDX-D)
                       MOVE     WRK-K-SRYCD
                                           TO  WRK-NYUSRYCD (IDX-D)
      *                入院料包括あり
                       MOVE    1           TO  ORCSCHKTALL-NYUHKTKBN
                   ELSE
                       MOVE    "XXX"       TO  WRK-NYUHKTCD (IDX-D)
                   END-IF
               END-IF
           END-PERFORM
           .
       5001-NYUINACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括入院料の判定処理
      *****************************************************************
       5001-SRYCD-CHK-SEC         SECTION.
      *
           PERFORM VARYING     IDX-N   FROM    1  BY  1
                   UNTIL      (IDX-N       >   TBL-N-MAX )
                           OR (FLG-CHK     =   1         )
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   99   )  OR
                                  (TBL-N-SRYCD (IDX-N IDY) =   SPACE)
                   IF      NSRY-SRYCD(IDZ) =   TBL-N-SRYCD (IDX-N IDY)
                       MOVE    NSRY-SRYCD(IDZ)     TO  WRK-K-SRYCD
                       MOVE    1                   TO  FLG-CHK
                       MOVE    IDX-N               TO  WRK-IDX-N
                       MOVE    99                  TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       5001-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料包括チェック処理
      *****************************************************************
       300-HKTSRY-CHEK-SEC         SECTION.
      *
      *    出来高算定日検索
           PERFORM 3009-SANTEI-CHK-SEC
      *    入院料包括診療区分編集処理
           PERFORM 3001-HKTSRYKBN-SET-SEC
      *    日毎の包括設定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF     (WRK-NYUHKTCD (IDX)  =   SPACE
                                           OR  "XXX"  )
                   MOVE    ZERO            TO  ORCSCHKTALL-HKTCHKFLG
                                                                  (IDX)
                   MOVE    SPACE           TO  ORCSCHKTALL-NYUHKTCD
                                                                  (IDX)
               ELSE
                   IF     (IDX             >   1   )  AND
                          (WRK-NYUHKTCD (IDX)
                                           =   WRK-NYUHKTCD (IDX - 1))
                       MOVE    ORCSCHKTALL-HKTCHKFLG (IDX - 1)
                                               TO  ORCSCHKTALL-HKTCHKFLG
                                                                  (IDX)
                       MOVE    WRK-NYUHKTCD (IDX - 1)
                                               TO  ORCSCHKTALL-NYUHKTCD
                                                                  (IDX)
                   ELSE
                       PERFORM 3002-NYUHKTCD-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
      *        出来高算定日
               IF     (TBL-DEKIDAKA (IDX)  =   1      )
                   MOVE    ZERO            TO  ORCSCHKTALL-HKTCHKFLG
                                                                  (IDX)
                   MOVE    SPACE           TO  ORCSCHKTALL-NYUHKTCD
                                                                  (IDX)
               END-IF
           END-PERFORM
           .
       300-HKTSRY-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    入院料包括診療区分編集処理
      *****************************************************************
       3001-HKTSRYKBN-SET-SEC         SECTION.
      *
           EVALUATE    ORCSCHKTALL-HKTSRYKBN(1:1)
               WHEN    "2"
                   MOVE    1               TO  IDX-SRY
               WHEN    "3"
                   MOVE    2               TO  IDX-SRY
               WHEN    "4"
                   MOVE    3               TO  IDX-SRY
               WHEN    "5"
                   MOVE    4               TO  IDX-SRY
               WHEN    "6"
                   IF      ORCSCHKTALL-HKTSRYKBN   =  "64"
                       MOVE    10              TO  IDX-SRY
                   ELSE
                       MOVE    5               TO  IDX-SRY
                   END-IF
               WHEN    "7"
                   MOVE    6               TO  IDX-SRY
               WHEN    "8"
                   EVALUATE    ORCSCHKTALL-HKTSRYSYUKBN
                       WHEN    "800"
                           MOVE    7               TO  IDX-SRY
                       WHEN    "830"
                           MOVE    8               TO  IDX-SRY
                       WHEN    "840"
                           MOVE    9               TO  IDX-SRY
                   END-EVALUATE
           END-EVALUATE
           .
       3001-HKTSRYKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    出来高算定日検索チェック処理
      *****************************************************************
       3009-SANTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-DEKIDAKA-AREA
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    ORCSCHKTALL-PTID    TO  SANTEI-PTID
           MOVE    ORCSCHKTALL-SRYYM   TO  SANTEI-SRYYM
           MOVE    "099999904"         TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               MOVE    SANTEI-DAY (IDD)    TO  TBL-DEKIDAKA (IDD)
           END-PERFORM
      *
           .
       3009-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括入院料の判定処理
      *****************************************************************
       3002-NYUHKTCD-CHK-SEC         SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-N-MAX )
              MOVE     ZERO                TO  FLG-HKTCHK
              IF       TBL-N-KANRICD (IDY) =   WRK-NYUHKTCD (IDX)
      *            包括判定
                   PERFORM 30021-HKTSRYCD-CHK-SEC
                   IF      TBL-N-SANTEI-HOU (IDY)  =   "1"
      *                包括作成
                       IF      FLG-HKTCHK      =   1
                           MOVE    1           TO  ORCSCHKTALL-HKTCHKFLG
                                                                   (IDX)
                       END-IF
                   ELSE
      *                エラー（作成なし）
                       IF      FLG-HKTCHK      =   1
                           MOVE    9           TO  ORCSCHKTALL-HKTCHKFLG
                                                                   (IDX)
                       END-IF
                   END-IF
                   IF      FLG-HKTCHK          =   1
                       MOVE    1               TO  ORCSCHKTALL-NYUHKTCHK
                   END-IF
                   MOVE    TBL-N-KANRICD (IDY) TO  ORCSCHKTALL-NYUHKTCD
                                                                  (IDX)
                   MOVE    TBL-N-MAX           TO  IDY
               END-IF
           END-PERFORM
           .
       3002-NYUHKTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括入院料の判定処理
      *****************************************************************
       30021-HKTSRYCD-CHK-SEC         SECTION.
      *
      *    診療区分毎
           IF      IDX-SRY             >   ZERO
               IF      TBL-N-SRYKBN-KBN (IDY IDX-SRY)  =   "1"
      *            包括される
                   MOVE    1                   TO  FLG-HKTCHK
               END-IF
           END-IF
      *    診療コード
           IF      ORCSCHKTALL-HKTSRYCD-G  NOT =   SPACE
               MOVE    TBL-N-KANRICD (IDY)     TO  WRK-KANRICD
               PERFORM 30022-HKTSANTEI-CHK-SEC
           END-IF
           .
       30021-HKTSRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力項目の包括チェク処理
      *****************************************************************
       30022-HKTSANTEI-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  HKTSANTEI-REC
           INITIALIZE                  HKTSANTEI-REC
           MOVE    WRK-KANRICD         TO  HKTSANTEI-KANRICD
           MOVE    WRK-SRYYMD          TO  HKTSANTEI-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  HKTSANTEI-YUKOEDYMD
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    HKTSANTEI-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_hktsantei"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsantei"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSANTEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
           INITIALIZE                  TBL-HKTSANTEI-AREA
           MOVE    ZERO                TO  IDX-H
      *    電子点数表対象分から対象外
           IF      WRK-SRYYMD          >=  "20100401"
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
      *
           PERFORM UNTIL  (FLG-HKTSANTEI   =   1  )
                      OR  (IDX-H           >=  600 )
               ADD     1                   TO  IDX-H
               MOVE    HKTSANTEI-SRYCD     TO  TBL-HKTSRYCD (IDX-H)
               MOVE    HKTSANTEI-SANTEIKBN TO  TBL-SANTEIKBN (IDX-H)
               MOVE    HKTSANTEI-KENTAIKBN TO  TBL-KENTAIKBN (IDX-H)
               MOVE    "tbl_hktsantei"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsantei"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
           PERFORM VARYING     IDX-T    FROM    1   BY  1
                   UNTIL      (IDX-T    >   10   )  OR
                              (ORCSCHKTALL-HKTSRYCD(IDX-T) =   SPACE)
               MOVE    ORCSCHKTALL-HKTSRYCD(IDX-T)  TO  WRK-SRYCD
               IF     (TBL-HKTSRYCD (1)     =   SPACE )
      *            電子点数表よりチェック
                   PERFORM 300222-ETENSU-CHK-SEC
               ELSE
      *
               PERFORM VARYING     IDX-H    FROM    1   BY  1
                       UNTIL      (IDX-H    >  600   )
                               OR (TBL-HKTSRYCD(IDX-H) =   SPACE)
                   IF      WRK-SRYCD           =   TBL-HKTSRYCD (IDX-H)
                       MOVE    1               TO  FLG-CHK2
                       IF      TBL-SANTEIKBN (IDX-H)   =   "1"
                           MOVE    2               TO  FLG-CHK2
                       END-IF
                       IF      TBL-KENTAIKBN (IDX-H)    =   "1"
                           MOVE    1               TO  FLG-CHK3
                       END-IF
                       MOVE    600                 TO  IDX-H
                   END-IF
               END-PERFORM
      *
               END-IF
           END-PERFORM
      *    包括される
           IF      FLG-CHK2            =   1
               MOVE    1                   TO  FLG-HKTCHK
           END-IF
           .
       30022-HKTSANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    電子点数表よりチェック
      *****************************************************************
       300222-ETENSU-CHK-SEC         SECTION.
      *
           IF      FLG-HKTCHK          =   1
      *        診療区分で包括の時のみ
               INITIALIZE                  ORCSCETNSCHKAREA
               MOVE    "1"                 TO  ORCSCETNSCHK-SYORI
               MOVE    WRK-SRYYMD          TO  ORCSCETNSCHK-SRYYMD
               MOVE    WRK-K-SRYCD         TO  ORCSCETNSCHK-SRYCD1
               MOVE    WRK-SRYCD           TO  ORCSCETNSCHK-SRYCD2
               CALL    "ORCSCETNSCHK"      USING
                                           ORCSCETNSCHKAREA
                                           SPA-AREA
               IF      ORCSCETNSCHK-CHKKBN =   ZERO
      *            包括対象なし
                   MOVE    ZERO            TO  FLG-HKTCHK
               END-IF
           END-IF
           .
       300222-ETENSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療コード読み込み
      *****************************************************************
       910-HKTSRYCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSRYCD-REC
               MOVE    ZERO            TO  FLG-HKTSRYCD
           ELSE
               INITIALIZE                  HKTSRYCD-REC
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
      *
           .
       910-HKTSRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括診療コード読み込み
      *****************************************************************
       910-HKTSANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSANTEI-REC
               MOVE    ZERO            TO  FLG-HKTSANTEI
           ELSE
               INITIALIZE                  HKTSANTEI-REC
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
      *
           .
       910-HKTSANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター次読込
      *****************************************************************
       900-NYUINACCT-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
      *
       900-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       930-NYUINACT-RAED-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
      *
       930-NYUINACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
