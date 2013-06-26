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
       PROGRAM-ID.                 ORCSRNYURECEDEN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 労災レセ電用入院会計取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/12/13    NACL−小豆沢  新規作成
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
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-ROUSAI                  PIC 9(01).
           03  FLG-RYAKU                   PIC 9(01).
           03  FLG-OK                      PIC 9(01).
           03  FLG-KSN                     PIC 9(01).
           03  FLG-TENSUWRK                PIC 9(01).
           03  FLG-TOKU-SET                PIC 9(01).
           03  FLG-TOKU-SHOKU              PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
           03  IDY                         PIC 9(04).
           03  KSN-IDX                     PIC 9(04).
           03  IDV                         PIC 9(04).
           03  IDX-ADL                     PIC 9(04).
           03  MATOME-IDX                  PIC 9(04).
           03  CIDX                        PIC 9(04).
           03  CIDX2                       PIC 9(04).
           03  IDX-DAY                     PIC 9(04).
           03  IDX-NYUKSN                  PIC 9(02).
           03  IDX-CHUKSN1                 PIC 9(02).
           03  IDX-CHUKSN1S                PIC 9(02).
           03  IDX-CHUKSN1SS               PIC 9(02).
           03  IDX-CHUKSN2                 PIC 9(02).
           03  IDX-CHUKSN2S                PIC 9(02).
           03  IDX-CHUKSN2SS               PIC 9(02).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *    点数マスタ読み込みワーク用
           03  TTNS-CONST-MAX              PIC 9(03) VALUE 200.
      *
      *    標欠（１００分の１０減額）
           03  CONS-HYOKETU1               PIC X(09) VALUE "190107890".
      *    標欠（１００分の１５減額）
           03  CONS-HYOKETU2               PIC X(09) VALUE "190107990".
      *    標欠（１００分の３減額）（離島等）
           03  CONS-HYOKETU3               PIC X(09) VALUE "190108590".
      *    標欠（１００分の２減額）（離島等）
           03  CONS-HYOKETU4               PIC X(09) VALUE "190108690".
      *    定数超過入院基本料減額（１００分の１０）
           03  CONS-TEISU1                 PIC X(09) VALUE "190107690".
      *    定数超過入院基本料減額（１００分の２０）
           03  CONS-TEISU2                 PIC X(09) VALUE "190107790".
      *    定数超過入院基本料減額（１００分の１０）（特定入院料）
           03  CONS-TEISU3                 PIC X(09) VALUE "190122990".
      *    定数超過入院基本料減額（１００分の２０）（特定入院料）
           03  CONS-TEISU4                 PIC X(09) VALUE "190123090".
      *
      *    労災治療計画加算
           03  CONS-ROUSAI-CHIRYOU         PIC X(09) VALUE "101910050".
      *    病衣貸与料
           03  CONS-ROUSAI-BYOUI           PIC X(09) VALUE "101910060".
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-MM-X                    PIC X(02).
           03  WRK-MM-9     REDEFINES   WRK-MM-X
                                           PIC 9(02).
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-CHUKSNCD-XXX            PIC X(04).
      *
      *    同日再入院処理用
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
           03  WRK-DOUJITSU-KBN            PIC X(01).
      *
      *    入院会計ワーク
           03  WRK-NYUIN-TABLE.
               05  WRK-NYUIN-TBL           OCCURS  31.
                   07  WRK-HKNCOMBI-DAY    PIC 9(03).
                   07  WRK-SRYKBN          PIC X(02).
                   07  WRK-NYUINKHN        PIC X(09).
                   07  WRK-SHOKIKSN        PIC X(09).
                   07  WRK-HYOUKETSU       PIC X(09).
                   07  WRK-TEISU           PIC X(09).
                   07  WRK-GAIHAKU         PIC X(09).
                   07  WRK-GAIHAKU2        PIC X(09).
                   07  WRK-NYUKHNKSNKBN    PIC 9(03).
                   07  WRK-GAIHAKU-KBN     PIC 9(02).
                   07  WRK-CHUKSNCD        PIC X(04).
      *
      *    入院会計並び替えワーク
           03  WRK-NYUIN-SORT-TABLE.
               05  WRK-NYUIN-SORT-TBL      OCCURS  31.
                   07  WRK-SO-MEISAISU     PIC 9(02).
                   07  WRK-SO-DAY-TBL.
                     09  WRK-SO-DAY        PIC 9(01)  OCCURS 31.
                   07  WRK-SO-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SO-SRYKBN       PIC X(02).
                   07  WRK-SO-SRYCD        PIC X(09)  OCCURS 20.
                   07  WRK-SO-SRYCDTOTAL   PIC 9(14).
                   07  WRK-SO-TEN          PIC 9(07).
                   07  WRK-SO-KAISU        PIC 9(02).
                   07  WRK-SO-ADL-TEN-MOJI PIC X(04).
                   07  WRK-SO-GAIHAKU-TBL.
                     09  WRK-SO-GAIHAKU-KBN  PIC 9(02)  OCCURS 31.
                   07  WRK-SO-IRYOADL-DAY  PIC 9(02).
      *
      *    同一剤まとめ用比較キー
           03  WRK-OLD-KEY.
               05  WRK-OLD-MEISAISU        PIC 9(02).
               05  WRK-OLD-HKNCOMBI-DAY    PIC 9(03).
               05  WRK-OLD-SRYCDTOTAL      PIC 9(14).
               05  WRK-OLD-TEN             PIC 9(07).
           03  WRK-NEW-KEY.
               05  WRK-NEW-MEISAISU        PIC 9(02).
               05  WRK-NEW-HKNCOMBI-DAY    PIC 9(03).
               05  WRK-NEW-SRYCDTOTAL      PIC 9(14).
               05  WRK-NEW-TEN             PIC 9(07).
      *
      *    療養病床コメント並び替えワーク
           03  WRK-IRYOADL-TABLE.
               05  WRK-IRYOADL-SORT-TBL      OCCURS  31.
                   07  WRK-IRYOADL-DAY          PIC 9(02).
                   07  WRK-IRYOADL-MEISAISU     PIC 9(02).
                   07  WRK-IRYOADL-SRYCD        PIC X(09)  OCCURS 30.
                   07  WRK-IRYOADL-SRYCDTOTAL   PIC 9(14).
                   07  WRK-IRYOADL-ADL-SRYCD    PIC X(09).
                   07  WRK-IRYOADL-TEN          PIC 9(07).
                   07  WRK-IRYOADL-TEN-MOJI     PIC X(04).
                   07  WRK-IRYOADL-ADL-SRYCD-H22 PIC X(09) OCCURS 4.
                   07  WRK-IRYOADL-TEN-H22      PIC 9(07)  OCCURS 4.
                   07  WRK-IRYOADL-TEN-MOJI-H22 PIC X(04)  OCCURS 4.
                   07  WRK-IRYOADL-GAITOU-TBL.
                     09  WRK-IRYOADL-GAITOU-KBN   PIC 9(01)  OCCURS 31.
      *
      *-------------------------------------------------------------
      *  平成２４年４月入院分からの注加算退避領域
      *
      *    入院料（Ａ１００番台）、特定入院料（Ａ３００番台）の注加算退避用
           03  WRK-CHUKSN-AREA-1.
             05   WRK-CHUKSN-TBL-1      OCCURS  50.
               07  WRK-CHUKSN-SRYCD-1          PIC X(09).
               07  WRK-CHUKSN-CHUCD-1.
                 09  WRK-CHUKSN-CHUKSNCD-1     PIC X(04).
                 09  WRK-CHUKSN-CHUKSNTUBAN-1  PIC X(01).
               07  WRK-CHUKSN-ZAITEN-1         PIC 9(08).
               07  WRK-CHUKSN-DAY-1            PIC 9(03)  OCCURS 31.
      *
      *    入院基本料等加算（Ａ２００番台）の注加算退避用
           03  WRK-CHUKSN-AREA-2.
             05   WRK-CHUKSN-TBL-2      OCCURS  50.
               07  WRK-CHUKSN-SRYCD-2          PIC X(09).
               07  WRK-CHUKSN-CHUCD-2.
                 09  WRK-CHUKSN-CHUKSNCD-2     PIC X(04).
                 09  WRK-CHUKSN-CHUKSNTUBAN-2  PIC X(01).
               07  WRK-CHUKSN-ZAITEN-2         PIC 9(08).
               07  WRK-CHUKSN-DAY-2            PIC 9(03)  OCCURS 31.
      *
      *    注加算以外の加算退避用
           03  WRK-NYUKSN-AREA-1.
             05   WRK-NYUKSN-TBL-1             OCCURS  50.
               07  WRK-NYUKSN-SRYCD-1          PIC X(09).
               07  WRK-NYUKSN-ZAITEN-1         PIC 9(08).
               07  WRK-NYUKSN-DAY-1            PIC 9(03)  OCCURS 31.
      *
      *    並び替え時の退避用
           03  WRK-CHUKSN-TAIHI.
             05  WRK-CHUKSN-SRYCD-T          PIC X(09).
             05  WRK-CHUKSN-CHUCD-T.
               07  WRK-CHUKSN-CHUKSNCD-T     PIC X(04).
               07  WRK-CHUKSN-CHUKSNTUBAN-T  PIC X(01).
             05  WRK-CHUKSN-ZAITEN-T         PIC 9(08).
             05  WRK-CHUKSN-DAY-T            PIC 9(03)  OCCURS 31.
      *-------------------------------------------------------------
      *
      *    診療コード計集計用
           03  WRK-SRYCD-X                 PIC X(09).
           03  WRK-SRYCD-9    REDEFINES    WRK-SRYCD-X
                                           PIC 9(09).
      *
      *    システム管理食事退避領域
           03  WRK-SYS5000-SKJ             OCCURS  5.
               05  WRK-SYS5000-SHOKUCD     PIC X(09) OCCURS 31.
      *
      *    食事ワーク４（食事療養費）（平成１８年４月以降）
           03  WRK-SKJ4-TABLE.
               05  WRK-SKJ4-TBL          OCCURS  93.
                   07  WRK-SKJ4-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ4-RYOYO          PIC X(09).
                   07  WRK-SKJ4-KAISU          PIC 9(02).
      *    食事ワーク５（特別食加算）（平成１８年４月以降）
           03  WRK-SKJ5-TABLE.
               05  WRK-SKJ5-TBL          OCCURS  93.
                   07  WRK-SKJ5-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ5-TOKU-SHOKU     PIC X(09).
                   07  WRK-SKJ5-KAISU          PIC 9(02).
      *    食事ワーク６（食堂加算）（平成１８年４月以降）
           03  WRK-SKJ6-TABLE.
               05  WRK-SKJ6-TBL          OCCURS  31.
                   07  WRK-SKJ6-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ6-SHOKUDOU       PIC X(09).
                   07  WRK-SKJ6-KAISU          PIC 9(02).
      *    食事ワーク７
           03  WRK-SKJ7-TABLE.
               05  WRK-SKJ7-TBL           OCCURS  31.
                   07  WRK-SKJ7-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ7-GRP.
                       09  WRK-SKJ7-RYOYO       PIC X(09).
                       09  WRK-SKJ7-RYOYO-SU    PIC 9(01).
                       09  WRK-SKJ7-SHOKUDOU    PIC X(09).
                       09  WRK-SKJ7-TOKU-SHOKU  PIC X(09).
                       09  WRK-SKJ7-TOKU-SHOKU-SU PIC 9(01).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU1 PIC 9(02).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU2 PIC 9(02).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU3 PIC 9(02).
                       09  WRK-SKJ7-SKJFTN      PIC X(09).
                       09  WRK-SKJ7-SKJFTN-SU   PIC 9(01).
                   07  WRK-SKJ7-SRYCDTOTAL      PIC 9(14).
                   07  WRK-SKJ7-KAISU           PIC 9(02).
                   07  WRK-SKJ7-DAY-TBL.
                       09  WRK-SKJ7-DAY         PIC 9(01)  OCCURS  31.
      *
      *
      *    点数マスタ退避ワーク
           03  WRK-TENSU-OCC                   OCCURS  200.
               05  WRK-TENSU-SRYCD             PIC X(09).
               05  WRK-TENSU-DATA              PIC X(2000).
           03  WRK-TENSU-CD                    PIC X(09).
           03  TTNS-IDX                        PIC 9(03).
           03  TTNS-MAX                        PIC 9(03).
      *
      *
      *    外泊コメント用日付退避領域
           03  WRK-GAIHAKU-DAY-TABLE.
               05  WRK-GAIHAKU-DAY             OCCURS  31.
                   07  WRK-GAIHAKU-DAY-ST      PIC  9(02).
                   07  WRK-GAIHAKU-DAY-ED      PIC  9(02).
      *
      *    療養コメント用日付退避領域
           03  WRK-RYOYO-DAY-TABLE.
               05  WRK-RYOYO-DAY               OCCURS  31.
                   07  WRK-RYOYO-DAY-ST        PIC  9(02).
                   07  WRK-RYOYO-DAY-ED        PIC  9(02).
      *
      *    他保険での食堂加算判定用領域
           03  WRK-TAHOKEN-SHOKUDOU-TABLE.
               05  WRK-TAHOKEN-SHOKUDOU-FLG    PIC  X(01)  OCCURS 31.
      *
      *    日付全角変換領域
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "０１".
           03  FILLER              PIC X(04)  VALUE "０２".
           03  FILLER              PIC X(04)  VALUE "０３".
           03  FILLER              PIC X(04)  VALUE "０４".
           03  FILLER              PIC X(04)  VALUE "０５".
           03  FILLER              PIC X(04)  VALUE "０６".
           03  FILLER              PIC X(04)  VALUE "０７".
           03  FILLER              PIC X(04)  VALUE "０８".
           03  FILLER              PIC X(04)  VALUE "０９".
           03  FILLER              PIC X(04)  VALUE "１０".
           03  FILLER              PIC X(04)  VALUE "１１".
           03  FILLER              PIC X(04)  VALUE "１２".
           03  FILLER              PIC X(04)  VALUE "１３".
           03  FILLER              PIC X(04)  VALUE "１４".
           03  FILLER              PIC X(04)  VALUE "１５".
           03  FILLER              PIC X(04)  VALUE "１６".
           03  FILLER              PIC X(04)  VALUE "１７".
           03  FILLER              PIC X(04)  VALUE "１８".
           03  FILLER              PIC X(04)  VALUE "１９".
           03  FILLER              PIC X(04)  VALUE "２０".
           03  FILLER              PIC X(04)  VALUE "２１".
           03  FILLER              PIC X(04)  VALUE "２２".
           03  FILLER              PIC X(04)  VALUE "２３".
           03  FILLER              PIC X(04)  VALUE "２４".
           03  FILLER              PIC X(04)  VALUE "２５".
           03  FILLER              PIC X(04)  VALUE "２６".
           03  FILLER              PIC X(04)  VALUE "２７".
           03  FILLER              PIC X(04)  VALUE "２８".
           03  FILLER              PIC X(04)  VALUE "２９".
           03  FILLER              PIC X(04)  VALUE "３０".
           03  FILLER              PIC X(04)  VALUE "３１".
       01  TBL-DAY-AREA-R          REDEFINES   TBL-DAY-AREA.
           03  TBL-DAY             PIC X(04)   OCCURS   31.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK5000.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    療養レセコメント取得サブ
           COPY    "CPORCSRYOCOM.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    労災入院会計作成サブ
           COPY    "CPORCSROUNYUIN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    入院会計作成用パラメタ
           COPY    "CPORCSRNYURECEDEN.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSRNYURECEDEN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-RNYURECEDEN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-RNYURECEDEN-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
      *
      *    主処理
           IF      LNK-RNYURECEDEN-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    算定年月チェック
           IF    ( LNK-RNYURECEDEN-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-RNYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-RNYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    保険組合せ番号チェック
           IF     LNK-RNYURECEDEN-HKNCOMBI  NOT =   ZERO
               CONTINUE
           ELSE
               MOVE    3               TO  LNK-RNYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスターの保険組合せ検索）
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                      TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"          TO  MCP-TABLE
           MOVE    "key17"                  TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT            NOT =   ZERO
               MOVE    4                TO  LNK-RNYURECEDEN-RC
           ELSE
               MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
               MOVE    "key17"              TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT           NOT =   ZERO
                    MOVE    4               TO  LNK-RNYURECEDEN-RC
               ELSE
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                               UNTIL     IDX1   >      31
                        IF     LNK-RNYURECEDEN-HKNCOMBI
                                            =  NACCT-DAY (IDX1)
                          MOVE   NACCT-DAY (IDX1) TO 
                                            WRK-HKNCOMBI-DAY(IDX1)
                        END-IF
                    END-PERFORM
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    医療機関情報の取得（システム管理テーブルより）
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-RNYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-RNYURECEDEN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF       FLG-SYSKANRI   =   0
                   MOVE   SYS-1001-HOSPSBT
                                       TO  SPA-HOSPSBT
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    保険区分に固定値をセット
           MOVE    "4"                 TO  LNK-RNYURECEDEN-HKNKBN
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           PERFORM   200-MAIN-1-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    主取得（労災レセ電返却エリア編集）
      *****************************************************************
       200-MAIN-1-SEC            SECTION.
      *
      *短期滞在手術基本料２と３の入院料未算定日について取得
           INITIALIZE                             ORCSNYUINDAY-AREA
           MOVE    LNK-RNYURECEDEN-HOSPNUM    TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID       TO  LNK-NYUINDAY-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM   TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"         USING   ORCSNYUINDAY-AREA
                                                  SPA-AREA
      *
      *労災から労災への同日再入院で保険組合せが変更とならない場合は
      *同日再入院扱いとする必要がないので、"ORCSNYUINDAY"の返却値を
      *クリアすることとする
           PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL     IDX1   >   31
               IF  (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)  = "1")
               AND (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "4")
               AND (LNK-RNYURECEDEN-HKNCOMBI  =
                               LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1))
               AND (LNK-RNYURECEDEN-HKNCOMBI  =
                               LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX1))
      *            同日再入院区分と保険変動区分をクリア
                   MOVE  SPACE TO LNK-NYUINDAY-DOUJITSU-KBN(IDX1)
                   MOVE  SPACE TO LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1)
               END-IF
           END-PERFORM
      *
      *月末に同日再入院時のレセ電対象保険組合せ番号の取得
      *    月末日を取得
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    LNK-RNYURECEDEN-SANTEIYM
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-GETSUMATSU-YMD
           END-IF
      *
           MOVE    SPACE                   TO  WRK-DOUJITSU-KBN
           IF (LNK-NYUINDAY-DOUJITSU-KBN(WRK-GETSUMATSU-DD) = "1") AND
              (LNK-NYUINDAY-DOUJITSU-HKNCOMBI(WRK-GETSUMATSU-DD)
                                           =  LNK-RNYURECEDEN-HKNCOMBI)
                MOVE      "1"              TO  WRK-DOUJITSU-KBN
           END-IF
      *
      *
      *労災入院会計作成サブの呼び出し（入院料と入院料加算の取得）
      *    診療コード識別  (LNK-SROUNYUIN-SRYCD-SK)
      *      １：入院料
      *      ２：外泊（他医療機関受診含む）
      *      ３：入院料加算（労災乗数対象分）
      *      ４：労災乗数
      *      ５：初期加算（長期減算）
      *      ６：標欠、定数超過
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    LNK-RNYURECEDEN-HOSPNUM
                                           TO  LNK-SROUNYUIN-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID
                                           TO  LNK-SROUNYUIN-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM
                                           TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    LNK-RNYURECEDEN-HKNCOMBI
                                           TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    SPACE                   TO  LNK-SROUNYUIN-RYAKUKBN
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
                                               SPA-AREA
      **     IF   LNK-SROUNYUIN-RC   =   ZERO
      **        PERFORM    VARYING   IDX   FROM   1   BY   1
      **                    UNTIL    IDX    >    31
      *
      **            IF   LNK-SROUNYUIN-NYUIN-FLG(IDX)   =   "1"
      **                DISPLAY  "=  IDX  === " IDX
      **                DISPLAY  "=  ZAITEN = " LNK-SROUNYUIN-ZAITEN(IDX)
      **                DISPLAY  "=  KSNTEN = " 
      **                                 LNK-SROUNYUIN-ZAITEN-NACCT(IDX)
      **                PERFORM   VARYING  IDY  FROM  1  BY  1
      **                  UNTIL   IDY      >  7
      **                     DISPLAY "==  IDY  ===" IDY
      **                     MOVE    LNK-RNYURECEDEN-HOSPNUM
      **                                           TO  TNS-HOSPNUM
      **                     MOVE    LNK-SROUNYUIN-SRYCD(IDX IDY)
      **                                           TO  TNS-SRYCD
      **                     MOVE    LNK-RNYURECEDEN-SANTEIYM
      **                                           TO  TNS-YUKOSTYMD(1:6)
      **                     MOVE    "01"          TO  TNS-YUKOSTYMD(7:2)
      **                     MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
      **                     PERFORM   910-TENSU-READ-SEC
      **                     DISPLAY  "LNK-SROUNYUIN-SRYCD(IDX IDY) = "
      **                               LNK-SROUNYUIN-SRYCD(IDX IDY)
      **                              " (" 
      **                               LNK-SROUNYUIN-SRYCD-SK(IDX IDY)
      **                              ") " 
      **                               TNS-NAME(1:50)
      **                END-PERFORM
      **            END-IF
      *
      **        END-PERFORM
      *
      **     END-IF
      *
      *
      *入院料の取得
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                   MOVE      SPACE    TO      WRK-CHUKSNCD-XXX
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                該当入院行為テーブルの読み込み
                       PERFORM    210-NYUINACT-GET-SEC
      *                入院基本料と初期加算（長期減算）の退避
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                             MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
      *                      入院の期間加算
                             IF  (NSRY-SRYCD(2) NOT =  SPACE)  AND
                                 (NSRY-SRYCD(2) NOT = CONS-HYOKETU1 AND
                                                      CONS-HYOKETU2 AND
                                                      CONS-HYOKETU3 AND
                                                      CONS-HYOKETU4 AND
                                                      CONS-TEISU1   AND
                                                      CONS-TEISU2   AND
                                                      CONS-TEISU3   AND
                                                      CONS-TEISU4)
                                                                AND
                                   (NSRY-SRYCD(2)(1:3) NOT = "188")
                                   MOVE   NSRY-SRYCD(2)  TO
                                                   WRK-SHOKIKSN(IDX1)
                             END-IF
      *                      標欠コード
                             IF  (NSRY-SRYCD(2)  NOT =   SPACE)  AND
                                 (NSRY-SRYCD(2)  =   CONS-HYOKETU1 OR
                                                     CONS-HYOKETU2 OR
                                                     CONS-HYOKETU3 OR
                                                     CONS-HYOKETU4)
                                 MOVE   NSRY-SRYCD(2)  TO
                                                 WRK-HYOUKETSU(IDX1)
                             END-IF
      *                      標欠コード
                             IF  (NSRY-SRYCD(3)  NOT =   SPACE)  AND
                                 (NSRY-SRYCD(3)  =   CONS-HYOKETU1 OR
                                                     CONS-HYOKETU2 OR
                                                     CONS-HYOKETU3 OR
                                                     CONS-HYOKETU4)
                                 MOVE   NSRY-SRYCD(3)  TO
                                                   WRK-HYOUKETSU(IDX1)
                             END-IF
      *                      定数超過コード
                             IF  (NSRY-SRYCD(2)  NOT =   SPACE)  AND
                                 (NSRY-SRYCD(2)  =   CONS-TEISU1 OR
                                                     CONS-TEISU2 OR
                                                     CONS-TEISU3 OR
                                                     CONS-TEISU4)
                                 MOVE   NSRY-SRYCD(2)  TO
                                                 WRK-TEISU(IDX1)
                             END-IF
      *                      定数超過コード
                             IF  (NSRY-SRYCD(3)  NOT =   SPACE)  AND
                                 (NSRY-SRYCD(3)  =   CONS-TEISU1 OR
                                                     CONS-TEISU2 OR
                                                     CONS-TEISU3 OR
                                                     CONS-TEISU4)
                                 MOVE   NSRY-SRYCD(3)  TO
                                                 WRK-TEISU(IDX1)
                             END-IF
      *                      診療区分
                             MOVE    NACCT-SRYKBN
                                              TO  WRK-SRYKBN(IDX1)
                             IF   FLG-RYAKU   =   ZERO
      *
      *                         外泊コード編集用に入院基本料加算区分
      *                         の退避
                                PERFORM   210-GAIHAKU-MAE-SEC
      *
                                MOVE    1     TO    FLG-RYAKU
                             END-IF
      *
                             MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
                             MOVE  WRK-CHUKSNCD-XXX
                                         TO  WRK-CHUKSNCD(IDX1)
      *
                             MOVE    1    TO   FLG-OK
                         END-IF
                         IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                         AND   (LNK-NYUINDAY-KBN(IDX1)  =  "1")
                             MOVE    1    TO   FLG-OK
                         END-IF
                       END-PERFORM
                   END-IF
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
               IF  (FLG-OK     =   ZERO)   AND
                   (WRK-DOUJITSU-KBN   =   SPACE)
                   MOVE     4      TO      LNK-RNYURECEDEN-RC
      *            入院会計クローズ
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
                   GO   TO    200-MAIN-1-EXT
               END-IF
           ELSE
               MOVE     4      TO      LNK-RNYURECEDEN-RC
      *        入院会計クローズ
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
               GO   TO    200-MAIN-1-EXT
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *入院時の外泊取得
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            外泊区分の退避
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *食事療養情報の取得（システム管理テーブルより）
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-RNYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-RNYURECEDEN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
      *        食事情報の退避
               PERFORM    VARYING   IDX  FROM  1  BY  1
                          UNTIL     IDX   >    31
      *            入院時食事療養費
                   IF     SYS-5000-SHOKUJIRYOYOCD  NOT =  SPACE
                       MOVE  SYS-5000-SHOKUJIRYOYOCD
                                          TO WRK-SYS5000-SHOKUCD(1 IDX)
                   END-IF
               END-PERFORM
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *食堂加算取得（入院会計より）
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key43"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key43"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL     =   197000570
      *            食堂加算の退避
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                       AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                             (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
      *                  入院時食事療養（１）の場合のみセット
                           IF  WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                          "197000110"
                               MOVE  "197000570"
                                       TO WRK-SYS5000-SHOKUCD(3 IDX1)
      **                       DISPLAY  "== SHOKUDO-SET IDX1 = " IDX1
                           END-IF
                       END-IF
                   END-PERFORM
      *            食堂加算の退避（同日再入院日）
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF  (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)  = "1")
                       AND (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2"
                                                                 OR "4")
                       AND (LNK-RNYURECEDEN-HKNKBN  =
                                     LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1))
                       AND (NACCT-DAY(IDX1)    NOT =  ZERO)
      *                  入院時食事療養（１）の場合のみセット
                           IF  WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                          "197000110"
                               MOVE  "197000570"
                                       TO WRK-SYS5000-SHOKUCD(3 IDX1)
      **                       DISPLAY  "=== SHOKUDO-DOU-SET IDX1 = " IDX1
                           END-IF
                       END-IF
                   END-PERFORM
                 END-IF
                 MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                 MOVE    "key43"                 TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key43"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    入院料加算取得（入院会計）
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
             MOVE     "tbl_nyuinacct"      TO  MCP-TABLE
             MOVE     "key17"              TO  MCP-PATHNAME
             PERFORM   910-NYUINACCT-READ-SEC
             PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
              IF       NACCT-ZAIKAISU   NOT =    ZERO
      *        該当入院行為テーブルの読み込み
               PERFORM    210-NYUINACT-GET-SEC
      *        点数マスタより注加算の判定
               INITIALIZE                      TNS-TENSU-REC
               MOVE    LNK-RNYURECEDEN-HOSPNUM
                                           TO  TNS-HOSPNUM
               MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
               MOVE    LNK-RNYURECEDEN-SANTEIYM
                                           TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
               PERFORM   910-TENSU-READ-SEC
      *
      *        病衣貸与料と労災治療計画加算は"ORCSRNYURECEDEN"の返却
      *        対象外とする
               IF    NSRY-SRYCD(1)     =    "101910050"  OR
                                            "101910060"
                   MOVE   "9999"       TO   TNS-CHUKSNCD
                   MOVE   999          TO   TNS-CDKBN-KBNNUM
               END-IF
      *
      *        入院料加算の退避
      *
               IF   TNS-CHUKSNCD     =   "0000"
                   ADD    1         TO  IDX-NYUKSN
                   MOVE   NSRY-SRYCD(1)    TO  WRK-NYUKSN-SRYCD-1
                                                      (IDX-NYUKSN)
                   MOVE   NACCT-ZAITEN     TO  WRK-NYUKSN-ZAITEN-1
                                                      (IDX-NYUKSN)
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                     UNTIL     IDX1   >   31
                        IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                              (LNK-NYUINDAY-KBN(IDX1) = "1"))
                        AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                              (WRK-GAIHAKU-KBN(IDX1) >=  4))
                        AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                          MOVE   NACCT-DAY(IDX1)  TO WRK-NYUKSN-DAY-1
                                                      (IDX-NYUKSN IDX1)
                        END-IF
                   END-PERFORM
               ELSE
                 IF ((TNS-CDKBN-KBNNUM   >=   100)   AND
                     (TNS-CDKBN-KBNNUM   <=   199))  OR
                    ((TNS-CDKBN-KBNNUM   >=   300)   AND
                     (TNS-CDKBN-KBNNUM   <=   399))
                      ADD    1         TO  IDX-CHUKSN1
                      MOVE   NSRY-SRYCD(1) TO  WRK-CHUKSN-SRYCD-1
                                                      (IDX-CHUKSN1)
                      MOVE   TNS-CHUKSNCD  TO  WRK-CHUKSN-CHUKSNCD-1
                                                      (IDX-CHUKSN1)
                      MOVE   TNS-CHUKSNTUBAN
                                           TO  WRK-CHUKSN-CHUKSNTUBAN-1
                                                          (IDX-CHUKSN1)
                      MOVE   NACCT-ZAITEN  TO  WRK-CHUKSN-ZAITEN-1
                                                      (IDX-CHUKSN1)
                      PERFORM    VARYING   IDX1   FROM   1   BY   1
                        UNTIL     IDX1   >   31
                          IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                (LNK-NYUINDAY-KBN(IDX1) = "1"))
                          AND  ((WRK-GAIHAKU-KBN(IDX1) = ZERO) OR
                                (WRK-GAIHAKU-KBN(IDX1) >= 4))
                          AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                             MOVE   NACCT-DAY(IDX1)
                                                 TO WRK-CHUKSN-DAY-1
                                                     (IDX-CHUKSN1 IDX1)
                          END-IF
                      END-PERFORM
                 END-IF
      *
                 IF ((TNS-CDKBN-KBNNUM   >=   200)   AND
                     (TNS-CDKBN-KBNNUM   <=   299))
                    ADD    1         TO  IDX-CHUKSN2
                    MOVE   NSRY-SRYCD(1)   TO  WRK-CHUKSN-SRYCD-2
                                                     (IDX-CHUKSN2)
                    MOVE   TNS-CHUKSNCD    TO  WRK-CHUKSN-CHUKSNCD-2
                                                     (IDX-CHUKSN2)
                    MOVE   TNS-CHUKSNTUBAN TO  WRK-CHUKSN-CHUKSNTUBAN-2
                                                          (IDX-CHUKSN2)
                    MOVE   NACCT-ZAITEN    TO  WRK-CHUKSN-ZAITEN-2
                                                          (IDX-CHUKSN2)
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL     IDX1   >   31
                        IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                              (LNK-NYUINDAY-KBN(IDX1) = "1"))
                        AND  ((WRK-GAIHAKU-KBN(IDX1) = ZERO) OR
                              (WRK-GAIHAKU-KBN(IDX1) >= 4))
                        AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                           MOVE   NACCT-DAY(IDX1)  TO WRK-CHUKSN-DAY-2
                                                     (IDX-CHUKSN2 IDX1)
                        END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
              END-IF
      *
              MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
              MOVE    "key17"                 TO  MCP-PATHNAME
              PERFORM 910-NYUINACCT-READ-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    入院料加算取得（診療会計）
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-RNYURECEDEN-PTID     TO  ACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  ACCT-SRYYM
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-ZAIKBN    =        ZERO)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
                     INITIALIZE      SRYACT-REC
                     MOVE    ACCT-HOSPNUM   TO   SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN TO   SRY-NYUGAIKBN
                     MOVE    ACCT-PTID      TO   SRY-PTID
                     MOVE    ACCT-SRYKA     TO   SRY-SRYKA
                     MOVE    ACCT-SRYYM     TO   SRY-SRYYM
                     MOVE    ACCT-ZAINUM    TO   SRY-ZAINUM
                     MOVE    1              TO   SRY-RENNUM
                     MOVE    "tbl_sryact"   TO  MCP-TABLE
                     MOVE    "key"          TO  MCP-PATHNAME
                     PERFORM 910-SRYACT-SELECT-SEC
                     IF      FLG-SRYACT     =    ZERO
                         MOVE     "tbl_sryact"   TO  MCP-TABLE
                         MOVE     "key"          TO  MCP-PATHNAME
                         PERFORM   910-SRYACT-READ-SEC
                         IF  FLG-SRYACT    =    ZERO
      *                    点数マスタより注加算の判定
                           INITIALIZE                TNS-TENSU-REC
                           MOVE    LNK-RNYURECEDEN-HOSPNUM
                                                 TO  TNS-HOSPNUM
                           MOVE    SRY-SRYCD(1)  TO  TNS-SRYCD
                           MOVE    LNK-RNYURECEDEN-SANTEIYM
                                                 TO  TNS-YUKOSTYMD(1:6)
                           MOVE    "01"          TO  TNS-YUKOSTYMD(7:2)
                           MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                           PERFORM   910-TENSU-READ-SEC
      *
      *                   病衣貸与料と労災治療計画加算は
      *                   "ORCSRNYURECEDEN"の返却対象外とする
                           IF    SRY-SRYCD(1)     =    "101910050"  OR
                                                       "101910060"
                               MOVE   "9999"      TO   TNS-CHUKSNCD
                               MOVE   999         TO   TNS-CDKBN-KBNNUM
                           END-IF
                         END-IF
                     END-IF
      *              診療行為クローズ
                     MOVE     "tbl_sryact"   TO  MCP-TABLE
                     MOVE     "key"          TO  MCP-PATHNAME
                     PERFORM   900-CLOSE-SEC
      *             入院料加算の退避
                    IF  TNS-CHUKSNCD         =   "0000"
                       ADD    1              TO  IDX-NYUKSN
                       MOVE   SRY-SRYCD(1)   TO  WRK-NYUKSN-SRYCD-1
                                                        (IDX-NYUKSN)
                       MOVE   ACCT-ZAITEN    TO  WRK-NYUKSN-ZAITEN-1
                                                        (IDX-NYUKSN)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                         UNTIL     IDX1   >   31
                         IF  ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                              (LNK-NYUINDAY-KBN(IDX1) = "1"))
                         AND ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                              (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                         AND  (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                         AND   (ACCT-HKNCOMBI  =
                                                WRK-HKNCOMBI-DAY(IDX1))
                           MOVE   ACCT-DAY(1 IDX1)  TO WRK-NYUKSN-DAY-1
                                                      (IDX-NYUKSN IDX1)
                         END-IF
                       END-PERFORM
                    ELSE
      *
                      IF ((TNS-CDKBN-KBNNUM   >=   100)   AND
                          (TNS-CDKBN-KBNNUM   <=   199))  OR
                         ((TNS-CDKBN-KBNNUM   >=   300)   AND
                          (TNS-CDKBN-KBNNUM   <=   399))
                         ADD    1         TO  IDX-CHUKSN1
                         MOVE   SRY-SRYCD(1)
                                           TO  WRK-CHUKSN-SRYCD-1
                                                          (IDX-CHUKSN1)
                         MOVE   TNS-CHUKSNCD
                                           TO  WRK-CHUKSN-CHUKSNCD-1
                                                          (IDX-CHUKSN1)
                         MOVE   TNS-CHUKSNTUBAN
                                           TO  WRK-CHUKSN-CHUKSNTUBAN-1
                                                          (IDX-CHUKSN1)
                         MOVE   ACCT-ZAITEN
                                           TO  WRK-CHUKSN-ZAITEN-1
                                                          (IDX-CHUKSN1)
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                           UNTIL     IDX1   >   31
                            IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                  (LNK-NYUINDAY-KBN(IDX1) = "1"))
                            AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                                  (WRK-GAIHAKU-KBN(IDX1) >=  4))
                            AND   (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                            AND   (ACCT-HKNCOMBI  =
                                                WRK-HKNCOMBI-DAY(IDX1))
                              MOVE   ACCT-DAY(1 IDX1)
                                                  TO  WRK-CHUKSN-DAY-1
                                                     (IDX-CHUKSN1 IDX1)
                            END-IF
                         END-PERFORM
                      END-IF
      *
                      IF ((TNS-CDKBN-KBNNUM   >=   200)   AND
                          (TNS-CDKBN-KBNNUM   <=   299))
                         ADD    1         TO  IDX-CHUKSN2
                         MOVE   SRY-SRYCD(1)
                                           TO  WRK-CHUKSN-SRYCD-2
                                                          (IDX-CHUKSN2)
                         MOVE   TNS-CHUKSNCD
                                           TO  WRK-CHUKSN-CHUKSNCD-2
                                                          (IDX-CHUKSN2)
                         MOVE   TNS-CHUKSNTUBAN
                                           TO  WRK-CHUKSN-CHUKSNTUBAN-2
                                                          (IDX-CHUKSN2)
                         MOVE   ACCT-ZAITEN
                                           TO  WRK-CHUKSN-ZAITEN-2
                                                          (IDX-CHUKSN2)
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                           UNTIL     IDX1   >   31
                            IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                  (LNK-NYUINDAY-KBN(IDX1) = "1"))
                            AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                                  (WRK-GAIHAKU-KBN(IDX1) >=   4))
                            AND   (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                            AND   (ACCT-HKNCOMBI  =
                                                WRK-HKNCOMBI-DAY(IDX1))
                              MOVE   ACCT-DAY(1 IDX1)
                                                  TO  WRK-CHUKSN-DAY-2
                                                     (IDX-CHUKSN2 IDX1)
                            END-IF
                         END-PERFORM
                      END-IF
      *
                    END-IF
      *
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key30"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"   TO  MCP-TABLE
           MOVE     "key30"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    取得した入院料加算を注加算コード、注加算通番順に並び替え
           PERFORM      VARYING    IDX-CHUKSN1   FROM   1  BY  1
             UNTIL     (IDX-CHUKSN1   >   50)   OR
                       (WRK-CHUKSN-SRYCD-1(IDX-CHUKSN1)  =   SPACE)
               COMPUTE   IDX-CHUKSN1SS   =   IDX-CHUKSN1  +  1
               PERFORM   VARYING IDX-CHUKSN1S FROM IDX-CHUKSN1SS  BY  1
                 UNTIL   (IDX-CHUKSN1S   >   50)   OR
                         (WRK-CHUKSN-SRYCD-1(IDX-CHUKSN1S) =  SPACE)
                   IF  WRK-CHUKSN-CHUCD-1(IDX-CHUKSN1)
                                   <=  WRK-CHUKSN-CHUCD-1(IDX-CHUKSN1S)
                       CONTINUE
                   ELSE
                       MOVE   WRK-CHUKSN-TBL-1(IDX-CHUKSN1)
                                      TO   WRK-CHUKSN-TAIHI
                       MOVE   WRK-CHUKSN-TBL-1(IDX-CHUKSN1S)
                                      TO  WRK-CHUKSN-TBL-1(IDX-CHUKSN1)
                       MOVE   WRK-CHUKSN-TAIHI
                                      TO  WRK-CHUKSN-TBL-1(IDX-CHUKSN1S)
                   END-IF
               END-PERFORM
           END-PERFORM
           PERFORM      VARYING    IDX-CHUKSN2   FROM   1  BY  1
             UNTIL     (IDX-CHUKSN2   >   50)   OR
                       (WRK-CHUKSN-SRYCD-2(IDX-CHUKSN2)  =   SPACE)
               COMPUTE   IDX-CHUKSN2SS   =   IDX-CHUKSN2  +  1
               PERFORM   VARYING IDX-CHUKSN2S FROM IDX-CHUKSN2SS  BY  1
                 UNTIL   (IDX-CHUKSN2S   >   50)   OR
                         (WRK-CHUKSN-SRYCD-2(IDX-CHUKSN2S) =  SPACE)
                   IF  WRK-CHUKSN-CHUCD-2(IDX-CHUKSN2)
                                   <=  WRK-CHUKSN-CHUCD-2(IDX-CHUKSN2S)
                       CONTINUE
                   ELSE
                       MOVE   WRK-CHUKSN-TBL-2(IDX-CHUKSN2)
                                      TO   WRK-CHUKSN-TAIHI
                       MOVE   WRK-CHUKSN-TBL-2(IDX-CHUKSN2S)
                                      TO  WRK-CHUKSN-TBL-2(IDX-CHUKSN2)
                       MOVE   WRK-CHUKSN-TAIHI
                                      TO  WRK-CHUKSN-TBL-2(IDX-CHUKSN2S)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    取得した入院情報から日単位の入院会計情報を再構築
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        診療区分
               MOVE   WRK-SRYKBN(IDX1)  TO  WRK-SO-SRYKBN(IDX1)
               IF    (LNK-NYUINDAY-KBN(IDX1)   =    "1")   AND
                     (LNK-NYUINDAY-TANKI2-KBN(IDX1)  NOT =    SPACE)
                   MOVE   "92"     TO       WRK-SO-SRYKBN(IDX1)
               END-IF
               IF    (LNK-NYUINDAY-KBN(IDX1)   =    "1")   AND
                     (LNK-NYUINDAY-TANKI2-KBN(IDX1)  =    SPACE)
                   MOVE   "90"     TO       WRK-SO-SRYKBN(IDX1)
               END-IF
      *        保険組合せ
               MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                        TO  WRK-SO-HKNCOMBI-DAY(IDX1)
               MOVE     ZERO                TO  WRK-MEISAISU
      *        入院基本料
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        他医療機関受診（３０％減算と７０％減算）
      *        特定時間退院減算
      *        特定曜日入退院減算
               IF     ((WRK-GAIHAKU(IDX1)        =
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590"))
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   IF        (WRK-NYUINKHN(IDX1)  NOT =   SPACE)
                   AND       (WRK-GAIHAKU2(IDX1)  NOT =   SPACE)
                       ADD      1               TO  WRK-MEISAISU
                       MOVE   WRK-GAIHAKU2(IDX1) TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
               END-IF
      *
      *        初期加算（長期減算）
               IF     ((WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590"))
               AND     (WRK-SHOKIKSN(IDX1)      NOT =   SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        標欠
               IF     ((WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590"))
               AND     (WRK-HYOUKETSU(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOUKETSU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        定数超過
               IF     ((WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590"))
               AND     (WRK-TEISU(IDX1)       NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-TEISU(IDX1)  TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        外泊
               IF         WRK-GAIHAKU(IDX1)   NOT =        SPACE
                 IF     ((WRK-GAIHAKU(IDX1)     =
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" )  OR
                         (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590"))
                          CONTINUE
                 ELSE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE   WRK-GAIHAKU-KBN(IDX1)
                                            TO  WRK-SO-GAIHAKU-KBN
                                                           (IDX1 IDX1)
                 END-IF
               END-IF
      *
      *        入院料加算セット（Ａ１００番台、Ａ３００番台）
               PERFORM    VARYING   IDX-CHUKSN1  FROM  1   BY  1
                 UNTIL    (IDX-CHUKSN1    >   50)    OR
                          (WRK-CHUKSN-SRYCD-1(IDX-CHUKSN1)  =  SPACE)
                   IF    WRK-CHUKSN-DAY-1(IDX-CHUKSN1 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-SRYCD-1(IDX-CHUKSN1)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
               END-PERFORM
      *
      *        入院料加算セット（Ａ２００番台）
               PERFORM    VARYING   IDX-CHUKSN2  FROM  1   BY  1
                 UNTIL    (IDX-CHUKSN2    >   50)    OR
                          (WRK-CHUKSN-SRYCD-2(IDX-CHUKSN2)  =  SPACE)
                   IF    WRK-CHUKSN-DAY-2(IDX-CHUKSN2 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-SRYCD-2(IDX-CHUKSN2)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
               END-PERFORM
      *
      *        その他入院料加算セット
               PERFORM   VARYING   IDX-NYUKSN   FROM   1  BY   1
                 UNTIL  (IDX-NYUKSN    >     50)    OR
                        (WRK-NYUKSN-SRYCD-1(IDX-NYUKSN)  =  SPACE)
                  IF    WRK-NYUKSN-DAY-1(IDX-NYUKSN IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD   1    TO  WRK-MEISAISU
                      MOVE  WRK-NYUKSN-SRYCD-1(IDX-NYUKSN)
                                 TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                  END-IF
               END-PERFORM
      *
      *        労災乗数セット
               IF    WRK-SO-SRYKBN(IDX1)    =     "90"
                 IF         LNK-SROUNYUIN-JOUSU-KBN(IDX1)   =    "1"
                     ADD      1               TO  WRK-MEISAISU
                     MOVE   "101900010"   TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                 END-IF
                 IF         LNK-SROUNYUIN-JOUSU-KBN(IDX1)   =    "2"
                     ADD      1               TO  WRK-MEISAISU
                     MOVE   "101900020"   TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                 END-IF
               END-IF
      *
      *
      *
           END-PERFORM
      *
      *    再構築した入院会計情報から点数の算定
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        点数マスタの読み込み
               PERFORM   VARYING   IDX2    FROM   1  BY   1
                         UNTIL (WRK-SO-SRYCD(IDX1 IDX2) = SPACE)
                            OR (IDX2    >      20)
                   PERFORM    210-TENSU-KEISAN-SEC
               END-PERFORM
      *
      *        労災点数の再セット
                   COMPUTE    WRK-SO-TEN(IDX1)    =
                              LNK-SROUNYUIN-ZAITEN(IDX1)  +
                              LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)  +
                              LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
      *        回数情報のセット
               IF    WRK-SO-SRYCD(IDX1 1) NOT  =   SPACE
                   MOVE     1      TO    WRK-SO-DAY(IDX1 IDX1)
               END-IF
           END-PERFORM
      *
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      *                                                         (IDX1)
      *          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      *          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      *          DISPLAY  "WRK-SO-SRYCD(1) = " WRK-SO-SRYCD(IDX1 1)
      *          DISPLAY  "WRK-SO-SRYCD(2) = " WRK-SO-SRYCD(IDX1 2)
      *          DISPLAY  "WRK-SO-SRYCD(3) = " WRK-SO-SRYCD(IDX1 3)
      *          DISPLAY  "WRK-SO-SRYCD(4) = " WRK-SO-SRYCD(IDX1 4)
      *          DISPLAY  "WRK-SO-SRYCD(5) = " WRK-SO-SRYCD(IDX1 5)
      *          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      *          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      *     END-PERFORM
      *
      *    医療区分、ＡＤＬ点数情報の取得
           INITIALIZE                          ORCSRYOCOM-AREA
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  LNK-RYOCOM-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  LNK-RYOCOM-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  LNK-RYOCOM-SANTEIYM
           CALL    "ORCSRYOCOM"     USING  ORCSRYOCOM-AREA
                                           SPA-AREA
           IF      LNK-RYOCOM-RC           NOT =  ZERO
               DISPLAY  "LNK-RYOCOM-RC = " LNK-RYOCOM-RC
           END-IF
      *
      *     PERFORM   VARYING  IDX1   FROM  1   BY   1
      *               UNTIL    IDX1    >   31
      *       DISPLAY "IDX1 = " IDX1
      *       DISPLAY "LNK-RYOCOM-NYUINKBN = " LNK-RYOCOM-NYUINKBN(IDX1)
      *       DISPLAY "LNK-RYOCOM-MEISAISU = " LNK-RYOCOM-MEISAISU(IDX1)
      *       DISPLAY "LNK-RYOCOM-SRYCDTOTAL = " 
      *                                     LNK-RYOCOM-SRYCDTOTAL(IDX1)
      *       DISPLAY "LNK-RYOCOM-SRYCDTOTAL2 = " 
      *                                     LNK-RYOCOM-SRYCDTOTAL2(IDX1)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 1) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 1)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 2) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 2)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 3) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 3)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 4) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 4)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 5) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 5)
      *       DISPLAY "LNK-RYOCOM-ADL-SRYCD (IDX1) = "
      *                             LNK-RYOCOM-ADL-SRYCD(IDX1)
      *       DISPLAY "LNK-RYOCOM-ADL-TEN (IDX1) = "
      *                             LNK-RYOCOM-ADL-TEN(IDX1)
      *       DISPLAY "LNK-RYOCOM-ADL-TEN-MOJI (IDX1) = "
      *                             LNK-RYOCOM-ADL-TEN-MOJI(IDX1)
      *     END-PERFORM
      *
           MOVE      ZERO      TO   MATOME-IDX
      *
      *    再構築した入院会計情報から同一剤のまとめ処理
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)
                   IF    (LNK-RYOCOM-NYUINKBN(IDX1)   =   "1")   AND
                         (LNK-RYOCOM-MEISAISU(IDX1)   NOT =  ZERO)
                       ADD   1              TO  MATOME-IDX
                       MOVE  MATOME-IDX     TO  WRK-IRYOADL-DAY(IDX1)
                       MOVE  MATOME-IDX     TO  WRK-SO-IRYOADL-DAY(IDX1)
                   END-IF
                   ADD   1                  TO  WRK-SO-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SO-MEISAISU(IDX3)  =
                                             WRK-SO-MEISAISU(IDX1)) AND
                             (WRK-SO-HKNCOMBI-DAY(IDX3)  =
                                         WRK-SO-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SO-SRYCDTOTAL(IDX3)  =
                                         WRK-SO-SRYCDTOTAL(IDX1))   AND
                             (WRK-SO-TEN(IDX3)  =
                                         WRK-SO-TEN(IDX1))
                             IF (LNK-RYOCOM-NYUINKBN(IDX3) = "1")   AND
                                (LNK-RYOCOM-MEISAISU(IDX3) NOT =  ZERO)
                                 MOVE  MATOME-IDX    TO  
                                                 WRK-IRYOADL-DAY(IDX3)
                             END-IF
                             ADD     1    TO    WRK-SO-KAISU(IDX1)
                             MOVE    1    TO    WRK-SO-DAY(IDX1 IDX3)
                             IF  WRK-SO-GAIHAKU-KBN(IDX3 IDX3)
                                                             NOT = ZERO
                                 MOVE  WRK-SO-GAIHAKU-KBN(IDX3 IDX3)
                                          TO  WRK-SO-GAIHAKU-KBN
                                                            (IDX1 IDX3)
                             END-IF
                             INITIALIZE   WRK-NYUIN-SORT-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      **     DISPLAY "===  DAY-SET2 ======"
      **     PERFORM   VARYING   IDX1    FROM   1  BY  1
      **       UNTIL   IDX1     >   31
      **       DISPLAY "IDX1 = "IDX1
      **         DISPLAY "      ----5----0----5----0----5----0-"
      **         DISPLAY "DAY = " WRK-SO-DAY-TBL(IDX1)
      **     END-PERFORM
      *
      *    療養レセコメントの同一剤まとめ処理
           IF    (MATOME-IDX     NOT =   ZERO)   AND
                 (LNK-RYOCOM-RC  =       ZERO)
      *      療養レセコメント取得サブの返却結果をまとめワークに転送
             PERFORM      VARYING    IDX1   FROM   1  BY  1
               UNTIL      IDX1   >   31
                 IF   (LNK-RYOCOM-NYUINKBN(IDX1)  NOT =   ZERO)   AND
                      (LNK-RYOCOM-MEISAISU(IDX1)  NOT =   ZERO)
                     MOVE   LNK-RYOCOM-MEISAISU(IDX1)
                                   TO  WRK-IRYOADL-MEISAISU(IDX1)
                     PERFORM  VARYING  IDX2  FROM  1  BY  1
                       UNTIL  IDX2 >  LNK-RYOCOM-MEISAISU(IDX1)
                         MOVE  LNK-RYOCOM-IRYO-SRYCD(IDX1 IDX2)
                                     TO  WRK-IRYOADL-SRYCD(IDX1 IDX2)
                     END-PERFORM
                     MOVE   LNK-RYOCOM-SRYCDTOTAL2(IDX1)
                                   TO  WRK-IRYOADL-SRYCDTOTAL(IDX1)
                     IF     LNK-RNYURECEDEN-SANTEIYM   <=  "201003"
                         MOVE   LNK-RYOCOM-ADL-SRYCD(IDX1)
                                       TO  WRK-IRYOADL-ADL-SRYCD(IDX1)
                         MOVE   LNK-RYOCOM-ADL-TEN(IDX1)
                                       TO  WRK-IRYOADL-TEN(IDX1)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI(IDX1)
                                       TO  WRK-IRYOADL-TEN-MOJI(IDX1)
                     ELSE
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 1)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 1)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 1)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 2)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 2)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 2)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 3)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 3)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 3)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 4)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 4)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 4)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 4)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 4)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 4)
                     END-IF
                 END-IF
             END-PERFORM
      *
      *      療養レセコメントまとめ
             PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF   (WRK-IRYOADL-DAY(IDX1)  NOT =   ZERO)   AND
                    (WRK-IRYOADL-MEISAISU(IDX1)  NOT =   ZERO)
                   MOVE       1   TO  WRK-IRYOADL-GAITOU-KBN(IDX1 IDX1)
                   MOVE       ZERO      TO  IDX2
                   COMPUTE    IDX2      =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                     IF    LNK-RNYURECEDEN-SANTEIYM   <=   "201003"
                       IF    (WRK-IRYOADL-DAY(IDX3)  =
                                         WRK-IRYOADL-DAY(IDX1)) AND
                             (WRK-IRYOADL-MEISAISU(IDX3)  =
                                         WRK-IRYOADL-MEISAISU(IDX1)) AND
                             (WRK-IRYOADL-SRYCDTOTAL(IDX3)  =
                                       WRK-IRYOADL-SRYCDTOTAL(IDX1)) AND
                             (WRK-IRYOADL-TEN(IDX3)  =
                                         WRK-IRYOADL-TEN(IDX1))
                             MOVE      1    TO
                                       WRK-IRYOADL-GAITOU-KBN(IDX1 IDX3)
                             INITIALIZE   WRK-IRYOADL-SORT-TBL(IDX3)
                       END-IF
                     ELSE
                       IF    (WRK-IRYOADL-DAY(IDX3)  =
                                         WRK-IRYOADL-DAY(IDX1)) AND
                             (WRK-IRYOADL-MEISAISU(IDX3)  =
                                         WRK-IRYOADL-MEISAISU(IDX1)) AND
                             (WRK-IRYOADL-SRYCDTOTAL(IDX3)  =
                                       WRK-IRYOADL-SRYCDTOTAL(IDX1)) AND
                             (WRK-IRYOADL-TEN-H22(IDX3 1)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 1))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 2)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 2))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 3)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 3))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 4)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 4))
                             MOVE      1    TO
                                       WRK-IRYOADL-GAITOU-KBN(IDX1 IDX3)
                             INITIALIZE   WRK-IRYOADL-SORT-TBL(IDX3)
                       END-IF
                     END-IF
                   END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *    まとめ結果確認
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-IRYOADL-DAY(IDX1) = "
      *                    WRK-IRYOADL-DAY(IDX1)
      *          DISPLAY  "WRK-IRYOADL-MEISAISU(IDX1) = "
      *                    WRK-IRYOADL-MEISAISU(IDX1)
      *          DISPLAY  "WRK-IRYOADL-SRYCDTOTAL(IDX1) = "
      *                    WRK-IRYOADL-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-IRYOADL-TEN(IDX1) = "
      *                    WRK-IRYOADL-TEN(IDX1)
      *          DISPLAY  "WRK-IRYOADL-GAITOU-TBL(IDX1) = "
      *                    WRK-IRYOADL-GAITOU-TBL(IDX1)
      *     END-PERFORM
      *
      *    入院会計まとめ結果と療養レセコメントのリンクを確認
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-IRYOADL-DAY(IDX1) = "
      *                    WRK-IRYOADL-DAY(IDX1)
      *          DISPLAY  "WRK-SO-IRYOADL-DAY(IDX1) = "
      *                    WRK-SO-IRYOADL-DAY(IDX1)
      *     END-PERFORM
      *
      *   結果表示
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      *                                                         (IDX1)
      *          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      *          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      *          DISPLAY  "WRK-SO-SRYCD(1) = " WRK-SO-SRYCD(IDX1 1)
      *          DISPLAY  "WRK-SO-SRYCD(2) = " WRK-SO-SRYCD(IDX1 2)
      *          DISPLAY  "WRK-SO-SRYCD(3) = " WRK-SO-SRYCD(IDX1 3)
      *          DISPLAY  "WRK-SO-SRYCD(4) = " WRK-SO-SRYCD(IDX1 4)
      *          DISPLAY  "WRK-SO-SRYCD(5) = " WRK-SO-SRYCD(IDX1 5)
      *          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      *          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      *          PERFORM   VARYING  IDX2  FROM  1  BY  1
      *                    UNTIL  IDX2  >  31
      *                     DISPLAY  "IDX2 = " IDX2
      *                     DISPLAY  "WRK-SO-GAIHAKU-KBN = " 
      *                               WRK-SO-GAIHAKU-KBN(IDX1 IDX2)
      *          END-PERFORM
      *     END-PERFORM
      *
      *    レセ電サブ返却エリアへのセット（入院料　診療区分９０）
           MOVE       ZERO      TO     IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "90")
                     ADD    1      TO    IDX2
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                     MOVE  WRK-SO-DAY-TBL(IDX1)    TO
                                      LNK-RNYURECEDEN-DAY-TBL(1 IDX2)
      *
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   20)
                         MOVE   "RI"   TO
                                    LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                         IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-RNYURECEDEN-SRYSKB(1 IDX2 IDX3)
                         END-IF
      *                  入院料加算の労災用置き換え
      *                  一般病棟看護必要度加算（一般病棟）（労災用）
                         IF    WRK-SO-SRYCD(IDX1 IDX3)  =  "190131570"
                            MOVE "101900100" TO WRK-SO-SRYCD(IDX1 IDX3)
                         END-IF
      *                  一般病棟看護必要度加算（専門病院）（労災用）
                         IF    WRK-SO-SRYCD(IDX1 IDX3)  =  "190134970"
                            MOVE "101900120" TO WRK-SO-SRYCD(IDX1 IDX3)
                         END-IF
      *                  重度認知症加算（精神病棟）（労災用）
                         IF    WRK-SO-SRYCD(IDX1 IDX3)  =  "190118570"
                            MOVE "101900110" TO WRK-SO-SRYCD(IDX1 IDX3)
                         END-IF
      *
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                         MOVE   WRK-SO-KAISU(IDX1)   TO
                                      LNK-RNYURECEDEN-KAISU(1 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =   SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-RNYURECEDEN-TEN(1 IDX2 IDX3)
                         END-IF
                     END-PERFORM
      *
      *              外泊コメントセット処理
                     INITIALIZE    WRK-GAIHAKU-DAY-TABLE
                     IF   WRK-SO-GAIHAKU-TBL(IDX1)   =  ALL"0"
                         CONTINUE
                     ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-SO-GAIHAKU-KBN(IDX1 IDX5)  =
                                                         1   OR  2
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-SO-GAIHAKU-KBN(IDX1 IDX7) =
                                                         1   OR  2
                                        MOVE  IDX5  TO
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                       PERFORM   VARYING  IDX5  FROM  1  BY  1
                       UNTIL   (IDX5  >  31)   OR
                               (WRK-GAIHAKU-DAY-ST(IDX5) =  ZERO)
                           ADD    1     TO
                                       LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                           IF   WRK-GAIHAKU-DAY-ST(IDX5) = 
                                               WRK-GAIHAKU-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "840000015"  TO
                                      LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                                MOVE   "CO"   TO   
                                    LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "840000016"  TO
                                      LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-GAIHAKU-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                           END-IF
                           ADD    1     TO   IDX3
                       END-PERFORM
                     END-IF
      *
                     IF  LNK-RYOCOM-RC  =       ZERO
      *              療養レセプトコメントセット処理
      *              月日セット
                     PERFORM   VARYING  CIDX   FROM   1  BY  1
                       UNTIL   CIDX     >   31
                      IF   WRK-SO-IRYOADL-DAY(IDX1)  =
                                         WRK-IRYOADL-DAY(CIDX)
                       INITIALIZE    WRK-RYOYO-DAY-TABLE
                       IF   WRK-IRYOADL-GAITOU-TBL(CIDX)   =  ALL"0"
                         CONTINUE
                       ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-IRYOADL-GAITOU-KBN(CIDX IDX5)
                                                          =       1
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-RYOYO-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-RYOYO-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-IRYOADL-GAITOU-KBN(CIDX IDX7)
                                                           =     1
                                        MOVE  IDX5  TO
                                              WRK-RYOYO-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-RYOYO-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-RYOYO-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
      *                  平成２２年４月より記録コメントの変更
      *                  （記録コメントコード810000001  ->  840000125 840000134）
                         IF  LNK-RNYURECEDEN-SANTEIYM    <=   "201003"
                          PERFORM   VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  31)   OR
                               (WRK-RYOYO-DAY-ST(IDX5) =  ZERO)
                           IF   LNK-RNYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                       LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                             IF   WRK-RYOYO-DAY-ST(IDX5) = 
                                               WRK-RYOYO-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "810000001"  TO
                                      LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   "　　月　　日"  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                MOVE   LNK-RNYURECEDEN-SANTEIYM(5:2)
                                                       TO  WRK-MM-X
                                MOVE   WRK-MM-9        TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (7:4)
                             ELSE
                                MOVE   "CO"   TO   
                                    LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "810000001"  TO
                                      LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   "　　月　　日〜　　月　　日"  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                MOVE   LNK-RNYURECEDEN-SANTEIYM(5:2)
                                                       TO  WRK-MM-X
                                MOVE   WRK-MM-9        TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (15:4)
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (7:4)
                                MOVE   WRK-RYOYO-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (21:4)
                             END-IF
                             ADD    1     TO   IDX3
                           END-IF
                          END-PERFORM
                         ELSE
                          PERFORM   VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  31)   OR
                               (WRK-RYOYO-DAY-ST(IDX5) =  ZERO)
                           IF   LNK-RNYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                       LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                             IF   WRK-RYOYO-DAY-ST(IDX5) = 
                                               WRK-RYOYO-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                                IF   SPA-HOSPSBT    =    1
                                    MOVE  "840000125"  TO
                                      LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                                ELSE
                                    MOVE  "840000134"  TO
                                      LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                                END-IF
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                             ELSE
                                MOVE   "CO"   TO   
                                    LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                                IF   SPA-HOSPSBT    =    1
                                    MOVE  "840000125"  TO
                                      LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                                ELSE
                                    MOVE  "840000134"  TO
                                      LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                                END-IF
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-RYOYO-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                             END-IF
                             ADD    1     TO   IDX3
                           END-IF
                          END-PERFORM
                         END-IF
      *
      *                  医療区分コメント
                         PERFORM   VARYING  IDX5  FROM  1  BY  1
                         UNTIL   (IDX5  >  30)   OR
                           (WRK-IRYOADL-SRYCD(CIDX IDX5) =  SPACE)
                           IF   LNK-RNYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                   LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                             MOVE   "CO"   TO   
                                LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                             MOVE  WRK-IRYOADL-SRYCD(CIDX IDX5)  TO
                                  LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                             ADD    1     TO   IDX3
                           END-IF
                         END-PERFORM
      *
      *                  ＡＤＬコメント（平成２２年４月より記録コメントの変更）
                         IF  LNK-RNYURECEDEN-SANTEIYM    <=   "201003"
                          IF   LNK-RNYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000103"   TO
                                  LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN(CIDX)   =   ZERO
                             MOVE  "００"    TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN(CIDX) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
                         ELSE
      *                   ＡＤＬａ
                          IF   LNK-RNYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000126"   TO
                                  LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 1)   =   ZERO
                             MOVE  "００"    TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 1) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   ＡＤＬｂ
                          IF   LNK-RNYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000127"   TO
                                  LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 2)   =   ZERO
                             MOVE  "００"    TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 2) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   ＡＤＬｃ
                          IF   LNK-RNYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000128"   TO
                                  LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 3)   =   ZERO
                             MOVE  "００"    TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 3) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   ＡＤＬｄ
                          IF   LNK-RNYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-RNYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-RNYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000129"   TO
                                  LNK-RNYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 4)   =   ZERO
                             MOVE  "００"    TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 4) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
                         END-IF
      *
                       END-IF
                      END-IF
                     END-PERFORM
      *
                     END-IF
      *
               END-IF
           END-PERFORM
      *    レセ電サブ返却エリアへのセット（特定入院料　診療区分９２）
           MOVE      ZERO     TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "92")
                     ADD    1      TO    IDX2
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-RNYURECEDEN-MEISAISU(2 IDX2)
                     MOVE  WRK-SO-DAY-TBL(IDX1)    TO
                                     LNK-RNYURECEDEN-DAY-TBL(2 IDX2)
      *
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   20)
                         MOVE   "RI"   TO   
                                    LNK-RNYURECEDEN-RECSKB(2 IDX2 IDX3)
                         IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-RNYURECEDEN-SRYSKB(2 IDX2 IDX3)
                         END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-RNYURECEDEN-SRYCD(2 IDX2 IDX3)
                         MOVE   WRK-SO-KAISU(IDX1)   TO
                                      LNK-RNYURECEDEN-KAISU(2 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =    SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-RNYURECEDEN-TEN(2 IDX2 IDX3)
                         END-IF
                     END-PERFORM
      *              外泊コメントセット処理
                     INITIALIZE    WRK-GAIHAKU-DAY-TABLE
                     IF   WRK-SO-GAIHAKU-TBL(IDX1)   =  ALL"0"
                         CONTINUE
                     ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-SO-GAIHAKU-KBN(IDX1 IDX5)  =
                                                         1   OR  2
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-SO-GAIHAKU-KBN(IDX1 IDX7) =
                                                         1   OR  2
                                        MOVE  IDX5  TO
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                       PERFORM   VARYING  IDX5  FROM  1  BY  1
                       UNTIL   (IDX5  >  31)   OR
                               (WRK-GAIHAKU-DAY-ST(IDX5) =  ZERO)
                           ADD    1     TO
                                       LNK-RNYURECEDEN-MEISAISU(2 IDX2)
                           IF   WRK-GAIHAKU-DAY-ST(IDX5) = 
                                               WRK-GAIHAKU-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-RNYURECEDEN-RECSKB(2 IDX2 IDX3)
                                MOVE  "840000015"  TO
                                      LNK-RNYURECEDEN-SRYCD(2 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(2 IDX2 IDX3)
                           ELSE
                                MOVE   "CO"   TO   
                                    LNK-RNYURECEDEN-RECSKB(2 IDX2 IDX3)
                                MOVE  "840000016"  TO
                                      LNK-RNYURECEDEN-SRYCD(2 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(2 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-GAIHAKU-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-RNYURECEDEN-COMMENT(2 IDX2 IDX3)
                                                          (5:4)
                           END-IF
                           ADD    1     TO   IDX3
                       END-PERFORM
                     END-IF
                END-IF
           END-PERFORM
      *
      *
      *    食事情報の編集
           PERFORM   2103-SHOKUJI-HENSHU-SEC
      *
           .
       200-MAIN-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為取得
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    入院行為クローズ
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊前処理
      *****************************************************************
       210-GAIHAKU-MAE-SEC            SECTION.
      *
      *    外泊コード編集用に入院基本料加算区分と診療区分の退避
           INITIALIZE                      TNS-TENSU-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM
                                       TO  TNS-HOSPNUM
           MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
           MOVE    LNK-RNYURECEDEN-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-NYUKHNKSNKBN    TO  WRK-KSNKBN
           MOVE    TNS-CHUKSNCD        TO  WRK-CHUKSNCD-XXX
      *
           .
       210-GAIHAKU-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊診療コード取得
      *****************************************************************
       210-GAIHAKU-SET-SEC            SECTION.
      *
           MOVE   NACCT-DAY(IDX1)     TO    WRK-GAIHAKU-KBN(IDX1)
      *
           EVALUATE   WRK-SRYKBN(IDX1)
              WHEN     "90"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107290"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107390"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                    WHEN     8
                    WHEN     9
                    WHEN    10
                    WHEN    11
                    WHEN    12
                    WHEN    13
                      PERFORM   2101-GAIHAKU-GENSAN-SEC
                 END-EVALUATE
              WHEN     "92"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107490"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107590"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                    WHEN     8
                    WHEN     9
                    WHEN    10
                    WHEN    11
                    WHEN    12
                    WHEN    13
                      PERFORM   2101-GAIHAKU-GENSAN-SEC
                 END-EVALUATE
           END-EVALUATE
      *
           .
       210-GAIHAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊診療コード取得
      *****************************************************************
       2101-GAIHAKU-TAKIKAN-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *
               WHEN   099  THRU  100
                 IF   LNK-RNYURECEDEN-SANTEIYM   >=   "201004"
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 END-IF
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  229
               WHEN   230  THRU  239
               WHEN   241  THRU  249
               WHEN   250  THRU  255
                 IF    WRK-NYUKHNKSNKBN(IDX1) = 221 OR 232
                   EVALUATE   NACCT-DAY(IDX1)
                      WHEN     4
                        MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                      WHEN     5
                        MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                      WHEN     6
                        MOVE   "190153190"    TO   WRK-GAIHAKU(IDX1)
                      WHEN     7
                        MOVE   "190153390"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 ELSE
                   IF  WRK-NYUKHNKSNKBN(IDX1) =
                           229 OR 238 OR 215 OR 216 OR 217 OR 239
                     EVALUATE   NACCT-DAY(IDX1)
                       WHEN     4
                         MOVE   "190108790" TO   WRK-GAIHAKU(IDX1)
                       WHEN     5
                         MOVE   "190142090" TO   WRK-GAIHAKU(IDX1)
                       WHEN     6
                         MOVE   "190153190" TO   WRK-GAIHAKU(IDX1)
                     END-EVALUATE
                   ELSE
                     EVALUATE   NACCT-DAY(IDX1)
                       WHEN     4
                         MOVE   "190108790" TO   WRK-GAIHAKU(IDX1)
                       WHEN     5
                         MOVE   "190142090" TO   WRK-GAIHAKU(IDX1)
                     END-EVALUATE
                   END-IF
                 END-IF
               WHEN   013  THRU  021
               WHEN   101  THRU  109
               WHEN   147  THRU  152
               WHEN   173  THRU  190
               WHEN   196
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   075  THRU  080
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108990"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   085  THRU  086
               WHEN   132  THRU  133
               WHEN   153  THRU  158
                 IF    WRK-NYUKHNKSNKBN(IDX1) =
                           153 OR 154 OR 155 OR 156 OR 157 OR 158
                   EVALUATE   NACCT-DAY(IDX1)
                      WHEN     4
                        MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                      WHEN     5
                        MOVE   "190142290"    TO   WRK-GAIHAKU(IDX1)
                      WHEN     7
                        MOVE   "190153290"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 ELSE
                   EVALUATE   NACCT-DAY(IDX1)
                      WHEN     4
                        MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                      WHEN     5
                        MOVE   "190142290"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 END-IF
               WHEN   226
               WHEN   240
                 CONTINUE
               WHEN   031
               WHEN   033
               WHEN   034
               WHEN   035
               WHEN   038
               WHEN   170
               WHEN   191
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   022
               WHEN   023
               WHEN   024
               WHEN   025
               WHEN   026
               WHEN   029
               WHEN   136
               WHEN   168
               WHEN   169
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   081
               WHEN   083
               WHEN   172
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   OTHER
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
      *
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊診療コード取得２
      *****************************************************************
       2101-GAIHAKU-GENSAN-SEC           SECTION.
      *
           EVALUATE   NACCT-DAY(IDX1)
             WHEN     8
             WHEN    10
             WHEN    11
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      MOVE   "190161470"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9018"
                      MOVE   "190161570"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9026"
                      MOVE   "190161870"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162070"    TO   WRK-GAIHAKU(IDX1)
                IF   NACCT-DAY(IDX1) = 10 OR 11
                   PERFORM   2101-GAIHAKU-TAKIKAN2-SEC
                END-IF
                END-EVALUATE
      *
             WHEN     9
             WHEN    12
             WHEN    13
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      MOVE   "190161670"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9018"
                      MOVE   "190161770"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9026"
                      MOVE   "190161970"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162170"    TO   WRK-GAIHAKU(IDX1)
                END-EVALUATE
                IF   NACCT-DAY(IDX1) = 12 OR 13
                   PERFORM   2101-GAIHAKU-TAKIKAN2-SEC
                END-IF
           END-EVALUATE
      *
           .
       2101-GAIHAKU-GENSAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊診療コード取得３
      *****************************************************************
       2101-GAIHAKU-TAKIKAN2-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
             WHEN   099  THRU  100
                EVALUATE   NACCT-DAY(IDX1)
                    WHEN    10
                    WHEN    12
                      MOVE   "190108890"    TO   WRK-GAIHAKU2(IDX1)
                    WHEN    11
                    WHEN    13
                      MOVE   "190142190"    TO   WRK-GAIHAKU2(IDX1)
                END-EVALUATE
             WHEN   OTHER
                EVALUATE   NACCT-DAY(IDX1)
                    WHEN    11
                    WHEN    13
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                END-EVALUATE
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       210-TENSU-KEISAN-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           IF     (IDX2                   =     1  )
           AND    (WRK-GAIHAKU(IDX1)   NOT =   SPACE)
               MOVE    SPACE           TO      WRK-GAIHAKU-SRYCD
               IF    LNK-RNYURECEDEN-SANTEIYM   <=  "201203"
                  PERFORM    2101-GAIHAKU-NYUINKHN-SET-SEC
               END-IF
               IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
                  MOVE    WRK-GAIHAKU-SRYCD   TO  TNS-SRYCD
               ELSE
                  MOVE    WRK-SO-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
               END-IF
           ELSE
                  MOVE    WRK-SO-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
           END-IF
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  TNS-HOSPNUM
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD           TO  TNS-YUKOEDYMD
           MOVE    TNS-SRYCD    TO   WRK-TENSU-CD
           PERFORM 910-TENSUWRK-CHK-SEC
           IF      FLG-TENSUWRK   =    ZERO
               PERFORM 910-TENSU-READ-SEC
               PERFORM 910-TTNS-HEN-SEC
           END-IF
      *
      *    点数算定
           EVALUATE    TNS-TENSIKIBETU
      *        点数（プラス）
               WHEN    3
                   ADD   TNS-TEN     TO  WRK-SO-TEN(IDX1)
      *        ％加算
               WHEN    5
                   COMPUTE    WRK-SO-TEN(IDX1)  =
                              WRK-SO-TEN(IDX1)  +
                            ((WRK-SO-TEN(IDX1)  *
                              (TNS-TEN   /  100))  + 0.5)
      *        ％減算
               WHEN    6
                   COMPUTE    WRK-SO-TEN(IDX1)  =
                            ( WRK-SO-TEN(IDX1)  -
                             (WRK-SO-TEN(IDX1)  *
                             (TNS-TEN   /  100)))        +  0.5
      *        点数（マイナス）
               WHEN    8
                   COMPUTE  WRK-SO-TEN(IDX1)  =
                            WRK-SO-TEN(IDX1)  -  TNS-TEN
      *        WHEN  OTHER
      *            ADD   TNS-TEN     TO  WRK-SO-TEN(IDX1)
           END-EVALUATE
      *
      *    診療コード計
           MOVE    TNS-SRYCD          TO  WRK-SRYCD-X
           COMPUTE WRK-SO-SRYCDTOTAL(IDX1)  =
                   WRK-SO-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
           ADD      1          TO  WRK-SO-MEISAISU(IDX1)
      *
           .
       210-TENSU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    外泊時の入院基本料取得
      **************************************************************
       2101-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
      *
           EVALUATE  WRK-SO-SRYCD(IDX1 IDX2)
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *        診療所老人医療管理料
               WHEN  "190774210"
                     MOVE  "190774110"      TO   WRK-GAIHAKU-SRYCD
      *        老人性認知症疾患治療病棟入院料１
               WHEN  "190740010"
                     MOVE  "190739910"      TO   WRK-GAIHAKU-SRYCD
      *        老人性認知症疾患治療病棟入院料２
               WHEN  "190813510"
                     MOVE  "190813410"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (LNK-RNYURECEDEN-SANTEIYM  >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD   NOT =   SPACE
               MOVE   WRK-GAIHAKU-SRYCD   TO   WRK-SO-SRYCD(IDX1 IDX2)
           END-IF
      *
           .
       2101-GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事情報の編集（食事療養費食数ベース）
      *****************************************************************
       2103-SHOKUJI-HENSHU-SEC        SECTION.
      *
      *食事情報の取得（朝食）（入院会計）
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
      *
      *            同日再入院で退院日の食事無しであれば、会計をクリアする
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3"
                                                            OR "4") AND
                      (LNK-RNYURECEDEN-HKNCOMBI
                          =  LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX1)) AND
                      (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
      *                        特別食加算は入院時食事療養（１）の場合
      *                        のみセット
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX1)
                               END-IF
                       END-EVALUATE
      *                食堂加算
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            同日再入院日（朝から）
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2"
                                                               OR "4")
                                                                     AND
                         (LNK-RNYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))          AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
      *                        特別食加算は入院時食事療養（１）の場合
      *                        のみセット
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX1)
                               END-IF
                       END-EVALUATE
      *                食堂加算
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            同日再入院日に他保険で食事の算定がある場合に
      *            食堂加算をクリアする
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2"
                                                             OR "4")
                                                                     AND
                       (LNK-RNYURECEDEN-HKNCOMBI   NOT =
                             LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "1"   OR
                                                               "2"   OR
                                                               "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                食堂加算クリアフラグのセット
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
               END-PERFORM
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key30"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *食事情報の取得（昼食）（入院会計）
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key31"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX2   =    IDX1   +   31
      *
      *            同日再入院で退院日の食事無しであれば、会計をクリアする
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3"
                                                            OR "4") AND
                      (LNK-RNYURECEDEN-HKNCOMBI
                          =  LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX1)) AND
                         (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0" OR
                                                                 "1")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX2)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
      *                        特別食加算は入院時食事療養（１）の場合
      *                        のみセット
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX2)
                               END-IF
                       END-EVALUATE
      *                食堂加算
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            同日再入院日（昼から）
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2"
                                                               OR "4")
                                                                     AND
                         (LNK-RNYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1" OR
                                                               "2")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX2)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
      *                        特別食加算は入院時食事療養（１）の場合
      *                        のみセット
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX2)
                               END-IF
                       END-EVALUATE
      *                食堂加算
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            同日再入院日に他保険で食事の算定がある場合に
      *            食堂加算をクリアする
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2"
                                                             OR "4")
                                                                     AND
                       (LNK-RNYURECEDEN-HKNCOMBI   NOT =
                             LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "2"   OR
                                                               "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                食堂加算クリアフラグのセット
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
               END-PERFORM
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key31"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *食事情報の取得（夕食）（入院会計）
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-RNYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RNYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-RNYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key32"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX3   =    IDX1   +   62
      *
      *            同日再入院で退院日の食事無しであれば、会計をクリアする
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3"
                                                            OR "4") AND
                      (LNK-RNYURECEDEN-HKNCOMBI
                          =  LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX1)) AND
                         (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0" OR
                                                                 "1" OR
                                                                 "2")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX3)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
      *                        特別食加算は入院時食事療養（１）の場合
      *                        のみセット
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX3)
                               END-IF
                       END-EVALUATE
      *                食堂加算
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            同日再入院日（夕から）
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2"
                                                               OR "4")
                                                                     AND
                         (LNK-RNYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1" OR
                                                               "2" OR
                                                               "3")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX3)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
      *                        特別食加算は入院時食事療養（１）の場合
      *                        のみセット
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX3)
                               END-IF
                       END-EVALUATE
      *                食堂加算
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            同日再入院日に他保険で食事の算定がある場合に
      *            食堂加算をクリアする
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2"
                                                             OR "4")
                                                                     AND
                       (LNK-RNYURECEDEN-HKNCOMBI   NOT =
                             LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                食堂加算クリアフラグのセット
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
               END-PERFORM
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    食堂加算クリアフラグによる食堂加算領域のクリア
           PERFORM      VARYING   IDX1   FROM   1  BY  1
             UNTIL      IDX1     >    31
             IF    WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)  =   "1"
                 MOVE    SPACE     TO  WRK-SKJ6-SHOKUDOU(IDX1)
             END-IF
           END-PERFORM
      *
      *
      *    食事ワーク７の編集（同一剤まとめ用）
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        食事療養費（朝）
               IF    WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        特別食加算（朝）
               IF    WRK-SKJ5-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX1)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        食事療養費（昼）
               MOVE      ZERO          TO  IDX2
               COMPUTE   IDX2     =    IDX1    +   31
               IF    WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX2)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        特別食加算（昼）
               IF    WRK-SKJ5-TOKU-SHOKU(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX2)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        食事療養費（夕）
               MOVE      ZERO          TO  IDX3
               COMPUTE   IDX3     =    IDX1    +   62
               IF    WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX3)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        特別食加算（夕）
               IF    WRK-SKJ5-TOKU-SHOKU(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX3)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        食事回数
      **         IF    (WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE)
      **             MOVE      1         TO   WRK-SKJ7-KAISU(IDX1)
      **         END-IF
      *        食堂加算
               IF    WRK-SKJ6-SHOKUDOU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ6-SHOKUDOU(IDX1)
                                       TO  WRK-SKJ7-SHOKUDOU(IDX1)
               END-IF
           END-PERFORM
      *
      *    食事の診療コード計
           PERFORM    VARYING   IDX1    FROM  1  BY  1
                      UNTIL     IDX1    >   31
               IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-RYOYO(IDX1)       TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-SHOKUDOU(IDX1)    TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               EVALUATE    WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                   WHEN    1
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                   WHEN    2
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                   WHEN    3
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
               END-EVALUATE
               ADD   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               MOVE  1               TO  WRK-SKJ7-DAY(IDX1 IDX1)
           END-PERFORM
      *
      *     DISPLAY "WRK-SKJ7  MATOME BEFORE"
      *     PERFORM      VARYING    IDX1   FROM   1  BY  1
      *                  UNTIL      IDX1   >   31
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
      *          DISPLAY "WRK-SKJ7-SRYCDTOTAL =(IDX1) = "
      *                   WRK-SKJ7-SRYCDTOTAL (IDX1)
      *     END-PERFORM
      *
      *    再構築した食事情報から同一剤のまとめ処理
      *    食事ワーク
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ7-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ7-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ7-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ7-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ7-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ7-SRYCDTOTAL(IDX1))
                             IF   WRK-SKJ7-TOKU-SHOKU(IDX3) NOT = SPACE
                                 MOVE  WRK-SKJ7-TOKU-SHOKU(IDX3)  TO
                                           WRK-SKJ7-TOKU-SHOKU(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
                             END-IF
                             ADD     1    TO    WRK-SKJ7-KAISU(IDX1)
                             MOVE    1    TO    WRK-SKJ7-DAY(IDX1 IDX3)
                             INITIALIZE   WRK-SKJ7-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    レセ電サブ返却エリアへの食事セット
           MOVE       ZERO    TO   IDX2
           MOVE       ZERO    TO   IDX3
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *          DISPLAY "WRK-SKJ7  MATOME AFTER"
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
              IF    WRK-SKJ7-RYOYO(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ7-DAY-TBL(IDX1)  TO
                                    LNK-RNYURECEDEN-DAY-TBL(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-RNYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                  LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                  LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
      *
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               入院時食事療養（１）
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
      *                         労災（１．２倍）（食事療養）
                                ADD     1     TO    IDX4
                                MOVE   "RI"   TO
                                    LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                                MOVE   "101970010"  TO
                                    LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                                MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                                MOVE   770     TO  
                                    LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
      *                         労災（１．２倍）（食事療養）
                                ADD     1     TO    IDX4
                                MOVE   "RI"   TO
                                    LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                                MOVE   "101970010"  TO
                                    LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                                MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                                MOVE  1540     TO  
                                      LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
      *                         労災（１．２倍）（食事療養）
                                ADD     1     TO    IDX4
                                MOVE   "RI"   TO
                                    LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                                MOVE   "101970010"  TO
                                    LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                                MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                                MOVE  2310     TO  
                                      LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               入院時食事療養（２）
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
      *                         労災（１．２倍）（食事療養）
                                ADD     1     TO    IDX4
                                MOVE   "RI"   TO
                                    LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                                MOVE   "101970010"  TO
                                    LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                                MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                                MOVE   610     TO  
                                      LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
      *                         労災（１．２倍）（食事療養）
                                ADD     1     TO    IDX4
                                MOVE   "RI"   TO
                                    LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                                MOVE   "101970010"  TO
                                    LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                                MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                                MOVE  1220     TO  
                                      LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
      *                         労災（１．２倍）（食事療養）
                                ADD     1     TO    IDX4
                                MOVE   "RI"   TO
                                    LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                                MOVE   "101970010"  TO
                                    LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                                MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                                MOVE  1830     TO  
                                      LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
      *
      *             食堂加算
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
      *                  労災（１．２倍）（食事療養）
                         ADD     1     TO    IDX4
                         MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   "101970010"  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   60     TO
                                 LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
      *
      *             特別食加算
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
      *                  労災（１．２倍）（食事療養）
                         ADD     1     TO    IDX4
                         MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   "101970010"  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   90     TO
                                      LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
      *                  労災（１．２倍）（食事療養）
                         ADD     1     TO    IDX4
                         MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   "101970010"  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   180    TO
                                      LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
      *                  労災（１．２倍）（食事療養）
                         ADD     1     TO    IDX4
                         MOVE   "RI"   TO
                              LNK-RNYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   "101970010"  TO
                                  LNK-RNYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-RNYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   270    TO
                                      LNK-RNYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-RNYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                    END-IF
                    MOVE  IDX4  TO  LNK-RNYURECEDEN-MEISAISU(3 IDX2)
              END-IF
           END-PERFORM
      *
           .
       2103-SHOKUJI-HENSHU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       910-TENSUWRK-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSUWRK
      *
           PERFORM VARYING TTNS-IDX      FROM    1   BY  1
                   UNTIL ( TTNS-IDX      >   TTNS-MAX ) OR
                         ( FLG-TENSUWRK    =   1 )      OR
                         ( TTNS-IDX      >   200 )
               IF    ( WRK-TENSU-SRYCD (TTNS-IDX)
                                       =   WRK-TENSU-CD )
                   MOVE    1           TO  FLG-TENSUWRK
                   MOVE    WRK-TENSU-DATA (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       910-TENSUWRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       910-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  WRK-TENSU-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  WRK-TENSU-DATA
                                                   (TTNS-MAX)
           END-IF
      *
           .
       910-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
                                               SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
                                               SYS-5000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター検索
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター検索
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       910-SRYACT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
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
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
