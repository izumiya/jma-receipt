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
       PROGRAM-ID.                 ORCSNYUACCT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入院会計作成サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/04    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/12/19  食堂加算追加
      * 01.00.02     NACL-多々納  02/12/26  転科時、会計削除の中止
      * 01.00.03     NACL-多々納  02/12/26  年齢チェック追加
      * 01.00.04     NACL-多々納  03/01/28  次月分作成追加
      * 01.00.05     NACL-多々納  03/03/12  選定療養費追加
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
           03  FLG-END             PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SYORI-END       PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *    選定療養費
           03  FLG-SENTEI              PIC 9(01).
      *    取消
           03  FLG-DELETE              PIC 9(01).
           03  FLG-DELETE-OK           PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-N               PIC 9(04).
           03  IDX-N2              PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX1                PIC 9(02).
           03  IDX2                PIC 9(02).
           03  IDX-D               PIC 9(02).
      *
           03  IDX-TBL             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-STYMD.
               05  WRK-STYYMM.
                   07  WRK-STYY            PIC 9(04).
                   07  WRK-STMM            PIC 9(02).
               05  WRK-STDD                PIC 9(02).
           03  WRK-EDYMD.
               05  WRK-EDYYMM.
                   07  WRK-EDYY            PIC 9(04).
                   07  WRK-EDMM            PIC 9(02).
               05  WRK-EDDD                PIC 9(02).
      *
           03  WRK-RRK-YMD                 PIC X(08).
           03  WRK-YMD                     PIC X(08).
      *
           03  WRK-CNT                     PIC 9(03).
      *
           03  WRK-KHN-STYMD               PIC X(08).
           03  WRK-KHN-EDYMD               PIC X(08).
           03  WRK-ENDYMD                  PIC X(08).
      *    日付算定
           03  WRK-INYMD                   PIC X(08).
           03  WRK-OTYMD                   PIC X(08).
           03  WRK-OTYMD2                  PIC X(08).
           03  WRK-NISUU                   PIC 9(03).
           03  WRK-NISUU-KEI               PIC 9(03).
           03  WRK-ZOGEN                   PIC S9(03).
           03  WRK-KAGEN-SRYCD             PIC X(09).
      *    会計情報ワーク
           03  WRK-ACT-STYMD-HZN           PIC X(08).
           03  WRK-ACT-EDYMD-HZN           PIC X(08).
           03  WRK-STYMD-OK                PIC X(08).
           03  WRK-EDYMD-OK                PIC X(08).
           03  WRK-EDYMD-HZN               PIC X(08).
           03  WRK-STYMD-HZN               PIC X(08).
      *    会計情報
           03  WRK-ACT-STYMD               PIC X(08).
           03  WRK-ACT-EDYMD               PIC X(08).
           03  WRK-ACT-ZAISKBKBN           PIC X(01).
           03  WRK-ACT-DAY                 PIC 9(03).
      *    会計情報
           03  WRK-SRYACCT-AREA.
               05  WRK-SRYCD-9                 PIC 9(09).
               05  WRK-TEN                     PIC S9(08).
               05  WRK-TENSU                   PIC S9(08).
      *
               05  WRK-ZAITEN                  PIC 9(08).
               05  WRK-ZAITEN-G                PIC 9(08).
               05  WRK-SRYCDTOTAL              PIC 9(14).
               05  WRK-SURYOUTOTAL             PIC 9(07)V9(03).
               05  WRK-MEISAISU                PIC 9(07).
               05  WRK-ZAIKAISU                PIC 9(07).
      *    点数計算
           03  WRK-KASAN               PIC 9(08)V999.
           03  WRK-KASAN-ALL           PIC 9(08)V999.
           03  WRK-GENZAN              PIC 9(08)V999.
           03  WRK-GENZAN-ALL          PIC 9(08)V999.
      *
           03  WRK-MAXCNT              PIC 9(04).
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-H-ZAINUM            PIC 9(08).
           03  WRK-RENNUM              PIC 9(02).
      *    診療科
           03  WRK-SRYKA               PIC X(02).
      *    下限到達日
           03  WRK-KGN-YMD.
               05  WRK-KGN-YY          PIC 9(04).
               05  WRK-KGN-MM          PIC 9(02).
               05  WRK-KGN-DD          PIC 9(02).
      *    上限到達日
           03  WRK-JGN-YMD.
               05  WRK-JGN-YY          PIC 9(04).
               05  WRK-JGN-MM          PIC 9(02).
               05  WRK-JGN-DD          PIC 9(02).
           03  WRK-YY                  PIC 9(03).
      *
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
      *
       01  TBL-NYUIN-AREA.
           03  TBL-NYUIN-OCC           OCCURS   10.
               05  TBL-SRYCD           PIC X(09).
               05  TBL-TENSU           PIC S9(07)V9(03).
               05  TBL-TENSIKIBETU     PIC 9(01).
               05  TBL-DATAKBN         PIC X(01).
               05  TBL-KGN-YMD         PIC X(08).
               05  TBL-JGN-YMD         PIC X(08).
      *
       01  HZN-NYUIN-AREA.
           03  HZN-NYUIN-OCC           OCCURS   10.
               05  HZN-SRYCD           PIC X(09).
               05  HZN-TENSU           PIC S9(07)V9(03).
               05  HZN-TENSIKIBETU     PIC 9(01).
               05  HZN-DATAKBN         PIC X(01).
               05  HZN-KGN-YMD         PIC X(08).
               05  HZN-JGN-YMD         PIC X(08).
      *
      *    診療行為内容比較用テーブル
       01  TBL-AREA.
           03  TBL-MAE-AREA.
               05  TBL-MAE-REC     OCCURS   30.
                   07  TBL-MAE-SRYCD       PIC  X(09).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療会計マスタ−
       01  HZN-NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC"   REPLACING
                                     //NACCT-// BY //HZN-NACCT-//.
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCNYUIN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    入院会計作成用パラメタ
           COPY    "CPORCSNYUACCT.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSNYUACCT-AREA
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
           MOVE    ZERO                TO  LNK-SCNYUACCT-RC
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
      *
      *
      *    処理区分判定
           EVALUATE    LNK-SCNYUACCT-SYORI-KBN
      *        入院
               WHEN    "1"
                   PERFORM 100-SINKI-SYORI-SEC
      *        退院
               WHEN    "2"
      *        退院（入院会計のみ）
               WHEN    "4"
      *            退院日＋１日を対象とする
                   MOVE    1                   TO  WRK-ZOGEN
                   PERFORM 200-TAIIN-SYORI-SEC
      *        入院取消
               WHEN    "5"
      *        入院取消（全部）
               WHEN    "6"
                   PERFORM 500-NYUINDEL-SYORI-SEC
      *        退院取消
               WHEN    "7"
                   PERFORM 700-TAIINDEL-SYORI-SEC
      *        転出
               WHEN    "8"
                   PERFORM 800-TENSYUTU-SYORI-SEC
      *        次月作成
               WHEN    "9"
                   PERFORM 900-NEXTTUKI-SYORI-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    入院処理
      *****************************************************************
       100-SINKI-SYORI-SEC            SECTION.
      *
      *TEST
      *     DISPLAY "STSRYYMD:"  LNK-SCNYUACCT-STSRYYMD 
      *     DISPLAY "NYUKHN-SRYCD 1:"  LNK-SCNYUACCT-NYUKHN-SRYCD (1)
      *     DISPLAY "NYUKHN-SRYCD 2:"  LNK-SCNYUACCT-NYUKHN-SRYCD (2)
      *     DISPLAY "NYUKHN-SRYCD 3:"  LNK-SCNYUACCT-NYUKHN-SRYCD (3)
      *     DISPLAY "NYUKHN-SRYCD 4:"  LNK-SCNYUACCT-NYUKHN-SRYCD (4)
      *     DISPLAY "KAGEN-SRYCD 1:"  LNK-SCNYUACCT-KAGEN-SRYCD (1)
      *     DISPLAY "KAGEN-SRYCD 2:"  LNK-SCNYUACCT-KAGEN-SRYCD (2)
      *     DISPLAY "KAGEN-SRYCD 3:"  LNK-SCNYUACCT-KAGEN-SRYCD (3)
      *     DISPLAY "KAGEN-SRYCD 4:"  LNK-SCNYUACCT-KAGEN-SRYCD (4)
      *     DISPLAY "KAGEN-SRYCD 5:"  LNK-SCNYUACCT-KAGEN-SRYCD (5)
      *     DISPLAY "KAGEN-STYUKYMD 1:"  LNK-SCNYUACCT-KAGEN-STYUKYMD (1)
      *     DISPLAY "KAGEN-EDYUKYMD 1:"  LNK-SCNYUACCT-KAGEN-EDYUKYMD (1)
      *     DISPLAY "NYUKHN-SRYCD1:"  LNK-SCNYUACCT-NYUKHNKSN-SRYCD(1)
      *     DISPLAY "NYUKHN-SRYCD2:"  LNK-SCNYUACCT-NYUKHNKSN-SRYCD(2)
      *     DISPLAY "GAIHAKU-SRYCD:"  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *     DISPLAY "SAGAKU-SRYCD:"  LNK-SCNYUACCT-SAGAKU-SRYCD
      *     DISPLAY "SHOKUJI-SRYCD:"  LNK-SCNYUACCT-SHOKUJI-SRYCD
      *     DISPLAY "HKNCOMBINUM-SRYCD:"  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
      *TEST
      *
      *    チェック
           IF      LNK-SCNYUACCT-STSRYYMD  =   SPACE  OR  ZERO
               GO      TO     100-SINKI-SYORI-EXT
           END-IF
      *
      *    入院期間作成
      *    開始日の翌月の末日を求める
           MOVE    LNK-SCNYUACCT-STSRYYMD  TO  WRK-STYMD
           PERFORM 100-INIT-SEC
      *
      *    入院基本料作成
           PERFORM 200-NYUKHN-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     100-SINKI-SYORI-EXT
           END-IF
      *
      *    入院料加算作成
           MOVE    ZERO                TO  WRK-NISUU
           PERFORM 210-NYUKHNKSN-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     100-SINKI-SYORI-EXT
           END-IF
      *
      *    その他料作成
           PERFORM 220-SONOTA-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     100-SINKI-SYORI-EXT
           END-IF
      *
           .
       100-SINKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院期間作成処理
      *****************************************************************
       100-INIT-SEC            SECTION.
      *
      *    開始日の翌月の末日を求める
           MOVE    WRK-STYMD           TO  WRK-EDYMD
           COMPUTE WRK-EDMM            =   WRK-EDMM
                                       +   1
           IF      WRK-EDMM            >   12
               COMPUTE WRK-EDYY            =   WRK-EDYY
                                           +   1
               MOVE    1                   TO  WRK-EDMM
           END-IF
      *    月末算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-EDYYMM          TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院基本料作成処理
      *****************************************************************
       200-NYUKHN-SYORI-SEC            SECTION.
      *
           MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
           MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
           MOVE    WRK-STYMD           TO  WRK-ENDYMD
      *
      *    加算により剤を決定する
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL       IDX-K   >   7
      *      開始日が対象であること
             IF     (LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               >=  WRK-STYMD) AND
                    (LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               <=  WRK-EDYMD)
      *
               IF      LNK-SCNYUACCT-KAGEN-SRYCD (IDX-K) =   SPACE
                   IF      WRK-EDYMD           >=  WRK-ENDYMD
                       MOVE    WRK-ENDYMD      TO  WRK-KHN-STYMD
                       MOVE    WRK-EDYMD       TO  WRK-KHN-EDYMD
                       MOVE    SPACE           TO  WRK-KAGEN-SRYCD
                       PERFORM 2001-NYUKHN-HENSYU-SEC
                   END-IF
                   MOVE    7                   TO  IDX-K
               ELSE
      *            対象期間により決定する
                   IF      LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               >   WRK-ENDYMD
      *                前回分より以降の時、次の前日まで
                       MOVE    WRK-ENDYMD      TO  WRK-KHN-STYMD
                       MOVE    LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               TO  WRK-INYMD
                       MOVE    -1              TO  WRK-ZOGEN
                       PERFORM 800-HIZUKE-KEI-SEC
                       MOVE    WRK-OTYMD       TO  WRK-KHN-EDYMD
                       MOVE    SPACE           TO  WRK-KAGEN-SRYCD
                       PERFORM 2001-NYUKHN-HENSYU-SEC
                   END-IF
                   MOVE    LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               TO  WRK-KHN-STYMD
                   MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD (IDX-K)
                                               TO  WRK-KHN-EDYMD
                   MOVE    LNK-SCNYUACCT-KAGEN-SRYCD (IDX-K)
                                               TO  WRK-KAGEN-SRYCD
                   PERFORM 2001-NYUKHN-HENSYU-SEC
               END-IF
             END-IF
           END-PERFORM
      *
      *    残りの日で作成する
           IF      WRK-EDYMD           >=  WRK-ENDYMD
               MOVE    WRK-ENDYMD      TO  WRK-KHN-STYMD
               MOVE    WRK-EDYMD       TO  WRK-KHN-EDYMD
               MOVE    SPACE           TO  WRK-KAGEN-SRYCD
               PERFORM 2001-NYUKHN-HENSYU-SEC
           END-IF
      *
           .
       200-NYUKHN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院基本編集処理
      *****************************************************************
       2001-NYUKHN-HENSYU-SEC            SECTION.
      *
           IF      WRK-KHN-EDYMD       >   WRK-EDYMD
               MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
           END-IF
      *
           INITIALIZE                  TBL-NYUIN-AREA
           MOVE    ZERO                TO  IDX-N
      *
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL      (IDX-N2      >   6   )   OR
                              (LNK-SCNYUACCT-NYUKHN-SRYCD(IDX-N2)
                                           =   SPACE )
               ADD     1                   TO  IDX-N
               MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD (IDX-N2)
                                           TO  TBL-SRYCD (IDX-N)
           END-PERFORM
           IF      WRK-KAGEN-SRYCD NOT =   SPACE
               ADD     1                   TO  IDX-N
               MOVE    WRK-KAGEN-SRYCD     TO  TBL-SRYCD (IDX-N)
           END-IF
      *    入院会計照会・診療行為作成処理
           MOVE    "1"                 TO  WRK-ACT-ZAISKBKBN
           MOVE    1                   TO  WRK-ACT-DAY
           PERFORM 400-NYUACT-SYORI-SEC
      *
      *    終了日の＋１日算定
           MOVE    WRK-KHN-EDYMD       TO  WRK-INYMD
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-HIZUKE-KEI-SEC
           MOVE    WRK-OTYMD           TO  WRK-ENDYMD
      *
           .
       2001-NYUKHN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算作成処理
      *****************************************************************
       210-NYUKHNKSN-SYORI-SEC            SECTION.
      *
      *    加算により剤を決定する
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   20  )  OR
                              (LNK-SCNYUACCT-NYUKHNKSN-SRYCD (IDX-K)
                                          =   SPACE )
      *        点数マスタ検索
               MOVE    SPACE               TO  TNS-TENSU-REC
               MOVE    LNK-SCNYUACCT-NYUKHNKSN-SRYCD (IDX-K)
                                           TO  TNS-SRYCD
               MOVE    WRK-STYMD           TO  WRK-YMD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU       NOT =   ZERO
                   DISPLAY "ORCSNYUACCT TENSU INV:" 
                           LNK-SCNYUACCT-NYUKHNKSN-SRYCD (IDX-K)
                   MOVE    2               TO  LNK-SCNYUACCT-RC
               END-IF
               MOVE    ZERO                TO  WRK-MAXCNT
      *
      *        月上限
               IF      TNS-JGNCNT          >   ZERO
                   MOVE    TNS-JGNCNT          TO  WRK-MAXCNT
               END-IF
               IF      TNS-JGNCNT1W        >   ZERO
                   MOVE    TNS-JGNCNT1W        TO  WRK-MAXCNT
               END-IF
               MOVE    ZERO                TO  FLG-SYORI
               IF      WRK-NISUU       >   LNK-SCNYUACCT-NYUKHN-NISUU
                   MOVE    1               TO  FLG-SYORI
                   MOVE    WRK-NISUU       TO  WRK-NISUU-KEI
               ELSE
                   COMPUTE WRK-NISUU-KEI   =
                                           LNK-SCNYUACCT-NYUKHN-NISUU
                                           -   WRK-NISUU
               END-IF
      *?
               MOVE    ZERO                TO  FLG-SYORI
               MOVE    LNK-SCNYUACCT-NYUKHN-NISUU
                                           TO  WRK-NISUU-KEI
      *?
               IF      WRK-MAXCNT          >   ZERO
                   IF      WRK-NISUU-KEI       >=  WRK-MAXCNT
                       MOVE    1               TO  FLG-SYORI
                   ELSE
                       COMPUTE WRK-ZOGEN   =   WRK-MAXCNT
                                           -   WRK-NISUU-KEI
                   END-IF
               ELSE
      *************MOVE    WRK-NISUU-KEI       TO  WRK-ZOGEN
                   MOVE    ZERO                TO  WRK-ZOGEN
               END-IF
      *TEST
      *         DISPLAY "NYUKHNKSN-SRYCD:" 
      *                          LNK-SCNYUACCT-NYUKHNKSN-SRYCD (IDX-K)
      *                ","      LNK-SCNYUACCT-NYUKHN-NISUU
      *                  ", WRK-MAXCNT:"  WRK-MAXCNT
      *                  ", WRK-NISUU-KEI:"  WRK-NISUU-KEI
      *                  ",WRK-NISUU:"  WRK-NISUU
      *                  ",WRK-ZOGEN:"  WRK-ZOGEN
      *                  ",FLG-SYORI:"  FLG-SYORI
      *
               IF      FLG-SYORI           =   ZERO
      *            通院済み日数より終了日算定
                   IF      WRK-ZOGEN           >   ZERO
                       MOVE    WRK-STYMD           TO  WRK-INYMD
                       PERFORM 800-HIZUKE-KEI-SEC
                   ELSE
                       MOVE    WRK-EDYMD           TO  WRK-OTYMD2
                   END-IF
      *
                   INITIALIZE                  TBL-NYUIN-AREA
                   MOVE    LNK-SCNYUACCT-NYUKHNKSN-SRYCD (IDX-K)
                                               TO  TBL-SRYCD (1)
                   MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
                   MOVE    WRK-OTYMD2          TO  WRK-KHN-EDYMD
                   IF      WRK-KHN-EDYMD       >   WRK-EDYMD
                       MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
                   END-IF
                   MOVE    "6"                 TO  WRK-ACT-ZAISKBKBN
                   MOVE    1                   TO  WRK-ACT-DAY
      *TEST
      *         DISPLAY "NYUKHNKSN-SRYCD:" TBL-SRYCD (1)
      *                 ",WRK-KHN-STYMD:" WRK-KHN-STYMD
      *                 ",WRK-KHN-EDYMD:" WRK-KHN-EDYMD
      *TEST
                   PERFORM 400-NYUACT-SYORI-SEC
               END-IF
           END-PERFORM
      *
           .
       210-NYUKHNKSN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他料作成処理
      *****************************************************************
       220-SONOTA-SYORI-SEC            SECTION.
      *
      *    外泊
           IF      LNK-SCNYUACCT-GAIHAKU-SRYCD NOT =   SPACE
               INITIALIZE                  TBL-NYUIN-AREA
               MOVE    LNK-SCNYUACCT-GAIHAKU-SRYCD
                                           TO  TBL-SRYCD (1)
               MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
               MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
               MOVE    "2"                 TO  WRK-ACT-ZAISKBKBN
               MOVE    ZERO                TO  WRK-ACT-DAY
               PERFORM 400-NYUACT-SYORI-SEC
           END-IF
      *
      *    室料差額診療コード
           IF      LNK-SCNYUACCT-SAGAKU-SRYCD  NOT =   SPACE
               INITIALIZE                  TBL-NYUIN-AREA
               MOVE    LNK-SCNYUACCT-SAGAKU-SRYCD
                                           TO  TBL-SRYCD (1)
               MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
               MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
               MOVE    "3"                 TO  WRK-ACT-ZAISKBKBN
               MOVE    LNK-SCNYUACCT-SAGAKU-KBN
                                           TO  WRK-ACT-DAY
               PERFORM 400-NYUACT-SYORI-SEC
           END-IF
      *
      *    食事診療コード
           IF      LNK-SCNYUACCT-SHOKUJI-SRYCD  NOT =   SPACE
               INITIALIZE                  TBL-NYUIN-AREA
               MOVE    LNK-SCNYUACCT-SHOKUJI-SRYCD
                                           TO  TBL-SRYCD (1)
               MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
               MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
               MOVE    "4"                 TO  WRK-ACT-ZAISKBKBN
               MOVE    LNK-SCNYUACCT-SHOKUJIKBN
                                           TO  WRK-ACT-DAY
               PERFORM 400-NYUACT-SYORI-SEC
           END-IF
      *
      *    食堂加算
           IF      LNK-SCNYUACCT-SHOKUDO-SRYCD NOT =   SPACE
               INITIALIZE                  TBL-NYUIN-AREA
               MOVE    LNK-SCNYUACCT-SHOKUDO-SRYCD
                                           TO  TBL-SRYCD (1)
               MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
               MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
               MOVE    "4"                 TO  WRK-ACT-ZAISKBKBN
               MOVE    LNK-SCNYUACCT-SHOKUDO-KBN
                                           TO  WRK-ACT-DAY
               PERFORM 400-NYUACT-SYORI-SEC
           END-IF
      *
      *    保険組合せ
           IF      LNK-SCNYUACCT-HKNCOMBINUM-SRYCD  NOT =   SPACE
               INITIALIZE                  TBL-NYUIN-AREA
               MOVE    LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
                                           TO  TBL-SRYCD (1)
               MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
               MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
               MOVE    "5"                 TO  WRK-ACT-ZAISKBKBN
               MOVE    LNK-SCNYUACCT-HKNCOMBINUM
                                           TO  WRK-ACT-DAY
               PERFORM 400-NYUACT-SYORI-SEC
           END-IF
      *
      *    選定療養費
           IF      LNK-SCNYUACCT-SENTEI-SRYCD  NOT =   SPACE
               IF      LNK-SCNYUACCT-SENTEI-STYMD
                                               <=  WRK-EDYMD
                   INITIALIZE                  TBL-NYUIN-AREA
                   MOVE    LNK-SCNYUACCT-SENTEI-SRYCD
                                               TO  TBL-SRYCD (1)
      *            選定療養開始日判定
                   IF      LNK-SCNYUACCT-SENTEI-STYMD
                                               >=  WRK-STYMD
                       MOVE    LNK-SCNYUACCT-SENTEI-STYMD
                                                   TO  WRK-KHN-STYMD
                   ELSE
                       MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
                   END-IF
                   MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
                   MOVE    "1"                 TO  WRK-ACT-ZAISKBKBN
                   MOVE    1                   TO  WRK-ACT-DAY
                   MOVE    1                   TO  FLG-SENTEI
                   PERFORM 400-NYUACT-SYORI-SEC
                   MOVE    ZERO                TO  FLG-SENTEI
               END-IF
           END-IF
      *
           .
       220-SONOTA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計・診療行為作成処理
      *****************************************************************
       400-NYUACT-SYORI-SEC            SECTION.
      *
      *    点数算定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (TBL-SRYCD (IDX) =   SPACE )
      *        点数マスタより計算
               PERFORM 4001-TENSU-KEI-SEC
           END-PERFORM
      *
           IF      WRK-KHN-STYMD(1:6)  =   WRK-KHN-EDYMD(1:6)
      *        開始と終了が同じ月の時
               MOVE    WRK-KHN-STYMD       TO  WRK-ACT-STYMD
               MOVE    WRK-KHN-EDYMD       TO  WRK-ACT-EDYMD
               PERFORM 4001-NYUACT-TBL-SEC
           ELSE
      *        開始と終了が違う月の時
               MOVE    WRK-KHN-STYMD       TO  WRK-ACT-STYMD
      *        月末算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-ACT-STYMD(1:6)  TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY7-KOYOMI     TO  WRK-ACT-EDYMD
               END-IF
               PERFORM 4001-NYUACT-TBL-SEC
      *        終了日まで
               MOVE    WRK-KHN-EDYMD       TO  WRK-ACT-STYMD
               MOVE    "01"                TO  WRK-ACT-STYMD(7:2)
               MOVE    WRK-KHN-EDYMD       TO  WRK-ACT-EDYMD
               PERFORM 4001-NYUACT-TBL-SEC
           END-IF
      *
      *
           .
       400-NYUACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       4001-TENSU-KEI-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSU
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    TBL-SRYCD (IDX)     TO  TNS-SRYCD
           MOVE    WRK-KHN-STYMD       TO  WRK-YMD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               DISPLAY "ORCSNYUACCT TENSU INV:" TBL-SRYCD (IDX)
               MOVE    2               TO  LNK-SCNYUACCT-RC
           END-IF
      *    対象年齢チェック（入院基本料以外）
           IF      WRK-ACT-ZAISKBKBN    =  "1"
               CONTINUE
           ELSE
               PERFORM 40011-NENREI-CHK-SEC
           END-IF
      *
           MOVE    TNS-TENSIKIBETU     TO  TBL-TENSIKIBETU (IDX)
           MOVE    TNS-TEN             TO  TBL-TENSU       (IDX)
           MOVE    TNS-DATAKBN         TO  TBL-DATAKBN     (IDX)
      *
      *    労災のコード
           IF      TNS-SRYCD           =   "101"
      *        金額を点数へ置換える
               IF      TNS-TENSIKIBETU     =   1
                   COMPUTE TBL-TENSU  (IDX)    =   TNS-TEN
                                               /   10
               END-IF
           END-IF
      *
           .
       4001-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象年齢チェック処理
      *****************************************************************
       40011-NENREI-CHK-SEC            SECTION.
      *
      *    対象年月日計算
           MOVE    SPACE               TO  WRK-KGN-YMD
           MOVE    SPACE               TO  WRK-JGN-YMD
      *
      *    下限年齢チェック
           IF      TNS-KGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-KGNAGE          =   "AA"
                   MOVE    PTINF-BIRTHDAY      TO  WRK-INYMD
                   MOVE    27                  TO  WRK-ZOGEN
                   PERFORM 800-HIZUKE-KEI-SEC
                   MOVE    WRK-OTYMD           TO  WRK-KGN-YMD
               ELSE
                   MOVE    TNS-KGNAGE          TO  WRK-YY
                   MOVE    PTINF-BIRTHDAY      TO  WRK-KGN-YMD
                   COMPUTE WRK-KGN-YY          =   WRK-KGN-YY
                                               +   WRK-YY
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      TNS-JGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-JGNAGE          =   "AA"
                   MOVE    PTINF-BIRTHDAY      TO  WRK-INYMD
                   MOVE    27                  TO  WRK-ZOGEN
                   PERFORM 800-HIZUKE-KEI-SEC
                   MOVE    WRK-OTYMD           TO  WRK-JGN-YMD
               ELSE
                   MOVE    TNS-JGNAGE          TO  WRK-YY
                   MOVE    PTINF-BIRTHDAY      TO  WRK-JGN-YMD
                   COMPUTE WRK-JGN-YY          =   WRK-JGN-YY
                                               +   WRK-YY
               END-IF
      *        到達日の前日
               MOVE    WRK-JGN-YMD         TO  WRK-INYMD
               MOVE    -1                  TO  WRK-ZOGEN
               PERFORM 800-HIZUKE-KEI-SEC
               MOVE    WRK-OTYMD           TO  WRK-JGN-YMD
           END-IF
      *    対象年月日計算
           MOVE    WRK-KGN-YMD         TO  TBL-KGN-YMD (IDX)
           MOVE    WRK-JGN-YMD         TO  TBL-JGN-YMD (IDX)
           .
       40011-NENREI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    対象テーブル年齢チェック処理
      *****************************************************************
       4001-NYUACT-TBL-SEC            SECTION.
      *
           INITIALIZE                  HZN-NYUIN-AREA
           MOVE    TBL-NYUIN-AREA      TO  HZN-NYUIN-AREA
      *    開始終了期間内で剤を作成する
           MOVE    ZERO                TO  FLG-SYORI-END
           MOVE    WRK-ACT-STYMD       TO  WRK-ACT-STYMD-HZN
           MOVE    WRK-ACT-EDYMD       TO  WRK-ACT-EDYMD-HZN
           MOVE    WRK-ACT-STYMD-HZN   TO  WRK-STYMD-OK
           MOVE    WRK-ACT-EDYMD-HZN   TO  WRK-EDYMD-OK
           PERFORM
               UNTIL   FLG-SYORI-END       =   1
      *        入院会計照会・診療行為作成処理
               PERFORM 40009-TBL-NYUIN-SYORI-SEC
               MOVE    WRK-STYMD-OK        TO  WRK-ACT-STYMD
               MOVE    WRK-EDYMD-OK        TO  WRK-ACT-EDYMD
      *        入院作成処理
               PERFORM 4009-NYUIN-HENSYORI-SEC
      *
               IF      WRK-EDYMD-OK        =   WRK-ACT-EDYMD-HZN
                   MOVE    1               TO  FLG-SYORI-END
               ELSE
      *            前回終了日の翌日から終了日まで
                   MOVE    WRK-EDYMD-OK        TO  WRK-INYMD
                   MOVE    1                   TO  WRK-ZOGEN
                   PERFORM 800-HIZUKE-KEI-SEC
                   MOVE    WRK-OTYMD           TO  WRK-STYMD-OK
                   MOVE    WRK-ACT-EDYMD-HZN   TO  WRK-EDYMD-OK
                   MOVE    WRK-STYMD-OK        TO  WRK-ACT-STYMD
                   MOVE    WRK-EDYMD-OK        TO  WRK-ACT-EDYMD
               END-IF
           END-PERFORM
           .
       4001-NYUACT-TBL-EXT.
           EXIT.
      *****************************************************************
      *    対象年齢チェック処理
      *****************************************************************
       40009-TBL-NYUIN-SYORI-SEC            SECTION.
      *
           INITIALIZE                  TBL-NYUIN-AREA
           MOVE    ZERO                TO  IDX-TBL
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (HZN-SRYCD (IDX) =   SPACE )
               MOVE    ZERO            TO  FLG-CHK
               MOVE    SPACE           TO  WRK-EDYMD-HZN
               MOVE    SPACE           TO  WRK-STYMD-HZN
      *
               IF      HZN-KGN-YMD (IDX)   NOT =   SPACE
      *            下限日が終了日以降は対象外
                   IF      HZN-KGN-YMD (IDX)   >   WRK-ACT-EDYMD
                       MOVE    1               TO  FLG-CHK
                   ELSE
      *            下限日が開始日以降は対象外
                       IF      HZN-KGN-YMD(IDX)    >   WRK-ACT-STYMD
      *********************MOVE    1               TO  FLG-CHK
                           MOVE    HZN-KGN-YMD(IDX)    TO  WRK-STYMD-HZN
                       END-IF
                   END-IF
               END-IF
      *
               IF      HZN-JGN-YMD (IDX)   NOT =   SPACE
      *            上限日が開始日以前は対象外
                   IF      HZN-JGN-YMD (IDX)   <   WRK-ACT-STYMD
                       MOVE    1               TO  FLG-CHK
                   ELSE
      *            上限日が終了日以前は対象外
                       IF      HZN-JGN-YMD(IDX)    <   WRK-ACT-EDYMD
                           MOVE    HZN-JGN-YMD(IDX)    TO  WRK-EDYMD-HZN
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  IDX-TBL
                   MOVE    HZN-NYUIN-OCC (IDX) TO  TBL-NYUIN-OCC
                                                       (IDX-TBL)
                   IF      WRK-EDYMD-HZN   NOT =   SPACE
                       IF      WRK-EDYMD-HZN   <   WRK-EDYMD-OK
                           MOVE    WRK-EDYMD-HZN   TO  WRK-EDYMD-OK
                       END-IF
                   END-IF
      *            開始日変更
                   IF      WRK-STYMD-HZN   NOT =   SPACE
                       IF     (WRK-STYMD-HZN   >   WRK-STYMD-OK ) AND
                              (WRK-STYMD-HZN   <=  WRK-EDYMD-OK )
                           MOVE    WRK-STYMD-HZN   TO  WRK-STYMD-OK
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       40009-TBL-NYUIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計照会・診療行為作成処理
      *****************************************************************
       4009-NYUIN-HENSYORI-SEC         SECTION.
      *
      *    点数計算
           PERFORM 40091-NYUACT-KEISAN-SEC
           IF      WRK-MEISAISU        =   ZERO
               GO      TO      4009-NYUIN-HENSYORI-EXT
           END-IF
      *
      *    対象の会計照会のチェック
           PERFORM 4002-NYUSRYACCT-SYORI-SEC
      *    診療行為追加
           IF      FLG-SINKI           =   1
               PERFORM 4003-NYUSYRACT-SYORI-SEC
           END-IF
      *
           .
       4009-NYUIN-HENSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       40091-NYUACT-KEISAN-SEC            SECTION.
      *
           INITIALIZE                  WRK-SRYACCT-AREA
           MOVE    ZERO                TO  IDX-ATO
           MOVE    ZERO                TO  IDX-SYU
      *    点数算定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (TBL-SRYCD (IDX) =   SPACE )
      *        点数集計
               EVALUATE    TBL-TENSIKIBETU (IDX)
      *            ％加算・減算
                   WHEN    5
                   WHEN    6
                       CONTINUE
      *            点数（マイナス）
                   WHEN    8
                       COMPUTE WRK-ZAITEN-G    =   WRK-ZAITEN-G
                                               +   TBL-TENSU (IDX)
      *            点数
                   WHEN    OTHER
                       COMPUTE WRK-ZAITEN      =   WRK-ZAITEN
                                               +   TBL-TENSU (IDX)
               END-EVALUATE
      *        手技料
               IF      TBL-DATAKBN (IDX)   =   "1"
                   IF      IDX-SYU             =   ZERO
                       MOVE    IDX             TO  IDX-SYU
                   END-IF
               END-IF
      *
      *        各集計
               MOVE    TBL-SRYCD (IDX)     TO  WRK-SRYCD-9
               COMPUTE WRK-SRYCDTOTAL      =   WRK-SRYCDTOTAL
                                           +   WRK-SRYCD-9
               ADD     1                   TO  WRK-MEISAISU
               ADD     1                   TO  WRK-SURYOUTOTAL
               ADD     1                   TO  IDX-ATO
      *
           END-PERFORM
      *
      *    点数集計処理
           MOVE    ZERO                TO  WRK-KASAN
           MOVE    ZERO                TO  WRK-KASAN-ALL
           MOVE    ZERO                TO  WRK-GENZAN
           MOVE    ZERO                TO  WRK-GENZAN-ALL
      *    点数計算
           IF      WRK-ZAITEN-G        >   WRK-ZAITEN
               MOVE    ZERO                TO  WRK-ZAITEN
           ELSE
               COMPUTE WRK-ZAITEN          =   WRK-ZAITEN
                                           -   WRK-ZAITEN-G
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (TBL-SRYCD (IDX) =   SPACE )
               EVALUATE    TBL-TENSIKIBETU (IDX)
      *            ％加算
                   WHEN    5
                       COMPUTE WRK-KASAN   =   WRK-ZAITEN
                                               *   TBL-TENSU  (IDX)
                                               /   100
                                               +   0.5
                       ADD     WRK-KASAN       TO  WRK-KASAN-ALL
      *            ％減算
                   WHEN    6
                       COMPUTE WRK-GENZAN  =   TBL-TENSU  (IDX-SYU)
                                               *   TBL-TENSU  (IDX)
                                               /   100
                       ADD     WRK-GENZAN     TO  WRK-GENZAN-ALL
               END-EVALUATE
           END-PERFORM
      *
      *    点数計算
           COMPUTE WRK-ZAITEN          =   WRK-ZAITEN
                                       +   WRK-KASAN-ALL
                                       -   WRK-GENZAN-ALL
      *
           .
       40091-NYUACT-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計照会のチェック処理
      *****************************************************************
       4002-NYUSRYACCT-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  FLG-OK
      *    診療会計マスターを特定する
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SPA-SRYKA           TO  NACCT-SRYKA
           MOVE    WRK-ACT-STYMD(1:6)  TO  NACCT-SRYYM
           IF     (TBL-SRYCD (01)(2:2) =   "97" )  OR
                  (TBL-SRYCD (01)      =   "099999913")
               MOVE    "97"                TO  WRK-SRYKBN
               MOVE    "970"               TO  WRK-SRYSYUKBN
           ELSE
               MOVE    "90"                TO  WRK-SRYKBN
               MOVE    "900"               TO  WRK-SRYSYUKBN
           END-IF
      *    特定入院料の時、９２０とする
           IF     (LNK-SCNYUACCT-TOKUTEINYUIN-KBN   =   "1" )  AND
                  (WRK-ACT-ZAISKBKBN       =   "1"  OR  "6")
               MOVE    "92"                TO  WRK-SRYKBN
               MOVE    "920"               TO  WRK-SRYSYUKBN
           END-IF
      *
      *    選定療養費の時、９００とする
           IF      FLG-SENTEI              =   1
               MOVE    "90"                TO  WRK-SRYKBN
               MOVE    "900"               TO  WRK-SRYSYUKBN
           END-IF
      *
           MOVE    WRK-SRYKBN          TO  NACCT-SRYKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY3"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY3"    TO  ORC-DBPATH
               PERFORM 930-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           PERFORM
               UNTIL   (FLG-NYUINACCT      =   1    ) OR
                       (FLG-OK             =   1    )
      *
      *        診療コード計を比較する
               IF     (NACCT-SRYCDTOTAL    =   WRK-SRYCDTOTAL ) AND
                      (NACCT-ZAITEN        =   WRK-ZAITEN     ) AND
                      (NACCT-MEISAISU      =   WRK-MEISAISU   ) AND
                      (NACCT-SURYOUTOTAL   =   WRK-SURYOUTOTAL)
                   MOVE    NACCT-ZAINUM        TO  WRK-H-ZAINUM
      *            診療行為内容の比較処理
                   PERFORM 40021-NYUINACCT-CHK-SEC
      *
      *            同一診療会計マスターあり
                   IF      FLG-OK          =   1
      *                新規追加
                       MOVE    WRK-H-ZAINUM        TO  WRK-ZAINUM
                   END-IF
               END-IF
               IF      FLG-OK              =   ZERO
                   MOVE    "NYUINACCT-KEY3"    TO  ORC-DBPATH
                   PERFORM 930-NYUINACCT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY3"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      WRK-ZAINUM          =   ZERO
      *        剤番号取得処理
               PERFORM 40022-ZAINUM-SET-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4002-NYUSRYACCT-SYORI-EXT
               END-IF
           END-IF
      *TEST
      *     DISPLAY "WRK-ZAINUM:" WRK-ZAINUM
      *TEST
      *    診療会計マスター更新用読み込み
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SPA-SRYKA           TO  NACCT-SRYKA
           MOVE    WRK-ACT-STYMD(1:6)  TO  NACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  NACCT-SRYKBN
           MOVE    WRK-ZAINUM          TO  NACCT-ZAINUM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
               PERFORM 930-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-NYUINACCT        =   1
      *        診療会計マスター新規編集処理
               PERFORM 40023-NYUINACCT-SRY-SEC
      *        診療会計マスター編集処理
               PERFORM 40024-NYUINACCT-HEN-SEC
      *
               MOVE    SMCNDATE-YMD        TO  NACCT-CREYMD
               MOVE    SMCNDATE-YMD        TO  NACCT-UPYMD
               MOVE    SMCNDATE-HMS        TO  NACCT-UPHMS
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "NYUINACCT INS ERR KEY:" NACCT-KEY
                                       ",MCP-RC:"   MCP-RC
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF
      *********DISPLAY "NYUINACCT:" NYUINACCT-REC
      *        新規のときのみ、診療行為マスターを追加する
               MOVE    1                   TO  FLG-SINKI
           ELSE
      *        診療会計マスター編集処理
               PERFORM 40024-NYUINACCT-HEN-SEC
      *
               MOVE    SMCNDATE-YMD        TO  NACCT-UPYMD
               MOVE    SMCNDATE-HMS        TO  NACCT-UPHMS
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "NYUINACCT UPDTE ERR KEY:" NACCT-KEY
                                         ",MCP-RC:"   MCP-RC
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF
               MOVE    ZERO                TO  FLG-SINKI
           END-IF
      *
           .
       4002-NYUSRYACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為内容の比較処理
      *****************************************************************
       40021-NYUINACCT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDX-SIN
           MOVE    ZERO                TO  FLG-OK
           INITIALIZE                      TBL-MAE-AREA 
      *    診療会計マスターから診療行為マスターを検索する
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID        TO  NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACT-KEY2"       TO  ORC-DBPATH
               PERFORM 920-NYUINACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           PERFORM UNTIL     (FLG-NYUINACT       =   1  )
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (NSRY-SRYCD(IDX2)    =   SPACE )
      *            入力コードのみの判定とする
                   ADD     1                   TO  IDX-SIN
                   MOVE    NSRY-SRYCD  (IDX2)  TO  TBL-MAE-SRYCD
                                                             (IDX-SIN)
               END-PERFORM
               MOVE    "NYUINACT-KEY2"       TO  ORC-DBPATH
               PERFORM 920-NYUINACT-READ-SEC
           END-PERFORM
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
      *    診療行為数が違う
           IF      IDX-SIN         NOT =   IDX-ATO
               GO      TO      40021-NYUINACCT-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   IDX-ATO  )  OR
                              (FLG-ERR     =   1        )
               MOVE    ZERO                TO  FLG-ARI
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   IDX-SIN  )  OR
                                  (FLG-ARI     =   1        )
                   IF      TBL-SRYCD (IDX1)    =   TBL-MAE-SRYCD (IDX2)
                        MOVE    1                  TO  FLG-ARI
                   END-IF
               END-PERFORM
               IF      FLG-ARI             =   ZERO
                    MOVE    1                  TO  FLG-ERR
               END-IF
           END-PERFORM
           IF      FLG-ERR             =   ZERO
               MOVE    1               TO  FLG-OK
           END-IF
           .
      *
       40021-NYUINACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤番号取得処理
      *****************************************************************
       40022-ZAINUM-SET-SEC          SECTION.
      *
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           MOVE    SPACE               TO  SPA-ERRCD
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "0001"          TO  SPA-ERRCD
               DISPLAY "ORCSNYUACCT ZAINUM-SET ERR:" SPA-PTID 
               GO      TO      40022-ZAINUM-SET-EXT
           END-IF
      *
      *    最大剤番号
           IF      PTINF-MAXZAINUM     =   ALL "9"
               MOVE    ZERO                TO  PTINF-MAXZAINUM
           END-IF
           ADD     1                   TO  PTINF-MAXZAINUM
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSNYUACCT PTINF UPDTE ERR:"   MCP-RC
               MOVE    "0005"              TO  SPA-ERRCD
           END-IF 
      *
           .
      *
       40022-ZAINUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスター新規編集処理
      *****************************************************************
       40023-NYUINACCT-SRY-SEC          SECTION.
      *
           MOVE    SPACE               TO  NYUINACCT-REC
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SPA-SRYKA           TO  NACCT-SRYKA
           MOVE    WRK-ACT-STYMD(1:6)  TO  NACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  NACCT-SRYKBN
           MOVE    WRK-ZAINUM          TO  NACCT-ZAINUM
           MOVE    ZERO                TO  NACCT-HKNCOMBI
      *    剤識別区分
           MOVE    WRK-ACT-ZAISKBKBN   TO  NACCT-ZAISKBKBN
      *    剤点数
           MOVE    WRK-ZAITEN          TO  NACCT-ZAITEN
      *    診療コード計
           MOVE    WRK-SRYCDTOTAL      TO  NACCT-SRYCDTOTAL
      *    数量計
           MOVE    WRK-SURYOUTOTAL     TO  NACCT-SURYOUTOTAL
      *    明細数
           MOVE    WRK-MEISAISU        TO  NACCT-MEISAISU
      *    手技点数１
           MOVE    WRK-ZAITEN          TO  NACCT-SYUTEN1
      *    手技点数２
           MOVE    ZERO                TO  NACCT-SYUTEN2
      *    薬剤点数
           MOVE    ZERO                TO  NACCT-YKZTEN
      *    器材点数
           MOVE    ZERO                TO  NACCT-KIZAITEN
           .
      *
       45015-SRYACCT-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスター編集処理
      *****************************************************************
       40024-NYUINACCT-HEN-SEC          SECTION.
      *
      *    回数セット
           MOVE    WRK-ACT-STYMD(7:2)  TO  IDX1
           MOVE    WRK-ACT-EDYMD(7:2)  TO  IDX2
           PERFORM VARYING     IDX-D   FROM    IDX1   BY  1
                   UNTIL       IDX-D   >   IDX2
               MOVE    WRK-ACT-DAY         TO  NACCT-DAY (IDX-D)
           END-PERFORM
      *
      *    剤回数
           MOVE    ZERO                TO  NACCT-ZAIKAISU
           PERFORM VARYING     IDX-D   FROM    1      BY  1
                   UNTIL       IDX-D   >   31
               IF      NACCT-DAY (IDX-D)   NOT =   ZERO
                   ADD     1                   TO  NACCT-ZAIKAISU
               END-IF
           END-PERFORM
           .
      *
       40024-NYUINACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター処理
      *****************************************************************
       4003-NYUSYRACT-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  IDX-S
      *
      *    新規分のみ、作成する
           INITIALIZE                      NYUINACT-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (TBL-SRYCD (IDX) =   SPACE )
               ADD     1               TO  IDX-S
               IF      IDX-S           >   5
                   PERFORM 40031-NYUINACT-INS-SEC
                   MOVE    1               TO  IDX-S
               END-IF
      *
               MOVE    TBL-SRYCD (IDX)     TO  NSRY-SRYCD    (IDX-S)
               MOVE    1                   TO  NSRY-SRYSURYO (IDX-S)
               MOVE    ZERO                TO  NSRY-SRYKAISU (IDX-S)
               MOVE    SPACE               TO  NSRY-AUTOKBN  (IDX-S)
           END-PERFORM
      *
           IF      IDX-S           >   ZERO
               PERFORM 40031-NYUINACT-INS-SEC
           END-IF
      *
           .
      *
       4003-NYUSYRACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為追加処理
      *****************************************************************
       40031-NYUINACT-INS-SEC            SECTION.
      *
           ADD     1                   TO  WRK-RENNUM
           MOVE    SPA-HOSPID          TO  NSRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  NSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  NSRY-PTID
           MOVE    SPA-SRYKA           TO  NSRY-SRYKA
           MOVE    WRK-ACT-STYMD(1:6)  TO  NSRY-SRYYM
      *     剤番号
           MOVE    WRK-ZAINUM          TO  NSRY-ZAINUM
      *    連番号
           MOVE    WRK-RENNUM          TO  NSRY-RENNUM
      **** IF      TBL-SRYCD (01)(2:2) =   "97"
      *        MOVE    "970"               TO  NSRY-SRYSYUKBN
      *        MOVE    "97"                TO  NSRY-SRYKBN
      *    ELSE
      *        MOVE    "900"               TO  NSRY-SRYSYUKBN
      *        MOVE    "90"                TO  NSRY-SRYKBN
      *****END-IF
           MOVE    WRK-SRYSYUKBN       TO  NSRY-SRYSYUKBN
           MOVE    WRK-SRYKBN          TO  NSRY-SRYKBN
      *
           MOVE    SMCNDATE-YMD        TO  NSRY-CREYMD
           MOVE    SMCNDATE-YMD        TO  NSRY-UPYMD
           MOVE    SMCNDATE-HMS        TO  NSRY-UPHMS
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "NYUINACT-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSNYUACCT NYUINACT INS ERR:" NSRY-KEY 
                       ",MCP-RC:" MCP-RC ":" 
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *****DISPLAY "NYUINACT-REC:" NYUINACT-REC
      *
           INITIALIZE                      NYUINACT-REC
      *
           .
       40031-NYUINACT-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院処理
      *****************************************************************
       200-TAIIN-SYORI-SEC            SECTION.
      *
      *    チェック
           IF      LNK-SCNYUACCT-EDSRYYMD  =   SPACE  OR  ZERO
               GO      TO     200-TAIIN-SYORI-EXT
           END-IF
      *    退院日＋Ｎ日を対象とする
           IF      WRK-ZOGEN           =   ZERO
               MOVE    LNK-SCNYUACCT-EDSRYYMD  TO  WRK-STYMD
           ELSE
               MOVE    LNK-SCNYUACCT-EDSRYYMD
                                           TO  WRK-INYMD
               PERFORM 800-HIZUKE-KEI-SEC
               MOVE    WRK-OTYMD           TO  WRK-STYMD
           END-IF
           MOVE    WRK-STYMD           TO  WRK-RRK-YMD
      *
           MOVE    ZERO                TO  FLG-END
           PERFORM
               UNTIL       FLG-END     =   1
      *        診療会計退院処理
               PERFORM 2001-NYUINACCT-SYORI-SEC
      *
      *        診療会計があった時、次月を設定
               IF      FLG-END             =   ZERO
                   COMPUTE WRK-STMM        =   WRK-STMM    +   1
                   IF      WRK-STMM        >   12
                       COMPUTE WRK-STYY        =   WRK-STYY    +   1
                       MOVE    1               TO  WRK-STMM
                   END-IF
                   MOVE    01                  TO  WRK-STDD
               END-IF
           END-PERFORM
      *
      *    転出の時、削除をしない
           IF      LNK-SCNYUACCT-SYORI-KBN     =   "8"
               GO      TO      200-TAIIN-SYORI-EXT
           END-IF
      *
      *    退院（入院会計のみ）の時、削除をしない
           IF      LNK-SCNYUACCT-SYORI-KBN     =   "4"
               GO      TO      200-TAIIN-SYORI-EXT
           END-IF
      *    次月分の時、削除をしない
           IF      LNK-SCNYUACCT-SYORI-KBN     =   "9"
               GO      TO      200-TAIIN-SYORI-EXT
           END-IF
      *
      *    診療会計をすべて削除する・退院日を最終受診日とする
           INITIALIZE                  ORCSCNYUINAREA
           MOVE    "2"                 TO  ORCSCNYUIN-KBN
           MOVE    WRK-RRK-YMD         TO  ORCSCNYUIN-YMD
           MOVE    LNK-SCNYUACCT-EDSRYYMD
                                       TO  ORCSCNYUIN-TAIINYMD
           CALL    "ORCSCNYUIN"        USING
                                       SPA-AREA
                                       ORCSCNYUINAREA
      *
           .
       200-TAIIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計退院処理
      *****************************************************************
       2001-NYUINACCT-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *    診療会計マスターを特定する
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SPA-SRYKA           TO  NACCT-SRYKA
           MOVE    WRK-STYMD(1:6)      TO  NACCT-SRYYM
      *****MOVE    "90"                TO  NACCT-SRYKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      **** MOVE    "NYUINACCT-KEY3"    TO  ORC-DBPATH
           MOVE    "NYUINACCT-KEY2"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY2"    TO  ORC-DBPATH
               PERFORM 930-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *    診療会計がない時、終了とする
           IF      FLG-NYUINACCT       =   1
               MOVE    1               TO  FLG-END
           END-IF
           PERFORM
               UNTIL    FLG-NYUINACCT      =   1
      *
               PERFORM 20011-NYUINACCT-DELDAY-SEC
      *        回数がゼロか、外泊で月をすべて削除する時
               IF    ((NACCT-ZAISKBKBN NOT =   "2"  )  AND
                      (NACCT-ZAIKAISU      =   ZERO ))   OR
                     ((NACCT-ZAISKBKBN     =   "2"  )  AND
                      (NACCT-ZAIKAISU      =   ZERO )  AND
                      (WRK-STDD            =   01   ))
      *            退院月の時は削除しない
      *            （転出以外の時）
                   IF     (NACCT-SRYYM(1:6)    =   WRK-RRK-YMD(1:6))
                     AND  (WRK-RRK-YMD(7:2)        NOT =   "01" )
                     AND  (LNK-SCNYUACCT-SYORI-KBN NOT =   "8" )
      *                 診療会計更新
                       PERFORM 20013-NYUINACCT-UPDATE-SEC
                   ELSE
      *                診療会計削除
                       PERFORM 20012-NYUINACCT-DELETE-SEC
                   END-IF
               ELSE
      *            診療会計更新
                   PERFORM 20013-NYUINACCT-UPDATE-SEC
               END-IF
      *
               MOVE    "NYUINACCT-KEY2"    TO  ORC-DBPATH
               PERFORM 930-NYUINACCT-READ-SEC
           END-PERFORM
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY2"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       2001-NYUINACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスター編集処理
      *****************************************************************
       20011-NYUINACCT-DELDAY-SEC          SECTION.
      *
      *    回数セット
           PERFORM VARYING     IDX-D   FROM    WRK-STDD   BY  1
                   UNTIL       IDX-D   >   31
               MOVE    ZERO                TO  NACCT-DAY (IDX-D)
           END-PERFORM
      *
      *    剤回数
           MOVE    ZERO                TO  NACCT-ZAIKAISU
           PERFORM VARYING     IDX-D   FROM    1      BY  1
                   UNTIL       IDX-D   >   31
               IF      NACCT-DAY (IDX-D)   NOT =   ZERO
                   ADD     1                   TO  NACCT-ZAIKAISU
               END-IF
           END-PERFORM
           .
      *
       20011-NYUINACCT-DELDAY-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計更新処理
      *****************************************************************
       20013-NYUINACCT-UPDATE-SEC          SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  NACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  NACCT-UPHMS
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSNYUACCT NYUINACCT UP  ERR:" NACCT-KEY 
                                       ",MCP-RC:" MCP-RC ":" 
           END-IF
           .
      *
       20013-NYUINACCT-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスター削除処理
      *****************************************************************
       20012-NYUINACCT-DELETE-SEC          SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID        TO  NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACT-KEY2"       TO  ORC-DBPATH
               PERFORM 920-NYUINACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           IF      FLG-NYUINACT       =    ZERO
               MOVE    NYUINACT-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "NYUINACT-DEL11"    TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "ORCSNYUACCT NYUINACT DEL ERR:" NSRY-KEY 
                          ",MCP-RC:" MCP-RC ":" 
               END-IF
           END-IF
      *
      *    診療会計削除
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSNYUACCT NYUINACCT DEL ERR:" NACCT-KEY 
                       ",MCP-RC:" MCP-RC ":" 
           END-IF
      *
           .
      *
       20012-NYUINACCT-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    入院取消処理
      *****************************************************************
       500-NYUINDEL-SYORI-SEC            SECTION.
      *
      *    チェック
           IF      LNK-SCNYUACCT-STSRYYMD  =   SPACE  OR  ZERO
               GO      TO     500-NYUINDEL-SYORI-EXT
           END-IF
      *
      *    開始日からすべてを削除する
           MOVE    LNK-SCNYUACCT-STSRYYMD
                                       TO  WRK-STYMD
      *
           MOVE    ZERO                TO  FLG-END
           PERFORM
               UNTIL       FLG-END     =   1
               MOVE    ZERO                TO  WRK-CNT
               MOVE    ZERO                TO  FLG-DELETE-OK
      *        診療会計処理
               PERFORM 5001-NYUINACCT-DEL-SEC
      *
      *        診療会計があった時、次月を設定
               IF      FLG-END             =   ZERO
                   COMPUTE WRK-STMM        =   WRK-STMM    +   1
                   IF      WRK-STMM        >   12
                       COMPUTE WRK-STYY        =   WRK-STYY    +   1
                       MOVE    1               TO  WRK-STMM
                   END-IF
                   MOVE    01                  TO  WRK-STDD
               END-IF
           END-PERFORM
      *
      *    診療会計をすべて削除する
           IF      LNK-SCNYUACCT-SYORI-KBN     =   "6"
               INITIALIZE                  ORCSCNYUINAREA
               MOVE    "2"                 TO  ORCSCNYUIN-KBN
               MOVE    LNK-SCNYUACCT-STSRYYMD
                                           TO  ORCSCNYUIN-YMD
               CALL    "ORCSCNYUIN"        USING
                                           SPA-AREA
                                           ORCSCNYUINAREA
           END-IF
      *
           .
      *
       500-NYUINDEL-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計退院処理
      *****************************************************************
       5001-NYUINACCT-DEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *    診療会計マスターを特定する
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SPA-SRYKA           TO  NACCT-SRYKA
           MOVE    WRK-STYMD(1:6)      TO  NACCT-SRYYM
      **** MOVE    WRK-SRYKBN          TO  NACCT-SRYKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY15"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY15"   TO  ORC-DBPATH
               PERFORM 930-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *    診療会計がない時、終了とする
           IF      FLG-NYUINACCT       =   1
               MOVE    1               TO  FLG-END
           END-IF
           PERFORM
               UNTIL   FLG-NYUINACCT       =   1
               MOVE    ZERO                TO  FLG-DELETE
               IF      LNK-SCNYUACCT-SYORI-KBN     =   "5"
      *            入院取消の時、入院・入院加算のみ削除する
                   IF      NACCT-ZAISKBKBN     =   "1"  OR  "6"
                       MOVE    1               TO  FLG-DELETE
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-DELETE
               END-IF
      *
               IF      FLG-DELETE          =   1
                   ADD     1               TO  WRK-CNT
      *            回数削除
                   PERFORM 20011-NYUINACCT-DELDAY-SEC
      *            回数がゼロか、外泊で月をすべて削除する時
                   IF    ((NACCT-ZAISKBKBN NOT =   "2"  )  AND
                          (NACCT-ZAIKAISU      =   ZERO ))   OR
                         ((NACCT-ZAISKBKBN     =   "2"  )  AND
                          (NACCT-ZAIKAISU      =   ZERO )  AND
                          (FLG-DELETE-OK       =   1    ))
      *                診療会計削除
                       PERFORM 20012-NYUINACCT-DELETE-SEC
      *                最初の剤が削除の時、外泊も削除とする
                       IF      WRK-CNT         =   1
                           MOVE    1               TO  FLG-DELETE-OK
                       END-IF
                   ELSE
      *                診療会計更新
                       MOVE    SMCNDATE-YMD        TO  NACCT-UPYMD
                       MOVE    SMCNDATE-HMS        TO  NACCT-UPHMS
      *
                       MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"         USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "ORCSNYUACCT NYUINACCT UP  ERR:"
                                   NACCT-KEY 
                                  ",MCP-RC:" MCP-RC ":" 
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "NYUINACCT-KEY15"   TO  ORC-DBPATH
               PERFORM 930-NYUINACCT-READ-SEC
           END-PERFORM
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY15"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *    入院取消の時外泊は残す
           IF      LNK-SCNYUACCT-SYORI-KBN     =   "5"
               GO      TO      5001-NYUINACCT-DEL-EXT
           END-IF
      *
      *    外泊削除
           MOVE    ZERO                TO  WRK-CNT
      *    診療会計マスターを特定する
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SPA-SRYKA           TO  NACCT-SRYKA
           MOVE    WRK-STYMD(1:6)      TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY15"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY15"   TO  ORC-DBPATH
               PERFORM 930-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           MOVE    ZERO                TO  FLG-DELETE
           PERFORM
               UNTIL  (FLG-NYUINACCT       =   1 )  OR
                      (FLG-DELETE          =   1 )
               ADD     1                   TO  WRK-CNT
      *        回数がゼロか、外泊で月をすべて削除する時
               IF      NACCT-ZAISKBKBN     =   "2"
                   MOVE    NYUINACCT-REC       TO  HZN-NYUINACCT-REC
               ELSE
                   MOVE    1               TO  FLG-DELETE
               END-IF
      *
               MOVE    "NYUINACCT-KEY15"   TO  ORC-DBPATH
               PERFORM 930-NYUINACCT-READ-SEC
           END-PERFORM
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY15"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      (FLG-DELETE          =   ZERO )  AND
                   (WRK-CNT             =   1    )
               MOVE    HZN-NYUINACCT-REC    TO  NYUINACCT-REC
      *        診療会計削除
               PERFORM 20012-NYUINACCT-DELETE-SEC
           END-IF
      *
           .
       5001-NYUINACCT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    退院取消処理
      *****************************************************************
       700-TAIINDEL-SYORI-SEC            SECTION.
      *
      *    チェック
           IF      LNK-SCNYUACCT-STSRYYMD  =   SPACE  OR  ZERO
               GO      TO     700-TAIINDEL-SYORI-EXT
           END-IF
           IF      LNK-SCNYUACCT-EDSRYYMD  =   SPACE  OR  ZERO
               GO      TO     700-TAIINDEL-SYORI-EXT
           END-IF
      *
      *    入院期間作成
      *    退院日＋１日を開始日とする
           MOVE    LNK-SCNYUACCT-EDSRYYMD
                                       TO  WRK-INYMD
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-HIZUKE-KEI-SEC
           MOVE    WRK-OTYMD           TO  WRK-STYMD
      *    開始日の翌月の末日を求める
           PERFORM 100-INIT-SEC
      *
      *    入院から退院までの期間を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    LNK-SCNYUACCT-STSRYYMD
                                       TO  LNK-DAY5-START
           MOVE    LNK-SCNYUACCT-EDSRYYMD
                                       TO  LNK-DAY5-END
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY5-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY5-NISSU2     TO  WRK-NISUU
           ELSE
               MOVE    ZERO                TO  WRK-NISUU
           END-IF
      *
      *    入院基本料作成
           PERFORM 7001-NYUKHN-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     700-TAIINDEL-SYORI-EXT
           END-IF
      *
      *    入院料加算作成
           PERFORM 210-NYUKHNKSN-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     700-TAIINDEL-SYORI-EXT
           END-IF
      *
      *    その他料作成
           PERFORM 220-SONOTA-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     700-TAIINDEL-SYORI-EXT
           END-IF
           .
      *
       700-TAIINDEL-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料作成処理
      *****************************************************************
       7001-NYUKHN-SYORI-SEC            SECTION.
      *
           MOVE    WRK-STYMD           TO  WRK-KHN-STYMD
           MOVE    WRK-EDYMD           TO  WRK-KHN-EDYMD
           MOVE    WRK-STYMD           TO  WRK-ENDYMD
      *
      *    加算により剤を決定する
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL       IDX-K   >   7
      *      開始日が対象であること
      *****  IF     (LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
      *****                            >=  LNK-SCNYUACCT-STSRYYMD) AND
             IF     (LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               <=  WRK-EDYMD  )   AND
                    (LNK-SCNYUACCT-KAGEN-EDYUKYMD (IDX-K)
                                               >=  WRK-STYMD)
      *
               IF      LNK-SCNYUACCT-KAGEN-SRYCD (IDX-K) =   SPACE
                   IF      WRK-EDYMD           >=  WRK-ENDYMD
                       MOVE    WRK-ENDYMD      TO  WRK-KHN-STYMD
                       MOVE    WRK-EDYMD       TO  WRK-KHN-EDYMD
                       MOVE    SPACE           TO  WRK-KAGEN-SRYCD
                       PERFORM 2001-NYUKHN-HENSYU-SEC
                   END-IF
                   MOVE    7                   TO  IDX-K
               ELSE
      *            対象期間により決定する
                   IF      LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               >   WRK-ENDYMD
      *                前回分より以降の時、次の前日まで
                       MOVE    WRK-ENDYMD      TO  WRK-KHN-STYMD
                       MOVE    LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               TO  WRK-INYMD
                       MOVE    -1              TO  WRK-ZOGEN
                       PERFORM 800-HIZUKE-KEI-SEC
                       MOVE    WRK-OTYMD       TO  WRK-KHN-EDYMD
                       MOVE    SPACE           TO  WRK-KAGEN-SRYCD
                       PERFORM 2001-NYUKHN-HENSYU-SEC
                   END-IF
                   IF      LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               <   WRK-STYMD
                       MOVE    WRK-STYMD       TO  WRK-KHN-STYMD
                   ELSE
                       MOVE    LNK-SCNYUACCT-KAGEN-STYUKYMD (IDX-K)
                                               TO  WRK-KHN-STYMD
                   END-IF
                   MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD (IDX-K)
                                               TO  WRK-KHN-EDYMD
                   MOVE    LNK-SCNYUACCT-KAGEN-SRYCD (IDX-K)
                                               TO  WRK-KAGEN-SRYCD
                   PERFORM 2001-NYUKHN-HENSYU-SEC
               END-IF
             END-IF
           END-PERFORM
      *
      *    残りの日で作成する
           IF      WRK-EDYMD           >=  WRK-ENDYMD
               MOVE    WRK-ENDYMD      TO  WRK-KHN-STYMD
               MOVE    WRK-EDYMD       TO  WRK-KHN-EDYMD
               MOVE    SPACE           TO  WRK-KAGEN-SRYCD
               PERFORM 2001-NYUKHN-HENSYU-SEC
           END-IF
      *
           .
       7001-NYUKHN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    転出処理
      *****************************************************************
       800-TENSYUTU-SYORI-SEC            SECTION.
      *
      *    チェック
           IF      LNK-SCNYUACCT-STSRYYMD  =   SPACE  OR  ZERO
               GO      TO     800-TENSYUTU-SYORI-EXT
           END-IF
           IF      LNK-SCNYUACCT-EDSRYYMD  =   SPACE  OR  ZERO
               GO      TO     800-TENSYUTU-SYORI-EXT
           END-IF
      *
      *    終了日から退院処理をする
      *    転科時元入院科を診療科へ
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    LNK-SCNYUACCT-MOTO-NYUINKA
                                       TO  SPA-SRYKA
           MOVE    ZERO                TO  WRK-ZOGEN
           PERFORM 200-TAIIN-SYORI-SEC
      *    診療科を戻す
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
      *
      *    退院日から新規入院を作成する
           MOVE    LNK-SCNYUACCT-EDSRYYMD
                                       TO  WRK-STYMD
      *    入院期間作成
           PERFORM 100-INIT-SEC
      *
      *    入院基本料作成
           PERFORM 200-NYUKHN-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     800-TENSYUTU-SYORI-EXT
           END-IF
      *
      *    入院料加算作成
           MOVE    ZERO                TO  WRK-NISUU
           PERFORM 210-NYUKHNKSN-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     800-TENSYUTU-SYORI-EXT
           END-IF
      *
      *    その他料作成
           PERFORM 220-SONOTA-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     800-TENSYUTU-SYORI-EXT
           END-IF
           .
      *
       800-TENSYUTU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次月分作成処理
      *****************************************************************
       900-NEXTTUKI-SYORI-SEC            SECTION.
      *
      *    チェック
           IF      LNK-SCNYUACCT-STSRYYMD  =   SPACE  OR  ZERO
               GO      TO     900-NEXTTUKI-SYORI-EXT
           END-IF
      *    終了日から退院処理をする
      *    転科時元入院科を診療科へ
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    LNK-SCNYUACCT-MOTO-NYUINKA
                                       TO  SPA-SRYKA
           MOVE    ZERO                TO  WRK-ZOGEN
           PERFORM 200-TAIIN-SYORI-SEC
      *    診療科を戻す
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
      *
      *    開始日の末日を求める
           MOVE    LNK-SCNYUACCT-STSRYYMD
                                       TO  WRK-STYMD
           MOVE    WRK-STYMD           TO  WRK-EDYMD
      *    月末算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-EDYYMM          TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
           END-IF
      *
      *    入院基本料作成
           PERFORM 200-NYUKHN-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     900-NEXTTUKI-SYORI-EXT
           END-IF
      *
      *    入院料加算作成
           MOVE    ZERO                TO  WRK-NISUU
           PERFORM 210-NYUKHNKSN-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     900-NEXTTUKI-SYORI-EXT
           END-IF
      *
      *    その他料作成
           PERFORM 220-SONOTA-SYORI-SEC
           IF      LNK-SCNYUACCT-RC    NOT =   ZERO
               GO      TO     900-NEXTTUKI-SYORI-EXT
           END-IF
           .
      *
       900-NEXTTUKI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    日付変換処理
      *****************************************************************
       800-HIZUKE-KEI-SEC            SECTION.
      *
      *    Ｎ日後算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-INYMD           TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-OTYMD
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-OTYMD2
           ELSE
               MOVE    WRK-INYMD           TO  WRK-OTYMD
               MOVE    WRK-INYMD           TO  WRK-OTYMD2
           END-IF
      *
           .
       800-HIZUKE-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           MOVE    WRK-YMD             TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       920-NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       920-NYUINACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           .
       930-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       940-PTINF-READ-EXT.
           EXIT.
      *
      *
