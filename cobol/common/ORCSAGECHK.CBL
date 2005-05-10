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
       PROGRAM-ID.             ORCSAGECHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 保険算定用年齢・割合処理
      *  返却エラーコード  : 00:正常　99:パラメータ不正・対象データなし
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/09/10    NACL- 多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/10/01  生活保護追加
      * 01.00.02     NACL-多々納  02/10/07  船員本人追加
      *                                     ７５歳以上の老人公費有無追加
      * 01.00.03     NACL-多々納  02/11/21  共済下船３月追加
      * 01.00.04     NACL-多々納  03/02/05  原爆単独老人判定をしない
      * 01.00.05     NACL-多々納  03/03/06  ３歳未満判定に自費・地験をはずす
      * 01.00.06     NACL-多々納  03/04/14  労災の割合変更追加
      * 01.00.07     NACL-多々納  03/05/12  自賠責割合変更追加
      * 01.00.08     NACL-多々納  03/09/03  公費期限チェック追加
      * 01.00.09     NACL-多々納  03/11/13  老人区分追加
      * 01.00.10     NACL-多々納  04/01/06  包括まとめ入力追加
      * 01.00.11     NACL-多々納  04/04/21  老人区分２、保険区分追加
      *                                     実年齢追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNNUM-OK       PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-HKN-ERR         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-BYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
           03  WRK-NENREI-YY           PIC 9(03).
           03  WRK-NENREI-MM           PIC 9(02).
           03  WRK-NENREI-DD           PIC 9(02).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
      *
           03  WRK-ZZ9                 PIC ZZZ.
      *
           03  WRK-SEIDONAME           PIC X(20).
      *
           03  WRK-BIRTHDAY            PIC X(08).
           03  WRK-ROU-NEN             PIC 9(03).
      *
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSAGECHK.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSAGECHKAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  AGECHK-OUT-AREA
           INITIALIZE                  AGECHK-RC
      *
           IF     (AGECHK-HOSPID       =   SPACE   ) OR
                  (AGECHK-PTID         NOT NUMERIC )
               MOVE    99              TO  AGECHK-RC
               GO      TO      000-PROC-EXT
           END-IF
      *    年齢計算
           PERFORM 100-NENREI-SEC
           IF      AGECHK-RC       NOT =   ZERO
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    保険組合番号があるとき、負担割合を算定する
           IF      AGECHK-HKNCOMBINUM  NOT =   SPACE
               PERFORM 200-FTNRATE-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       100-NENREI-SEC                SECTION.
      *
      *    患者生年月日
           IF      AGECHK-BIRTHDAY     =   SPACE
      *        患者情報検索
               INITIALIZE                      PTINF-REC
               MOVE    AGECHK-HOSPID       TO  PTINF-HOSPID
               MOVE    AGECHK-PTID         TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   1
                   MOVE    99              TO  AGECHK-RC
                   GO      TO      100-NENREI-EXT
               END-IF
               MOVE    PTINF-BIRTHDAY      TO  WRK-BYMD
               MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           ELSE
               MOVE    AGECHK-BIRTHDAY     TO  WRK-BYMD
               MOVE    AGECHK-BIRTHDAY     TO  WRK-BIRTHDAY
           END-IF
      *
      *    当日の年齢を計算する
           MOVE    AGECHK-SRYYMD       TO  WRK-SYMD
           COMPUTE WRK-NENREI-YY       =   WRK-SYY
                                       -   WRK-BYY
           IF      WRK-BYMD(5:4)       >   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI-YY       =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM       =  (WRK-SMM   +  12 )
                                           -   WRK-BMM
           ELSE
               COMPUTE WRK-NENREI-MM       =   WRK-SMM
                                           -   WRK-BMM
           END-IF
           IF      WRK-BDD             >   WRK-SDD
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                       -   1
           END-IF
      *    年齢期間算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-BIRTHDAY        TO  LNK-DAY5-START
           MOVE    AGECHK-SRYYMD       TO  LNK-DAY5-END
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY5-AREA
           IF      LNK-DAY5-NISSU2     <=  31
               MOVE    LNK-DAY5-NISSU2     TO  WRK-NENREI-DD
               MOVE    ZERO                TO  WRK-NENREI-MM
           END-IF
           MOVE    WRK-NENREI-YY       TO  AGECHK-NENREI2-YY
           MOVE    WRK-NENREI-MM       TO  AGECHK-NENREI2-MM
           MOVE    WRK-NENREI-DD       TO  AGECHK-NENREI2-DD
      *
      *保険年齢計算
      *    １日以外の誕生日の時は、翌月１日を基準とする
           IF      WRK-BDD             >   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY         =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01              TO  WRK-BDD
           END-IF
      *
      *    保険年齢を計算する
           MOVE    AGECHK-SRYYMD       TO  WRK-SYMD
           COMPUTE WRK-NENREI-YY       =   WRK-SYY
                                       -   WRK-BYY
           IF      WRK-BYMD(5:4)       >   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI-YY       =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM       =  (WRK-BMM   +  12 )
                                           -   WRK-SMM
           ELSE
               COMPUTE WRK-NENREI-MM       =   WRK-BMM
                                           -   WRK-SMM
           END-IF
           MOVE    WRK-NENREI-YY       TO  AGECHK-NENREI
      *
      *    年齢での老人判定
      *    Ｈ１４．１０以降７０歳を７５歳とする
      *    Ｈ１４．１０時点で７０歳未満の時
           IF     (AGECHK-SRYYMD       >=   "20021001" )  AND
                  (WRK-BIRTHDAY        >=   "19321001" )
               MOVE    75                   TO  WRK-ROU-NEN
           ELSE
               MOVE    70                   TO  WRK-ROU-NEN
           END-IF
      *    ７０歳以上は老人とする（該当月１日に７０歳）
      *    保険年齢計算判定
           IF      AGECHK-NENREI       >=  WRK-ROU-NEN
               MOVE    1               TO  AGECHK-ROUJIN
           END-IF
      *    ６５歳以上の時、老人公費をもっているかの判定
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20010-ROUJIN-CHK2-SEC
                   END-IF
               ELSE
                   MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
               END-IF
           END-IF
      *
           .
       100-NENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担割合を算定処理
      *****************************************************************
       200-FTNRATE-SEC                SECTION.
      *
      *    包括まとめ入力
           IF      AGECHK-HKNCOMBINUM  =   "9999"
               PERFORM 2009-HOUKATUHKN-SEC
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    AGECHK-HOSPID       TO  COMB-HOSPID
           MOVE    AGECHK-PTID         TO  COMB-PTID
           MOVE    AGECHK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           IF      FLG-HKNCOMBI        =   1
               MOVE    99              TO  AGECHK-RC
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
           MOVE    ZERO               TO  AGECHK-HKNKBN
      *
      *    労災の時、割合の変更
           IF      COMB-HKNNUM         =   "971"
               PERFORM 2001-RSI-FTNRATE-SEC
               MOVE    1               TO  AGECHK-HKNKBN
           END-IF
      *
      *    自賠責の時、割合の変更
           IF      COMB-HKNNUM         =   "973"
               PERFORM 2002-JIBAI-FTNRATE-SEC
               MOVE    2               TO  AGECHK-HKNKBN
           END-IF
      *
           IF      AGECHK-NYUGAIKBN    =   "1"
               MOVE    COMB-NYU-KFTNRATE   TO  AGECHK-FTNRATE
           ELSE
               MOVE    COMB-GAI-KFTNRATE   TO  AGECHK-FTNRATE
           END-IF
           IF      AGECHK-FTNRATE      =   ZERO
               MOVE    SPACE               TO  AGECHK-FTNRATEMEI
           ELSE
               MOVE    AGECHK-FTNRATE      TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  AGECHK-FTNRATEMEI(1:3)
               MOVE    "%"                 TO  AGECHK-FTNRATEMEI(4:)
           END-IF
      *    Ｈ１４．１０改正後
      *    ３歳未満の時、２割を上限とする
           IF     (AGECHK-SRYYMD       >=  "20021001") AND
                  (AGECHK-NENREI       <   03        )
      *        主保険が自費以外の時
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               PERFORM 3001-HKNNUM-CHK-SEC
      *        自費・治験・労災・自賠責
               IF     (HKN-HKNKOHSBTKBN    =   "8"  OR  "9" )  OR
                      (COMB-HKNNUM         =   "971"  OR  "973")
                   CONTINUE
      *            自費
                   IF      HKN-HKNKOHSBTKBN    =   "8"
                       MOVE    3               TO  AGECHK-HKNKBN
                   END-IF
               ELSE
                   IF      AGECHK-FTNRATE      >   20
                       MOVE    20                  TO  AGECHK-FTNRATE
                       MOVE    " 20%"              TO  AGECHK-FTNRATEMEI
                   END-IF
               END-IF
           END-IF
      *
      *    老人判定
           PERFORM 2009-ROUJIN-HANTEI-SEC
      *
      *    Ｈ１４．１０改正後以下のチェックを行う
           IF      AGECHK-SRYYMD       >=  "20021001"
               CONTINUE
           ELSE
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    Ｈ１５．４改正後以下のチェックを行う
           IF      AGECHK-SRYYMD       >=  "20030401"
               PERFORM 400-PTHKNCHK-SEC
               IF      AGECHK-HKNCHK   NOT =   ZERO
                   GO      TO      200-FTNRATE-EXT
               END-IF
      *        公費の期限切れチェック
               PERFORM 410-PTKOHCHK-SEC
               IF      AGECHK-HKNCHK   NOT =   ZERO
                   GO      TO      200-FTNRATE-EXT
               END-IF
           END-IF
      *
      *
      *    ７０歳以上の時、老人であること
           IF      AGECHK-NENREI       <   70
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    主保険判定
           MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
           MOVE    ZERO                TO  WRK-PAYKBN
           PERFORM 3001-HKNNUM-CHK-SEC
           IF      FLG-HKNNUM          =   ZERO
      *        労災・自費・治験
               IF      HKN-HKNKOHSBTKBN    =   "4"  OR  "8"  OR  "9"
                   MOVE    1                   TO  FLG-OK
                   GO      TO      200-FTNRATE-EXT
               END-IF
      *        保険者編集
               INITIALIZE                  PTHKNINF-REC
               MOVE    AGECHK-HOSPID       TO  PTHKN-HOSPID
               MOVE    AGECHK-PTID         TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
                   PERFORM 910-PTHKNINF-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
               IF      FLG-PTHKNINF        =   ZERO
                   IF     (PTHKN-HOJOKBN       =   "8"  OR  "9") OR
                          ((COMB-HKNNUM        =   "060"   )  AND
                           (PTHKN-HOJOKBN      =   "1"  OR  "2"
                                               OR  "4"  OR  "5"))
                       MOVE    1               TO  FLG-OK
                   END-IF
      *            船員で補助区分のある時ＯＫとする
                   IF     (COMB-HKNNUM         =   "002" ) AND
                          (PTHKN-HOJOKBN   NOT =   SPACE )
                       IF      PTHKN-HOJOKBN       >=  "A" AND
                                                   <=  "F"
                           MOVE    2               TO  FLG-OK
                       ELSE
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-IF
      *            共済で老人区分のある時ＯＫとする
                   IF     (COMB-HKNNUM         =   "031"  OR
                                                   "032"  OR
                                                   "033"  OR
                                                   "034" ) AND
                          (PTHKN-HOJOKBN       =   "B"  OR  "E")
                       MOVE    2               TO  FLG-OK
                   END-IF
               END-IF
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           END-IF
      **** IF      FLG-OK                  =   1
      *        GO      TO      200-FTNRATE-EXT
      **** END-IF
      *
      *    生活保護のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "012" )  OR
                  (COMB-KOH2HKNNUM     =   "012" )  OR
                  (COMB-KOH3HKNNUM     =   "012" )  OR
                  (COMB-KOH4HKNNUM     =   "012" ))
               MOVE    1               TO  FLG-OK2
      *********GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    原爆のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "018" )  OR
                  (COMB-KOH2HKNNUM     =   "018" )  OR
                  (COMB-KOH3HKNNUM     =   "018" )  OR
                  (COMB-KOH4HKNNUM     =   "018" ))
               MOVE    1               TO  FLG-OK2
           END-IF
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "019" )  OR
                  (COMB-KOH2HKNNUM     =   "019" )  OR
                  (COMB-KOH3HKNNUM     =   "019" )  OR
                  (COMB-KOH4HKNNUM     =   "019" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    結３５のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "011" )  OR
                  (COMB-KOH2HKNNUM     =   "011" )  OR
                  (COMB-KOH3HKNNUM     =   "011" )  OR
                  (COMB-KOH4HKNNUM     =   "011" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    精神通院のみ時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "021" )  OR
                  (COMB-KOH2HKNNUM     =   "021" )  OR
                  (COMB-KOH3HKNNUM     =   "021" )  OR
                  (COMB-KOH4HKNNUM     =   "021" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    老人公費で、支払区分がある時
           IF    ((COMB-KOH1HKNNUM     =   "027" )  AND
                  (COMB-KOH1PAYKBN     =   "01" OR "02" ))  OR
                 ((COMB-KOH2HKNNUM     =   "027" )  AND
                  (COMB-KOH2PAYKBN     =   "01" OR "02"  ))  OR
                 ((COMB-KOH3HKNNUM     =   "027" )  AND
                  (COMB-KOH3PAYKBN     =   "01" OR "02" ))  OR
                 ((COMB-KOH4HKNNUM     =   "027" )  AND
                  (COMB-KOH4PAYKBN     =   "01" OR "02" ))
               MOVE    1               TO  FLG-OK2
           ELSE
      *        公費１
               IF      COMB-KOH1HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH1HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH1PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                           PERFORM 2001-KOH-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *        公費２
               IF      COMB-KOH2HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH2HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH2PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                           PERFORM 2001-KOH-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *        公費３
               IF      COMB-KOH3HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH3HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH3PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                           PERFORM 2001-KOH-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *        公費４
               IF      COMB-KOH4HKNNUM     NOT =   SPACE
                   MOVE    COMB-KOH4HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOH4PAYKBN     TO  WRK-PAYKBN
                   PERFORM 3001-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                           PERFORM 2001-KOH-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *
           END-IF
      *
           IF      FLG-OK              =   ZERO   AND
                   FLG-OK2             =   ZERO
               MOVE    1               TO  AGECHK-RJNCHK
               GO      TO      200-FTNRATE-EXT
           END-IF
      *    ７５歳以上の時、老人公費があること
      **** IF     (AGECHK-NENREI       >=  75 )  AND
           IF     (AGECHK-NENREI       >=  WRK-ROU-NEN )  AND
                  (FLG-OK2             =   ZERO)
               MOVE    2               TO  AGECHK-RJNCHK
           END-IF
      *
      *
           .
       200-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報処理
      *****************************************************************
       2001-KOH-CHK-SEC         SECTION.
      *
           MOVE    1                   TO  FLG-OK2
           .
       2001-KOH-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    労災の時、割合の変更処理
      *****************************************************************
       2001-RSI-FTNRATE-SEC         SECTION.
      *
      *    システム管理検索（労災情報）
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    AGECHK-SRYYMD       TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
      *
               IF      SYS-4001-RSIIRYOKBN     =   "1"
                   MOVE    ZERO                TO  COMB-GAI-KFTNRATE
                   MOVE    ZERO                TO  COMB-NYU-KFTNRATE
               ELSE
                   MOVE    100                 TO  COMB-GAI-KFTNRATE
                   MOVE    100                 TO  COMB-NYU-KFTNRATE
               END-IF
           END-IF
      *
           .
       2001-RSI-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責割合の変更処理
      *****************************************************************
       2002-JIBAI-FTNRATE-SEC         SECTION.
      *
      *    患者保険情報検索
           INITIALIZE                      PTRSIINF-REC
           MOVE    COMB-HOSPID         TO  PTRSI-HOSPID
           MOVE    COMB-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
      *        自賠責患者請求区分
           IF     (FLG-PTRSIINF        =   ZERO )  AND
                  (PTRSI-HKNKBN        =   "4"  )
               EVALUATE    PTRSI-JIBAISEIKBN
                   WHEN    "1"
                       MOVE    100             TO  COMB-GAI-KFTNRATE
                       MOVE    100             TO  COMB-NYU-KFTNRATE
                   WHEN    "2"
                       MOVE    ZERO            TO  COMB-GAI-KFTNRATE
                       MOVE    ZERO            TO  COMB-NYU-KFTNRATE
               END-EVALUATE
           END-IF
           .
      *
       2002-JIBAI-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人・一般定処理
      *****************************************************************
       2009-ROUJIN-HANTEI-SEC         SECTION.
      *
           MOVE    ZERO                TO  AGECHK-ROUJIN2
      *    生活保護で、６５歳以上で老人公費を持っている場合、老人
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "012" )  OR
                  (COMB-KOH2HKNNUM     =   "012" )  OR
                  (COMB-KOH3HKNNUM     =   "012" )  OR
                  (COMB-KOH4HKNNUM     =   "012" ))
               IF     (AGECHK-NENREI       >=  65  )  AND
                      (AGECHK-ROUJIN       =   ZERO)
                   PERFORM 2009-ROUJIN-CHK-SEC
               END-IF
           ELSE
      *        老人公費を選択している場合、老人
               IF     (COMB-KOH1HKNNUM     =   "027" )  OR
                      (COMB-KOH2HKNNUM     =   "027" )  OR
                      (COMB-KOH3HKNNUM     =   "027" )  OR
                      (COMB-KOH4HKNNUM     =   "027" )
                   MOVE    1               TO  AGECHK-ROUJIN
               END-IF
           END-IF
      *    ６５歳以上の時、老人公費をもっているかの判定
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20010-ROUJIN-CHK2-SEC
                   END-IF
               ELSE
                   MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
               END-IF
           END-IF
      *
      *    労災・自賠責の時、すべて一般へ
           IF      COMB-HKNNUM         =   "971"
                                       OR  "973"
               MOVE    ZERO            TO  AGECHK-ROUJIN
           END-IF
      *
           .
       2009-ROUJIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費判定処理
      *****************************************************************
       2009-ROUJIN-CHK-SEC         SECTION.
      *
      *    公費編集
           INITIALIZE                  PTKOHINF-REC
           MOVE    AGECHK-HOSPID       TO  PTKOH-HOSPID
           MOVE    AGECHK-PTID         TO  PTKOH-PTID
           MOVE    "027"               TO  PTKOH-KOHNUM
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    1                   TO  AGECHK-ROUJIN
               MOVE    1                   TO  AGECHK-ROUJIN2
           END-IF
      *
           .
      *
       2009-ROUJIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費判定処理
      *****************************************************************
       20010-ROUJIN-CHK2-SEC         SECTION.
      *
      *    公費編集
           INITIALIZE                  PTKOHINF-REC
           MOVE    AGECHK-HOSPID       TO  PTKOH-HOSPID
           MOVE    AGECHK-PTID         TO  PTKOH-PTID
           MOVE    "027"               TO  PTKOH-KOHNUM
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    1                   TO  AGECHK-ROUJIN2
           END-IF
      *
           .
      *
       20010-ROUJIN-CHK2-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険情報処理
      *****************************************************************
       3001-HKNNUM-CHK-SEC         SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    AGECHK-HOSPID       TO  HKN-HOSPID
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKEDYMD
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    AGECHK-SRYYMD       TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       3001-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険の切り替えチェック処理
      *****************************************************************
       400-PTHKNCHK-SEC         SECTION.
      *
           IF      COMB-HKNNUM         =   SPACE
               GO      TO      400-PTHKNCHK-EXT
           END-IF
      *
           MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
           MOVE    ZERO                TO  WRK-PAYKBN
           PERFORM 4101-KIKANCHK-CHK-SEC
           IF      FLG-HKNNUM-OK       =   ZERO
               MOVE    1               TO  AGECHK-HKNCHK
           END-IF
      *
           .
       400-PTHKNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の切り替えチェック処理
      *****************************************************************
       410-PTKOHCHK-SEC         SECTION.
      *
           IF      COMB-KOH1HKNNUM     NOT =   SPACE
      *        公費の期限切れチェック
               MOVE    COMB-KOH1HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH1PAYKBN     TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    2               TO  AGECHK-HKNCHK
                   GO      TO      410-PTKOHCHK-EXT
               END-IF
           END-IF
      *
           IF      COMB-KOH2HKNNUM     NOT =   SPACE
      *        公費の期限切れチェック
               MOVE    COMB-KOH2HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH2PAYKBN     TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    2               TO  AGECHK-HKNCHK
                   GO      TO      410-PTKOHCHK-EXT
               END-IF
           END-IF
      *
           IF      COMB-KOH3HKNNUM     NOT =   SPACE
      *        公費の期限切れチェック
               MOVE    COMB-KOH3HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH3PAYKBN     TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    2               TO  AGECHK-HKNCHK
                   GO      TO      410-PTKOHCHK-EXT
               END-IF
           END-IF
      *
           IF      COMB-KOH4HKNNUM     NOT =   SPACE
      *        公費の期限切れチェック
               MOVE    COMB-KOH4HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH4PAYKBN     TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    2               TO  AGECHK-HKNCHK
                   GO      TO      410-PTKOHCHK-EXT
               END-IF
           END-IF
      *
           .
       410-PTKOHCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険の切り替えチェック処理
      *****************************************************************
       4101-KIKANCHK-CHK-SEC         SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    AGECHK-HOSPID       TO  HKN-HOSPID
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    AGECHK-SRYYMD       TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY7"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY7"       TO  ORC-DBPATH
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    ZERO                TO  FLG-HKNNUM-OK
           PERFORM
               UNTIL   (FLG-HKNNUM     =   1  )  OR
                       (FLG-HKNNUM-OK  =   1  )
      *
               IF     (HKN-TEKSTYMD        <=  COMB-TEKSTYMD ) AND
                      (HKN-TEKEDYMD        >=  COMB-TEKEDYMD )
      *            保険期間対象あり
                   MOVE    1                   TO  FLG-HKNNUM-OK
               END-IF
      *
               IF     (HKN-TEKSTYMD        <=  COMB-TEKSTYMD ) AND
                      (HKN-TEKEDYMD        <   COMB-TEKEDYMD ) AND
                      (HKN-TEKEDYMD        >=  AGECHK-SRYYMD )
      *            診療日に保険期間対象あり
                   MOVE    1                   TO  FLG-HKNNUM-OK
               END-IF
      *
               MOVE    "HKNNUM-KEY7"       TO  ORC-DBPATH
               PERFORM 921-HKNNUM-NEXT-SEC
           END-PERFORM
      *
           .
       4101-HKNNUM-KIKANCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括まとめ入力処理
      *****************************************************************
       2009-HOUKATUHKN-SEC         SECTION.
      *
           MOVE    ZERO                TO  AGECHK-FTNRATE
           MOVE    SPACE               TO  AGECHK-FTNRATEMEI
      *
      *    老人判定
           IF     (AGECHK-NENREI       >=  65  )  AND
                  (AGECHK-ROUJIN       =   ZERO)
               PERFORM 2009-ROUJIN-CHK-SEC
           END-IF
      *
           .
       2009-HOUKATUHKN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
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
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
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
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険番号マスタ次検索処理
      ****************************************************************
       921-HKNNUM-NEXT-SEC              SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNNUM-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *
      *
