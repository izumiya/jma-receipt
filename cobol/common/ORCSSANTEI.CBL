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
       PROGRAM-ID.             ORCSSANTEI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 算定履歴 更新処理
      *  返却エラーコード  : 00:正常　10:パラメータ不正・対象データなし
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01.00.01    MCC-多々納   02.05.17  労災用保険種別毎の追加
      *  01.00.02    NACL-多々納  02.12.05  慢性疼痛疾患管理料の初回算定日修正
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
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-SYOKAI          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
      *
           03  FLG-MANSEI-ALL      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(02).
      *
           03  IDX-Y2              PIC 9(02).
           03  IDX-RSI             PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD               PIC X(09).
           03  WRK-MAE-SRYCD           PIC X(09).
           03  WRK-SRYYMD              PIC X(08).
      *    初回算定日
           03  WRK-FSANTEIYMD          PIC X(08).
           03  WRK-MAE-FSANTEIYMD      PIC X(08).
           03  WRK-KON-FSANTEIYMD      PIC X(08).
      *
           03  WRK-FSANTEIYMD-OLD      PIC X(08).
           03  WRK-FSANTEIYMD2         PIC X(08).
           03  WRK-SRYYM               PIC X(06).
      *    チェック用発生日
           03  WRK-HASSYOYMD3          PIC X(08).
      *    初診料算定日
           03  WRK-SYOYMD              PIC X(08).
           03  WRK-SYOSINYMD           PIC X(008).
           03  WRK-SYOSINYMD-1         PIC X(008).
      *
           03  WRK-KBN                 PIC X(02).
      * 
           03  WRK-MAE-YMD.
               05  WRK-MAE-YY              PIC 9(04).
               05  WRK-MAE-MM              PIC 9(02).
               05  WRK-MAE-DD              PIC 9(02).
      * 
           03  WRK-YMD3                 PIC X(08).
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-DBPATH              PIC X(64). 
      *
           03  WRK-RIGAKU-RYO          PIC X(01). 
      *
      *    同一初回算定日コードテーブル
       01  TBL-AREA.
      *    特定薬剤管理加算料テーブル
           03  FILLER              PIC X(12)   VALUE   "01 113000410".
           03  FILLER              PIC X(12)   VALUE   "01 180000110".
           03  FILLER              PIC X(12)   VALUE   "01 180000210".
           03  FILLER              PIC X(12)   VALUE   "01 113000510".
       01  TBL-AREA-R              REDEFINES   TBL-AREA.
           03  TBL-OCC             OCCURS   4  INDEXED  TBL-IDX.
               05  TBL-KBN         PIC X(02).
               05  TBL-FILLER      PIC X(01).
               05  TBL-SRYCD       PIC X(09).
      *
       01  TBL-MAX                 PIC 9(04)   VALUE   4.
      *
      *    慢性疼痛疾患
       01  WRK-MANSE-AREA.
           03  WRK-MANSEI          PIC X(09)   VALUE   "113006510".
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *    算定履歴
       01  WRK-SANTEI-REC.
           COPY    "CPSANTEI.INC"   REPLACING
                                    //SANTEI-// BY //WRK-SANTEI-//.
      *    算定履歴
       01  HZN-SANTEI-REC.
           COPY    "CPSANTEI.INC"   REPLACING
                                    //SANTEI-// BY //HZN-SANTEI-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
     *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSANTEI.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSSANTEIAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           EVALUATE    SSANTEI-SYORI
      *        新規、変更
               WHEN    "1"
                   PERFORM 200-SINKI-SYORI-SEC
      *        変更、削除
               WHEN    "3"
                   PERFORM 300-DELETE-SYORI-SEC
           END-EVALUATE
      *
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初回算定日決定処理
      *****************************************************************
       100-SYOKAI-SEC                SECTION.
      *    初回年月日は、特定薬剤治療管理加算のみとする
           MOVE    SPACE               TO  WRK-KBN
           SET     TBL-IDX             TO  1
           SEARCH      TBL-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KBN
                   WHEN    SSANTEI-SRYCD   =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-KBN (TBL-IDX)
                                                TO  WRK-KBN
           END-SEARCH
           IF     (WRK-KBN             =   "01" )  AND
                  (SSANTEI-SYOKAIYMD   NOT =   SPACE)
               MOVE    SSANTEI-SYOKAIYMD   TO  WRK-FSANTEIYMD
               GO      TO      100-SYOKAI-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-FSANTEIYMD
           MOVE    SPACE               TO  HZN-SANTEI-REC
      *
      *    対象コードの初回回数
           MOVE    SSANTEI-SRYCD       TO  WRK-SRYCD
           MOVE    SPACE               TO  WRK-SRYYMD
           MOVE    SPACE               TO  WRK-FSANTEIYMD-OLD
      *    算定履歴検索（診療日降順）
           MOVE    "SANTEI-KEY3"       TO  WRK-DBPATH
           PERFORM 1001-SANTEI-KEN-SEC
           PERFORM UNTIL   (FLG-SANTEI     =   1 )  OR
                           (SANTEI-SRYYM   <=  SSANTEI-SRYYMD(1:6))
               MOVE    SANTEI-FSANTEIYMD   TO  WRK-FSANTEIYMD-OLD
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           IF      FLG-SANTEI          =   ZERO
               IF     (SANTEI-FSANTEIYMD   <=  SSANTEI-SRYYMD) AND
                      (SANTEI-MSANTEICNT   >   ZERO      ) AND
                      (SANTEI-FSANTEIYMD   >=  WRK-FSANTEIYMD)
                   MOVE    SANTEI-FSANTEIYMD   TO  WRK-FSANTEIYMD
                   MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
               END-IF
           END-IF
      *
      *    同一算定コードがある時、すべてでチェック
           SET     TBL-IDX             TO  1
           SEARCH      TBL-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KBN
                   WHEN    WRK-SRYCD       =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-KBN (TBL-IDX)
                                                TO  WRK-KBN
           END-SEARCH
           MOVE    WRK-SRYCD           TO  WRK-MAE-SRYCD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   TBL-MAX  )  OR
                              (WRK-KBN     =   SPACE    )
               SET     TBL-IDX             TO  IDX
               IF     (TBL-KBN (TBL-IDX)   =   WRK-KBN )  AND
                      (TBL-SRYCD (TBL-IDX) NOT =   WRK-MAE-SRYCD )
                   MOVE    TBL-SRYCD (TBL-IDX)     TO  WRK-SRYCD
      *            算定履歴検索
                   MOVE    "SANTEI-KEY3"       TO  WRK-DBPATH
                   PERFORM 1001-SANTEI-KEN-SEC
                   PERFORM UNTIL   (FLG-SANTEI     =   1 )  OR
                                   (SANTEI-SRYYM   <=  SSANTEI-SRYYMD
                                                                (1:6))
                       MOVE    WRK-DBPATH          TO  ORC-DBPATH
                       PERFORM 920-SANTEI-READ-SEC
                   END-PERFORM
                   IF      FLG-SANTEI          =   ZERO
                       IF     (SANTEI-FSANTEIYMD   <=
                                                   SSANTEI-SRYYMD) AND
                              (SANTEI-MSANTEICNT   >   ZERO      ) AND
                              (SANTEI-FSANTEIYMD   >=  WRK-FSANTEIYMD)
                           MOVE    SANTEI-FSANTEIYMD
                                                   TO  WRK-FSANTEIYMD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    存在しない時、診療年月
           IF      WRK-FSANTEIYMD      =   SPACE
                MOVE    SSANTEI-SRYYMD     TO  WRK-FSANTEIYMD
           END-IF
      *
           IF      (WRK-FSANTEIYMD-OLD NOT =   SPACE )  AND
                   (WRK-FSANTEIYMD     NOT =   SPACE )  AND
                   (WRK-FSANTEIYMD-OLD NOT =   WRK-FSANTEIYMD)
               MOVE    WRK-FSANTEIYMD-OLD  TO  WRK-MAE-FSANTEIYMD
               MOVE    WRK-FSANTEIYMD      TO  WRK-KON-FSANTEIYMD
               MOVE    1                   TO  FLG-SYOKAI
           END-IF
      *    慢性疼痛疾患管理料の時、初診料算定日から初回算定日を始める
           IF     (WRK-MAE-SRYCD       =   WRK-MANSEI ) 
               PERFORM 1009-MANSE-CHK-SEC
           END-IF
      *
           .
       100-SYOKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初回算定日の日変更処理
      *****************************************************************
       10010-MAE-SANTEI-HEN-SEC        SECTION.
      *
           MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
           MOVE    SPACE               TO  WRK-YMD3
      *    算定履歴検索
           MOVE    "SANTEI-KEY4"       TO  WRK-DBPATH
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM
               UNTIL      FLG-SANTEI       =   1
               IF      SANTEI-MSANTEICNT   >   ZERO
                   MOVE    SANTEI-SRYYM        TO  WRK-YMD(1:6)
                   MOVE    SANTEI-MSANTEID     TO  WRK-DD
                   IF     (WRK-YMD3            =   SPACE ) OR
                          (WRK-YMD3            >   WRK-YMD)
                       MOVE    WRK-YMD         TO  WRK-YMD3
                   END-IF
               END-IF
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           IF      WRK-YMD3            =   SPACE
      *        他の履歴が無い時
               MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
               MOVE    SANTEI-MSANTEID     TO  WRK-FSANTEIYMD (7:2)
           ELSE
               MOVE    WRK-YMD3            TO  WRK-FSANTEIYMD
           END-IF
      *
           MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
      *    算定履歴検索
           MOVE    "SANTEI-KEY4"       TO  WRK-DBPATH
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM
               UNTIL      FLG-SANTEI       =   1
               MOVE    WRK-FSANTEIYMD      TO  SANTEI-FSANTEIYMD
      *
               MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *
               IF      HZN-SANTEI-KEY      =   SANTEI-KEY
                   MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
               END-IF
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                  DISPLAY "ORCSSANTEISANTEI UPD ERR:" SANTEI-KEY
                                                    ","   MCP-RC
               END-IF
      *
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           .
       10010-MAE-SANTEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       1001-SANTEI-KEN-SEC                SECTION.
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF     (WRK-SRYCD (1:3)     =   "099"  )  OR
                  (TNS-SRYKBN      NOT =   "70"   )
               CONTINUE
           ELSE
      *        ＣＴ、ＭＲＩの時算定のコードを置換える
               PERFORM 10010-SANTEI-SRYCD-SEC
               IF      WRK-SRYCD       NOT =   TNS-SRYCD
                   MOVE    WRK-SRYCD           TO  SSANTEI-SRYCD
                   MOVE    WRK-SRYCD           TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
               END-IF
           END-IF
      *
           EVALUATE    SSANTEI-PG
               WHEN    "K03"
                   MOVE    SPA-NYUGAIKBN       TO  SSANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SSANTEI-SRYKA
           END-EVALUATE
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SSANTEI-PTID        TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    WRK-SRYYMD(1:6)     TO  SANTEI-SRYYM
           IF      WRK-DBPATH          =   "SANTEI-KEY4" 
               MOVE    WRK-FSANTEIYMD      TO  SANTEI-FSANTEIYMD
           END-IF
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SSANTEI-NYUGAIKBN   TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SSANTEI-SRYKA       TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SSANTEI-NYUGAIKBN   TO  SANTEI-NYUGAIKBN
                   MOVE    SSANTEI-SRYKA       TO  SANTEI-SRYKA
           END-EVALUATE
      *    健保以外で保険別の時、保険をセット
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           .
      *
       1001-SANTEI-KEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定用の診療コードへ変更処理
      *****************************************************************
       10010-SANTEI-SRYCD-SEC              SECTION.
      *
      *    包括逓減区分判定
           EVALUATE    TNS-HOUKATUTEIGENKBN
      *        頭部
               WHEN    201
                   MOVE    "099700101"         TO  WRK-SRYCD
      *        躯幹
               WHEN    202
                   MOVE    "099700102"         TO  WRK-SRYCD
      *        四肢
               WHEN    203
                   MOVE    "099700103"         TO  WRK-SRYCD
           END-EVALUATE
           .
       10010-SANTEI-SRYCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性疼痛疾患管理料判定処理
      *****************************************************************
       1009-MANSE-CHK-SEC      SECTION.
      *
      *    初診料算定日がない時、診療科履歴から算定
           IF      SSANTEI-SYOYMD      =   SPACE
               MOVE    SSANTEI-SRYYMD(1:6)     TO  WRK-SRYYM
               PERFORM 10091-SRYKARRK-CHK-SEC
               IF     (WRK-SYOSINYMD       =   SPACE )  OR
                      (WRK-SYOSINYMD       >   WRK-FSANTEIYMD)
                   MOVE    SSANTEI-SRYYMD      TO  WRK-FSANTEIYMD
               END-IF
               MOVE    WRK-SYOSINYMD       TO  WRK-SYOSINYMD-1
           ELSE
               MOVE    SSANTEI-SYOYMD      TO  WRK-SYOSINYMD-1
               IF      SSANTEI-SYOYMD      >   WRK-FSANTEIYMD
                   MOVE    SSANTEI-SRYYMD      TO  WRK-FSANTEIYMD
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-MANSEI-ALL
           MOVE    ZERO                TO  FLG-SYOKAI
           IF      WRK-FSANTEIYMD-OLD  NOT =   SPACE
               MOVE    WRK-FSANTEIYMD-OLD(1:6) TO  WRK-SRYYM
               PERFORM 10091-SRYKARRK-CHK-SEC
               IF     (WRK-SYOSINYMD   NOT =   SPACE     )   AND
                      (WRK-SYOSINYMD       <   WRK-FSANTEIYMD-OLD) AND
                      (WRK-SYOSINYMD       =   WRK-SYOSINYMD-1)
                   MOVE    1               TO  FLG-MANSEI-ALL
                   MOVE    WRK-FSANTEIYMD  TO  WRK-KON-FSANTEIYMD
               END-IF
           END-IF
           .
      *
       1009-MANSE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料算定日判定処理
      *****************************************************************
       10091-SRYKARRK-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOSINYMD
      *    病院
      *    老人
           MOVE    "111700110"         TO  WRK-SRYCD
           PERFORM 100912-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD       =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD
           END-IF
           MOVE    "111000110"         TO  WRK-SRYCD
           PERFORM 100912-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD       =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD
           END-IF
      *    診療所
      *    老人
           MOVE    "111700210"         TO  WRK-SRYCD
           PERFORM 100912-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD       =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD
           END-IF
      *    老人外
           MOVE    "111003610"         TO  WRK-SRYCD
           PERFORM 100912-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD       =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD
           END-IF
      *    小児科診察料
      *    院外処方   初診
           MOVE    "113003510"         TO  WRK-SRYCD
           PERFORM 100912-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD       =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD
           END-IF
      *    　　院内処方   初診
           MOVE    "113003710"         TO  WRK-SRYCD
           PERFORM 100912-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD       =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD
           END-IF
      *
      *    仮の初診料
           MOVE    "099110001"         TO  WRK-SRYCD
           PERFORM 100912-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD       =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD
           END-IF
      *
      *    労災初診料
           MOVE    "101110010"         TO  WRK-SRYCD
           PERFORM 100912-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD       =   SPACE  )  OR
                  (WRK-YMD             >   WRK-SYOSINYMD)
               MOVE    WRK-YMD         TO  WRK-SYOSINYMD
           END-IF
           .
      *
       1009-MANSE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       100912-SANTEI-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-YMD
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    WRK-SRYYM           TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY8"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SANTEI-KEY8"       TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL       FLG-SANTEI          =   1
      *        終了判定
               IF      SANTEI-MSANTEICNT   >   ZERO
                   IF      SANTEI-SRYYM    <=  WRK-SRYYM
                       PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                               UNTIL       IDX-Y2      >   31
                           IF      SANTEI-DAY (IDX-Y2) >   ZERO
                               MOVE    IDX-Y2          TO  WRK-DD
                           END-IF
                       END-PERFORM
                       MOVE    SANTEI-SRYYM        TO  WRK-YMD(1:6)
                       MOVE    1                   TO  FLG-SANTEI
                   END-IF
               END-IF
      *
               IF      FLG-SANTEI          =   ZERO
                   MOVE    "SANTEI-KEY8"       TO  ORC-DBPATH
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
           .
      *
       100912-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RIGAKU-RYO
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
                   MOVE    TBL-RIGAKU-RYO (TBL-SAN)
                                           TO  WRK-RIGAKU-RYO
           END-SEARCH
      *
           IF      WRK-RIGAKU-RYO      =   SPACE
               MOVE    ZERO                TO  FLG-HKN-SANTEI
           ELSE
               MOVE    1                   TO  FLG-HKN-SANTEI
           END-IF
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     新規、変更処理
      *****************************************************************
       200-SINKI-SYORI-SEC             SECTION.
      *
           MOVE    SSANTEI-SRYCD       TO  WRK-SRYCD
           MOVE    SSANTEI-SRYYMD      TO  WRK-SRYYMD
      *    算定履歴検索
           MOVE    "SANTEI-KEY"        TO  WRK-DBPATH
           PERFORM 1001-SANTEI-KEN-SEC
      *
      *    健保以外で保険別の時、3月以内は作成なし
      *DDDDIF     (FLG-HKN-SANTEI      =   1   )  AND
      *           (SPA-HKNKBN      NOT =   ZERO)  AND
      *********** (SSANTEI-SRYYMD(1:6) <   SPA-RSI-HASSYOYMD3(1:6))
      *           (SSANTEI-SRYYMD(1:6) <   WRK-HASSYOYMD3(1:6))
      *         GO      TO      200-SINKI-SYORI-EXT
      ***** END-IF
      *    健保以外でＣＴ・ＭＲＩの作成なし
           IF     (SPA-HKNKBN      NOT =   ZERO   )  AND
                  (SSANTEI-SRYCD       =   "099700101"  OR
                                           "099700102"  OR
                                           "099700103" )
                GO      TO      200-SINKI-SYORI-EXT
            END-IF
      *
           IF      FLG-SANTEI          =   1
      *
      *DDDDDDD 健保以外で保険別の時、3月以内は作成なし
      *        IF     (FLG-HKN-SANTEI      =   1   )  AND
      *               (SPA-HKNKBN      NOT =   ZERO)  AND
      ****************(SSANTEI-SRYYMD      <    SPA-RSI-HASSYOYMD3)
      *               (SSANTEI-SRYYMD      <    WRK-HASSYOYMD3)
      *            GO      TO      200-SINKI-SYORI-EXT
      *****    END-IF
               MOVE    1                   TO  FLG-SYORI
      *
      *        初回算定日決定
               PERFORM 100-SYOKAI-SEC
      *
               MOVE    SSANTEI-SRYCD       TO  WRK-SRYCD
               MOVE    SSANTEI-SRYYMD      TO  WRK-SRYYMD
      *
      *        新規追加編集
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
               MOVE    SSANTEI-PTID        TO  SANTEI-PTID
               MOVE    WRK-SRYYMD(1:6)     TO  SANTEI-SRYYM
               MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
               EVALUATE    TNS-SANTEIRRKKBN
                   WHEN    1
                       MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                       MOVE    ZERO                TO  SANTEI-SRYKA
                   WHEN    2
                       MOVE    SSANTEI-NYUGAIKBN   TO  SANTEI-NYUGAIKBN
                       MOVE    ZERO                TO  SANTEI-SRYKA
                   WHEN    3
                       MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                       MOVE    SSANTEI-SRYKA       TO  SANTEI-SRYKA
                   WHEN    4
                       MOVE    SSANTEI-NYUGAIKBN   TO  SANTEI-NYUGAIKBN
                       MOVE    SSANTEI-SRYKA       TO  SANTEI-SRYKA
               END-EVALUATE
      *        健保以外で保険別の時、保険をセット
               IF     (FLG-HKNSAN          =   1   )  AND
                      (SPA-HKNKBN      NOT =   ZERO)
                   MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
               ELSE
                   MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               END-IF
      *        初回算定日
               MOVE    WRK-FSANTEIYMD      TO  SANTEI-FSANTEIYMD
               MOVE    WRK-SRYYMD(7:2)     TO  SANTEI-MSANTEID
           ELSE
      *        修正
               IF      SANTEI-FSANTEIYMD(7:2)  =   ZERO
                   MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
                   PERFORM 10010-MAE-SANTEI-HEN-SEC
                   MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
               END-IF
               MOVE    2                   TO  FLG-SYORI
               MOVE    SANTEI-SRYYM        TO  WRK-MAE-YMD(1:6)
               MOVE    SANTEI-MSANTEID     TO  WRK-MAE-DD
               MOVE    WRK-SRYYMD(7:2)     TO  IDX-Y2
               IF     (IDX-Y2              <   WRK-MAE-DD ) OR
                      ( SANTEI-MSANTEID    =   ZERO       )
                   MOVE    IDX-Y2          TO  SANTEI-MSANTEID
               END-IF
           END-IF
      *
           EVALUATE    SSANTEI-PG
               WHEN    "K03"
                   IF      SSANTEI-KAISU       =   ZERO
                       MOVE    1               TO  SSANTEI-KAISU
                   END-IF
      *            当月算定歴回数
                   ADD     SSANTEI-KAISU       TO  SANTEI-MSANTEICNT
      *            当日算定日
                   MOVE    WRK-SRYYMD(7:2)     TO  IDX-Y2
                   ADD     SSANTEI-KAISU       TO  SANTEI-DAY (IDX-Y2)
               WHEN    "J02"
                   MOVE    ZERO                TO  SANTEI-MSANTEID
                   MOVE    ZERO                TO  SANTEI-MSANTEICNT
      *            前回分を削除する
                   PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                           UNTIL       IDX-Y2  >   31
                       IF      SANTEI-DAY (IDX-Y2) <   SSANTEI-MAE-DAY
                                                             (IDX-Y2)
                           MOVE    ZERO        TO  SANTEI-DAY (IDX-Y2)
                       ELSE
                           COMPUTE SANTEI-DAY (IDX-Y2)
                                               =   SANTEI-DAY (IDX-Y2)
                                               -   SSANTEI-MAE-DAY
                                                             (IDX-Y2)
                       END-IF
                   END-PERFORM
      *            登録する日に数字が入っている
                   PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                           UNTIL       IDX-Y2  >   31
                       IF      SSANTEI-DAY (IDX-Y2)    >   ZERO
                           COMPUTE SANTEI-DAY (IDX-Y2)
                                               =   SANTEI-DAY (IDX-Y2)
                                               +   SSANTEI-DAY(IDX-Y2)
                       END-IF
                       ADD     SANTEI-DAY (IDX-Y2)
                                               TO  SANTEI-MSANTEICNT
                       IF     (SANTEI-DAY (IDX-Y2) >   ZERO ) AND
                              (SANTEI-MSANTEID     =   ZERO )
                           MOVE    IDX-Y2          TO  SANTEI-MSANTEID
                       END-IF
                   END-PERFORM
               WHEN    "J04"
                   MOVE    ZERO                TO  SANTEI-MSANTEID
                   MOVE    ZERO                TO  SANTEI-MSANTEICNT
      *            登録する日に数字が入っている
                   PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                           UNTIL       IDX-Y2  >   31
                       IF      SSANTEI-DAY (IDX-Y2)    >   ZERO
                           COMPUTE SANTEI-DAY (IDX-Y2)
                                               =   SANTEI-DAY (IDX-Y2)
                                               +   SSANTEI-DAY(IDX-Y2)
                       END-IF
                       ADD     SANTEI-DAY (IDX-Y2)
                                               TO  SANTEI-MSANTEICNT
                       IF     (SANTEI-DAY (IDX-Y2) >   ZERO ) AND
                              (SANTEI-MSANTEID     =   ZERO )
                           MOVE    IDX-Y2          TO  SANTEI-MSANTEID
                       END-IF
                   END-PERFORM
           END-EVALUATE
      *
      *DDD 健保以外で保険別の時、3月以内は作成なし
      *    IF     (FLG-HKN-SANTEI      =   1   )  AND
      *           (WRK-RIGAKU-RYO  NOT =   SPACE) AND
      *           (SPA-HKNKBN      NOT =   ZERO)  AND
      ************(SSANTEI-SRYYMD(1:6) =   SPA-RSI-HASSYOYMD3(1:6))
      *           (SSANTEI-SRYYMD(1:6) =   WRK-HASSYOYMD3(1:6))
      *        PERFORM 2009-SSANTEI-RSI-SEC
      *****END-IF
      *
           IF      FLG-SYORI           =   1
      *
               MOVE    SMCNDATE-YMD        TO  SANTEI-CREYMD
               MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *        新規
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "ORCSSANTEI  INS ERR:" SANTEI-KEY
                                          ","   MCP-RC
               END-IF
      *
               IF      FLG-MANSEI-ALL      =   1
                   PERFORM 400-SANTEI-MANSEI-SEC
               ELSE
                   IF      FLG-SYOKAI          =   1
                       PERFORM 400-SANTEI-ALLUPD-SEC
                   END-IF
               END-IF
           ELSE
      *
      *        修正
      *        初回算定日変更チェック
               PERFORM 2001-SYOKAI-CHK-SEC
      *
               MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "ORCSSANTEI UPD ERR:" SANTEI-KEY
                                                  ","  MCP-RC
               END-IF
      *
               IF      FLG-SYOKAI          =   1
                   PERFORM 400-SANTEI-ALLUPD-SEC
               END-IF
           END-IF
           .
      *
       200-SINKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    健保以外で保険別の時算定テーブル変更
      *****************************************************************
       2009-SSANTEI-RSI-SEC                SECTION.
      *
      *    月回数と当月初回日の変更
           MOVE    ZERO                TO  SANTEI-MSANTEICNT
           MOVE    ZERO                TO  SANTEI-MSANTEID
      *
      **** MOVE    SPA-RSI-HASSYOYMD3(7:2) TO  IDX-RSI
      *    MOVE    WRK-HASSYOYMD3(7:2) TO  IDX-RSI
      *
           PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                   UNTIL       IDX-Y2  >   31
               IF      IDX-Y2          <   IDX-RSI
                   MOVE    ZERO            TO  SANTEI-DAY (IDX-Y2)
               END-IF
               ADD     SANTEI-DAY (IDX-Y2) TO  SANTEI-MSANTEICNT
               IF     (SANTEI-DAY (IDX-Y2) >   ZERO )  AND
                      (SANTEI-MSANTEID     =   ZERO )
                   MOVE    IDX-Y2          TO  SANTEI-MSANTEID
               END-IF
           END-PERFORM
           .
      *
       2009-SSANTEI-RSI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初回算定日変更チェック
      *****************************************************************
       2001-SYOKAI-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOKAI
      *    更新前の当月初回算定日
           IF    ((WRK-MAE-YMD         =   SANTEI-FSANTEIYMD ) AND
                  (WRK-MAE-DD      NOT =   SANTEI-MSANTEID   ))
               MOVE    SANTEI-SRYYM        TO  WRK-KON-FSANTEIYMD(1:6)
               MOVE    SANTEI-MSANTEID     TO  WRK-KON-FSANTEIYMD(7:2)
               IF      SANTEI-MSANTEID     =   ZERO
                   MOVE    WRK-MAE-DD      TO  WRK-KON-FSANTEIYMD(7:2)
               END-IF
               MOVE    SANTEI-FSANTEIYMD   TO  WRK-MAE-FSANTEIYMD
               MOVE    SANTEI-REC          TO  WRK-SANTEI-REC
               PERFORM 20011-SANTEI-CHK-SEC
               MOVE    WRK-SANTEI-REC      TO  SANTEI-REC
      *        今回初回算定日が前回と違うとき修正
               IF      WRK-KON-FSANTEIYMD  NOT =   SANTEI-FSANTEIYMD
                   MOVE    1               TO  FLG-SYOKAI
               END-IF
               MOVE    WRK-KON-FSANTEIYMD  TO  SANTEI-FSANTEIYMD
           END-IF
           .
      *
       2001-SYOKAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初回算定日変更処理
      *****************************************************************
       20011-SANTEI-CHK-SEC                SECTION.
      *
      *    同一算定コードがある時、すべてでチェック
           SET     TBL-IDX             TO  1
           SEARCH      TBL-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KBN
                   WHEN    WRK-SRYCD       =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-KBN (TBL-IDX)
                                                TO  WRK-KBN
           END-SEARCH
           MOVE    WRK-SRYCD           TO  WRK-MAE-SRYCD
           MOVE    SPACE               TO  WRK-YMD3
      *
      *    同一初回算定日で、今回変更以前の日があれば初回算定日にする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   TBL-MAX  )  OR
                              (WRK-KBN     =   SPACE    )
               SET     TBL-IDX             TO  IDX
               IF      TBL-KBN (TBL-IDX)   =   WRK-KBN
                   MOVE    TBL-SRYCD (TBL-IDX)     TO  WRK-SRYCD
                   MOVE    WRK-MAE-FSANTEIYMD      TO  WRK-FSANTEIYMD
      *            算定履歴検索
                   MOVE    "SANTEI-KEY4"       TO  WRK-DBPATH
                   PERFORM 1001-SANTEI-KEN-SEC
                   PERFORM UNTIL     FLG-SANTEI    =   1
                       IF      SANTEI-MSANTEICNT   >   ZERO
                          IF      (SANTEI-SRYCD    =   WRK-MAE-SRYCD )
                              AND (SANTEI-SRYYM    =   SSANTEI-SRYYMD
                                                                (1:6))
                               CONTINUE
                           ELSE
                               MOVE    SANTEI-SRYYM    TO  WRK-YMD(1:6)
                               MOVE    SANTEI-MSANTEID TO  WRK-DD
                               IF      WRK-KON-FSANTEIYMD  >   WRK-YMD
                                   MOVE    WRK-YMD     TO
                                                   WRK-KON-FSANTEIYMD
                               END-IF
                           END-IF
                       END-IF
                       MOVE    WRK-DBPATH          TO  ORC-DBPATH
                       PERFORM 920-SANTEI-READ-SEC
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       20011-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       300-DELETE-SYORI-SEC                SECTION.
      *
           MOVE    SSANTEI-SRYCD       TO  WRK-SRYCD
           MOVE    SSANTEI-SRYYMD      TO  WRK-SRYYMD
      *    算定履歴検索
           MOVE    "SANTEI-KEY"        TO  WRK-DBPATH
           PERFORM 1001-SANTEI-KEN-SEC
      *
      *    健保以外で保険別の時、3月以内は作成なし
      *DDD IF     (FLG-HKN-SANTEI      =   1   )  AND
      *           (SPA-HKNKBN      NOT =   ZERO)  AND
     ***********  (SSANTEI-SRYYMD(1:6) <   SPA-RSI-HASSYOYMD3(1:6))
      *           (SSANTEI-SRYYMD(1:6) <   WRK-HASSYOYMD3(1:6))
      *         GO      TO      300-DELETE-SYORI-EXT
      ***** END-IF
      *
      *    健保以外でＣＴ・ＭＲＩの作成なし
           IF     (SPA-HKNKBN      NOT =   ZERO   )  AND
                  (SSANTEI-SRYCD       =   "099700101"  OR
                                           "099700102"  OR
                                           "099700103" )
               GO      TO      300-DELETE-SYORI-EXT
            END-IF
      *
      *    対象データが無い時、終了
           IF      FLG-SANTEI          =   1
               GO      TO      300-DELETE-SYORI-EXT
           END-IF
      *
      *    初回算定日の日がゼロの場合一括変更
           IF      SANTEI-FSANTEIYMD(7:2)  =   ZERO
               MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
               PERFORM 10010-MAE-SANTEI-HEN-SEC
               MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
           END-IF
      *
      *    変更前の初回算定日保存
           MOVE    SANTEI-SRYYM        TO  WRK-MAE-YMD(1:6)
           MOVE    SANTEI-MSANTEID     TO  WRK-MAE-DD
      *
           EVALUATE    SSANTEI-PG
               WHEN    "K03"
                   IF      SSANTEI-KAISU       =   ZERO
                       MOVE    1               TO  SSANTEI-KAISU
                   END-IF
      *            当月算定歴回数
                   IF      SANTEI-MSANTEICNT   <   SSANTEI-KAISU
                       MOVE    ZERO            TO  SANTEI-MSANTEICNT
                   ELSE
                       COMPUTE SANTEI-MSANTEICNT   =   SANTEI-MSANTEICNT
                                                   -   SSANTEI-KAISU
                   END-IF
      *            当日算定日
                   MOVE    WRK-SRYYMD(7:2)     TO  IDX-Y2
                   IF      SANTEI-DAY (IDX-Y2) <   SSANTEI-KAISU
                       MOVE    ZERO            TO  SANTEI-DAY (IDX-Y2)
                   ELSE
                       COMPUTE SANTEI-DAY (IDX-Y2)
                                               =   SANTEI-DAY (IDX-Y2)
                                               -   SSANTEI-KAISU
                   END-IF
                   MOVE    ZERO                TO  SANTEI-MSANTEICNT
                   MOVE    ZERO                TO  SANTEI-MSANTEID
                   PERFORM VARYING     IDX2    FROM   1   BY  1
                           UNTIL       IDX2    >   31
                       IF      SANTEI-DAY (IDX2)   >   ZERO
                           IF      SANTEI-MSANTEID     =   ZERO
      *                        当月初回算定日
                               MOVE    IDX2        TO  SANTEI-MSANTEID
                           END-IF
      *                    回数
                           ADD     SANTEI-DAY (IDX2)
                                                   TO  SANTEI-MSANTEICNT
                       END-IF
                   END-PERFORM
      *
               WHEN    "J02"
               WHEN    "J04"
                   MOVE    ZERO                TO  SANTEI-MSANTEID
                   MOVE    ZERO                TO  SANTEI-MSANTEICNT
      *            削除する日に数字が入っている
                   PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                           UNTIL       IDX-Y2  >   31
                       IF      SSANTEI-DAY (IDX-Y2)    >   ZERO
                           IF      SSANTEI-DAY(IDX-Y2)
                                               <=   SANTEI-DAY (IDX-Y2)
                               COMPUTE SANTEI-DAY (IDX-Y2)
                                               =   SANTEI-DAY (IDX-Y2)
                                               -   SSANTEI-DAY(IDX-Y2)
                           ELSE
                               MOVE    ZERO        TO  SANTEI-DAY
                                                            (IDX-Y2)
                           END-IF
                       END-IF
                       ADD     SANTEI-DAY (IDX-Y2)
                                               TO  SANTEI-MSANTEICNT
                       IF     (SANTEI-DAY (IDX-Y2) >   ZERO ) AND
                              (SANTEI-MSANTEID     =   ZERO )
                           MOVE    IDX-Y2          TO  SANTEI-MSANTEID
                       END-IF
                   END-PERFORM
           END-EVALUATE
      *
      *    健保以外で保険別の時、3月以内は作成なし
      *DDD IF     (FLG-HKN-SANTEI      =   1   )  AND
      *           (SPA-HKNKBN      NOT =   ZERO)  AND
      ************(SSANTEI-SRYYMD(1:6) =   SPA-RSI-HASSYOYMD3(1:6))
      *           (SSANTEI-SRYYMD(1:6) =   WRK-HASSYOYMD3(1:6))
      *        PERFORM 2009-SSANTEI-RSI-SEC
      ***  END-IF
      *
           IF      SANTEI-MSANTEICNT   =   ZERO
      *        初回算定日変更チェック
               PERFORM 2003-DEL-SYOKAI-SEC
      *        削除
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                  DISPLAY "ORCSSANTEISANTEI DEL ERR:"  SANTEI-KEY
                                              ","  MCP-RC
               END-IF
           ELSE
      *        初回算定日変更チェック
               PERFORM 2001-SYOKAI-CHK-SEC
      *        修正
               MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                  DISPLAY "ORCSSANTEISANTEI UPD ERR:" SANTEI-KEY
                                          ","  MCP-RC
               END-IF
           END-IF
      *
      *    すべての初回回数の変更
           IF      FLG-SYOKAI          =   1
               PERFORM 400-SANTEI-ALLUPD-SEC
           END-IF
      *
           .
       300-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除初回算定日変更チェック
      *****************************************************************
       2003-DEL-SYOKAI-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOKAI
           MOVE    SANTEI-REC          TO  WRK-SANTEI-REC
      *    更新前の当月初回算定日
           IF     (WRK-MAE-YMD         =   SANTEI-FSANTEIYMD )
               MOVE    SANTEI-FSANTEIYMD   TO  WRK-MAE-FSANTEIYMD
               MOVE    SPACE               TO  WRK-KON-FSANTEIYMD
               PERFORM 20011-SANTEI-CHK-SEC
               MOVE    WRK-SANTEI-REC      TO  SANTEI-REC
      *        今回初回算定日が前回と違うとき修正
               IF      WRK-KON-FSANTEIYMD  NOT =   SANTEI-FSANTEIYMD
                   MOVE    1               TO  FLG-SYOKAI
               END-IF
           END-IF
      *
           IF      FLG-SYOKAI          =   1
               PERFORM 30011-DEL-SYOKAI-CHK-SEC
           END-IF
           MOVE    WRK-SANTEI-REC      TO  SANTEI-REC
           .
      *
       2003-DEL-SYOKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除時、初回算定日変更
      *****************************************************************
       30011-DEL-SYOKAI-CHK-SEC        SECTION.
      *
           MOVE    WRK-SANTEI-REC      TO  SANTEI-REC
      *
           MOVE    SPACE               TO  WRK-FSANTEIYMD2
           MOVE    SPACE               TO  WRK-YMD3
           MOVE    SSANTEI-SRYCD       TO  WRK-SRYCD
           MOVE    WRK-MAE-YMD         TO  WRK-FSANTEIYMD
           PERFORM 300111-DEL-SYOKAI-SEC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   TBL-MAX  )  OR
                              (WRK-KBN     =   SPACE    )
               SET     TBL-IDX             TO  IDX
               IF      TBL-KBN (TBL-IDX)   =   WRK-KBN
                   MOVE    TBL-SRYCD (TBL-IDX)     TO  WRK-SRYCD
      *            算定履歴初回算定日変更処理
                   PERFORM 300111-DEL-SYOKAI-SEC
               END-IF
           END-PERFORM
           IF      WRK-YMD3        NOT =   SPACE
               IF     (WRK-FSANTEIYMD2 NOT =   SPACE )  AND
                      (WRK-YMD3            >   WRK-FSANTEIYMD2)
                   MOVE    WRK-FSANTEIYMD2     TO  WRK-KON-FSANTEIYMD
               ELSE
                   MOVE    WRK-YMD3            TO  WRK-KON-FSANTEIYMD
               END-IF
           END-IF
      *
           IF      WRK-KON-FSANTEIYMD  =   SPACE
               MOVE    ZERO                TO  FLG-SYOKAI
           END-IF
           MOVE    WRK-SANTEI-REC      TO  SANTEI-REC
      *
           .
       30011-DEL-SYOKAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全算定履歴初回算定日判定処理
      *****************************************************************
       300111-DEL-SYOKAI-SEC                SECTION.
      *    算定履歴検索
           IF      WRK-SRYCD           =   WRK-MANSEI
               MOVE    "SANTEI-KEY4"       TO  WRK-DBPATH
           ELSE
               MOVE    "SANTEI-KEY3"       TO  WRK-DBPATH
           END-IF
           PERFORM 1001-SANTEI-KEN-SEC
           PERFORM UNTIL     FLG-SANTEI    =   1
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (WRK-SANTEI-KEY  NOT =   SANTEI-KEY )
                   MOVE    SANTEI-SRYYM        TO  WRK-YMD(1:6)
                   MOVE    SANTEI-MSANTEID     TO  WRK-DD
                   IF     (WRK-YMD3        =   SPACE  )  OR
                          (WRK-YMD3        >   WRK-YMD)
                       MOVE    WRK-YMD         TO  WRK-YMD3
                       IF      SANTEI-FSANTEIYMD(1:6)  =   SANTEI-SRYYM
                           MOVE    SANTEI-FSANTEIYMD
                                               TO  WRK-FSANTEIYMD2
                       END-IF
                   END-IF
               END-IF
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           .
       300111-DEL-SYOKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全算定履歴初回算定日変更処理
      *****************************************************************
       400-SANTEI-ALLUPD-SEC                SECTION.
      *
      *    すべての算定履歴の初回算定日を変更する
           MOVE    SSANTEI-SRYCD       TO  WRK-SRYCD
      *    算定履歴初回算定日変更処理
           PERFORM 3002-SANTEI-UPD-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   TBL-MAX  )  OR
                              (WRK-KBN     =   SPACE    )
               SET     TBL-IDX             TO  IDX
               IF      TBL-KBN (TBL-IDX)   =   WRK-KBN
                   MOVE    TBL-SRYCD (TBL-IDX)     TO  WRK-SRYCD
      *            算定履歴初回算定日変更処理
                   PERFORM 3002-SANTEI-UPD-SEC
               END-IF
           END-PERFORM
      *
           .
       400-SANTEI-ALLUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴初回算定日検索 処理
      *****************************************************************
       3002-SANTEI-UPD-SEC                SECTION.
      *
           MOVE    WRK-MAE-FSANTEIYMD  TO  WRK-FSANTEIYMD
      *    算定履歴検索
           MOVE    "SANTEI-KEY4"       TO  WRK-DBPATH
           PERFORM 1001-SANTEI-KEN-SEC
           PERFORM
               UNTIL      FLG-SANTEI       =   1
      *
                   MOVE    WRK-KON-FSANTEIYMD  TO  SANTEI-FSANTEIYMD
      *
                   MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *
                   MOVE    SANTEI-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                      DISPLAY "ORCSSANTEISANTEI UPD ERR:" SANTEI-KEY
                                                    ","   MCP-RC
                   END-IF
      *
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           .
       3002-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性疼痛指導管理料初回算定日変更処理
      *****************************************************************
       400-SANTEI-MANSEI-SEC         SECTION.
      *
           MOVE    WRK-FSANTEIYMD-OLD  TO  WRK-FSANTEIYMD
      *    算定履歴検索
           MOVE    "SANTEI-KEY4"       TO  WRK-DBPATH
           PERFORM 1001-SANTEI-KEN-SEC
           PERFORM
               UNTIL      FLG-SANTEI       =   1
      *
                   MOVE    WRK-KON-FSANTEIYMD  TO  SANTEI-FSANTEIYMD
      *
                   MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *
                   MOVE    SANTEI-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                      DISPLAY "ORCSSANTEISANTEI UPD ERR:" SANTEI-KEY
                                                    ","   MCP-RC
                   END-IF
      *
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           .
       400-SANTEI-MANSEI-EXT.
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
           MOVE    SSANTEI-SRYYMD      TO  ORC-DBYMD
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
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *
