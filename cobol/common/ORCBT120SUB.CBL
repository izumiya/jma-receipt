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
       PROGRAM-ID.                 ORCBT120SUB.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 日次統計用入院情報取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/12/15    NACL−小豆沢  新規作成
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
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-PTRSIINF                PIC 9(01).
           03  FLG-NYUIN-HIT1              PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  SYU-IDX                     PIC 9(04).
           03  COMB-IDX                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
      *    算定日退避
           03  WRK-SANTEIYMD.
               05  WRK-SANTEIYM.
                   07  WRK-SANTEIYY            PIC 9(04).
                   07  WRK-SANTEIMM            PIC 9(02).
               05  WRK-SANTEIDD                PIC 9(02).
      *
      *    前日算定日
           03  WRK-MAE-SANTEIYMD.
               05  WRK-MAE-SANTEIYM.
                   07  WRK-MAE-SANTEIYY        PIC 9(04).
                   07  WRK-MAE-SANTEIMM        PIC 9(02).
               05  WRK-MAE-SANTEIDD            PIC 9(02).
      *
      *    保険組合せ番号数字変換
           03  WRK-HKNCOMBINUM-X               PIC X(04).
           03  WRK-HKNCOMBINUM-9  REDEFINES  WRK-HKNCOMBINUM-X
                                               PIC 9(04).
      *
      *    レセプト種別判定用領域
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                       PIC X(01).
           03  WK-IHO                          PIC X(01).
           03  WK-RJN                          PIC X(01).
           03  WK-KOHI-CNT                     PIC 9(02).
           03  WK-HONKZKKBN                    PIC X(01).
           03  WRK-AGE                         PIC 9(03).
           03  WRK-RECESYUBETU                 PIC 9(04).
           03  WRK-NYUGAIKBN                   PIC X(01).
      *
       01  WRK-CONS-AREA.
      *    
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012". 
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    原爆保険番号 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".       
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
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
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    日次統計用入院情報パラメタ
           COPY    "CPORCBT120SUB.INC".
      *
       PROCEDURE                   DIVISION    USING
            ORCBT120SUB-AREA
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
           INITIALIZE                  LNK-ORCBT120SUB-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-ORCBT120SUB-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-ORCBT120SUB-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-ORCBT120SUB-RC      =    ZERO
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
           IF    ( LNK-ORCBT120SUB-SANTEIYMD    =   SPACE )
               MOVE    1               TO  LNK-ORCBT120SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    LNK-ORCBT120SUB-HOSPID   TO  PTINF-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-ORCBT120SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    診療科チェック
           IF     LNK-ORCBT120SUB-SRYKA    NOT =   SPACE
               CONTINUE
           ELSE
               MOVE    3               TO  LNK-ORCBT120SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスター検索）
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-ORCBT120SUB-HOSPID  TO  NACCT-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
           MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                           TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    4               TO  LNK-ORCBT120SUB-RC
      *        入院会計クローズ
               MOVE    "NYUINACCT-KEY17"    TO  ORC-DBPATH
               PERFORM   910-DB-CLOSE-SEC
               GO  TO  100-PRM-CHK-EXT
           ELSE
               MOVE    "NYUINACCT-KEY17"   TO  ORC-DBPATH
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    4              TO  LNK-ORCBT120SUB-RC
      *             入院会計クローズ
                    MOVE    "NYUINACCT-KEY17"    TO  ORC-DBPATH
                    PERFORM   910-DB-CLOSE-SEC
                    GO  TO  100-PRM-CHK-EXT
               ELSE
                    MOVE       LNK-ORCBT120SUB-SANTEIYMD
                                           TO  WRK-SANTEIYMD
                    IF     NACCT-DAY(WRK-SANTEIDD)   =  ZERO
                        MOVE   4              TO  LNK-ORCBT120SUB-RC
                    END-IF
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "NYUINACCT-KEY17"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *入院科／入退院区分／病棟番号／病室番号／退院事由／ドクターの取得
           MOVE    ZERO                     TO  FLG-NYUIN-HIT1
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCBT120SUB-HOSPID   TO  PTNYUINRRK-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    "PTNYUINRRK-KEY3"        TO  ORC-DBPATH
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK            =   ZERO
               MOVE     "PTNYUINRRK-KEY3"   TO  ORC-DBPATH
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK    =   1)  OR
                                 (FLG-NYUIN-HIT1    =   1)
                   IF   PTNYUINRRK-JTIKBN    =    "5" OR "6"
                       CONTINUE
                   ELSE
                       IF   (PTNYUINRRK-TENNYUYMD <=
                               LNK-ORCBT120SUB-SANTEIYMD)   AND
                            (PTNYUINRRK-TENSTUYMD >=
                               LNK-ORCBT120SUB-SANTEIYMD)
                           MOVE    1      TO   FLG-NYUIN-HIT1
      *                    入院科
                           MOVE    PTNYUINRRK-NYUINKA
                                         TO  LNK-ORCBT120SUB-NYUINKA
      *                    入退院区分
                           MOVE   3      TO  LNK-ORCBT120SUB-NYUIN-KBN
                           IF    LNK-ORCBT120SUB-SANTEIYMD  =  
                                                   PTNYUINRRK-NYUINYMD
                               MOVE   1  TO  LNK-ORCBT120SUB-NYUIN-KBN
                           END-IF
                           IF    LNK-ORCBT120SUB-SANTEIYMD  =  
                                                   PTNYUINRRK-TAIINYMD
                               MOVE   2  TO  LNK-ORCBT120SUB-NYUIN-KBN
                           END-IF
      *                    病棟番号
                           MOVE   PTNYUINRRK-BTUNUM
                                         TO  LNK-ORCBT120SUB-BTUNUM
      *                    病室番号
                           MOVE   PTNYUINRRK-BRMNUM(3:6)
                                         TO  LNK-ORCBT120SUB-BRMNUM
      *                    退院事由
                           MOVE   PTNYUINRRK-TAIINCD
                                         TO  LNK-ORCBT120SUB-TAIINCD
      *                    医師コード
                           MOVE   PTNYUINRRK-DRCD1
                                         TO  LNK-ORCBT120SUB-DRCD
                       END-IF
                   END-IF
                   MOVE    "PTNYUINRRK-KEY3"   TO  ORC-DBPATH
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE     5      TO      LNK-ORCBT120SUB-RC
           END-IF
      *    患者入院履歴クローズ
           MOVE    "PTNYUINRRK-KEY3"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *前日入院科／前日病棟番号／前日病室番号の取得
      *    前日日の算定
           INITIALIZE                           STS-AREA-DAY
           INITIALIZE                           LNK-DAY6-AREA
           MOVE    "61"                     TO  LNK-DAY6-IRAI
           MOVE    LNK-ORCBT120SUB-SANTEIYMD TO LNK-DAY6-KIJUN
           MOVE    "1"                      TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                       TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"                USING  STS-AREA-DAY
                                                   LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN          TO  WRK-MAE-SANTEIYMD
      *    入院履歴の検索
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCBT120SUB-HOSPID   TO  PTNYUINRRK-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    "PTNYUINRRK-KEY3"        TO  ORC-DBPATH
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK            =   ZERO
               MOVE     "PTNYUINRRK-KEY3"   TO  ORC-DBPATH
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK    =   1)
                   IF   PTNYUINRRK-JTIKBN    =    "5" OR "6"
                       CONTINUE
                   ELSE
                       IF   (PTNYUINRRK-TENNYUYMD <=
                               WRK-MAE-SANTEIYMD)   AND
                            (PTNYUINRRK-TENSTUYMD >=
                               WRK-MAE-SANTEIYMD)
      *                    入院科
                           MOVE    PTNYUINRRK-NYUINKA
                                       TO  LNK-ORCBT120SUB-MAE-NYUINKA
      *                    病棟番号
                           MOVE   PTNYUINRRK-BTUNUM
                                       TO  LNK-ORCBT120SUB-MAE-BTUNUM
      *                    病室番号
                           MOVE   PTNYUINRRK-BRMNUM(3:6)
                                       TO  LNK-ORCBT120SUB-MAE-BRMNUM
                       END-IF
                   END-IF
                   MOVE    "PTNYUINRRK-KEY3"       TO  ORC-DBPATH
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE     5      TO      LNK-ORCBT120SUB-RC
           END-IF
      *    患者入院履歴クローズ
           MOVE    "PTNYUINRRK-KEY3"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *    レセ電入院科の取得(算定日）
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    LNK-ORCBT120SUB-NYUINKA
                                       TO  SYS-1005-KBNCD
           MOVE    LNK-ORCBT120SUB-SANTEIYMD
                                       TO  ORC-DBYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *            レセ電診療科
                   IF      SYS-1005-RECESRYKA  =   SPACE
                       MOVE    SPACE       TO  
                                         LNK-ORCBT120SUB-RECENYUINKA
                   ELSE
                       MOVE    SYS-1005-RECESRYKA
                                     TO  LNK-ORCBT120SUB-RECENYUINKA
                   END-IF
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *    レセ電入院科の取得(前算定日）
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    LNK-ORCBT120SUB-MAE-NYUINKA
                                       TO  SYS-1005-KBNCD
           MOVE    LNK-ORCBT120SUB-SANTEIYMD
                                       TO  ORC-DBYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *            レセ電診療科
                   IF      SYS-1005-RECESRYKA  =   SPACE
                       MOVE    SPACE       TO  
                                     LNK-ORCBT120SUB-MAE-RECENYUINKA
                   ELSE
                       MOVE    SYS-1005-RECESRYKA
                                 TO  LNK-ORCBT120SUB-MAE-RECENYUINKA
                   END-IF
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *入院時の外泊取得
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCBT120SUB-HOSPID  TO  NACCT-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
           MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                           TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "NYUINACCT-KEY17"  TO  ORC-DBPATH
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    NACCT-DAY(WRK-SANTEIDD)
                                 TO  LNK-ORCBT120SUB-GAIHAKU-KBN
           END-IF
      *    入院会計クローズ
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *食事情報の取得（入院会計）
           IF     LNK-ORCBT120SUB-GAIHAKU-KBN  =  1  OR  2  OR  3
               CONTINUE
           ELSE
               INITIALIZE                          NYUINACCT-REC
               MOVE    LNK-ORCBT120SUB-HOSPID  TO  NACCT-HOSPID
               MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
               MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                               TO  NACCT-SRYYM
               MOVE    "NYUINACCT-KEY28"       TO  ORC-DBPATH
               PERFORM 910-NYUINACCT-SELECT-SEC
               IF      FLG-NYUINACCT           =   ZERO
                   MOVE     "NYUINACCT-KEY28"  TO  ORC-DBPATH
                   PERFORM   910-NYUINACCT-READ-SEC
                   MOVE    NACCT-DAY(WRK-SANTEIDD)
                                     TO  LNK-ORCBT120SUB-SKJ-KBN
               END-IF
      *       入院会計クローズ
               MOVE    "NYUINACCT-KEY28"       TO  ORC-DBPATH
               PERFORM   910-DB-CLOSE-SEC
           END-IF
      *
      *    収納情報の取得
           MOVE    ZERO                     TO  SYU-IDX
           INITIALIZE                           SYUNOU-REC
           MOVE    LNK-ORCBT120SUB-HOSPID   TO  SYU-HOSPID
           MOVE    "1"                      TO  SYU-NYUGAIKBN
           MOVE    LNK-ORCBT120SUB-PTID     TO  SYU-PTID
           MOVE    "SYUNOU-KEY2"            TO  ORC-DBPATH
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU                =  ZERO
               MOVE     "SYUNOU-KEY2"       TO  ORC-DBPATH
               PERFORM   910-SYUNOU-READ-SEC
               PERFORM   UNTIL   (FLG-SYUNOU    =   1)   OR
                                 (SYU-IDX       >=  10)
                   IF   SYU-DENPJTIKBN    =    "3"
                       CONTINUE
                   ELSE
                       IF   SYU-DENPPRTYMD = LNK-ORCBT120SUB-SANTEIYMD
                           PERFORM   210-SYUNOU-SET-SEC
                       END-IF
                   END-IF
                   MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
                   PERFORM 910-SYUNOU-READ-SEC
               END-PERFORM
           END-IF
      *    収納クローズ
           MOVE    "SYUNOU-KEY2"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *    保険組合せ番号の取得（収納が無い場合に入院会計から取得）
           IF    LNK-ORCBT120SUB-HKNCOMBINUM(1)    =   ZERO
               INITIALIZE                          NYUINACCT-REC
               MOVE    LNK-ORCBT120SUB-HOSPID  TO  NACCT-HOSPID
               MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
               MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                               TO  NACCT-SRYYM
               MOVE    "5"                     TO  NACCT-ZAISKBKBN
               MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
               PERFORM 910-NYUINACCT-SELECT-SEC
               IF      FLG-NYUINACCT           =   ZERO
                   MOVE     "NYUINACCT-KEY17"  TO  ORC-DBPATH
                   PERFORM   910-NYUINACCT-READ-SEC
                   MOVE    NACCT-DAY(WRK-SANTEIDD)
                               TO  LNK-ORCBT120SUB-HKNCOMBINUM(1)
               END-IF
      *        入院会計クローズ
               MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
               PERFORM   910-DB-CLOSE-SEC
           END-IF
      *
      *
      *    保険情報の取得
           PERFORM    VARYING   SYU-IDX   FROM  1  BY  1
             UNTIL   (LNK-ORCBT120SUB-HKNCOMBINUM(SYU-IDX) = ZERO) OR
                     (SYU-IDX     >     10)
               INITIALIZE                      HKNCOMBI-REC
               MOVE    LNK-ORCBT120SUB-HOSPID
                                        TO     COMB-HOSPID
               MOVE    LNK-ORCBT120SUB-PTID
                                        TO      COMB-PTID
               MOVE    LNK-ORCBT120SUB-HKNCOMBINUM(SYU-IDX)
                                        TO      COMB-HKNCOMBINUM
               MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
               PERFORM 910-HKNCOMBI-SELECT-SEC
               IF    FLG-HKNCOMBI       =       ZERO
                   MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
                   PERFORM 910-HKNCOMBI-READ-SEC
                   IF    FLG-HKNCOMBI   =       ZERO
                       MOVE   COMB-HKNNUM
                                  TO  LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                       MOVE   COMB-HKNID
                                  TO  LNK-ORCBT120SUB-HKNID(SYU-IDX)
                       MOVE   COMB-HONKZKKBN
                               TO  LNK-ORCBT120SUB-HONKZKKBN(SYU-IDX)
                       MOVE   COMB-HOJOKBN
                                  TO  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                       MOVE   COMB-CONTKBN
                                  TO  LNK-ORCBT120SUB-CONTKBN(SYU-IDX)
                       PERFORM   220-HKNJANUM-SET-SEC
                       IF        COMB-HKNNUM  =  "971"  OR  "973"
                           PERFORM   230-PTRSIINF-SET-SEC
                       END-IF
                       PERFORM   VARYING  COMB-IDX  FROM  1  BY  1
                                 UNTIL   (COMB-KOHID(COMB-IDX) = ZERO)
                                    OR   (COMB-IDX    >   4)
                           MOVE   COMB-KOHHKNNUM(COMB-IDX)
                               TO  LNK-ORCBT120SUB-KOH-HKNNUM
                                                 (SYU-IDX COMB-IDX)
                           MOVE   COMB-KOHID(COMB-IDX)
                               TO  LNK-ORCBT120SUB-KOH-ID
                                                 (SYU-IDX COMB-IDX)
                           MOVE   COMB-KOHPAYKBN(COMB-IDX)
                               TO  LNK-ORCBT120SUB-KOH-PAYKBN
                                                 (SYU-IDX COMB-IDX)
                       END-PERFORM
                   END-IF
               END-IF
      *        保険組合せクローズ
               MOVE    "HKNCOMBI-KEY"       TO  ORC-DBPATH
               PERFORM   910-DB-CLOSE-SEC
           END-PERFORM
      *
      *    レセプト種別の取得
           PERFORM    VARYING   SYU-IDX   FROM  1  BY  1
             UNTIL   (LNK-ORCBT120SUB-HKNCOMBINUM(SYU-IDX) = ZERO) OR
                     (SYU-IDX     >     10)
               PERFORM   240-RECESYUBETU-SET-SEC
               MOVE      WRK-RECESYUBETU    TO
                                 LNK-ORCBT120SUB-RECESYUBETU(SYU-IDX)
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納セット処理
      *****************************************************************
       210-SYUNOU-SET-SEC         SECTION.
      *
           ADD    1     TO   SYU-IDX
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  LNK-ORCBT120SUB-DENPNUM
                                                           (SYU-IDX)
      *    伝票状態区分
           MOVE    SYU-DENPJTIKBN      TO  LNK-ORCBT120SUB-DENPJTIKBN
                                                           (SYU-IDX)
      *    伝票作成区分
           MOVE    SYU-CREATEKBN       TO  LNK-ORCBT120SUB-CREATEKBN
                                                           (SYU-IDX)
      *    保険点数
           MOVE    SYU-TOTAL-HKNTEN    TO  LNK-ORCBT120SUB-HKNTEN
                                                           (SYU-IDX)
      *    保険負担金額
           COMPUTE    LNK-ORCBT120SUB-HKNMONEY (SYU-IDX)
                                       =   SYU-HKNTEKMONEY
                                       +   SYU-RJN-FTN
                                       +   SYU-KOH-FTN
      *    保険適用外金額
           COMPUTE    LNK-ORCBT120SUB-TGMONEY (SYU-IDX)
                                       =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       -   SYU-TGMONEY-TAX-SAI
      *    保険適用外税額
           MOVE    SYU-TGMONEY-TAX-SAI
                                       TO  LNK-ORCBT120SUB-TGMONEY-TAX
                                                           (SYU-IDX)
      *    自費金額 
           COMPUTE LNK-ORCBT120SUB-JIHI (SYU-IDX)
                                       =  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
      *    自費税額
           MOVE    SYU-JIHI-TAX        TO  LNK-ORCBT120SUB-JIHI-TAX
                                                           (SYU-IDX)
      *    食事負担金額
           COMPUTE  LNK-ORCBT120SUB-SKJ (SYU-IDX)
                                       =   SYU-SKYMONEY-SKJ
                                       +   SYU-SKYMONEY-SKJ-JIHI
      *    食事消費税額
           COMPUTE  LNK-ORCBT120SUB-SKJ-TAX (SYU-IDX)
                                       =   SYU-SKYMONEY-SKJ-TAX
                                       +   SYU-SKYMONEY-SKJ-JIHI-TAX
      *    室料差額
           COMPUTE  LNK-ORCBT120SUB-RMSAGAKU (SYU-IDX)
                                       =   SYU-RMSAGAKU
                                       -   SYU-RMSAGAKU-TAX-SAI
      *    室料差額税額
           MOVE     SYU-RMSAGAKU-TAX-SAI   TO  
                                LNK-ORCBT120SUB-RMSAGAKU-TAX (SYU-IDX)
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  LNK-ORCBT120SUB-SKYMONEY
                                                           (SYU-IDX)
      *    労災／自賠責がある場合の請求額
           IF      SYU-RSIJIBAI-SKYMONEY   NOT =  ZERO
               MOVE   SYU-RSIJIBAI-SKYMONEY
                                       TO  LNK-ORCBT120SUB-SKYMONEY
                                                           (SYU-IDX)
           END-IF
      *
      *    保険組合せ番号
           MOVE    SYU-HKNCOMBINUM     TO  WRK-HKNCOMBINUM-X
           MOVE    WRK-HKNCOMBINUM-9   TO  LNK-ORCBT120SUB-HKNCOMBINUM
                                                           (SYU-IDX)
      *
           .
       210-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号セット処理
      *****************************************************************
       220-HKNJANUM-SET-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    LNK-ORCBT120SUB-HOSPID
                                    TO     PTHKN-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID
                                    TO     PTHKN-PTID
           MOVE    COMB-HKNID       TO     PTHKN-HKNID
           MOVE    "PTHKNINF-KEY"   TO     ORC-DBPATH
           PERFORM 910-PTHKNINF-SELECT-SEC
           IF    FLG-PTHKNINF       =       ZERO
               MOVE    "PTHKNINF-KEY"   TO      ORC-DBPATH
               PERFORM 910-PTHKNINF-READ-SEC
               IF    FLG-PTHKNINF   =       ZERO
                   MOVE   PTHKN-HKNJANUM
                              TO  LNK-ORCBT120SUB-HKNJANUM(SYU-IDX)
               END-IF
           END-IF
      *    患者保険情報クローズ
           MOVE    "PTHKNINF-KEY"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       220-HKNJANUN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災情報セット処理
      *****************************************************************
       230-PTRSIINF-SET-SEC         SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    LNK-ORCBT120SUB-HOSPID
                                    TO     PTRSI-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID
                                    TO     PTRSI-PTID
           MOVE    COMB-HKNID       TO     PTRSI-HKNID
           MOVE    "PTRSIINF-KEY"   TO     ORC-DBPATH
           PERFORM 910-PTRSIINF-SELECT-SEC
           IF    FLG-PTRSIINF       =       ZERO
               MOVE    "PTRSIINF-KEY"   TO      ORC-DBPATH
               PERFORM 910-PTRSIINF-READ-SEC
               IF    FLG-PTRSIINF   =       ZERO
                   MOVE   PTRSI-HKNKBN
                              TO  LNK-ORCBT120SUB-RSIHKNKBN(SYU-IDX)
                   MOVE   PTRSI-JIBAISEIKBN
                              TO  LNK-ORCBT120SUB-JIBAIKBN(SYU-IDX)
                   MOVE   PTRSI-SAIGAIKBN
                              TO  LNK-ORCBT120SUB-SAIGAIKBN(SYU-IDX)
               END-IF
           END-IF
      *    患者労災保険情報クローズ
           MOVE    "PTRSIINF-KEY"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       230-PTRSIINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       240-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
           MOVE    "1"                 TO  WRK-NYUGAIKBN
      *
      *    労災・自賠責のとき
           IF      LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                       =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM
               EVALUATE    LNK-ORCBT120SUB-RSIHKNKBN(SYU-IDX)
                                       ALSO
                           LNK-ORCBT120SUB-SAIGAIKBN(SYU-IDX)
                                       ALSO
                           WRK-NYUGAIKBN
                   WHEN    "1"         ALSO    "1"     ALSO    "1"   
                       MOVE    7081        TO    WRK-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "1"
                       MOVE    7082        TO    WRK-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "1"
                       MOVE    7101        TO    WRK-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "1"
                       MOVE    7102        TO    WRK-RECESYUBETU
                   WHEN    "1"         ALSO    "1"     ALSO    "2"   
                       MOVE    7071        TO    WRK-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "2"
                       MOVE    7072        TO    WRK-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "2"
                       MOVE    7091        TO    WRK-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "2"
                       MOVE    7092        TO    WRK-RECESYUBETU
                   WHEN    "3"         ALSO    ANY     ALSO    ANY
                       MOVE    7021        TO    WRK-RECESYUBETU
                   WHEN    "4"         ALSO    ANY     ALSO    ANY
                       MOVE    7011        TO    WRK-RECESYUBETU
                   END-EVALUATE
               GO  TO  240-RECESYUBETU-SET-EXT
           END-IF
      *
      *    自費のとき
           IF     LNK-ORCBT120SUB-HKNNUM(SYU-IDX)(1:2) =   "98"
               MOVE    8888                TO  WRK-RECESYUBETU
               GO  TO  240-RECESYUBETU-SET-EXT
           END-IF
      *    年齢計算
           INITIALIZE                      PTINF-REC
           MOVE    LNK-ORCBT120SUB-HOSPID
                                       TO  PTINF-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID
                                       TO  PTINF-PTID
           PERFORM    900-PTINF-READ-SEC
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    LNK-ORCBT120SUB-HOSPID
                                       TO  AGECHK-HOSPID
           MOVE    LNK-ORCBT120SUB-PTID
                                       TO  AGECHK-PTID
           MOVE    LNK-ORCBT120SUB-SANTEIYMD
                                       TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
           IF      AGECHK-RC           =   ZERO
               MOVE    AGECHK-NENREI       TO  WRK-AGE
           ELSE
               MOVE    ZERO                TO  WRK-AGE
           END-IF
      *
           IF      LNK-ORCBT120SUB-HKNNUM(SYU-IDX)  NOT =   SPACE
               IF      LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                           =   "060"   OR   "067"
                   IF      LNK-ORCBT120SUB-HKNNUM(SYU-IDX)  =   "060"
                        MOVE   "1"             TO  WK-KOKUHO
                   ELSE
                        MOVE   "2"             TO  WK-KOKUHO
                   END-IF
               ELSE
                   MOVE    "1"             TO  WK-IHO
               END-IF
           END-IF
      *
           IF      LNK-ORCBT120SUB-SANTEIYMD(1:6)    <      "200210"
               IF    LNK-ORCBT120SUB-KOH1HKNNUM(SYU-IDX)  =   "027"
                   MOVE        "1"             TO  WK-RJN
               END-IF
           END-IF
      *
           MOVE    LNK-ORCBT120SUB-HONKZKKBN(SYU-IDX) TO WK-HONKZKKBN
      *
           MOVE    ZERO                TO  WK-KOHI-CNT
           PERFORM VARYING IDX FROM    1   BY  1
             UNTIL  (IDX     >       4)   OR
                    (LNK-ORCBT120SUB-KOH-HKNNUM(SYU-IDX IDX) = SPACE)
               IF LNK-ORCBT120SUB-KOH-HKNNUM(SYU-IDX IDX) NOT = "027"
                   ADD     1               TO  WK-KOHI-CNT
               END-IF
           END-PERFORM
      *
           IF  LNK-ORCBT120SUB-HKNNUM(SYU-IDX)  NOT =   SPACE
               IF  LNK-ORCBT120SUB-SANTEIYMD(1:6)    >=  "200210"
      *            ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
                   IF      WRK-AGE             <     3
                       MOVE    "3"             TO   WK-HONKZKKBN
                   END-IF
      *
      *            １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *            "９"または"８"をセット
      *            老人保険の場合はＷＫ−ＲＪＮにセットする	
                   EVALUATE    LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                       WHEN    "002" 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                           IF  (WRK-AGE        >=   70)  AND
                               (WRK-AGE        <=   74)
                              EVALUATE LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                              END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF   LNK-ORCBT120SUB-KOH1HKNNUM(SYU-IDX)  =   "027"
                       EVALUATE   LNK-ORCBT120SUB-KOH1PAYKBN(SYU-IDX)
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )     
           AND   ( LNK-ORCBT120SUB-HONKZKKBN(SYU-IDX)  =   "1"   )     
               IF  LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                       =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *
      *      DISPLAY "HONKZKKBN : " WK-HONKZKKBN 
      *      DISPLAY "NYUGAIKBN : " TOUKEI01-NYUGAIKBN 
      *      DISPLAY "IHO       : " WK-IHO 
      *      DISPLAY "KOKUHO    : " WK-KOKUHO 
      *      DISPLAY "RJN       : " WK-RJN 
      *      DISPLAY "KOHI-CNT  : " WK-KOHI-CNT 
      *
      *    平成１４年９月まで
           IF      LNK-ORCBT120SUB-SANTEIYMD(1:6)    <=      "200209"
             EVALUATE  WK-HONKZKKBN ALSO WRK-NYUGAIKBN   ALSO
                    WK-IHO ALSO WK-KOKUHO ALSO WK-RJN ALSO WK-KOHI-CNT
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1122            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1122            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1125            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1126            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1131            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1131            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1132            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1132            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1135            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1136            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1311            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1312            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1321            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1322            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1331            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1332            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1411            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1412            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1415            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1416            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1421            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1422            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1425            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1426            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1431            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1432            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1435            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1436            TO    WRK-RECESYUBETU
             END-EVALUATE
           END-IF
      *    平成１４年１０月改正対応
           IF      LNK-ORCBT120SUB-SANTEIYMD(1:6)    >=      "200210"
             EVALUATE  WK-HONKZKKBN ALSO WRK-NYUGAIKBN   ALSO
                    WK-IHO ALSO WK-KOKUHO ALSO WK-RJN ALSO WK-KOHI-CNT
      *保険単独
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    WRK-RECESYUBETU
      *保険と公費１種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120            TO    WRK-RECESYUBETU
      *保険と公費２種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130            TO    WRK-RECESYUBETU
      *公費単独
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212            TO    WRK-RECESYUBETU
      *２種の公費併用
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222            TO    WRK-RECESYUBETU
      *３種の公費併用
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232            TO    WRK-RECESYUBETU
      *老人単独
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1317            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1318            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1319            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1319            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1310            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1310            TO    WRK-RECESYUBETU
      *老人と公費１種の併用
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1327            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1327            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1328            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1328            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1329            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1329            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1320            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1320            TO    WRK-RECESYUBETU
      *老人と公費２種の併用
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1337            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1337            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1338            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1338            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1339            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1339            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1330            TO    WRK-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1330            TO    WRK-RECESYUBETU
      *退職者単独
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1417            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1418            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1409            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1410            TO    WRK-RECESYUBETU
      *退職者と公費１種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1427            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1428            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1429            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1420            TO    WRK-RECESYUBETU
      *退職者と公費２種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431            TO    WRK-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433            TO    WRK-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435            TO    WRK-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1437            TO    WRK-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1438            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1439            TO    WRK-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1430            TO    WRK-RECESYUBETU
             END-EVALUATE
           END-IF
           .
       240-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                 PTINF-REC
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "PTINF-KEY"        TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       910-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       910-SYUNOU-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SYUNOU-REC
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               INITIALIZE                     SYUNOU-REC
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター検索
      *****************************************************************
       910-PTHKNINF-SELECT-SEC        SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター読込
      *****************************************************************
       910-PTHKNINF-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTHKNINF-REC
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                     PTHKNINF-REC
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスター検索
      *****************************************************************
       910-PTRSIINF-SELECT-SEC        SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTRSIINF
           ELSE
               MOVE    1                  TO  FLG-PTRSIINF
           END-IF
      *
           .
       910-PTRSIINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスター読込
      *****************************************************************
       910-PTRSIINF-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTRSIINF-REC
               MOVE    ZERO               TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                     PTRSIINF-REC
               MOVE    1                  TO  FLG-PTRSIINF
           END-IF
      *
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
