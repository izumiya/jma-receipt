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
       PROGRAM-ID.             ORCHCM19.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 処方箋　印刷（Ａ５）
      *                      Ｈ１４．１０対応分（ORCHC19のCOPY）
      *                      MONPE で作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/09/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/10/07  在宅薬剤・器材の院外用追加
      * 01.00.02     NACL-多々納  02/10/25  入院対応
      * 01.00.03     NACL-多々納  02/11/01  受給者番号の全角８文字以上対応
      * 01.00.04     NACL-多々納  02/12/29  高８、高９編集修正
      * 01.00.05     NACL-多々納  03/01/20  その他器材の編集追加
      *                                     中途の在宅編集修正
      * 01.00.06     NACL-多々納  03/03/06  剤明細１９以上の編集修正
      *                                     全角変換時空白判定追加
      * 01.00.07     NACL-多々納  03/04/17  免除判定削除
      * 01.00.08     NACL-多々納  03/06/18  用法２０文字以上編集修正
      * 01.00.09     NACL-多々納  03/06/27  出力先プリンタサブ追加
      * 01.00.10     NACL-多々納  03/12/16  院内処方（213,223,233)を
      *                                     院内処方せんのみの印刷とする
      * 01.00.11     NACL-多々納  04/03/08  在宅の対象判定修正
      *****************************************************************
      *
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
           COPY    "HCM19.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-ZAITAKU         PIC 9(01).
           03  FLG-ZAITAKU-ARI     PIC 9(01).
      *
           03  FLG-INNAI           PIC 9(01).
      *
           03  FLG-SYOHOU          PIC 9(01).
      *
           03  FLG-PRINT           PIC 9(01).
      *    麻薬区分
           03  FLG-MAYAKU          PIC 9(01).
      *
           03  FLG-OK2              PIC 9(01).
      *    老人フラグ
           03  FLG-RJNHKN              PIC 9(01).
      *    対象外フラグ
           03  FLG-TAIGAI              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDXA2               PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
           03  IDX-B               PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
      *
           03  KETA                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-PAGE            PIC 9(03).
      *
           03  WRK-DRCD            PIC  X(05).
      *
           03  WRK-RENNUM          PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-HKNJA           PIC X(08).
           03  WRK-HKNJA-G         PIC X(16).
      *
           03  WRK-FTNSNO          PIC X(16).
      *****03  WRK-JKYSNO          PIC X(14).
           03  WRK-JKYSNO          PIC X(20).
           03  WRK-FTNJA1          PIC X(16).
      **** 03  WRK-JKYSNUM1        PIC X(14).
           03  WRK-JKYSNUM1        PIC X(20).
           03  WRK-FTNJA2          PIC X(16).
      **** 03  WRK-JKYSNUM2        PIC X(14).
           03  WRK-JKYSNUM2        PIC X(20).
      *
           03  WRK-SURYO           PIC 9(05)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(03).
      *
           03  WRK-SURYO-X.
      *********05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1    PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZ.
           03  WRK-SURYO-N         PIC X(18).
      *
           03  WRK-TANINAME        PIC X(04).
           03  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-KAISU           PIC 9(05).
      *
           03  WRK-TNS-NAME        PIC X(200).
      *
           03  WRK-ZAITENKEI       PIC 9(07).
           03  WRK-YKZTEN          PIC 9(07).
           03  WRK-KIZAITEN        PIC 9(07).
      *
      *    文字変換
           03  WRK-ZENKAKU-G           PIC X(100).
           03  WRK-MOJI                PIC X(100).
           03  WRK-MOJISU              PIC 9(02).
      *---(01.00.01) LINE ADD START -------------------------------------
           03  WRK-FTNKBN              PIC X(20).
      *---(01.00.01) LINE ADD END   -------------------------------------
           03  WRK-KIGONUM             PIC X(80).
           03  WRK-NUM                 PIC X(80).
      *
           03  WRK-HEN-KIGONUM         PIC X(80).
      *
      *    麻薬編集
           03  WRK-HENSYU-AREA.
               05  WRK-HENSYU          PIC X(86)   OCCURS  3.
           03  IDX-H                   PIC 9(04).
           03  WRK-ADRS                PIC X(200).
      *---(01.00.08) LINE ADD START -----------------------------------
      *    保険組合せ
           03  WRK-HKNCOMBINUM         PIC X(04).
      *---(01.00.08) LINE ADD END   ------------------------------------
      *
      *    負担割合
           03  WRK-BIKOU               PIC X(86).
      *
      *    剤毎の内容
       01  WRK-NAIYOU-AREA.
           03  WRK-NAIYOU-TBL          OCCURS   100.
               05  WRK-NAIYOU-Z            PIC X(86).
               05  WRK-NAIYOU-R1           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-NO              PIC X(04).
                   07  WRK-K2              PIC X(02).
                   07  WRK-MEISYOU1        PIC X(44).
                   07  WRK-SURYOU          PIC X(18).
                   07  WRK-TANI            PIC X(12).
                   07  WRK-FIL11           PIC X(06).
               05  WRK-NAIYOU-R2           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-FIL21           PIC X(06).
                   07  WRK-MEISYOU2        PIC X(44).
                   07  WRK-FIL22           PIC X(22).
                   07  WRK-K3              PIC X(02).
                   07  WRK-KAISUU          PIC X(06).
                   07  WRK-HINICHI         PIC X(06).
               05  WRK-NAIYOU-R3           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-FIL23           PIC X(06).
                   07  WRK-MEISYOU3        PIC X(80).
      *
      *    処方内容
       01  WRK-SYOHOU-AREA.
           03  WRK-SYOHOU-PAGE         OCCURS   10.
               05  WRK-SYOHOU-TBL          OCCURS   20.
                   07  WRK-NAIYOU          PIC X(86).
      *
           03  WRK-PAGE-MAX                PIC 9(03).
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    免除判定領域
       01  WRK-MENJYO-ARAE.
           03  WRK-MENJYO-FLG          PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *    処方箋ＧＥ表示
           COPY    "CPSK1030.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *---(01.00.07) LINE ADD START ------------------------------------
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   免除判定サブ
           COPY    "CPORCSMENJYO.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    処方箋共通パラメタ
           COPY    "CPORCHC19.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC19AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  WRK-MENJYO-ARAE
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *    処方内容編集
           PERFORM 200-SYOHOU-HEN-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-PAGE-MAX
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HCM19-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    "HCM19"             TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *    変更★★★★★★
      *****MOVE    "001"               TO  HC01PARA-CNT
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT               PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      *　　印刷用データ出力
            WRITE  HCM19
      *
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HCM19.red"         TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
            MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(2)   TO  MKPRT-PRTNM
            CALL   "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
      *
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    ZERO                TO  SYS-1001-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    ZERO                TO  SYS-1002-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1002-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-DRCD            TO  SYS-1010-KBNCD
           MOVE    ZERO                TO  SYS-1010-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1010-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SRYKA           TO  SYS-1005-KBNCD
           MOVE    ZERO                TO  SYS-1005-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1005-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
      **** MOVE    SPACE               TO  SYS-1031-REC
      *    INITIALIZE                  SYS-1031-REC
      *    MOVE    "1031"              TO  SYS-1031-KANRICD
      *    MOVE    "*"                 TO  SYS-1031-KBNCD
      *    MOVE    SYS-1031-REC        TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *        PERFORM 900-KANRIMST-READ-SEC
      *        IF      MCP-RC              =   ZERO
      *            MOVE    MCPDATA-REC     TO  SYS-1031-REC
      *        END-IF
      *    ELSE
      *        INITIALIZE                  SYS-1031-REC
      *        MOVE    1               TO  FLG-SYSKANRI
      *    END-IF
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      ******                            MCPDATA-REC
      *
      *    処方箋ＧＥ出力情報編集
           MOVE    SPACE               TO  SYS-1030-REC
           INITIALIZE                  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1030-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1030-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
      **********DISPLAY "*** HKNCOMBI  エラー:"   COMB-KEY "."
                INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
      *
      *    患者保険情報検索
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPID          TO  PTHKN-HOSPID
           MOVE    SPA-PTID            TO  PTHKN-PTID
      *    主保険−ＩＤ
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM 920-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF      NOT =   ZERO
               INITIALIZE              PTHKNINF-REC
           END-IF
      *
      *    保険者番号変換処理
           MOVE    SPACE               TO  WRK-HKNJA
           MOVE    9                   TO  IDY
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    8   BY  -1
                   UNTIL       IDX     <   1
               IF      PTHKN-HKNJANUM(IDX:1)   NOT =   SPACE
                   MOVE    1               TO  FLG-OK
               END-IF
               IF      FLG-OK              =   1
                   COMPUTE IDY         =   IDY     -   1
                   MOVE    PTHKN-HKNJANUM(IDX:1)   TO  WRK-HKNJA (IDY:1)
               END-IF
           END-PERFORM
           MOVE    WRK-HKNJA           TO  WRK-MOJI
           MOVE    8                   TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:16) TO  WRK-HKNJA-G
      *
      *    保険番号マスター検索
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPID          TO  HKN-HOSPID
           MOVE    COMB-HKNNUM         TO  HKN-HKNNUM
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKSTYMD
           PERFORM 930-HKNNUM-READ-SEC
           IF      FLG-HKNNUM      NOT =   ZERO
               INITIALIZE              HKNNUM-REC
           END-IF
      *
      *    公費情報１
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPID          TO  PTKOH-HOSPID
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    COMB-KOH1ID         TO  PTKOH-KOHID
           PERFORM 940-PTKOHINF-READ-SEC
           IF      FLG-PTKOHINF        =   ZERO
               PERFORM 3102-PTKOHINF-HEN-SEC
               MOVE    WRK-FTNSNO      TO  WRK-FTNJA1
               MOVE    WRK-JKYSNO      TO  WRK-JKYSNUM1
           END-IF
      *
      *    公費情報２
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPID          TO  PTKOH-HOSPID
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    COMB-KOH2ID         TO  PTKOH-KOHID
           PERFORM 940-PTKOHINF-READ-SEC
           IF      FLG-PTKOHINF          =   ZERO
               PERFORM 3102-PTKOHINF-HEN-SEC
               MOVE    WRK-FTNSNO      TO  WRK-FTNJA2
               MOVE    WRK-JKYSNO      TO  WRK-JKYSNUM2
           END-IF
      *
      *    記号・番号
      *    全角変換
           IF      PTHKN-KIGO          =   SPACE
               MOVE    SPACE               TO  WRK-KIGONUM
           ELSE
               MOVE    PTHKN-KIGO          TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                           TO  WRK-KIGONUM
           END-IF
           IF      PTHKN-NUM           =   SPACE
               MOVE    SPACE               TO  WRK-NUM
           ELSE
               MOVE    PTHKN-NUM           TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                           TO  WRK-NUM
           END-IF
      *
           STRING  WRK-KIGONUM         DELIMITED   BY  SPACE
                   "・"                DELIMITED   BY  SIZE
                   WRK-NUM             DELIMITED   BY  SPACE
                   INTO                    WRK-HEN-KIGONUM
           END-STRING
      *
      *---(01.00.07) LINE ADD START ------------------------------------
      *
      *    労災の時、保険者番号・記号の変更
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
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
               IF      FLG-PTRSIINF        =   ZERO
                   MOVE    SPACE               TO  WRK-HEN-KIGONUM
                   MOVE    SPACE               TO  WRK-HKNJA-G
      *            保険者番号へ記号（労災区分名）
                   MOVE    WRK-KIGONUM         TO  WRK-HKNJA-G
      *            記号へ患者交付番号
                   IF      PTRSI-KOFUNUM       =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    PTRSI-KOFUNUM       TO  WRK-MOJI
                       PERFORM 2009-HENKAN-SEC
                       MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                                   TO  WRK-HEN-KIGONUM
                   END-IF
      *
               END-IF
           END-IF
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
      *---(01.00.01) LINE ADD START ------------------------------------
      *
      *****自己負担編集
      *    MOVE    SPACE               TO  WRK-FTNKBN
      *    老人判定
      *    IF     (COMB-KOH1HKNNUM     =   "027"   )  OR
      *           (COMB-KOH2HKNNUM     =   "027"   )  OR
      *           (COMB-KOH3HKNNUM     =   "027"   )  OR
      *           (COMB-KOH4HKNNUM     =   "027"   )
      *        EVALUATE    SYS-1001-HOSPSBT
      *            WHEN    1
      *                IF      SYS-1001-BEDSU      >=   200
      *                    MOVE    "　病２００以上"       TO  WRK-FTNKBN
      *                ELSE
      *                    MOVE    "　病２００未満"       TO  WRK-FTNKBN
      *                END-IF
      *            WHEN    2
      ***              IF      SYS-1001-ROUPAYKBN  =   "1"
      *                    MOVE    "　診　定　額"       TO  WRK-FTNKBN
      *                ELSE
      *                    MOVE    "　診　定　率"       TO  WRK-FTNKBN
      ***              END-IF
      *                EVALUATE    SYS-1001-ROUPAYKBN
      *                    WHEN    "1"
      *                        MOVE    "　診　定　率"
      *                                                 TO  WRK-FTNKBN
      *                    WHEN    "2"
      *                        MOVE    "　診　定　額"
      *                                                 TO  WRK-FTNKBN
      *                    WHEN    OTHER
      *                        MOVE    SPACE            TO  WRK-FTNKBN
      *                END-EVALUATE
      *        END-EVALUATE
      *****END-IF
      *
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    免除判定処理
      **** INITIALIZE                  ORCSMENJYOAREA
      *    CALL    "ORCSMENJYO"        USING
      *                                ORCSMENJYOAREA
      *****                            SPA-AREA
      *
      *    ３歳未満・老人判定
           MOVE    SPACE               TO  WRK-BIKOU
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPID          TO  AGECHK-HOSPID
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
      *    ３歳未満
           IF      AGECHK-NENREI       <   03
               MOVE    "（３歳）"          TO  WRK-BIKOU
           END-IF
      *---(01.00.04) LINE UPDATE START ---------------------------------
      *    ７０歳以上
      *****IF      AGECHK-NENREI       >=  70
      *        EVALUATE    AGECHK-FTNRATE
      *            WHEN    10
      *                MOVE    "（高９）"          TO  WRK-BIKOU
      *            WHEN    20
      *                MOVE    "（高８）"          TO  WRK-BIKOU
      *        END-EVALUATE
      **** END-IF
      *    ７０歳以上
           MOVE    ZERO                TO  FLG-RJNHKN
           IF      AGECHK-NENREI       >=  70
      *        老人保険
               EVALUATE    COMB-HKNNUM
                   WHEN    "060"
      *                国保
                       EVALUATE    COMB-HOJOKBN
                           WHEN    "1"
                               MOVE    1           TO  FLG-RJNHKN
                           WHEN    "2"
                               MOVE    2           TO  FLG-RJNHKN
                       END-EVALUATE
      *                以外
                   WHEN    OTHER
      *                自費以外
                       IF      COMB-HKNNUM (1:2)   =   "98"
                           CONTINUE
                       ELSE
                           EVALUATE    COMB-HOJOKBN
                               WHEN    "9"
                               WHEN    "A"
                               WHEN    "B"
                               WHEN    "C"
      *                        １割
                                   MOVE    1           TO  FLG-RJNHKN
                               WHEN    "8"
                               WHEN    "D"
                               WHEN    "E"
                               WHEN    "F"
      *                        ２割
                                   MOVE    2           TO  FLG-RJNHKN
                           END-EVALUATE
                       END-IF
               END-EVALUATE
           END-IF
      *    老人公費（保険より公費の割合を優先する）
           EVALUATE    TRUE
               WHEN    COMB-KOH1HKNNUM     =  "027"
                   EVALUATE    COMB-KOH1PAYKBN
                       WHEN    "01"
      *                        １割
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "02"
      *                        ２割
                           MOVE    2           TO  FLG-RJNHKN
                   END-EVALUATE
               WHEN    COMB-KOH2HKNNUM     =  "027"
                   EVALUATE    COMB-KOH2PAYKBN
                       WHEN    "01"
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "02"
                           MOVE    2           TO  FLG-RJNHKN
                   END-EVALUATE
               WHEN    COMB-KOH3HKNNUM     =  "027"
                   EVALUATE    COMB-KOH3PAYKBN
                       WHEN    "01"
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "02"
                           MOVE    2           TO  FLG-RJNHKN
                   END-EVALUATE
               WHEN    COMB-KOH4HKNNUM     =  "027"
                   EVALUATE    COMB-KOH4PAYKBN
                       WHEN    "01"
                           MOVE    1           TO  FLG-RJNHKN
                       WHEN    "02"
                           MOVE    2           TO  FLG-RJNHKN
                   END-EVALUATE
           END-EVALUATE
      *
           EVALUATE    FLG-RJNHKN
               WHEN    1
                   MOVE    "（高９）"          TO  WRK-BIKOU
               WHEN    2
                   MOVE    "（高８）"          TO  WRK-BIKOU
           END-EVALUATE
      *---(01.00.04) LINE UPDATE END   ---------------------------------
      *
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者公費変換処理
      *****************************************************************
       3102-PTKOHINF-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-FTNSNO
           MOVE    SPACE               TO  WRK-JKYSNO
      *    保険者番号
      *****MOVE    PTKOH-FTNJANUM      TO  WRK-FTNSNO
      *    受給者番号
      *****MOVE    PTKOH-JKYSNUM       TO  WRK-JKYSNO
      *    保険者番号
           IF      PTKOH-FTNJANUM      =   SPACE
               MOVE    SPACE           TO  WRK-FTNSNO
           ELSE
               MOVE    PTKOH-FTNJANUM      TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                           TO  WRK-FTNSNO
           END-IF
      *    受給者番号
           IF      PTKOH-JKYSNUM       =   SPACE
               MOVE    SPACE               TO  WRK-JKYSNO
           ELSE
               MOVE    PTKOH-JKYSNUM       TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    SPACE               TO  WRK-JKYSNO
               MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
                                            TO  WRK-JKYSNO
           END-IF
      *
           .
       3102-PTKOHINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       200-SYOHOU-HEN-SEC                   SECTION.
      *
           INITIALIZE                  WRK-SYOHOU-AREA
           MOVE    1                   TO  WRK-PAGE-MAX
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-Z
      *    見出しのみ
           IF      ORCHC19-KBN         =   "2"
               MOVE    1                   TO  WRK-PAGE-MAX
               GO      TO      200-SYOHOU-HEN-EXT
           END-IF
      *---(01.00.08) LINE ADD START ------------------------------------
      *    保険選択がない時、見出しのみとする
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    1                   TO  WRK-PAGE-MAX
               MOVE    "2"                 TO  ORCHC19-KBN
               GO      TO      200-SYOHOU-HEN-EXT
           END-IF
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
      *---(01.00.08) LINE ADD END   ------------------------------------
      *
      *---(01.00.07) LINE ADD START ------------------------------------
      *    中途終了時、発行
           IF      ORCHC19-KBN         =   "4"  OR  "5"
               PERFORM 2003-WKSRYACT-SYORI-SEC
               GO      TO      200-SYOHOU-HEN-EXT
           END-IF
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
           IF      ORCHC19-RENNUM      =   ZERO
               MOVE    ZERO                TO  WRK-RENNUM
               PERFORM 2001-RENNUM-SET-SEC
           ELSE
               MOVE    ORCHC19-RENNUM      TO  WRK-RENNUM
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
      *        編集
               PERFORM 2002-SYOHOU-SYORI-SEC
           END-IF
      *
      *    麻薬編集
           IF      FLG-MAYAKU          =   1
               PERFORM 2003-MAYAKU-HEN-SEC
           END-IF
      *
           .
       200-SYOHOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       2002-SYOHOU-SYORI-SEC          SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-SYOHOU
      *
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *---(01.00.08) LINE ADD START ------------------------------------
             IF      JYURRK-HKNCOMBINUM    =   WRK-HKNCOMBINUM
      *---(01.00.08) LINE ADD END   ------------------------------------
      *
               PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                       UNTIL      (IDX-JYU         >   15  )   OR
                                  (JYURRK-ZAINUM (IDX-JYU) =   ZERO )
                   MOVE    JYURRK-ZAINUM (IDX-JYU) TO  WRK-ZAINUM
                   COMPUTE IDX-REN         =   JYURRK-RENNUM
                                           +   1
                   IF      IDX-REN         >   4
                       MOVE    4               TO  IDX-REN
                   END-IF
                   PERFORM 2002-SRYACT-SYORI-SEC
               END-PERFORM
      *
      *---(01.00.08) LINE ADD START ------------------------------------
             END-IF
      *---(01.00.08) LINE ADD END   ------------------------------------
      *
               MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    対象明細がない時、出力しない
           IF      IDX                 =   ZERO
               IF      ORCHC19-KBN         =   SPACE
      *************MOVE    ZERO                TO  WRK-PAGE-MAX
      *            処方箋料がある時、見出しのみ印刷とする
                   IF      FLG-SYOHOU          =   1
                       MOVE    1                   TO  WRK-PAGE-MAX
                   ELSE
                       MOVE    ZERO                TO  WRK-PAGE-MAX
                   END-IF
               ELSE
                   IF      ORCHC19-KBN         =   "3"
                       MOVE    ZERO                TO  WRK-PAGE-MAX
                   END-IF
               END-IF
      *---(01.00.01) LINE ADD START ------------------------------------
               MOVE    ZERO            TO  FLG-PRINT
           ELSE
      *        最終行のメッセージ編集
               ADD     1               TO  IDX
               IF      IDX             <=  20
                   MOVE    "　　　　　　以下余白"
                                       TO  WRK-NAIYOU (WRK-PAGE-MAX IDX)
               END-IF
      *---(01.00.01) LINE ADD START ------------------------------------
               MOVE    1               TO  FLG-PRINT
           END-IF
      *
           .
       2002-SYOHOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の連番確定処理
      *****************************************************************
       2001-RENNUM-SET-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-PRINT
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY7"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY7"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
               MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
               MOVE    ZERO                TO  FLG-SYORI
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   IF      SPA-HKNCOMBINUM     =   JYURRK-HKNCOMBINUM
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               END-IF
      *        編集
               IF      FLG-SYORI           =   1
                   PERFORM 2002-SYOHOU-SYORI-SEC
               END-IF
               IF      FLG-PRINT           =   1
                   MOVE    1               TO  FLG-JYURRK
               ELSE
                   MOVE    ZERO            TO  FLG-JYURRK
                   MOVE    "JYURRK-KEY7"   TO  ORC-DBPATH
                   PERFORM 970-JYURRK-NEXT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "JYURRK-KEY7"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *    受診履歴なし（見出しのみ）
           IF     (WRK-PAGE-MAX        =   ZERO ) OR
                  (FLG-PRINT           =   ZERO )
               MOVE    1                   TO  WRK-PAGE-MAX
           END-IF
      *
           .
       2001-RENNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HCM19-HEN-SEC                SECTION.
      *
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               INITIALIZE              PTINF-REC
           END-IF
      *
           INITIALIZE                  HCM19
      *
      *    診療科
      *****MOVE    SPA-SRYKAMEI        TO  HCM19-SRYKA
      *    患者番号
           MOVE    SPA-PTNUM           TO  HCM19-PTNUM
      *    患者名称カナ
           IF    ( PTINF-KANANAME (31 : )  =   SPACE )
               MOVE    PTINF-KANANAME      TO  HCM19-KANANAME
           ELSE
               MOVE    PTINF-KANANAME      TO  HCM19-KANANAME2
           END-IF
      *    患者名称
           IF    ( PTINF-NAME (27 : )  =   SPACE )
               MOVE    PTINF-NAME      TO  HCM19-NAME
           ELSE
               MOVE    PTINF-NAME      TO  HCM19-NAME2
           END-IF
      *
           MOVE    SPA-SEX             TO  HCM19-SEX
      *    生年月日
           MOVE    SPA-BIRTHDAY        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM19-BIRTH
      *
      *    医療機関住所
           MOVE    SYS-1002-ADRS       TO  HCM19-HOSPADRS
      *    医療機関名
           MOVE    SYS-1001-HOSPNAME   TO  HCM19-HOSPNAME
      *    医療機関ＴＥＬ
           MOVE    SYS-1002-TEL        TO  HCM19-HOSPTEL
      *
      *    ドクター
           MOVE    SYS-1010-NAME       TO  HCM19-DRNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    保険者情報
      *    保険者番号
           MOVE    WRK-HKNJA-G         TO  HCM19-HKNJA
      *
      *    記号・番号
      **** STRING  PTHKN-KIGO          DELIMITED   BY  SPACE
      *            "・"                DELIMITED   BY  SIZE
      *            PTHKN-NUM           DELIMITED   BY  SPACE
      *            INTO                    HCM19-KIGONUM
      *****END-STRING
      *    全角変換
      *    MOVE    PTHKN-KIGO          TO  WRK-MOJI
      *    PERFORM 2009-HENKAN-SEC
      *    MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
      *                                TO  WRK-KIGONUM
      *    MOVE    PTHKN-NUM           TO  WRK-MOJI
      *    PERFORM 2009-HENKAN-SEC
      *    MOVE    WRK-ZENKAKU-G(1:KANACHK-MAX)
      *                                TO  WRK-NUM
      *
      *    STRING  WRK-KIGONUM         DELIMITED   BY  SPACE
      *            "・"                DELIMITED   BY  SIZE
      *            WRK-NUM             DELIMITED   BY  SPACE
      *            INTO                    HCM19-KIGONUM
      *****END-STRING
           MOVE    WRK-HEN-KIGONUM     TO  HCM19-KIGONUM
      *
      *    本人家族区分
           EVALUATE    PTHKN-HONKZKKBN
               WHEN    "1"
                   MOVE    "被保険者"      TO  HCM19-HONKZKKBN
               WHEN    "2"
                   MOVE    "被扶養者"      TO  HCM19-HONKZKKBN
           END-EVALUATE
      *
      *    交付年月日
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM19-KOFYMD
      *
      *    公費負担者番号
           MOVE    WRK-FTNJA1          TO  HCM19-FTNJA
      *****MOVE    WRK-JKYSNUM1        TO  HCM19-JKYSNUM
      *    ７文字以上判定
           IF      WRK-JKYSNUM1(15:)   =   SPACE
               MOVE    WRK-JKYSNUM1        TO  HCM19-JKYSNUM
           ELSE
               MOVE    WRK-JKYSNUM1        TO  HCM19-JKYSNUMN
           END-IF
      *
           MOVE    WRK-FTNJA2          TO  HCM19-FTNJA2
      **** MOVE    WRK-JKYSNUM2        TO  HCM19-JKYSNUM2
      *    ７文字以上判定
           IF      WRK-JKYSNUM2(15:)   =   SPACE
               MOVE    WRK-JKYSNUM2        TO  HCM19-JKYSNUM2
           ELSE
               MOVE    WRK-JKYSNUM2        TO  HCM19-JKYSNUM2N
           END-IF
      *
      *    処方内容
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   20
               MOVE    WRK-NAIYOU (IDX IDX2)   TO  HCM19-NAIYO (IDX2)
           END-PERFORM
      *
      **** 免除編集
      *    IF     (WRK-MENJYO-FLG      =   1   )  OR
      *           (MENJYO-FLG          =   1   )
      *        MOVE    "（免）"            TO  HCM19-MENJYO
      **** END-IF
      *
      *備考欄
      *    ３歳未満・老人負担割合
           MOVE    ZERO                TO  IDX-B
           IF      WRK-BIKOU       NOT =   SPACE
               ADD     1               TO  IDX-B
               MOVE    WRK-BIKOU       TO  HCM19-BIKOH (IDX-B)
           END-IF
      *
      *    麻薬編集
           IF      FLG-MAYAKU          =   1
               PERFORM 31020-MAYAKU-HEN-SEC
           END-IF
      *
           .
       300-HCM19-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *        麻薬編集編集処理
      *****************************************************************
       31020-MAYAKU-HEN-SEC      SECTION.
      *
           PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL      (IDX-H   >   3   )  OR
                              (IDX-B   >=  9   )
               IF      WRK-HENSYU (IDX-H)  NOT =   SPACE
                   ADD     1                   TO  IDX-B
                   MOVE    WRK-HENSYU (IDX-H)  TO  HCM19-BIKOH (IDX-B)
               END-IF
           END-PERFORM
      *
           .
       31020-MAYAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *        麻薬編集編集処理
      *****************************************************************
       2003-MAYAKU-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-HENSYU-AREA
           MOVE    ZERO                TO  IDX-H
      *    患者住所
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                                       INTO    WRK-ADRS
           END-STRING
           ADD     1                   TO  IDX-H
           MOVE    "患者住所："        TO  WRK-HENSYU  (IDX-H)
           MOVE    WRK-ADRS            TO  WRK-HENSYU  (IDX-H)(11:76)
           IF      WRK-ADRS(77:)   NOT =   SPACE
               ADD     1                   TO  IDX-H
               MOVE    WRK-ADRS(77:)       TO  WRK-HENSYU (IDX-H)(11:)
           END-IF
      *
      *    麻薬施用者免許
           ADD     1                   TO  IDX-H
           STRING  "麻薬施用者免許証番号："
                                       DELIMITED   BY  SIZE
                   SYS-1010-MAYAKULICENSE
                                       DELIMITED   BY  SPACE
                                       INTO    WRK-HENSYU  (IDX-H)
           END-STRING
      *
           .
       2003-MAYAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       2002-SRYACT-SYORI-SEC      SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPID          TO  SRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-SRYKA           TO  SRY-SRYKA
           MOVE    WRK-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY2"         TO  ORC-DBPATH
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
           MOVE    ZERO                TO  FLG-ZAITAKU
           MOVE    ZERO                TO  FLG-ZAITAKU-ARI
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *
               MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
               MOVE    SRY-SRYKBN          TO  WRK-SRYKBN
      *
               MOVE    ZERO                TO  FLG-TAIGAI
      *
      *        在宅の時、薬剤の有無をチェックする
               IF     (SRY-SRYKBN          =   "14"  )  AND
                      (FLG-ZAITAKU         =   ZERO  )
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )   OR
                                      (SRY-SRYCD (IDXA)    =   SPACE)
                       IF     (SRY-SRYCD (IDXA)(1:1)       =   "6"  OR
                                                               "7" )  OR
                              (SRY-SRYCD (IDXA)(1:3)       =   "059" )
      ******************** MOVE    1               TO  FLG-ZAITAKU
                           MOVE    1               TO  FLG-ZAITAKU-ARI
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        在宅で薬剤・器材がある時、
               MOVE    ZERO                TO  FLG-ZAITAKU
               IF     (SRY-SRYKBN          =   "14" ) 
                   IF      ORCHC19-KBN         =   "3"
      *                院内
                       IF     (SRY-SRYSYUKBN       =   "148"  OR
                                                       "149" )  OR
                              (FLG-ZAITAKU-ARI     =   ZERO )
                           MOVE    1               TO  FLG-TAIGAI
                       ELSE
                           MOVE    1               TO  FLG-ZAITAKU
                       END-IF
                   ELSE
      *                院外
                       IF     (SRY-SRYSYUKBN       =   "148"  OR
                                                       "149" )
                           MOVE    1               TO  FLG-ZAITAKU
                       ELSE
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
                   END-IF
               END-IF
      *        処方のみで、院内処方印刷の時
               MOVE    ZERO                TO  FLG-INNAI
               IF     (SRY-SRYSYUKBN       =   "213"  OR
                                               "223"  OR
                                               "233")
                   IF      ORCHC19-KBN         =   "3"
                       MOVE    1               TO  FLG-INNAI
                   ELSE
                       MOVE    1               TO  FLG-TAIGAI
                   END-IF
               END-IF
      *
      *        薬剤、在宅薬剤を対象とする
               IF     (SRY-SRYKBN          =   "21"   OR  "22" OR "23"
                                           OR  "14"   )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-TAIGAI
               END-IF
      *
      *        在宅薬剤がないとき対象外
               IF     (SRY-SRYKBN          =   "14" )  AND
                      (SRY-SRYSYUKBN   NOT =   "148"  AND
                                               "149" ) AND
                      (FLG-ZAITAKU-ARI     =   ZERO)
                   MOVE    1               TO  FLG-TAIGAI
               END-IF
      *
               IF      FLG-TAIGAI          =   ZERO
      *
      *            診療会計より、点数チェック
                   PERFORM 20021-SRYACCT-SYORI-SEC
      *            点数がゼロのものを院外処方分とする
                   IF    ((ORCHC19-KBN     NOT =   "3"  ) AND
                          (ACCT-ZAITEN         =   ZERO ))  OR
                         ((ORCHC19-KBN         =   "3"  ) AND
                          (ACCT-ZAITEN     NOT =   ZERO ))  OR
                          (FLG-ZAITAKU         =   1    )  OR
                          (FLG-INNAI           =   1    )
                       PERFORM VARYING     IDXA    FROM    1   BY  1
                               UNTIL      (IDXA    >   5   )  OR
                                          (SRY-SRYCD (IDXA)   =   SPACE)
                         IF      SRY-SRYKBN        =   "14"
                           IF      SRY-SRYCD (IDXA)(1:1)   NOT =   "1"
                               MOVE    SRY-SRYCD (IDXA)    TO  WRK-SRYCD
                               PERFORM 3103-SRYACT-HEN-SEC
                           END-IF
                         ELSE
                           IF     (SRY-SRYCD (IDXA)(1:1)   =   "6"  OR
                                                               "0"  OR
                                                               "8") AND
                                  (SRY-SRYCD (IDXA)    NOT =
                                                           "630010002")
                               MOVE    SRY-SRYCD (IDXA)    TO  WRK-SRYCD
                               PERFORM 3103-SRYACT-HEN-SEC
                           END-IF
                         END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
      *        処方箋料がある時、印刷をおこなう
               IF      SRY-SRYSYUKBN       =   "820"
                   MOVE    1                   TO  FLG-SYOHOU
               END-IF
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    剤ごとに、編集する
           IF      IDX-S               =   ZERO
               GO      TO      2002-SRYACT-SYORI-EXT
           END-IF
      *
      *    最後の行に回数を編集
           IF      WRK-SRYCD (1:1)     =   "6"
               ADD     1                   TO  IDX-S
           END-IF
      *
      *    最後の行に回数を編集
           IF      ORCHC19-KBN         =   "1"
      *        回数編集なし
               CONTINUE
           ELSE
      *        診療会計より回数編集
               EVALUATE    WRK-SRYKBN
                   WHEN    "21"
                   WHEN    "22"
                       PERFORM 20022-KAISU-HEN-SEC
               END-EVALUATE
           END-IF
      *
           EVALUATE    WRK-SRYKBN
               WHEN    "21"
                   MOVE    "（"                  TO  WRK-K3      (IDX-S)
                   MOVE    "日分）"              TO  WRK-HINICHI (IDX-S)
               WHEN    "22"
                   MOVE    "（"                  TO  WRK-K3      (IDX-S)
                   MOVE    "回分）"              TO  WRK-HINICHI (IDX-S)
      *---(01.00.01) LINE ADD START ------------------------------------
               WHEN    "23"
               WHEN    "14"
                   IF      WRK-NAIYOU-Z (IDX-S)  =   SPACE
                       COMPUTE IDX-S             =   IDX-S    -   1
                   END-IF
      *---(01.00.01) LINE ADD END   ------------------------------------
           END-EVALUATE
      *
           INSPECT WRK-NAIYOU-Z (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
           INSPECT WRK-NAIYOU-Z (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-KAISUU   (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-FIL21    (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-FIL22    (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-K3       (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-MEISYOU2 (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
      *    帳票用編集処理
           PERFORM 20022-PRINT-HEN-SEC
      *
           .
       2002-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計より回数編集処理
      *****************************************************************
       20021-SRYACCT-SYORI-SEC         SECTION.
      *
      *    診療会計マスター読み込み
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-ACT
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  WRK-KAISU
      *
           .
       20021-SRYACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計より回数編集処理
      *****************************************************************
       20022-KAISU-HEN-SEC         SECTION.
      *
           MOVE    WRK-KAISU           TO  WRK-ZZZ
           MOVE    3                   TO  WRK-MOJISU
           MOVE    WRK-ZZZ             TO  WRK-MOJI
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:6)  TO  WRK-KAISUU (IDX-S)
           INSPECT WRK-KAISUU (IDX-S)  REPLACING  ALL "  "  BY  "　"
      *
           .
       20022-KAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票用編集処理
      *****************************************************************
       20022-PRINT-HEN-SEC         SECTION.
      *
      *    最大行数のセット
           MOVE    19                  TO  IDX-MAX
      *
           IF     (IDX-S   +   IDX)    >   IDX-MAX
               MOVE    "次ページへ続く"    TO  WRK-NAIYOU
                                                 (WRK-PAGE-MAX 20)(50:)
               IF      IDX             >   ZERO
      *            改ページ
                   ADD     1                   TO  WRK-PAGE-MAX
                   MOVE    ZERO                TO  IDX
               END-IF
           END-IF
      *
           PERFORM VARYING IDXA2       FROM    1   BY  1
                   UNTIL   IDXA2       >   IDX-S
               ADD     1                   TO  IDX
               MOVE    WRK-NAIYOU-Z (IDXA2) TO  WRK-NAIYOU
                                                    (WRK-PAGE-MAX IDX)
           END-PERFORM
           ADD     1                   TO  IDX
      *
           .
       20022-PRINT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       3103-SRYACT-HEN-SEC         SECTION.
      *
           ADD     1               TO  IDX-S
      *
      *    １剤で２０行以上の時
           IF      IDX-S               >=  20
               MOVE    19              TO  IDX-S
      *        帳票編集処理
               PERFORM 20022-PRINT-HEN-SEC
               MOVE    SPACE               TO  WRK-NAIYOU-AREA
               MOVE    1                   TO  IDX-S
               MOVE    1                   TO  FLG-KAISU 
           END-IF
      *
           IF     (IDX-S           =   1   )  AND
                  (FLG-KAISU       =   ZERO)
               ADD     1               TO  IDX-Z
               MOVE    IDX-Z           TO  WRK-ZZ
               MOVE    2               TO  WRK-MOJISU
               MOVE    WRK-ZZ          TO  WRK-MOJI
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:4)
                                       TO  WRK-NO     (IDX-S)
               MOVE    "）"            TO  WRK-K2     (IDX-S)
           ELSE
               MOVE    ALL "　"        TO  WRK-NO     (IDX-S)
               MOVE    ALL "　"        TO  WRK-K2     (IDX-S)
           END-IF
      *
      *    点数検索
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    内容
           IF      SRY-SRYCD (IDXA)(1:1)   =   "6"
      *        ＧＥ付き表示の時、表示
               MOVE    TNS-NAME        TO  WRK-TNS-NAME
               IF      SYS-1030-SYOHOGEPRTFLG  =   1
                   IF     (TNS-SYUSAISKB       =   8   )  AND
                          (TNS-SYOMEIKANREN    NOT =   SPACE AND
                                                       ZERO )
                       COMPUTE KETA            =  (TNS-YUKOKETA  * 2)
                                               +   1
                       MOVE    "（ＧＥ）"      TO  WRK-TNS-NAME(KETA:)
                   END-IF
               END-IF
      *        後発医薬品の時、表示（優先）
               IF      SYS-1030-SYOHODSPNAME   NOT =   SPACE
      *            後発医薬品区分
                   IF      TNS-KOUHATUKBN      NOT =   ZERO
                       MOVE    TNS-NAME        TO  WRK-TNS-NAME
                       COMPUTE KETA            =  (TNS-YUKOKETA  * 2)
                                               +   1
                       MOVE    SYS-1030-SYOHODSPNAME
                                               TO  WRK-TNS-NAME(KETA:)
                   END-IF
               END-IF
      *        名称編集
               PERFORM 31031-MEISYOU-HEN-SEC
      *        麻薬判定
               IF      TNS-MADOKUKBN       =   1
                   MOVE   1                TO  FLG-MAYAKU
               END-IF
           ELSE
      *        用法
               IF      SRY-SRYCD (IDXA)(1:3)   =   "001"
                   MOVE    SPACE           TO  WRK-TNS-NAME
                   STRING     "【"             DELIMITED   BY  SIZE
                              TNS-NAME         DELIMITED   BY  "    "
                              "】"             DELIMITED   BY  SIZE
      ********************INTO                 WRK-MEISYOU1 (IDX-S)
                          INTO                 WRK-TNS-NAME
                   END-STRING
               ELSE
      *            その他
                   IF      SRY-INPUTNUM (IDXA)     =   ZERO
                       MOVE    TNS-NAME        TO  WRK-TNS-NAME
                   ELSE
                       PERFORM 31032-PTCOM-SET-SEC
                   END-IF
               END-IF
      *        名称編集
               PERFORM 31031-MEISYOU-HEN-SEC
      *********END-IF
           END-IF
      *
           INSPECT WRK-MEISYOU1 (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "6"  OR  "7" )  OR
                  (SRY-SRYCD (IDXA)(1:3)   =   "059"        )
      *        数量
               MOVE    SRY-SRYSURYO (IDXA)     TO  WRK-SURYO
               PERFORM 10011-SURYO-HEN-SEC
               MOVE    WRK-SURYO-N             TO  WRK-SURYOU (IDX-S)
      *        数量単位
               MOVE    TNS-TANINAME            TO  WRK-TANI   (IDX-S)
      *D   ELSE
      *D       MOVE    ALL "　"                TO  WRK-SURYOU (IDX-S)
      *D       MOVE    ALL "　"                TO  WRK-TANI   (IDX-S)
           END-IF
      *
           INSPECT WRK-SURYOU (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-TANI   (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
      *    免除の判定（在宅薬剤で注射以外のもの）
           IF      FLG-ZAITAKU         =   1
               IF     (TNS-SRYCD(1:1)      =   "6"  )  AND
                      (TNS-YKZKBN      NOT =   4    )
                   MOVE    1               TO  WRK-MENJYO-FLG
               END-IF
           END-IF
      *
           .
       3103-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     名称編集処理
      *****************************************************************
       31031-MEISYOU-HEN-SEC           SECTION.
      *
           MOVE    WRK-TNS-NAME        TO  WRK-MEISYOU1   (IDX-S)
           IF      WRK-TNS-NAME(45:)   NOT =   SPACE
               MOVE    WRK-TNS-NAME        TO  WRK-MEISYOU3   (IDX-S)
               IF    ((SRY-SRYCD (IDXA)(1:1)   =   "6"  OR  "7" )  OR
                      (SRY-SRYCD (IDXA)(1:3)   =   "059"  ))
                 AND  (WRK-TNS-NAME(81:)       =   SPACE   )
                   ADD     1                   TO  IDX-S
                   MOVE    ALL "　"            TO  WRK-NO     (IDX-S)
                   MOVE    ALL "　"            TO  WRK-K2     (IDX-S)
               END-IF
           END-IF
           IF      WRK-TNS-NAME(81:)   NOT =   SPACE
               ADD     1                   TO  IDX-S
               MOVE    WRK-TNS-NAME(81:)   TO  WRK-MEISYOU3 (IDX-S)
               IF     (SRY-SRYCD (IDXA)(1:1)   =   "6"  OR  "7" )  OR
                      (SRY-SRYCD (IDXA)(1:3)   =   "059"  )
                   ADD     1                   TO  IDX-S
                   MOVE    ALL "　"            TO  WRK-NO     (IDX-S)
                   MOVE    ALL "　"            TO  WRK-K2     (IDX-S)
               END-IF
           END-IF
      *
           .
       31031-MEISYOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     数量単位編集処理
      *****************************************************************
       31031-TANI-HENSYU-SEC            SECTION.
      *
           MOVE    TNS-TANINAME(1:2)     TO  WRK-TANINAME
      *
           EVALUATE    TNS-TANICD
      *            セット
               WHEN    009
                   MOVE    "Set"               TO  WRK-TANINAME
      *            トローチ
               WHEN    013
                   MOVE    TNS-TANINAME(1:2) TO  WRK-TANINAME
      *            アンプル
               WHEN    014
                   MOVE    "A"                 TO  WRK-TANINAME
      *            カプセル
               WHEN    015
                   MOVE    "Cap"               TO  WRK-TANINAME
      *            バイアル
               WHEN    039
                   MOVE    "B"                 TO  WRK-TANINAME
      *            キット
               WHEN    051
                   MOVE    "Kit"               TO  WRK-TANINAME
      *            フィート
               WHEN    030
                   MOVE    "FT"                TO  WRK-TANINAME
      *            ｍｇ
               WHEN    032
                   MOVE    "mg"                TO  WRK-TANINAME
               WHEN    034
                   MOVE    "Kg"                TO  WRK-TANINAME
               WHEN    035
                   MOVE    "cc"                TO  WRK-TANINAME
               WHEN    036
                   MOVE    "mL"                TO  WRK-TANINAME
               WHEN    038
                   MOVE    "mLV"                TO  WRK-TANINAME
               WHEN    040
                   MOVE    "cm"                TO  WRK-TANINAME
               WHEN    041
                   MOVE    "cm2"               TO  WRK-TANINAME
               WHEN    042
                   MOVE    "m"                 TO  WRK-TANINAME
               WHEN    043
                   MOVE    "uCi"               TO  WRK-TANINAME
               WHEN    044
                   MOVE    "mCi"               TO  WRK-TANINAME
               WHEN    045
                   MOVE    "ug"                TO  WRK-TANINAME
               WHEN    048
                   MOVE    "GBq"                TO  WRK-TANINAME
               WHEN    049
                   MOVE    "MBq"                TO  WRK-TANINAME
               WHEN    050
                   MOVE    "KBq"                TO  WRK-TANINAME
               WHEN    058
                   MOVE    "mLg"                TO  WRK-TANINAME
           END-EVALUATE
           .
       41011231-TANI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       10011-SURYO-HEN-SEC                  SECTION.
      *
           IF      WRK-SURYO           =   ZERO
               MOVE    SPACE               TO  WRK-SURYO-N
               GO      TO      10011-SURYO-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM   FROM    3  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1) TO  WRK-SURYO-Z3(IDXM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
           MOVE    WRK-SURYO-X         TO  WRK-MOJI
           MOVE    9                   TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:18) TO  WRK-SURYO-N
      *
           .
       10011-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       31032-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPID          TO  PTCOM-HOSPID
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDXA)    TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDXA)  TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WRK-TNS-NAME
           END-IF
           .
       31032-PTCOM-SET-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-MAX         >   ZERO )
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ZENKAKU-G
           ELSE
               MOVE    WRK-MOJI            TO  WRK-ZENKAKU-G
           END-IF
      *
      *
      *****MOVE    ALL "　"            TO  WRK-ZENKAKU-G
      *
      *    PERFORM VARYING     IDK     FROM    1   BY  1
      *            UNTIL       IDK     >   WRK-MOJISU
      *        IF      WRK-MOJI (IDK:1)    NUMERIC
      *            MOVE    WRK-MOJI(IDK:1)     TO  IDX2-1
      *            IF      IDX2-1              =   ZERO
      *                MOVE    10                  TO  IDX2
      *            ELSE
      *                MOVE    IDX2-1              TO  IDX2
      *            END-IF
      *            MOVE    TBL-SUJI (IDX2)     TO  WRK-ZENKAKU(IDK)
      *        ELSE
      *            IF      WRK-MOJI (IDK:1)    =   "."
      *                MOVE    "．"            TO  WRK-ZENKAKU(IDK)
      *            END-IF
      *        END-IF
      *****END-PERFORM
           .
       2009-HENKAN-EXT.
           EXIT.
      *---(01.00.07) LINE ADD START ------------------------------------
      *****************************************************************
      *    中途終了時の発行処理
      *****************************************************************
       2003-WKSRYACT-SYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPID          TO  WKSRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "WKSRYACT-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "WKSRYACT-KEY2"   TO  ORC-DBPATH
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYOHOU
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
           MOVE    ZERO                TO  FLG-ZAITAKU
           MOVE    ZERO                TO  FLG-OK2
           MOVE    ZERO                TO  WRK-YKZTEN
           MOVE    ZERO                TO  WRK-KIZAITEN
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               IF      WKSRY-RENNUM        =   1
      *            剤終了時、編集へ
                   IF      IDX-S           >   ZERO
                       PERFORM 20032-PRINT-HENSYU-SEC
                   END-IF
      *            回数
                   MOVE    WKSRY-ZAIKAIKEI     TO  WRK-KAISU
                   MOVE    WKSRY-SRYSYUKBN     TO  WRK-SRYSYUKBN
                   MOVE    WKSRY-SRYKBN        TO  WRK-SRYKBN
                   MOVE    ZERO                TO  FLG-OK
                   MOVE    ZERO                TO  FLG-OK2
                   MOVE    ZERO                TO  IDX-S
               END-IF
      *
      *******  投薬で、点数がゼロのもを対象（院外）
      *        IF      ORCHC19-KBN         =   "4" 
      *            IF    ((WKSRY-SRYKBN        =   "21"  OR
      *                                            "22"  OR
      *                                            "23"    )  AND
      *                   (WKSRY-ZAITENKEI     =   ZERO    ))  OR
      *                  ((WKSRY-SRYKBN        =   "14"    )  AND
      *                   (WKSRY-YKZTEN        >   ZERO    ))
      *                MOVE    1               TO  FLG-OK
      *            END-IF
      *        END-IF
      *        投薬で、点数がゼロのもを対象（院内）
      *        IF      ORCHC19-KBN         =   "5"
      *            IF    ((WKSRY-SRYKBN        =   "21"  OR
      *                                            "22"  OR
      *                                            "23"    )  AND
      *                   (WKSRY-ZAITENKEI     >   ZERO    ))  OR
      *                  ((WKSRY-SRYKBN        =   "14"    )  AND
      *                   (WKSRY-YKZTEN        >   ZERO    ))
      *                MOVE    1               TO  FLG-OK
      *            END-IF
      *********END-IF
      *        在宅自己注射はすべてで対象とする
               IF     (WKSRY-SRYKBN        =   "14"    )
                   MOVE    WKSRY-YKZTEN        TO  WRK-YKZTEN
                   MOVE    WKSRY-KIZAITEN      TO  WRK-KIZAITEN
                   MOVE    1               TO  FLG-OK
      ************ MOVE    2               TO  FLG-OK2
               END-IF
      *        投薬対象
               IF      WKSRY-SRYKBN        =   "21"  OR
                                               "22"  OR
                                               "23"
                   MOVE    WKSRY-ZAITENKEI     TO  WRK-ZAITENKEI
                   MOVE    1                   TO  FLG-OK
               END-IF
      *
               IF      FLG-OK              =   1
                   PERFORM 20031-WKSRYACT-HEN-SEC
               END-IF
      *
               MOVE    "WKSRYACT-KEY2"     TO  ORC-DBPATH
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
      *
      *    剤終了時、編集へ
           IF      IDX-S           >   ZERO
               PERFORM 20032-PRINT-HENSYU-SEC
           END-IF
      *
      *    対象明細がない時、出力しない
           IF      IDX                 =   ZERO
               MOVE    ZERO                TO  WRK-PAGE-MAX
               MOVE    ZERO                TO  FLG-PRINT
           ELSE
      *        最終行のメッセージ編集
               ADD     1               TO  IDX
               IF      IDX             <=  20
                   MOVE    "　　　　　　以下余白"
                                       TO  WRK-NAIYOU (WRK-PAGE-MAX IDX)
               END-IF
               MOVE    1               TO  FLG-PRINT
           END-IF
      *    麻薬編集
           IF      FLG-MAYAKU          =   1
               PERFORM 2003-MAYAKU-HEN-SEC
           END-IF
      *
           .
       2003-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為内容を診療行為へ
      *****************************************************************
       20031-WKSRYACT-HEN-SEC         SECTION.
      *
           INITIALIZE                  SRYACT-REC
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   5   )  OR
                              (WKSRY-SRYCD (IDXA) =   SPACE)
               IF    ((WKSRY-SRYCD (IDXA)(1:1) =   "6"  OR
                                                   "7"  OR
                                                   "0"  OR
                                                   "8") OR
                      (WKSRY-SRYCD (IDXA)(1:3) =   "059"))   AND
                      (WKSRY-SRYCD (IDXA)  NOT =  "630010002")
                   MOVE    WKSRY-SRYCD (IDXA)  TO  SRY-SRYCD (IDXA)
      *            数量
                   MOVE    WKSRY-SRYSURYO (IDXA)
                                               TO  SRY-SRYSURYO (IDXA)
      *            回数
                   MOVE    WKSRY-SRYKAISU (IDXA)
                                               TO  SRY-SRYKAISU (IDXA)
                   MOVE    WKSRY-SRYCD (IDXA)  TO  WRK-SRYCD
      *            内容編集
                   PERFORM 3103-SRYACT-HEN-SEC
      *
                   IF     (WKSRY-SRYCD (IDXA)(1:1) =    "8") AND
                          (WKSRY-INPUTCOMENT (IDXA)
                                               NOT =   SPACE)
                       MOVE    WKSRY-INPUTCOMENT (IDXA)
                                               TO  WRK-TNS-NAME
      *                名称編集
                       PERFORM 31031-MEISYOU-HEN-SEC
                       INSPECT WRK-MEISYOU1 (IDX-S)
                                       REPLACING  ALL "  "  BY  "　"
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       20031-WKSRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    中途終了剤毎の編集処理
      *****************************************************************
       20032-PRINT-HENSYU-SEC         SECTION.
      *
      *    投薬で、点数がゼロのもを対象（院外）
           IF      ORCHC19-KBN         =   "4" 
               IF     (WRK-SRYKBN          =   "21"  OR
                                               "22"  OR
                                               "23"    )  AND
                      (WRK-ZAITENKEI        =   ZERO   )
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-IF
      *    投薬で、点数がゼロのもを対象（院内）
           IF      ORCHC19-KBN         =   "5"
               IF     (WRK-SRYKBN          =   "21"  OR
                                               "22"  OR
                                               "23"    )  AND
                      (WRK-ZAITENKEI       >   ZERO    )
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-IF
      *
      *    在宅で薬剤・器材がある時、
           IF      WRK-SRYKBN          =   "14"
               IF     (WRK-SRYSYUKBN       =   "148"  OR
                                               "149" )  AND
                      (ORCHC19-KBN         =   "4"   )
                   MOVE    1               TO  FLG-OK2
               END-IF
               IF     (WRK-SRYSYUKBN   NOT =   "148"  AND
                                               "149" )  AND
                      (ORCHC19-KBN         =   "5"  )
      *            在宅で薬剤・器材がある時、
                   IF     (WRK-YKZTEN      >   ZERO )  OR
                          (WRK-KIZAITEN    >   ZERO )
                       MOVE    1               TO  FLG-OK2
                   END-IF
               END-IF
           END-IF
      *        処方のみで、院内処方印刷の時
           IF      WRK-SRYKBN          =   "21"  OR  "22" OR "23"
               IF     (WRK-SRYSYUKBN       =   "213"  OR
                                               "223"  OR
                                               "233")   AND
                      (ORCHC19-KBN         =   "5"   )
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-IF
      *
           IF      FLG-OK2             =   ZERO
               IF      IDX-Z               >   ZERO
                   COMPUTE IDX-Z           =   IDX-Z     -   1
               END-IF
               MOVE    SPACE               TO  WRK-NAIYOU-AREA
               MOVE    SPACE               TO  WRK-SRYCD
               MOVE    ZERO                TO  FLG-KAISU
               MOVE    ZERO                TO  FLG-ZAITAKU
               MOVE    ZERO                TO  WRK-YKZTEN
               MOVE    ZERO                TO  WRK-KIZAITEN
               GO      TO       20032-PRINT-HENSYU-EXT
           END-IF
      *
      *    最後の行に回数を編集
           IF      WRK-SRYCD (1:1)     =   "6"
               ADD     1                   TO  IDX-S
           END-IF
      *
      *    最後の行に回数を編集
           EVALUATE    WRK-SRYKBN
               WHEN    "21"
               WHEN    "22"
                   PERFORM 20022-KAISU-HEN-SEC
           END-EVALUATE
      *
           EVALUATE    WRK-SRYKBN
               WHEN    "21"
                   MOVE    "（"                  TO  WRK-K3      (IDX-S)
                   MOVE    "日分）"              TO  WRK-HINICHI (IDX-S)
               WHEN    "22"
                   MOVE    "（"                  TO  WRK-K3      (IDX-S)
                   MOVE    "回分）"              TO  WRK-HINICHI (IDX-S)
               WHEN    "23"
               WHEN    "14"
                   IF      WRK-NAIYOU-Z (IDX-S)  =   SPACE
                       COMPUTE IDX-S             =   IDX-S    -   1
                   END-IF
           END-EVALUATE
      *
           INSPECT WRK-NAIYOU-Z (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
      *    帳票用編集処理
           PERFORM 20022-PRINT-HEN-SEC
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
           MOVE    ZERO                TO  FLG-ZAITAKU
           MOVE    ZERO                TO  WRK-YKZTEN
           MOVE    ZERO                TO  WRK-KIZAITEN
      *
           .
       20032-PRINT-HENSYU-EXT.
           EXIT.
      *---(01.00.07) LINE ADD END   ------------------------------------
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
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
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
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
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
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
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
     *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
     *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読み込み
      *****************************************************************
       930-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
     *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       930-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
     *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
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
       900-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ順読み
      *****************************************************************
       975-SRYKARRK-READ-SEC         SECTION.
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
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *---(01.00.07) LINE ADD START ------------------------------------
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *
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
      *---(01.00.07) LINE ADD END   ------------------------------------
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
