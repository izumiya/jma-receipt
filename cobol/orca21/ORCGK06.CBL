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
       PROGRAM-ID.             ORCGK06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 算定履歴照会（Ｋ０６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01.04.18 戻る時、確認画面を表示する等
      *  01.00.02    MCC−多々納  02.05.20 保険組合せ追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
       01  SPA-K06.
           05  SPA-K06-AREA.
               07  SPA-GMN-MAX         PIC 9(04).
               07  SPA-GMN-PAGE        PIC 9(02).
               07  SPA-GMN-CUR         PIC 9(02).
               07  SPA-GMN-AREA.
                   09  SPA-GMN-PTNUM       PIC X(20).
                   09  SPA-GMN-KANANAME    PIC X(30).
                   09  SPA-GMN-NAME        PIC X(30).
                   09  SPA-GMN-SEX         PIC X(02).
                   09  SPA-GMN-BIRTHDAY    PIC X(09).
                   09  SPA-GMN-SRYYM       PIC X(07).
                   09  SPA-GMN-TBL.
                       11  SPA-GMN-TBLO    OCCURS  100.
                           13  SPA-GMN-TRENNUM     PIC 9(03).
                           13  SPA-GMN-TSRYCD      PIC X(09).
                           13  SPA-GMN-TNYUGAIKBN  PIC X(03).
                           13  SPA-GMN-TSRYKA      PIC X(11).
                           13  SPA-GMN-THKNCOMBI   PIC X(21).
                           13  SPA-GMN-TMEISYO     PIC X(71).
                           13  SPA-GMN-TFSANTEIYMD PIC X(09).
                           13  SPA-GMN-TMSANTEID   PIC 9(02).
                           13  SPA-GMN-TMSANTEICNT PIC 9(03).
                   09  SPA-GMN-KOUMOKU.
                       11  SPA-GMN-RENNUM          PIC 9(03).
                       11  SPA-GMN-SRYCD           PIC X(09).
                       11  SPA-GMN-NYUGAIKBN       PIC 9(01).
                       11  SPA-GMN-SRYKA           PIC 9(02).
                       11  SPA-GMN-SRYKAH          PIC X(10). 
                       11  SPA-GMN-HKNCOMBI        PIC 9(04).
                       11  SPA-GMN-HKNCOMBIMEI     PIC X(21). 
                       11  SPA-GMN-MEISYO          PIC X(70).
                       11  SPA-GMN-FSANTEIYMD      PIC X(10).
                       11  SPA-GMN-MSANTEID        PIC 9(02).
                       11  SPA-GMN-MSANTEICNT      PIC 9(03).
                       11  SPA-GMN-DAY-G.
                           13  SPA-GMN-DAY         OCCURS  31
                                                   PIC 9(02).
      *
                   07  SPA-NAIB-AREA.
                       09  SPA-NAI-SRYYM           PIC X(06).
                       09  SPA-NAI-KOUMOKU.
                           11  SPA-NAI-FSANTEIYMD  PIC X(08).
                           11  SPA-NAI-SANTEIRRKKBN
                                                   PIC 9(01).
                           11  SPA-NAI-JGNCNT      PIC 9(03).
                           11  SPA-NAI-JGNCNT1D    PIC 9(03).
      * 
                       09  SPA-NAI-TBL.
                           11  SPA-NAI-TBLO        OCCURS  100.
                               13  SPA-NAI-TNYUGAIKBN  PIC 9(01).
                               13  SPA-NAI-TSRYKA      PIC 9(02).
                               13  SPA-NAI-THKNCOMBI   PIC 9(04).
                               13  SPA-NAI-TFSANTEIYMD PIC X(08).
                               13  SPA-NAI-TDAY-G      OCCURS  31.
                                   15  SPA-NAI-TDAY    PIC 9(02).
                               13  SPA-NAI-TUPDFLG     PIC 9(01).
      *
                      09  SPA-DEL-TBL.
                          11  SPA-DEL-TBLO         OCCURS  100.
                              13  SPA-DEL-TSRYCD       PIC X(09).
                              13  SPA-DEL-TNYUGAIKBN   PIC 9(01).
                              13  SPA-DEL-TSRYKA       PIC 9(02).
                              13  SPA-DEL-THKNCOMBI    PIC 9(04).
                      09  SPA-DEL-MAX              PIC 9(03). 
      *----(01.00.01) LINE ADD START ----------------------------------
      *            変更確認フラグ
               07  SPA-UPDFLG              PIC X(01).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *----(01.00.01) LINE ADD START ----------------------------------
           03  IDY-CHK             PIC 9(04).
      *----(01.00.01) LINE ADD END   ----------------------------------
           03  IDXS                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(03).
               05  WRK-WIDGET2     PIC 9(02).  
           03  WRK-YMD             PIC X(09).
           03  WRK-YMDMAX          PIC 9(01). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    保険適用期間
           03  WRK-ST-SRYYMD.
               05  WRK-STYY            PIC 9(04).
               05  WRK-STMM            PIC 9(02).
               05  WRK-STDD            PIC 9(02).
           03  WRK-ED-SRYYMD.
               05  WRK-EDYY            PIC 9(04).
               05  WRK-EDMM            PIC 9(02).
               05  WRK-EDDD            PIC 9(02).
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           03  WRK-KIDMSG          PIC X(80).
      *----(01.00.01) LINE ADD END   ----------------------------------
           03  WRK-DAY             PIC 9(02).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *入力タイプ
           03  WRK-INTYPE             PIC X(01).
      *
           03  WRK-ZZ                 PIC ZZ9.
      *
           03  WRK-SRYYM.
               05  WRK-SRY-YY         PIC 9(04).
               05  WRK-SRY-MM         PIC 9(02).
      *
           03  WRK-RENNUM             PIC 9(03).
           03  WRK-RENNUM2            PIC 9(03).
      *    保険組合せ名称
           03  WRK-SRYCD              PIC X(09).
      *    保険組合せ名称
           03  WRK-HKNCOMBI         PIC X(04).
           03  WRK-HKNCOMBIMEI      PIC X(100).
           03  WRK-HKNKBN-MEI       PIC X(02).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    画面用ワーク
       01  WRK-GMN-SORT.
           03  WRK-GMN-TBL.
               05  WRK-GMN-TBLO    OCCURS  100.
                   07  WRK-GMN-TRENNUM     PIC 9(03).
                   07  WRK-GMN-TSRYCD      PIC X(09).
                   07  WRK-GMN-TNYUGAIKBN  PIC X(03).
                   07  WRK-GMN-TSRYKA      PIC X(11).
                   07  WRK-GMN-THKNCOMBI   PIC X(21).
                   07  WRK-GMN-TMEISYO     PIC X(71).
                   07  WRK-GMN-TFSANTEIYMD PIC X(09).
                   07  WRK-GMN-TMSANTEID   PIC 9(02).
                   07  WRK-GMN-TMSANTEICNT PIC 9(03).
      * 
           03  WRK-NAI-TBL.
               05  WRK-NAI-TBLO        OCCURS  100.
                   07  WRK-NAI-TNYUGAIKBN  PIC 9(01).
                   07  WRK-NAI-TSRYKA      PIC 9(02).
                   07  WRK-NAI-THKNCOMBI   PIC 9(04).
                   07  WRK-NAI-TFSANTEIYMD PIC X(08).
                   07  WRK-NAI-TDAY-G      OCCURS  31.
                       09  WRK-NAI-TDAY    PIC 9(02).
                   07  WRK-NAI-TUPDFLG     PIC 9(01).
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *
      *   診療科履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "K01.INC".
           COPY    "K02.INC".
           COPY    "K02N.INC".
           COPY    "K021.INC".
           COPY    "K022.INC".
           COPY    "K023.INC".
           COPY    "K03.INC".
           COPY    "K04.INC".
           COPY    "K05.INC".
           COPY    "K051.INC".
           COPY    "K052.INC".
           COPY    "K06.INC".
           COPY    "K07.INC".
           COPY    "K08.INC".
           COPY    "K09.INC".
           COPY    "K10.INC".
           COPY    "K98.INC".
           COPY    "K99.INC".
           COPY    "K03X.INC".
           COPY    "KERR.INC".
           COPY    "KERR2.INC".
           COPY    "KID1.INC".
           COPY    "KID2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K06
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K06         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "K06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *----(01.00.01) LINE ADD START ----------------------------------
      *    確認画面より
           IF      SPA-MOTOPG          =   "KID1"
               IF      SPA-KID1-FLG        =   "OK"
                   MOVE    SPACE               TO  SPA-UPDFLG
                   MOVE    1                   TO  FLG-SYORI
                   PERFORM 210-BACK
                   GO      TO      300-SCREEN-EXT
               ELSE
                   MOVE    1               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-KIDCD
                   GO      TO      300-SCREEN-EXT
               END-IF
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           INITIALIZE                  SPA-K06-AREA
      *
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-SRYYMDW (1:6)   TO  SPA-GMN-SRYYM
           MOVE    SPA-SRYYMD  (1:6)   TO  SPA-NAI-SRYYM
      *
      *
           PERFORM 3101-SANTEI-SPA-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴画面編集処理
      *****************************************************************
       3101-SANTEI-SPA-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
                                           SPA-DEL-MAX
           INITIALIZE                      SPA-GMN-TBL
                                           SPA-NAI-TBL
                                           SPA-DEL-TBL
      *  
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SANTEI-KEY2"       TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   100  )  OR
                               (FLG-SANTEI     =   1    )
      *
               MOVE    IDX             TO  SPA-GMN-MAX
      *
               MOVE    IDX               TO  SPA-GMN-TRENNUM (IDX)
               MOVE    SANTEI-SRYCD      TO  SPA-GMN-TSRYCD  (IDX)
               INITIALIZE                    TNS-TENSU-REC
               MOVE    SPA-GMN-TSRYCD (IDX)
                                         TO  TNS-SRYCD
               PERFORM 900-TENSU-READ-SEC
               IF      FLG-TENSU         =   ZERO
                   MOVE    TNS-NAME          TO  SPA-GMN-TMEISYO (IDX)
               END-IF
               MOVE    SANTEI-NYUGAIKBN      TO  SPA-NAI-TNYUGAIKBN
                                                                 (IDX)
               EVALUATE    SANTEI-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  SPA-GMN-TNYUGAIKBN(IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  SPA-GMN-TNYUGAIKBN(IDX)
               END-EVALUATE 
               MOVE    SANTEI-SRYKA     TO  SPA-NAI-TSRYKA  (IDX)
               INITIALIZE                   SYS-1005-REC
               MOVE    "1005"           TO  SYS-1005-KANRICD
               MOVE    SANTEI-SRYKA     TO  SYS-1005-KBNCD
               MOVE    SPA-NAI-SRYYM    TO  ORC-DBYMD (1:6)
               MOVE    "01"             TO  ORC-DBYMD (7:2)
               MOVE    SYS-1005-REC     TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC          TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1  TO  SPA-GMN-TSRYKA(IDX)
               END-IF
               MOVE    SANTEI-MSANTEICNT   TO  SPA-GMN-TMSANTEICNT(IDX)
               MOVE    SANTEI-MSANTEID     TO  SPA-GMN-TMSANTEID  (IDX)
               MOVE    SANTEI-FSANTEIYMD   TO  SPA-NAI-TFSANTEIYMD(IDX)
               IF      SANTEI-FSANTEIYMD   NOT =   SPACE
                   MOVE    SANTEI-FSANTEIYMD   TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-TFSANTEIYMD
                                                                  (IDX)
               END-IF
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       31
                   MOVE    SANTEI-DAY (IDY)        TO  SPA-NAI-TDAY
                                                             (IDX IDY)
               END-PERFORM
      *        保険組合せ
               MOVE    SANTEI-HKNCOMBINUM      TO  SPA-NAI-THKNCOMBI
                                                                 (IDX)
               IF      SANTEI-HKNCOMBINUM  NOT =   ZERO
                   MOVE    SANTEI-HKNCOMBINUM  TO  WRK-HKNCOMBI
                   PERFORM 31011-HKNCOMBI-SET-SEC
                   MOVE    WRK-HKNCOMBIMEI     TO  SPA-GMN-THKNCOMBI
                                                                 (IDX)
               END-IF
      *
               MOVE    "SANTEI-KEY2"           TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SANTEI-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    1                   TO  SPA-GMN-CUR 
           .
       3101-SANTEI-SPA-EXT.
           EXIT.
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       31011-HKNCOMBI-SET-SEC          SECTION.
      *
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
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
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *    労災の時、コメントの追加
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
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
           END-IF
      *
           .
       31011-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    ZERO                TO  FLG-SYORI
                   PERFORM 210-BACK
      *     追加
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-TUIKA-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 440-SAKUJYO-SEC
      *    変更
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 430-HENKOU-SEC
      *    前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 430-MAEGMN-SEC
      *    次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *    前月
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-MAETUKI-SEC
      *    次月
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-ATOTUKI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-RENNUM-HEN-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 41012-SRYYM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYCD"
                   PERFORM 4102-SRYCD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 4107-NYUGAIKBN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 4108-SRYKA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "FSANTEIYMD"
                   PERFORM 41012-FSANTEIYMD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "HKNCOMBI"
                   PERFORM 4109-HKNCOMBI-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "DAY01"
               WHEN    "ACTIVATE"      ALSO    "DAY02"
               WHEN    "ACTIVATE"      ALSO    "DAY03"
               WHEN    "ACTIVATE"      ALSO    "DAY04"
               WHEN    "ACTIVATE"      ALSO    "DAY05"
               WHEN    "ACTIVATE"      ALSO    "DAY06"
               WHEN    "ACTIVATE"      ALSO    "DAY07"
               WHEN    "ACTIVATE"      ALSO    "DAY08"
               WHEN    "ACTIVATE"      ALSO    "DAY09"
               WHEN    "ACTIVATE"      ALSO    "DAY10"
               WHEN    "ACTIVATE"      ALSO    "DAY11"
               WHEN    "ACTIVATE"      ALSO    "DAY12"
               WHEN    "ACTIVATE"      ALSO    "DAY13"
               WHEN    "ACTIVATE"      ALSO    "DAY14"
               WHEN    "ACTIVATE"      ALSO    "DAY15"
               WHEN    "ACTIVATE"      ALSO    "DAY16"
               WHEN    "ACTIVATE"      ALSO    "DAY17"
               WHEN    "ACTIVATE"      ALSO    "DAY18"
               WHEN    "ACTIVATE"      ALSO    "DAY19"
               WHEN    "ACTIVATE"      ALSO    "DAY20"
               WHEN    "ACTIVATE"      ALSO    "DAY21"
               WHEN    "ACTIVATE"      ALSO    "DAY22"
               WHEN    "ACTIVATE"      ALSO    "DAY23"
               WHEN    "ACTIVATE"      ALSO    "DAY24"
               WHEN    "ACTIVATE"      ALSO    "DAY25"
               WHEN    "ACTIVATE"      ALSO    "DAY26"
               WHEN    "ACTIVATE"      ALSO    "DAY27"
               WHEN    "ACTIVATE"      ALSO    "DAY28"
               WHEN    "ACTIVATE"      ALSO    "DAY29"
               WHEN    "ACTIVATE"      ALSO    "DAY30"
               WHEN    "ACTIVATE"      ALSO    "DAY31"
                   PERFORM 41013-DAY-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "SANTEILST"
                   PERFORM 41012-LISTSEL-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       41011-RENNUM-HEN-SEC         SECTION.
      *
           MOVE    1                  TO  SPA-GMN-CUR
      *
           INITIALIZE                 SPA-GMN-KOUMOKU
                                      SPA-NAI-KOUMOKU
      *
           MOVE    K06-SELNUM         TO  SPA-GMN-RENNUM
      *
           IF      SPA-GMN-RENNUM      =   ZERO
               IF      SPA-GMN-MAX         =   100
                   MOVE    "0002"              TO  SPA-ERRCD
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR 
               END-IF
               GO  TO  41011-RENNUM-HEN-EXT
           END-IF
      *
           IF      SPA-GMN-RENNUM      >   SPA-GMN-MAX
               MOVE    "0001"          TO  SPA-ERRCD
               GO      TO      41011-RENNUM-HEN-EXT
           END-IF
      *
           MOVE    SPA-GMN-TSRYCD (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-SRYCD
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-NYUGAIKBN
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-SRYKA
           MOVE    SPA-GMN-TSRYKA (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-SRYKAH
           MOVE    SPA-NAI-THKNCOMBI (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-HKNCOMBI
           MOVE    SPA-GMN-THKNCOMBI (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-HKNCOMBIMEI
           MOVE    SPA-GMN-TMEISYO (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-MEISYO
           MOVE    SPA-GMN-TMSANTEICNT (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-MSANTEICNT
           MOVE    SPA-GMN-TMSANTEID (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-MSANTEID
           MOVE    SPA-GMN-TFSANTEIYMD (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-FSANTEIYMD
           MOVE    SPA-NAI-TFSANTEIYMD (SPA-GMN-RENNUM)
                                       TO  SPA-NAI-FSANTEIYMD
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       31
               MOVE    SPA-NAI-TDAY (SPA-GMN-RENNUM IDZ)
                                           TO  SPA-GMN-DAY(IDZ)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SRYCD        TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           IF      FLG-TENSU            =   ZERO
               MOVE    TNS-SANTEIRRKKBN TO  SPA-NAI-SANTEIRRKKBN
               MOVE    TNS-JGNCNT       TO  SPA-NAI-JGNCNT
               MOVE    TNS-JGNCNT1D     TO  SPA-NAI-JGNCNT1D
           END-IF 
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       41011-RENNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-RENNUM2
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX
              IF      K06-SELECT (IDX)        =  "T"
                  IF      IDX                 =   SPA-GMN-RENNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-GMN-TRENNUM (IDX)    TO  WRK-RENNUM2
                  END-IF
                  MOVE    SPA-GMN-TRENNUM (IDX)    TO  WRK-RENNUM
              END-IF
           END-PERFORM
      *
           IF      WRK-RENNUM2         >   ZERO
               MOVE    WRK-RENNUM2         TO  SPA-GMN-RENNUM
           ELSE
               MOVE    WRK-RENNUM          TO  SPA-GMN-RENNUM
           END-IF
      *
           MOVE    SPA-GMN-RENNUM      TO  K06-SELNUM
           PERFORM 41011-RENNUM-HEN-SEC
      *
           .
       41012-LISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       41012-SRYYM-CHK-SEC           SECTION.
      *
           MOVE    39                TO  SPA-GMN-CUR
      *
           IF      K06-SRYYM            =   SPA-GMN-SRYYM
               CONTINUE
           ELSE
               MOVE   K06-SRYYM          TO  SPA-GMN-SRYYM
               MOVE    SPACE             TO  SPA-NAI-SRYYM
      *
               INITIALIZE                    SPA-GMN-KOUMOKU
                                             SPA-NAI-KOUMOKU
      *
               IF     SPA-GMN-SRYYM       =   SPACE
                   MOVE    "0001"             TO  SPA-ERRCD
                   GO  TO  41012-SRYYM-CHK-EXT
               ELSE
                   MOVE    SPA-GMN-SRYYM      TO  WRK-YMD
                   MOVE    "1"                TO  WRK-INTYPE
                   PERFORM 5002-HIZUKE-CHK-SEC
                   IF      SPA-ERRCD            =   SPACE
                       MOVE    WRK-SYMD   (1:6)     TO  SPA-NAI-SRYYM 
                       MOVE    WRK-HENYMDG(1:6)     TO  SPA-GMN-SRYYM
                   ELSE
                      GO  TO  41012-SRYYM-CHK-EXT
                   END-IF
               END-IF 
      *
               PERFORM 3101-SANTEI-SPA-SEC
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       41012-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード  入力処理
      *****************************************************************
       4102-SRYCD-CHK-SEC               SECTION.
      *   
           MOVE    2                    TO  SPA-GMN-CUR
      *
           MOVE    K06-SRYCD            TO  SPA-GMN-SRYCD
           MOVE    SPACE                TO  SPA-GMN-MEISYO
           MOVE    ZERO                 TO  SPA-NAI-SANTEIRRKKBN
      *
           IF      SPA-GMN-SRYCD        =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE
               INITIALIZE                   TNS-TENSU-REC
               MOVE    SPA-GMN-SRYCD        TO  TNS-SRYCD
               PERFORM 900-TENSU-READ-SEC
               IF      FLG-TENSU            =   ZERO
                   IF      TNS-SANTEIRRKKBN     =   ZERO
                       MOVE    "0011"           TO  SPA-ERRCD
                   ELSE
                       MOVE    TNS-NAME         TO  SPA-GMN-MEISYO
                       MOVE    TNS-SANTEIRRKKBN TO
                                                SPA-NAI-SANTEIRRKKBN
                       MOVE    TNS-JGNCNT       TO  SPA-NAI-JGNCNT
                       MOVE    TNS-JGNCNT1D     TO
                                                SPA-NAI-JGNCNT1D
                   END-IF 
               ELSE
                   MOVE    "0012"               TO  SPA-ERRCD
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD             =   SPACE
               MOVE    3                     TO  SPA-GMN-CUR
           END-IF     
           .
       4102-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4107-NYUGAIKBN-CHK-SEC                SECTION.
      *   
           MOVE    3                    TO  SPA-GMN-CUR
      *
           MOVE    K06-NYUGAIKBN        TO  SPA-GMN-NYUGAIKBN
      *
           EVALUATE    SPA-NAI-SANTEIRRKKBN
               WHEN    1
               WHEN    3
                   IF      SPA-GMN-NYUGAIKBN    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    "0013"               TO  SPA-ERRCD
                   END-IF
               WHEN    2
               WHEN    4
                   IF      SPA-GMN-NYUGAIKBN    =   1   OR  2
                       CONTINUE
                   ELSE
                       MOVE    "0014"               TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD             =   SPACE
               MOVE    4                     TO  SPA-GMN-CUR
           END-IF     
           .
       4107-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科  入力処理
      *****************************************************************
       4108-SRYKA-CHK-SEC              SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           MOVE    K06-SRYKA           TO  SPA-GMN-SRYKA
           MOVE    SPACE               TO  SPA-GMN-SRYKAH 
      *
           INITIALIZE                   TNS-TENSU-REC
           MOVE    SPA-GMN-SRYCD        TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
      *
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
               WHEN    2
                   IF      SPA-GMN-SRYKA        =   ZERO
                       MOVE    6                    TO  SPA-GMN-CUR
                   ELSE
                       MOVE    "0015"               TO  SPA-ERRCD
                   END-IF
               WHEN    3
               WHEN    4
                   IF      SPA-GMN-SRYKA        =   ZERO
                       MOVE    "0016"               TO  SPA-ERRCD
                   ELSE
                       INITIALIZE                   SYS-1005-REC
                       MOVE    "1005"           TO  SYS-1005-KANRICD
                       MOVE    SPA-GMN-SRYKA    TO  SYS-1005-KBNCD
                       MOVE    SPA-NAI-SRYYM    TO  ORC-DBYMD (1:6)
                       MOVE    "01"             TO  ORC-DBYMD (7:2)
                       MOVE    SYS-1005-REC     TO  MCPDATA-REC
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI     NOT =   ZERO
                           MOVE    "0017"               TO  SPA-ERRCD
                       ELSE
                           MOVE    MCPDATA-REC      TO  SYS-1005-REC
                           MOVE    SYS-1005-SRYKANAME1
                                                    TO  SPA-GMN-SRYKAH
                           MOVE    6                TO  SPA-GMN-CUR
                       END-IF
                   END-IF    
           END-EVALUATE
           .
       4108-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ  入力処理
      *****************************************************************
       4109-HKNCOMBI-CHK-SEC              SECTION.
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
           MOVE    K06-HKNCOMBI        TO  SPA-GMN-HKNCOMBI
           MOVE    SPACE               TO  SPA-GMN-HKNCOMBIMEI
           IF      SPA-GMN-HKNCOMBI    =   ZERO
               MOVE    5                    TO  SPA-GMN-CUR
               GO      TO      4109-HKNCOMBI-CHK-EXT
           END-IF
      *
      *    保険毎のコードの判定
           MOVE    SPA-GMN-SRYCD       TO  WRK-SRYCD
           PERFORM 1000-SANTEI-CHK-SEC
      *    保険組合せがキーにない時、エラー
           IF      FLG-HKNSAN          =   ZERO
               MOVE    "0021"              TO  SPA-ERRCD
               GO      TO      4109-HKNCOMBI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
           PERFORM 31011-HKNCOMBI-SET-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *         労災の時、コメントの追加
               IF      COMB-HKNNUM         =   "971"
                                           OR  "973"
                   MOVE    WRK-HKNCOMBIMEI     TO  SPA-GMN-HKNCOMBIMEI
               ELSE
                   MOVE    "0023"              TO  SPA-ERRCD
                   GO      TO      4109-HKNCOMBI-CHK-EXT
               END-IF
           ELSE
               MOVE    "0022"              TO  SPA-ERRCD
               GO      TO      4109-HKNCOMBI-CHK-EXT
           END-IF
      *
           MOVE    5                   TO  SPA-GMN-CUR
           .
       4109-HKNCOMBI-CHK-EXT.
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
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
           END-SEARCH
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    初回算定日  入力処理
      *****************************************************************
       41012-FSANTEIYMD-CHK-SEC            SECTION.
      *
           MOVE    5                       TO  SPA-GMN-CUR
      *
           MOVE    K06-FSANTEIYMD          TO  SPA-GMN-FSANTEIYMD
           MOVE    SPACE                   TO  SPA-NAI-FSANTEIYMD 
      *
           IF     SPA-GMN-FSANTEIYMD       =   SPACE
               MOVE    "0018"                  TO  SPA-ERRCD
               GO  TO  41012-FSANTEIYMD-CHK-EXT
           ELSE
               MOVE    SPA-GMN-FSANTEIYMD      TO  WRK-YMD
               MOVE    SPACE                   TO  WRK-INTYPE
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYMD            TO  SPA-NAI-FSANTEIYMD
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-FSANTEIYMD
               ELSE    
                  GO  TO  41012-FSANTEIYMD-CHK-EXT
               END-IF
           END-IF 
      *
           MOVE    8                     TO  SPA-GMN-CUR
           .
       41012-FSANTEIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療回数テーブル  入力処理
      *****************************************************************
       41013-DAY-CHK-SEC                   SECTION.
      *
           MOVE    7                       TO  SPA-GMN-CUR
      *
           MOVE    MCP-WIDGET              TO  WRK-WIDGET
           MOVE    WRK-WIDGET2             TO  IDY
      *
           COMPUTE SPA-GMN-CUR     =   SPA-GMN-CUR    +   IDY
      *
           MOVE    K06-DAY (IDY)           TO  SPA-GMN-DAY (IDY)
      *
           IF      SPA-NAI-JGNCNT1D        >   ZERO
               IF      SPA-GMN-DAY (IDY)       >   SPA-NAI-JGNCNT1D
                   MOVE    "0019"                  TO  SPA-ERRCD
                   GO  TO  41013-DAY-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO                    TO  SPA-GMN-MSANTEICNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               ADD     SPA-GMN-DAY (IDX)       TO  SPA-GMN-MSANTEICNT
           END-PERFORM 
      *
           IF      SPA-NAI-JGNCNT          >   ZERO
               IF      SPA-GMN-MSANTEICNT      >   SPA-NAI-JGNCNT
                   MOVE    "0020"                  TO  SPA-ERRCD
                   GO  TO  41013-DAY-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN-DAY (IDY)       >   ZERO
               IF      SPA-GMN-MSANTEID        >   IDY     OR 
                       SPA-GMN-MSANTEID        =   ZERO 
                   MOVE    IDY                     TO  SPA-GMN-MSANTEID
               END-IF
           END-IF
      *
           IF      IDY                     =   31
               MOVE    2                       TO  SPA-GMN-CUR
           ELSE
               ADD     1                       TO  SPA-GMN-CUR
           END-IF
           .
       41013-DAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月分表示　処理
      *****************************************************************
       450-MAETUKI-SEC                    SECTION.
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
           COMPUTE WRK-SRY-MM          =   WRK-SRY-MM    -   1
           IF      WRK-SRY-MM          =   ZERO
               COMPUTE WRK-SRY-YY      =   WRK-SRY-YY    -   1
               MOVE    12              TO  WRK-SRY-MM
           END-IF
      *    表示用編集
           MOVE    WRK-SRYYM           TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-SRYYM           TO  SPA-NAI-SRYYM
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           PERFORM 3101-SANTEI-SPA-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       450-MAETUKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月分表示　処理
      *****************************************************************
       480-ATOTUKI-SEC                    SECTION.
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
           COMPUTE WRK-SRY-MM          =   WRK-SRY-MM    +   1
           IF      WRK-SRY-MM          >   12
               COMPUTE WRK-SRY-YY      =   WRK-SRY-YY    +   1
               MOVE    1               TO  WRK-SRY-MM
           END-IF
      *    表示用編集
           MOVE    WRK-SRYYM           TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-SRYYM           TO  SPA-NAI-SRYYM
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           PERFORM 3101-SANTEI-SPA-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       480-ATOTUKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0101"          TO  SPA-KIDCD
               GO      TO      210-BACK-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K06"               TO  SPA-MOTOPG
      *
           IF      FLG-SYORI           =   ZERO
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加　処理
      *****************************************************************
       420-TUIKA-SEC                SECTION.
      *
           IF      SPA-GMN-RENNUM  >   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               GO  TO  420-TUIKA-EXT
           END-IF
      *
           IF      SPA-GMN-MAX      =   100
               MOVE    "0002"               TO  SPA-ERRCD
               GO      TO      420-TUIKA-EXT
           END-IF
      *
           IF     (SPA-GMN-KOUMOKU  =   SPACE )  OR
                  (SPA-GMN-SRYCD    =   SPACE )
               MOVE    "0008"               TO  SPA-ERRCD
               GO      TO      420-TUIKA-EXT
           END-IF
      *
      *    確認
           PERFORM 4102-SRYCD-CHK-SEC
           IF      SPA-ERRCD        NOT =   SPACE
               GO      TO      420-TUIKA-EXT
           END-IF
      *
      *----(01.00.01) LINE UPD START ----------------------------------
      *****COMPUTE IDY     =   SPA-GMN-MAX  +   1
           COMPUTE IDY-CHK     =   SPA-GMN-MAX  +   1
      *----(01.00.01) LINE UPD END   ----------------------------------
           PERFORM 4202-INTBL-CHK-SEC
           IF      SPA-ERRCD        NOT =   SPACE
              GO  TO  420-TUIKA-EXT
           END-IF
      *
           ADD     1                TO  SPA-GMN-MAX
           MOVE    SPA-GMN-MAX      TO
                                SPA-GMN-TRENNUM   (SPA-GMN-MAX)
      *
           MOVE    SPA-GMN-MAX          TO  IDX
           PERFORM 4201-INTBL-SET-SEC
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    1                   TO  SPA-NAI-TUPDFLG (IDX)
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           MOVE    1                TO  SPA-GMN-CUR  
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    "1"              TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           .
       420-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目重複チェック処理
      *****************************************************************
       4202-INTBL-CHK-SEC                SECTION.
      *
           MOVE    SPACE                   TO  SPA-ERRCD
      *
           MOVE    SPA-GMN-CUR             TO  WRK-GMN-CUR
           MOVE    "ACTIVATE"              TO  MCP-EVENT
           MOVE    2                       TO  SPA-GMN-CUR 
           PERFORM UNTIL   SPA-GMN-CUR     =   38
                   OR      SPA-ERRCD   NOT =   SPACE    
               IF      IDX         =   6   OR  7 
                   CONTINUE
               ELSE
                   PERFORM 5001-MAPCUR-SEC
                   PERFORM 200-ENTRY
               END-IF
           END-PERFORM
           MOVE    "CLICKED"               TO  MCP-EVENT
           IF      SPA-ERRCD           NOT =   SPACE 
               GO TO  4202-INTBL-CHK-EXT
           END-IF
      * 
           MOVE    WRK-GMN-CUR             TO  SPA-GMN-CUR
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       100
                   OR      IDX     >       SPA-GMN-MAX
      *----(01.00.01) LINE UPD START ----------------------------------
      *D       IF      IDX         =   IDY 
               IF      IDX         =   IDY-CHK
      *----(01.00.01) LINE UPD END   ----------------------------------
                   CONTINUE
               ELSE
                   IF      SPA-GMN-TSRYCD    (IDX) =  SPA-GMN-SRYCD
                   AND     SPA-NAI-TNYUGAIKBN(IDX) =  SPA-GMN-NYUGAIKBN
                   AND     SPA-NAI-TSRYKA    (IDX) =  SPA-GMN-SRYKA
                   AND     SPA-NAI-THKNCOMBI (IDX) =  SPA-GMN-HKNCOMBI
      *----(01.00.01) LINE UPD START ----------------------------------
      **************** MOVE    "0002"              TO  SPA-ERRCD
                       MOVE    "0010"              TO  SPA-ERRCD
      *----(01.00.01) LINE UPD END   ----------------------------------
                       MOVE    100                 TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    保険の適用期間のチェック
           IF      SPA-GMN-HKNCOMBI    NOT =   ZERO
               MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
               PERFORM 31011-HKNCOMBI-SET-SEC
               IF      FLG-HKNCOMBI        =   ZERO
                   MOVE    ZERO                TO  WRK-ST-SRYYMD
                   MOVE    ZERO                TO  WRK-ED-SRYYMD
                   MOVE    SPA-SRYYMD(1:6)     TO  WRK-ST-SRYYMD
                   MOVE    SPA-SRYYMD(1:6)     TO  WRK-ED-SRYYMD
                   PERFORM VARYING IDXS    FROM    1   BY  1
                           UNTIL   IDXS    >       31
                       IF      SPA-GMN-DAY (IDXS)  >   ZERO
                           IF      WRK-STDD        =   ZERO
                               MOVE    IDXS        TO  WRK-STDD
                           END-IF
                           MOVE    IDXS            TO  WRK-EDDD
                       END-IF
                   END-PERFORM
                   IF      WRK-STDD        NOT =   ZERO
                      IF  ((WRK-ST-SRYYMD      >=  COMB-TEKSTYMD )  AND
                           (WRK-ST-SRYYMD      <=  COMB-TEKEDYMD )) AND
                          ((WRK-ED-SRYYMD      >=  COMB-TEKSTYMD )  AND
                           (WRK-ED-SRYYMD      <=  COMB-TEKEDYMD ))
                         CONTINUE
                     ELSE
                         MOVE    "0024"              TO  SPA-ERRCD
                         GO      TO      4202-INTBL-CHK-EXT
                     END-IF
                   END-IF
               END-IF
           END-IF
           .
       4202-INTBL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4201-INTBL-SET-SEC                  SECTION.
      *
           MOVE    SPA-GMN-SRYCD           TO  SPA-GMN-TSRYCD    (IDX)
           MOVE    SPA-GMN-NYUGAIKBN       TO  SPA-NAI-TNYUGAIKBN(IDX)
           EVALUATE    SPA-GMN-NYUGAIKBN
               WHEN    ZERO
                   MOVE    SPACE           TO  SPA-GMN-TNYUGAIKBN(IDX)
               WHEN    "1"
                       MOVE    "入"        TO  SPA-GMN-TNYUGAIKBN(IDX)
               WHEN    "2"
                       MOVE    "外"        TO  SPA-GMN-TNYUGAIKBN (IDX)
           END-EVALUATE
           MOVE    SPA-GMN-SRYKA           TO  SPA-NAI-TSRYKA    (IDX)
           MOVE    SPA-GMN-SRYKAH          TO  SPA-GMN-TSRYKA    (IDX)
           MOVE    SPA-GMN-HKNCOMBI        TO  SPA-NAI-THKNCOMBI (IDX)
           MOVE    SPA-GMN-HKNCOMBIMEI     TO  SPA-GMN-THKNCOMBI (IDX)
           MOVE    SPA-GMN-MEISYO          TO  SPA-GMN-TMEISYO   (IDX)
           MOVE    SPA-GMN-FSANTEIYMD      TO  SPA-GMN-TFSANTEIYMD(IDX)
           MOVE    SPA-NAI-FSANTEIYMD      TO  SPA-NAI-TFSANTEIYMD(IDX)
           MOVE    SPA-GMN-MSANTEID        TO  SPA-GMN-TMSANTEID  (IDX)
           MOVE    SPA-GMN-MSANTEICNT      TO  SPA-GMN-TMSANTEICNT(IDX)
      *
           MOVE    ZERO                TO  WRK-DAY
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY >   31
               MOVE    SPA-GMN-DAY (IDY)   TO  SPA-NAI-TDAY(IDX IDY)
               IF      SPA-GMN-DAY (IDY)   >   ZERO
                   IF      WRK-DAY             =   ZERO
                       MOVE    IDY             TO  WRK-DAY
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-DAY             TO  SPA-GMN-TMSANTEID   (IDX)
      *
      *****IF       WRK-DAY            =   ZERO
      *        MOVE    SPACE               TO  SPA-NAI-TFSANTEIYMD(IDX)
      *        MOVE    SPACE               TO  SPA-GMN-TFSANTEIYMD(IDX)
      *    ELSE
      *        MOVE    SPA-NAI-TFSANTEIYMD(IDX)    TO  WRK-SYMD
      ******** IF      SPA-NAI-TFSANTEIYMD (IDX)(7:2)  >  WRK-DAY
      *        IF      WRK-SDD             >  WRK-DAY
      *            MOVE    WRK-DAY         TO  SPA-NAI-TFSANTEIYMD(IDX)
      *                                                           (7:2)
      *            MOVE    SPA-NAI-TFSANTEIYMD(IDX)    TO  WRK-SYMD
      *            PERFORM 5002-HIZUKE-HEN-SEC
      *            MOVE    WRK-HENYMDG         TO  SPA-GMN-TFSANTEIYMD
      *                                                          (IDX)
      *        END-IF
      *****END-IF
      *
           EVALUATE   MCP-WIDGET
               WHEN    "B02"
                   MOVE    1               TO  SPA-NAI-TUPDFLG (IDX)
               WHEN    "B04"
                   MOVE    2               TO  SPA-NAI-TUPDFLG (IDX)
           END-EVALUATE
      *
           INITIALIZE               SPA-GMN-KOUMOKU
                                    SPA-NAI-KOUMOKU
      *
          .
       4201-INTBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新　処理
      *****************************************************************
       430-HENKOU-SEC              SECTION.
      *
           IF      SPA-GMN-RENNUM  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  430-HENKOU-EXT
           END-IF
      * 
      *----(01.00.01) LINE UPD START ----------------------------------
      *****MOVE    SPA-GMN-RENNUM  TO  IDY
           MOVE    SPA-GMN-RENNUM  TO  IDY-CHK
      *----(01.00.01) LINE UPD END   ----------------------------------
      *
           PERFORM 4202-INTBL-CHK-SEC
           IF      SPA-ERRCD        NOT =   SPACE
              GO  TO  430-HENKOU-EXT
           END-IF
      *
           MOVE    SPA-GMN-RENNUM  TO  IDX
      *
           PERFORM 4201-INTBL-SET-SEC
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    2                   TO  SPA-NAI-TUPDFLG (IDX)
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           MOVE    1                   TO  SPA-GMN-CUR 
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    "1"              TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
          .
       430-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       440-SAKUJYO-SEC             SECTION.
      *
           IF      SPA-GMN-RENNUM  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-EXT
           END-IF
      *
           MOVE    SPA-GMN-RENNUM      TO  IDX
      *
           ADD     1                   TO  SPA-DEL-MAX
           MOVE    SPA-DEL-MAX         TO  IDZ
           MOVE    SPA-GMN-SRYCD       TO  SPA-DEL-TSRYCD     (IDZ)
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-DEL-TNYUGAIKBN (IDZ)
           MOVE    SPA-GMN-SRYKA       TO  SPA-DEL-TSRYKA     (IDZ)
           MOVE    SPA-GMN-HKNCOMBI    TO  SPA-DEL-THKNCOMBI  (IDZ)
      * 
           INITIALIZE                  SPA-GMN-TBLO (IDX)
           INITIALIZE                  SPA-NAI-TBLO (IDX)
      *
           INITIALIZE                  WRK-GMN-SORT
           MOVE    SPA-GMN-TBL         TO  WRK-GMN-TBL
           MOVE    SPA-NAI-TBL         TO  WRK-NAI-TBL
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   100
               IF      WRK-GMN-TRENNUM  (IDX)     =   ZERO
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    WRK-GMN-TBLO (IDX)  TO
                                               SPA-GMN-TBLO (IDX2)
                   MOVE    WRK-NAI-TBLO (IDX)  TO
                                               SPA-NAI-TBLO (IDX2)
                   MOVE    IDX2                TO
                                               SPA-GMN-TRENNUM(IDX2)
               END-IF
           END-PERFORM
           MOVE    IDX2                TO  SPA-GMN-MAX
      *
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
      *
           MOVE    1                   TO  SPA-GMN-CUR  
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    "1"              TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
          .
       440-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
           IF      SPA-GMN-SRYCD      NOT =   SPACE
           OR      SPA-GMN-FSANTEIYMD NOT =   SPACE
               MOVE    "0009"             TO  SPA-ERRCD
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           IF      SPA-GMN-RENNUM  NOT =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               GO  TO  450-TOUROKU-EXT
           END-IF
      *    更新済
           MOVE    SPACE            TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    マスター削除
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-DEL-MAX
               INITIALIZE                      SANTEI-KEY
               MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
               MOVE    SPA-DEL-TSRYCD(IDX) TO  SANTEI-SRYCD
               MOVE    SPA-DEL-TNYUGAIKBN (IDX)
                                           TO  SANTEI-NYUGAIKBN
               MOVE    SPA-DEL-TSRYKA(IDX) TO  SANTEI-SRYKA  
               MOVE    SPA-DEL-THKNCOMBI(IDX)
                                           TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"                  TO  SPA-ERRCD
                   DISPLAY "SANTEI-KEY =>" SANTEI-KEY 
                   GO  TO  450-TOUROKU-EXT
               END-IF
           END-PERFORM
      *
      *    マスター更新
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               EVALUATE    SPA-NAI-TUPDFLG (IDX)
                   WHEN    ZERO
                       CONTINUE
                   WHEN    1
      *                算定追加
                       PERFORM 45011-SANTEI-INS-SEC
      ***************  INITIALIZE                      SANTEI-REC
      *                MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
      *                MOVE    SPA-PTID            TO  SANTEI-PTID
      *                MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
      *                MOVE    SPA-GMN-TSRYCD(IDX) TO  SANTEI-SRYCD
      *                MOVE    SPA-NAI-TNYUGAIKBN (IDX)
      *                                            TO  SANTEI-NYUGAIKBN
      *                MOVE    SPA-NAI-TSRYKA(IDX) TO  SANTEI-SRYKA
      *                PERFORM 4501-SANTEI-HENSYU-SEC 
      *                MOVE    SANTEI-REC          TO  MCPDATA-REC
      *                MOVE    "DBINSERT"          TO  MCP-FUNC
      *                MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
      *                CALL    "MCPDBSUB"          USING
      *                                                MCPAREA
      *                                                MCPDATA-REC
      *                IF      MCP-RC          NOT =   ZERO 
      *                    MOVE    "1001"              TO  SPA-ERRCD
      *                    DISPLAY "INS SANTEI-KEY =>" SANTEI-KEY 
      *                    GO  TO  450-TOUROKU-EXT
      *****************END-IF
                   WHEN    2
                       INITIALIZE                      SANTEI-KEY
                       MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
                       MOVE    SPA-PTID            TO  SANTEI-PTID
                       MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
                       MOVE    SPA-GMN-TSRYCD(IDX) TO  SANTEI-SRYCD
                       MOVE    SPA-NAI-TNYUGAIKBN (IDX)
                                                   TO  SANTEI-NYUGAIKBN
                       MOVE    SPA-NAI-TSRYKA(IDX) TO  SANTEI-SRYKA  
                       MOVE    SPA-NAI-THKNCOMBI(IDX)
                                               TO  SANTEI-HKNCOMBINUM
                       PERFORM 900-SANTEI-INV-SEC
                       IF      FLG-SANTEI          =   1
      *                    MOVE    "1003"              TO  SPA-ERRCD
      *                    DISPLAY "INV SANTEI-KEY =>" SANTEI-KEY 
      *                    算定追加
                           PERFORM 45011-SANTEI-INS-SEC
                           GO  TO  450-TOUROKU-EXT
                       END-IF
                       PERFORM 4501-SANTEI-HENSYU-SEC 
                       MOVE    SANTEI-REC          TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"          USING
                                                        MCPAREA
                                                        ORCMCPAREA
                                                        MCPDATA-REC
                       IF      MCP-RC          NOT =   ZERO 
                           MOVE    "1001"              TO  SPA-ERRCD
                           DISPLAY "UPD SANTEI-KEY =>" SANTEI-KEY 
                           GO  TO  450-TOUROKU-EXT
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        初診料の修正の時、診療科履歴の初診算定日修正
               PERFORM 4510-KARRK-UPD-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ZERO                TO  FLG-SYORI
               PERFORM 210-BACK
           END-IF
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       45011-SANTEI-INS-SEC                SECTION.
      *
           INITIALIZE                      SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
           MOVE    SPA-GMN-TSRYCD(IDX) TO  SANTEI-SRYCD
           MOVE    SPA-NAI-TNYUGAIKBN (IDX)
                                       TO  SANTEI-NYUGAIKBN
           MOVE    SPA-NAI-TSRYKA(IDX) TO  SANTEI-SRYKA
           MOVE    SPA-NAI-THKNCOMBI(IDX)
                                       TO  SANTEI-HKNCOMBINUM
           PERFORM 4501-SANTEI-HENSYU-SEC 
      *
           MOVE    SMCNDATE-YMD        TO  SANTEI-CREYMD
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "INS SANTEI-KEY =>" SANTEI-KEY 
            END-IF
           .
       45011-SANTEI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       4501-SANTEI-HENSYU-SEC                SECTION.
      *
           MOVE    SPA-NAI-TFSANTEIYMD(IDX)
                                           TO  SANTEI-FSANTEIYMD
           MOVE    SPA-GMN-TMSANTEID  (IDX)
                                           TO  SANTEI-MSANTEID
           MOVE    SPA-GMN-TMSANTEICNT(IDX)
                                           TO  SANTEI-MSANTEICNT
           PERFORM VARYING     IDY    FROM    1   BY  1
                   UNTIL       IDY    >       31
               MOVE    SPA-NAI-TDAY (IDX IDY) TO  SANTEI-DAY (IDY)
           END-PERFORM
      *
           MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *
           .
       4501-SANTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴の初診算定日修正処理
      *****************************************************************
       4510-KARRK-UPD-SEC             SECTION.
      *
           INITIALIZE                  ORCSKARRKAREA
           MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
      *
           CALL    "ORCSKARRK"         USING
                                       ORCSKARRKAREA
                                       SPA-AREA.
      *
           IF      SKARRK-ERRMSG   NOT =   SPACE
               MOVE    "1004"              TO  SPA-ERRCD
           END-IF
      *
           .
       4510-KARRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           .
       430-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-INTYPE          TO  SGDAY-INTYPE
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           GO      TO      5002-HIZUKE-CHK-EXT
      *----
      *
      *    PERFORM VARYING IDW     FROM    9   BY  -1
      *            UNTIL   IDW     <       1
      *        IF      WRK-YMD (IDW:1)     NOT =   SPACE
      *            MOVE    IDW             TO  WRK-YMDMAX
      *            MOVE    1               TO  IDW
      *        END-IF
      *    END-PERFORM
      *        
      *    IF      WRK-YMD  (7:3)       =   SPACE  
      *        IF      WRK-YMD  (1:4)  NOT NUMERIC
      *            INSPECT WRK-YMD (1:WRK-YMDMAX)
      *                                     REPLACING   ALL " " BY  "0"
      *            MOVE    WRK-YMD(1:1)     TO  WRK-WGO
      *            MOVE    WRK-YMD(2:2)     TO  WRK-WYY
      *            MOVE    WRK-YMD(5:2)     TO  WRK-WMM
      *        ELSE
      *            MOVE    WRK-YMD(1:6)     TO  WRK-WYMD (1:6)
      *        END-IF
      *        MOVE    "01"             TO  WRK-WDD
      *    ELSE  
      *        IF      WRK-YMD  (1:7)  NOT NUMERIC
      *            INSPECT     WRK-YMD (1:WRK-YMDMAX)
      *                                     REPLACING   ALL " " BY  "0"
      *            MOVE    WRK-YMD(1:1)     TO  WRK-WGO
      *            MOVE    WRK-YMD(2:2)     TO  WRK-WYY
      *            MOVE    WRK-YMD(5:2)     TO  WRK-WMM
      *            MOVE    WRK-YMD(8:2)     TO  WRK-WDD
      *        ELSE
      *            MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
      *        END-IF
      *    END-IF 
      *
      *    INITIALIZE                      STS-AREA-DAY
      *    INITIALIZE                      LNK-DAY1-AREA
      *    MOVE    "12"                TO  LNK-DAY1-IRAI
      *    MOVE    ZERO                TO  LNK-DAY1-YMD
      *    MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
      *    CALL    "ORCSDAY"           USING   STS-AREA-DAY
      *                                        LNK-DAY1-AREA
      *    IF      STS-DAY-RC1     NOT =   ZERO
      *        MOVE    "0001"          TO  SPA-ERRCD
      *    ELSE
      *        MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
      *        PERFORM 5002-HIZUKE-HEN-SEC
      *****END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  K06
      *
           MOVE    SPA-GMN-PTNUM       TO  K06-PTNUM
           MOVE    SPA-GMN-SRYYM       TO  K06-SRYYM
           MOVE    SPA-GMN-KANANAME    TO  K06-KANANAME
           MOVE    SPA-GMN-NAME        TO  K06-NAME
           MOVE    SPA-GMN-SEX         TO  K06-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  K06-BIRTHDAY
      *
           MOVE    SPA-NENREI-HEN      TO  K06-NENREI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX                 >   100
                   OR          SPA-GMN-TRENNUM (IDX)=   ZERO  
               MOVE    SPA-GMN-TRENNUM (IDX)   TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K06-TRENNUM    (IDX)
               MOVE    SPA-GMN-TSRYCD (IDX)    TO  K06-TSRYCD     (IDX)
               MOVE    SPA-GMN-TNYUGAIKBN (IDX)
                                               TO  K06-TNYUGAIKBN (IDX)
               MOVE    SPA-GMN-TSRYKA  (IDX)   TO  K06-TSRYKA     (IDX)
               MOVE    SPA-GMN-THKNCOMBI(IDX)  TO  K06-THKNCOMBI  (IDX)
               MOVE    SPA-GMN-TMEISYO (IDX)   TO  K06-TMEISYO    (IDX)
               MOVE    SPA-GMN-TFSANTEIYMD (IDX)
                                               TO  K06-TFSANTEIYMD(IDX)
               MOVE    SPA-GMN-TMSANTEICNT (IDX)
                                               TO  K06-TMSANTEICNT(IDX)
               MOVE    SPA-GMN-TMSANTEID   (IDX)
                                               TO  K06-TMSANTEID  (IDX)
      *
               MOVE    IDX                     TO  K06-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-RENNUM         TO  K06-SELNUM
           MOVE    SPA-GMN-SRYCD          TO  K06-SRYCD
           MOVE    SPA-GMN-NYUGAIKBN      TO  K06-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA          TO  K06-SRYKA
           MOVE    SPA-GMN-MEISYO         TO  K06-MEISYO
           MOVE    SPA-GMN-MSANTEICNT     TO  K06-MSANTEICNT
           MOVE    SPA-GMN-MSANTEID       TO  K06-MSANTEID
           MOVE    SPA-GMN-FSANTEIYMD     TO  K06-FSANTEIYMD
           MOVE    SPA-GMN-HKNCOMBI       TO  K06-HKNCOMBI
           MOVE    SPA-GMN-HKNCOMBIMEI    TO  K06-HKNCOMBIMEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       31
               MOVE    SPA-GMN-DAY(IDX)   TO  K06-DAY(IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN-SRYKAH         TO  K06-SRYKAMEI
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
         EVALUATE    SPA-GMN-CUR
             WHEN    01
                    MOVE  "SELNUM"     TO  MCP-WIDGET
             WHEN    39
                    MOVE  "SRYYM"      TO  MCP-WIDGET
             WHEN    02
                    MOVE  "SRYCD"      TO  MCP-WIDGET
             WHEN    03
                    MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
             WHEN    04
                    MOVE  "SRYKA"      TO  MCP-WIDGET
             WHEN    05
                    MOVE  "FSANTEIYMD" TO  MCP-WIDGET
             WHEN    06
                    MOVE  "HKNCOMBI"   TO  MCP-WIDGET
             WHEN    08  THRU  38
                    MOVE  "DAY"        TO  WRK-WIDGET1
                    COMPUTE IDZ =   SPA-GMN-CUR   -   7
                    MOVE    IDZ        TO  WRK-WIDGET2
                    MOVE    WRK-WIDGET TO  MCP-WIDGET
             WHEN    19
                    MOVE  "B01"        TO  MCP-WIDGET   
             WHEN    20
                    MOVE  "B02"        TO  MCP-WIDGET   
             WHEN    21
                    MOVE  "B03"        TO  MCP-WIDGET   
             WHEN    22
                    MOVE  "B04"        TO  MCP-WIDGET   
             WHEN    99
                    MOVE  "B12"        TO  MCP-WIDGET   
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"            TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "追加できません"            TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "警告メッセージ"            TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが選択してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "入力の途中です"            TO  SPA-ERRMSG
      *----(01.00.01) LINE ADD START ----------------------------------
               WHEN    "0010"
                   MOVE    "同じキーが存在します。処理できません"
                                                       TO  SPA-ERRMSG
      *----(01.00.01) LINE ADD END   ----------------------------------
               WHEN    "0011"
                   MOVE    "算定履歴の算定できない診療です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "点数マスタに存在しません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "入外区分の入力できない診療です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "入外区分を入力して下さい。"
                                                   TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "診療科の入力できない診療です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "診療科を入力して下さい。"
                                                   TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "診療科が存在しません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "算定日を入力して下さい。"
                                                   TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "１日の算定回数を超えています。"
                                                   TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "１月の算定回数を超えています。"
                                                   TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "保険組合せは入力できません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "保険組合せがありません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "保険組合せが健保です。入力できません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "保険組合せの適用期間外です。"
                                                   TO  SPA-ERRMSG
                   MOVE    "算定日を変更して下さい。"
                                                   TO  SPA-ERRMSG(29:)
               WHEN    "1001"
                   MOVE    "更新処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "マスタが存在しません" 
                                                   TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "診療科履歴更新エラー" 
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  KERR
           MOVE    SPA-ERRCD            TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  KERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "K06"                TO  SPA-MOTOPG
           MOVE    "KERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "KERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END 
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   MOVE
                   "内容が修正されています。"
                                       TO  WRK-KIDMSG(1:)
                   MOVE
                   "ＯＫで修正分は反映されずに終了します"
                                       TO  WRK-KIDMSG(25:)
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    SPACE               TO  KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K06"               TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-NAI-SRYYM       TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
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
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "TENSU-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴マスター読込
      *****************************************************************
       900-SANTEI-INV-SEC              SECTION.
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SANTEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-SANTEI-INV-EXT.
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
      *    保険組合せマスター読込
      *****************************************************************
       940-HKNCOMBI-NEXT-SEC         SECTION.
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
       940-HKNCOMBI-NEXT-EXT.
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
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCMCPSUB"   USING
                                MCPAREA
                                ORCMCPAREA
                                MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
