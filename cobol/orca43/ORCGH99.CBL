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
       PROGRAM-ID.             ORCGH99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 請求管理−手書き訂正（Ｈ９９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     01/10/02  連番の追加      
      *  01.01.03    MCC-藤原     01/10/26  保険種別の記号修正
      *
      *  ２００２．１０改正対応 
      *  01.02.01    NACL-藤原    02/10/10  レセプト種別の出力の修正
      *  01.02.02    NACL-藤原    02/12/10  エラーメッセージの修正
      *  01.02.03    NACL-藤原    03/02/17  補助区分の追加
      *  01.02.04    NACL-藤原    03/05/28  補助区分の判定の修正
      *  01.02.05    NACL-藤原    04/01/26  排他制御処理追加
      *  01.02.06    NACL-藤原    05/09/29  補助区分からの編集の修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求管理
           COPY    "H01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "H01SCR-SPA".
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-Z9              PIC Z9.
           03  WRK-SELNO           PIC 9(02).
           03  WRK-RECESYUBETU     PIC X(08).
           03  WRK-YM              PIC X(07). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
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
           COPY    "CPRCF010.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
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
           COPY    "H01.INC".
           COPY    "H99.INC".
           COPY    "HERR.INC".
           COPY    "HID1.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-H01FREE
           MOVE    SPA-WORK        TO  SPA-H01KYOUTU
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
           MOVE    SPA-H01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-H01FREE     TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "HERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "H99    "           TO  MCP-WINDOW.
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
      *
           MOVE    4                   TO  SPA-H99GMN-CUR
      *
           PERFORM 41021-H99-HENSYU-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *     確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-SEC
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
      *********WHEN    "ACTIVATE"      ALSO    "PTNUM"
      *********    PERFORM 4101-PTNUM-CHK-SEC
      *********WHEN    "ACTIVATE"      ALSO    "SRYYM"
      *********    PERFORM 4102-SRYYM-CHK-SEC
      *********WHEN    "ACTIVATE"      ALSO    "SRYKA"
      *********    PERFORM 4103-SRYKA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4104-SELNUM-CHK-SEC
               WHEN    ANY             ALSO    "RECELST"
                   PERFORM 4105-RECESEL-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    2                   TO  SPA-H99GMN-CUR
      *
           MOVE    H99-PTNUM           TO  SPA-H99GMN-PTNUM
      *
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPID          TO  SPTID-HOSPID
           MOVE    SPA-H99GMN-PTNUM    TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-1009-TBL
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0004"          TO  SPA-ERRCD
           ELSE
               IF      SPTID-PTID          =   SPA-H99NAI-PTID
                   CONTINUE
               ELSE
                   INITIALIZE                  SPA-H99GMN-TBL
                                               SPA-H99NAI-TBL
                   MOVE    ZERO                TO  SPA-H99GMN-MAX
                   MOVE    SPTID-PTNUM     TO  SPA-H99GMN-PTNUM
                   MOVE    SPTID-PTID      TO  SPA-H99NAI-PTID
      *            患者マスター存在チェック
                   MOVE    SPA-HOSPID      TO  PTINF-HOSPID
                   MOVE    SPA-H99NAI-PTID TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF   NOT =   ZERO
                       MOVE    "0001"      TO  SPA-ERRCD
                   ELSE
      *
      *                排他制御処理
                       PERFORM 990-LOCK-IN-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           GO      TO     4101-PTNUM-CHK-EXT
                       ELSE
                           MOVE    PTINF-NAME  TO  SPA-H99GMN-NAME
                       END-IF
                   END-IF
               END-IF     
           END-IF
      *
           IF      SPA-ERRCD         =   SPACE
      *    請求管理ＤＢ検索処理
               IF      SPA-SYS-1001-HOSPSBT1   =   ZERO 
                   PERFORM 41021-SEIKYU-KENSAKU-SEC
               ELSE
                   MOVE    3               TO  SPA-H99GMN-CUR
               END-IF
           ELSE 
               MOVE    SPACE               TO  SPA-H99GMN-NAME
               MOVE    ZERO                TO  SPA-H99NAI-PTID 
           END-IF 
           .
       4101-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月　入力処理
      *****************************************************************
       4102-SRYYM-CHK-SEC          SECTION.
      *
           MOVE    1               TO  SPA-H99GMN-CUR
      *
           MOVE    H99-SRYYM       TO  SPA-H99GMN-SRYYM
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    SPA-H99GMN-SRYYM       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-H99GMN-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-H99NAI-SRYYM
               MOVE    2                 TO  SPA-H99GMN-CUR
           ELSE
               INITIALIZE                  SPA-H99GMN-TBL
                                           SPA-H99NAI-TBL
               MOVE    ZERO                TO  SPA-H99GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
               GO  TO  4102-SRYYM-CHK-EXT
           END-IF
           .
       4102-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ検索処理
      *****************************************************************
       41021-SEIKYU-KENSAKU-SEC        SECTION.
      *
           INITIALIZE                  SPA-H99GMN-TBL
                                       SPA-H99NAI-TBL
           MOVE    ZERO                TO  SPA-H99GMN-MAX
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPID          TO  RECE10-HOSPID
           MOVE    SPA-H99NAI-PTID     TO  RECE10-PTID
           IF      SPA-H99GMN-SRYKA    =   SPACE
               MOVE    ZERO            TO  RECE10-RECEKA
           ELSE
               MOVE    SPA-H99GMN-SRYKA
                                       TO  RECE10-RECEKA
           END-IF
           MOVE    SPA-H99NAI-SRYYM    TO  RECE10-SRYYM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SEIKYU-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SEIKYU-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    ZERO                TO  IDX 
           PERFORM             UNTIL   FLG-SEIKYU     =   1
                               OR      IDX            =   2
               ADD     1                   TO  IDX
               IF      IDX                 =   1 
                   MOVE    MCPDATA-REC     TO  RECE10-REC
               END-IF
      *
               MOVE    "SEIKYU-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *   
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SEIKYU-KEY2"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           EVALUATE    IDX
               WHEN    ZERO
                   MOVE    "0003"         TO  SPA-ERRCD
               WHEN    1
                   MOVE    RECE10-KEY     TO  SPA-H99-KEY
                   PERFORM 210-BACK
               WHEN    OTHER
                   PERFORM 41021-H99-HENSYU-SEC
           END-EVALUATE
           .
       41021-SEIKYU-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト検索画面編集処理
      *****************************************************************
       41021-H99-HENSYU-SEC         SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPID          TO  RECE10-HOSPID
           MOVE    SPA-H99NAI-PTID     TO  RECE10-PTID
           IF      SPA-H99GMN-SRYKA    =   SPACE
               MOVE    ZERO                TO  RECE10-RECEKA
           ELSE
               MOVE    SPA-H99GMN-SRYKA    TO  RECE10-RECEKA
           END-IF
           MOVE    SPA-H99NAI-SRYYM    TO  RECE10-SRYYM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SEIKYU-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SEIKYU-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    ZERO                TO  IDX 
           PERFORM     UNTIL       FLG-SEIKYU     =   1
                       OR          IDX            =  10
               ADD     1                   TO  IDX
               MOVE    MCPDATA-REC     TO  RECE10-REC
               MOVE    RECE10-KEY      TO
                                   SPA-H99NAI-RECE10-KEY (IDX)
               MOVE    IDX             TO  WRK-Z9
               MOVE    WRK-Z9          TO
                                   SPA-H99GMN-TRENNUM    (IDX)
               IF      RECE10-NYUGAIKBN    =   "1"
                   MOVE    "入　院"    TO
                                   SPA-H99GMN-TNYUGAIKBN (IDX)
               ELSE    
                   MOVE    "入院外"    TO
                                   SPA-H99GMN-TNYUGAIKBN (IDX)
               END-IF
               IF      RECE10-RECEKA   =   ZERO
                   CONTINUE
               ELSE
                   INITIALIZE              SYS-1005-REC
                   MOVE    "1005"          TO  SYS-1005-KANRICD
                   MOVE    RECE10-RECEKA   TO  SYS-1005-KBNCD
                   MOVE    SPA-SYSYMD      TO  ORC-DBYMD
                   MOVE    SYS-1005-REC    TO  MCPDATA-REC
                   PERFORM 910-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI    =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1005-REC
                   ELSE
                       INITIALIZE              SYS-1005-REC 
                   END-IF
                   MOVE    SYS-1005-SRYKANAME  TO
                                       SPA-H99GMN-TSRYKA(IDX)
               END-IF
               IF      RECE10-TEISYUTUSAKI     =   "1"
                   MOVE    "支払基金"      TO
                                   SPA-H99GMN-TTEISYUTUSAKI(IDX)
               ELSE    
                   MOVE    "国保連合会"    TO
                                   SPA-H99GMN-TTEISYUTUSAKI(IDX)
               END-IF
      *
               IF      RECE10-SRYYM        >   "200209"
                   MOVE    RECE10-RECESYUBETU  TO  WRK-RECESYUBETU
               ELSE        
                 EVALUATE  RECE10-RECESYUBETU
                                 ALSO  RECE10-TEISYUTUSAKI
                 WHEN    1111      ALSO      2
                   IF  RECE10-HONKZKKBN    =   2
                       MOVE    "★★★"    TO  WRK-RECESYUBETU
                   ELSE
                       MOVE    "★"        TO  WRK-RECESYUBETU
                   END-IF
                 WHEN    1111      ALSO      1
                 WHEN    1111      ALSO      ANY
                   MOVE    "★"            TO  WRK-RECESYUBETU
      *
                 WHEN    1112      ALSO      2
                   IF  RECE10-HONKZKKBN    =   2
                       MOVE    "☆☆☆☆"  TO  WRK-RECESYUBETU
                   ELSE
                       MOVE    "☆☆"      TO  WRK-RECESYUBETU
                   END-IF
                 WHEN    1112      ALSO      1
                   MOVE    "☆☆"          TO  WRK-RECESYUBETU
      *
                 WHEN    1115      ALSO      ANY
                   MOVE    "★★★"        TO  WRK-RECESYUBETU
      *
                 WHEN    1116      ALSO      ANY
                   MOVE    "☆☆☆☆"      TO  WRK-RECESYUBETU
      *
                 WHEN    1121      ALSO      2
                   IF  RECE10-HONKZKKBN    =   2
                       MOVE    "★★★"    TO  WRK-RECESYUBETU
                   ELSE
                       MOVE    "★"        TO  WRK-RECESYUBETU
                   END-IF
                  WHEN    1121      ALSO      1
                   MOVE    "★"            TO  WRK-RECESYUBETU
      *
                 WHEN    1122      ALSO      2
                   IF  RECE10-HONKZKKBN    =   2
                       MOVE    "☆☆☆☆"  TO  WRK-RECESYUBETU
                   ELSE
                       MOVE    "☆☆"      TO  WRK-RECESYUBETU
                   END-IF
                 WHEN    1122      ALSO      1
                   MOVE    "☆☆"          TO  WRK-RECESYUBETU
      *
                 WHEN    1125      ALSO      ANY
                   MOVE    "★★★"        TO  WRK-RECESYUBETU
      *
                 WHEN    1126      ALSO      ANY
                   MOVE    "☆☆☆☆"      TO  WRK-RECESYUBETU
      *
                 WHEN    1131      ALSO      2
                   IF  RECE10-HONKZKKBN    =   2
                       MOVE    "★★★"    TO  WRK-RECESYUBETU
                   ELSE
                       MOVE    "★"        TO  WRK-RECESYUBETU
                   END-IF
                 WHEN    1131      ALSO      1
                   MOVE    "★"            TO  WRK-RECESYUBETU
      *
                 WHEN    1132      ALSO      2
                   IF  RECE10-HONKZKKBN    =   2
                       MOVE    "☆☆☆☆"  TO  WRK-RECESYUBETU
                   ELSE
                       MOVE    "☆☆"      TO  WRK-RECESYUBETU
                   END-IF
                 WHEN    1132      ALSO      1
                   MOVE    "☆☆"          TO  WRK-RECESYUBETU
      *
                 WHEN    1135      ALSO      ANY
                   MOVE    "★★★"        TO  WRK-RECESYUBETU
      *
                 WHEN    1136      ALSO      ANY
                   MOVE    "☆☆☆☆"      TO  WRK-RECESYUBETU
      *
                 WHEN    1211      ALSO      ANY
                 WHEN    1221      ALSO      ANY
                 WHEN    1231      ALSO      ANY
                   MOVE    "●"            TO  WRK-RECESYUBETU
      *
                 WHEN    1212      ALSO      ANY
                 WHEN    1222      ALSO      ANY
                 WHEN    1232      ALSO      ANY
                   MOVE    "○○"          TO  WRK-RECESYUBETU
      *
                 WHEN    1311      ALSO      ANY
                 WHEN    1321      ALSO      ANY
                 WHEN    1331      ALSO      ANY
                   MOVE    "▲"            TO  WRK-RECESYUBETU
      *
                 WHEN    1312      ALSO      ANY
                 WHEN    1322      ALSO      ANY
                 WHEN    1332      ALSO      ANY
                   MOVE    "△△"          TO  WRK-RECESYUBETU
      *
                 WHEN    1411      ALSO      ANY
                 WHEN    1421      ALSO      ANY
                 WHEN    1431      ALSO      ANY
                   MOVE    "◆"            TO  WRK-RECESYUBETU
      *
                 WHEN    1412      ALSO      ANY
                 WHEN    1422      ALSO      ANY
                 WHEN    1432      ALSO      ANY
                   MOVE    "◇◇"          TO  WRK-RECESYUBETU
      *
                 WHEN    1415      ALSO      ANY
                 WHEN    1425      ALSO      ANY
                 WHEN    1435      ALSO      ANY
                   MOVE    "◆◆◆"        TO  WRK-RECESYUBETU
      *
                 WHEN    1416      ALSO      ANY
                 WHEN    1426      ALSO      ANY
                 WHEN    1436      ALSO      ANY
                   MOVE    "◇◇◇◇"      TO  WRK-RECESYUBETU
                 END-EVALUATE
               END-IF  
               MOVE    WRK-RECESYUBETU  TO
                                       SPA-H99GMN-TRECESYUBETU (IDX)
               MOVE    RECE10-HKNJANUM     TO
                                       SPA-H99GMN-THKNJANUM    (IDX)
               EVALUATE    RECE10-HKNNUM
                   WHEN    "002"
                   WHEN    "031"  THRU  "034"
                       EVALUATE    RECE10-HOJOKBN-KEY
                           WHEN    "1"
                           WHEN    "A"
                           WHEN    "D"
                               MOVE    "職務上"    TO
                                               SPA-H99GMN-THOJOKBN(IDX)
                           WHEN    "2"
                           WHEN    "B"
                           WHEN    "E"
                               MOVE    "下船３月"  TO
                                               SPA-H99GMN-THOJOKBN(IDX)
                           WHEN    "3"
                           WHEN    "C"
                           WHEN    "F"
                               MOVE    "通勤災害"  TO
                                               SPA-H99GMN-THOJOKBN(IDX)
                       END-EVALUATE
               END-EVALUATE
               STRING  RECE10-KIGO     DELIMITED   BY  SPACE
                       "　"            DELIMITED   BY  SIZE
                       RECE10-NUM      DELIMITED   BY  SPACE
                                       INTO    SPA-H99GMN-TKIGO(IDX)
               END-STRING
      *
               MOVE    "SEIKYU-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *   
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SEIKYU-KEY2"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    IDX                 TO  SPA-H99GMN-MAX
      *
           MOVE    4                   TO  SPA-H99GMN-CUR
           .
       41021-H99-HENSYU-EXT.
           EXIT.
      *
      ******************************************************
      *    診療科入力処理
      *****************************************************************
       4103-SRYKA-CHK-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-H99GMN-CUR
      *
           MOVE    H99-SRYKA           TO  SPA-H99GMN-SRYKA
      *
           MOVE    SPACE               TO SPA-H99GMN-SRYKANAME
      *
           IF      SPA-H99GMN-SRYKA    =   SPACE
               CONTINUE
           ELSE
               INITIALIZE              SYS-1005-REC
               MOVE    "1005"          TO  SYS-1005-KANRICD
               MOVE    SPA-H99GMN-SRYKA
                                       TO  SYS-1005-KBNCD
               MOVE    SPA-SYSYMD      TO  ORC-DBYMD
               MOVE    SYS-1005-REC    TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME
                                       TO  SPA-H99GMN-SRYKANAME
               ELSE
                   MOVE    "0001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE 
      *    請求管理ＤＢ検索処理
               PERFORM 41021-SEIKYU-KENSAKU-SEC
           END-IF
           .
       4103-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号　入力処理
      *****************************************************************
       4104-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    4               TO  SPA-H99GMN-CUR
      *
           MOVE    H99-SELNUM      TO  SPA-H99GMN-SELNUM
      *
           IF      SPA-H99GMN-SELNUM   <   1   OR
                   SPA-H99GMN-SELNUM   >   SPA-H99GMN-MAX
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-H99NAI-RECE10-KEY (SPA-H99GMN-SELNUM)
                                       TO  SPA-H99-KEY
               PERFORM 210-BACK
           END-IF
           .
       4104-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4105-RECESEL-SEC               SECTION.
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      H99-SELECT (IDX)    =   "T"
                   MOVE    IDX             TO  WRK-SELNO
                   MOVE    5               TO  IDX
               END-IF    
           END-PERFORM
      *
           MOVE    WRK-SELNO           TO  SPA-H99GMN-SELNUM
      *
           MOVE    SPA-H99NAI-RECE10-KEY (SPA-H99GMN-SELNUM)
                                       TO  SPA-H99-KEY
           PERFORM 210-BACK
           .
       41016-BYOMEISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "H01"               TO  SPA-SAKIPG
           MOVE    "H99"               TO  SPA-MOTOPG
      *
      ***  MOVE   "BACK"               TO  MCP-PUTTYPE
           MOVE   "JOIN"               TO  MCP-PUTTYPE
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
      *    出力処理
      *****************************************************************
       450-KAKUTEI-SEC                SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               IF      H99-SELECT (IDX)    =   "T"
                   MOVE    SPA-H99NAI-RECE10-KEY (IDX)
                                           TO  SPA-H99-KEY
                   MOVE    5               TO  IDX 
               END-IF
           END-PERFORM
      * 
           MOVE    SPA-H99GMN-PTNUM    TO  SPA-H01GMN-PTNUM
           MOVE    SPA-H99NAI-PTID     TO  SPA-H01NAI-PTID
           MOVE    SPA-H99GMN-SRYYM    TO  SPA-H01GMN-SRYYM
           MOVE    SPA-H99NAI-SRYYM    TO  SPA-H01NAI-SRYYM
           MOVE    SPA-H99GMN-NAME     TO  SPA-H01GMN-NAME 
      *
           PERFORM 210-BACK
           .
       450-KAKUTEI-EXT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "H99    "           TO  MCP-WINDOW.
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
           INITIALIZE                   H99
           MOVE    SPA-H99GMN-PTNUM     TO  H99-PTNUM
           MOVE    SPA-H99GMN-NAME      TO  H99-NAME-VALUE
           MOVE    SPA-H99GMN-SRYYM     TO  H99-SRYYM
           MOVE    SPA-H99GMN-SRYKA     TO  H99-SRYKA
           MOVE    SPA-H99GMN-SRYKANAME TO
                                        H99-SRYKANAME-VALUE
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
                   OR      SPA-H99GMN-TNYUGAIKBN (IDX)
                                               =   SPACE
               MOVE    SPA-H99GMN-TRENNUM      (IDX)
                                   TO  H99-TRENNUM      (IDX)
               MOVE    SPA-H99GMN-TNYUGAIKBN   (IDX)
                                   TO  H99-TNYUGAIKBN   (IDX)
               MOVE    SPA-H99GMN-TSRYKA       (IDX)
                                   TO  H99-TSRYKA       (IDX)
               MOVE    SPA-H99GMN-TTEISYUTUSAKI(IDX)
                                   TO  H99-TTEISYUTUSAKI(IDX)
               MOVE    SPA-H99GMN-TRECESYUBETU (IDX)
                                   TO  H99-TRECESYUBETU (IDX)
               MOVE    SPA-H99GMN-THKNJANUM    (IDX)
                                   TO  H99-THKNJANUM    (IDX)
               MOVE    SPA-H99GMN-THOJOKBN     (IDX)
                                   TO  H99-THOJOKBN     (IDX)
               MOVE    SPA-H99GMN-TKIGO        (IDX)
                                   TO  H99-TKIGO        (IDX)
               MOVE    IDX         TO  H99-COUNT
           END-PERFORM
      ***  MOVE    "T"              TO  H99-SELECT (1)
      *            
           MOVE    "blue"              TO  H99-NAME-STYLE
                                           H99-SRYKANAME-STYLE
      *            
           IF      SPA-SYS-1001-HOSPSBT1   =   0
               MOVE    WIDGET-INSENSITIVE  TO  H99-SRYKA-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  H99-SRYKA-STATE
           END-IF  
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
         EVALUATE    SPA-H99GMN-CUR
             WHEN    1
                    MOVE  "SRYYM"        TO  MCP-WIDGET
             WHEN    2
                    MOVE  "PTNUM"        TO  MCP-WIDGET
             WHEN    3
                    MOVE  "SRYKA"        TO  MCP-WIDGET
             WHEN    4
                    MOVE  "SELNUM"       TO  MCP-WIDGET
             WHEN    12
                    MOVE  "B12"          TO  MCP-WIDGET   
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
                   MOVE    "入力エラーです"  TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "年月の入力エラーです"
                                             TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "対象のレセプトは存在しません"
                                             TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "該当の患者が存在しません"
                                               TO  SPA-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE                TO  HERR
           MOVE    SPA-ERRCD            TO  HERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  HERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "H99"                TO  SPA-MOTOPG
           MOVE    "HERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-SYSKANRI-INV-EXT.
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
      *    請求マスター読込（主キー）
      *****************************************************************
       900-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SEIKYU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SEIKYU-KEY"        TO  ORC-DBPATH
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SEIKYU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-H01NAI-PTID     TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************`PN@`PN@ ******
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
