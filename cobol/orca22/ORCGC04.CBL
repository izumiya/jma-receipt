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
       PROGRAM-ID.             ORCGC04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 自院病名検索（Ｃ０４）
      *  管理者            : 
      *  01/12/11    MCC−藤原   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/05/08  半角チェック追加
      *  01.00.02    MCC-多々納   02/05/08  半角チェック時、半角空白を
      *                                     全角空白へ置換えの追加
      *  01.00.03    MCC-藤原     02/05/17  コラムリスト700に変更
      *
      *  01.01.01    NACL-藤原    02/08/23  新傷病名マスタ対応
      *  01.01.02    NACL-藤原    02/11/18  分類名の選択番号のチェック修正
      *
      *  01.02.01    NACL-藤原    03/08/28  登録数の制限をなくす
      *                                     病名種別の追加
      *  01.02.02    NACL-藤原    04/02/25  分類毎の表示の前次頁の処理
      *  ver.2.6.0
      *  01.02.03    NACL-藤原    05/04/20  移行病名警告メッセージの表示、
      *                                     廃止年月日警告メッセージ表示
      *                                     をシス管の設定より行う
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    06/03/13  病名検索時の修正
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
       WORKING-STORAGE             SECTION.
      *   
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).   
           03  FLG-ROW             PIC 9(01).  
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXX                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM             PIC ZZ9.
           03  WRK-SELNO           PIC 9(03). 
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(07).
               05  WRK-WIDGET2     PIC 9(01).  
           05  WRK-BYOMEI          PIC X(400).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-STR             PIC 9(05).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *     
           03  WRK-HAISIYMDH       PIC X(22).
      * 
           03  WRK-SYOKBN          PIC 9(01).
           03  WRK-BANGO           PIC S9(05).
           03  WRK-SEIGYO          PIC X(01). 
      * 
           03  WRK-C04-BYO-ROW     PIC S9(09). 
      *
           03  WRK-CIDMSG          PIC X(80).
      *
       01  WRK-TBL-MAX             PIC 9(03)   VALUE   100.
      *
      *    未コード化傷病名コード
       01  WRK-CONS-BYOMEICD       PIC X(07)   VALUE   "0000999". 
      *
      *    患者病名マスタ−（ワーク）
       01  WRK-BYOMEI-AREA.
           03  WRK-BYOMEI-O        PIC X(100)  OCCURS  6.
      *
       01  WRK-HEN-AREA.
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    自院病名マスタ−
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "C02.INC".
           COPY    "C03.INC".
           COPY    "C04.INC".
           COPY    "C05.INC".
           COPY    "C06.INC".
           COPY    "C07.INC".
           COPY    "C08.INC".
           COPY    "C09.INC".
           COPY    "C10.INC".
           COPY    "C11.INC".
           COPY    "CERR.INC".
           COPY    "CID1.INC".
           COPY    "CID2.INC".
           COPY    "CID3.INC".
           COPY    "CID4.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
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
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "CERR"
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
           MOVE    "C04    "           TO  MCP-WINDOW.
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
      *    病名移行確認画面より
           IF      SPA-MOTOPG(1:4)     =   "CID2"
               IF      SPA-CID2-FLG    =   "OK"
                   MOVE    SPA-C04-IKOSAKI-INDEX
                                               TO  IDZ
                   MOVE    SPA-C04-IKOSAKI-INDEX1
                                               TO  IDX2
                   MOVE    SPA-C04-IKOSAKI-BYOMEICD
                                               TO
                                       SPA-NAI-C04-BYOMEICD-O (IDZ IDX2)
                   MOVE     SPA-C04-IKOSAKI-BYOMEI TO
                                       SPA-NAI-C04-BYOMEI-O   (IDZ IDX2)
                   MOVE    SPA-NAI-C04-BYOMEI-O (IDZ 1)
                                               TO
                                           SPA-NAI-C04-BYOMEI (IDZ)
                   PERFORM VARYING IDX2    FROM    2   BY  1
                           UNTIL   IDX2    >       21
                           OR      SPA-NAI-C04-BYOMEICD-O (IDZ IDX2)
                                               =   SPACE
                       STRING   SPA-NAI-C04-BYOMEI (IDZ)
                                           DELIMITED  BY  SPACE
                                SPA-NAI-C04-BYOMEI-O (IDZ IDX2)
                                           DELIMITED  BY  SPACE
                                    INTO    SPA-NAI-C04-BYOMEI (IDZ)
                       END-STRING
                   END-PERFORM        
               END-IF
               MOVE    SPACE           TO  SPA-CIDCD2
                                           SPA-ERRCD
                                           WRK-HAISIYMDH
               PERFORM 410022-BYOMEI-HENSYU-SEC
               ADD     1                   TO  SPA-GMN-C04-CUR
               IF      SPA-GMN-C04-CUR     >   8
                   MOVE    2                   TO  SPA-GMN-C04-CUR
               END-IF
               GO      TO      300-SCREEN-EXT
           END-IF
      *
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    検索画面スパ編集処理     
           INITIALIZE              SPA-C04
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
                   OR      SPA-C01-TBYOMEICD(IDX)  =   SPACE
               MOVE    SPA-C01-TBYOMEICD(IDX)
                                       TO  SPA-GMN-C04-INPUTCD(IDX)
      *********MOVE    SPA-C01-TBYOMEI  (IDX)
      *********                        TO  SPA-NAI-C04-BYOMEI (IDX)
      *
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
               MOVE    SPA-GMN-C04-INPUTCD(IDX)
                                           TO  USBYO-BYOMEIINPUTCD
               PERFORM 900-USBYO-INV-SEC                            
               IF      FLG-USERBYOMEI      =   ZERO
                   MOVE    USBYO-BYOMEI        TO
                                            SPA-NAI-C04-BYOMEI   (IDX)
                   MOVE    USBYO-BYOMEISBT     TO
                                           SPA-NAI-C04-BYOMEISBT (IDX)
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       21
                           OR      USBYO-BYOMEICD (IDY)    =   SPACE
                       MOVE    USBYO-BYOMEICD (IDY)    TO
                                       SPA-NAI-C04-BYOMEICD-O (IDX IDY)
                       IF      SPA-NAI-C04-BYOMEICD-O (IDX IDY)
                                       =   WRK-CONS-BYOMEICD
                           MOVE    SPA-NAI-C04-BYOMEI   (IDX)
                                           TO
                                       SPA-NAI-C04-BYOMEI-O (IDX IDY)
                       ELSE                           
                           MOVE    SPA-NAI-C04-BYOMEICD-O (IDX IDY)
                                           TO   BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI        =   ZERO
                               MOVE    BYO-BYOMEI
                                           TO
                                   SPA-NAI-C04-BYOMEI-O    (IDX IDY)
                           END-IF 
                       END-IF    
                   END-PERFORM   
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               PERFORM 3101-BYOMEITOP-HENSYU-SEC
           END-PERFORM
      *
      *    病名編集 
           PERFORM 3100-BYOMEI-HENSYU-SEC
      *
      *    分類一覧
           PERFORM 3102-BUNRUI-LST-SEC
      *
      *    自院病名一覧
           MOVE    1                   TO  SPA-GMN-C04-PAGE
      *
      *    分類が選択されている場合
           IF      SPA-C01-BUNRUICD    >   ZERO
               MOVE    SPA-C01-BUNRUICD    TO  C04-BUNRUICD
               PERFORM 41001-BUNRUICD-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-GMN-C04-BUNRUICD
               END-IF
           END-IF
           IF      SPA-GMN-C04-BUNRUICD    =   ZERO
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
               PERFORM 3101-USERBYOMEI-LST-SEC
               MOVE    9                   TO  SPA-GMN-C04-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       3100-BYOMEI-HENSYU-SEC      SECTION.
      *
      *    病名コード数
           MOVE    ZERO            TO  SPA-NAI-C04-BYOMEICD-MAX
                                       SPA-NAI-C04-BYOMEISU-MAX
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               IF      SPA-GMN-C04-INPUTCD (IDX)   NOT =   SPACE
                   ADD     1               TO  SPA-NAI-C04-BYOMEICD-MAX
                   IF      SPA-NAI-C04-BYOMEITOP (IDX) =   "1"
                       ADD     1           TO  SPA-NAI-C04-BYOMEISU-MAX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    病名編集
           MOVE    SPACE                   TO  WRK-BYOMEI-AREA
           MOVE    ZERO                    TO  IDY
      *
           MOVE    SPA-NAI-C04-BYOMEI (1)  TO  WRK-BYOMEI
      *     
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C04-INPUTCD (IDX)
                                   NOT =   SPACE
                   IF      SPA-NAI-C04-BYOMEITOP (IDX) =   "1"
                       IF      WRK-BYOMEI  NOT =   SPACE
                           ADD     1           TO  IDY
                           MOVE    WRK-BYOMEI  TO  WRK-BYOMEI-O (IDY)
                       END-IF
                       MOVE    SPACE           TO  WRK-BYOMEI
                   END-IF
                   STRING  WRK-BYOMEI              DELIMITED   BY  SPACE
                           SPA-NAI-C04-BYOMEI(IDX) DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                   END-STRING
               END-IF    
           END-PERFORM    
           IF      WRK-BYOMEI      NOT =   SPACE
               ADD     1               TO  IDY
               MOVE    WRK-BYOMEI      TO  WRK-BYOMEI-O (IDY)
           END-IF
      *
           MOVE   WRK-BYOMEI-O (1)     TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
                   OR      WRK-BYOMEI-O (IDX)  =   SPACE     
               STRING  WRK-BYOMEI      DELIMITED  BY  SPACE
                   "／"                DELIMITED  BY  SIZE
                   WRK-BYOMEI-O (IDX)  DELIMITED  BY  SPACE
                                 INTO             WRK-BYOMEI
               END-STRING
           END-PERFORM
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C04-BYOMEI
           .
       3100-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数病名の先頭フラグ編集処理
      *****************************************************************
       3101-BYOMEITOP-HENSYU-SEC      SECTION.
      *
           IF      SPA-GMN-C04-INPUTCD (IDX) 
                                     =   SPACE
               MOVE    SPACE       TO  SPA-NAI-C04-BYOMEITOP(IDX)
               GO  TO  3101-BYOMEITOP-HENSYU-EXT
           END-IF
      *
           IF      IDX               =   1  
               MOVE    "1"               TO  SPA-NAI-C04-BYOMEITOP(IDX)
               GO  TO  3101-BYOMEITOP-HENSYU-EXT
           END-IF
      *
      *   自院病名の部位が入力されている場合は複数病名にはしない
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       6
               IF      SPA-NAI-C04-BYOMEISBT(IDW)
                                           =   "1"
                   MOVE    SPACE       TO  SPA-NAI-C04-BYOMEITOP(IDX)
                   GO  TO  3101-BYOMEITOP-HENSYU-EXT
               END-IF
           END-PERFORM
      *
           COMPUTE IDW     =   IDX     -   1
           EVALUATE    SPA-NAI-C04-BYOMEISBT(IDX)
               WHEN    "8"
                   MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
               WHEN    "7"
               WHEN    "0"
                   EVALUATE    SPA-NAI-C04-BYOMEISBT (IDW)
                       WHEN    "7"
                       WHEN    "1"
                           MOVE    SPACE       TO
                                       SPA-NAI-C04-BYOMEITOP(IDX)
                       WHEN    "8"
                       WHEN    "0"
                           MOVE    "1"         TO
                                       SPA-NAI-C04-BYOMEITOP(IDX)
                   END-EVALUATE
               WHEN    "1"
                   MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
           END-EVALUATE
           .
       3101-BYOMEITOP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類一覧 編集処理
      *****************************************************************
       3102-BUNRUI-LST-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
      *
      *    分類コード ’Ｃ’のもの
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           PERFORM UNTIL       (FLG-USERBYOMEI   =   1    )  OR
                               (IDX              >=  100  )
      *
               ADD     1                   TO  IDX
               IF      USBYO-BYOMEIINPUTCD NOT =   SPACE
                   MOVE    USBYO-BYOMEIINPUTCD(1:3)
                                       TO  SPA-GMN-C04-BBUNRUICD(IDX)
               END-IF                            
               MOVE    USBYO-BYOMEI    TO  SPA-GMN-C04-BBUNRUIMEI(IDX)
      *
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-GMN-C04-BUNRUI-MAX
           .
       3102-BUNRUI-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名一覧 編集処理
      *****************************************************************
       3101-USERBYOMEI-LST-SEC         SECTION.
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           COMPUTE WRK-STR         = ( SPA-GMN-C04-PAGE  -  1 )
                                   *   WRK-TBL-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
           PERFORM UNTIL ( IDX             >=  WRK-TBL-MAX ) OR
                         ( FLG-USERBYOMEI  =   1                )
               ADD     1                   TO  IDX2
               IF      IDX2                    >=  WRK-STR
                   ADD     1               TO  IDX
                   IF      IDX             <=  WRK-TBL-MAX
                       MOVE    IDX2        TO  SPA-GMN-C04-TBANGO  (IDX)
                       MOVE    USBYO-BYOMEIINPUTCD
                                           TO  SPA-GMN-C04-TINPUTCD(IDX)
                       MOVE    USBYO-BYOMEI
                                           TO  SPA-GMN-C04-TBYOMEI (IDX)
                       MOVE    USBYO-BYOMEISBT
                                           TO
                                           SPA-NAI-C04-TBYOMEISBT (IDX)
                       MOVE    IDX         TO  SPA-GMN-C04-MAX
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-TBL-MAX )  AND
                 ( FLG-USERBYOMEI      =   ZERO             )
               MOVE    1                   TO  SPA-GMN-C04-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C04-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROW 
           .
       3101-USERBYOMEI-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE           TO  SPA-C01KYOUTU 
                   MOVE    SPA-C01KYOUTU   TO  SPA-WORK
                   PERFORM 210-BACK
      *    全分類検索
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 450-BUNRUI-HENSYU-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *    確定
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
               WHEN    "ACTIVATE"      ALSO    "BUNRUICD"
                   PERFORM 41001-BUNRUICD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM2"
                   PERFORM 41002-SELNUM2-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "INPUTCD1"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD2"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD3"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD4"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD5"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD6"
                   PERFORM 41003-INPUTCD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "KENBYOMEI"
                   PERFORM 41004-KENBYOMEI-CHK-SEC
      *        病名選択
               WHEN    ANY             ALSO    "BYOMEILST"
                   PERFORM 41020-BYOMEISEL-SEC
      *        分類選択
               WHEN    ANY             ALSO    "BUNRUILST"
                   PERFORM 41021-BUNRUILST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類名選択処理
      *****************************************************************
       41001-BUNRUICD-CHK-SEC            SECTION.
      *
           MOVE    9                   TO  SPA-GMN-C04-CUR
      *
           MOVE    ZERO                TO  SPA-NAI-C04-BUNRUICD-IDX 
      *
           MOVE    C04-BUNRUICD        TO  SPA-GMN-C04-BUNRUICD
      *
           IF      SPA-GMN-C04-BUNRUICD =   ZERO
               MOVE    2                   TO  SPA-GMN-C04-CUR
           ELSE
               MOVE    ZERO                TO  FLG-CHK  
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-C04-BUNRUI-MAX
                       OR      FLG-CHK     =   1
                   IF      SPA-GMN-C04-BUNRUICD =
                                           SPA-GMN-C04-BBUNRUICD (IDX)
                       MOVE    IDX             TO
                                               SPA-NAI-C04-BUNRUICD-IDX
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO                           
                   MOVE    "0012"          TO  SPA-ERRCD
               ELSE
                   PERFORM 410011-BUNRUICD-SELECT-SEC
               END-IF
           END-IF    
           .
       41001-BUNRUICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択分類コード編集処理
      *****************************************************************
       410011-BUNRUICD-SELECT-SEC      SECTION.
      *                                     
           INITIALIZE                  SPA-GMN-C04-BYOMEI-TBL
                                       SPA-GMN-C04-SELNUM 
                                       SPA-GMN-C04-KENBYOMEI
           MOVE    ZERO                TO  SPA-GMN-C04-MAX
           MOVE    1                   TO  SPA-GMN-C04-PAGE
      *
           MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           MOVE    SPA-GMN-C04-BBUNRUICD 
                           (SPA-NAI-C04-BUNRUICD-IDX)
                                       TO  USBYO-BUNRUICD
           PERFORM 3101-USERBYOMEI-LST-SEC
           MOVE    2                   TO  SPA-GMN-C04-CUR
           .
       410011-BUNRUICD-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名番号選択処理
      *****************************************************************
       41002-SELNUM2-CHK-SEC            SECTION.
      *                                     
           MOVE    2                   TO  SPA-GMN-C04-CUR
      *
      *    病名コードが６つのとき
           IF      SPA-NAI-C04-BYOMEICD-MAX
                                       =   6
               MOVE    99                  TO  SPA-GMN-C04-CUR
               MOVE    "0013"              TO  SPA-ERRCD
               GO  TO  41002-SELNUM2-CHK-EXT
           END-IF
      *
           MOVE    C04-SELNUM2         TO  SPA-GMN-C04-SELNUM
      *
           IF      SPA-GMN-C04-SELNUM  >   ZERO
               COMPUTE WRK-BANGO       =   SPA-GMN-C04-SELNUM
                                       - ( SPA-GMN-C04-PAGE    -   1 )
                                           *   WRK-TBL-MAX
               IF    ( WRK-BANGO           >   SPA-GMN-C04-MAX )
               OR    ( WRK-BANGO           <   1               )
                   MOVE    "0012"              TO  SPA-ERRCD
               ELSE
                   MOVE    ZERO            TO  IDZ
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       6
                       IF      SPA-GMN-C04-INPUTCD (IDXX)  =   SPACE
                           MOVE    SPA-GMN-C04-TINPUTCD   (WRK-BANGO)
                                           TO  SPA-GMN-C04-INPUTCD(IDXX)
                           MOVE    IDXX    TO  IDZ
                           PERFORM 410021-INPUTCD-HENSYU-SEC
      *
                           MOVE    6       TO  IDXX
                       END-IF
                   END-PERFORM
               END-IF    
           END-IF
      *
           IF      SPA-CIDCD2      NOT =   SPACE
               GO  TO  41002-SELNUM2-CHK-EXT
           END-IF
      *      
           PERFORM 410022-BYOMEI-HENSYU-SEC 
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ZERO                TO  SPA-GMN-C04-SELNUM
               MOVE    9                   TO  SPA-GMN-C04-CUR
           END-IF
           .
       41002-SELNUM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理
      *****************************************************************
       410021-INPUTCD-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
      *
           INITIALIZE                      SPA-NAI-C04-BYOMEI-G (IDZ)
      *
           IF      SPA-GMN-C04-INPUTCD (IDZ)
                                   NOT =   SPACE
               INITIALIZE                      USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
               MOVE    SPA-GMN-C04-INPUTCD (IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
               PERFORM 900-USBYO-INV-SEC                            
               IF      FLG-USERBYOMEI      =   1
                   MOVE    "0004"          TO  SPA-ERRCD    
               ELSE
                   MOVE    USBYO-BYOMEI    TO  SPA-NAI-C04-BYOMEI (IDZ)
                   MOVE    USBYO-BYOMEISBT TO
                                           SPA-NAI-C04-BYOMEISBT (IDZ)
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       21
                           OR      USBYO-BYOMEICD (IDY)    =   SPACE
                       MOVE    USBYO-BYOMEICD (IDY)    TO
                                       SPA-NAI-C04-BYOMEICD-O (IDZ IDY)
                       IF      SPA-NAI-C04-BYOMEICD-O (IDZ IDY)
                                       =   WRK-CONS-BYOMEICD
                           MOVE    SPA-NAI-C04-BYOMEI   (IDZ)
                                           TO
                                       SPA-NAI-C04-BYOMEI-O (IDZ IDY)
                       ELSE                           
                           MOVE    SPA-NAI-C04-BYOMEICD-O (IDZ IDY)
                                           TO   BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI        =   ZERO
                               MOVE    BYO-BYOMEI
                                           TO
                                   SPA-NAI-C04-BYOMEI-O    (IDZ IDY)
                           END-IF 
                       END-IF    
                   END-PERFORM   
      *            廃止年月日・移行先がある病名のチェック
                   IF      USBYO-BYOMEIHENFLG      =   SPACE        
                       PERFORM 4100211-HAISIYMD-CHECK-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       410021-INPUTCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       410022-BYOMEI-HENSYU-SEC      SECTION.
      * 
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               PERFORM 3101-BYOMEITOP-HENSYU-SEC
           END-PERFORM
      *                                     
           PERFORM 3100-BYOMEI-HENSYU-SEC
      *
      *    変更処理のときの複数病名入力
           IF    ( SPA-NAI-C04-BYOMEISU-MAX
                                       >   1    )
           AND   ( SPA-GMN-C02-BANGO   >   ZERO )               
               MOVE    99                  TO  SPA-GMN-C04-CUR
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-HAISIYMDH       =   SPACE )
               OR    ( SPA-C02-1042-MSG3-DSPKBN
                                           =   "1"   )
                   CONTINUE
               ELSE
                   MOVE    "0075"              TO  SPA-ERRCD
               END-IF                     
           END-IF
      *
           .
       410022-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    廃止年月日チェック処理
      *****************************************************************
       4100211-HAISIYMD-CHECK-SEC      SECTION.
      * 
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       21
                   OR      USBYO-BYOMEICD (IDX1)   =   SPACE
               IF      USBYO-BYOMEICD (IDX1) (1:1)     =   "Z"
                   CONTINUE
               ELSE
                   MOVE    USBYO-BYOMEICD (IDX1)
                                           TO  BYO-BYOMEICD
                   MOVE    ZERO            TO  FLG-BYOMEI
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI          =   ZERO
                       IF    ( BYO-IKOSAKICD   NOT =   SPACE )
                       AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                   =   ZERO  )
                           MOVE     USBYO-BYOMEICD (IDX1)
                                           TO  SPA-C04-IKOMOTO-BYOMEICD
                           MOVE    BYO-BYOMEI 
                                           TO  SPA-C04-IKOMOTO-BYOMEI
                           MOVE    BYO-IKOSAKICD
                                           TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI          =   ZERO
                               MOVE    BYO-BYOMEICD    TO
                                               SPA-C04-IKOSAKI-BYOMEICD
                               MOVE    BYO-BYOMEI      TO
                                               SPA-C04-IKOSAKI-BYOMEI
                               MOVE    IDZ             TO
                                               SPA-C04-IKOSAKI-INDEX                    
                               MOVE    IDX1            TO
                                               SPA-C04-IKOSAKI-INDEX1                    
                               MOVE    "0104"          TO
                                               SPA-CIDCD2
                               GO  TO  4100211-HAISIYMD-CHECK-EXT
                           END-IF
                       ELSE        
                           IF      BYO-HAISIYMD    =   ALL "9" OR  SPACE
                               CONTINUE
                           ELSE
                               MOVE    BYO-HAISIYMD    TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3
                                                       TO  WRK-HAISIYMDH
                           END-IF
                       END-IF
                   END-IF
                   MOVE    21                  TO  IDX1
               END-IF    
           END-PERFORM        
           .
        4100211-HAISIYMD-CHECK-EXT.
           EXIT.         
      *
      *****************************************************************
      *    自院病名コードチェック処理
      *****************************************************************
       41003-INPUTCD-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-HAISIYMDH
                                       WRK-SEIGYO
      *
           MOVE    MCP-WIDGET      TO  WRK-WIDGET
           MOVE    WRK-WIDGET2     TO  IDZ
      *
           COMPUTE SPA-GMN-C04-CUR =   2   +   IDZ
      *
      *    コードの挿入・削除の制御文字取得
           IF      C04-INPUTCD (IDZ)  NOT =   SPACE
               PERFORM VARYING IDX FROM    10  BY  -1
                       UNTIL   IDX <       2
                   IF      C04-INPUTCD (IDZ)(IDX:1)
                                       NOT =   SPACE
                       EVALUATE    C04-INPUTCD (IDZ)(IDX:1)
                           WHEN    "-"
                               MOVE    "-"         TO  WRK-SEIGYO
                           WHEN    "+"
                               MOVE    "+"         TO  WRK-SEIGYO
                           WHEN    OTHER
                               IF      IDX             >   2
                                   COMPUTE IDW     =   IDX     -   1
                                   IF      C04-INPUTCD (IDZ)(IDW:2)
                                                   =   "−"
                                       MOVE    "-"     TO  WRK-SEIGYO
                                   END-IF
                                   IF      C04-INPUTCD (IDZ)(IDW:2)
                                                   =   "＋"
                                       MOVE    "+"     TO  WRK-SEIGYO
                                   END-IF
                               END-IF
                       END-EVALUATE
      *                追加のとき
                       IF      WRK-SEIGYO      =   "+"
                           IF      SPA-GMN-C04-INPUTCD (6)
                                                   NOT =   SPACE
                               MOVE    SPACE           TO  WRK-SEIGYO
                           END-IF 
                           IF      IDZ         >   1
                               COMPUTE IDW     =   IDZ     -   1
                               IF      SPA-GMN-C04-INPUTCD (IDW)
                                                           =   SPACE 
                                   MOVE    SPACE       TO  WRK-SEIGYO
                               END-IF
                           END-IF
                           IF      WRK-SEIGYO      =   SPACE
                               COMPUTE SPA-GMN-C04-CUR =   2   +   IDZ
                               GO  TO  41003-INPUTCD-CHK-EXT
                           END-IF
                       END-IF
                       MOVE    2               TO  IDX
                   END-IF 
               END-PERFORM
           ELSE
               MOVE    "-"                     TO  WRK-SEIGYO
           END-IF
      *       
           EVALUATE    WRK-SEIGYO
               WHEN    "-"
                   PERFORM VARYING IDX FROM    IDZ BY  1
                           UNTIL   IDX >       6
                       IF      IDX             =   6
                           INITIALIZE  SPA-NAI-C04-BYOMEI-G (IDX)
                                       SPA-GMN-C04-INPUTCD  (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     +   1
                           MOVE    SPA-NAI-C04-BYOMEI-G (IDW)
                                   TO  SPA-NAI-C04-BYOMEI-G (IDX)
                           MOVE    SPA-GMN-C04-INPUTCD   (IDW)
                                   TO  SPA-GMN-C04-INPUTCD  (IDX)
                       END-IF                     
                   END-PERFORM                           
                   PERFORM 410022-BYOMEI-HENSYU-SEC 
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  41003-INPUTCD-CHK-EXT
               WHEN    "+"
                   PERFORM VARYING IDX FROM    6   BY  -1
                           UNTIL   IDX <       IDZ
                       IF      IDX             =   IDZ
                           INITIALIZE  SPA-NAI-C04-BYOMEI-G(IDX)
                                       SPA-GMN-C04-INPUTCD (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     -   1
                           MOVE    SPA-NAI-C04-BYOMEI-G (IDW)
                                   TO  SPA-NAI-C04-BYOMEI-G(IDX)
                           MOVE    SPA-GMN-C04-INPUTCD   (IDW)
                                   TO  SPA-GMN-C04-INPUTCD  (IDX)
                       END-IF                     
                   END-PERFORM
                   PERFORM 410022-BYOMEI-HENSYU-SEC 
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  41003-INPUTCD-CHK-EXT
           END-EVALUATE                               
      *
           INITIALIZE              SPA-NAI-C04-BYOMEI-G (IDZ)
      *
      *    半角チェック
           IF      C04-INPUTCD (IDZ)   =   SPACE
               CONTINUE
           ELSE    
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    C04-INPUTCD (IDZ)   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-RC2         =   ZERO
                   MOVE    KANACHK-OUT-INPUT   TO  C04-INPUTCD (IDZ)
               ELSE
                   MOVE    "0072"              TO  SPA-ERRCD
                   GO  TO  41003-INPUTCD-CHK-EXT
               END-IF
           END-IF    
      *
           MOVE    C04-INPUTCD (IDZ)   TO  SPA-GMN-C04-INPUTCD(IDZ)
      *
           PERFORM 410021-INPUTCD-HENSYU-SEC
           IF      SPA-CIDCD2      NOT =   SPACE
               GO  TO  41003-INPUTCD-CHK-EXT
           END-IF     
      *
           PERFORM 410022-BYOMEI-HENSYU-SEC 
      *
           IF      SPA-ERRCD           =   SPACE   OR  "0075"
               ADD     1                   TO  SPA-GMN-C04-CUR
               IF      SPA-GMN-C04-CUR     >   8
                   MOVE    2                   TO  SPA-GMN-C04-CUR
               END-IF
           END-IF
           .
       41003-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索病名チェック処理
      *****************************************************************
       41004-KENBYOMEI-CHK-SEC          SECTION.
      *
           MOVE    1                  TO  SPA-GMN-C04-CUR
      *         
           MOVE    C04-KENBYOMEI      TO  SPA-GMN-C04-KENBYOMEI
      *
           IF      SPA-GMN-C04-KENBYOMEI  =   SPACE
               CONTINUE
           ELSE
      *        半角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C04-KENBYOMEI
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-C04-KENBYOMEI
               ELSE
                   MOVE    "0071"              TO  SPA-ERRCD
                   GO  TO  41004-KENBYOMEI-CHK-EXT
               END-IF
      *
               INITIALIZE                  SPA-GMN-C04-BYOMEI-TBL
                                           SPA-GMN-C04-SELNUM 
               MOVE    ZERO                TO  SPA-GMN-C04-MAX
                                               SPA-NAI-C04-BUNRUICD-IDX
               MOVE    1                   TO  SPA-GMN-C04-PAGE
      *
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key8"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
               STRING  "%"                 DELIMITED  BY  SIZE
                       SPA-GMN-C04-KENBYOMEI
                                           DELIMITED  BY  SPACE
                       "%"                 DELIMITED  BY  SIZE
                                           INTO    USBYO-BYOMEI
               END-STRING
               PERFORM 3101-USERBYOMEI-LST-SEC
           END-IF    
      *
           MOVE    2                  TO  SPA-GMN-C04-CUR
           .
       41004-KENBYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名行選択処理
      *****************************************************************
       41020-BYOMEISEL-SEC       SECTION.
      *
      *    病名コードが６つのとき
           IF      SPA-NAI-C04-BYOMEICD-MAX
                                       =   6
               MOVE    99                  TO  SPA-GMN-C04-CUR
               MOVE    "0013"              TO  SPA-ERRCD
               GO  TO  41020-BYOMEISEL-EXT
           END-IF
      * 
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C04-MAX
               IF      C04-BYOMEILST-SELECT (IDX)    =   "T"
                   MOVE    IDX                  TO  WRK-SELNO
                   MOVE    SPA-GMN-C04-MAX      TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    WRK-SELNO           TO  WRK-BANGO
      * 
           MOVE    ZERO            TO  IDZ
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6
               IF      SPA-GMN-C04-INPUTCD (IDXX)  =   SPACE
                   MOVE    SPA-GMN-C04-TINPUTCD   (WRK-BANGO)
                                       TO  SPA-GMN-C04-INPUTCD(IDXX)
                   MOVE    IDXX        TO  IDZ
                   PERFORM 410021-INPUTCD-HENSYU-SEC
      *
                   MOVE    6       TO  IDXX
               END-IF
           END-PERFORM
           PERFORM 410022-BYOMEI-HENSYU-SEC 
      *
           MOVE    2                   TO  SPA-GMN-C04-CUR     
           .
       41020-BYOMEISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類名行選択処理
      *****************************************************************
       41021-BUNRUILST-SEC       SECTION.
      *          
      *    行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C04-BUNRUI-MAX
               IF      C04-BUNRUI-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-C04-BUNRUICD-IDX
                       CONTINUE
                   ELSE        
                       MOVE    IDX                     TO  WRK-SELNO
                       MOVE    SPA-GMN-C04-BUNRUI-MAX  TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-C04-BUNRUICD-IDX
               MOVE    SPA-GMN-C04-BBUNRUICD (SPA-NAI-C04-BUNRUICD-IDX) 
                                           TO SPA-GMN-C04-BUNRUICD
           END-IF
      *
           PERFORM 410011-BUNRUICD-SELECT-SEC
           .
       41021-BUNRUISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "C04"               TO  SPA-MOTOPG
      *
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
      *    全分類編集処理
      *****************************************************************
       450-BUNRUI-HENSYU-SEC           SECTION.
      *
           INITIALIZE                      SPA-GMN-C04-BYOMEI-TBL
                                           SPA-NAI-C04-BYOMEI-TBL
                                           SPA-GMN-C04-KENBYOMEI
           MOVE    ZERO                TO  SPA-GMN-C04-MAX
                                           SPA-GMN-C04-BUNRUICD
                                           SPA-NAI-C04-BUNRUICD-IDX
                                           SPA-GMN-C04-SELNUM
      *    検索処理
           MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
           MOVE    "key3"              TO  WRK-MCP-PATHNAME
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           MOVE    9                   TO  SPA-GMN-C04-CUR     
           .
       450-BUNRUI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C04-PAGE        =   1
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-C04-PAGE    =   SPA-GMN-C04-PAGE    -   1
      *
           INITIALIZE                      SPA-GMN-C04-BYOMEI-TBL
                                           SPA-NAI-C04-BYOMEI-TBL
                                           SPA-GMN-C04-KENBYOMEI
           MOVE    ZERO                TO  SPA-GMN-C04-MAX
      *****                                SPA-GMN-C04-BUNRUICD
      *****                                SPA-NAI-C04-BUNRUICD-IDX
                                           SPA-GMN-C04-SELNUM
      *    検索処理
           IF      SPA-GMN-C04-BUNRUICD    =   ZERO
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           ELSE
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
               MOVE    SPA-GMN-C04-BUNRUICD 
                                       TO  USBYO-BUNRUICD
           END-IF
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           MOVE    2                   TO  SPA-GMN-C04-CUR     
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C04-NEXT    =   ZERO
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C04-PAGE
      *
           INITIALIZE                      SPA-GMN-C04-BYOMEI-TBL
                                           SPA-NAI-C04-BYOMEI-TBL
                                           SPA-GMN-C04-KENBYOMEI
           MOVE    ZERO                TO  SPA-GMN-C04-MAX
      *****                                SPA-GMN-C04-BUNRUICD
      *****                                SPA-NAI-C04-BUNRUICD-IDX
                                           SPA-GMN-C04-SELNUM
      *    検索処理
           IF      SPA-GMN-C04-BUNRUICD    =   ZERO
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           ELSE
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
               MOVE    SPA-GMN-C04-BUNRUICD 
                                       TO  USBYO-BUNRUICD
           END-IF
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           MOVE    2                   TO  SPA-GMN-C04-CUR     
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       450-KAKUTEI-SEC             SECTION.
      *
      *    変更処理のときの複数病名入力
           IF    ( SPA-NAI-C04-BYOMEISU-MAX
                                       >   1    )
           AND   ( SPA-GMN-C02-BANGO   >   ZERO )               
               MOVE    99                  TO  SPA-GMN-C04-CUR
               MOVE    "0003"              TO  SPA-ERRCD
               GO  TO  450-KAKUTEI-EXT
           END-IF
      *
      *    病名重複チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
                   OR      WRK-BYOMEI-O (IDX)  =   SPACE
               PERFORM VARYING IDY FROM    IDX  BY  1
                       UNTIL   IDY >       6
                       OR      WRK-BYOMEI-O (IDY)  =   SPACE
                   IF      IDY             >   IDX
                       IF      WRK-BYOMEI-O (IDX)  =
                                               WRK-BYOMEI-O (IDY)
                           MOVE    "0002"              TO  SPA-ERRCD
                           GO  TO  450-KAKUTEI-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM        
      *
           MOVE    ZERO            TO  IDY
      *     
           INITIALIZE                  SPA-C01KYOUTU 
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               IF      SPA-GMN-C04-INPUTCD (IDX)   NOT =   SPACE
                   ADD     1           TO  IDY    
                   MOVE    SPA-GMN-C04-INPUTCD  (IDX)
                                       TO  SPA-C01-TBYOMEICD (IDY)
               END-IF 
           END-PERFORM
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
      *
           PERFORM 210-BACK
           .
       450-KAKUTEI-EXT.
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
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD2-G  TO  WRK-NENGO
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
           IF      SPA-CIDCD2      NOT =   SPACE
               PERFORM 530-CIDSET2-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "C04    "           TO  MCP-WINDOW
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
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-C04-BYO-ROW
           ELSE
               MOVE    C04-BYOMEILST-ROW   TO  WRK-C04-BYO-ROW
           END-IF
      * 
           MOVE    SPACE               TO  C04
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C04-MAX
               MOVE    SPA-GMN-C04-TBANGO    (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C04-TBANGO    (IDX)
               MOVE    SPA-GMN-C04-TINPUTCD(IDX)
                                           TO  C04-TBYOMEIINPUTCD(IDX)
               MOVE    SPA-GMN-C04-TBYOMEI    (IDX)
                                           TO  C04-TBYOMEI   (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C04-MAX     TO  C04-BYOMEILST-COUNT
           MOVE    ZERO                TO  C04-BYOMEILST-ROW
           IF      WRK-C04-BYO-ROW     >   ZERO
               MOVE    WRK-C04-BYO-ROW     TO  C04-BYOMEILST-ROW
           END-IF
      *
           MOVE    SPA-GMN-C04-KENBYOMEI   TO  C04-KENBYOMEI
           MOVE    SPA-GMN-C04-BUNRUICD    TO  C04-BUNRUICD
           MOVE    SPA-GMN-C04-SELNUM      TO  C04-SELNUM2
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      6
               MOVE    SPA-GMN-C04-INPUTCD(IDX)   TO  C04-INPUTCD (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C04-BYOMEI      TO  C04-BYOMEI
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C04-BUNRUI-MAX
               MOVE    SPA-GMN-C04-BBUNRUICD (IDX)
                                           TO  C04-BBUNRUICD (IDX)
               MOVE    SPA-GMN-C04-BBUNRUIMEI(IDX)
                                           TO  C04-BBUNRUIMEI(IDX)
      *        
               IF      IDX             =   SPA-NAI-C04-BUNRUICD-IDX
                   MOVE    "T"         TO  C04-BUNRUI-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  C04-BUNRUI-SELECT (IDX)
               END-IF
           END-PERFORM    
           MOVE    SPA-GMN-C04-BUNRUI-MAX  TO  C04-BUNRUI-COUNT
           MOVE    ZERO                    TO  C04-BUNRUI-ROW
               
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
         EVALUATE    SPA-GMN-C04-CUR
             WHEN    1
                     MOVE  "KENBYOMEI"  TO  MCP-WIDGET
             WHEN    2
                     MOVE  "SELNUM2"    TO  MCP-WIDGET
             WHEN    3
                     MOVE  "INPUTCD1"   TO  MCP-WIDGET
             WHEN    4
                     MOVE  "INPUTCD2"   TO  MCP-WIDGET
             WHEN    5
                     MOVE  "INPUTCD3"   TO  MCP-WIDGET
             WHEN    6
                     MOVE  "INPUTCD4"   TO  MCP-WIDGET
             WHEN    7
                     MOVE  "INPUTCD5"   TO  MCP-WIDGET
             WHEN    8
                     MOVE  "INPUTCD6"   TO  MCP-WIDGET
             WHEN    9
                     MOVE  "BUNRUICD"   TO  MCP-WIDGET
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
                   MOVE    "既に同一病名入力コードが存在します" 
                                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE
                       "変更処理中なので病名はひとつしか入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "自院病名マスタが存在しません"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが選択してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "病名入力コード入力エラー" 
                                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "病名が入力されていません"  TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "病名コードの入力でエラーがあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "選択番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "これ以上選択できません"    TO  SPA-ERRMSG
               WHEN    "0071"
                   MOVE    "全角入力して下さい"       TO  SPA-ERRMSG
               WHEN    "0072"
                   MOVE    "全角または半角のみで入力して下さい" 
                                                       TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE    "入力された病名はマスター上で"
                                       TO  SPA-ERRMSG
                   MOVE    WRK-HAISIYMDH
                                       TO  SPA-ERRMSG (29:22)
                   MOVE    "で廃止の扱いとなっています。"
                                       TO  SPA-ERRMSG (51:)
               WHEN    "1003"
                   MOVE    "自院病名マスタが存在しません"
                                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WRK-HAISIYMDH     
      *
           MOVE    SPACE                TO  CERR
           MOVE    SPA-ERRCD            TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  CERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C04"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       530-CIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID2-FLG
      *
           EVALUATE    SPA-CIDCD2
               WHEN    "0104"
                   MOVE    "入力された自院病名の病名はマスタ"
                                       TO  WRK-CIDMSG
                   MOVE    "ー上で移行病名の扱いとなっています。"
                                       TO  WRK-CIDMSG (33:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  CID2
           MOVE    WRK-CIDMSG          TO  CID2-MSG
           MOVE    SPA-C04-IKOMOTO-BYOMEICD
                                       TO  CID2-BYOMEICD1
           MOVE    SPA-C04-IKOMOTO-BYOMEI  
                                       TO  CID2-BYOMEI1
           MOVE    SPA-C04-IKOSAKI-BYOMEICD
                                       TO  CID2-BYOMEICD2
           MOVE    SPA-C04-IKOSAKI-BYOMEI
                                       TO  CID2-BYOMEI2
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C04"               TO  SPA-MOTOPG
           MOVE    "CID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID2"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-CIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込（主キー）
      *****************************************************************
       900-USBYO-INV-SEC         SECTION.
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-USBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込
      *****************************************************************
       900-USBYO-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
               MOVE    ZERO            TO  FLG-USERBYOMEI
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
       900-USBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
