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
       PROGRAM-ID.                 ORCSEIYOUKKS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 栄養管理経過措置入院料コード取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/03/12    NACL−小豆沢  新規作成
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
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSK5000.INC".
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
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    栄養管理経過措置入院料コード取得パラメタ
           COPY    "CPORCSEIYOUKKS.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSEIYOUKKS-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-EIYOUKKS-SANTEIYM    >=   "201204"
      *        システム管理検索（栄養管理経過措置の区分取得）
               MOVE    SPACE               TO  SYS-5000-REC
               INITIALIZE                      SYS-5000-REC
               MOVE    "5000"              TO  SYS-5000-KANRICD
               MOVE    "*"                 TO  SYS-5000-KBNCD
               MOVE    LNK-EIYOUKKS-SANTEIYM
                                           TO  SYS-5000-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-5000-STYUKYMD(7:2)
               MOVE    SYS-5000-STYUKYMD   TO  SYS-5000-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
               MOVE    SYS-5000-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    ZERO                TO  FLG-SYSKANRI
                       MOVE    MCPDATA-REC         TO  SYS-5000-REC
                   ELSE
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               ELSE
                   MOVE    SPACE               TO  SYS-5000-REC
                   INITIALIZE                  SYS-5000-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF    SYS-5000-EIYOUKSNKBN  =   "1"
                   IF    LNK-EIYOUKKS-SRYCD    NOT  =  SPACE
                       PERFORM    200-CDCHG-SEC
                   END-IF
      *
               END-IF
           END-IF
      *
           IF    LNK-EIYOUKKS-CHGSRYCD   =     SPACE
               MOVE    1                 TO     LNK-EIYOUKKS-RC
           END-IF
      *
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    コード置き換え（栄管経過措置）
      *****************************************************************
       200-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-EIYOUKKS-SRYCD
      *        療養病床・有床診療所入院基本料１（１４日以内）
               WHEN    "190153410"
                  MOVE "190153510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床・有床診療所入院基本料１（１５〜３０日）
               WHEN    "190153610"
                  MOVE "190153710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床・有床診療所入院基本料１（３１日以上）
               WHEN    "190153810"
                  MOVE "190153910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床・有床診療所入院基本料２（１４日以内）
               WHEN    "190154010"
                  MOVE "190154110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床・有床診療所入院基本料２（１５〜３０日）
               WHEN    "190154210"
                  MOVE "190154310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床・有床診療所入院基本料２（３１日以上）
               WHEN    "190154410"
                  MOVE "190154510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床・有床診療所入院基本料３（１４日以内）
               WHEN    "190154610"
                  MOVE "190154710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床・有床診療所入院基本料３（１５〜３０日）
               WHEN    "190154810"
                  MOVE "190154910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床・有床診療所入院基本料３（３１日以上）
               WHEN    "190155010"
                  MOVE "190155110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ａ
               WHEN    "190155210"
                  MOVE "190155310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ａ（生活療養）
               WHEN    "190155410"
                  MOVE "190155510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ｂ 
               WHEN    "190155610"
                  MOVE "190155710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ｂ（生活療養）
               WHEN    "190155810"
                  MOVE "190155910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ｃ
               WHEN    "190156010"
                  MOVE "190156110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ｃ（生活療養）
               WHEN    "190156210"
                  MOVE "190156310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ｄ
               WHEN    "190156410"
                  MOVE "190156510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ｄ（生活療養）
               WHEN    "190156610"
                  MOVE "190156710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ｅ
               WHEN    "190156810"
                  MOVE "190156910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病床以外・有床診療所療養病床入院Ｅ（生活療養）
               WHEN    "190157010"
                  MOVE "190157110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ａ）
               WHEN    "190157210"
                  MOVE "190157310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ａ・生活療養）
               WHEN    "190157410"
                  MOVE "190157510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｂ）
               WHEN    "190157610"
                  MOVE "190157710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｂ・生活療養）
               WHEN    "190157810"
                  MOVE "190157910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｃ）
               WHEN    "190158010"
                  MOVE "190158110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｃ・生活療養）
               WHEN    "190158210"
                  MOVE "190158310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｄ）
               WHEN    "190158410"
                  MOVE "190158510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｄ・生活療養）
               WHEN    "190158610"
                  MOVE "190158710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｅ）
               WHEN    "190158810"
                  MOVE "190158910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｅ・生活療養）
               WHEN    "190159010"
                  MOVE "190159110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｆ）
               WHEN    "190159210"
                  MOVE "190159310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｆ・生活療養）
               WHEN    "190159410"
                  MOVE "190159510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｇ）
               WHEN    "190159610"
                  MOVE "190159710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｇ・生活療養）
               WHEN    "190159810"
                  MOVE "190159910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｈ）
               WHEN    "190160010"
                  MOVE "190160110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｈ・生活療養）
               WHEN    "190160210"
                  MOVE "190160310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｉ）
               WHEN    "190160410"
                  MOVE "190160510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟・療養病棟入院基本料１（Ｉ・生活療養）
               WHEN    "190160610"
                  MOVE "190160710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        一般病棟７対１入院基本料
               WHEN    "190117710"
                  MOVE "190700210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        一般病棟１０対１入院基本料
               WHEN    "190077410"
                  MOVE "190700310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        一般病棟１３対１入院基本料
               WHEN    "190077510"
                  MOVE "190700410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        一般病棟１５対１入院基本料
               WHEN    "190077610"
                  MOVE "190700510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        一般病棟７対１入院基本料（経過措置）
               WHEN    "190700110"
                  MOVE "190700610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定入院基本料（一般病棟入院基本料）
               WHEN    "190799410"
                  MOVE "190700810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ａ）
               WHEN    "190121310"
                  MOVE "190700910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ａ）（生活療養）
               WHEN    "190123710"
                  MOVE "190701010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｂ）
               WHEN    "190121410"
                  MOVE "190701110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
               WHEN    "190123810"
                  MOVE "190701210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｃ）
               WHEN    "190121510"
                  MOVE "190701310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
               WHEN    "190123910"
                  MOVE "190701410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｄ）
               WHEN    "190121610"
                  MOVE "190701510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
               WHEN    "190124010"
                  MOVE "190701610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｅ）
               WHEN    "190121710"
                  MOVE "190701710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
               WHEN    "190124110"
                  MOVE "190701810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｆ）
               WHEN    "190131610"
                  MOVE "190701910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
               WHEN    "190131710"
                  MOVE "190702010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｇ）
               WHEN    "190131810"
                  MOVE "190702110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
               WHEN    "190131910"
                  MOVE "190702210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｈ）
               WHEN    "190132010"
                  MOVE "190702310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
               WHEN    "190132110"
                  MOVE "190702410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｉ）
               WHEN    "190132210"
                  MOVE "190702510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
               WHEN    "190132310"
                  MOVE "190702610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ａ）
               WHEN    "190132410"
                  MOVE "190702710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ａ）（生活療養）
               WHEN    "190132510"
                  MOVE "190702810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｂ）
               WHEN    "190132610"
                  MOVE "190702910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
               WHEN    "190132710"
                  MOVE "190703010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｃ）
               WHEN    "190132810"
                  MOVE "190703110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
               WHEN    "190132910"
                  MOVE "190703210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｄ）
               WHEN    "190133010"
                  MOVE "190703310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
               WHEN    "190133110"
                  MOVE "190703410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｅ）
               WHEN    "190133210"
                  MOVE "190703510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
               WHEN    "190133310"
                  MOVE "190703610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｆ）
               WHEN    "190133410"
                  MOVE "190703710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
               WHEN    "190133510"
                  MOVE "190703810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｇ）
               WHEN    "190133610"
                  MOVE "190703910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
               WHEN    "190133710"
                  MOVE "190704010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｈ）
               WHEN    "190133810"
                  MOVE "190704110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
               WHEN    "190133910"
                  MOVE "190704210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｉ）
               WHEN    "190134010"
                  MOVE "190704310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
               WHEN    "190134110"
                  MOVE "190704410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        結核病棟７対１入院基本料
               WHEN    "190117910"
                  MOVE "190704510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        結核病棟１０対１入院基本料
               WHEN    "190081710"
                  MOVE "190704610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        結核病棟１３対１入院基本料
               WHEN    "190081810"
                  MOVE "190704710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        結核病棟１５対１入院基本料
               WHEN    "190081910"
                  MOVE "190704810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        結核病棟１８対１入院基本料
               WHEN    "190082010"
                  MOVE "190704910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        結核病棟２０対１入院基本料
               WHEN    "190082110"
                  MOVE "190705010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神病棟１０対１入院基本料
               WHEN    "190083610"
                  MOVE "190705110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神病棟１３対１入院基本料
               WHEN    "190134510"
                  MOVE "190705210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神病棟１５対１入院基本料
               WHEN    "190083810"
                  MOVE "190705310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神病棟１８対１入院基本料
               WHEN    "190083910"
                  MOVE "190705410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神病棟２０対１入院基本料
               WHEN    "190084010"
                  MOVE "190705510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院一般病棟７対１入院基本料
               WHEN    "190118610"
                  MOVE "190705710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院一般病棟１０対１入院基本料
               WHEN    "190085910"
                  MOVE "190705810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院一般病棟７対１入院基本料（経過措置）
               WHEN    "190705610"
                  MOVE "190705910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院結核病棟７対１入院基本料
               WHEN    "190118810"
                  MOVE "190706010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院結核病棟１０対１入院基本料
               WHEN    "190086410"
                  MOVE "190706110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院結核病棟１３対１入院基本料
               WHEN    "190086510"
                  MOVE "190706210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院結核病棟１５対１入院基本料
               WHEN    "190118910"
                  MOVE "190706310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院精神病棟７対１入院基本料
               WHEN    "190119010"
                  MOVE "190706410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院精神病棟１０対１入院基本料
               WHEN    "190086910"
                  MOVE "190706510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院精神病棟１３対１入院基本料
               WHEN    "190134710"
                  MOVE "190706610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定機能病院精神病棟１５対１入院基本料
               WHEN    "190119110"
                  MOVE "190706710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        専門病院７対１入院基本料
               WHEN    "190119310"
                  MOVE "190706910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        専門病院１０対１入院基本料
               WHEN    "190090210"
                  MOVE "190707010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        専門病院１３対１入院基本料
               WHEN    "190090310"
                  MOVE "190707110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        専門病院７対１入院基本料（経過措置）
               WHEN    "190706810"
                  MOVE "190707210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        障害者施設等７対１入院基本料
               WHEN    "190126310"
                  MOVE "190707310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        障害者施設等１０対１入院基本料
               WHEN    "190093710"
                  MOVE "190707410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        障害者施設等１３対１入院基本料
               WHEN    "190093810"
                  MOVE "190707510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        障害者施設等１５対１入院基本料
               WHEN    "190093910"
                  MOVE "190707610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料１（１４日以内）
               WHEN    "190097010"
                  MOVE "190708610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料１（１５日以上３０日以内）
               WHEN    "190119510"
                  MOVE "190708710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料１（３１日以上）
               WHEN    "190119610"
                  MOVE "190708810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料２（１４日以内）
               WHEN    "190097210"
                  MOVE "190708910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料２（１５日以上３０日以内）
               WHEN    "190119710"
                  MOVE "190709010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料２（３１日以上）
               WHEN    "190119810"
                  MOVE "190709110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料３（１４日以内）
               WHEN    "190135110"
                  MOVE "190709210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料３（１５日以上３０日以内）
               WHEN    "190135210"
                  MOVE "190709310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所入院基本料３（３１日以上）
               WHEN    "190135310"
                  MOVE "190709410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ａ
               WHEN    "190122010"
                  MOVE "190711410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ａ（生活療養）
               WHEN    "190124310"
                  MOVE "190711510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ｂ
               WHEN    "190122110"
                  MOVE "190711610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ｂ（生活療養）
               WHEN    "190124410"
                  MOVE "190711710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ｃ
               WHEN    "190122210"
                  MOVE "190711810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ｃ（生活療養）
               WHEN    "190124510"
                  MOVE "190711910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ｄ
               WHEN    "190122310"
                  MOVE "190712010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ｄ（生活療養）
               WHEN    "190124610"
                  MOVE "190712110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ｅ
               WHEN    "190122410"
                  MOVE "190712210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        有床診療所療養病床入院基本料Ｅ（生活療養）
               WHEN    "190124710"
                  MOVE "190712310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料１（３日以内）
               WHEN    "190024510"
                  MOVE "190714410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料１（４〜７日）
               WHEN    "190128610"
                  MOVE "190714510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料１（８〜１４日）
               WHEN    "190074510"
                  MOVE "190714610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料２（３日以内）
               WHEN    "190024310"
                  MOVE "190714710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料２（４〜７日）
               WHEN    "190128710"
                  MOVE "190714810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料２（８〜１４日）
               WHEN    "190024410"
                  MOVE "190714910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料３（救命救急）（３日以内）
               WHEN    "190138110"
                  MOVE "190715010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料３（救命救急）（４〜７日）
               WHEN    "190138210"
                  MOVE "190715110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料３（救命救急）（８〜１４日）
               WHEN    "190138310"
                  MOVE "190715210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料３（広範囲熱傷・３日以内）
               WHEN    "190138410"
                  MOVE "190715310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料３（広範囲熱傷・４〜７日）
               WHEN    "190138510"
                  MOVE "190715410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料３（広範囲熱傷・８〜６０日）
               WHEN    "190138610"
                  MOVE "190715510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料４（救命救急入院料）（３日以内）
               WHEN    "190138710"
                  MOVE "190715610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料４（救命救急入院料）（４〜７日）
               WHEN    "190138810"
                  MOVE "190715710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料４（救命救急入院料）（８〜１４日）
               WHEN    "190138910"
                  MOVE "190715810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料４（広範囲熱傷・３日以内）
               WHEN    "190139010"
                  MOVE "190715910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料４（広範囲熱傷・４〜７日）
               WHEN    "190139110"
                  MOVE "190716010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料４（広範囲熱傷・８〜１４日）
               WHEN    "190139210"
                  MOVE "190716110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        救命救急入院料４（広範囲熱傷・１５〜６０日）
               WHEN    "190139310"
                  MOVE "190716210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定集中治療室管理料１（７日以内）
               WHEN    "190116310"
                  MOVE "190717210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定集中治療室管理料１（８日以上１４日以内）
               WHEN    "190116410"
                  MOVE "190717310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定集中治療室管理料２（特定集中・７日以内）
               WHEN    "190139810"
                  MOVE "190717410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定集中治療室管理料２（特定集中・８〜１４日）
               WHEN    "190139910"
                  MOVE "190717510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定集中治療室管理料２（広範囲熱傷・７日以内）
               WHEN    "190140010"
                  MOVE "190717610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定集中治療室管理料２（広範囲熱傷・８〜６０日）
               WHEN    "190140110"
                  MOVE "190717710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        ハイケアユニット入院医療管理料
               WHEN    "190117310"
                  MOVE "190717810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        脳卒中ケアユニット入院医療管理料
               WHEN    "190120810"
                  MOVE "190717910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        小児特定集中治療室管理料（７日以内）
               WHEN    "190149910"
                  MOVE "190718010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        小児特定集中治療室管理料（８日以上１４日以内）
               WHEN    "190150010"
                  MOVE "190718110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        新生児特定集中治療室管理料１
               WHEN    "190024710"
                  MOVE "190718210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        新生児特定集中治療室管理料２
               WHEN    "190140410"
                  MOVE "190718310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        総合周産期特定集中治療室管理料（母体・胎児）
               WHEN    "190066710"
                  MOVE "190718410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        総合周産期特定集中治療室管理料（新生児）
               WHEN    "190066810"
                  MOVE "190718510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        新生児治療回復室入院医療管理料
               WHEN    "190140510"
                  MOVE "190718610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        一類感染症患者入院医療管理料（７日以内）
               WHEN    "190075510"
                  MOVE "190718710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        一類感染症患者入院医療管理料（８日〜１４日以内）
               WHEN    "190075610"
                  MOVE "190718810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特殊疾患入院医療管理料
               WHEN    "190075710"
                  MOVE "190718910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        小児入院医療管理料１
               WHEN    "190129310"
                  MOVE "190719010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        小児入院医療管理料２
               WHEN    "190140710"
                  MOVE "190719110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        小児入院医療管理料３
               WHEN    "190111110"
                  MOVE "190719210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        小児入院医療管理料４
               WHEN    "190111210"
                  MOVE "190719310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        小児入院医療管理料５
               WHEN    "190075810"
                  MOVE "190719410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        回復期リハビリテーション病棟入院料１
               WHEN    "190150110"
                  MOVE "190719510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        回復期リハビリテーション病棟入院料１（生活療養）
               WHEN    "190150210"
                  MOVE "190719610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        回復期リハビリテーション病棟入院料２
               WHEN    "190141610"
                  MOVE "190719710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        回復期リハビリテーション病棟入院料２（生活療養）
               WHEN    "190141710"
                  MOVE "190719810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        回復期リハビリテーション病棟入院料３
               WHEN    "190141810"
                  MOVE "190719910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        回復期リハビリテーション病棟入院料３（生活療養）
               WHEN    "190141910"
                  MOVE "190720010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        亜急性期入院医療管理料１
               WHEN    "190117410"
                  MOVE "190720110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        亜急性期入院医療管理料２
               WHEN    "190129910"
                  MOVE "190720210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        亜急性期入院医療管理料１（指定地域）
               WHEN    "190150310"
                  MOVE "190720310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        亜急性期入院医療管理料２（指定地域）
               WHEN    "190150410"
                  MOVE "190720410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特殊疾患病棟入院料１
               WHEN    "190055210"
                  MOVE "190720510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特殊疾患病棟入院料２
               WHEN    "190055310"
                  MOVE "190720610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        緩和ケア病棟入院料（３０日以内）
               WHEN    "190028910"
                  MOVE "190720710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        緩和ケア病棟入院料（３１日以上６０日以内）
               WHEN    "190150510"
                  MOVE "190720810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        緩和ケア病棟入院料（６１日以上）
               WHEN    "190150610"
                  MOVE "190720910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科救急入院料１（３０日以内）
               WHEN    "190121010"
                  MOVE "190721010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科救急入院料１（３１日以上）
               WHEN    "190111410"
                  MOVE "190721110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科救急入院料２（３０日以内）
               WHEN    "190130010"
                  MOVE "190721210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科救急入院料２（３１日以上）
               WHEN    "190130110"
                  MOVE "190721310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科急性期治療病棟入院料１（３０日以内）
               WHEN    "190121110"
                  MOVE "190721410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科急性期治療病棟入院料１（３１日以上）
               WHEN    "190066910"
                  MOVE "190721510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科急性期治療病棟入院料２（３０日以内）
               WHEN    "190121210"
                  MOVE "190721610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科急性期治療病棟入院料２（３１日以上）
               WHEN    "190067010"
                  MOVE "190721710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科救急・合併症入院料（３０日以内）
               WHEN    "190130210"
                  MOVE "190721810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神科救急・合併症入院料（３１日以上）
               WHEN    "190130310"
                  MOVE "190721910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        児童・思春期精神科入院医療管理料
               WHEN    "190151110"
                  MOVE "190722010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        精神療養病棟入院料
               WHEN    "190055010"
                  MOVE "190722110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        認知症治療病棟入院料１（３０日以内）
               WHEN    "190151610"
                  MOVE "190722310" TO  LNK-EIYOUKKS-CHGSRYCD
      *        認知症治療病棟入院料１（３１日以上６０日以内）
               WHEN    "190739910"
                  MOVE "190722410" TO  LNK-EIYOUKKS-CHGSRYCD
      *        認知症治療病棟入院料１（６１日以上）
               WHEN    "190740010"
                  MOVE "190722510" TO  LNK-EIYOUKKS-CHGSRYCD
      *        認知症治療病棟入院料２（３０日以内）
               WHEN    "190151710"
                  MOVE "190722610" TO  LNK-EIYOUKKS-CHGSRYCD
      *        認知症治療病棟入院料２（３１日以上６０日以内）
               WHEN    "190813410"
                  MOVE "190722710" TO  LNK-EIYOUKKS-CHGSRYCD
      *        認知症治療病棟入院料２（６１日以上）
               WHEN    "190813510"
                  MOVE "190722810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟入院料１
               WHEN    "190151910"
                  MOVE "190722910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟入院料２
               WHEN    "190152010"
                  MOVE "190723010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟入院料（亜急性期入院医療管理）
               WHEN    "190152610"
                  MOVE "190723110" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定一般病棟入院料（亜急性期・リハビリ算定患者）
               WHEN    "190152710"
                  MOVE "190723210" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定入院基本料（特定機能病院入院基本料）
               WHEN    "190160810"
                  MOVE "190751810" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定入院基本料（専門病院入院基本料）
               WHEN    "190161010"
                  MOVE "190751910" TO  LNK-EIYOUKKS-CHGSRYCD
      *        特定入院基本料（障害者施設等入院基本料）
               WHEN    "190161210"
                  MOVE "190752010" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）一般病棟７対１入院基本料
               WHEN    "190117810"
                  MOVE "190752830" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）一般病棟７対１入院基本料（経過措置）
               WHEN    "190752730"
                  MOVE "190752930" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）一般病棟１０対１入院基本料
               WHEN    "190111810"
                  MOVE "190753030" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）一般病棟１３対１入院基本料
               WHEN    "190111910"
                  MOVE "190753130" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）一般病棟１５対１入院基本料
               WHEN    "190112010"
                  MOVE "190753230" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）特定入院基本料
               WHEN    "190811210"
                  MOVE "190753330" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）特定機能病院一般病棟７対１入院基本料
               WHEN    "190118710"
                  MOVE "190753530" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）特定機能病院一般病棟７対１入院基本料（経過）
               WHEN    "190753430"
                  MOVE "190753630" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）特定機能病院一般病棟１０対１入院基本料
               WHEN    "190114710"
                  MOVE "190753730" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）専門病院７対１入院基本料
               WHEN    "190119410"
                  MOVE "190753930" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）専門病院７対１入院基本料（経過措置）
               WHEN    "190753830"
                  MOVE "190754030" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）専門病院１０対１入院基本料
               WHEN    "190115710"
                  MOVE "190754130" TO  LNK-EIYOUKKS-CHGSRYCD
      *        （選）専門病院１３対１入院基本料
               WHEN    "190115810"
                  MOVE "190754230" TO  LNK-EIYOUKKS-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       200-CDCHG-EXT.
           .
      *
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
