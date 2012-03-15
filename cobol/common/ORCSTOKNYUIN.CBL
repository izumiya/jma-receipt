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
       PROGRAM-ID.                 ORCSTOKNYUIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 特定入院料コード取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/08    NACL−小豆沢  新規作成
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
           COPY    "CPSK5001.INC".
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
      *    特定入院料コード取得パラメタ
           COPY    "CPORCSTOKNYUIN.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTOKNYUIN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-TOKNYUIN-SANTEIYM    >=   "201204"
               PERFORM    202-EDIT-SEC
           ELSE
               IF  LNK-TOKNYUIN-SANTEIYM    >=   "201004"
                   PERFORM    201-EDIT-SEC
               ELSE
                   PERFORM    200-EDIT-SEC
               END-IF
           END-IF
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    特定入院料コードセット（平成２２年３月まで）
      *****************************************************************
       200-EDIT-SEC                    SECTION.
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料２
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料３
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症病棟入院料１
                       MOVE   "190739910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                老人性認知症疾患療養病棟入院料
                       MOVE   "190740110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科救急入院料
                       MOVE   "190111410"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "11"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190066910"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "12"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190067010"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "13"
      *                回復期リハビリテーション病棟入院料
                       MOVE   "190075910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料４
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症病棟入院料２
                       MOVE   "190813410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                回復期リハビリテーション病棟入院料（生活療養）
                       MOVE   "190124910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190129410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190129510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190129610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190129710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                後期高齢者特定入院基本料
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                後期高齢者特定入院基本料（後期高齢者特別入院基本料算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料Ａ
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                療養病棟入院基本料Ｂ
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                療養病棟入院基本料Ｃ
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                療養病棟入院基本料Ｄ
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                療養病棟入院基本料Ｅ
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料Ａ（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料Ｂ（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料Ｃ（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料Ｄ（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料Ｅ（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       200-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２２年４月から平成２４年３月まで）
      *****************************************************************
       201-EDIT-SEC                    SECTION.
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症病棟入院料１
                       MOVE   "190739910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                老人性認知症疾患療養病棟入院料
                       MOVE   "190740110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科救急入院料
                       MOVE   "190111410"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "11"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190066910"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "12"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190067010"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "13"
      *                回復期リハビリテーション病棟入院料
                       MOVE   "190075910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症病棟入院料２
                       MOVE   "190813410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                回復期リハビリテーション病棟入院料（生活療養）
                       MOVE   "190124910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                回復期リハビリテーション病棟入院料１（基準未適合）
                       MOVE   "190129410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                回復期リハビリテーション病棟入院料１（生活療養）（基準未適合）
                       MOVE   "190129510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                回復期リハビリテーション病棟入院料２（基準未適合）
                       MOVE   "190129610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                回復期リハビリテーション病棟入院料２（生活療養）（基準未適合）
                       MOVE   "190129710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                後期高齢者特定入院基本料
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                後期高齢者特定入院基本料（後期高齢者特別入院基本料算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       201-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２４年４月から）
      *****************************************************************
       202-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYM
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190150110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190150210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料１
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "92"
      *                亜急性期入院医療管理料１（指定地域）
                       MOVE   "190150310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "93"
      *                亜急性期入院医療管理料２（指定地域）
                       MOVE   "190150410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "95"
      *                特定一般病棟入院料（亜急性期入院医療管理）
                       MOVE   "190152610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "96"
      *                特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
                       MOVE   "190152710"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       202-EDIT-EXT.
           EXIT.
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
