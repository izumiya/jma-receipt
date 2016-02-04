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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI021S3V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為　診療行為登録　（入力一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/04/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       DATA                DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-WKSRYACT            PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
      *    03  FLG-PTNYUINRRK          PIC 9(01).
      *    03  FLG-HKNNUM              PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SP                  PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
      *
           03  FLG-DELCHK              PIC 9(01).
           03  FLG-DELOK               PIC 9(01).
           03  FLG-DEL                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDW2                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
      *
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    保険組合せ
     **    03  WRK-HKNCOMBI            PIC 9(04).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-ERRMSG-02           PIC X(100).
      *
      *    内容エラー
           03  WRK-MD-ERRCD            PIC X(04).
           03  WRK-MD-ERRMSG           PIC X(100).
      *
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
           03  WRK-ORCA-UID            PIC X(64).
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
      *    保険組合せ内容
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
      *    伝票番号
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-SYORI-MODE          PIC X(03).
      *
      *    金額
           03  WRK-KINGAKU             PIC S9(08).
      *    請求金額
           03  WRK-SKYMONEY-GOK        PIC S9(08).
      *    訂正用　入金上限額
           03  WRK-NYUKIN-GOK          PIC S9(08).
           03  WRK-HENKIN-GOK          PIC S9(08).
           03  WRK-TEIKEI-SKYMONEY     PIC S9(08).
           03  WRK-OLD-SKYMONEY        PIC S9(08).
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
      *
      *    負担率・額編集
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
      *
      *    03  WRK-H-LINE              PIC X(03).
      *    03  WRK-H-IDX               PIC X(02).
      *
      *    03  WRK-KIDMSG              PIC X(100).
      *    03  WRK-MAE-KIDCD           PIC X(04).
      *
           03  WRK-DELNUM              PIC 9(02).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE        PIC X(10).
           03  WRK-PERFORM-TIME8       PIC X(08).
      *
           03  WRK-PERFORM-YMD         PIC X(08).
           03  WRK-PERFORM-TIME.
               05  WRK-PERFORM-THH     PIC X(02).
               05  WRK-PERFORM-TMM     PIC X(02).
               05  WRK-PERFORM-TSS     PIC X(02).
           03  WRK-BASE-YMD            PIC X(08).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC 9(10).
      *    03  WRK-SRYKA               PIC X(02).
      *    03  WRK-SRYKA-NAME          PIC X(80).
      *    03  WRK-DRCD                PIC X(05).
      *    03  WRK-DRCD-NAME           PIC X(80).
      *
           03  WRK-NYUKIN-HOHO         PIC X(02).
           03  WRK-NYUKIN-HOHO-NAME    PIC X(20).
           03  WRK-NYUKIN-JYOTAI       PIC X(01).
      *    入金額
           03  WRK-NYUKIN              PIC  9(07).
           03  WRK-NYUKIN-OK           PIC  9(01).
      *    調整金１
           03  WRK-CHOSEI1             PIC S9(07).
      *    調整金２
           03  WRK-CHOSEI2             PIC S9(07).
      *    返金額
           03  WRK-HENKIN              PIC S9(07).
           03  WRK-HENKIN-OK           PIC  9(01).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    共通パラメタ内容
       01  WRK-APIV3DATA-AREA.
      *    電子カルテUID
           03  WRK-KARTE-UID           PIC X(36).
      *    患者番号
           03  WRK-PARA-PTNUM          PIC X(20).
           03  WRK-APIV3DATA-REC.
      *        レスポンス番号
               05  WRK-RESPONSE-NUMBER     PIC X(02).
               05  WRK-SELECT-CODE         PIC X(04).
      *        処理区分
               05  WRK-PARA-MODE           PIC X(03).
      *        伝票番号
               05  WRK-PARA-DENPNUM        PIC 9(07).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *    ワーク診療行為マスタ−
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
      *    API用剤削除連番
           03  LNK-WKSRYAPI-RENNUM         PIC 9(02).
      *    剤削除区分
           03  LNK-WKSRYAPI-DELKBN         PIC 9(01).
      *
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    収納マスタワーク
       01  LNK-SYUNOU-AREA.
           02  LNK-SYUNOU-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUNOU-MAX              PIC 9(02).
      *
      *    収納マスタワーク（計算用）
       01  LNK-SYUNOU2-AREA.
           02  LNK-SYUNOU2-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU2-//.
       01  LNK-SYUNOU2-MAX              PIC 9(02).
      *
      *    収納マスタワーク（訂正前）
       01  TEI-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //TEI-SYU-//.
      *
      *    診療行為マスタワーク(パラメタ）
       01  PARA-WKSRYACT-AREA.
           02  PARA-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //PARA-WKSRY-//.
      *    附加情報を追加
           03  PARA-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //PARA-WKSRYP-//.
      *
      *
      *    請求点数名称
       01  TBL-HKNTEN-MEI-AREA.
           03  FILLER              PIC X(30)   VALUE   "初・再診料".
           03  FILLER              PIC X(30)   VALUE   "医学管理等".
           03  FILLER              PIC X(30)   VALUE   "在宅療養".
           03  FILLER              PIC X(30)   VALUE   "投薬".
           03  FILLER              PIC X(30)   VALUE   "注射".
           03  FILLER              PIC X(30)   VALUE   "処置".
           03  FILLER              PIC X(30)   VALUE   "手術".
           03  FILLER              PIC X(30)   VALUE   "麻酔".
           03  FILLER              PIC X(30)   VALUE   "検査".
           03  FILLER              PIC X(30)   VALUE   "画像診断".
           03  FILLER              PIC X(30)   VALUE   "リハビリ".
           03  FILLER              PIC X(30)   VALUE   "精神科専門".
           03  FILLER              PIC X(30)   VALUE   "放射線治療".
           03  FILLER              PIC X(30)   VALUE   "病理診断".
           03  FILLER              PIC X(30)   VALUE   "入院料".
           03  FILLER              PIC X(30)   VALUE   "療養担当手当".
       01  TBL-HKNTEN-MEI-RES      REDEFINES   TBL-HKNTEN-MEI-AREA.
           03  TBL-HKNTEN-MEI      PIC X(30)   OCCURS   16.
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    外来登録サブ
           COPY    "CPORCSCGAIRAI.INC".
      *
      *    外来収納負担金計算サブ
           COPY    "CPORCSCSYUNOUG.INC".
      *
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    ＳＰＡ画面パラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV3REQ3.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV3RES3.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    保険年齢・負担割合算定サブ
      ***  COPY    "CPORCSAGECHK.INC".
      *
      *    診療種別名テーブル
      *    COPY    "CPORCSSRYSYU.INC".
      *
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPI021SUB2V3.INC".
      *
      *    処理終了パラメタ
           COPY    "CPORAPIV3SUB01.INC".
      *
      *    API XML読み込み共通定義
        01  XML-APIREQ.
            03  APIREQ-CONTEXT          PIC S9(9)   BINARY.
            03  APIREQ-OBJECT           PIC X(8).
            03  APIREQ-MODE             PIC S9(9)   BINARY.
            03  APIREQ-RECORDNAME       PIC X(128).
            03  APIREQ-PATIENTINFOREQ   PIC X(1000000).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    入金方法
           COPY    "CPSK1041.INC".
      *    診療科情報
      *    COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    医療機関情報
      *    COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
      *
           COPY    "MCPDATA3.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "CPAPIMOD.INC".
           COPY    "CPAPILSTV2.INC".
      *V3  一体化
           COPY    "API21V3SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "*********************"
           DISPLAY   "* medicalreqv33 start*"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
      *
           MOVE    MCP-TERM            TO  WRK-MCP-TERM
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST04-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
      *
           DISPLAY   "*********************"
           DISPLAY   "* medicalreqv33 end  *"
           DISPLAY   "*********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-MEDICALV3RES3
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  MDCRES-RESKEY
      *
           STRING  "A+"
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  MDCRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
           END-IF
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
      *    診療種別テーブルセット
      *    CALL    "ORCSSRYSYU"        USING
      *                                MSYU-DATA-REC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD-G
      *
           MOVE    MDCREQ-REQUEST-NUMBER   TO  MDCRES-REQUEST-NUMBER
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "04"
      *        確認処理
               MOVE    1                   TO  FLG-SYORI
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
           WHEN    "05"
      *        登録処理
               MOVE    2                   TO  FLG-SYORI
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      FLG-SYORI       NOT =   9
      *        患者情報内容編集
               PERFORM 20021-MDCRES-HEN-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    "04"                TO  MDCRES-RESPONSE-NUMBER
               IF      FLG-SYORI           =   9
      *            終了処理
                   PERFORM 300-SYORI-END-SEC
                   IF      WRK-ERRCD       NOT =   "08"
                       MOVE    "初回接続からおこなってください。"
                                           TO  WRK-ERRMSG-02
                   END-IF
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    FLG-SYORI
           WHEN    1
      *        確認処理
               PERFORM 2002-SYORI-01-SEC
           WHEN    2
      *        登録処理
               PERFORM 2003-SYORI-02-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST05-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALV3REQ3
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST05-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req3"     TO  APIREQ-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO   1000-APTXMLREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    ORCA-BLOB-OBJECT    TO  APIREQ-OBJECT
           MOVE    "medicalv3req3"     TO  APIREQ-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALV3REQ3
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_medicalv3req3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    ORCA-BLOB-OBJECT    TO  APIREQ-OBJECT
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               CONTINUE
           END-IF
      *
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "登録正常終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MDCRES-API-RESULT-MSG
      *
               IF      WRK-ERRMSG-02   NOT =   SPACE
                   MOVE    SPACE           TO  MDCRES-API-RESULT-MSG
                   STRING  WRK-ERRMSG
                           WRK-ERRMSG-02   DELIMITED  BY  SPACE
                                           INTO  MDCRES-API-RESULT-MSG
                   END-STRING
               END-IF
           END-IF
      *
      *    診療内容エラー・警告編集
           PERFORM 3001-WAINGMSG-HEN-SEC
      *
           IF      APILST05-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res3"     TO  MDCRES-RECORDNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALV3RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalv3res3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALV3RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_medicalv3res3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
     ***** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-MEDICALV3RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  APILST05-BODY 
               MOVE  "application/xml" TO  APILST05-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
           END-IF
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容エラー・警告編集処理
      *****************************************************************
       3001-WAINGMSG-HEN-SEC        SECTION.
      *
           IF      WRK-MD-ERRCD    NOT =   SPACE
               MOVE    WRK-MD-ERRCD        TO  MDCRES-MEDI-RESULT
               MOVE    WRK-MD-ERRMSG       TO  MDCRES-MEDI-RESULT-MSG
           END-IF
      *
          .
       3001-WAINGMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
      *    IF      MDCREQ-OUTPATIENT-CLASS    =   LOW-VALUE
      *        MOVE  SPACE             TO  MDCREQ-OUTPATIENT-CLASS
      *    END-IF
           IF      MDCREQ-PATIENTID    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PATIENTID
           END-IF
           IF     MDCREQ-PERFORM-DATE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-DATE
           END-IF
           IF     MDCREQ-PERFORM-TIME  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-TIME
           END-IF
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    SPATMPからSPAの取得
           IF     (MDCREQ-ORCA-UID NOT =   SPACE  )
             AND  (WRK-ERRCD           =   SPACE  )
               PERFORM 2008-SPATMP-SEL-SEC
           END-IF
      *
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        排他チェック & ロック処理
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "90"            TO  WRK-ERRCD
                END-IF
           END-IF
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *!!!!
      *    訂正判定
           IF      WRK-ERRCD           =   SPACE
      *        伝票番号
               MOVE    SPACE               TO  MDCRES-INVOICD-NUMBER
               IF     (MDCREQ-PATIENT-MODE =   "Modify")
                  OR  (WRK-PARA-MODE       =   "UPD" )
                   PERFORM 20011-DENPNUM-CHK-SEC
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        ワーク診療行為検索
               PERFORM 200234-WKSRYACT-INIT-SEC
               IF      LNK-WKSRYACT-MAX    =   ZERO
                   MOVE    "08"            TO  WRK-ERRCD
               END-IF
           END-IF
      *    電子カルテＵＩＤチェック
           IF      WRK-ERRCD           =   SPACE
               IF      MDCREQ-KARTE-UID    NOT =   WRK-KARTE-UID
      *            他端末使用中
                   MOVE    "90"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    上記のエラー時は初回から
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    9                   TO  FLG-SYORI
           END-IF
      *
      *    基準日付（伝票発行日）チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-BASEDATE-CHK-SEC
           END-IF
      *    入金方法
           IF     WRK-ERRCD            =   SPACE
               PERFORM 20013-NYUKIN-HOHO-CHK-SEC
           END-IF
      *
      *    入金額
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-NYUKIN-CHK-SEC
           END-IF
      *
      *    調整金
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-CHOSEIKIN-CHK-SEC
           END-IF
      *
      *    返金
           IF    (WRK-ERRCD            =   SPACE )
               PERFORM 20013-HENKIN-CHK-SEC
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    患者番号
           MOVE    MDCREQ-PATIENTID        TO  MDCRES-PATIENTID
           MOVE    MDCREQ-PATIENTID        TO  WRK-PTNUM
      *    診療日・時間
           MOVE    MDCREQ-PERFORM-DATE     TO  MDCRES-PERFORM-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  MDCRES-PERFORM-TIME
      *    数字のみに編集
           MOVE    MDCREQ-PERFORM-DATE     TO  WRK-HEN-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           MOVE    WRK-THMS                TO  WRK-PERFORM-TIME
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    MDCRES-INFORMATION-DATE
                                           TO  MDCRES-PERFORM-DATE
           END-IF
      *
      *    基準日付（伝票発行日）
           MOVE    MDCREQ-BASE-DATE        TO  MDCRES-BASE-DATE
      *    数字のみに編集
           MOVE    MDCREQ-BASE-DATE        TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-BASE-YMD
      *    未設定時はシステム情報設定
           IF      WRK-BASE-YMD            =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-BASE-YMD
               MOVE    MDCRES-INFORMATION-DATE
                                           TO  MDCRES-BASE-DATE
           END-IF
      *
      *    入力UID
           MOVE    MDCREQ-ORCA-UID     TO  MDCRES-ORCA-UID
           MOVE    MDCREQ-KARTE-UID    TO  MDCRES-KARTE-UID
      *    入金方法
           MOVE    MDCREQ-IC-CODE      TO  MDCREQ-IC-CODE
      *    入金金額
           MOVE    MDCREQ-IC-MONEY     TO  MDCRES-IC-MONEY
      *    調整金１
           MOVE    MDCREQ-AD-MONEY1    TO  MDCRES-AD-MONEY1
           MOVE    MDCREQ-AD-MONEY2    TO  MDCRES-AD-MONEY2
      *
      *    必須入力チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *
      *    UID（電子カルテ）
           IF      MDCREQ-KARTE-UID    =   SPACE
               MOVE    "04"                TO  WRK-ERRCD
           END-IF
      *    SPATMPからSPAの取得
           IF      MDCREQ-ORCA-UID     =   SPACE
               MOVE    "05"                TO  WRK-ERRCD
           END-IF
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-PERFORM-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-PERFORM-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
               DISPLAY "Accept Ymd =  " WRK-PERFORM-YMD
           END-IF
      *    ＳＰＡと一致すること
           IF      WRK-PERFORM-YMD NOT =   SPA-SRYYMD
               MOVE    "06"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-PERFORM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準日付（伝票発行日）チェック
      *****************************************************************
       20011-BASEDATE-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-BASE-YMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-BASEDATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC     SECTION.
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO                  20011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  MDCRES-PATIENTID
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  WRK-PTID
      *    ＳＰＡと一致すること
           IF     (WRK-PTID            =   SPA-PTID )
              AND (WRK-PTNUM           =   SPA-PTNUM)
               CONTINUE
           ELSE
               MOVE    "07"                TO  WRK-ERRCD
               GO      TO                  20011-PTNUM-CHK-EXT
           END-IF
      *    MOVE    SPTID-PTID          TO  SPA-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *
      *H28.1
      *****************************************************************
      *    伝票番号チェック処理
      *****************************************************************
       20011-DENPNUM-CHK-SEC         SECTION.
      *
      *    MOVE    ZERO                TO  WRK-DENPNUM
      *
      *    INITIALIZE                      ORCSNUMAREA
      *    MOVE    MDCREQ-INVOICD-NUMBER   TO  SNUM-INX
      *    CALL    "ORCSNUM"           USING
      *                                ORCSNUMAREA
      *    IF     (SNUM-RC         NOT =   ZERO  )   OR
      *           (SNUM-SEISUU         >   7     )   OR
      *           (SNUM-SYOSUU         >   0     )
      *        数値７桁
      *        MOVE    "27"                TO  WRK-ERRCD
      *    ELSE
      *        MOVE    SNUM-OUTNUM         TO  WRK-DENPNUM
      *    END-IF
      *
           MOVE    WRK-PARA-DENPNUM    TO  WRK-DENPNUM
      *
           IF     (MDCREQ-PATIENT-MODE =   "Modify")
              AND (WRK-PARA-MODE       =   "UPD"   )
              AND (SPA-SYORIFLG        =   1       )
               MOVE    "UPD"               TO  WRK-SYORI-MODE
               IF     (SPA-DENPNUM     NOT =   WRK-PARA-DENPNUM)
                   MOVE    "27"                TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    "28"                TO  WRK-ERRCD
           END-IF
           .
       20011-DENPNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法区分チェック処理
      *****************************************************************
       20013-NYUKIN-HOHO-CHK-SEC         SECTION.
      *
           IF      MDCREQ-IC-CODE      =   SPACE
               MOVE    SPA-NYUKIN-HOHO     TO  WRK-NYUKIN-HOHO
               IF      SPA-HKNCOMBINUM     =   "9999"
                   GO      TO      20013-NYUKIN-HOHO-CHK-EXT
               END-IF
           ELSE
               MOVE    MDCREQ-IC-CODE      TO  WRK-NYUKIN-HOHO
           END-IF
      *
      *    入金方法区分
           MOVE    SPACE               TO  SYS-1041-REC
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1041-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1041-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1041-HOSPNUM
           IF      WRK-NYUKIN-HOHO     =   SPACE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    WRK-NYUKIN-HOHO     TO  SYS-1041-KBNCD
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
           END-IF
      *    システム管理検索
           MOVE    SYS-1041-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYS-1041-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1041-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYS-1041-NYKN-TANNAME
                                           TO  WRK-NYUKIN-HOHO-NAME
               MOVE    SYS-1041-NYKN-JYOTAI
                                           TO  WRK-NYUKIN-JYOTAI
               MOVE    SYS-1041-KBNCD(1:2) TO  WRK-NYUKIN-HOHO
           ELSE
               MOVE    SPACE               TO  WRK-NYUKIN-HOHO-NAME
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-NYUKIN-HOHO         TO  MDCRES-IC-CODE
           MOVE    WRK-NYUKIN-HOHO-NAME    TO  MDCRES-IC-CODE-NAME
           .
       20013-NYUKIN-HOHO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金金額数値チェック処理
      *****************************************************************
       20013-NYUKIN-CHK-SEC         SECTION.
      *
           IF      MDCREQ-IC-MONEY     =   SPACE
               MOVE    ZERO            TO  WRK-NYUKIN-OK
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-IC-MONEY     TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   0     )
      *            整数７桁
                   MOVE    "16"                TO  WRK-ERRCD
               ELSE
      *            入金額設定あり
                   MOVE    SNUM-OUTNUM         TO  WRK-NYUKIN
                   MOVE    1                   TO  WRK-NYUKIN-OK
               END-IF
               IF      SPA-HKNCOMBINUM     =   "9999"
                   IF      WRK-NYUKIN      NOT =   ZERO
                       MOVE    "23"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       20013-NYUKIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調整金数値チェック処理
      *****************************************************************
       20013-CHOSEIKIN-CHK-SEC         SECTION.
      *
      *    調整金１
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-AD-MONEY1    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "17"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-CHOSEI1
           END-IF
      *
      *    調整金２
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-AD-MONEY2    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "18"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-CHOSEI2
           END-IF
      *
           IF      SPA-HKNCOMBINUM     =   "9999"
               IF     (WRK-CHOSEI1     NOT =   ZERO )
                 OR   (WRK-CHOSEI2     NOT =   ZERO )
                   MOVE    "24"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       20013-CHOSEIKIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返金金額数値チェック処理
      *****************************************************************
       20013-HENKIN-CHK-SEC         SECTION.
      *
           IF      MDCREQ-RE-MONEY     =   SPACE
               MOVE    ZERO            TO  WRK-NYUKIN-OK
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-RE-MONEY     TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
      ****            (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   0     )
      *            整数７桁
                   MOVE    "16"                TO  WRK-ERRCD
               ELSE
      *            設定あり
                   MOVE    SNUM-OUTNUM         TO  WRK-HENKIN
                   MOVE    1                   TO  WRK-HENKIN-OK
               END-IF
               IF      WRK-HENKIN      NOT =   ZERO
                   IF      SPA-HKNCOMBINUM     =   "9999"
                       MOVE    "25"            TO  WRK-ERRCD
                   END-IF
                   IF      SPA-SYORIFLG    NOT =   1
                       MOVE    "26"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       20013-HENKIN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療内容チェック処理
      *****************************************************************
       2002-SYORI-01-SEC        SECTION.
      *
      *    レスポンス番号チェック
           IF      WRK-RESPONSE-NUMBER =   "04"
                                       OR  "05"
               CONTINUE
           ELSE
               MOVE    "09"            TO  WRK-ERRCD
               GO      TO      2002-SYORI-01-EXT
           END-IF
      *
      *    収納レコード編集
           PERFORM 20020-SYUNOU-PARA-SEC
      *    剤削除処理
           IF      MDCREQ-MDC-MODE     =   "1"
               PERFORM 20021-ZAIDELETE-SEC
      *        ワーク診療行為更新
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 200291-WKSRYACT-UPD-SEC
               END-IF
           END-IF
      *    請求金額取得
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-SYUNOU-HEN-SEC
           END-IF
      *
      *    入金・請求額
           IF     (WRK-ERRCD           =   SPACE)
           AND    (LNK-SYUNOU2-MAX     =   1    )
           AND    (SPA-HKNCOMBINUM     NOT =   "9999" )
      *        訂正前の収納編集
               IF     (SPA-SYORIFLG        =   1     )
                   PERFORM 20025-TEISEI-HENSYU-SEC
               END-IF
      *
               MOVE    LNK-SYUNOU2-REC (1) TO  SYUNOU-REC
               PERFORM 20023-NYUKIN-HEN-SEC
           END-IF
      *
      *    診療行為内容編集
           PERFORM 20021-HENSYU-01-SEC
      *
      *    返却レスポンス番号
           IF     (WRK-ERRCD           =   SPACE )
      *        エラーなしは次へ
               MOVE    "05"            TO  MDCRES-RESPONSE-NUMBER
      *        データ保存
               PERFORM 20029-PARA-INS-SEC
           END-IF
      *
      *    API処理登録
           PERFORM 2008-APIPARAINS-SEC
      *
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-MDCRES-HEN-SEC     SECTION.
      *
           MOVE    PTINF-NAME          TO  MDCRES-NAME
           MOVE    PTINF-KANANAME      TO  MDCRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDCRES-SEX
      *
      *    診療科
           MOVE    SPA-SRYKA           TO  MDCRES-APPOINT-DEP-CODE
           MOVE    SPA-SRYKAMEI        TO  MDCRES-APPOINT-DEP-NAME
      *   ドクター
           MOVE    SPA-DRCD            TO  MDCRES-APPOINT-DR-CODE
           MOVE    SPA-DRCD-NAME       TO  MDCRES-APPOINT-DR-NAME
      *    保険組合せ編集処理
           MOVE    SPA-HKNCOMBINUM    TO  MDCRES-COMBINATION-NUMBER
           IF      SPA-HKNCOMBINUM     =   "9999"
               MOVE    SPA-GMN-HKNCOMBIMEI
                                       TO  MDCRES-MAIN-INSURANCE-NAME
           ELSE
               INITIALIZE                      ORCSAPIHKNAREA
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
               MOVE    "2"                 TO  ORCSAPIHKN-KBN2
               MOVE    SPA-SRYKA           TO  ORCSAPIHKN-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCSAPIHKN-IN-HKNCOMBI
               CALL    "ORCSAPIHKN"        USING
                                           SPA-AREA
                                           ORCSAPIHKNAREA
               MOVE    1                   TO  IDX
               PERFORM 2002101-HKNHEN-TBL-SEC
      *        保険組合せ負担割合編集
               IF      SPA-FTNRATE     NOT =   ZERO
                   COMPUTE WRK-FTNRATE         =   SPA-FTNRATE
                                               /   100
                   MOVE    WRK-FTNRATE         TO  WRK-Z9
                   MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
                   MOVE    WRK-FTNRATEMEI  TO  MDCRES-COMBINATION-RATE
               ELSE
                   MOVE    "0"             TO  MDCRES-COMBINATION-RATE
               END-IF
           END-IF
      *
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納データ編集処理
      *****************************************************************
       20020-SYUNOU-PARA-SEC        SECTION.
      *
           MOVE    SPACE               TO  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-SYUNOU-AREA
           MOVE    ZERO                TO  LNK-SYUNOU-MAX
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
      *        収納
               ADD     1                   TO  LNK-SYUNOU-MAX
               IF      LNK-SYUNOU-MAX      <=  15
                   MOVE    PARA-DATA-REC       TO  LNK-SYUNOU-REC
                                                       (LNK-SYUNOU-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20020-SYUNOU-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正前の収納編集処理
      *****************************************************************
       20025-TEISEI-HENSYU-SEC        SECTION.
      *
           INITIALIZE                  TEI-SYUNOU-REC
           MOVE    LNK-SYUNOU2-REC (1) TO  SYUNOU-REC
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TEI-SYUNOU-REC
                   MOVE    ZERO                TO  FLG-SYUNOU
               ELSE
                   MOVE    1                   TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20025-TEISEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤削除処理
      *****************************************************************
       20021-ZAIDELETE-SEC        SECTION.
      *
      *    削除区分解除
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL      (IDW     >   LNK-WKSRYACT-MAX)
               MOVE    ZERO            TO  LNK-WKSRYAPI-DELKBN(IDW)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   30 )
                         OR   (MDCREQ-DEL-NUMBER (IDX) =   SPACE)
                         OR   (WRK-ERRCD       NOT =   SPACE )
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-DEL-NUMBER (IDX) TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   2     )   OR
                      (SNUM-SYOSUU         >   0     )
                   MOVE    "20"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-DELNUM
               END-IF
      *
               MOVE    ZERO                TO  FLG-DELOK
               PERFORM VARYING     IDW     FROM    1   BY  1
                       UNTIL      (IDW     >   LNK-WKSRYACT-MAX)
                              OR  (FLG-DELOK   =   1  )
                              OR  (WRK-ERRCD   NOT =   SPACE )
                   IF      LNK-WKSRYAPI-RENNUM (IDW)    =   WRK-DELNUM
                       MOVE    1           TO  LNK-WKSRYAPI-DELKBN(IDW)
                       MOVE    1           TO  FLG-DELOK
                   END-IF
               END-PERFORM
               IF      FLG-DELOK       =   ZERO
                   MOVE    "20"            TO  WRK-ERRCD
               END-IF
           END-PERFORM
      *
           .
       20021-ZAIDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納内容処理
      *****************************************************************
       20022-SYUNOU-HEN-SEC        SECTION.
      *
      *
           INITIALIZE                  PARA-WKSRYACT-AREA
           MOVE    ZERO                TO  IDW2
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   LNK-WKSRYACT-MAX
               MOVE    ZERO                TO  FLG-DEL
      *        包括分は対象外
               IF     (LNK-WKSRY-HKNCOMBI (IDW)    =   "9999")
                   OR (LNK-WKSRY-CONTKBN  (IDW)    =   "H"   )
                   MOVE    1               TO  FLG-DEL
               END-IF
               IF      LNK-WKSRYAPI-DELKBN (IDW)   NOT =   ZERO
                   MOVE    1               TO  FLG-DEL
               END-IF
               IF      FLG-DEL             =   ZERO
                   ADD     1               TO  IDW2
                   MOVE    LNK-WKSRYACT-REC (IDW)
                                           TO  PARA-WKSRYACT-REC(IDW2)
               END-IF
           END-PERFORM
      *
      *    収納　負担金計算
           INITIALIZE                  ORCSCSYUNOUGAREA
           MOVE    "K08"               TO  ORCSCSYU-SYORIPG
      *    初期設定編集
           MOVE    "1"                 TO  ORCSCSYU-SYORIKBN
           INITIALIZE                  LNK-ORCS01-REC
      *
           MOVE    IDW2                TO  ORCSCSYU-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-SYUNOU2-AREA
           MOVE    LNK-SYUNOU-AREA     TO  LNK-SYUNOU2-AREA
           CALL    "ORCSCSYUNOUG"      USING
                                       ORCSCSYUNOUGAREA
                                       LNK-ORCS01-REC
                                       SPA-AREA
                                       LNK-SYUNOU2-AREA
                                       PARA-WKSRYACT-AREA
           MOVE    ORCSCSYU-SYUNOU-MAX TO  LNK-SYUNOU2-MAX
      *
           .
       20022-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金・請求額編集処理
      *****************************************************************
       20023-NYUKIN-HEN-SEC        SECTION.
      *
      *    今回請求額
           IF      SPA-SYORIFLG        =   1
      *        請求額（前回調整金を除く）
               IF      SYU-SKYMONEY    <   SYU-CHOSEI
                   MOVE    ZERO            TO  WRK-TEIKEI-SKYMONEY
               ELSE
                   COMPUTE WRK-TEIKEI-SKYMONEY
                                           =   SYU-SKYMONEY
                                           -   SYU-CHOSEI
               END-IF
      *        今回未入力であれば収納の調整金
               IF      MDCREQ-AD-MONEY1    =   SPACE
                   MOVE    SYU-CHOSEI1         TO  WRK-CHOSEI1
               END-IF
               IF      MDCREQ-AD-MONEY2    =   SPACE
                   MOVE    SYU-CHOSEI2         TO  WRK-CHOSEI2
               END-IF
           ELSE
               MOVE    SYU-SKYMONEY       TO  WRK-TEIKEI-SKYMONEY
           END-IF
      *
      *    今回合計請求額（請求額＋調整金）
           COMPUTE WRK-SKYMONEY-GOK    =   WRK-TEIKEI-SKYMONEY
                                       +   WRK-CHOSEI1
                                       +   WRK-CHOSEI2
           IF      WRK-SKYMONEY-GOK    <   ZERO
               MOVE    "22"            TO  WRK-ERRCD
           END-IF
      *!!
      *    入金上限額
           MOVE    ZERO                TO  WRK-NYUKIN-GOK
           MOVE    ZERO                TO  WRK-HENKIN-GOK
           IF      SPA-SYORIFLG        =   1
      *        今回請求額−前回請求額 
               COMPUTE WRK-NYUKIN-GOK      =   WRK-SKYMONEY-GOK
                                           -   TEI-SYU-SKYMONEY
               IF      WRK-NYUKIN-GOK      <   ZERO
                   MOVE    WRK-NYUKIN-GOK      TO  WRK-HENKIN-GOK
                   MOVE    ZERO                TO  WRK-NYUKIN-GOK
               END-IF
           ELSE
               MOVE    WRK-SKYMONEY-GOK    TO  WRK-NYUKIN-GOK
           END-IF
      *
      *    入金額チェック
           IF      WRK-NYUKIN-OK       =   1
               IF      WRK-NYUKIN-GOK      <   WRK-NYUKIN
                   MOVE    "21"            TO  WRK-ERRCD
               END-IF
           ELSE
      *        入金額設定
               IF      WRK-NYUKIN-GOK     >   ZERO
      *            今回請求額
                   MOVE    WRK-NYUKIN-GOK      TO  WRK-NYUKIN
               ELSE
                   MOVE    ZERO                TO  WRK-NYUKIN
               END-IF
               IF      WRK-NYUKIN-JYOTAI   =   "2"
      *            入金状態が未入金の時、ゼロ
                   MOVE    ZERO                TO  WRK-NYUKIN
               END-IF
           END-IF
      *
      *    訂正時の返金
           IF      SPA-SYORIFLG        =   1
               IF      WRK-HENKIN-OK       =   "1"
                   IF     WRK-HENKIN-GOK   >   WRK-HENKIN
                       MOVE    "29"        TO  WRK-ERRCD
                   END-IF
               ELSE
                   MOVE    WRK-HENKIN-GOK  TO  WRK-HENKIN
               END-IF
           ELSE
               MOVE    ZERO            TO  WRK-HENKIN
           END-IF
      *
           .
       20023-NYUKIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-HENSYU-01-SEC        SECTION.
      *
      *    診療明細編集
           PERFORM 200211-MEDICAL-OUTHEN-SEC
      *
      *    収納情報編集
           IF     (LNK-SYUNOU2-MAX     =   1      )
           AND    (SPA-HKNCOMBINUM NOT =   "9999" )
               MOVE    LNK-SYUNOU2-REC (1) TO  SYUNOU-REC
               PERFORM 200212-SYUNOU-OUTHEN-SEC
           END-IF
      *
           .
       20021-HENSYU-01-EXT.
           EXIT.
      *****************************************************************
      *    明細編集（登録）処理
      *****************************************************************
       200211-MEDICAL-OUTHEN-SEC      SECTION.
      *
      *    診療内容編集サブ
           INITIALIZE                  ORAPI021SUB2V3AREA
           MOVE    "1"                 TO  ORAPISUB2-SYORIKBN
           MOVE    LNK-WKSRYACT-MAX    TO  ORAPISUB2-WKSRYACT-MAX
           CALL    "ORAPI021SUB2V3"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPI021SUB2V3AREA
                                       LNK-WKSRYACT-AREA
      *
           INITIALIZE                  MDCRES-MEDICAL-INF-G
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   120   )
                       OR  (ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           =   SPACE )
               MOVE    ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           TO  MDCRES-MDC-CLASS-KBN(IDX)
               MOVE    ORAPISUB2-MDC-CLASS (IDX)
                                           TO  MDCRES-MDC-CLASS (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NAME (IDX)
                                           TO  MDCRES-MDC-CLASS-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NUMBER (IDX)
                                           TO  MDCRES-MDC-CLASS-NUMBER
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-POINT (IDX)
                                           TO  MDCRES-MDC-POINT (IDX)
               MOVE    ORAPISUB2-MDC-MONEY (IDX)
                                           TO  MDCRES-MDC-MONEY (IDX)
      *        剤区分（包括分）
               IF       ORAPISUB2-MDC-CODE (IDX)  =   "1"
                    MOVE    "True"         TO  MDCRES-INCLUSION-CLASS
                                                                (IDX)
               END-IF
               MOVE    ORAPISUB2-DEL-NUMBER (IDX)
                                           TO  MDCRES-DEL-NUMBER(IDX)
      *        剤内容
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   (IDY            >   50    )
                           OR  (ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                               =   SPACE )
                   MOVE    ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CODE
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAME
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NUMBER (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NUMBER
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-MONEY (IDX IDY)
                                       TO  MDCRES-PRSCRPT-MONEY
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  MDCRES-PRSCRPT-ATAI-G
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-GENERIC (IDX IDY)
                                       TO  MDCRES-PRSCRPT-GENERIC
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-GENNAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-GENNAME
                                                            (IDX IDY)
      *            一般名称があれば、名称は削除
                   IF      ORAPISUB2-PRSCRPT-GENNAME (IDX IDY)
                                           NOT =   SPACE
                       MOVE    SPACE   TO  MDCRES-PRSCRPT-NAME
                                                            (IDX IDY)
                   END-IF
                   MOVE    ORAPISUB2-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CONTKBN
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAIFKBN
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-AUTOKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-AUTOKBN
                                                            (IDX IDY)
               END-PERFORM
      *        剤削除区分
               IF      ORAPISUB2-DEL-INF (IDX) =   "1"
                   MOVE    "Delete"            TO  MDCRES-DEL-INF (IDX)
               END-IF
           END-PERFORM
           IF     (ORAPISUB2-ERRCD NOT =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        剤数オーバー
      *******  MOVE    ORAPISUB2-ERRCD     TO  WRK-ERRCD
               MOVE    "13"                TO  WRK-ERRCD
               MOVE    ORAPISUB2-ERRMSG    TO  WRK-ERRMSG
           END-IF
      *
           .
       200211-MEDICAL-OUTHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報編集処理
      *****************************************************************
       200212-SYUNOU-OUTHEN-SEC        SECTION.
      *
      *    伝票番号
           IF     SYU-DENPNUM      NOT =   ZERO
               MOVE    SYU-DENPNUM         TO  MDCRES-INVOICD-NUMBER
           END-IF
      *
      *    保険適用金額
           MOVE    SYU-HKNTEKMONEY     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AI-MONEY
      *
      *    自費金額（自費合計金額）
           COMPUTE WRK-KINGAKU         =   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TOTAL
      *                                保険適応外
                                       +   SYU-TGMONEY (17)
                                       +   SYU-TGMONEY-TAX (17)
           MOVE    WRK-KINGAKU         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-OE-MONEY
      *    薬剤一部負担金
           MOVE    SYU-YKZ-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-DG-SMONEY  
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-OM-SMONEY
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-PI-SMONEY
      *    労災金額合計
      *    労災・自賠責金額
           COMPUTE WRK-KINGAKU         =   SYU-RSISHOSHIN-MONEY
                                       +   SYU-RSISAISHIN-MONEY
                                       +   SYU-RSISDO-MONEY
                                       +   SYU-RSIETC-MONEY
           MOVE    WRK-KINGAKU         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-LSI-TOTALMONEY
      *
      *    請求点数情報
      *    保険点数明細
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   16
      *        手術料・療養担当手当は、点数ありのみ
               IF     (IDX             =   15   OR  16  )
                  AND (SYU-HKNTEN (IDX)    =   ZERO     )
                   CONTINUE
               ELSE
      *
                   MOVE    TBL-HKNTEN-MEI(IDX)
                                           TO  MDCRES-AC-POINT-NAME(IDX)
                   MOVE    SYU-HKNTEN(IDX) TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-AC-POINT (IDX)
               END-IF
           END-PERFORM
      *    保険合計点数
           MOVE    SYU-HKNTEN(17)      TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AC-TOTAL-POINT
      *
      *    請求金額
           MOVE    WRK-TEIKEI-SKYMONEY TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AC-MONEY
      *    入金金額
           MOVE    WRK-NYUKIN          TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-IC-MONEY
      *    調整金額１
           MOVE    WRK-CHOSEI1         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AD-MONEY1
      *    調整金額２
           MOVE    WRK-CHOSEI2         TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-AD-MONEY2
      *
      *    今回請求額合計(入金上限）
           MOVE    WRK-NYUKIN-GOK      TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-TOTAL-AC-MONEY
      *    返金額（訂正）
           IF     (SPA-SYORIFLG        =   1    )
              AND ((WRK-HENKIN      NOT =   ZERO ) OR
                   (WRK-HENKIN-OK       =   "1"  ))
               MOVE    WRK-HENKIN          TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-RE-MONEY
           END-IF
           IF     (SPA-SYORIFLG        =   1   )
      *        前回までの入金額
               MOVE    TEI-SYU-NYUKIN-TOTAL    TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-OLD-IC-MONEY
      *        前回請求額
               COMPUTE WRK-OLD-SKYMONEY    =   TEI-SYU-SKYMONEY
                                           -   TEI-SYU-CHOSEI
               MOVE    WRK-OLD-SKYMONEY    TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-OLD-AC-MONEY
      *        前回調整金額１
               MOVE    TEI-SYU-CHOSEI1     TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-OLD-AD-MONEY1
      *        前回調整金額２
               MOVE    TEI-SYU-CHOSEI2     TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDCRES-OLD-AD-MONEY2
           END-IF
           .
       200212-SYUNOU-OUTHEN-EXT.
           EXIT.
      *****************************************************************
      *    データ保存処理
      *****************************************************************
       20029-PARA-INS-SEC     SECTION.
      *
      *    収納データ更新
           PERFORM 200292-SYUNOU-UPD-SEC
           .
       20029-PARA-INS-EXT.
           EXIT.
      *****************************************************************
      *     パラメタファイル更新処理
      *****************************************************************
       200291-WKSRYACT-UPD-SEC          SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           PERFORM 30012-PARA-DBDELETE-SEC
      *
           MOVE    ZERO                TO  IDW2
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   LNK-WKSRYACT-MAX)
      *        削除区分の登録
               ADD     1                   TO  IDW2
      *        TBL_PARAに保存
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "K02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "WKSRYACT"          TO  PARA-FILEMEI
               MOVE    IDW2                TO  PARA-EDANUM
               MOVE    LNK-WKSRYACT-REC(IDX)
                                               TO  PARA-DATA-REC
      *
                PERFORM 30010-PARA-DBINSERT-SEC
           END-PERFORM
           .
       200291-WKSRYACT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ追加処理
      *****************************************************************
       30010-PARA-DBINSERT-SEC     SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
      *
           .
       30010-PARA-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ追加処理
      *****************************************************************
       30012-PARA-DBDELETE-SEC     SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
           .
       30012-PARA-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納データ更新処理
      *****************************************************************
       200292-SYUNOU-UPD-SEC           SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
           PERFORM 30012-PARA-DBDELETE-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU2-MAX
      *        TBL_PARAに保存
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "K02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "SYUNOU"            TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    LNK-SYUNOU2-REC(IDX)
                                           TO  PARA-DATA-REC
      *
               PERFORM 30010-PARA-DBINSERT-SEC
      *
           END-PERFORM
           .
       200292-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       2003-SYORI-02-SEC        SECTION.
      *
      *    レスポンス番号チェック
           IF      WRK-RESPONSE-NUMBER =   "05"
               CONTINUE
           ELSE
               MOVE    "09"                TO  WRK-ERRCD
               GO      TO      2003-SYORI-02-EXT
           END-IF
      *
      *    入金額等編集
           PERFORM 20031-HENSYU-02-SEC
      *
      *    診療行為内容編集
           PERFORM 20021-HENSYU-01-SEC
      *
           IF      WRK-ERRCD       =   SPACE
      *        登録処理
               PERFORM 20032-TOUROKU-SYORI-SEC
           END-IF
      *
      *    返却内容編集
           IF      SPA-HKNCOMBINUM NOT =   "9999"
               PERFORM 20033-MDCRES-HEN02-SEC
           END-IF
      *
      *    返却レスポンス番号
           IF     (SPA-ERRCD       =   SPACE )
              AND (WRK-ERRCD       =   SPACE )
      *        終了処理
               PERFORM 300-SYORI-END-SEC
      *
               MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療確認処理
      *****************************************************************
       20031-HENSYU-02-SEC        SECTION.
      *
      *    収納レコード編集
           PERFORM 20020-SYUNOU-PARA-SEC
      *
      *    入金・請求額
           IF     (WRK-ERRCD           =   SPACE )
           AND    (SPA-HKNCOMBINUM NOT =   "9999" )
      *        訂正前の収納編集
               IF     (SPA-SYORIFLG        =   1     )
                   PERFORM 20025-TEISEI-HENSYU-SEC
               END-IF
      *
               MOVE    LNK-SYUNOU-REC (1)  TO  SYUNOU-REC
               PERFORM 20023-NYUKIN-HEN-SEC
           END-IF
      *
           .
       20031-HENSYU-02-EXT.
           EXIT.
      *****************************************************************
      *     登録　処理
      *****************************************************************
       20032-TOUROKU-SYORI-SEC          SECTION.
      *
      *    ワーク診療行為更新
           PERFORM 20033-WKSRYACT-UPD-SEC
      *
      *    外来更新処理
           INITIALIZE                  ORCSCGAIRAIAREA
      *    登録・更新
           MOVE    "1"                 TO  ORCSCGAIRAI-KBN
      *    請求書作成区分（発行なし）＃＃変更するかも
           MOVE    "0"                 TO  ORCSCGAIRAI-HAKFLG
      *    今回請求額
           MOVE    SYU-SKYMONEY        TO  ORCSCGAIRAI-KON-SKYMONEY
      *    入金額
           MOVE    WRK-NYUKIN          TO  ORCSCGAIRAI-NYUKIN
      *    入金方法
           MOVE    WRK-NYUKIN-HOHO     TO  ORCSCGAIRAI-NYUKIN-HOHO
      *    発行方法（診療科・保険組合せ別のみ）
           MOVE    1                   TO  ORCSCGAIRAI-HAKHOUFLG
      *    伝票発行日
           MOVE    WRK-BASE-YMD        TO  ORCSCGAIRAI-HAKYMD
      *
      *    収納情報
           MOVE    LNK-SYUNOU-MAX      TO  ORCSCGAIRAI-SYUNOU-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU-MAX
               MOVE    LNK-SYUNOU-REC(IDX) TO  SYUNOU-REC
      *        調整金
               MOVE    WRK-CHOSEI1         TO  SYU-CHOSEI1
               MOVE    WRK-CHOSEI2         TO  SYU-CHOSEI2
      *        調整金合計
               COMPUTE SYU-CHOSEI          =   SYU-CHOSEI1
                                           +   SYU-CHOSEI2
      *        請求額（調整金なし）
               MOVE    WRK-TEIKEI-SKYMONEY
                                           TO  SYU-SKYMONEY
               IF     (SPA-SYORIFLG        =   1 )
                 AND  (WRK-HENKIN      NOT =   ZERO )
      **           COMPUTE ORCSCGAIRAI-SYU-HENKIN (IDX)
      *                                        =   WRK-HENKIN
      **                                       *   -1
                   MOVE    WRK-HENKIN      TO  ORCSCGAIRAI-SYU-HENKIN
                                                                (IDX)
                   MOVE    ZERO                TO  SYU-NYUKIN-TOTAL
               ELSE
      *            入金額
                   MOVE    WRK-NYUKIN          TO  SYU-NYUKIN-TOTAL
               END-IF
      *
               MOVE    SYUNOU-REC          TO  ORCSCGAIRAI-SYUNOU-REC
                                                               (IDX)
           END-PERFORM
           CALL    "ORCSCGAIRAI"       USING
                                       SPA-AREA
                                       ORCSCGAIRAIAREA
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *    伝票番号を編集する
           IF     (SPA-ERRCD           =   SPACE )
             OR   (SPA-ERRCD(1:1)      =   "K"   )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   LNK-SYUNOU-MAX )
                   MOVE    ORCSCGAIRAI-SYUNOU-REC (IDX)
                                       TO  LNK-SYUNOU-REC(IDX)
               END-PERFORM
           END-IF
      *
      *    警告エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-ERRMSG2     NOT =   SPACE)
                   MOVE    "W"                 TO  WRK-ERRKBN
                   MOVE    "30"                TO  WRK-ERRCD
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG-02
               END-IF
           ELSE
      *        エラー
               PERFORM 200329-ERRCD-MSG-SEC
               MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
               IF      WRK-ERRMSG-02   NOT =   SPACE
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG-02
               END-IF
               IF      SPA-ERRCD(1:1)      =   "K"
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20032-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *     パラメタファイル更新処理
      *****************************************************************
       20033-WKSRYACT-UPD-SEC          SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           PERFORM 30012-PARA-DBDELETE-SEC
      *
           MOVE    ZERO                TO  IDW2
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   LNK-WKSRYACT-MAX)
      *        削除分以外を保存
               IF      LNK-WKSRYAPI-DELKBN (IDX)   =   ZERO
                   ADD     1                   TO  IDW2
      *            TBL_PARAに保存
                   INITIALIZE                      PARA-REC
                   MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
                   MOVE    "K02"               TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                   MOVE    IDW2                TO  PARA-EDANUM
      *API         一般名記載用コメントのクリア
                   PERFORM VARYING    IDY      FROM    1   BY  1
                           UNTIL      IDY      >   5
                       IF     (LNK-WKSRY-SRYCD (IDX IDY)(1:1) =   "6")
                           MOVE    SPACE       TO  LNK-WKSRY-INPUTCOMENT
                                                              (IDX IDY)
                       END-IF
                   END-PERFORM
      *
                   MOVE    LNK-WKSRYACT-REC(IDX)
                                               TO  PARA-DATA-REC
      *
                    PERFORM 30010-PARA-DBINSERT-SEC
               END-IF
           END-PERFORM
           .
       200291-WKSRYACT-UPD-EXT.
           EXIT.
      *****************************************************************
      *    返却内容編集処理
      *****************************************************************
       20033-MDCRES-HEN02-SEC          SECTION.
      *
      *    収納情報編集
           IF      LNK-SYUNOU-MAX      =   1
               MOVE    LNK-SYUNOU-REC (1)  TO  SYUNOU-REC
      *        請求金額等　収納の内容
               IF     (SPA-ERRCD       =   SPACE )
                 AND  (WRK-ERRCD       =   SPACE )
                   MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY-GOK
                   MOVE    SYU-CHOSEI1         TO  WRK-CHOSEI1
                   MOVE    SYU-CHOSEI2         TO  WRK-CHOSEI2
                   MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN
                   IF      SYU-SKYMONEY    <   SYU-CHOSEI
                       MOVE    ZERO            TO  WRK-TEIKEI-SKYMONEY
                   ELSE
                       COMPUTE WRK-TEIKEI-SKYMONEY
                                           =   SYU-SKYMONEY
                                           -   SYU-CHOSEI
                   END-IF
               END-IF
      *
               PERFORM 200212-SYUNOU-OUTHEN-SEC
           END-IF
      *
           .
       20033-MDCRES-HEN02-EXT.
           EXIT.
      *****************************************************************
      *     パラメタファイルより編集処理
      *****************************************************************
       200234-WKSRYACT-INIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "K02 LNK-WKSRYACT-MAX 400 OVR"
                   MOVE    "50"            TO  WRK-ERRCD
               ELSE
                   MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200234-WKSRYACT-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
      *
           IF      ORCSAPIHKN-THKNCOMBI (IDX)
                                   NOT =   ORCSAPIHKN-OT-HKNCOMBI
      *        保険組合せ番号なし
               MOVE    "19"                TO  WRK-ERRCD
               GO      TO      2002101-HKNHEN-TBL-EXT
           END-IF
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-CLASS
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-NUMBER
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-NAME
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  MDCRES-MAIN-MARK
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  MDCRES-MAIN-NUMBER
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  MDCRES-MAIN-CONTINUATION
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  MDCRES-MAIN-ASSISTANCE
           MOVE    ORCSAPIHKN-HOJOKBN-NAME (IDX)
                                   TO  MDCRES-MAIN-ASSI-NAME
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  MDCRES-MAIN-FAMILY
      *    被保険者名
      *    MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
      *                            TO  MDCRES-MAIN-POLICYHOLDER
      *    有効開始日
      *    MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
      *                            TO  MDCRES-MAIN-START-DATE
      *    有効終了日
      *    MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
      ***                          TO  MDCRES-MAIN-END-DATE
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  MDCRES-INSURANCE-CLASS
                                                             (IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  MDCRES-INSURANCE-NAME
                                                             (IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  MDCRES-PROVIDER-NUMBER
                                                             (IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  MDCRES-RECIPIENT-NUMBER
                                                             (IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-RATE-OUTPATIENT
                                                             (IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-MONEY-OUTPATIENT
                                                             (IDY2)
      *        有効開始日
      *        MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
      *                                TO  MDCRES-INSURANCE-START-DATE
      *                                                      (IDY2)
      *        有効終了日
      *        MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
      *                                TO  MDCRES-INSURANCE-END-DATE
      *                                                      (IDY2)
      *        入院−負担率（割）
      *        MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
      *                                TO  MDCRES-RATE-ADMISSION
      *                                                      (IDY2)
      *        入院−固定額
      *        MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
      *                                TO  MDCRES-MONEY-ADMISSION
      *                                                      (IDY2)
           END-PERFORM
      *
      *    労災区分
           MOVE    ORCSAPIHKN-RSI-HKNKBN (IDX) TO  WRK-RSI-HKNKBN
           MOVE    ORCSAPIHKN-HKNNUM (IDX)     TO  WRK-HKNNUM
           .
       2002101-HKNHEN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *     SPATMPのＳＰＡの取得処理
      *****************************************************************
       2008-SPATMP-SEL-SEC            SECTION.
      *
      *    UIDコードをTERMID
           MOVE    MDCREQ-ORCA-UID     TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                       INTO    SPA-TERMID
           END-STRING
      *
      *    TBL-SPATMP
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-AREA"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-AREA
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           IF      SPA-TERMID      NOT =   SPATMP-TERMID
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
      *    レスポンス番号取得
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 9010-APIPARA-KEYREAD-SEC
      *
           IF      FLG-APIPARA         =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               MOVE    APIPARA-KARTE-UID   TO  WRK-KARTE-UID
               MOVE    APIPARA-PTNUM       TO  WRK-PARA-PTNUM
           ELSE
               MOVE    SPACE               TO  WRK-APIV3DATA-AREA
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-K02"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-K02
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           .
       2008-SPATMP-SEL-EXT.
           EXIT.
      *****************************************************************
      *     API処理登録処理
      *****************************************************************
       2008-APIPARAINS-SEC            SECTION.
      *
      *    削除
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    登録
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
      *    レスポンス番号・電子カルテUID保存
           MOVE    SPACE               TO  WRK-APIV3DATA-AREA
           MOVE    MDCRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
           MOVE    WRK-DENPNUM         TO  WRK-PARA-DENPNUM
           MOVE    WRK-SYORI-MODE      TO  WRK-PARA-MODE
      *
           MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
      *
           MOVE    MDCRES-KARTE-UID    TO  APIPARA-KARTE-UID
           MOVE    WRK-PTNUM           TO  APIPARA-PTNUM
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
      *
           .
       2008-APIPARAINS-EXT.
           EXIT.
      *****************************************************************
      *     SPATMPの登録処理
      *****************************************************************
       2008-SPATMP-INS-SEC            SECTION.
      *
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-AREA"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
      *
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "K02-SPA"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-K02             TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
           .
       2008-SPATMP-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ一時保存テーブル追加
      *****************************************************************
       20081-SPATMP-INSERT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "80"                TO  WRK-ERRCD
           END-IF
           .
       20081-SPATMP-INSERT-EXT.
           EXIT.
      *****************************************************************
      *     SPATMPの削除処理
      *****************************************************************
       2009-SPATMP-DEL-SEC            SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "API1"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SPATMP DELETE ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
           END-IF
      *
           .
       2009-SPATMP-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-SYORI-END-SEC          SECTION.
      *
      *    一時データ削除・排他制御解除
           INITIALIZE                  ORAPIV3SUB01AREA
           MOVE    "1"                 TO  APIV3SUB01-SYORI
           MOVE    SPA-TERMID          TO  APIV3SUB01-TERMID
           MOVE    MDCREQ-KARTE-UID    TO  APIV3SUB01-KARTE-UID
           MOVE    "AP21"              TO  APIV3SUB01-API-GYOUMUID
           MOVE    "API21V3"           TO  APIV3SUB01-API-FILEMEI
           MOVE    WRK-PTNUM           TO  APIV3SUB01-PTNUM
           MOVE    "K02"               TO  APIV3SUB01-PARA-GYOUMUID
           CALL    "ORAPIV3SUB01"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPIV3SUB01AREA
           IF      APIV3SUB01-ERRCD     NOT =   SPACE
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "60"                TO  WRK-ERRCD
                   MOVE    APIV3SUB01-ERRMSG    TO  WRK-ERRMSG
               END-IF
           END-IF
           .
       300-SYORI-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200329-ERRCD-MSG-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0002"
                   STRING  "患者情報が検索できませんでした。"
                           "患者情報テーブルを確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "40"            TO  WRK-ERRCD
               WHEN    "0003"
                   STRING  "伝票番号が取得できませんでした。"
                           "システム管理テーブルを確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "41"            TO  WRK-ERRCD
               WHEN    "0005"
                   STRING  "ＤＢ更新がエラーになりました。"
                           "ＳＥに連絡して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "42"            TO  WRK-ERRCD
               WHEN    "0105"
                   STRING  "この診療科の当日の受診が９回登録済です。"
                           "これ以上登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "43"            TO  WRK-ERRCD
               WHEN    "9000"
                   MOVE    "算定履歴の更新ができませんでした"
                                           TO  WRK-ERRMSG
                   MOVE    "44"            TO  WRK-ERRCD
               WHEN    "0051"
                   STRING  "中間データに不整合が発生しました。"
                           "強制終了後、"
                           "初回接続からおこなってください。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "45"            TO  WRK-ERRCD
               WHEN    "0052"
                   STRING  "外用薬剤の回数が１回以上になります。"
                           "同じ剤は回数をまとめて入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
                   MOVE    "46"            TO  WRK-ERRCD
               WHEN    OTHER
                   MOVE    SPA-ERRCD       TO  WRK-ERRMSG
                   MOVE    "47"            TO  WRK-ERRCD
           END-EVALUATE
           .
       200329-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       700-FILE-DELETE-SEC          SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "API1"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "API1"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       700-FILE-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "01"
               MOVE    "患者番号が未設定です"
                                               TO  WRK-ERRMSG
      *    WHEN    "02"
      *        MOVE    "診療科が未設定です"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "03"
      *        MOVE    "ドクターが未設定です"
      *                                        TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "電子カルテＵＩＤが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "オルカＵＩＤが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "06"
               MOVE    "前回と診療日付が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "07"
               MOVE    "前回と患者番号が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "08"
               STRING  "初回接続が完了していません。"
                       "初回接続からおこなってください。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "09"
               STRING  "処理の順番が違います。"
                       "正しい順で処理を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE     "基準日付が暦日ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE     SPACE                  TO  WRK-ERRMSG
           WHEN    "15"
               STRING  "入金方法区分が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "16"
               STRING  "入金額の数値エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "17"
               STRING  "調整金１の数値エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "18"
               STRING  "調整金２の数値エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "19"
               STRING  "保険組合せ番号が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "20"
               MOVE     "剤削除番号が存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               STRING  "今回請求額＜入金額となります。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "22"
               STRING  "今回請求額がマイナスとなります。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "23"
               STRING  "包括分入力は収納を作成しません。"
                       "入金額の設定はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "24"
               STRING  "包括分入力は収納を作成しません。"
                       "調整金の設定はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "25"
               STRING  "包括分入力は収納を作成しません。"
                       "返金額の設定はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "26"
               STRING  "訂正処理ではありません。"
                       "返金額の設定はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "27"
               STRING  "伝票番号が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "28"
               STRING  "訂正処理の順番が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "29"
               STRING  "今回返金額以上です。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "30"
      *        ORCSCGAIRAI 警告
               CONTINUE
           WHEN    "40"  THRU  "49"
      *        ORCSCGAIRAI エラー
               CONTINUE
           WHEN    "50"
               STRING  "診療行為が４００行をオーバーします。"
                       "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "80"
               STRING  "一時データ出力エラーです。"
                       "強制終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
      *共通エラー
           WHEN    "89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "他端末使用中。"        TO  WRK-ERRMSG
           WHEN    "91"
     ********  MOVE    "処理区分未設定"        TO  WRK-ERRMSG
               MOVE    "リクエスト番号不正"    TO  WRK-ERRMSG
           WHEN    "97"
               MOVE   "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤ未登録。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
      *    電子カルテキー
           MOVE    MDCREQ-KARTE-UID    TO  SLOCK-KARTE-UID
      *    API+オルカＵＩＤ をTERMID
           MOVE    SPA-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
           END-IF
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理(削除)
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
      *    API+オルカＵＩＤ をTERMID
           MOVE    SPA-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータテーブル検索処理
      *****************************************************************
       900-PARA-CALL-SEC                SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-PARA-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      **** PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       9100-SPATMP-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-APIPARA-KEYREAD-SEC         SECTION.
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  APIPARA-REC
                   MOVE    ZERO            TO  FLG-APIPARA
               ELSE
                   INITIALIZE                  APIPARA-REC
                   MOVE    1               TO  FLG-APIPARA
               END-IF
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-APIPARA-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       990-SPATMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルCALL処理
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
