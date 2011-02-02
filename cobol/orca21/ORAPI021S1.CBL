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
       PROGRAM-ID.         ORAPI021S1.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール(サンプル)
      *  コンポーネント名  : 診療行為
      *  管理者            :
      *  作成日付    作業者        記述
      *  09/04/07    NACL−竹田　　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-多々納  10/12/06  標準対応
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
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
           03  CNT-BYOCNT              PIC 9(04).
      *    一時領域
      *
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
      *    受付ＩＤ
           03  WRK-UKEID               PIC 9(05).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC 9(04).
           03  WRK-IN-HKNCOMBI         PIC 9(04).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *    ユーザプログラム
           03  WRK-PTID                PIC X(10).
           03  WRK-RENNUM              PIC 9(02).
           03  WRK-GMN-SELNUM          PIC 9(02).
           03  WRK-SYORI-FLG           PIC 9(01).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-YMD10       PIC X(10).
           03  WRK-PERFORM-TIME8       PIC X(08).
      *
           03  WRK-PERFORM-YMD         PIC X(08).
           03  WRK-PERFORM-TIME.
               05  WRK-PERFORM-THH     PIC X(02).
               05  WRK-PERFORM-TMM     PIC X(02).
               05  WRK-PERFORM-TSS     PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    claim連携リターンエリア
       01  WRK-ERROR-AREA.
           03  WRK-KBN-CODE            PIC X(01).
           03  WRK-ERR-CODE            PIC 9(02).
           03  WRK-ERR-MSG             PIC X(100).
           03  WRK-OUT-HKNCOMBI        PIC X(04).
           03  WRK-OUT-CNT1            PIC 9(04).
           03  WRK-OUT-CNT2            PIC 9(04).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
           COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *   日付変換サブ
          COPY     "CPORCSGDAY.INC".
      *   患者番号発生サブ
           COPY    "CPORCSP01.INC".
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *    ユーザプログラム起動チェックサブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLREQ.INC".
      *01  FIL-REC        PIC X(50).
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLRES.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
       01  LF04-REC.
           COPY    "CPRLF004.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
      *01  SYSBASE-REC.
      *    COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
      *01  SYSUSER-REC.
      *    COPY    "CPSYSUSER.INC".
      *
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
      *01  HKNCOMBI-REC.
      *    COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
      *01  PTHKNINF-REC.
      *    COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
      *01  PTKOHINF-REC.
      *    COPY    "CPPTKOHINF.INC".
      *
      *　　（ＣＬＡＩＭ接続）
      *    COPY    "CPSK9000.INC".
      *    COPY    "CPSK9001.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "K01.INC".
           COPY    "K02.INC".
           COPY    "K02N.INC".
           COPY    "K021.INC".
           COPY    "K023.INC".
           COPY    "K03.INC".
           COPY    "K04.INC".
           COPY    "K05.INC".
           COPY    "K051.INC".
           COPY    "K052.INC".
           COPY    "K06.INC".
           COPY    "K061.INC".
           COPY    "K062.INC".
           COPY    "K07.INC".
           COPY    "K08.INC".
           COPY    "K081.INC".
           COPY    "K09.INC".
           COPY    "K10.INC".
           COPY    "K97.INC".
           COPY    "K98.INC".
           COPY    "K99.INC".
           COPY    "K03X.INC".
           COPY    "KERR.INC".
           COPY    "KERR2.INC".
           COPY    "KID1.INC".
           COPY    "KID2.INC".
           COPY    "KHELP.INC".
           COPY    "CPAPIMOD.INC".
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
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-APTXMLMAKE-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
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
           INITIALIZE                      XML-MEDICALRES
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
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
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
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APIREQ-CLASS
           WHEN    "01"
      *        登録
               MOVE    1                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *TEST!!
      *        PERFORM 890-ERRCD-MSG-SEC
      *        DISPLAY "必須項目チェックエラー "    WRK-ERRCD
      *                " " WRK-ERRMSG
      *TEST!!
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    APIREQ-CLASS
           WHEN    "01"
      *TEST
               DISPLAY "登録"    
      *TEST
      *        登録処理
               PERFORM 2002-WKSRYACT-ADD-SEC
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
           IF      API-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    API-BODY            TO  MDCREQ-OBJECT
           MOVE    ZERO                TO  MDCREQ-MODE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALREQ
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
           MOVE    "xml_medicalreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    ORCA-BLOB-OBJECT    TO  MDCREQ-OBJECT
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_medicalreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    ORCA-BLOB-OBJECT    TO  MDCREQ-OBJECT
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALREQ
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
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               IF      FLG-SYORI           =   1
                   MOVE    WRK-KEICD           TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "登録処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  MDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MDCRES-API-RESULT-MSG
           END-IF
      *
           IF      API-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_medicalres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  API-BODY 
               MOVE  "application/xml" TO  APIREQ-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
      * CALL    "coblog"    USING   WRK-ERRMSG
           END-IF
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      MDCREQ-OUTPATIENT-CLASS    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-OUTPATIENT-CLASS
           END-IF
           IF      MDCREQ-PATIENTID    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PATIENTID
           END-IF
           IF     MDCREQ-PERFORM-DATE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-DATE
           END-IF
           IF     MDCREQ-PERFORM-TIME  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-TIME
           END-IF
           IF      MDCREQ-APPOINT-DEP-CODE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-APPOINT-DEP-CODE
           END-IF
           IF      MDCREQ-APPOINT-DR-CODE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-APPOINT-DR-CODE
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-CLASS  =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-CLASS
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-NUMBER =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-NUMBER
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-NAME   =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-NAME
           END-IF
           IF  MDCREQ-MAIN-MARK      =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-MARK
           END-IF
           IF  MDCREQ-MAIN-NUMBER    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-NUMBER
           END-IF
           IF  MDCREQ-MAIN-CONTINUATION     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-CONTINUATION
           END-IF
           IF  MDCREQ-MAIN-ASSISTANCE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-ASSISTANCE
           END-IF
           IF  MDCREQ-MAIN-FAMILY     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-FAMILY
           END-IF
           IF  MDCREQ-MAIN-POLICYHOLDER     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-POLICYHOLDER
           END-IF
           IF  MDCREQ-MAIN-START-DATE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-START-DATE
           END-IF
           IF  MDCREQ-MAIN-END-DATE   =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-END-DATE
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               IF      MDCREQ-INSURANCE-CLASS(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  MDCREQ-INSURANCE-CLASS
                                                                  (IDX)
               END-IF
               IF      MDCREQ-INSURANCE-NAME(IDX)    =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-INSURANCE-NAME(IDX)
               END-IF
               IF      MDCREQ-PROVIDER-NUMBER(IDX)   =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-PROVIDER-NUMBER(IDX)
               END-IF
               IF      MDCREQ-RECIPIENT-NUMBER(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-RECIPIENT-NUMBER(IDX)
               END-IF
               IF      MDCREQ-INSURANCE-START-DATE(IDX) =   LOW-VALUE
                   MOVE    SPACE      TO   MDCREQ-INSURANCE-START-DATE
                                                                 (IDX)
               END-IF
               IF      MDCREQ-INSURANCE-END-DATE(IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  MDCREQ-INSURANCE-END-DATE
                                                                (IDX)
               END-IF
           END-PERFORM 
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
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    診療科コードチェック
           IF     (WRK-SRYKA       NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *    ドクターコードチェック
           IF     (WRK-DRCD        NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-DRCD-CHK-SEC
           END-IF
      *
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
      *    診療科
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                           TO  MDCRES-APPOINT-DEP-CODE
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                           TO  WRK-SRYKA
      *    ドクター
           MOVE    MDCREQ-APPOINT-DR-CODE
                                           TO  MDCRES-APPOINT-DR-CODE
           MOVE    MDCREQ-APPOINT-DR-CODE
                                           TO  WRK-DRCD
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
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
      *    診療日をシステム日付へ
           MOVE    WRK-PERFORM-YMD      TO  SPA-SYSYMD
      *
      *    必須入力チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    診療科
           IF      WRK-SRYKA           =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    ドクター
           IF      WRK-DRCD            =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "03"                TO  WRK-ERRCD
               END-IF
           END-IF
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
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-PERFORM-YMD     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
               DISPLAY "Accept Ymd =  " WRK-PERFORM-YMD
           END-IF
      *
           .
       20011-PERFORM-CHK-EXT.
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
           MOVE    SPTID-PTID          TO  SPA-PTID
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
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクタコードチェック
      *****************************************************************
       20011-DRCD-CHK-SEC          SECTION.
      *
      *    ドクタの存在チェック
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了データ登録処理
      *****************************************************************
       2002-WKSRYACT-ADD-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      * 
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO      2002-WKSRYACT-ADD-EXT
           END-IF
      *
      *    診療行為登録処理
           PERFORM 20021-CLAIM-CALL-SEC
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2002-WKSRYACT-ADD-EXT.
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
      *    MOVE    PTINF-HOME-POST     TO  MDCRES-HOME-ZIP-CODE
      *    STRING  PTINF-HOME-ADRS
      *            PTINF-HOME-BANTI    DELIMITED  BY  SPACE
      *                                INTO    MDCRES-HOME-ADDRESSES
      *    END-STRING
      *
           MOVE    WRK-SRYKA-NAME      TO  MDCRES-APPOINT-DEP-NAME
           MOVE    WRK-DRCD-NAME       TO  MDCRES-APPOINT-DR-NAME
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-CLAIM-CALL-SEC        SECTION.
      *
           MOVE    SPACE               TO  LF04-REC
           INITIALIZE                  LF04-REC
      *    入外区分
           IF      MDCREQ-OUTPATIENT-CLASS =  "I"
               MOVE    "true"              TO  LF04-NYUGAIKBN
           END-IF
      *    医療機関ＩＤ
           MOVE    SPA-HOSPID          TO  LF04-CRE001-FACILITYID
      *    診療日付
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  LF04-PERFORM-TIME
      *    患者番号
           MOVE    WRK-PTNUM           TO  LF04-PTID1
           MOVE    WRK-SRYKA           TO  LF04-CRE001-DPERTID
           MOVE    WRK-DRCD            TO  LF04-DOCCD
      *    保険情報
           MOVE    MDCREQ-MAIN-INSURANCE-CLASS
                                       TO  LF04-HKN-HKNSBTCODE (01)
           PERFORM 2001-HKNNUM-SET-SEC
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   4)
               MOVE    MDCREQ-INSURANCE-NAME (IDY)
                                       TO  LF04-HKNKOH-PROVIDER(1 IDY)
               MOVE    MDCREQ-PROVIDER-NUMBER (IDY)
                                       TO  LF04-HKNKOH-FTNJANUM(1 IDY)
               MOVE    MDCREQ-RECIPIENT-NUMBER (IDY)
                                       TO  LF04-HKNKOH-JKYSNUM(1 IDY)
           END-PERFORM
      *    診療情報設定
           PERFORM VARYING   IDY   FROM    1   BY  1
                   UNTIL    (IDY           >   40)
               MOVE    MDCREQ-MDC-CLASS (IDY)
                                           TO  LF04-SRYCD(IDY)
               MOVE    MDCREQ-MDC-CLASS-NUMBER (IDY)
                                           TO  LF04-KAISU(IDY)
      *        剤内容
               PERFORM VARYING   IDZ   FROM    1   BY  1
                       UNTIL    (IDZ           >   40)
                   MOVE    MDCREQ-PRSCRPT-CODE (IDY IDZ)
                                           TO  LF04-IJICD(IDY IDZ)
                   MOVE    MDCREQ-PRSCRPT-NAME (IDY IDZ)
                                           TO  LF04-ALSNAME(IDY IDZ)
                   MOVE    MDCREQ-PRSCRPT-NUMBER(IDY IDZ)
                                           TO  LF04-SURYO(IDY IDZ)
      *
                   IF      MDCREQ-PRSCRPT-CODE (IDY IDZ)   NOT =   SPACE
                       ADD     1               TO  CNT-SRYCNT
                   END-IF
               END-PERFORM
           END-PERFORM
      *    診断履歴情報設定
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   50)
               MOVE    MDCREQ-DIAGNOSIS-NUMBER(IDY)
                                           TO  LF04-SND-SPICOD1(IDY)
               MOVE    MDCREQ-DIAGNOSIS-NAME (IDY)
                                           TO  LF04-SND-SPINAME1(IDY)
               PERFORM VARYING   IDZ   FROM    1   BY  1
                       UNTIL    (IDZ           >   6)
                   MOVE    MDCREQ-DIAGNOSIS-SINGLE-NUMBER(IDY IDZ)
                                           TO  LF04-SNDSPITBL-SPICOD2
                                                          (IDY IDZ)
                   MOVE    MDCREQ-DIAGNOSIS-SINGLE-NAME (IDY IDZ)
                                           TO  LF04-SNDSPITBL-SPINAME2
                                                           (IDY IDZ)
                   IF      MDCREQ-DIAGNOSIS-SINGLE-NUMBER(IDY IDZ)
                                               NOT =   SPACE
                       ADD     1               TO  CNT-BYOCNT
                   END-IF
               END-PERFORM
               IF      MDCREQ-DIAGNOSIS-CATEGORY (IDY)
                                           = "PD"
                   MOVE    "MML0012"       TO  LF04-SNDBNRTBL-BNRTBL1
                                                             (IDY 1)
                   MOVE    "mainDiagnosis"
                                           TO  LF04-SNDBNRTBL-BNRNAME1
                                                              (IDY 1)
               END-IF
               IF      MDCREQ-DIAGNOSIS-SUSPECTEDFLAG (IDY)
                                           = "S"
                   MOVE    "MML0015"       TO  LF04-SNDBNRTBL-BNRTBL1
                                                             (IDY 2)
                   MOVE    "suspectedDiagnosis"
                                           TO  LF04-SNDBNRTBL-BNRNAME1
                                                             (IDY 2)
               END-IF
               MOVE    MDCREQ-DIAGNOSIS-START-DATE(IDY)
                                           TO  LF04-SND-SKNSTARTDATE
                                                             (IDY)
               MOVE    MDCREQ-DIAGNOSIS-END-DATE(IDY)
                                           TO  LF04-SND-SKNENDDATE(IDY)
               EVALUATE    MDCREQ-DIAGNOSIS-OUTCOME(IDY)
                   WHEN    "D"
                     MOVE  "died"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "F"
                     MOVE  "fullyRecovered"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "W"
                     MOVE  "worsening"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "N"
                     MOVE  "unchanged"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "R"
                     MOVE  "recovering"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "U"
                     MOVE  "pause"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "S"
                     MOVE  "continued"
                                       TO  LF04-SND-TENKI(IDY)
               END-EVALUATE
           END-PERFORM
      *
           INITIALIZE                  WRK-ERROR-AREA
           MOVE    "1"                 TO  WRK-KBN-CODE
           CALL    "ORCL0031"          USING LF04-REC
                                             WRK-ERROR-AREA
           IF      WRK-OUT-HKNCOMBI    NOT =   ZERO
      *        対象保険組合せ情報編集
               PERFORM 200210-HKNCOMBI-HEN-SEC
           END-IF
           IF      WRK-ERR-CODE    NOT =   ZERO
      ******** MOVE    WRK-ERR-CODE        TO  WRK-ERRCD
               MOVE    "80"                TO  WRK-ERRCD
               MOVE    WRK-ERR-MSG         TO  WRK-ERRMSG
           ELSE
               IF      WRK-OUT-HKNCOMBI    =   ZERO
                                           OR  SPACE
                   MOVE    "K2"                TO  WRK-KEICD
               END-IF
               IF     (CNT-SRYCNT          >   ZERO )  AND
                      (WRK-OUT-CNT1        =   ZERO )
                   MOVE    "20"                TO  WRK-ERRCD
               END-IF
               IF     (CNT-BYOCNT          >   ZERO )  AND
                      (WRK-OUT-CNT2        =   ZERO )
                   MOVE    "21"                TO  WRK-ERRCD
               END-IF
               IF     (CNT-SRYCNT          =   ZERO )  AND
                      (CNT-BYOCNT          =   ZERO )
                   MOVE    "22"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       20021-CLAIM-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       2001-HKNNUM-SET-SEC     SECTION.
      *
           EVALUATE    MDCREQ-MAIN-INSURANCE-CLASS
               WHEN    "060"
                   MOVE    "00"            TO  LF04-HKN-HKNSBTCODE(01)
               WHEN    "971"
                   MOVE    "R1"            TO  LF04-HKN-HKNSBTCODE(01)
               WHEN    "973"
                   MOVE    "R3"            TO  LF04-HKN-HKNSBTCODE(01)
               WHEN    "975"
                   MOVE    "K5"            TO  LF04-HKN-HKNSBTCODE(01)
               WHEN    OTHER
                   EVALUATE    MDCREQ-MAIN-INSURANCE-CLASS (1:2)
                   WHEN    "98" 
                       MOVE  "Z"           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(1:1)
                       MOVE    MDCREQ-MAIN-INSURANCE-CLASS (3:1)
                                           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(2:1)
                   WHEN    "90" 
                       MOVE  "A"           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(1:1)
                       MOVE    MDCREQ-MAIN-INSURANCE-CLASS (3:1)
                                           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(2:1)
                   WHEN    "91" 
                       MOVE  "B"           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(1:1)
                       MOVE    MDCREQ-MAIN-INSURANCE-CLASS (3:1)
                                           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(2:1)
                   WHEN    OTHER
                       MOVE    MDCREQ-MAIN-INSURANCE-CLASS (2:2)
                                           TO  LF04-HKN-HKNSBTCODE
                                                          (01)(1:2)
                   END-EVALUATE
           END-EVALUATE
      *
           .
       2001-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       200210-HKNCOMBI-HEN-SEC     SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    追加処理
           MOVE    "1"                 TO  ORCSAPIHKN-KBN
      *    診療科
           MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *    保険組合せ
           MOVE    WRK-OUT-HKNCOMBI    TO  ORCSAPIHKN-IN-HKNCOMBI
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
      *
      *    出力領域保険情報編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   20  )
                         OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   SPACE  )
               IF      ORCSAPIHKN-THKNCOMBI (IDX)  =   WRK-OUT-HKNCOMBI
                   PERFORM 2002101-HKNHEN-TBL-SEC
                   MOVE    20                  TO  IDX
               END-IF
           END-PERFORM
           .
       200210-HKNCOMBI-HEN-EXT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
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
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  MDCRES-MAIN-FAMILY
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  MDCRES-MAIN-POLICYHOLDER
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  MDCRES-MAIN-START-DATE
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  MDCRES-MAIN-END-DATE
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
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  MDCRES-INSURANCE-START-DATE
                                                             (IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  MDCRES-INSURANCE-END-DATE
                                                             (IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  MDCRES-RATE-ADMISSION
                                                             (IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  MDCRES-MONEY-ADMISSION
                                                             (IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-RATE-OUTPATIENT
                                                             (IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-MONEY-OUTPATIENT
                                                             (IDY2)
           END-PERFORM
      *
           .
       2002101-HKNHENSYU-EXT.
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
               MOVE    "患者番号未設定"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "診療科未設定"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "ドクター未設定"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "該当患者番号なし"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療時間設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "ドクターが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "診療内容情報が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "中途終了データの登録ができません"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "病名の登録ができません"
                                               TO  WRK-ERRMSG
           WHEN    "22"
               MOVE    "登録対象のデータがありません"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "診療日を設定しました"
                                               TO  WRK-ERRMSG
           WHEN    "K2"
               MOVE    "保険組合せをゼロで登録しました"
                                               TO  WRK-ERRMSG
      *共通エラー
           WHEN    "98"
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
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
               MOVE    "処理区分未設定"        TO  WRK-ERRMSG
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
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
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
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
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
