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
       PROGRAM-ID.         ORAPI014S1.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール(サンプル)
      *  コンポーネント名  : 予約登録
      *  管理者            :
      *  作成日付    作業者        記述
      *  09/04/07    NACL−竹田　　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *    BLOB格納情報ファイル(CSV)
           SELECT  REQCSV-FILE     ASSIGN REQCSV    
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-REQCSV.
       DATA                DIVISION.
       FILE                    SECTION.
      *    BLOB格納情報ファイル(CSV)
       FD  REQCSV-FILE.
       01  REQCSV-RECORD.
           03  REQCSV-INFO             PIC X(5000).
      *
       WORKING-STORAGE             SECTION.
       01  REQCSV                  PIC X(512).
       01  STS-AREA.
           03  STS-REQCSV          PIC X(02).
      *    スパ領域
           COPY    "COMMON-SPA".
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDX1                PIC 9(04).
           03 TBL-MAX              PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PTINF           PIC 9(06).
           03  CNT-CHECK           PIC 9(02).
           03  CNT-UKEDELETE       PIC 9(06).
      *    公費並替え用ワーク
       01  WORK-KOHI-AREA.
           03  WRK-KOHI-AREA.
      *    並び替え前
               05  WRK-KOHI-TBL        OCCURS  4.
                   07  WRK-KOHID-O     PIC  9(10).
                   07  WRK-KOHHKNNUM-O PIC  X(03).
                   07  WRK-KOHMEI-O    PIC  X(50).
                   07  WRK-GAIJGNKBN-O PIC  X(01).
      *    並び替え後
           03  WRK-KOHI-AREA-X.
               05  WRK-KOHI-TBL-X      OCCURS  4.
                   07  WRK-KOHID       PIC  9(10).
                   07  WRK-KOHHKNNUM   PIC  X(03).
                   07  WRK-KOHMEI      PIC  X(50).
                   07  WRK-GAIJGNKBN   PIC  X(01).
      *    ＳＯＲＴ用ワーク
           03  WRK-KOHI.
               05  WRK-KOHID-W         PIC  9(10).
               05  WRK-KOHHKNNUM-W     PIC  X(03).
               05  WRK-KOHMEI-W        PIC  X(50).
               05  WRK-GAIJGNKBN-W     PIC  X(01).
      *
       01  WRK-CONST-AREA.
           03  WRK-CONST-TABLE.
               05  FILLER              PIC X(20)   VALUE   
                  "ああああああああああ".
               05  FILLER              PIC X(20)   VALUE   
                  "いいいいいいいいいい".
               05  FILLER              PIC X(20)   VALUE   
                  "うううううううううう".
               05  FILLER              PIC X(20)   VALUE   
                  "ええええええええええ".
               05  FILLER              PIC X(20)   VALUE   
                  "おおおおおおおおおお".
       01  WRK-SRT-AREA.
           03  WRK-SRT-TABLE.
               05  FILLER              PIC X(03)   VALUE   "027".
               05  FILLER              PIC X(03)   VALUE   "013".
               05  FILLER              PIC X(03)   VALUE   "014".
               05  FILLER              PIC X(03)   VALUE   "018".
               05  FILLER              PIC X(03)   VALUE   "029".
               05  FILLER              PIC X(03)   VALUE   "010".
               05  FILLER              PIC X(03)   VALUE   "011".
               05  FILLER              PIC X(03)   VALUE   "020".
               05  FILLER              PIC X(03)   VALUE   "021".
               05  FILLER              PIC X(03)   VALUE   "015".
               05  FILLER              PIC X(03)   VALUE   "016".
               05  FILLER              PIC X(03)   VALUE   "024".
               05  FILLER              PIC X(03)   VALUE   "022".
               05  FILLER              PIC X(03)   VALUE   "028".
               05  FILLER              PIC X(03)   VALUE   "017".
               05  FILLER              PIC X(03)   VALUE   "079".
               05  FILLER              PIC X(03)   VALUE   "019".
               05  FILLER              PIC X(03)   VALUE   "023".
               05  FILLER              PIC X(03)   VALUE   "051".
               05  FILLER              PIC X(03)   VALUE   "091".
               05  FILLER              PIC X(03)   VALUE   "052".
               05  FILLER              PIC X(03)   VALUE   "053".
               05  FILLER              PIC X(03)   VALUE   "066".
               05  FILLER              PIC X(03)   VALUE   "012".
      *********05  FILLER              PIC X(03)   VALUE   "972".
      *********05  FILLER              PIC X(03)   VALUE   "974".
           03  WRK-SRT-TABLE-R         REDEFINES   WRK-SRT-TABLE.
               05  WRK-SRT-KOHHKNNUM   PIC X(03)   OCCURS  24.
           03  WRK-SRT-KOHHKNNUM-MAX   PIC 9(02)   VALUE   24.
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-SYSYMD-8.
               05  WRK-SYSYY           PIC 9(02).
               05  WRK-SYSYMD          PIC 9(06).
           03  WRK-DATE                PIC 9(06).
      *
       01  WORK-AREA.
           03  WRK-YYKID               PIC 9(05).
           03  WK-MSG                  PIC X(40).
           03  WRK-YMD                 PIC X(10).
           03  WRK-TIME                PIC X(08).
           03  WRK-APPOINT-YMD         PIC X(08).
           03  WRK-APPOINT-TIME        PIC X(04).
           03  WRK-KEYYYKTIME          PIC X(04).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-NAMEKNJ             PIC X(100).
           03  WRK-NAMEKNA             PIC X(100).
           03  WRK-SEX                 PIC X(1).
           03  WRK-BIRTHDAY            PIC X(08).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-DRCD                PIC X(05).
           03  WRK-SRYNAIYO            PIC X(02).
           03  WRK-YYKNAIYO            PIC X(02).
           03  WRK-YYKMEMO             PIC X(100).
           03  WRK-APPOINT-ID          PIC X(05).
           03  WRK-USER-PG             PIC X(01).
           03  WRK-CLAIM               PIC X(01).
           03  WRK-MCP-WINDOW          PIC X(64).
           03  WRK-RENNUM              PIC 9(02).
           03  WRK-PTID                PIC X(10).
           03  WRK-GMN-SELNUM          PIC 9(02).
           03  WRK-HKNCOMBI            PIC 9(04).
           03  WRK-TIME                PIC X(06).
           03  WRK-CK-TIME1.
             05  WRK-CK-HH1            PIC 9(02).
             05  WRK-CK-MM1            PIC 9(02).
             05  WRK-CK-SS1            PIC 9(02).
           03  WRK-CK-TIME2.
             05  WRK-CK-HH2            PIC 9(02).
             05  WRK-CK-MM2            PIC 9(02).
             05  WRK-CK-SS2            PIC 9(02).
           03  WRK-CK-TIME3.
             05  WRK-CK-HH3            PIC 9(02).
             05  WRK-CK-MM3            PIC 9(02).
             05  WRK-CK-SS3            PIC 9(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-REQCSV              PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSUSER             PIC 9(01).
           03  FLG-YYK                 PIC 9(01).
           03  FLG-YYKHIT              PIC 9(01).
           03  FLG-INPUT               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-KANA                PIC 9(01).
           03  FLG-PTINF-CK            PIC 9(01).
           03  FLG-UKETUKE             PIC 9(01).
           03  FLG-USERPG              PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-CLAIM-MULTIHOST     PIC 9(01).
           03  FLG-CLAIM               PIC 9(01).
           03  FLG-SOUSIN              PIC 9(01).
           03  FLG-YYKTIME             PIC 9(01).
      *    ユーザプログラム起動
           03  FLG-GYOUMU-ARI          PIC 9(01).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL01.sh".
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
           COPY    "CPAPPTXMLREQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPAPPTXMLRES.INC".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
      *    予約内容
           COPY  "CPSK1028.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    予約メモマスター
       01  YYKCOM-REC.
           COPY    "CPYYKCOM.INC".
      *
      *    予約
       01  WRK-YYK-REC.
           COPY    "CPYYK.INC"       REPLACING
                                     //YYK-// BY //WRK-YYK-//.
      *
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "Y01.INC".
           COPY    "Y011.INC".
           COPY    "Y012.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "Y05.INC".
           COPY    "YERR.INC".
           COPY    "YID1.INC".
           COPY    "APPOINTMOD.INC".
      **************************************************************************
       PROCEDURE           DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
       000-MAIN                SECTION.
           ACCEPT  WRK-SYSYMD    FROM DATE
           MOVE    20                  TO  WRK-SYSYY
           DISPLAY   "****************"
           DISPLAY   "* appoint start*"
           DISPLAY   "****************"
      *    CALL  "cobsleep"   USING 30
      *
           INITIALIZE        PTNUM-REC
                             IDX-AREA
                             WK-MSG
                             SPA-AREA
                             FLG-AREA
                             CNT-AREA
                             XML-APPOINTRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    MOVE   "Patient Info"       TO  APNTRES-RESKEY
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
           MOVE    SMCNDATE-YMD(1:4)   TO  
                                       APNTRES-INFORMATION-DATE(1:4)
           MOVE    "-"                 TO  
                                       APNTRES-INFORMATION-DATE(5:1)
           MOVE    SMCNDATE-YMD(5:2)   TO  
                                       APNTRES-INFORMATION-DATE(6:2)
           MOVE    "-"                 TO  
                                       APNTRES-INFORMATION-DATE(8:1)
           MOVE    SMCNDATE-YMD(7:2)   TO  
                                       APNTRES-INFORMATION-DATE(9:2)
           MOVE    SMCNDATE-HMS(1:2)   TO  
                                       APNTRES-INFORMATION-TIME(1:2)
           MOVE    ":"                 TO  
                                       APNTRES-INFORMATION-TIME(3:1)
           MOVE    SMCNDATE-HMS(3:2)   TO  
                                       APNTRES-INFORMATION-TIME(4:2)
           MOVE    ":"                 TO  
                                       APNTRES-INFORMATION-TIME(6:1)
           MOVE    SMCNDATE-HMS(5:2)   TO  
                                       APNTRES-INFORMATION-TIME(7:2)
      *    医療機関識別番号セット 
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  APNTRES-API-RESULT
               MOVE   "ユーザID未登録" TO  APNTRES-API-RESULT-MSG
               GO  TO  000-MAIN-EXIT
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
      *
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M01"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
      * For Debug
      * ==================================================
      *    MOVE    "44444"             TO  WRK-PTNUM
      *    MOVE    "テスト　イッパン"  TO  WRK-NAMEKNJ
      *    MOVE    "テスト　イッパン"  TO  WRK-NAMEKNA
      *    MOVE    "1"                 TO  WRK-SEX
      *    MOVE    "19750101"          TO  WRK-BIRTHDAY
      *    MOVE    "01"                TO  WRK-SRYKA   
      *    MOVE    "10001"             TO  WRK-DRCD
      *    MOVE    "20090608"          TO  WRK-APPOINT-YMD
      *    MOVE    "1530"              TO  WRK-APPOINT-TIME
      *    MOVE    1                   TO  FLG-GYOUMU-ARI
      *    MOVE    "01"                TO  WRK-SRYNAIYO
      *    MOVE    "01"                TO  WRK-YYKNAIYO
      *    MOVE    ZERO                TO  WRK-HKNCOMBI
      * ==================================================
      * For Debug
           IF      APNTREQ-CONTENT-TYPE  =   "application/xml"
               PERFORM 1000-APTXMLREAD-SEC
           ELSE
               PERFORM 1000-APTCSVREAD-SEC
           END-IF
      *
           MOVE    ZERO                TO  APNTRES-API-RESULT
           MOVE    ZERO                TO  FLG-INPUT
           DISPLAY "CNT-CHECK  = " CNT-CHECK
           PERFORM 1001-PTINF-CHK-SEC
           MOVE    CNT-CHECK           TO  APNTRES-API-RESULT
           IF      CNT-CHECK           >   ZERO
               DISPLAY "必須項目チェックエラー " CNT-CHECK    
               DISPLAY "APNTRES-API-RESULT-MSG = "
                        APNTRES-API-RESULT-MSG
      *        GO  TO                  000-MAIN-EXIT
           ELSE
           EVALUATE    APNTREQ-CLASS
             WHEN    "01"
                 DISPLAY "予約登録"    
      * 入力項目の内容チェックが必要
                 PERFORM 1001-YYK-ADD-SEC
             WHEN    "02"
                 DISPLAY "予約取消"    
                 PERFORM 1001-YYK-DELETE-SEC
             END-EVALUATE
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  APNTRES-API-RESULT
           END-IF
      *
      *
      *
      *
      *
      *    INITIALIZE        XML-APPOINTRES
      *    MOVE "20090716"             TO  APNTRES-INFORMATION-DATE
      *    MOVE "01"                   TO
      *                            APNTRES-MAIN-INSURANCE-CLASS(01)
      *
      *
      *
           DISPLAY "APNTRES-MAIN-INSURANCE-CLASS " 
                    APNTRES-MAIN-INSURANCE-CLASS(1)
           DISPLAY "APNTRES-MAIN-INSURANCE-NUMBER " 
                    APNTRES-MAIN-INSURANCE-NUMBER(1)
           DISPLAY "" 
           DISPLAY "APNTRES-PATIENT-INFOMATION " 
                    APNTRES-PATIENT-INFOMATION(337:10)
           IF      APNTREQ-CONTENT-TYPE  =   "application/xml"
                   PERFORM 1000-APTXMLMAKE-SEC
           ELSE
                   PERFORM 1000-APTCSVMAKE-SEC
           END-IF
      *
           DISPLAY   "****************"
           DISPLAY   "* appoint end  *"
           DISPLAY   "****************"
      *    TEST ST
      *    PERFORM 900-NOCOMMIT-SEC
      *    TEST ED
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSUSER-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
               MOVE    ZERO            TO  FLG-SYSUSER
           ELSE
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           .
       900-SYSUSER-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APNTRES-OBJECT      NOT =   LOW-VALUE
               DISPLAY "APNTRES-OBJECT not low_value"
           END-IF
      * XML情報取り出し
           MOVE   "XMLOPEN"            TO  MCP-FUNC
           MOVE    "xml_appointreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APNT-BODY           TO  APNTREQ-OBJECT
           MOVE    ZERO                TO  APNTREQ-MODE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APPOINTREQ
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLOPEN OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
      *
           MOVE   "XMLREAD"            TO  MCP-FUNC
           MOVE    "xml_appointreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE   ORCA-BLOB-OBJECT     TO  APNTREQ-OBJECT
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-APPOINTREQ
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLREAD OK      = " MCP-RC
               DISPLAY "= " XML-APPOINTREQ
               MOVE    APNTREQ-PATIENTID   TO  WRK-PTNUM
               DISPLAY "req ptnum = " WRK-PTNUM
      *        MOVE    "テスト　イッパン"  TO  WRK-NAMEKNJ
      *        MOVE    "テスト　イッパン"  TO  WRK-NAMEKNA
      *        MOVE    "1"                 TO  WRK-SEX
      *        MOVE    "19750101"          TO  WRK-BIRTHDAY
               MOVE    APNTREQ-APPOINT-DEP-CODE
                                           TO  WRK-SRYKA   
               MOVE    APNTREQ-APPOINT-DR-CODE
                                           TO  WRK-DRCD
               IF      APNTREQ-APPOINT-DATE    =  SPACE
                 MOVE  SPACE               TO  WRK-YMD
                                               WRK-APPOINT-YMD
               ELSE
                IF      APNTREQ-APPOINT-DATE(5:1) =  "-"
                 MOVE   APNTREQ-APPOINT-DATE
                                           TO  WRK-YMD
                 MOVE    APNTREQ-APPOINT-DATE(1:4)
                                           TO  WRK-APPOINT-YMD(1:4)
                 MOVE    APNTREQ-APPOINT-DATE(6:2)
                                           TO  WRK-APPOINT-YMD(5:2)
                 MOVE    APNTREQ-APPOINT-DATE(9:2)
                                           TO  WRK-APPOINT-YMD(7:2)
                ELSE
                 MOVE    APNTREQ-APPOINT-DATE
                                           TO  WRK-APPOINT-YMD
                 MOVE   APNTREQ-APPOINT-DATE(1:4)
                                           TO  WRK-YMD(1:4)
                 MOVE   "-"                       
                                           TO  WRK-YMD(5:1)
                 MOVE   APNTREQ-APPOINT-DATE(5:2)
                                           TO  WRK-YMD(6:2)
                 MOVE   "-"                       
                                           TO  WRK-YMD(8:1)
                 MOVE   APNTREQ-APPOINT-DATE(7:2)
                                           TO  WRK-YMD(9:2)
                END-IF
               END-IF
               IF      APNTREQ-APPOINT-TIME    =  SPACE
                 MOVE  SPACE               TO  WRK-APPOINT-TIME
                                               WRK-TIME
               ELSE
                IF     APNTREQ-APPOINT-TIME(3:1)  =  ":"
                 MOVE  APNTREQ-APPOINT-TIME
                                           TO  WRK-TIME
                 MOVE  APNTREQ-APPOINT-TIME(1:2)
                                           TO  WRK-APPOINT-TIME(1:2)
                 MOVE  APNTREQ-APPOINT-TIME(4:2)
                                           TO  WRK-APPOINT-TIME(3:2)
      *          MOVE  APNTREQ-APPOINT-TIME(7:2)
      *                                    TO  WRK-APPOINT-TIME(5:2)
                ELSE
                 MOVE  APNTREQ-APPOINT-TIME
                                           TO  WRK-APPOINT-TIME
                 MOVE  APNTREQ-APPOINT-TIME(1:2)
                                           TO  WRK-TIME(1:2)
                 MOVE  ":"
                                           TO  WRK-TIME(3:1)
                 MOVE  APNTREQ-APPOINT-TIME(3:2)
                                           TO  WRK-TIME(4:2)
                 MOVE  ":"
                                           TO  WRK-TIME(6:1)
                 MOVE  "00"
                                           TO  WRK-TIME(7:2)
                END-IF 
               END-IF 
      *
               MOVE    APNTREQ-APPOINT-ID
                                           TO  WRK-APPOINT-ID
               DISPLAY "APNTREQ-APPOINT-TIME = " APNTREQ-APPOINT-TIME
               MOVE    APNTREQ-APPOINT-MEMO
                                           TO  WRK-YYKMEMO
               DISPLAY "APNTREQ-APPOINT-MEMO = " APNTREQ-APPOINT-MEMO
               MOVE    APNTREQ-APPOINT-DEP-INFO
                                           TO  WRK-SRYNAIYO
               MOVE    APNTREQ-APPOINT-INFO
                                           TO  WRK-YYKNAIYO
      *        MOVE    1                   TO  FLG-GYOUMU-ARI
      *        MOVE    ZERO                TO  WRK-HKNCOMBI
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE   'XMLCLOSE'           TO  MCP-FUNC
           MOVE    "xml_appointreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE   ORCA-BLOB-OBJECT     TO  APNTREQ-OBJECT
           CALL   'ORCDBMAIN'          USING
                                       MCPAREA
                                       XML-APPOINTREQ
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLCLOSE OK     = " MCP-RC
               CONTINUE
           END-IF
      *
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV情報から内容を取り出す
      *****************************************************************
       1000-APTCSVREAD-SEC   SECTION.
      *
           DISPLAY "1000-APTCSVREAD-SEC start "
      *  For Debug start
           MOVE   "BLOBEXPORT"         TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APNT-BODY           TO  ORCA-BLOB-OBJECT
      *
           STRING  "/tmp/"             DELIMITED  BY  SPACE
                   MCP-TERM            DELIMITED  BY  SPACE
                   ".csv"              DELIMITED  BY  SPACE
                                       INTO   ORCA-BLOB-FILE 
           END-STRING
      *
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "BLOBEXPORT OK"
               CONTINUE
           ELSE
               DISPLAY "BLOBEXPORT failure"
           END-IF
      *
           MOVE    ORCA-BLOB-FILE      TO  REQCSV
           DISPLAY "file name = " ORCA-BLOB-FILE
      *
           OPEN    INPUT     REQCSV-FILE
      *
           IF      STS-REQCSV          =   ZERO
               DISPLAY "FILE OPEN OK"
           ELSE
               DISPLAY "FILE OPEN NG " STS-REQCSV
           END-IF
      *
           READ    REQCSV-FILE
               AT  END
                   MOVE    1               TO  FLG-REQCSV
               NOT AT  END
                   MOVE    ZERO            TO  FLG-REQCSV
           END-READ
      *
           UNSTRING    REQCSV-RECORD   DELIMITED  BY  ","
                                       INTO    WRK-PTNUM
                                               WRK-APPOINT-YMD
                                               WRK-APPOINT-TIME
                                               WRK-APPOINT-ID
                                               WRK-SRYKA
                                               WRK-DRCD
                                               WRK-SRYNAIYO
                                               WRK-YYKNAIYO
                                               WRK-YYKMEMO
           END-UNSTRING
      *
           CLOSE             REQCSV-FILE
           DISPLAY "FILE PTNUM = " WRK-PTNUM
           DISPLAY "FILE DRCD  = " WRK-DRCD
      *
           .
       1000-APTCSVREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       1000-APTXMLMAKE-SEC   SECTION.
      *
           MOVE   "XMLOPEN"            TO  MCP-FUNC
           MOVE    "xml_appointres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE   1                    TO  APNTRES-MODE
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-APPOINTRES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLOPEN OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE   "XMLWRITE"           TO  MCP-FUNC
           MOVE    "xml_appointres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-APPOINTRES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLWRITE OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE   "XMLCLOSE"           TO  MCP-FUNC
           MOVE    "xml_appointres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-APPOINTRES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLCLOSE OK     = " MCP-RC
               MOVE  APNTRES-OBJECT    TO  APNT-BODY 
               MOVE  "application/xml" TO  APNTREQ-CONTENT-TYPE 
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
      * CALL    "coblog"    USING   WRK-ERRMSG
           END-IF
      *
          .
       1000-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV情報書き込み処理
      *****************************************************************
       1000-APTCSVMAKE-SEC   SECTION.
      *
           STRING  "/tmp/"             DELIMITED  BY  SPACE
                   MCP-TERM            DELIMITED  BY  SPACE
                   ".csv"              DELIMITED  BY  SPACE
                                       INTO   REQCSV
           END-STRING
      *
           MOVE    REQCSV              TO     ORCA-BLOB-FILE 
           OPEN    OUTPUT    REQCSV-FILE
           MOVE    SPACE               TO    REQCSV-RECORD
           STRING  APNTRES-INFORMATION-DATE  DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INFORMATION-TIME  DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-API-RESULT        DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-API-RESULT-MSG    DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-APPOINT-DATE      DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-APPOINT-TIME      DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-APPOINT-ID        DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-APPOINT-DEP-CODE  DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-APPOINT-DEP-NAME  DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-APPOINT-DR-CODE   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-APPOINT-DR-NAME   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-APPOINT-MEMO      DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PATIENTID         DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-NAME              DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-KANANAME          DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-BIRTHDAY          DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-SEX               DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-HOME-ZIP-CODE     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-HOME-ADDRESSES    DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-CLASS(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-NUMBER(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-NAME(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-MARK(01)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-NUMBER(01)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-CONTINUATION(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-ASSISTANCE(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-FAMILY(01)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-POLICYHOLDER(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-START-DATE(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-END-DATE(01) DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(01 03)
                                             DELIMITED BY SPACE
      *
      *
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-CLASS(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-NUMBER(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-NAME(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-MARK(02)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-NUMBER(02)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-CONTINUATION(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-ASSISTANCE(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-FAMILY(02)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-POLICYHOLDER(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-START-DATE(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-END-DATE(02) DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(02 03)
                                             DELIMITED BY SPACE
      *
      *
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-CLASS(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-NUMBER(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-NAME(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-MARK(03)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-NUMBER(03)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-CONTINUATION(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-ASSISTANCE(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-FAMILY(03)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-POLICYHOLDER(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-START-DATE(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-END-DATE(03) DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(03 03)
                                             DELIMITED BY SPACE
      *
      *
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-CLASS(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-NUMBER(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-INSURANCE-NAME(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-MARK(04)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-NUMBER(04)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-CONTINUATION(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-ASSISTANCE(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-FAMILY(04)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-POLICYHOLDER(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-START-DATE(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-MAIN-END-DATE(04) DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NUMBER(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-NAME(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-PROVIDER-NUMBER(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-RECIPIENT-NUMBER(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-START-DATE(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   APNTRES-INSURANCE-END-DATE(04 03)
                                             DELIMITED BY SPACE
      *
      *
                   INTO    REQCSV-RECORD
           END-STRING
      *
           DISPLAY "out = "   REQCSV-RECORD(1:100)
           WRITE   REQCSV-RECORD
           CLOSE   REQCSV-FILE
      *
      *
      *    INITIALIZE ORCA-BLOB
      *    MOVE    "./in.txt"            TO     REQCSV
      *    MOVE    REQCSV              TO     ORCA-BLOB-FILE 
      *    OPEN    INPUT     REQCSV-FILE
      *    READ    REQCSV-FILE
      *    DISPLAY "in file = " REQCSV(1:10)
      *    DISPLAY "in = " REQCSV-RECORD(1:50)
      *    CLOSE     REQCSV-FILE
      *
      *
      *
           DISPLAY "blob file = "     ORCA-BLOB-FILE 
           MOVE    "BLOBIMPORT"        TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
           DISPLAY "blob file out = "     ORCA-BLOB-FILE 
           IF  ORCA-BLOB-OBJECT = LOW-VALUE
               DISPLAY "object is low value"
           ELSE
               DISPLAY "object is not low value"
           END-IF
           DISPLAY "mcp-rc = " MCP-RC
           MOVE    ORCA-BLOB-OBJECT    TO  APNT-BODY
           MOVE    "data/csv"          TO  APNTREQ-CONTENT-TYPE
      *
          .
       1000-APTCSVMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約前チェックおよび予約登録
      *****************************************************************
       1001-YYK-ADD-SEC        SECTION.
      *
           MOVE  PTINF-NAME        TO  APNTRES-NAME
           MOVE  PTINF-KANANAME    TO  APNTRES-KANANAME
           MOVE  PTINF-BIRTHDAY(1:4)
                                   TO  APNTRES-BIRTHDAY(1:4)
           MOVE  "-"
                                   TO  APNTRES-BIRTHDAY(5:1)
           MOVE  PTINF-BIRTHDAY(5:2)
                                   TO  APNTRES-BIRTHDAY(6:2)
           MOVE  "-"
                                   TO  APNTRES-BIRTHDAY(8:1)
           MOVE  PTINF-BIRTHDAY(7:2)
                                   TO  APNTRES-BIRTHDAY(9:2)
           MOVE  PTINF-SEX         TO  APNTRES-SEX
           MOVE  PTINF-HOME-POST   TO  APNTRES-HOME-ZIP-CODE
           MOVE  PTINF-HOME-ADRS   TO  APNTRES-HOME-ADDRESSES
      * 
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF  SLOCK-RETURN    NOT =   ZERO
               MOVE   "90"           TO  APNTRES-API-RESULT
               MOVE   "他端末使用中" TO  APNTRES-API-RESULT-MSG
               GO     TO             1001-YYK-ADD-EXT
           END-IF
      * 
      *    二重登録チェック(何分にするか)
      *    バッチ処理、ユーザ起動処理、予約取り込み
           PERFORM 1001-PTHKN-GET-SEC
           PERFORM 4901-YYKADD-SEC
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF  SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       1001-YYK-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約削除
      *****************************************************************
       1001-YYK-DELETE-SEC     SECTION.
      *
           MOVE  PTINF-NAME        TO  APNTRES-NAME
           MOVE  PTINF-KANANAME    TO  APNTRES-KANANAME
           MOVE  PTINF-BIRTHDAY    TO  APNTRES-BIRTHDAY
           MOVE  PTINF-SEX         TO  APNTRES-SEX
           MOVE  PTINF-HOME-POST   TO  APNTRES-HOME-ZIP-CODE
           MOVE  PTINF-HOME-ADRS   TO  APNTRES-HOME-ADDRESSES
      * 
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF  SLOCK-RETURN    NOT =   ZERO
               MOVE   "90"           TO  APNTRES-API-RESULT
               MOVE   "他端末使用中" TO  APNTRES-API-RESULT-MSG
               GO     TO             1001-YYK-ADD-EXT
           END-IF
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF  SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *    二重登録チェック(何分にするか)
      *    バッチ処理、ユーザ起動処理
           PERFORM 1001-PTHKN-GET-SEC
           IF      WRK-APPOINT-ID       =   SPACE OR  ZERO
                   PERFORM 4900-SAKUJO-SEC
           ELSE
                   PERFORM 4901-SAKUJO-SEC
           END-IF
           .
       1001-YYK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       1001-PTINF-CHK-SEC      SECTION.
      *
      *    患者番号チェック
           PERFORM 10011-PTNUM-CHK-SEC
      *    予約日チェック
           PERFORM 10011-YYKYMD-CHK-SEC
      *    予約時間チェック
           PERFORM 10011-YYKTIME-CHK-SEC
      *    診療科コードチェック
           PERFORM 10011-SRYKA-CHK-SEC
      *    ドクターコードチェック
           PERFORM 10011-DRCD-CHK-SEC
      *    診療内容チェック
           PERFORM 10011-SRYNAIYO-CHK-SEC
      *    予約内容チェック
           PERFORM 10011-YYKNAIYO-CHK-SEC
      *    予約メモチェック
           PERFORM 10011-YYKMEMO-CHK-SEC
      *    患者氏名(漢字)チェック
           PERFORM 10011-PTNAMEKNJ-CHK-SEC
      *    患者氏名(カナ)チェック
           PERFORM 10011-PTNAMEKNA-CHK-SEC
      *    患者性別チェック
      *    PERFORM 10011-PTSEX-CHK-SEC
      *    患者生年月日チェック
      *    PERFORM 10011-PTBIRTHDAY-CHK-SEC
      *
           .
       1001-PTINF-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    予約日チェック
      *****************************************************************
       10011-YYKYMD-CHK-SEC    SECTION.
      *
           MOVE  WRK-YMD               TO  APNTRES-APPOINT-DATE
      *
           IF      WRK-APPOINT-YMD      =   SPACE
               ADD 2                   TO  CNT-CHECK
               MOVE  "予約日未設定"    TO  APNTRES-API-RESULT-MSG
               DISPLAY  "予約日未設定"   
               GO      TO      10011-YYKYMD-CHK-EXT
           END-IF
      *
      *    日付チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-APPOINT-YMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            NOT =   ZERO
               ADD 2                   TO  CNT-CHECK
               MOVE  "予約日設定誤り"  TO  APNTRES-API-RESULT-MSG
               DISPLAY  "予約日設定誤り"  
               GO      TO      10011-YYKYMD-CHK-EXT
           END-IF
      *
           .
       10011-YYKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約時間チェック
      *****************************************************************
       10011-YYKTIME-CHK-SEC   SECTION.
      *
           MOVE  WRK-TIME              TO  APNTRES-APPOINT-TIME
      *
           IF      WRK-APPOINT-TIME     =   SPACE
               ADD 4                   TO  CNT-CHECK
               MOVE  "予約時間未設定"  TO  APNTRES-API-RESULT-MSG
               DISPLAY   "予約時間未設定"  
               GO      TO      10011-YYKTIME-CHK-EXT
           END-IF
      *
           IF      WRK-APPOINT-TIME    NOT NUMERIC
               ADD 4                   TO  CNT-CHECK
               MOVE  "予約時間不正"    TO  APNTRES-API-RESULT-MSG
               DISPLAY   "予約時間不正"
               GO      TO      10011-YYKTIME-CHK-EXT
           END-IF
      *
           IF      WRK-APPOINT-TIME(1:2)   >=  "24" 
               ADD 4                   TO  CNT-CHECK
               MOVE  "予約時間設定誤り"    TO  APNTRES-API-RESULT-MSG
               DISPLAY  "予約時間設定誤り"  
               GO      TO      10011-YYKTIME-CHK-EXT
           END-IF
      *
           IF      WRK-APPOINT-TIME(3:2)   >=  "60" 
               ADD 4                   TO  CNT-CHECK
               MOVE  "予約時間設定誤り"    TO  APNTRES-API-RESULT-MSG
               DISPLAY  "予約時間設定誤り"  
               GO      TO      10011-YYKTIME-CHK-EXT
           END-IF
      *
           IF      WRK-APPOINT-TIME(1:2)   >=  "21" 
               MOVE  "21"              TO  WRK-APPOINT-TIME(1:2)
           END-IF
      *
           IF      WRK-APPOINT-TIME(1:2)   <=  "08" 
               MOVE  "08"              TO  WRK-APPOINT-TIME(1:2)
           END-IF
      *
           MOVE    WRK-APPOINT-TIME    TO  WRK-KEYYYKTIME
           MOVE    ZERO                TO  WRK-KEYYYKTIME(3:2)
      *
           .
       10011-YYKTIME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       10011-PTNUM-CHK-SEC     SECTION.
      *
      *    患者番号自由構成の場合のみ新患時にも必須
      *    SPA-1009-PTNUMKSIKBN    =   "1" (自由構成)
      *    SPA-1009-PTNUMKSIKBN    =   "2" (標準構成)
      *    SPA-1009-PTNUMKSIKBN    =   "3" (拡張構成)
      *
      *
           IF      WRK-PTNUM       =  SPACE
              DISPLAY "患者番号未設定"
                   MOVE   "該当患者なし"   TO  APNTRES-API-RESULT-MSG
                   MOVE   WRK-PTNUM        TO  APNTRES-PATIENTID
                   ADD 1               TO  CNT-CHECK
                   DISPLAY   "該当患者なし"
                   GO  TO              10011-PTNUM-CHK-EXT
           END-IF
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF    SPTID-RC          NOT =   ZERO
              DISPLAY "該当患者なし"
                 MOVE   "該当患者なし" TO  APNTRES-API-RESULT-MSG
                 ADD 1                 TO  CNT-CHECK
                 DISPLAY   "該当患者なし"
                 GO  TO                10011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  APNTRES-PATIENTID
           MOVE    SPTID-PTID          TO  PTINF-PTID
      *
           PERFORM 1001-PTINF-GET-SEC
           IF      MCP-RC          NOT =   ZERO
                 ADD 1                 TO  CNT-CHECK
                 MOVE   "該当患者なし" TO  APNTRES-API-RESULT-MSG
                 DISPLAY   "該当患者なし"
                 GO  TO                10011-PTNUM-CHK-EXT
           END-IF
           MOVE  PTINF-NAME            TO  WRK-NAMEKNJ 
      *
           .
       10011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       10011-SRYKA-CHK-SEC         SECTION.
      *
           MOVE  WRK-SRYKA             TO  APNTRES-APPOINT-DEP-CODE
           IF    WRK-SRYKA             =   SPACE
                 DISPLAY "診療科設定なし"
                 MOVE   "診療科未設定" TO  APNTRES-API-RESULT-MSG
                 ADD 16                TO  CNT-CHECK
                 GO  TO                10011-SRYKA-CHK-EXT
           END-IF
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           STRING    WRK-SRYKA         DELIMITED  BY  SPACE
                     "%"               DELIMITED  BY  SIZE
                                       INTO    SYS-1005-KBNCD
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
             IF  FLG-SYSKANRI          =   ZERO
                 MOVE    MCPDATA-REC   TO  SYS-1005-REC
                 MOVE    SYS-1005-SRYKANAME
                                       TO  APNTRES-APPOINT-DEP-NAME
             ELSE
                 MOVE   "診療科誤り"   
                                       TO  APNTRES-API-RESULT-MSG
                 ADD 16                TO  CNT-CHECK
                 DISPLAY   "診療科誤り"   
             END-IF
           ELSE
               INITIALIZE              SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           .
       10011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクタコードチェック
      *****************************************************************
       10011-DRCD-CHK-SEC          SECTION.
      *
           MOVE  WRK-DRCD              TO  APNTRES-APPOINT-DR-CODE
           IF    WRK-DRCD              =   SPACE
              DISPLAY "ドクタ設定なし"
                 MOVE   "ドクタ設定なし"   TO  APNTRES-API-RESULT-MSG
                 ADD 32                TO  CNT-CHECK
                 GO  TO                10011-DRCD-CHK-EXT
           END-IF
      *
      *    ドクタの存在チェック
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           STRING    WRK-DRCD          DELIMITED  BY  SPACE
                     "%"               DELIMITED  BY  SIZE
                                       INTO    SYS-1010-KBNCD
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
             IF  FLG-SYSKANRI          =   ZERO
                 MOVE    MCPDATA-REC   TO  SYS-1010-REC
                 MOVE    SYS-1010-NAME TO  APNTRES-APPOINT-DR-NAME
             ELSE
                 MOVE   "ドクタが存在しません"   
                                       TO  APNTRES-API-RESULT-MSG
                 ADD 32                TO  CNT-CHECK
             END-IF
           ELSE
               INITIALIZE              SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10011-DRCD-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療内容チェック
      *****************************************************************
       10011-SRYNAIYO-CHK-SEC      SECTION.
      *
           DISPLAY "WRK-SRYNAIYO = " WRK-SRYNAIYO
           IF      WRK-SRYNAIYO        =   SPACE
           DISPLAY "WRK-SRYNAIYO is space " 
               MOVE "01"               TO  WRK-SRYNAIYO
           END-IF
      *
      *    診療内容情報の存在チェック
           MOVE    SPACE               TO  SYS-1012-REC
           INITIALIZE                  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           STRING    WRK-SRYNAIYO      DELIMITED  BY  SPACE
                     "%"               DELIMITED  BY  SIZE
                                       INTO    SYS-1012-KBNCD
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1012-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1012-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
             IF  FLG-SYSKANRI          =   ZERO
                 MOVE    MCPDATA-REC   TO  SYS-1012-REC
             ELSE
                 MOVE   "診療内容情報が存在しません"   
                                       TO  APNTRES-API-RESULT-MSG
                 DISPLAY   "診療内容情報が存在しません"   
                 ADD 64                TO  CNT-CHECK
                 DISPLAY "run 2"
             END-IF
           ELSE
               INITIALIZE              SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10011-SRYNAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約内容チェック
      *****************************************************************
       10011-YYKNAIYO-CHK-SEC      SECTION.
      *
           IF      WRK-YYKNAIYO        =   SPACE
               MOVE "01"               TO  WRK-YYKNAIYO
           END-IF
      *
      *    診療内容情報の存在チェック
           MOVE    SPACE               TO  SYS-1012-REC
           INITIALIZE                  SYS-1012-REC
           MOVE    "1028"              TO  SYS-1028-KANRICD
           STRING    WRK-YYKNAIYO      DELIMITED  BY  SPACE
                     "%"               DELIMITED  BY  SIZE
                                       INTO    SYS-1028-KBNCD
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1028-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1028-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1028-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1028-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
             IF  FLG-SYSKANRI          =   ZERO
                 MOVE    MCPDATA-REC   TO  SYS-1028-REC
             ELSE
                 MOVE   "予約内容情報が存在しません"   
                                       TO  APNTRES-API-RESULT-MSG
                 DISPLAY   "予約内容情報が存在しません"   
                 ADD 64                TO  CNT-CHECK
                 DISPLAY "run 1"
             END-IF
           ELSE
               INITIALIZE              SYS-1028-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10011-YYKNAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約メモチェック
      *****************************************************************
       10011-YYKMEMO-CHK-SEC       SECTION.
      *
           MOVE    WRK-YYKMEMO         TO  APNTRES-APPOINT-MEMO
           IF      WRK-YYKMEMO     NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    WRK-YYKMEMO         TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  WRK-YYKMEMO
               IF      FLG-ERR             =   1
           DISPLAY "予約メモ不正 [" WRK-YYKMEMO "]"
                   MOVE   "予約メモ不正"   
                                       TO  APNTRES-API-RESULT-MSG
                   ADD 256                     TO  CNT-CHECK
                   DISPLAY "cnt = "  CNT-CHECK
                   GO      TO      10011-YYKMEMO-CHK-EXT
               END-IF
           END-IF
      *
           .
       10011-YYKMEMO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者氏名(漢字)チェック
      *****************************************************************
       10011-PTNAMEKNJ-CHK-SEC SECTION.
      *
           IF      WRK-NAMEKNJ     NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    WRK-NAMEKNJ         TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  WRK-NAMEKNJ
               IF      FLG-ERR             =   1
           DISPLAY "漢字名称不正 [" WRK-NAMEKNJ "]"
                   ADD 2                       TO  CNT-CHECK
                   DISPLAY "cnt = "  CNT-CHECK
                   GO      TO      10011-PTNAMEKNJ-CHK-EXT
               END-IF
           END-IF
      *
           .
       10011-PTNAMEKNJ-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者氏名(カナ)チェック
      *****************************************************************
       10011-PTNAMEKNA-CHK-SEC SECTION.
      *
      *
      *    半角空白を全角空白へ
           MOVE    WRK-NAMEKNA         TO  WRK-MAE-INPUT
           MOVE    50                  TO  WRK-LEN
           PERFORM 890-HALF-ZEN-SEC
           MOVE    WRK-ATO-INPUT       TO  WRK-NAMEKNA
           IF      FLG-ERR             =   1
               ADD 4                       TO  CNT-CHECK
               DISPLAY "kana err"
               GO      TO      10011-PTNAMEKNA-CHK-EXT
           END-IF
      *
      *    カタカナのみの判定
           MOVE    ZERO                TO  FLG-KANA
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL      (IDX     >    100 )  OR
                              (WRK-NAMEKNA(IDX:1)  =  SPACE)
               IF     (WRK-NAMEKNA (IDX:2)
                                       =   "　"
                                       OR  "ー"
                                       OR  "−"  )  OR
                      (WRK-NAMEKNA (IDX:2)
                                       >=  "ァ"  AND
                                       <=  "ヶ"   )  OR
                      (WRK-NAMEKNA (IDX:2)
                                       >=  "０"  AND
                                       <=  "ｚ"   )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-KANA
               END-IF
           END-PERFORM
           IF      FLG-KANA            =   1
               ADD 4                       TO  CNT-CHECK
               DISPLAY "kana err"
               GO      TO      10011-PTNAMEKNA-CHK-EXT
           END-IF
      *
           .
       10011-PTNAMEKNA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者性別チェック
      *****************************************************************
       10011-PTSEX-CHK-SEC         SECTION.
      *
           IF      WRK-SEX                 =   "1"  OR  "2"
               CONTINUE
           ELSE
               ADD 8                       TO  CNT-CHECK
               DISPLAY "sex err"
               GO      TO      10011-PTSEX-CHK-EXT
           END-IF
      *
           .
       10011-PTSEX-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者生年月日チェック
      *****************************************************************
       10011-PTBIRTHDAY-CHK-SEC    SECTION.
      *
           IF      WRK-BIRTHDAY            =   SPACE
               ADD 16                      TO  CNT-CHECK
               DISPLAY "birthday err"
               GO      TO      10011-PTBIRTHDAY-CHK-EXT
           END-IF
      *
      *    日付チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-BIRTHDAY        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            NOT =   ZERO
               ADD 16                  TO  CNT-CHECK
               DISPLAY "date err"
               GO      TO      10011-PTBIRTHDAY-CHK-EXT
           END-IF
      *
           .
       10011-PTBIRTHDAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約追加処理
      *****************************************************************
       4901-YYKADD-SEC              SECTION.
      *
           ACCEPT  WRK-TIME              FROM   TIME
      *        予約２重登録疑いチェック
           PERFORM 49011-YYKDUP-SEC
           IF      FLG-YYKTIME     =   1
                   GO  TO  4901-YYKADD-EXT
           END-IF
      *
      *    予約マスタの検索
      *    PERFORM 49011-YYKCHK-SEC
      *
      *    予約ＩＤ＝００検索(制御レコード)
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    ZERO                TO  YYK-YYKID
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           MOVE    ZERO                TO  YYK-PTID
           PERFORM 900-YYK-READ-SEC
           IF      FLG-YYK             =   ZERO
               ADD     1                   TO  YYK-YYKCNT
               IF      YYK-YYKCNT          >   999
                   MOVE    "1007"              TO  SPA-ERRCD
                   MOVE   "予約上限到達"   TO  APNTRES-API-RESULT-MSG
                   GO  TO  4901-YYKADD-EXT
               END-IF
           DISPLAY "予約 ヘッダ あり"
               MOVE    YYK-YYKCNT          TO  WRK-YYKID
      *
               MOVE    "api+"              TO  YYK-OPID
               MOVE    SPA-OPID            TO  YYK-OPID(5:)
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_yyk"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           ELSE
           DISPLAY "予約 ヘッダ なし"
               INITIALIZE                  YYK-REC
               MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
               MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
               MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
               MOVE    WRK-DRCD            TO  YYK-DRCD
               MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
               MOVE    ZERO                TO  YYK-YYKID
               MOVE    12                  TO  YYK-YYKMAXCNT
               MOVE    1                   TO  YYK-YYKCNT
               MOVE    1                   TO  WRK-YYKID
               MOVE    ZERO                TO  YYK-PTID
      *        作成年月日
               MOVE    SMCNDATE-YMD        TO  YYK-CREYMD
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    "api+"              TO  YYK-OPID
               MOVE    SPA-OPID            TO  YYK-OPID(5:)
      *
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_yyk"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "API YYK     KEY INS ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
               GO  TO  4901-YYKADD-EXT
           END-IF
      *
      *    明細レコード追加
      *
           INITIALIZE                       YYK-REC
                                            YYKCOM-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  YYKCOM-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    WRK-APPOINT-YMD     TO  YYKCOM-YYKYMD
           MOVE    ZERO                TO  YYK-YYKID
           MOVE    ZERO                TO  YYKCOM-YYKID
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-SRYNAIYO        TO  YYKCOM-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-DRCD            TO  YYKCOM-DRCD
           MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
           MOVE    WRK-KEYYYKTIME      TO  YYKCOM-KEYYYKTIME
           MOVE    WRK-APPOINT-TIME    TO  YYK-YYKTIME
           MOVE    WRK-YYKNAIYO        TO  YYK-YYKKBN
           MOVE    1                   TO  YYKCOM-RENNUM
           MOVE    WRK-YYKMEMO         TO  YYKCOM-COMMENT
      *
           MOVE    WRK-YYKID                TO  YYK-YYKID
                                                YYKCOM-YYKID
                                                APNTRES-APPOINT-ID
           DISPLAY "yyk hospnum = " YYK-HOSPNUM
           DISPLAY "yyk ymd     = "  YYK-YYKYMD
           DISPLAY "yyk id      = "  YYK-YYKID
      *    MOVE    YYK-YYKTIME              TO  APNTRES-APPOINT-TIME
           IF      SPTID-PTID               =   ZERO
               MOVE    ZERO                 TO  YYK-PTID
           ELSE
               MOVE    SPTID-PTID           TO  YYK-PTID
           END-IF
           MOVE    WRK-NAMEKNJ              TO  YYK-NAME
           MOVE    WRK-SRYKA                TO  YYK-SRYKA
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  YYK-CREYMD
           MOVE    SMCNDATE-YMD        TO  YYKCOM-CREYMD
           MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
           MOVE    SMCNDATE-YMD        TO  YYKCOM-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
           MOVE    SMCNDATE-HMS        TO  YYKCOM-UPHMS
      *
           MOVE    "api+"              TO  YYK-OPID
           MOVE    SPA-OPID            TO  YYK-OPID(5:)
           MOVE    "api+"              TO  YYKCOM-OPID
           MOVE    SPA-OPID            TO  YYKCOM-OPID(5:)
      *
      *
      *
           MOVE    YYK-REC              TO  WRK-YYK-REC
           PERFORM 900-YYK-READ-SEC
           IF      FLG-YYK             =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               DISPLAY "API YYK     INS ERR KEY:"  YYK-KEY
               GO      TO      4901-YYKADD-EXT
           END-IF
      *
      *    予約マスタが存在した場合
           IF      FLG-YYKHIT          =   1
                   MOVE    WRK-YYK-YYKTIME     TO  WRK-YYK-YYKTIME
                   MOVE    WRK-YYK-KEYYYKTIME  TO  WRK-YYK-KEYYYKTIME
                   MOVE    WRK-YYK-YYKID       TO  WRK-YYK-YYKID
                                                   APNTRES-APPOINT-ID
                  DISPLAY "yyk id hit  = "  YYK-YYKID
           END-IF
           MOVE    WRK-YYK-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "API YYK     INS ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
               GO      TO      4901-YYKADD-EXT
           END-IF
      *
           DISPLAY "memo = "     WRK-YYKMEMO  
           IF      WRK-YYKMEMO         =   SPACE
               GO      TO      4901-YYKADD-EXT
           END-IF
      *
           MOVE    YYKCOM-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "API YYKcom  INS ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
               GO      TO      4901-YYKADD-EXT
           END-IF
      *
           .
       4901-YYKADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約二重登録チェック処理
      *****************************************************************
       49011-YYKDUP-SEC            SECTION.
      * 
      *    予約マスタ読み込み (key4)
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    SPTID-PTID          TO  YYK-PTID
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              NOT =  ZERO
                   GO        TO        49011-YYKDUP-EXT
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-YYK
                                           FLG-YYKTIME
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-YYK-READ-SEC
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (FLG-YYK       =   1)
               IF      WRK-SRYKA        =   YYK-SRYKA
                 PERFORM 49012-YYK-CK-SEC
               END-IF
               IF      FLG-YYKTIME      =   1
                 MOVE  1               TO  FLG-YYK
               ELSE
                 MOVE    "key4"        TO  MCP-PATHNAME
                 PERFORM 910-YYK-READ-SEC
               END-IF
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           .
       49011-YYKDUP-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回予約からの経過時間チェック
      *****************************************************************
       49012-YYKTIME-CK-SEC        SECTION.
      *
           IF      WRK-APPOINT-TIME(1:4)   <   YYK-YYKTIME
               MOVE    YYK-YYKTIME     TO  WRK-CK-TIME1
               MOVE    ZERO            TO  WRK-CK-TIME1(5:2)
               MOVE    WRK-APPOINT-TIME
                                       TO  WRK-CK-TIME2
           ELSE
               MOVE    WRK-APPOINT-TIME
                                       TO  WRK-CK-TIME1
               MOVE    YYK-YYKTIME     TO  WRK-CK-TIME2
               MOVE    ZERO            TO  WRK-CK-TIME2(5:2)
           END-IF
      *
           IF      WRK-CK-TIME1(3:2)   <   "10"
             IF    WRK-CK-TIME1(1:2)   <   "01"
               COMPUTE WRK-CK-HH1      =   WRK-CK-HH1 + 24 - 1
               COMPUTE WRK-CK-MM1      =   WRK-CK-MM1 + 60 - 10
             ELSE
               COMPUTE WRK-CK-HH1      =   WRK-CK-HH1 - 1
               COMPUTE WRK-CK-MM1      =   WRK-CK-MM1 + 60 - 10
             END-IF
      *      ELSE
      *        COMPUTE WRK-CK-MM1      =   WRK-CK-MM1 - 10
           END-IF
           IF      WRK-CK-TIME1        <   WRK-CK-TIME2
               MOVE    1               TO  FLG-YYKTIME
               DISPLAY "yyk 1   time = " WRK-CK-TIME1
               DISPLAY "yyk 2   time = " WRK-CK-TIME2
               DISPLAY "10分 経過 していない"
               MOVE   "98"             TO  APNTRES-API-RESULT
               MOVE   "二重登録疑い"   TO  APNTRES-API-RESULT-MSG
           END-IF
               DISPLAY "yyk 1   time = " WRK-CK-TIME1
               DISPLAY "yyk 2   time = " WRK-CK-TIME2
      *
           .
       49012-YYKTIME-CK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約の時間チェック(1時間以内の予約とぶつける)
      *****************************************************************
       49012-YYK-CK-SEC            SECTION.
      *
      *    来院済みは対象としない
           IF      YYK-RAIINZUMI       =   "1"
             DISPLAY "YYK-RAIINZUMI"
               GO            TO        49012-YYK-CK-EXT
           END-IF
      *    診療科が異る場合も同様
           IF      YYK-SRYKA       NOT =   WRK-SRYKA
             DISPLAY "YYK-SRYKA"
               GO            TO        49012-YYK-CK-EXT
           END-IF
      *    深夜0時は、対象外(一時的)
           IF      YYK-YYKTIME(1:2)    <   "01"
             DISPLAY "時間外 = " YYK-YYKTIME
               GO            TO        49012-YYK-CK-EXT
           END-IF
      *
           MOVE    WRK-APPOINT-TIME    TO  WRK-CK-TIME1
                                           WRK-CK-TIME3
           MOVE    YYK-YYKTIME         TO  WRK-CK-TIME2
           MOVE    ZERO                TO  WRK-CK-TIME1(5:2)
                                           WRK-CK-TIME3(5:2)
      *
           COMPUTE WRK-CK-HH1      =   WRK-CK-HH1  - 1
           COMPUTE WRK-CK-HH3      =   WRK-CK-HH3  + 1
           IF      WRK-CK-TIME1        <   WRK-CK-TIME2
             AND   WRK-CK-TIME3        >   WRK-CK-TIME2
               MOVE    1               TO  FLG-YYKTIME
               DISPLAY "yyk 1   time = " WRK-CK-TIME1
               DISPLAY "yyk org time = " WRK-CK-TIME2
               DISPLAY "yyk 3   time = " WRK-CK-TIME3
               DISPLAY "前後1時間以内"
               MOVE   "98"             TO  APNTRES-API-RESULT
               MOVE   "二重登録疑い"   TO  APNTRES-API-RESULT-MSG
           END-IF
      *
           .
       49012-YYK-CK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスタ存在チェック処理
      *****************************************************************
       49011-YYKCHK-SEC            SECTION.
      * 
      *    予約マスタ読み込み (key5)
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-SYSYMD          TO  YYK-YYKYMD
           MOVE    SPTID-PTID          TO  YYK-PTID
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              NOT =  ZERO
                   GO        TO        49011-YYKCHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-YYK    
                                           FLG-YYKHIT 
           INITIALIZE                  WRK-YYK-REC
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-YYK-READ-SEC
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (FLG-YYK       =   1)
               PERFORM 49012-YYK-CK-SEC
               IF      FLG-YYKHIT      =   1
                 MOVE  YYK-YYKTIME     TO  APNTRES-APPOINT-TIME
                 MOVE  1               TO  FLG-YYK
                 MOVE  YYK-REC         TO  WRK-YYK-REC
                 MOVE  "1"             TO  WRK-YYK-RAIINZUMI
                 MOVE  WRK-YYK-REC     TO  MCPDATA-REC
                 MOVE  "DBUPDATE"      TO  MCP-FUNC
                 MOVE  "tbl_yyk"       TO  MCP-TABLE
                 MOVE  "key"           TO  MCP-PATHNAME
                 CALL  "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
                 IF    MCP-RC          NOT =   ZERO
                   DISPLAY "API YYK システムエラー UPD ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
                 END-IF
               ELSE
                 PERFORM 910-YYK-READ-SEC
               END-IF
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       49011-YYKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC                 SECTION.
      *
           EVALUATE    FLG-USERPG
               WHEN    1
      *        追加
                   MOVE    1               TO  SPA-SYORI-FLG
               WHEN    2
      *        更新
                   MOVE    2               TO  SPA-SYORI-FLG
               WHEN    3
      *        削除
                   MOVE    3               TO  SPA-SYORI-FLG
           END-EVALUATE
      *
           IF      FLG-GYOUMU-ARI          =   1
      *            起動するユーザプログラムある時、実行
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "Y01"               TO  ORCSUSERCHK-GMNPG
                   MOVE    "2"                 TO  ORCSUSERCHK-SYORI
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SRYYMD
                   MOVE    SPTID-PTID          TO  ORCSUSERCHK-PTID
                   MOVE    SPTID-PTID          TO  ORCSUSERCHK-PTNUM
                   MOVE    WRK-SRYKA           TO  ORCSUSERCHK-SRYKA
                   MOVE    WRK-HKNCOMBI        TO  ORCSUSERCHK-HKNCOMBI
                   IF      WRK-DRCD            =   SPACE
                       MOVE    SPACE               TO  ORCSUSERCHK-DRCD
                   ELSE
                       MOVE    WRK-DRCD            TO  ORCSUSERCHK-DRCD
                   END-IF
                   MOVE    SPA-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
                   CALL    "ORCSUSERCHK"       USING
                                               ORCSUSERCHKAREA
                                               SPA-AREA
           END-IF
      *
           .
       4907-USERPG-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約取消処理
      *****************************************************************
       4900-SAKUJO-SEC             SECTION.
      *
           DISPLAY "予約削除(ID なし )  " WRK-APPOINT-ID
      *    予約マスタ削除(key4 で対象を削除)
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
                                           APNTRES-APPOINT-DATE
           MOVE    SPTID-PTID          TO  YYK-PTID
           MOVE    WRK-SRYKA           TO  YYK-SRYKA
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-APPOINT-TIME    TO  YYK-KEYYYKTIME
                                           APNTRES-APPOINT-TIME
           MOVE    ZERO                TO  YYK-KEYYYKTIME(3:2)
           MOVE    YYK-REC         TO  MCPDATA-REC
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              NOT =  ZERO
                   GO        TO        4900-SAKUJO-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-YYKTIME
                                           FLG-YYK
                                           CNT-UKEDELETE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-YYK-READ-SEC
           PERFORM     UNTIL    (FLG-YYK       =   1)
           DISPLAY "                      = " WRK-APPOINT-TIME
               IF  WRK-SRYKA           =   YYK-SRYKA
               AND WRK-SRYNAIYO        =   YYK-SRYNAIYO
               AND WRK-SRYNAIYO        =   YYK-SRYNAIYO
               AND WRK-APPOINT-TIME    =   YYK-YYKTIME
               AND WRK-DRCD            =   YYK-DRCD
      *
                       MOVE    YYK-REC   TO  MCPDATA-REC
                       MOVE    "DBDELETE"    TO  MCP-FUNC
                       MOVE    "tbl_yyk" TO  MCP-TABLE
                       MOVE    "key"         TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"   USING   MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *
                     IF      MCP-RC          NOT =   ZERO
                       MOVE    "1005"        TO  SPA-ERRCD
                       DISPLAY "API YYK     DELETE ERR:" MCP-RC
                       GO        TO        4900-SAKUJO-EXT
                     END-IF
                     ADD     1               TO  CNT-UKEDELETE
      *    予約メモ削除
                     PERFORM 49031-YYKCOM-DEL-SEC
      *    予約ヘッダ更新処理
                     PERFORM 49001-YYKHEAD-MOD-SEC
               END-IF
      *
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      CNT-UKEDELETE       =  ZERO
               MOVE   "90"             TO  APNTRES-API-RESULT
               MOVE   "予約レコード無し"   TO  APNTRES-API-RESULT-MSG
               GO     TO               4900-SAKUJO-EXT
           END-IF
      *    ユーザ起動プログラム起動チェック
      *
           MOVE    3                   TO  FLG-USERPG
           IF      WRK-USER-PG         =   "1"
               PERFORM 4907-USERPG-SEC
           END-IF
      *
           .
       4900-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約メモ削除処理
      *****************************************************************
       49031-YYKCOM-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  YYKCOM-REC
           INITIALIZE                      YYKCOM-REC
           MOVE    YYK-HOSPNUM         TO  YYKCOM-HOSPNUM
           MOVE    YYK-SRYNAIYO        TO  YYKCOM-SRYNAIYO
           MOVE    YYK-DRCD            TO  YYKCOM-DRCD
           MOVE    YYK-YYKYMD          TO  YYKCOM-YYKYMD
           MOVE    YYK-KEYYYKTIME      TO  YYKCOM-KEYYYKTIME
           MOVE    YYK-YYKID           TO  YYKCOM-YYKID
      *
           DISPLAY "hosp = "    YYK-HOSPNUM 
           DISPLAY "srynaiyo = "  YYK-SRYNAIYO
           DISPLAY "drcd     = "  YYK-DRCD
           DISPLAY "ymd      = "  YYK-YYKYMD
           DISPLAY "time     = "  YYK-KEYYYKTIME
           DISPLAY "id       = "  YYK-YYKID
      *
           MOVE    YYKCOM-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       49031-YYKCOM-DEL-EXT.
           EXIT.
      *****************************************************************
      *    予約ヘッダ更新処理
      *****************************************************************
       49001-YYKHEAD-MOD-SEC       SECTION.
      *
      *    前予約ＩＤ＝００修正
           MOVE    ZERO                TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK          =   ZERO
               COMPUTE YYK-YYKCNT          =   YYK-YYKCNT  -   1
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1010"      TO  SPA-ERRCD
                   GO                  TO  49001-YYKHEAD-MOD-EXT
               END-IF
           END-IF
      *
           .
       49001-YYKHEAD-MOD-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約取消処理
      *****************************************************************
       4901-SAKUJO-SEC             SECTION.
      *
           DISPLAY "予約削除(ID あり )  " WRK-APPOINT-ID
           IF      WRK-APPOINT-ID   NOT NUMERIC 
               DISPLAY "予約ID 不正  = " WRK-APPOINT-ID
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE   "予約ID 不正"    TO  APNTRES-API-RESULT-MSG
               GO  TO  4901-SAKUJO-EXT
           END-IF
      *
      *    予約マスタ削除
      *
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM             TO  YYK-HOSPNUM
           IF      WRK-APPOINT-YMD          =   SPACE
               MOVE    SPA-SYSYMD          TO  YYK-YYKYMD
           ELSE
               MOVE    WRK-APPOINT-YMD      TO  YYK-YYKYMD
           END-IF
           MOVE    WRK-APPOINT-ID           TO  YYK-YYKID
           MOVE    WRK-SRYKA           TO  YYK-SRYKA
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-APPOINT-TIME    TO  YYK-KEYYYKTIME
                                           APNTRES-APPOINT-TIME
           MOVE    ZERO                TO  YYK-KEYYYKTIME(3:2)
      *
           PERFORM 900-YYK-READ-SEC
           IF      FLG-YYK             =   ZERO
             IF    YYK-PTID            =   PTINF-PTID
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_yyk"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1005"              TO  SPA-ERRCD
                   DISPLAY "API YYK     DELETE ERR:" MCP-RC
                   GO  TO  4901-SAKUJO-EXT
               END-IF
      *    予約レコードの更新
                   PERFORM 49031-YYKCOM-DEL-SEC
                   PERFORM 49001-YYKHEAD-MOD-SEC
             ELSE
               MOVE   "90"             TO  APNTRES-API-RESULT
               MOVE   "予約患者不一致" TO  APNTRES-API-RESULT-MSG
               GO      TO  4901-SAKUJO-EXT
             END-IF
      *
           ELSE
               MOVE   "90"             TO  APNTRES-API-RESULT
               MOVE   "予約レコード無し"   TO  APNTRES-API-RESULT-MSG
               GO      TO  4901-SAKUJO-EXT
           END-IF
      *
      *    ユーザ起動プログラム起動チェック
           MOVE    3                   TO  FLG-USERPG
           IF      WRK-USER-PG         =   "1"
               PERFORM 4907-USERPG-SEC
           END-IF
      *
           .
       4901-SAKUJO-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    予約マスタの更新(予約取消時)
      *****************************************************************
       49001-YYKUPD-SEC            SECTION.
      *
      *    予約情報がない場合は何もしない
           DISPLAY "YYK-YYKTIME = " YYK-YYKTIME
           IF      YYK-YYKTIME         =   ZERO
               GO            TO        49001-YYKUPD-EXT
           END-IF
      *    予約マスタ読み込み 
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    YYK-YYKYMD          TO  YYK-YYKYMD
           MOVE    YYK-PTID            TO  YYK-PTID
           MOVE    YYK-DRCD            TO  YYK-DRCD
           MOVE    YYK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    YYK-KEYYYKTIME      TO  YYK-KEYYYKTIME
           MOVE    YYK-YYKID           TO  YYK-YYKID
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              NOT =  ZERO
                   GO        TO        49001-YYKUPD-EXT
           END-IF
      *
           INITIALIZE                  YYK-REC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-YYK-READ-SEC
           DISPLAY "FLG-YYK = " FLG-YYK
           IF    FLG-YYK               =   ZERO
                 MOVE  1               TO  FLG-YYK
                 MOVE  SPACE           TO  YYK-RAIINZUMI
                 MOVE  YYK-REC         TO  MCPDATA-REC
                 MOVE  "DBUPDATE"      TO  MCP-FUNC
                 MOVE  "tbl_yyk"       TO  MCP-TABLE
                 MOVE  "key"           TO  MCP-PATHNAME
grpsys           CALL  "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
                 IF    MCP-RC          NOT =   ZERO
                   DISPLAY "API YYK システムエラー UPD ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
                 END-IF
               END-IF
      *
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           .
       49001-YYKUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角空白を全角空白へ変換処理
      *****************************************************************
       890-HALF-ZEN-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報チェック
      *****************************************************************
       10011-PTINF-CHK-SEC     SECTION.
      *
      *
           .
       10011-PTINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報変更
      *****************************************************************
       1001-PTINF-MOD-SEC      SECTION.
      *
           INITIALIZE        PTINF-REC
           .
       1001-PTINF-MOD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       1001-PTINF-GET-SEC      SECTION.
      *
           INITIALIZE        PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              NOT =  ZERO
                   GO        TO        1001-PTINF-GET-EXT
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              NOT =  ZERO
                   GO        TO        1001-PTINF-GET-EXT
           END-IF
           MOVE    MCPDATA-REC         TO  PTINF-REC
           .
       1001-PTINF-GET-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報取得
      *****************************************************************
       1001-PTHKN-GET-SEC      SECTION.
      *
      *    保険情報
           INITIALIZE                  HKNCOMBI-REC
           MOVE    PTINF-HOSPNUM       TO  COMB-HOSPNUM
           MOVE    PTINF-PTID          TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
             MOVE  ZERO      TO        FLG-HKNCOMBI
             MOVE    "tbl_hkncombi"    TO  MCP-TABLE
             MOVE    "key2"            TO  MCP-PATHNAME
             PERFORM 910-DBFETCH-SEC
             MOVE  MCPDATA-REC         TO  HKNCOMBI-REC
           ELSE
             MOVE  1         TO        FLG-HKNCOMBI
             INITIALIZE                HKNCOMBI-REC
           END-IF
      *
           DISPLAY "hkn ptid = "  COMB-PTID
           DISPLAY "hkn hkndate = "  COMB-TEKSTYMD
           DISPLAY "hkn date = "  WRK-SYSYMD-8
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (IDX           >    4    )            OR
                              (FLG-HKNCOMBI   =   1    )
               IF     (WRK-SYSYMD-8          >=  COMB-TEKSTYMD)  AND
                      (WRK-SYSYMD-8          <=  COMB-TEKEDYMD)  AND
                      (COMB-DELKBN           =   SPACE        )
                   ADD  1                    TO  IDX
                   DISPLAY "hkn name = "  COMB-SYU-TANSEIDONAME
                   MOVE COMB-SYU-TANSEIDONAME
                                             TO  
                                             APNTRES-MAIN-INSURANCE-NAME
                                             (IDX)
      *
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
                       MOVE    PTINF-PTID          TO  PTHKN-PTID
                       MOVE    COMB-HKNID          TO  PTHKN-HKNID
                       MOVE    WRK-SYSYMD-8        TO  PTHKN-TEKSTYMD
                       MOVE    WRK-SYSYMD-8        TO  PTHKN-TEKEDYMD
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       PERFORM 910-PTHKNINF-INV-SEC
                       MOVE    PTHKN-HKNNUM(2:2)
                                                   TO  
                                       APNTRES-MAIN-INSURANCE-CLASS(IDX)
                       MOVE    PTHKN-HKNJANUM      TO
                                       APNTRES-MAIN-INSURANCE-NUMBER
                                                   (IDX)
                       MOVE    PTHKN-KIGO          TO
                                       APNTRES-MAIN-MARK(IDX)
                       MOVE    PTHKN-NUM           TO
                                       APNTRES-MAIN-NUMBER(IDX)
                       MOVE    PTHKN-CONTKBN       TO
                                       APNTRES-MAIN-CONTINUATION(IDX)
                       MOVE    PTHKN-HOJOKBN       TO
                                       APNTRES-MAIN-ASSISTANCE(IDX)
                       MOVE    PTHKN-HONKZKKBN     TO
                                       APNTRES-MAIN-FAMILY(IDX)
                       MOVE    PTHKN-HIHKNJANAME   TO
                                       APNTRES-MAIN-POLICYHOLDER(IDX)
                       MOVE    PTHKN-TEKSTYMD(1:4) TO
                                   APNTRES-MAIN-START-DATE(IDX)(1:4)
                       MOVE    "-"                 TO
                                   APNTRES-MAIN-START-DATE(IDX)(5:1)
                       MOVE    PTHKN-TEKSTYMD(5:2) TO
                                   APNTRES-MAIN-START-DATE(IDX)(6:2)
                       MOVE    "-"                 TO
                                   APNTRES-MAIN-START-DATE(IDX)(8:1)
                       MOVE    PTHKN-TEKSTYMD(7:2) TO
                                   APNTRES-MAIN-START-DATE(IDX)(9:2)
                       IF      PTHKN-TEKEDYMD      = "99999999"
                         MOVE  "9999-12-31"        TO
                                   APNTRES-MAIN-END-DATE(IDX)
                       ELSE
                         MOVE  PTHKN-TEKEDYMD(1:4) TO
                                   APNTRES-MAIN-END-DATE(IDX)(1:4)
                         MOVE  "-"                 TO
                                   APNTRES-MAIN-END-DATE(IDX)(5:1)
                         MOVE  PTHKN-TEKEDYMD(5:2) TO
                                   APNTRES-MAIN-END-DATE(IDX)(6:2)
                         MOVE  "-"                 TO
                                   APNTRES-MAIN-END-DATE(IDX)(8:1)
                         MOVE  PTHKN-TEKEDYMD(7:2) TO
                                   APNTRES-MAIN-END-DATE(IDX)(9:2)
                       END-IF
                       IF      PTHKN-HKNNUM        =  SPACE
                         MOVE  "XX"               
                                                   TO  
                                       APNTRES-MAIN-INSURANCE-CLASS(IDX)
                         MOVE  SPACE               TO
                                   APNTRES-MAIN-START-DATE(IDX)
                                   APNTRES-MAIN-END-DATE(IDX)
                       END-IF
      *   公費読み出し
                   IF      COMB-KOHHKNNUM (1)      NOT =   SPACE
                       PERFORM 20012-KOHID-HENSYU-SEC
                       PERFORM VARYING IDY1 FROM    1   BY  1
                               UNTIL   IDY1 >       3
                               OR      WRK-KOHID(IDY1)    =   ZERO
                           INITIALIZE              PTKOHINF-REC
                           MOVE    PTINF-HOSPNUM   TO  PTKOH-HOSPNUM
                           MOVE    PTINF-PTID      TO  PTKOH-PTID
                           MOVE    WRK-KOHID(IDY1) TO  PTKOH-KOHID
                           MOVE    WRK-SYSYMD-8    TO  PTKOH-TEKSTYMD
                           MOVE    WRK-SYSYMD-8    TO  PTKOH-TEKEDYMD
                           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                           PERFORM 910-PTKOHINF-INV-SEC
                           MOVE    WRK-KOHMEI (IDY1)   TO
                                           APNTRES-INSURANCE-NAME
                                                          (IDX IDY1)
                           MOVE    PTKOH-FTNJANUM      TO
                                           APNTRES-PROVIDER-NUMBER
                                                          (IDX IDY1)
                           MOVE    PTKOH-JKYSNUM       TO
                                           APNTRES-RECIPIENT-NUMBER
                                                          (IDX IDY1)
                           MOVE    PTKOH-KOHNUM        TO
                                           APNTRES-INSURANCE-NUMBER
                                                          (IDX IDY1)
                           MOVE    PTKOH-TEKSTYMD(1:4) TO
                                           APNTRES-INSURANCE-START-DATE
                                                   (IDX IDY1)(1:4)
                           MOVE    "-"                 TO
                                           APNTRES-INSURANCE-START-DATE
                                                   (IDX IDY1)(5:1)
                           MOVE    PTKOH-TEKSTYMD(5:2) TO
                                           APNTRES-INSURANCE-START-DATE
                                                   (IDX IDY1)(6:2)
                           MOVE    "-"                 TO
                                           APNTRES-INSURANCE-START-DATE
                                                   (IDX IDY1)(8:1)
                           MOVE    PTKOH-TEKSTYMD(7:2) TO
                                           APNTRES-INSURANCE-START-DATE
                                                   (IDX IDY1)(9:2)
                           IF      PTKOH-TEKEDYMD  = "99999999"
                             MOVE  "9999-12-31"        TO
                                           APNTRES-INSURANCE-END-DATE
                                                   (IDX IDY1)
                           ELSE
                             MOVE  PTKOH-TEKEDYMD(1:4) TO
                                           APNTRES-INSURANCE-END-DATE
                                                   (IDX IDY1)(1:4)
                             MOVE  "-"                 TO
                                           APNTRES-INSURANCE-END-DATE
                                                   (IDX IDY1)(5:1)
                             MOVE  PTKOH-TEKEDYMD(5:2) TO
                                           APNTRES-INSURANCE-END-DATE
                                                   (IDX IDY1)(6:2)
                             MOVE  "-"                 TO
                                           APNTRES-INSURANCE-END-DATE
                                                   (IDX IDY1)(8:1)
                             MOVE  PTKOH-TEKEDYMD(7:2) TO
                                           APNTRES-INSURANCE-END-DATE
                                                   (IDX IDY1)(9:2)
                           END-IF
                       END-PERFORM
                   END-IF
      *
               END-IF
                   MOVE    "tbl_hkncombi"    TO  MCP-TABLE
                   MOVE    "key2"            TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              NOT =   ZERO
                   MOVE  1         TO        FLG-HKNCOMBI
               ELSE
                   MOVE  MCPDATA-REC         TO  HKNCOMBI-REC
               END-IF
           END-PERFORM
      *
           .
       1001-PTHKN-GET-EXT.
           EXIT.
      *****************************************************************
      *    公費個別処理
      *****************************************************************
       20012-KOHID-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      WRK-KOHI-AREA
                                           WRK-KOHI-AREA-X
           MOVE    ZERO                    TO  TBL-MAX
      *
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
               MOVE    COMB-KOHID    (IDZ) TO  WRK-KOHID-O     (IDZ)
               MOVE    COMB-KOHHKNNUM(IDZ) TO  WRK-KOHHKNNUM-O (IDZ)
      *        公費名称
               MOVE    COMB-KOH-TANSEIDONAME(IDZ)
                                           TO  WRK-KOHMEI-O    (IDZ)
      *        公費−外来−上限区分
               MOVE    COMB-KOH-GAIJGNKBN (IDZ)
                                           TO  WRK-GAIJGNKBN-O (IDZ)
           END-PERFORM
      *
      *    優先順位に並べ替え
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       WRK-SRT-KOHHKNNUM-MAX
                   OR      TBL-MAX     =   4
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       4
                   IF      WRK-SRT-KOHHKNNUM   (IDZ)
                                   =   WRK-KOHHKNNUM-O (IDY)
                       ADD     1               TO  TBL-MAX
                       MOVE    WRK-KOHI-TBL    (IDY)   TO
                                           WRK-KOHI-TBL-X (TBL-MAX)
                       INITIALIZE          WRK-KOHI-TBL   (IDY)
                       MOVE    4               TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    すべて並べ替えた時
           IF      TBL-MAX                 =   4
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    地方公費の並び替え
           PERFORM VARYING IDZ FROM    1    BY  1
                   UNTIL   IDZ >       3
               COMPUTE IDX1    =   IDZ     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       4
                   IF      WRK-KOHHKNNUM-O (IDZ) >
                                           WRK-KOHHKNNUM-O (IDY)
                   OR      WRK-KOHHKNNUM-O (IDZ) =   SPACE
                       MOVE    WRK-KOHI-TBL    (IDZ) TO
                                           WRK-KOHI
                       MOVE    WRK-KOHI-TBL    (IDY) TO
                                           WRK-KOHI-TBL(IDZ)
                       MOVE    WRK-KOHI              TO
                                           WRK-KOHI-TBL(IDY)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    地方公費の追加
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ         >   4
                   OR      TBL-MAX     =   4
               IF      WRK-KOHID-O (IDZ)   >   ZERO
                   ADD     1               TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDZ)
                           TO  WRK-KOHI-TBL-X (TBL-MAX)
               END-IF
           END-PERFORM
           .
       20012-KOHID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       900-YYK-READ-SEC         SECTION.
      *
           MOVE    YYK-REC         TO  MCPDATA-REC
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
      *    カーソルクロース
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-YYK-READ-EXT.
           EXIT.
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       910-YYK-READ-SEC            SECTION.
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           .
       910-YYK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       910-YOYAKU-INV-SEC         SECTION.
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-YOYAKU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           DISPLAY "mcp-window = "    MCP-WINDOW 
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "Y01"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
               DISPLAY "排他エラー = " PTINF-PTID
               DISPLAY "TERMID     = " SPA-TERMID
               DISPLAY "RC         = " SLOCK-RETURN
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
      *    MOVE    "Y01"               TO  SPA-MOTOPG
      *                                    MCP-WINDOW
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
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE          PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスタ読込
      *****************************************************************
       910-PTKOHINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTKOHINF-INV-EXT.
           EXIT.
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
      *    No Commit処理
      *****************************************************************
       900-NOCOMMIT-SEC            SECTION.
      *
      * For Debug
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      * For Debug
      *
           .
       900-NOCOMMIT-EXT.
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
