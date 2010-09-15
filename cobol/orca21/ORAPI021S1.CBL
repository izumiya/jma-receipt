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
       01  LNK-TIMEOFDAY       PIC X(100).
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
           03  WRK-PERFORM-YMD         PIC X(08).
           03  WRK-PERFORM-YMD-G       PIC X(10).
           03  WRK-PERFORM-TIME        PIC X(06).
           03  WRK-PERFORM-TIME-G      PIC X(08).
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
           03  WRK-DAY1                PIC X(10).
           03  WRK-DAY2                PIC X(10).
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
      *    claim連携リターンエリア
       01  WRK-ERROR-AREA.
           03  WRK-KBN-CODE            PIC X(01).
           03  WRK-ERR-CODE            PIC 9(02).
           03  WRK-ERR-MSG             PIC X(100).
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
       01  LF04-REC.
           COPY    "CPRLF004.INC".
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
      **************************************************************************
       PROCEDURE           DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
       000-MAIN                SECTION.
           ACCEPT  WRK-SYSYMD    FROM DATE
           MOVE    20                  TO  WRK-SYSYY
      *
           DISPLAY " program   START"
                       " "  LNK-TIMEOFDAY(1:15)
           INITIALIZE        PTNUM-REC
                             IDX-AREA
                             WK-MSG
                             SPA-AREA
                             FLG-AREA
                             CNT-AREA
                             XML-MEDICALRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  MDCRES-RESKEY
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
           MOVE    SMCNDATE-YMD(1:4)   TO
                                       MDCRES-INFORMATION-DATE(1:4)
           MOVE    "-"                 TO
                                       MDCRES-INFORMATION-DATE(5:1)
           MOVE    SMCNDATE-YMD(5:2)   TO
                                       MDCRES-INFORMATION-DATE(6:2)
           MOVE    "-"                 TO
                                       MDCRES-INFORMATION-DATE(8:1)
           MOVE    SMCNDATE-YMD(7:2)   TO
                                       MDCRES-INFORMATION-DATE(9:2)
           MOVE    SMCNDATE-HMS(1:2)   TO  
                                       MDCRES-INFORMATION-TIME(1:2)
           MOVE    ":"                 TO  
                                       MDCRES-INFORMATION-TIME(3:1)
           MOVE    SMCNDATE-HMS(3:2)   TO  
                                       MDCRES-INFORMATION-TIME(4:2)
           MOVE    ":"                 TO  
                                       MDCRES-INFORMATION-TIME(6:1)
           MOVE    SMCNDATE-HMS(5:2)   TO  
                                       MDCRES-INFORMATION-TIME(7:2)
      *    医療機関識別番号セット 
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  MDCRES-API-RESULT
               MOVE   "ユーザID未登録" TO  MDCRES-API-RESULT-MSG
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
           MOVE    ZERO                TO  MDCRES-API-RESULT
           IF      APIREQ-CONTENT-TYPE =   "application/xml"
               PERFORM 1000-APTXMLREAD-SEC
           ELSE
               PERFORM 1000-APTCSVREAD-SEC
           END-IF
      *
           IF  MDCRES-API-RESULT       NOT = ZERO
               PERFORM 1000-APTXMLMAKE-SEC
               GO  TO                  000-MAIN-EXIT
           END-IF
      *
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    ZERO                TO  FLG-INPUT
           PERFORM 1001-PTINF-CHK-SEC
           MOVE    CNT-CHECK           TO  MDCRES-API-RESULT
           IF      CNT-CHECK           >   ZERO
               DISPLAY "必須項目チェックエラー " CNT-CHECK    
               DISPLAY "MDCRES-API-RESULT-MSG = "
                        MDCRES-API-RESULT-MSG
      *        GO  TO                  000-MAIN-EXIT
           ELSE
           EVALUATE    APIREQ-CLASS
             WHEN    "01"
      * 入力項目の内容チェックが必要
                 PERFORM 1001-WKSRY-ADD-SEC
             END-EVALUATE
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  MDCRES-API-RESULT
           END-IF
      *
      *
           IF      APIREQ-CONTENT-TYPE =   "application/xml"
                   PERFORM 1000-APTXMLMAKE-SEC
           ELSE
                   DISPLAY "type err"
           END-IF
      *
      *    TEST ST
      *    PERFORM 900-NOCOMMIT-SEC
      *    TEST ED
           DISPLAY " program   END  "
                       " "  LNK-TIMEOFDAY(1:15)
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
           IF      API-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      * XML情報取り出し
           MOVE   "XMLOPEN"            TO  MCP-FUNC
           MOVE    "xml_medicalreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    API-BODY            TO  MDCREQ-OBJECT
           MOVE    ZERO                TO  MDCREQ-MODE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALREQ
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "99"           TO  MDCRES-API-RESULT
               MOVE   "構文誤りです" TO  MDCRES-API-RESULT-MSG
               GO     TO   1000-APTXMLREAD-EXT
           END-IF
      *
           MOVE   "XMLREAD"            TO  MCP-FUNC
           MOVE    "xml_medicalreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE   ORCA-BLOB-OBJECT     TO  MDCREQ-OBJECT
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-MEDICALREQ
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               MOVE    MDCREQ-PATIENTID    TO  WRK-PTNUM
               MOVE    MDCREQ-DEP-CODE     TO  WRK-SRYKA
               MOVE    MDCREQ-DR-CODE      TO  WRK-DRCD
            IF MDCREQ-PERFORM-DATE(5:1)    =   "-"
               MOVE    MDCREQ-PERFORM-DATE TO  WRK-PERFORM-YMD-G
               MOVE    MDCREQ-PERFORM-DATE(1:4)
                                         TO  WRK-PERFORM-YMD(1:4)
               MOVE    MDCREQ-PERFORM-DATE(6:2)
                                         TO  WRK-PERFORM-YMD(5:2)
               MOVE    MDCREQ-PERFORM-DATE(9:2)
                                         TO  WRK-PERFORM-YMD(7:2)
            ELSE
               MOVE    MDCREQ-PERFORM-DATE TO  WRK-PERFORM-YMD
               MOVE    MDCREQ-PERFORM-DATE(1:4)
                                         TO  WRK-PERFORM-YMD-G(1:4)
               MOVE    "-"
                                         TO  WRK-PERFORM-YMD-G(5:1)
               MOVE    MDCREQ-PERFORM-DATE(5:2)
                                         TO  WRK-PERFORM-YMD-G(6:2)
               MOVE    "-"
                                         TO  WRK-PERFORM-YMD-G(8:1)
               MOVE    MDCREQ-PERFORM-DATE(7:2)
                                         TO  WRK-PERFORM-YMD-G(9:2)
            END-IF
            IF MDCREQ-PERFORM-TIME(3:1)    =   ":"
               MOVE    MDCREQ-PERFORM-TIME
                                           TO  WRK-PERFORM-TIME-G
               MOVE    MDCREQ-PERFORM-TIME(1:2)
                                         TO  WRK-PERFORM-TIME(1:2)
               MOVE    MDCREQ-PERFORM-TIME(4:2)
                                         TO  WRK-PERFORM-TIME(3:2)
               MOVE    MDCREQ-PERFORM-TIME(7:2)
                                         TO  WRK-PERFORM-TIME(5:2)
            ELSE
               MOVE    MDCREQ-PERFORM-TIME
                                           TO  WRK-PERFORM-TIME
               MOVE    MDCREQ-PERFORM-TIME(1:2)
                                         TO  WRK-PERFORM-TIME-G(1:2)
               MOVE    ":"
                                         TO  WRK-PERFORM-TIME-G(3:1)
               MOVE    MDCREQ-PERFORM-TIME(3:2)
                                         TO  WRK-PERFORM-TIME-G(4:2)
               MOVE    ":"
                                         TO  WRK-PERFORM-TIME-G(6:1)
               MOVE    MDCREQ-PERFORM-TIME(5:2)
                                         TO  WRK-PERFORM-TIME-G(7:2)
            END-IF
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           MOVE   'XMLCLOSE'           TO  MCP-FUNC
           MOVE    "xml_medicalreq"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE   ORCA-BLOB-OBJECT     TO  MDCREQ-OBJECT
           CALL   'ORCDBMAIN'          USING
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
      *    CSV情報から内容を取り出す
      *****************************************************************
       1000-APTCSVREAD-SEC   SECTION.
      *
           MOVE   "BLOBEXPORT"         TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    API-BODY            TO  ORCA-BLOB-OBJECT
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
               CONTINUE
           ELSE
               DISPLAY "BLOBEXPORT failure"
           END-IF
      *
           MOVE    ORCA-BLOB-FILE      TO  REQCSV
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
                                               WRK-PERFORM-YMD
                                               WRK-PERFORM-TIME
                                               WRK-APPOINT-ID
                                               WRK-SRYKA
                                               WRK-DRCD
                                               WRK-SRYNAIYO
                                               WRK-YYKNAIYO
                                               WRK-YYKMEMO
           END-UNSTRING
      *
           CLOSE             REQCSV-FILE
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
           MOVE    "xml_medicalres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE   1                    TO  MDCRES-MODE
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-MEDICALRES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE   "XMLWRITE"           TO  MCP-FUNC
           MOVE    "xml_medicalres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-MEDICALRES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE   "XMLCLOSE"           TO  MCP-FUNC
           MOVE    "xml_medicalres"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-MEDICALRES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               MOVE  MDCRES-OBJECT     TO  API-BODY 
               MOVE  "application/xml" TO  APIREQ-CONTENT-TYPE
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
      *    予約前チェックおよび予約登録
      *****************************************************************
       1001-WKSRY-ADD-SEC        SECTION.
      *
           MOVE  PTINF-NAME            TO  MDCRES-NAME
           MOVE  PTINF-KANANAME        TO  MDCRES-KANANAME
           MOVE  PTINF-BIRTHDAY(1:4)   TO  MDCRES-BIRTHDAY(1:4)
           MOVE  "-"                   TO  MDCRES-BIRTHDAY(5:1)
           MOVE  PTINF-BIRTHDAY(5:2)   TO  MDCRES-BIRTHDAY(6:2)
           MOVE  "-"                   TO  MDCRES-BIRTHDAY(8:1)
           MOVE  PTINF-BIRTHDAY(7:2)   TO  MDCRES-BIRTHDAY(9:2)
           MOVE  PTINF-SEX             TO  MDCRES-SEX
      * 
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF  SLOCK-RETURN    NOT =   ZERO
               MOVE   "90"           TO  MDCRES-API-RESULT
               MOVE   "他端末使用中" TO  MDCRES-API-RESULT-MSG
               GO     TO             1001-WKSRY-ADD-EXT
           END-IF
      *    CLAIM形式に変更するために医療機関コードを取得
           PERFORM 1001-IRYOUKIKAN-GET-SEC
      * 
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX           >   4)
      *    DISPLAY "------- 本当はいらない処理 ↓ --------------"
      *    PERFORM 1001-IRYOUKIKAN-GET-SEC
      *    DISPLAY "------- 本当はいらない処理 ↑ メモリ破壊------------"
           DISPLAY "------- claim call --------------"
               PERFORM 10011-CLAIM-CALL-SEC
           DISPLAY "------- claim call end ----------"
           END-PERFORM
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF  SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       1001-WKSRY-ADD-EXT.
           EXIT.
      *
      *
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       10011-CLAIM-CALL-SEC        SECTION.
      *
           IF      MDCREQ-DEP-CODE(IDX)    = SPACE
             AND   MDCREQ-DR-CODE(IDX)     = SPACE
               MOVE  10                TO  IDX
               GO    TO                10011-CLAIM-CALL-EXT
           END-IF
           INITIALIZE        LF04-REC
           IF      MDCREQ-OUTPATIENT-CLASS  =  "I"
               MOVE  "true"            TO  LF04-NYUGAIKBN
           END-IF
           MOVE    SYS-1001-HOSPID     TO  LF04-CRE001-FACILITYID
           IF      MDCREQ-PERFORM-DATE(5:1)  = "-"
               MOVE    MDCREQ-PERFORM-DATE
                                       TO  LF04-PERFORM-TIME
           ELSE
               MOVE    MDCREQ-PERFORM-DATE(1:4) 
                                       TO  LF04-PERFORM-TIME(1:4)
               MOVE    "-"                 TO  LF04-PERFORM-TIME(5:1)
               MOVE    MDCREQ-PERFORM-DATE(5:2) 
                                       TO  LF04-PERFORM-TIME(6:2)
               MOVE    "-"                 TO  LF04-PERFORM-TIME(8:1)
               MOVE    MDCREQ-PERFORM-DATE(7:2) 
                                       TO  LF04-PERFORM-TIME(9:2)
           END-IF
           MOVE    MDCREQ-PATIENTID    TO  LF04-PTID1
           MOVE    MDCREQ-DEP-CODE(IDX)    
                                       TO  LF04-CRE001-DPERTID
           MOVE    MDCREQ-DR-CODE(IDX)    
                                       TO  LF04-DOCCD
           MOVE  MDCREQ-MAIN-INSURANCE-CLASS(IDX)
                                       TO  LF04-HKN-HKNSBTCODE(01)
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   3)
               MOVE  MDCREQ-INSURANCE-NAME(IDX IDY)
                                       TO  LF04-HKNKOH-PROVIDER(1 IDY)
               MOVE  MDCREQ-PROVIDER-NUMBER(IDX IDY)
                                       TO  LF04-HKNKOH-FTNJANUM(1 IDY)
               MOVE  MDCREQ-RECIPIENT-NUMBER(IDX IDY)
                                       TO  LF04-HKNKOH-JKYSNUM(1 IDY)
           END-PERFORM
      *  診療情報設定
           PERFORM     VARYING   IDY   FROM    1   BY  1
      *                UNTIL    (IDY           >   40)
                       UNTIL    (IDY           >   20)
               MOVE  MDCREQ-MDC-CLASS(IDX IDY)
                                       TO  LF04-SRYCD(IDY)
               MOVE  MDCREQ-MDC-CLASS-NUMBER(IDX IDY)
                                       TO  LF04-KAISU(IDY)
             PERFORM     VARYING   IDZ   FROM    1   BY  1
      *                UNTIL    (IDZ           >   40)
                       UNTIL    (IDZ           >   20)
             IF MDCREQ-PRSCRPT-CODE(IDX IDY IDZ) NOT = SPACE
             DISPLAY "code = " MDCREQ-PRSCRPT-CODE(IDX IDY IDZ) 
             END-IF
               MOVE  MDCREQ-PRSCRPT-CODE(IDX IDY IDZ)
                                       TO  LF04-IJICD(IDY IDZ)
               MOVE  MDCREQ-PRSCRPT-NUMBER(IDX IDY IDZ)
                                       TO  LF04-SURYO(IDY IDZ)
             END-PERFORM
           END-PERFORM
      *  診断履歴情報設定
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   50)
               MOVE  MDCREQ-DIAGNOSIS-NUMBER(IDX IDY)
                                       TO  LF04-SND-SPICOD1(IDY)
               MOVE  MDCREQ-DIAGNOSIS-NAME(IDX IDY)
                                       TO  LF04-SND-SPINAME1(IDY)
               PERFORM     VARYING   IDZ   FROM    1   BY  1
                           UNTIL    (IDZ           >   6)
                 MOVE  MDCREQ-DIAGNOSIS-SINGLE-NUMBER(IDX IDY IDZ)
                                       TO  
                                       LF04-SNDSPITBL-SPICOD2(IDY IDZ)
                 MOVE  MDCREQ-DIAGNOSIS-SINGLE-NAME(IDX IDY IDZ)
                                       TO  
                                       LF04-SNDSPITBL-SPINAME2(IDY IDZ)
               END-PERFORM
               IF    MDCREQ-DIAGNOSIS-CATEGORY(IDX IDY)
                                       = "PD"
                 MOVE  "MML0012"
                                       TO  
                                       LF04-SNDBNRTBL-BNRTBL1(IDY 1)
                 MOVE  "mainDiagnosis"
                                       TO  
                                       LF04-SNDBNRTBL-BNRNAME1(IDY 1)
               END-IF
               IF    MDCREQ-DIAGNOSIS-SUSPECTEDFLAG(IDX IDY)
                                       = "S"
                 MOVE  "MML0015"
                                       TO  
                                       LF04-SNDBNRTBL-BNRTBL1(IDY 2)
                 MOVE  "suspectedDiagnosis"
                                       TO  
                                       LF04-SNDBNRTBL-BNRNAME1(IDY 2)
               END-IF
               MOVE  MDCREQ-DIAGNOSIS-START-DATE(IDX IDY)
                                       TO  WRK-DAY1
               PERFORM 100-DAYCHG-DEC
               MOVE  WRK-DAY2          TO  LF04-SND-SKNSTARTDATE(IDY)
               MOVE  MDCREQ-DIAGNOSIS-END-DATE(IDX IDY)
                                       TO  WRK-DAY1
               PERFORM 100-DAYCHG-DEC
               MOVE  WRK-DAY2          TO  LF04-SND-SKNENDDATE(IDY)
               EVALUATE    MDCREQ-DIAGNOSIS-OUTCOME(IDX IDY)
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
      *    IF      MDCREQ-MAIN-INSURANCE-CLASS(IDX)
      *                                =   "60"
      *        MOVE  "00"              TO  LF04-HKN-HKNSBTCODE(01)
      *    ELSE
      *        MOVE  MDCREQ-MAIN-INSURANCE-CLASS(IDX)
      *                                TO  LF04-HKN-HKNSBTCODE(01)
      *    END-IF
      *
           INITIALIZE                  WRK-ERROR-AREA
           MOVE    "1"                 TO  WRK-KBN-CODE
           CALL    "ORCL0031"          USING LF04-REC
                                             WRK-ERROR-AREA
           IF      WRK-ERR-CODE    NOT = ZERO
               MOVE   WRK-ERR-CODE   TO  MDCRES-API-RESULT
               MOVE   WRK-ERR-MSG    TO  MDCRES-API-RESULT-MSG
           END-IF
      *
           .
       10011-CLAIM-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集
      *****************************************************************
       100-DAYCHG-DEC     SECTION.
      *    
           DISPLAY "WRK-DAY1 = " WRK-DAY1
           MOVE    SPACE               TO  WRK-DAY2
           IF      WRK-DAY1            =   SPACE
               GO  TO       100-DAYCHG-EXT
           END-IF
           IF      WRK-DAY1(5:1)       =   "-"
               MOVE    WRK-DAY1        TO  WRK-DAY2
               GO  TO       100-DAYCHG-EXT
           END-IF
           MOVE    WRK-DAY1(1:4)       TO  WRK-DAY2(1:)
           MOVE    "-"                 TO  WRK-DAY2(5:1)
           MOVE    WRK-DAY1(5:2)       TO  WRK-DAY2(6:2)
           MOVE    "-"                 TO  WRK-DAY2(8:1)
           MOVE    WRK-DAY1(7:2)       TO  WRK-DAY2(9:2)
           DISPLAY "WRK-DAY2 = " WRK-DAY2
      *
           .
       100-DAYCHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関コードの取得
      *****************************************************************
       1001-IRYOUKIKAN-GET-SEC     SECTION.
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           END-IF
      * SYS-1001-HOSPID
           .
       1001-IRYOUKIKAN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       1001-PTINF-CHK-SEC      SECTION.
      *
      *    患者番号チェック
           PERFORM 10011-PTNUM-CHK-SEC
           PERFORM 10011-SRYYMD-CHK-SEC
      *    診療時間チェック
           PERFORM 10011-SRYTIME-CHK-SEC
      *    診療科コードチェック
           PERFORM 10011-SRYKA-CHK-SEC
      *    ドクターコードチェック
           PERFORM 10011-DRCD-CHK-SEC
      *
           .
       1001-PTINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日チェック
      *****************************************************************
       10011-SRYYMD-CHK-SEC    SECTION.
      *
           MOVE  WRK-PERFORM-YMD-G     TO  MDCRES-PERFORM-DATE
      *
           IF      WRK-PERFORM-YMD      =   SPACE
               ADD 2                   TO  CNT-CHECK
               MOVE  "予約日未設定"    TO  MDCRES-API-RESULT-MSG
               DISPLAY  "予約日未設定"   
               GO      TO      10011-SRYYMD-CHK-EXT
           END-IF
      *
      *    日付チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-PERFORM-YMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            NOT =   ZERO
               ADD 2                   TO  CNT-CHECK
               MOVE  "予約日設定誤り"  TO  MDCRES-API-RESULT-MSG
               DISPLAY  "予約日設定誤り"  
               GO      TO      10011-SRYYMD-CHK-EXT
           END-IF
      *
           .
       10011-SRYYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療時間チェック
      *****************************************************************
       10011-SRYTIME-CHK-SEC   SECTION.
      *
           MOVE  WRK-PERFORM-TIME-G    TO  MDCRES-PERFORM-TIME
      *
           IF      WRK-PERFORM-TIME     =   SPACE
               ADD 4                   TO  CNT-CHECK
               MOVE  "診療時間未設定"  TO  MDCRES-API-RESULT-MSG
               DISPLAY   "予約時間未設定"  
               GO      TO      10011-SRYTIME-CHK-EXT
           END-IF
      *
           IF      WRK-PERFORM-TIME    NOT NUMERIC
               ADD 4                   TO  CNT-CHECK
               MOVE  "診療時間不正"    TO  MDCRES-API-RESULT-MSG
               DISPLAY   "診療時間不正 " WRK-PERFORM-TIME
               GO      TO      10011-SRYTIME-CHK-EXT
           END-IF
      *
           IF      WRK-PERFORM-TIME(1:2)   >=  "24" 
               ADD 4                   TO  CNT-CHECK
               MOVE  "診療時間設定誤り"    TO  MDCRES-API-RESULT-MSG
               DISPLAY  "予約時間設定誤り"  
               GO      TO      10011-SRYTIME-CHK-EXT
           END-IF
      *
           IF      WRK-PERFORM-TIME(3:2)   >=  "60" 
               ADD 4                   TO  CNT-CHECK
               MOVE  "診療時間設定誤り"    TO  MDCRES-API-RESULT-MSG
               DISPLAY  "予約時間設定誤り"  
               GO      TO      10011-SRYTIME-CHK-EXT
           END-IF
      *
           IF      WRK-PERFORM-TIME(1:2)   >=  "21" 
               MOVE  "21"              TO  WRK-PERFORM-TIME(1:2)
           END-IF
      *
           IF      WRK-PERFORM-TIME(1:2)   <=  "08" 
               MOVE  "08"              TO  WRK-PERFORM-TIME(1:2)
           END-IF
      *
           MOVE    WRK-PERFORM-TIME    TO  WRK-KEYYYKTIME
           MOVE    ZERO                TO  WRK-KEYYYKTIME(3:2)
      *
           .
       10011-SRYTIME-CHK-EXT.
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
                   MOVE   "該当患者なし"   TO  MDCRES-API-RESULT-MSG
                   MOVE   WRK-PTNUM        TO  MDCRES-PATIENTID
                   ADD 1               TO  CNT-CHECK
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
                 MOVE   "該当患者なし" TO  MDCRES-API-RESULT-MSG
                 ADD 1                 TO  CNT-CHECK
                 GO  TO                10011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  MDCRES-PATIENTID
           MOVE    SPTID-PTID          TO  PTINF-PTID
      *
           PERFORM 1001-PTINF-GET-SEC
           IF      MCP-RC          NOT =   ZERO
                 ADD 1                 TO  CNT-CHECK
                 MOVE   "該当患者なし" TO  MDCRES-API-RESULT-MSG
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
           MOVE  WRK-SRYKA             TO  MDCRES-APPOINT-DEP-CODE
           IF    WRK-SRYKA             =   SPACE
                 MOVE   "診療科未設定" TO  MDCRES-API-RESULT-MSG
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
                                       TO  MDCRES-APPOINT-DEP-NAME
             ELSE
                 MOVE   "診療科誤り"   
                                       TO  MDCRES-API-RESULT-MSG
                 ADD 16                TO  CNT-CHECK
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
           MOVE  WRK-DRCD              TO  MDCRES-APPOINT-DR-CODE
           IF    WRK-DRCD              =   SPACE
                 MOVE   "ドクタ設定なし"   TO  MDCRES-API-RESULT-MSG
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
                 MOVE    SYS-1010-NAME TO  MDCRES-APPOINT-DR-NAME
             ELSE
                 MOVE   "ドクタが存在しません"   
                                       TO  MDCRES-API-RESULT-MSG
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
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
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
