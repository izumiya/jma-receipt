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
       PROGRAM-ID.             ORCGZ91.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ出力設定画面（Ｚ９１）
      *  管理者            : 
      *  02/01/11    MCC-森脇　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00-00-01    森脇         02/04/02  出力形式追加
      *  01.00.02    MCC-藤原     02/04/24  印刷管理・印刷ＤＢ・
      *                                     ジョブ管理ＤＢの追加
      *  01.00.03    MCC-藤原     02/05/15  処理前にジョブ管理を削除する
      *  01.00.04    MCC-藤原     02/05/22  再印刷ボタン・情報削除ボタン追加
      *  01.00.05    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.01.01    NACL-森脇    02/12/06  コメント追加
      *  01.01.02    NACL-藤原    03/05/12  印刷開始・終了頁の編集の修正 
      *  01.01.03    NACL-太田    03/07/02  出力先プリンタサブ追加
      *                                     印刷する・しないのボタン追加
      *  01.01.04    NACL-太田    03/07/11  自費追加
      *  01.01.05    NACL-森脇    03/08/22  分類区分追加
      *  02.00.01    NACL-森脇    03/12/24  その他材料追加
      *  02.00.02    NACL-森脇    04/07/29  直前ジョブ管理チェック追加
      *  02.00.03    NACL-森脇    04/10/26  診療行為注射範囲修正
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
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //TENSUERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    SHELL用
           COPY    "CPSHELLTBL.INC".
           COPY    "ORCA-DBPATH".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *     
      *    画面用ＳＰＡ
       01  SPA-Z91.
           03  SPA-Z91-AREA.
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYCHK      PIC X.
                   07  SPA-GMN-SRY         PIC X     OCCURS  2.
                   07  SPA-GMN-IYKCHK-G.
      *----(01.01.01)--ADD-START----
                       09  SPA-GMN-IYKCHKALL         PIC X.
      *----(01.01.01)--ADD-END------
      *----(01.01.05)--UPD-START---
                       09  SPA-GMN-IYKCHK  PIC X     OCCURS  7.
      *----(01.01.05)--UPD-END-----
                   07  SPA-GMN-TOKCHK      PIC X.
      *----(02.00.01)--ADD-START---
                   07  SPA-GMN-HOKACHK     PIC X.
      *----(02.00.01)--ADD-END-----
                   07  SPA-GMN-JIHCHK      PIC X.
      *----(01.01.01)--ADD-START----
                   07  SPA-GMN-COMCHK-G.
                       09  SPA-GMN-COMCHKALL         PIC X.
                       09  SPA-GMN-COMCHK  PIC X     OCCURS  3.
      *----(01.01.01)--ADD-END------
                   07  SPA-GMN-MSTYMD      PIC X(9).
                   07  SPA-GMN-CYU         PIC X     OCCURS  2.
                   07  SPA-GMN-SYU         PIC X     OCCURS  3.
                   07  SPA-GMN-KEI         PIC X     OCCURS  2.
      *
                   07  SPA-Z91-SRYSTR.
                       09  SPA-Z91-SRYSTRL     PIC X(02).
                       09  SPA-Z91-F1L         PIC X(01).
                       09  SPA-Z91-SRYSTRMEIL  PIC X(13).
                   07  SPA-Z91-SRYEND.
                       09  SPA-Z91-SRYENDL     PIC X(02).
                       09  SPA-Z91-F2L         PIC X(01).
                       09  SPA-Z91-SRYENDMEIL  PIC X(13).
      *
               05  SPA-GMN-SRYSTR-AREA.
                   07  SPA-GMN-SRYSTR-LIST     OCCURS 13.
                       09  SPA-GMN-SRYSTRL     PIC X(02).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-SRYSTRMEIL  PIC X(14).
                   07  SPA-SRYSTR-MAX          PIC 9(04).
      *
               05  SPA-GMN-SRYEND-AREA.
                   07  SPA-GMN-SRYEND-LIST     OCCURS 13.
                       09  SPA-GMN-SRYENDL     PIC X(02).
                       09  SPA-GMN-F2L         PIC X(01).
                       09  SPA-GMN-SRYENDMEIL  PIC X(14).
                   07  SPA-SRYEND-MAX          PIC 9(04).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-MSTYMD          PIC 9(08).
      * 
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                PIC 9(01).
           03  FLG-TENSU              PIC 9(01).
           03  FLG-SRYCDCHG           PIC 9(01).
           03  FLG-SYSKANRI           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                    PIC 9(04).
           03  IDY                    PIC 9(04). 
      *      
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-PRTKANRI-RENNUM PIC 9(04).
           03  WRK-SHORIERR        PIC X(200).
           03  WRK-ZIDMSG          PIC X(70).
           03  WRK-PGID            PIC X(30).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
      *
           03  WRK-LNKYMD          PIC X(10).
      *  
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
      *
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *         
      *   印刷用呼出SHELL
           COPY    "CPHCMLSHELL.INC".
      *
      *
      *01  WRK-SHELL.
      *    03  FILLER             PIC  X(32) VALUE
      *             "/usr/local/orca/scripts/allways/".
      *    03  WRK-SHELL-NAME     PIC  X(30).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *              
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "102".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "TENSU".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "TENSU".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1031.INC".
      *
           COPY    "CPSK3003.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "Z01.INC".
           COPY    "Z02.INC".
           COPY    "Z03.INC".
           COPY    "Z04.INC".
           COPY    "Z05.INC".
           COPY    "Z06.INC".
           COPY    "Z07.INC".
           COPY    "Z90.INC".
           COPY    "Z91.INC".
           COPY    "Z97.INC".
           COPY    "Z98.INC".
           COPY    "Z99.INC".
           COPY    "ZERR.INC".
           COPY    "ZID1.INC".
           COPY    "ZID2.INC".
           COPY    "ZID3.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z91
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
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z91                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-ZIDCD2      NOT =   SPACE
                   PERFORM 530-ZIDSET2-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "Z91"               TO  MCP-WINDOW.
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF      WRK-MOTOPG      =   "XA01"  OR  "XB01"
                   MOVE    LNK-COMMON      TO  SPA-KYOTUKEY
                   MOVE    LNK-SCRSPA      TO  SPA-Z91
                   MOVE    SPA-WORK        TO  SPA-Z01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *
               MOVE    SPACE           TO  SPA-ZID1-FLG
               MOVE    SPACE           TO  SPA-ZIDCD      
                                           SPA-ZIDCD2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "ZID2"
                   PERFORM 340-ZID2-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "ZID3"
                   PERFORM 350-ZID3-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "Z97"
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
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
           INITIALIZE                  SPA-GMN-AREA
           INITIALIZE                  SPA-GMN-SRYSTR-AREA
           INITIALIZE                  SPA-GMN-SRYEND-AREA
      *
           MOVE  "F"     TO  SPA-GMN-SRYCHK  
      *
      *----(01.01.01)--ADD-START---
           MOVE  "F"     TO  SPA-GMN-IYKCHKALL
      *----(01.01.01)--ADD-END-----
           MOVE  "F"     TO  SPA-GMN-IYKCHK(1)
           MOVE  "F"     TO  SPA-GMN-IYKCHK(2)
           MOVE  "F"     TO  SPA-GMN-IYKCHK(3)
           MOVE  "F"     TO  SPA-GMN-IYKCHK(4)
           MOVE  "F"     TO  SPA-GMN-IYKCHK(5)
           MOVE  "F"     TO  SPA-GMN-IYKCHK(6)
      *----(01.01.05)--ADD-START---
           MOVE  "F"     TO  SPA-GMN-IYKCHK(7)
      *----(01.01.05)--ADD-END-----
      *
           MOVE  "F"     TO  SPA-GMN-TOKCHK
      *----(02.00.01)--ADD-START---
           MOVE  "F"     TO  SPA-GMN-HOKACHK
      *----(02.00.01)--ADD-END-----
           MOVE  "F"     TO  SPA-GMN-JIHCHK
      *
      *----(01.01.01)--ADD-START---
           MOVE  "F"     TO  SPA-GMN-COMCHKALL
           MOVE  "F"     TO  SPA-GMN-COMCHK(1)
           MOVE  "F"     TO  SPA-GMN-COMCHK(2)
           MOVE  "F"     TO  SPA-GMN-COMCHK(3)
      *----(01.01.01)--ADD-START---
      *
           MOVE  "T"     TO  SPA-GMN-SRY (1)
           MOVE  "F"     TO  SPA-GMN-SRY (2)
           MOVE  "T"     TO  SPA-GMN-CYU (1)
           MOVE  "F"     TO  SPA-GMN-CYU (2)
           MOVE  "T"     TO  SPA-GMN-SYU (1)
           MOVE  "F"     TO  SPA-GMN-SYU (2)
           MOVE  "F"     TO  SPA-GMN-SYU (3)
           MOVE  "T"     TO  SPA-GMN-KEI (1)
           MOVE  "F"     TO  SPA-GMN-KEI (2)
      *
           MOVE  "11"    TO  SPA-GMN-SRYSTRL(1)
           MOVE  "12"    TO  SPA-GMN-SRYSTRL(2)
           MOVE  "13"    TO  SPA-GMN-SRYSTRL(3)
           MOVE  "14"    TO  SPA-GMN-SRYSTRL(4)
           MOVE  "20"    TO  SPA-GMN-SRYSTRL(5)
           MOVE  "30"    TO  SPA-GMN-SRYSTRL(6)
           MOVE  "40"    TO  SPA-GMN-SRYSTRL(7)
           MOVE  "50"    TO  SPA-GMN-SRYSTRL(8)
           MOVE  "54"    TO  SPA-GMN-SRYSTRL(9)
           MOVE  "60"    TO  SPA-GMN-SRYSTRL(10)
           MOVE  "70"    TO  SPA-GMN-SRYSTRL(11)
           MOVE  "80"    TO  SPA-GMN-SRYSTRL(12)
      *----(01.01.01)--ADD-START---
           MOVE  "90"    TO  SPA-GMN-SRYSTRL(13)
      *----(01.01.01)--ADD-END-----
      *
           MOVE  "初診料"    TO  SPA-GMN-SRYSTRMEIL(1)
           MOVE  "再診料"    TO  SPA-GMN-SRYSTRMEIL(2)
           MOVE  "指導料"    TO  SPA-GMN-SRYSTRMEIL(3)
           MOVE  "在宅料"    TO  SPA-GMN-SRYSTRMEIL(4)
           MOVE  "投薬料"    TO  SPA-GMN-SRYSTRMEIL(5)
           MOVE  "注射料"    TO  SPA-GMN-SRYSTRMEIL(6)
           MOVE  "処置料"    TO  SPA-GMN-SRYSTRMEIL(7)
           MOVE  "手術料"    TO  SPA-GMN-SRYSTRMEIL(8)
           MOVE  "麻酔料"    TO  SPA-GMN-SRYSTRMEIL(9)
           MOVE  "検査料"    TO  SPA-GMN-SRYSTRMEIL(10)
           MOVE  "画像診断料"    TO  SPA-GMN-SRYSTRMEIL(11)
           MOVE  "その他料"  TO  SPA-GMN-SRYSTRMEIL(12)
      *----(01.01.01)--ADD-START---
           MOVE  "入院料"    TO  SPA-GMN-SRYSTRMEIL(13)
      *----(01.01.01)--ADD-END-----
      *
           MOVE  13      TO  SPA-SRYSTR-MAX
      *
           MOVE  SPA-GMN-SRYSTR-AREA  TO  SPA-GMN-SRYEND-AREA
      *
      *    マスタ日付セット
           MOVE  SPA-SYSYMD           TO  SPA-NAI-MSTYMD
           MOVE  SPA-SYSYMDWH         TO  SPA-GMN-MSTYMD 
      *
           MOVE   1      TO  SPA-GMN-CUR 
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           IF      SPA-ZID1-FLG    =   "OK"
               EVALUATE    SPA-ZIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1001"
                   WHEN    "1002"    
      *----(02.00.02)--UPD-START---
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD       =   SPACE
                           PERFORM 490-PRINT-SEC
                       END-IF
      *----(02.00.02)--UPD-END-----
               END-EVALUATE        
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD      
           .
       330-ZID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ２）ＯＫ処理
      *****************************************************************
       340-ZID2-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  SPA-ZIDCD2
           .
       340-ZID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＺＩＤ３）ＯＫ処理
      *****************************************************************
       350-ZID3-SET-SEC            SECTION.
      *
           IF      SPA-ZID1-FLG    =   "OK"    OR  "NO"
      *----(02.00.02)--UPD-START---
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 490-PRINT-SEC
               END-IF
      *----(02.00.02)--UPD-END-----
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
           .
       350-ZID3-SET-EXT.
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
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 310-SPASET-SEC         
      *     再印刷
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-REPRINT-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *        印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-PRINT-MAE-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
      *        診療行為チェック
               WHEN    "CLICKED"       ALSO    "SRYCHK"
                   EVALUATE    SPA-GMN-SRYCHK
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-SRYCHK
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-SRYCHK
                   END-EVALUATE     
                   MOVE  1              TO  SPA-GMN-CUR     
      *        医薬品チェック
      *----(01.01.01)--ADD-START---
               WHEN    "CLICKED"       ALSO    "IYKCHKALL"
      *----(01.01.05)--UPD-START---
                   EVALUATE    SPA-GMN-IYKCHKALL
                       WHEN    "T"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   7
                               MOVE    "F"  TO  SPA-GMN-IYKCHK(IDX)
                           END-PERFORM
                       WHEN    "F"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   7
                               MOVE    "T"  TO  SPA-GMN-IYKCHK(IDX)
                           END-PERFORM
                   END-EVALUATE
      *----(01.01.05)--UPD-END-----
                   MOVE  6              TO  SPA-GMN-CUR
      *----(01.01.01)--ADD-END-----
      *----(01.01.01)--UPD-START---
               WHEN    "CLICKED"       ALSO    "IYKCHK1"
                   EVALUATE    SPA-GMN-IYKCHK(1)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-IYKCHK(1)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-IYKCHK(1)
                   END-EVALUATE  
                   MOVE  7              TO  SPA-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK2"
                   EVALUATE    SPA-GMN-IYKCHK(2)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-IYKCHK(2)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-IYKCHK(2)
                   END-EVALUATE 
                   MOVE  8             TO  SPA-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK3"
                   EVALUATE    SPA-GMN-IYKCHK(3)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-IYKCHK(3)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-IYKCHK(3)
                   END-EVALUATE
                   MOVE  9              TO  SPA-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK4"
                   EVALUATE    SPA-GMN-IYKCHK(4)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-IYKCHK(4)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-IYKCHK(4)
                   END-EVALUATE
                   MOVE  10             TO  SPA-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK5"
                   EVALUATE    SPA-GMN-IYKCHK(5)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-IYKCHK(5)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-IYKCHK(5)
                   END-EVALUATE
                   MOVE  11             TO  SPA-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK6"
                   EVALUATE    SPA-GMN-IYKCHK(6)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-IYKCHK(6)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-IYKCHK(6)
                   END-EVALUATE
                   MOVE  12             TO  SPA-GMN-CUR
      *----(02.00.01)--UPD-START---
      *----(01.01.05)--ADD-START---
               WHEN    "CLICKED"       ALSO    "IYKCHK7"
                   EVALUATE    SPA-GMN-IYKCHK(7)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-IYKCHK(7)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-IYKCHK(7)
                   END-EVALUATE
                   MOVE  13             TO  SPA-GMN-CUR
      *----(01.01.05)--ADD-END-----
      *        特定器材チェック
               WHEN    "CLICKED"       ALSO    "TOKCHK" 
                   EVALUATE    SPA-GMN-TOKCHK
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-TOKCHK
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-TOKCHK
                   END-EVALUATE                
                   MOVE  14             TO  SPA-GMN-CUR
      *----(02.00.01)--ADD-START---
      *        その他材料チェック
               WHEN    "CLICKED"       ALSO    "HOKACHK" 
                   MOVE  Z91-HOKACHK     TO  SPA-GMN-HOKACHK
                   MOVE  15             TO  SPA-GMN-CUR
      *----(02.00.01)--ADD-END-----
      *        自費チェック
               WHEN    "CLICKED"       ALSO    "JIHCHK" 
                   MOVE  Z91-JIHCHK     TO  SPA-GMN-JIHCHK
                   MOVE  16             TO  SPA-GMN-CUR
      *----(01.01.01)--UPD-END-----
      *----(01.01.01)--ADD-START---
      *        コメントチェック
               WHEN    "CLICKED"       ALSO    "COMCHKALL"
                   EVALUATE    SPA-GMN-COMCHKALL
                       WHEN    "T"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   3
                               MOVE    "F"  TO  SPA-GMN-COMCHK(IDX)
                           END-PERFORM
                       WHEN    "F"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   3
                               MOVE    "T"  TO  SPA-GMN-COMCHK(IDX)
                           END-PERFORM
                   END-EVALUATE
                   MOVE  17             TO  SPA-GMN-CUR
               WHEN    "CLICKED"       ALSO    "COMCHK1"
                   EVALUATE    SPA-GMN-COMCHK(1)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-COMCHK(1)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-COMCHK(1)
                   END-EVALUATE  
                   MOVE  18             TO  SPA-GMN-CUR
               WHEN    "CLICKED"       ALSO    "COMCHK2"
                   EVALUATE    SPA-GMN-COMCHK(2)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-COMCHK(2)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-COMCHK(2)
                   END-EVALUATE 
                   MOVE  19             TO  SPA-GMN-CUR
               WHEN    "CLICKED"       ALSO    "COMCHK3"
                   EVALUATE    SPA-GMN-COMCHK(3)
                       WHEN    "T"
                           MOVE    "F"  TO  SPA-GMN-COMCHK(3)
                       WHEN    "F"
                           MOVE    "T"  TO  SPA-GMN-COMCHK(3)
                   END-EVALUATE
                   MOVE  20             TO  SPA-GMN-CUR
      *----(02.00.01)--UPD-END-----
      *----(01.01.01)--ADD-END-----
               WHEN    "CLICKED"       ALSO    "SRY1" 
                   MOVE  Z91-SRY(1)           TO  SPA-GMN-SRY(1)
               WHEN    "CLICKED"       ALSO    "SRY2" 
                   MOVE  Z91-SRY(2)           TO  SPA-GMN-SRY(2) 
                   IF  Z91-SRY(2)      =  "F"
                       MOVE  2                TO  SPA-GMN-CUR
                   ELSE
                       MOVE  4                TO  SPA-GMN-CUR
                   END-IF
               WHEN    "CLICKED"       ALSO    "KEI1" 
                   MOVE  Z91-KEI(1)           TO  SPA-GMN-KEI(1) 
               WHEN    "CLICKED"       ALSO    "KEI2" 
                   MOVE  Z91-KEI(2)           TO  SPA-GMN-KEI(2) 
               WHEN    "CLICKED"       ALSO    "CYU1" 
                   MOVE  Z91-CYU(1)           TO  SPA-GMN-CYU(1)
               WHEN    "CLICKED"       ALSO    "CYU2" 
                   MOVE  Z91-CYU(2)           TO  SPA-GMN-CYU(2) 
               WHEN    "CLICKED"       ALSO    "SYU1" 
                   MOVE  Z91-SYU(1)           TO  SPA-GMN-SYU(1)
               WHEN    "CLICKED"       ALSO    "SYU2" 
                   MOVE  Z91-SYU(2)           TO  SPA-GMN-SYU(2) 
               WHEN    "CLICKED"       ALSO    "SYU3" 
                   MOVE  Z91-SYU(3)           TO  SPA-GMN-SYU(3)
           END-EVALUATE
      *
      *----(01.01.01)--ADD-START---
           IF          SPA-GMN-IYKCHK(1)   =   "F"
               AND     SPA-GMN-IYKCHK(2)   =   "F"
               AND     SPA-GMN-IYKCHK(3)   =   "F"
               AND     SPA-GMN-IYKCHK(4)   =   "F"
               AND     SPA-GMN-IYKCHK(5)   =   "F"
               AND     SPA-GMN-IYKCHK(6)   =   "F"
      *----(01.01.05)--ADD-START---
               AND     SPA-GMN-IYKCHK(7)   =   "F"
      *----(01.01.05)--ADD-END-----
               MOVE    "F"  TO  SPA-GMN-IYKCHKALL
           ELSE
               MOVE    "T"  TO  SPA-GMN-IYKCHKALL
           END-IF
      *
           IF          SPA-GMN-COMCHK(1)   =   "F"
               AND     SPA-GMN-COMCHK(2)   =   "F"
               AND     SPA-GMN-COMCHK(3)   =   "F"
               MOVE    "F"  TO  SPA-GMN-COMCHKALL
           ELSE
               MOVE    "T"  TO  SPA-GMN-COMCHKALL
           END-IF
      *----(01.01.01)--ADD-END-----
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT        ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"       ALSO    "MSTYMD" 
                   PERFORM  400-MSTYMD-CHK-SEC
               WHEN    "ACTIVATE"       ALSO    "SRYSTR"
                   PERFORM  400-SRYSTR-SEC
               WHEN    "ACTIVATE"       ALSO    "SRYEND"
                   PERFORM  400-SRYEND-SEC
           END-EVALUATE     
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD 
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
      *********        AND     JOB-TERMID      =   SPA-TERMID
                           MOVE    "6666"          TO  SPA-ZIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      * 
           MOVE    "Z01"               TO  SPA-SAKIPG
           MOVE    "Z91"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *  　マスタ年月日チェック
      *****************************************************************
       400-MSTYMD-CHK-SEC                     SECTION.
      *
           MOVE  Z91-MSTYMD             TO  SPA-GMN-MSTYMD
      *
           MOVE    ZERO                 TO  SPA-NAI-MSTYMD
      *          
           IF      SPA-GMN-MSTYMD        =  SPACE
               MOVE    SPA-SYSYMDWH         TO  SPA-GMN-MSTYMD
           END-IF     
      *
           MOVE  SPA-GMN-MSTYMD         TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-LNKYMD          TO  SPA-GMN-MSTYMD
               MOVE    WRK-SYMD            TO  SPA-NAI-MSTYMD
           END-IF 
           
      *----(02.00.01)--UPD-START---
           MOVE  15                        TO  SPA-GMN-CUR
      *----(02.00.01)--UPD-END---
           .
       400-MSTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分開始
      *****************************************************************
       400-SRYSTR-SEC               SECTION.
      *
      *    診療区分変化なし
           IF  (Z91-SRYSTR  NOT  =  SPACE) AND
               (Z91-SRYSTR       =  SPA-Z91-SRYSTR)
      *
               IF  SPA-SRYSTR-MAX  >  ZERO
                   MOVE  1  TO  SPA-GMN-CUR
               END-IF
               GO  TO  400-SRYSTR-EXT
           END-IF
      *
           MOVE  Z91-SRYSTR      TO  SPA-Z91-SRYSTR
      *----(01.01.01)--ADD-START---
           MOVE  Z91-SRYSTR      TO  SPA-Z91-SRYEND
      *----(01.01.01)--ADD-END-----
      *
           MOVE  5               TO  SPA-GMN-CUR   
           .
       400-SRYSTR-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分終了
      *****************************************************************
       400-SRYEND-SEC               SECTION.
      *
      *    診療区分変化なし
           IF  (Z91-SRYEND  NOT  =  SPACE) AND
               (Z91-SRYEND       =  SPA-Z91-SRYEND)
      *
               IF  SPA-SRYEND-MAX  >  ZERO
                   MOVE  1  TO  SPA-GMN-CUR
               END-IF
               GO  TO  400-SRYEND-EXT
           END-IF
      *
           MOVE  Z91-SRYEND      TO  SPA-Z91-SRYEND 
      *
           MOVE  6               TO  SPA-GMN-CUR   
           .
       400-SRYEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Z91             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "Z91"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Z91             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "Z91"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-REPRINT-EXT.
           EXIT.
      *
      *
      *----(02.00.02)--ADD-START---
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-SHORIERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE      
                                                   INTO  WRK-SHORIERR
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
         .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *----(02.00.02)--ADD-END-----
      *****************************************************************
      *    印刷前確認処理
      *****************************************************************
       490-PRINT-MAE-SEC                   SECTION.
      *
      *----(01.01.01)--UPD-START---
           IF            (SPA-GMN-IYKCHKALL  =   "F")
                  AND    (SPA-GMN-SRYCHK     =   "F")
                  AND    (SPA-GMN-TOKCHK     =   "F")
                  AND    (SPA-GMN-JIHCHK     =   "F")
                  AND    (SPA-GMN-COMCHKALL  =   "F")
      *----(02.00.01)--ADD-START---
                  AND    (SPA-GMN-HOKACHK    =   "F")
      *----(02.00.01)--ADD-END-----
                  MOVE    "0004"    TO  SPA-ERRCD
           END-IF
      *----(01.01.01)--UPD-END-----
      *
           IF      SPA-ERRCD          =   SPACE
               IF     SPA-NAI-MSTYMD  =   ZERO
                      MOVE    "0003"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *----(02.00.02)--UPD-START---
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD       NOT  =   SPACE
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *----(02.00.02)--UPD-END-----
      *     
           IF     SPA-ERRCD           =   SPACE     
               IF      SPA-GMN-KEI(1)        =   "T"
                   MOVE    "1001"              TO  SPA-ZIDCD
               ELSE   
                   MOVE    "1002"              TO  SPA-ZIDCD
               END-IF
           END-IF
           .
       490-PRINT-MAE-EXT.
           EXIT.               
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       490-PRINT-SEC                       SECTION.
      * 
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD 
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE                                 
                   MOVE    "1005"              TO  SPA-ERRCD
                   GO  TO  490-PRINT-EXT
               END-IF
           END-IF                 
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  490-PRINT-EXT
           END-IF             
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF 
      *     
      *    出力先プリンタ名割り当て情報編集
      *    MOVE    SPACE               TO  SYS-1031-REC
      *    INITIALIZE                  SYS-1031-REC
      *    MOVE    "1031"              TO  SYS-1031-KANRICD
      *    MOVE    "*"                 TO  SYS-1031-KBNCD
      *    MOVE    SYS-1031-REC        TO  MCPDATA-REC
      *    PERFORM 900-SYSKANRI-INV-SEC
      *    IF      FLG-SYSKANRI        =   ZERO
      *        MOVE    MCPDATA-REC         TO  SYS-1031-REC
      *    ELSE    
      *        INITIALIZE                  SYS-1031-REC
      *    END-IF
      *
           INITIALIZE                  SHELL-AREA
      *
      *    診療行為チェックセット
           IF      SPA-GMN-SRYCHK      =   "T"
               MOVE    1                   TO  SHELL-SRYCHK
      *        診療区分セット
               IF      SPA-GMN-SRY(2)  =   "T"
                   MOVE  2                 TO  SHELL-SRYKBN
                   IF    SPA-Z91-SRYSTRL   <=  SPA-Z91-SRYENDL
                       MOVE  SPA-Z91-SRYSTRL   TO  SHELL-SRYSTR
                       MOVE  SPA-Z91-SRYENDL   TO  SHELL-SRYEND
                   ELSE
                       MOVE  SPA-Z91-SRYENDL   TO  SHELL-SRYSTR
                       MOVE  SPA-Z91-SRYSTRL   TO  SHELL-SRYEND
                   END-IF
      *----(02.00.03)--UPD-START---
                   EVALUATE    SHELL-SRYEND
                     WHEN      "20"
                       MOVE    "29"            TO  SHELL-SRYEND
                     WHEN      "30"
                       MOVE    "34"            TO  SHELL-SRYEND
                     WHEN      "90"
                       MOVE    "97"            TO  SHELL-SRYEND
                   END-EVALUATE
               ELSE
                   MOVE  1                 TO  SHELL-SRYKBN
                   MOVE  "11"              TO  SHELL-SRYSTR
                   MOVE  "97"              TO  SHELL-SRYEND
               END-IF
      *----(02.00.03)--UPD-END-----
           END-IF
      *     
      *    医薬品チェックセット
      *----(01.01.05)--UPD-START---
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   7
      *----(01.01.05)--UPD-END-----
               IF      SPA-GMN-IYKCHK(IDX)     =  "T"
                   EVALUATE    IDX
                       WHEN    1
                           MOVE    1           TO  SHELL-IYKCHK (IDX)
                       WHEN    2
                           MOVE    6           TO  SHELL-IYKCHK (IDX)
                       WHEN    3
                           MOVE    4           TO  SHELL-IYKCHK (IDX)
                       WHEN    4
                           MOVE    8           TO  SHELL-IYKCHK (IDX)
                       WHEN    5    
                           MOVE    9           TO  SHELL-IYKCHK (IDX)
                       WHEN    6
                           MOVE    3           TO  SHELL-IYKCHK (IDX)
      *----(01.01.05)--ADD-START---
                       WHEN    7
                           MOVE    1           TO  SHELL-IYKCHK (IDX)
      *----(01.01.05)--ADD-END-----
                   END-EVALUATE        
               END-IF
           END-PERFORM
      *
      *    特定器材チェックセット
           IF      SPA-GMN-TOKCHK      =   "T"
               MOVE  1                     TO  SHELL-TOKCHK
           ELSE
               MOVE  ZERO                  TO  SHELL-TOKCHK 
           END-IF
      *
      *----(02.00.01)--ADD-START---
      *    その他チェックセット
           IF      SPA-GMN-HOKACHK     =   "T"
               MOVE  1                     TO  SHELL-HOKACHK
           ELSE
               MOVE  ZERO                  TO  SHELL-HOKACHK
           END-IF
      *----(02.00.01)--ADD-END-----
      *
      *    自費チェックセット
           IF      SPA-GMN-JIHCHK      =   "T"
               MOVE  1                     TO  SHELL-JIHCHK
           ELSE
               MOVE  ZERO                  TO  SHELL-JIHCHK 
           END-IF
      *
      *----(01.01.01)--ADD-START---
      *    コメントチェックセット
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   3
               IF      SPA-GMN-COMCHK(IDX)     =  "T"
                   MOVE    1           TO  SHELL-COMCHK (IDX) 
               ELSE
                   MOVE    ZERO        TO  SHELL-COMCHK (IDX)
               END-IF
           END-PERFORM     
      *----(01.01.01)--ADD-END-----
      *
      *    抽出区分セット
           IF      SPA-GMN-CYU(1)      =   "T"
               MOVE  1                     TO  SHELL-CYUKBN
           ELSE
               MOVE  2                     TO  SHELL-CYUKBN
           END-IF
      *
      *    出力区分セット
           EVALUATE    TRUE
               WHEN    SPA-GMN-SYU (1) =   "T"
                   MOVE    1               TO  SHELL-SYUKBN
               WHEN    SPA-GMN-SYU (2) =   "T"
                   MOVE    2               TO  SHELL-SYUKBN
               WHEN    SPA-GMN-SYU (3) =   "T"
                   MOVE    3               TO  SHELL-SYUKBN
           END-EVALUATE
      *
      *    マスタ日付チェック
           MOVE  SPA-NAI-MSTYMD        TO  SHELL-MSTYMD
      *
      *    印刷処理
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
           MOVE    WRK-SYSYMD          TO
                                       WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                       WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
           MOVE    "TENSUER"           TO  TENSUERR-FILE-ID
           MOVE    SPA-TERMID          TO  TENSUERR-TERMID
      *   
           IF      SPA-GMN-KEI(1)        =   "T"
               IF      SHELL-SRYCHK          =   1
                       MOVE    "ORHCML03"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
      *----(01.01.05)--UPD-START---
               IF      SHELL-IYKCHK-G      =   ALL "0"
                   CONTINUE
               ELSE         
                   IF      SHELL-IYKCHK(7)   >   ZERO
                       MOVE    "ORHCML12"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
                   IF     (SHELL-IYKCHK(1)   >   ZERO)
                       OR (SHELL-IYKCHK(2)   >   ZERO)
                       OR (SHELL-IYKCHK(3)   >   ZERO)
                       OR (SHELL-IYKCHK(4)   >   ZERO)
                       OR (SHELL-IYKCHK(5)   >   ZERO)
                       OR (SHELL-IYKCHK(6)   >   ZERO)
                       MOVE    "ORHCML01"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
               END-IF
      *----(01.01.05)--UPD-END-----
      *
      *----(02.00.01)--UPD-START---
               IF      SHELL-TOKCHK        =   1
                   MOVE    1                 TO  SHELL-KBN
                   MOVE    "ORHCML02"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-HOKACHK       =   1
                   MOVE    2                 TO  SHELL-KBN
                   MOVE    "ORHCML02"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *----(02.00.01)--UPD-END-----
      *
               IF      SHELL-JIHCHK        =   1    
                   MOVE    "ORHCML10"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
      *----(01.01.01)--ADD-START---
               IF      SHELL-COMCHK-G        =   ALL "0"
                   CONTINUE
               ELSE
                   MOVE    "ORHCML04"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *----(01.01.01)--ADD-END-----
           ELSE
               IF      SHELL-SRYCHK          =   1
                   MOVE    "ORHCML08"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-IYKCHK-G      =   ALL "0"
                   CONTINUE
               ELSE         
                   IF      SHELL-IYKCHK(7)   >   ZERO
                       MOVE    "ORHCML12"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
                   IF     (SHELL-IYKCHK(1)   >   ZERO)
                       OR (SHELL-IYKCHK(2)   >   ZERO)
                       OR (SHELL-IYKCHK(3)   >   ZERO)
                       OR (SHELL-IYKCHK(4)   >   ZERO)
                       OR (SHELL-IYKCHK(5)   >   ZERO)
                       OR (SHELL-IYKCHK(6)   >   ZERO)
                       MOVE    "ORHCML06"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
               END-IF
      *----(01.01.05)--UPD-END-----
      *
      *----(02.00.01)--UPD-START---
               IF      SHELL-TOKCHK        =   1
                   MOVE    1                 TO  SHELL-KBN
                   MOVE    "ORHCML07"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-HOKACHK       =   1
                   MOVE    2                 TO  SHELL-KBN
                   MOVE    "ORHCML07"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *----(02.00.01)--UPD-END-----
      *
               IF      SHELL-JIHCHK        =   1    
                   MOVE    "ORHCML11"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
      *----(01.01.01)--ADD-START---
               IF      SHELL-COMCHK-G        =   ALL "0"
                   CONTINUE
               ELSE    
                   MOVE    "ORHCML05"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *----(01.01.01)--ADD-END-----
           END-IF
      *
      *    印刷処理
      *
           IF      SPA-ZID1-FLG        =   "OK"
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *             
               MOVE    SPACE               TO  SHELLTBL
      *****    MOVE    "/usr/local/orca/scripts/allways/print.sh"
      *****                                TO  SHELLTBL-NAME
      *        ファイルデイレクトリ位置指定
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    "scripts/allways/print.sh"
                                           TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY 
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    SPA-NAI-MSTYMD      TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print.sh"          TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
               MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
               MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4
               MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
               MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
               MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7
               MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8
               MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
               MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
               MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
      *                 
               MOVE    TENSUERR            TO  SHELLTBL-ARG12
               MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
               CALL    "MCPSUB"            USING
                                           MCPAREA
                                           MCPDATA-REC
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC 
      *     
           PERFORM 490-KAKUNIN-SEC
      *     
           .
       490-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4901-PRINT-SYORI-SEC            SECTION.
      *
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *                                   
           MOVE    SPACE               TO  SHELLTBL 
      *****MOVE    "tensu.sh"          TO  WRK-SHELL-NAME
      *             
           MOVE    SPACE               TO  SHELLTBL
      *****MOVE    WRK-SHELL           TO  SHELLTBL-NAME
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    "scripts/allways/tensu.sh"
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-MSTYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    "tensu.sh"      TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  LNK-PRTKANRI-PRTNM
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
      *                 
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
           MOVE    WRK-PGID        TO  SHELLTBL-ARG14                         
           MOVE    SHELL-AREA      TO  SHELLTBL-ARG15                               
           MOVE    TENSUERR        TO  SHELLTBL-ARG16
      *             
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    PATH-SHELL-SHELL
                                   TO  MCP-PATH
           CALL    "MCPSUB"        USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF                                       
           .
       4501-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID                        
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD 
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *       
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SYS-3003-JOBID                                      
      *
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           IF      FLG-SYSKANRI        =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF         
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"   
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    SPACE               TO  Z97
           MOVE    ZERO                TO  Z97-COUNT
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC 
                   IF      SJOBKANRI-RETURN    =   ZERO
                       MOVE    JOB-JOBID       TO  Z97-RENNUM   (IDX)
                       MOVE    JOB-SHELLMSG    TO  Z97-SHORINM  (IDX)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  Z97-STARTTIME(IDX)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  Z97-ENDTIME  (IDX)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  Z97-PAGE     (IDX)
                       MOVE    JOB-YOBI        TO  Z97-ERRMSG   (IDX)
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "処理中です"    TO  Z97-MSG
                       ELSE
                           IF      JOB-ERRCD       =   SPACE
                               CONTINUE
                           ELSE
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE      
                                                   INTO  Z97-MSG
                           END-IF                          
                       END-IF    
                       MOVE    IDX                     TO  Z97-COUNT    
                   END-IF
               END-PERFORM
           END-IF
           IF      Z97-MSG              =   SPACE    
               IF      Z97-COUNT            >   ZERO    
                   MOVE    "処理は正常に終了しました"
                                                TO  Z97-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  Z97-MSG
               END-IF                                 
           END-IF
      *                                      
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Z91"                TO  SPA-MOTOPG
           MOVE    "Z97"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "Z97"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
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
           IF      SPA-ZIDCD       NOT =   SPACE
               EVALUATE    SPA-ZIDCD
               WHEN    "1001"
               WHEN    "1002"
                   PERFORM 540-ZID3SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               WHEN    OTHER
                   PERFORM 520-ZIDSET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *     
           IF      SPA-ZIDCD2      NOT =   SPACE
               PERFORM 530-ZIDSET2-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z91    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE  SPACE  TO  Z91
      *
           MOVE  SPA-GMN-MSTYMD     TO  Z91-MSTYMD
           MOVE  SPA-GMN-SRYCHK     TO  Z91-SRYCHK
           MOVE  SPA-GMN-TOKCHK     TO  Z91-TOKCHK
           MOVE  SPA-GMN-JIHCHK     TO  Z91-JIHCHK
      *----(02.00.01)--ADD-START---
           MOVE  SPA-GMN-HOKACHK    TO  Z91-HOKACHK
      *----(02.00.01)--ADD-END-----
           MOVE  SPA-GMN-SRY(1)     TO  Z91-SRY(1)
           MOVE  SPA-GMN-SRY(2)     TO  Z91-SRY(2)
           MOVE  SPA-GMN-SYU(1)     TO  Z91-SYU(1)
           MOVE  SPA-GMN-SYU(2)     TO  Z91-SYU(2)
           MOVE  SPA-GMN-SYU(3)     TO  Z91-SYU(3)
           MOVE  SPA-GMN-KEI(1)     TO  Z91-KEI(1)
           MOVE  SPA-GMN-KEI(2)     TO  Z91-KEI(2)
           MOVE  SPA-GMN-CYU(1)     TO  Z91-CYU(1)
           MOVE  SPA-GMN-CYU(2)     TO  Z91-CYU(2)
           MOVE  SPA-GMN-IYKCHK(1)     TO  Z91-IYKCHK(1)
           MOVE  SPA-GMN-IYKCHK(2)     TO  Z91-IYKCHK(2)
           MOVE  SPA-GMN-IYKCHK(3)     TO  Z91-IYKCHK(3)
           MOVE  SPA-GMN-IYKCHK(4)     TO  Z91-IYKCHK(4)
           MOVE  SPA-GMN-IYKCHK(5)     TO  Z91-IYKCHK(5)
           MOVE  SPA-GMN-IYKCHK(6)     TO  Z91-IYKCHK(6)
      *----(01.01.05)--ADD-START---
           MOVE  SPA-GMN-IYKCHK(7)     TO  Z91-IYKCHK(7)
      *----(01.01.05)--ADD-END-----
      *----(01.01.01)--ADD-START---
           MOVE  SPA-GMN-COMCHK(1)     TO  Z91-COMCHK(1)
           MOVE  SPA-GMN-COMCHK(2)     TO  Z91-COMCHK(2)
           MOVE  SPA-GMN-COMCHK(3)     TO  Z91-COMCHK(3)
           MOVE  SPA-GMN-IYKCHKALL     TO  Z91-IYKCHKALL
           MOVE  SPA-GMN-COMCHKALL     TO  Z91-COMCHKALL
      *----(01.01.01)--ADD-END-----
      *
           PERFORM  VARYING IDX FROM 1 BY 1
                    UNTIL   IDX > SPA-SRYSTR-MAX
               MOVE  SPA-GMN-SRYSTR-LIST(IDX)  TO  Z91-SRYSTR-ITEM(IDX)
           END-PERFORM
      * 
           PERFORM  VARYING IDX FROM 1 BY 1
                    UNTIL   IDX > SPA-SRYEND-MAX
               MOVE  SPA-GMN-SRYEND-LIST(IDX)  TO  Z91-SRYEND-ITEM(IDX)
           END-PERFORM
      *    
           MOVE  SPA-SRYSTR-MAX  TO  Z91-SRYSTR-COUNT
           MOVE  SPA-SRYEND-MAX  TO  Z91-SRYEND-COUNT
      *
           IF  (SPA-GMN-SRYCHK  =  "T")  AND  (SPA-GMN-SRY(2)  =  "T")
               MOVE    SPA-Z91-SRYSTR      TO  Z91-SRYSTR 
               MOVE    SPA-Z91-SRYEND      TO  Z91-SRYEND 
               MOVE    WIDGET-NORMAL       TO  Z91-SRYSTR-STATE
               MOVE    WIDGET-NORMAL       TO  Z91-SRYEND-STATE 
           ELSE
               MOVE    SPACE               TO  SPA-Z91-SRYSTR
               MOVE    SPACE               TO  SPA-Z91-SRYEND
               MOVE    SPACE               TO  Z91-SRYSTR 
               MOVE    SPACE               TO  Z91-SRYEND 
               MOVE    WIDGET-INSENSITIVE  TO  Z91-SRYSTR-STATE
               MOVE    WIDGET-INSENSITIVE  TO  Z91-SRYEND-STATE     
           END-IF  
      *
           IF  SPA-GMN-KEI(2)  =  "T"
               MOVE    "F"                 TO  Z91-CYU(1)
               MOVE    "T"                 TO  Z91-CYU(2)
               MOVE    WIDGET-INSENSITIVE  TO  Z91-CYU-STATE(1)
               MOVE    WIDGET-INSENSITIVE  TO  Z91-CYU-STATE(2)  
           ELSE
               MOVE    WIDGET-NORMAL       TO  Z91-CYU-STATE(1)
               MOVE    WIDGET-NORMAL       TO  Z91-CYU-STATE(2)     
           END-IF      
      *             
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SRYCHK"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "SRY1"              TO  MCP-WIDGET
               WHEN    3
                   MOVE    "SRY2"              TO  MCP-WIDGET
               WHEN    4
                   MOVE    "SRYSTR"            TO  MCP-WIDGET
               WHEN    5
                   MOVE    "SRYEND"            TO  MCP-WIDGET
      *----(01.01.01)--ADD-START---
               WHEN    6
                   MOVE    "IYKCHKALL"         TO  MCP-WIDGET
      *----(01.01.01)--ADD-END-----
      *----(01.01.01)--UPD-START---
               WHEN    7
                   MOVE    "IYKCHK1"           TO  MCP-WIDGET
               WHEN    8
                   MOVE    "IYKCHK2"           TO  MCP-WIDGET
               WHEN    9
                   MOVE    "IYKCHK3"           TO  MCP-WIDGET
               WHEN    10
                   MOVE    "IYKCHK4"           TO  MCP-WIDGET
               WHEN    11
                   MOVE    "IYKCHK5"           TO  MCP-WIDGET
               WHEN    12
                   MOVE    "IYKCHK6"           TO  MCP-WIDGET
      *----(02.00.01)--UPD-START---
      *----(01.01.05)--ADD-START---
               WHEN    13
                   MOVE    "IYKCHK7"           TO  MCP-WIDGET
      *----(01.01.05)--ADD-END-----
               WHEN    14
                   MOVE    "TOKCHK"            TO  MCP-WIDGET
      *----(01.01.01)--UPD-END-----
      *----(02.00.01)--ADD-START---
               WHEN    15
                   MOVE    "HOKACHK"           TO  MCP-WIDGET
      *----(02.00.01)--ADD-END-----
               WHEN    16
                   MOVE    "JIHCHK"            TO  MCP-WIDGET
      *----(01.01.01)--ADD-START---
               WHEN    17
                   MOVE    "COMCHKALL"         TO  MCP-WIDGET
               WHEN    18
                   MOVE    "COMCHK1"           TO  MCP-WIDGET
               WHEN    19
                   MOVE    "COMCHK2"           TO  MCP-WIDGET
               WHEN    20
                   MOVE    "COMCHK3"           TO  MCP-WIDGET
      *----(01.01.01)--ADD-END-----
      *----(01.01.01)--UPD-START---
               WHEN    21
                   MOVE    "MSTYMD"            TO  MCP-WIDGET
               WHEN    22
                   MOVE    "KEI1"              TO  MCP-WIDGET
               WHEN    23
                   MOVE    "KEI2"              TO  MCP-WIDGET
               WHEN    24 
                   MOVE    "CYU1"              TO  MCP-WIDGET
               WHEN    25
                   MOVE    "CYU2"              TO  MCP-WIDGET
               WHEN    26
                   MOVE    "SYU1"              TO  MCP-WIDGET
               WHEN    27
                   MOVE    "SYU2"              TO  MCP-WIDGET
               WHEN    28
                   MOVE    "SYU3"              TO  MCP-WIDGET
      *----(01.01.01)--UPD-END-----
               WHEN    99
                   MOVE    "B12"               TO  MCP-WIDGET
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0002"
                   MOVE    "日付入力エラー"           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "日付に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "パラメタが入力されてない項目があります"
                                                      TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "出力ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-SHORIERR               TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-SHORIERR               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZERR
           MOVE    SPA-ERRCD            TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  ZERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Z91"                TO  SPA-MOTOPG
           MOVE    "ZERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "6666"
                   MOVE    "印刷処理中です"
                                            TO  WRK-ZIDMSG
               WHEN    "8888"
                   MOVE    "処理が正常終了しました"
                                            TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z91"                TO  SPA-MOTOPG
           MOVE    "ZID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-ZIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       530-ZIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD2
               WHEN    "8888"
                   MOVE    "印刷が終了しました"        TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID2
           MOVE    SPA-ZIDCD2           TO  ZID2-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID2-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z91"                TO  SPA-MOTOPG
           MOVE    "RID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-ZIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       540-ZID3SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
           WHEN    "1001"
               MOVE    "設定内容に従い帳票を印刷します"
                                       TO  WRK-ZIDMSG
           WHEN    "1002"
               MOVE    "設定内容に従い入力コード帳票を印刷します"
                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  ZID3
           MOVE    SPA-ZIDCD           TO  ZID3-ID1CODE
           MOVE    WRK-ZIDMSG          TO  ZID3-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "Z91"               TO  SPA-MOTOPG
           MOVE    "ZID3"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "ZID3"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-ZID3SET-EXT.
           EXIT.
      *****************************************************************
      *    日付変換　処理
      *****************************************************************
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
           INITIALIZE                            STS-AREA-DAY
           IF  WRK-LNKYMD  NOT  =  SPACE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                   MOVE    SGDAY-SDAY          TO  WRK-SYMD
               ELSE
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
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
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL   "ORCMCPSUB"                     USING  MCPAREA
                                                         ORCMCPAREA
                                                         MCPDATA-REC.
      *
       900-PUT-WINDOW-EXT.
           EXIT.
