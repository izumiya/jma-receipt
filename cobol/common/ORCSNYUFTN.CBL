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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSNYUFTN.
      ****************************************************************
      *    OS           LINUX                                         
      *                                                               
      *    PROGRAM      ORCSNYUFTN  /  負担金額算定サブ               
      *                                （入院用）                     
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/29    NACL−門脇    新規作成
      ****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-門脇    03/02/17  公費優先での負担計算
      *  00.00.02    NACL-小豆沢  03/03/06  点滴薬剤のまとめ計算
      *  00.00.03    NACL-太田    03/04/02  入力引数修正
      *  00.00.04    NACL-門脇    03/04/16  船員保険（被保険者）＋通勤災害
      *                                     ＋初診時　対応
      *  00.00.05    NACL-小豆沢  03/05/16  選定入院料の自己負担計算変更
      *  00.00.06    NACL-門脇    03/07/28  労災・自賠責入院対応
      *  00.00.07    NACL-小豆沢  03/08/04  入院料の保険適用外対応
      *  00.00.08    NACL-門脇    03/12/02  日別収納マスタ更新等
      *  00.00.09    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT7→33
      *                                     SRYACCT9→30
      *  01.00.10    NACL-門脇    04/01/21  労災の特定器材(09593)対応
      *  01.00.11    NACL-門脇    04/01/21  労災・自賠責の入院室料加算
      *                                     (09594)対応
      *  01.00.12    NACL-小豆沢  04/02/24  月途中の医療機関減算対応
      *  01.00.13    NACL-小豆沢  04/03/11  有床診療所１群入院基本料１
      *                                     （医師配置適合）の対応
      *  01.00.14    NACL-小豆沢  04/03/16  他医療機関受診の計算割合変更
      *                                     平成１６年４月より３０％
      *  01.00.15    NACL-門脇    04/04/23  選定入院時の他医療機関受診の
      *                                     算定修正
      *  01.00.16    NACL-門脇    05/05/31  室料差額の計算修正
      *****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01  WRK-AREA.
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-ZAIKAISU                PIC 9(04).
           03  WRK-TAX1001-KBN             PIC X(01).
      *
           03  WRK-KOYOMI-YMD              PIC 9(08).
           03  WRK-KOYOMI2-YMD.
               05  WRK-KOYOMI2-YYMM        PIC 9(06).
               05  WRK-KOYOMI2-DD          PIC 9(02).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-SHONUM                  PIC 9(03).
           03  WRK-KBNCD                   PIC X(02).
           03  WRK-KBNCD2                  PIC X(06).
      *
       01  WRK-ROSAI-AREA.
           03  WRK-SYUTEN                  PIC 9(09).
           03  WRK-TEN                     PIC 9(09).
           03  WRK-RSISHOSHIN              PIC 9(09).
           03  WRK-RSISDO                  PIC 9(09).
           03  WRK-RSIETC                  PIC 9(09).
           03  WRK-RSIETC2                 PIC 9(09).
           03  WRK-SINDAN                  PIC 9(09).
           03  WRK-MEISAI                  PIC 9(09).
           03  WRK-YOUBUKASAN              PIC 9(09).
           03  WRK-RYOYOTEATE              PIC 9(09).
      *
       01  WRK-JIBAI-TABLE.
           02  WRK-JIBAI-TBL     OCCURS   10.
              03 WRK-JIBAI-SYUTEN          PIC 9(09).
              03 WRK-JIBAI-SINDAN          PIC 9(09).
              03 WRK-JIBAI-MEISAI          PIC 9(09).
              03 WRK-JIBAI-RYOYO           PIC 9(09).
      *
       01  WRK-SKYYM-TABLE.
           02  WRK-SKYYM-TBL     OCCURS    2.
              03   WRK-SKYYM               PIC X(06).
              03   WRK-HI-ST               PIC 9(02).
              03   WRK-HI-ED               PIC 9(02).
      *
       01  WRK-DAY-TABLE.
           02  WRK-DAY-TBL       OCCURS    2.
              03 WRK-DAY-T       OCCURS   31.
                 04   WRK-DAY1             PIC 9(04).
                 04   WRK-DAY5             PIC 9(04).
      *
       01  WRK-HANBETU-TABLE.
           02  WRK-HANBETU-TBL OCCURS   10.
              03 WRK-HANBETU-KEY.
                05 WRK-HANBETU-SKYYM       PIC X(06).
                05 WRK-HANBETU-YUSENBAN    PIC 9(03).
              03 WRK-HANBETU-HKNCOMBINUM   PIC 9(04).
              03 WRK-HANBETU-DAY           PIC X(01)   OCCURS   31.
              03 WRK-HANBETU-KAISHI        PIC X(02).
              03 WRK-HANBETU-KAISHI2       PIC X(02).
              03 WRK-HANBETU-SYURYO        PIC X(02).
              03 WRK-HANBETU-SYUHKNNUM     PIC X(03).
              03 WRK-HANBETU-KOH1HKNNUM    PIC X(03).
              03 WRK-HANBETU-KOH2HKNNUM    PIC X(03).
              03 WRK-HANBETU-KOH3HKNNUM    PIC X(03).
              03 WRK-HANBETU-KOH4HKNNUM    PIC X(03).
              03 WRK-HANBETU-PTFTNRATE     PIC 9(03).
              03 WRK-HANBETU-SENINFLG      PIC X(01).                   
              03 WRK-HANBETU-SENINFLG2     PIC X(01).                   
      *
       01  WRK-HANBETU2-TABLE.
           02  WRK-HANBETU2-TBL OCCURS   10.
              03 WRK-HANBETU2-SKYYM        PIC X(06).
              03 WRK-HANBETU2-HKNCOMBINUM  PIC 9(04).
              03 WRK-HANBETU2-DAY          PIC X(01)   OCCURS   31.
      *
       01  WRK-HANBETU-DATA                PIC X(70).
      *
       01  WRK-KOSEI-TABLE.
           03  WRK-KOSEI-TBL    OCCURS    2.
              05 WRK-KOSEI-YYMM            PIC X(06).
              05 WRK-KOSEI-NISUU           PIC 9(02).
              05 WRK-KOSEI-DAY             PIC X(01)   OCCURS   31.
      *
       01  WRK-IKUSEI-TABLE.
           03  WRK-IKUSEI-TBL   OCCURS    2.
              05 WRK-IKUSEI-YYMM           PIC X(06).
              05 WRK-IKUSEI-NISUU          PIC 9(02).
              05 WRK-IKUSEI-DAY            PIC X(01)   OCCURS   31.
      *
      *    入院点数算定用ワークエリア
       01  WRK-NYUIN-TENSU-AREA.
           03  WRK-KHNTEN                  PIC  9(08). 
           03  WRK-KSNTEN                  PIC  S9(08).
           03  NYUIN-COUNT                 PIC  9(02). 
           03  SINRYO-COUNT                PIC  9(04). 
           03  WRK-SENTEI-ZAITEN           PIC  9(08).
           03  WRK-SENTEI-KHNTEN           PIC  9(08).
      *
       01  FLG-AREA.
           03  FLG-HANBETU                 PIC 9(01).
      *
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-GAIHAKU-SET             PIC 9(01).
           03  FLG-SENTEI-SET              PIC 9(01).
           03  FLG-NYUKHN                  PIC 9(01).
           03  FLG-KAIKEI-SAKUSEI          PIC 9(01).
           03  FLG-SENTEICDCHG             PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
      *
      * 室料差額退避用テーブル     
       01  TABLE-RMSAGAKU.
           03  TBL-RMSAGAKU                PIC 9(05)   OCCURS  99.
      *
      * 入院料フラグ退避用テーブル
       01  TABLE-NYUIN.                                                 
           03  TBL-NYUIN                   PIC 9(01)   OCCURS  31.      
      *
      * 外泊退避用テーブル     
       01  TABLE-GAIHAKU.
           03  TBL-GAIHAKU                 PIC 9(01)   OCCURS  31.
      *
      * 未実施減算退避用テーブル     
       01  TABLE-GENSAN.
           03  TBL-INNAI-KANSEN            PIC 9(02)   OCCURS  31.
           03  TBL-IRYOU-ANZEN             PIC 9(02)   OCCURS  31.
           03  TBL-JOKUSOU                 PIC 9(02)   OCCURS  31.
       01  WRK-INNAI-KANSEN-GENTEN         PIC 9(03).
       01  WRK-IRYOU-ANZEN-GENTEN          PIC 9(03).
       01  WRK-JOKUSOU-GENTEN              PIC 9(03).
      *
      * 診療会計剤回数退避用テーブル
       01  TABLE-ZAIKAISU.                                              
           03  TBL-ZAIKAISU                PIC 9(03)   OCCURS  31.      
      *
      * 外泊カウント領域     
       01  WRK-GAIHAKU-COUNT               PIC 9(02).
      *
      * 医療機関情報（入院−基本）
       01  WRK-SYS5000-COUNT               PIC 9(02).
      *
      * 選定療養退避用テーブル
       01  TABLE-SENTEI.
           03  TBL-SENTEI                  PIC 9(01)   OCCURS  31.
      *
      * 外泊時基本点数取得領域
       01  WRK-GAIHAKU-SRYCD               PIC X(09).
       01  WRK-GAIHAKU-KHNTEN              PIC 9(05).
      *
      * 選定外泊計算領域
       01  WRK-SENTEI-GAIHAKU-KEISU        PIC 9V99.
       01  WRK-SENTEI-GAIHAKU1             PIC 9(08).
       01  WRK-SENTEI-GAIHAKU2             PIC 9(08).
      *
      * 保険適用外金額退避用テーブル
       01  HKNTEKGAI-TABLE.
           03  HKNTEKGAI-TBL               OCCURS 11.
               05  TBL-TGMONEY             PIC 9(08).
               05  TBL-TGMONEY-TAX         PIC 9(08).
      *
      * 入院保険適用外金額退避用領域
       01  HKNTEKGAI-NYUIN.
           03  NYUIN-TGMONEY               PIC 9(08)V9(1).
           03  NYUIN-TGMONEY-TAX           PIC 9(08)V9(1).
      *
      * 自費退避用テーブル
       01  JIHI-TABLE.
           03  JIHI-TBL                    OCCURS 5.
               05  TBL-JIHI-TAXNASI        PIC 9(08).
               05  TBL-JIHI-TAXARI         PIC 9(08).
      *
      *    パス名領域
       01  WK-PATH                         PIC X(20).
      *
      *    添え字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(02).
           03  IDY                         PIC 9(02).
           03  IDZ                         PIC 9(02).
           03  IDX1                        PIC 9(02).
           03  IDX2                        PIC 9(02).
           03  IDX3                        PIC 9(02).
           03  IDX4                        PIC 9(02).
           03  IDX5                        PIC 9(02).
           03  SYU-IDX                     PIC 9(02).
           03  NYUINACT-IDX                PIC 9(02).
           03  RMSAGAKU-IDX                PIC 9(02).
           03  GENSAN-IDX                  PIC 9(02).
           03  SENTEI-IDX                  PIC 9(02).
           03  KOHFTN-IDX                  PIC 9(02).
           03  KOHFTN-IDX2                 PIC 9(02).
           03  KIRIKAE-IDX                 PIC 9(02).
           03  KIRIKAE-IDXX     REDEFINES   KIRIKAE-IDX
                                           PIC X(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY               PIC 9(04).
               05  WRK-SYSMM               PIC 9(02).
               05  WRK-SYSDD               PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK5000.INC".
           COPY    "CPSK5001.INC".
      *
           COPY    "CPSK5113.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ワーク収納マスター
       01  WRK-SYUNOU-REC-T.
         02 WRK-SYUNOU-REC     OCCURS 10.
           COPY    "CPSYUNOU.INC" REPLACING  //SYU-// BY //WRK-SYU-//.
      *
      *    日別収納マスタ                                              
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    ワーク日別収納マスタ                                        
       01  WRK-SYUDAY-REC-T.
         02 WRK-SYUDAY-REC     OCCURS 10.
           COPY    "CPSYUDAY.INC" REPLACING  //SDAY-// BY //WRK-SDAY-//.
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    負担金額計算用パラメタ
       01  WRK-ORCS02-REC.
           COPY    "CPORCS02.INC"   REPLACING  //LNK-// BY //WRK-//.
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    入院調剤料更新サブ
           COPY    "CPORCSCHOZAI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA". 
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    入院点滴一括更新サブ
           COPY    "CPORCSTENTEK.INC".
      *
      *    病棟選定入院自己負担取得サブ
           COPY    "CPORCSBTUSENTEI.INC".
      *
      *    労災入院会計作成サブ
           COPY    "CPORCSROUNYUIN.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "COMMON-SPA".
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.                                           
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      /
       PROCEDURE           DIVISION  USING
                                     SPA-AREA
                                     LNK-ORCS02-REC
                                     LNK-SYUNOU-REC-T
                                     LNK-SYUDAY-REC-T.                 
      ******************************************************************
      *        依頼制御処理
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
      *    ACCEPT  SYS-YMD         FROM    DATE.
      *    COMPUTE WRK-SYSYY       =   SYS-YY      +   2000.
      *    MOVE    SYS-MM          TO  WRK-SYSMM.
      *    MOVE    SYS-DD          TO  WRK-SYSDD.
      *
           MOVE    ZERO            TO  LNK-S02-RC.
           MOVE    ZERO            TO  NYUIN-COUNT.
      *
           INITIALIZE    LNK-SYUNOU-REC-T.
           INITIALIZE    LNK-SYUDAY-REC-T.                             
      *
           INITIALIZE    WRK-SYUNOU-REC-T.
           INITIALIZE    WRK-SYUDAY-REC-T.                             
      *
           INITIALIZE    WRK-AREA
                         WRK-SKYYM-TABLE
                         WRK-DAY-TABLE
                         WRK-HANBETU-TABLE
                         WRK-HANBETU2-TABLE
                         WRK-KOSEI-TABLE
                         WRK-IKUSEI-TABLE
                         WRK-JIBAI-TABLE
                         FLG-AREA.
      *
      *    調剤料一括変更処理
      ****     PERFORM       NYUIN-CHOZAI-SEC.
      *
           PERFORM       NYUIN-HANBETU-SEC.
      *
           IF    FLG-KAIKEI-SAKUSEI    =    ZERO
                 MOVE   13       TO    LNK-S02-RC
                 GO   TO   MAIN-EXT
           END-IF.
      *
           PERFORM       SYUNOU-SET-SEC.
      *
      *    入院会計から収納エリアへのセット
           PERFORM    VARYING   IDX    FROM   1   BY   1
                      UNTIL  (IDX            >    10)
                        OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
              IF   LNK-SYU-SYUHKNNUM(IDX)  =  "971"  OR  "973"
                      PERFORM       NYUIN-KAIKEI-SET2-SEC
              ELSE
                      PERFORM       NYUIN-KAIKEI-SET-SEC
              END-IF
           END-PERFORM.
      *
      *    診療会計から収納エリアへのセット
           PERFORM    VARYING   IDX    FROM   1   BY   1
                      UNTIL  (IDX            >    10)
                        OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
                      PERFORM       SINRYO-KAIKEI-SET-SEC
           END-PERFORM.
      *
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
                                OR   (LNK-S02-RC        NOT =  ZERO )
      *
                MOVE     WRK-HANBETU-SENINFLG2(IDX)                     
                                              TO  LNK-S02-SENINFLG      
      *
                MOVE     WRK-JIBAI-SYUTEN(IDX)
                                              TO  LNK-S02-SYUTEN
                MOVE     WRK-JIBAI-SINDAN(IDX)
                                              TO  LNK-S02-SINDAN
                MOVE     WRK-JIBAI-MEISAI(IDX)
                                              TO  LNK-S02-MEISAI
                MOVE     WRK-JIBAI-RYOYO(IDX) TO  LNK-S02-RYOYO
      *
                MOVE     LNK-ORCS02-REC       TO  WRK-ORCS02-REC
                MOVE     LNK-SYUNOU-REC(IDX)  TO  SYUNOU-REC
                MOVE     LNK-SYUDAY-REC(IDX)  TO  SYUDAY-REC           
      *
                IF       IDX   >   1
                   PERFORM    VARYING   IDY    FROM   1   BY   1
                                 UNTIL  IDY    >    IDX - 1
      *
                      MOVE  LNK-SYUNOU-REC(IDY) TO WRK-SYUNOU-REC(IDY)  
                      MOVE  LNK-SYUDAY-REC(IDY) TO WRK-SYUDAY-REC(IDY) 
      *
                   END-PERFORM
                END-IF
      *
                CALL     "ORCS02"     USING    MCPAREA
                                               WRK-ORCS02-REC
                                               SYUNOU-REC
                                               WRK-SYUNOU-REC-T
                                               SYUDAY-REC              
                                               WRK-SYUDAY-REC-T        
      *
                MOVE     "1"                  TO  SYU-CREATEKBN
                                                  SDAY-CREATEKBN       
                MOVE     SYUNOU-REC           TO  LNK-SYUNOU-REC(IDX)
                MOVE     SYUDAY-REC           TO  LNK-SYUDAY-REC(IDX)  
                MOVE     WRK-ORCS02-REC       TO  LNK-ORCS02-REC 
      *
           END-PERFORM.
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      *****************************************************************
      *    調剤料一括変更処理
      *****************************************************************
       NYUIN-CHOZAI-SEC           SECTION.
      *
           INITIALIZE             ORCSCHOZAIAREA
           MOVE       LNK-S02-HOSPID
                                   TO   SPA-HOSPID
           MOVE       "1"          TO   ORCSCHO-KBN
           MOVE       LNK-S02-PTID TO   ORCSCHO-PTID
           MOVE       LNK-S02-SKYSTYMD(1:6)
                                  TO    ORCSCHO-SRYYM
           CALL  "ORCSCHOZAI"     USING   SPA-AREA
                                          ORCSCHOZAIAREA
      *
           .
      *
       NYUIN-CHOZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    月別保険別判別処理
      *****************************************************************
       NYUIN-HANBETU-SEC           SECTION.
      *
           PERFORM    SKYYM-TABLE-SET-SEC.
      *
           PERFORM    NYUINKAIKEI-SYORI-SEC.
      *
           PERFORM    SINRYOKAIKEI-SYORI-SEC.
      *
           PERFORM    KAISHISYURYO-SYORI-SEC.
      *
           PERFORM    HKNNUM-SYUTOKU-SYORI-SEC.
      *
           PERFORM    KOHFTN-DAY-SYORI-SEC.
      *
           PERFORM    NARABIKAE-SYORI-SEC.
      *
       NYUIN-HANBETU-EXT.
           EXIT.
      *
      ******************************************************************
      *    請求年月別の開始日・終了日セット                            *
      ******************************************************************
       SKYYM-TABLE-SET-SEC         SECTION.
      *
           MOVE          LNK-S02-SKYSTYMD(1:6)  TO  WRK-SKYYM(1).
      *     
           INITIALIZE                               STS-AREA-DAY.
           INITIALIZE                               LNK-DAY7-AREA.
           MOVE          "71"                   TO  LNK-DAY7-IRAI.
           MOVE          LNK-S02-SKYSTYMD(1:6)  TO  LNK-DAY7-YM.
           CALL          "ORCSDAY"          USING   STS-AREA-DAY
                                                    LNK-DAY7-AREA.
           MOVE          LNK-DAY7-KOYOMI        TO  WRK-EDYMD. 
      *
           MOVE          LNK-S02-SKYSTYMD(7:2)  TO  WRK-HI-ST(1).
      *    IF      LNK-S02-SKYEDYMD(5:2) > LNK-S02-SKYSTYMD(5:2)
           IF      LNK-S02-SKYEDYMD(1:6) > LNK-S02-SKYSTYMD(1:6)
              MOVE       WRK-EDYMD(7:2)         TO  WRK-HI-ED(1) 
           ELSE
              MOVE       LNK-S02-SKYEDYMD(7:2)  TO  WRK-HI-ED(1)
           END-IF.
      *
      *    IF      LNK-S02-SKYEDYMD(5:2) > LNK-S02-SKYSTYMD(5:2)
           IF      LNK-S02-SKYEDYMD(1:6) > LNK-S02-SKYSTYMD(1:6)
              MOVE       LNK-S02-SKYEDYMD(1:6)  TO  WRK-SKYYM(2)
              MOVE       1                      TO  WRK-HI-ST(2)
              MOVE       LNK-S02-SKYEDYMD(7:2)  TO  WRK-HI-ED(2)
           END-IF.
      *
       SKYYM-TABLE-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    入院会計処理                                                *
      ******************************************************************
       NYUINKAIKEI-SYORI-SEC         SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL    (IDX            >     2)
                                OR     (WRK-SKYYM(IDX) =  SPACE)
      *
              MOVE       ZERO               TO      FLG-KAIKEI-SAKUSEI
              INITIALIZE                            NYUINACCT-REC
              MOVE    LNK-S02-HOSPID        TO      NACCT-HOSPID
              MOVE    LNK-S02-NYUGAIKBN     TO      NACCT-NYUGAIKBN
              MOVE    LNK-S02-PTID          TO      NACCT-PTID
              MOVE    "00"                  TO      NACCT-SRYKA
              MOVE    WRK-SKYYM(IDX)        TO      NACCT-SRYYM
              MOVE    NYUINACCT-REC         TO      MCPDATA-REC
              MOVE    "DBSELECT"            TO      MCP-FUNC
              MOVE    "NYUINACCT-KEY2"      TO      ORC-DBPATH
              CALL    "ORCMCPSUB"           USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
              IF      MCP-RC             =       ZERO
      *
                  MOVE    "NYUINACCT-KEY2"  TO      ORC-DBPATH
                  PERFORM  NYUINACCT-READ-SEC
                  PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                    MOVE   1                TO   FLG-KAIKEI-SAKUSEI
      *
                    IF     NACCT-ZAISKBKBN    =    "1"  OR  "5"   
                          PERFORM    NYUIN-DAY-TAIHI-SEC
                    END-IF
      * 
                    MOVE          "NYUINACCT-KEY2"  TO   ORC-DBPATH
                    PERFORM        NYUINACCT-READ-SEC
                  END-PERFORM
      *
              END-IF
      *
              MOVE    "DBCLOSE"               TO  MCP-FUNC
              MOVE    "NYUINACCT-KEY2"        TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
      *
           END-PERFORM.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL    (IDX            >     2)
                                OR     (WRK-SKYYM(IDX) =  SPACE)
      *
              PERFORM    VARYING   IDY   FROM   WRK-HI-ST(IDX)   BY   1
                                    UNTIL    IDY    >    WRK-HI-ED(IDX)
      *
               IF      ( WRK-DAY1(IDX IDY)   NOT =  ZERO ) 
               AND     ( WRK-DAY5(IDX IDY)   NOT =  ZERO )
      * 
                  MOVE   ZERO     TO    FLG-HANBETU
                  PERFORM    VARYING   IDZ  FROM   1   BY   1
                               UNTIL ( IDZ    >    10 )
                                 OR  ( WRK-HANBETU-SKYYM(IDZ) = SPACE )
                   IF   (WRK-HANBETU-SKYYM(IDZ)    = WRK-SKYYM(IDX)) 
                   AND  (WRK-HANBETU-HKNCOMBINUM(IDZ)
                                                   = WRK-DAY5(IDX IDY))
                     MOVE "1"                TO WRK-HANBETU-DAY(IDZ IDY)
                     MOVE  1                 TO FLG-HANBETU
                     MOVE  10                TO IDZ
                   END-IF
                  END-PERFORM
      *
                  IF   FLG-HANBETU   =   ZERO
                     MOVE WRK-SKYYM(IDX)     TO WRK-HANBETU-SKYYM(IDZ)
                     MOVE WRK-DAY5(IDX IDY)  TO  
                                          WRK-HANBETU-HKNCOMBINUM(IDZ)
                     MOVE "1"                TO WRK-HANBETU-DAY(IDZ IDY)
                  END-IF
      *
               END-IF
      *
              END-PERFORM
      *
           END-PERFORM.
      *
       NYUINKAIKEI-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    会計フラグ退避処理                                          *
      ******************************************************************
       NYUIN-DAY-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   WRK-HI-ST(IDX)   BY   1
                                    UNTIL    IDY    >    WRK-HI-ED(IDX)
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
      *
               IF     NACCT-ZAISKBKBN    =    "1"  
                    MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDX IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "5"  
                    MOVE   NACCT-DAY(IDY)  TO   WRK-DAY5(IDX IDY) 
               END-IF 
      *
             END-IF
      *
           END-PERFORM.
      *
       NYUIN-DAY-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *    診療会計処理                                                *
      ******************************************************************
       SINRYOKAIKEI-SYORI-SEC         SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL    (IDX            >     2)
                                OR     (WRK-SKYYM(IDX) =  SPACE)
      *
              INITIALIZE                            SRYACCT-REC
              MOVE    LNK-S02-HOSPID        TO      ACCT-HOSPID
              MOVE    LNK-S02-NYUGAIKBN     TO      ACCT-NYUGAIKBN
              MOVE    LNK-S02-PTID          TO      ACCT-PTID
              MOVE    WRK-SKYYM(IDX)        TO      ACCT-SRYYM
              MOVE    SRYACCT-REC           TO      MCPDATA-REC
              MOVE    "DBSELECT"            TO      MCP-FUNC
              MOVE    "SRYACCT-KEY33"       TO      ORC-DBPATH
              CALL    "ORCMCPSUB"           USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
              IF      MCP-RC             =       ZERO
      *
                  MOVE    "SRYACCT-KEY33"   TO      ORC-DBPATH
                  PERFORM  SRYACCT-READ-SEC
                  PERFORM  UNTIL  (FLG-SRYACCT  =    "1")
      *
                    PERFORM    NYUIN-DAY-TAIHI2-SEC
      *
                    MOVE          "SRYACCT-KEY33" TO   ORC-DBPATH
                    PERFORM        SRYACCT-READ-SEC
      *
                  END-PERFORM
      *
              END-IF
      *
              MOVE    "DBCLOSE"               TO  MCP-FUNC
              MOVE    "SRYACCT-KEY33"         TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
      *
           END-PERFORM.
      *
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU2-SKYYM(IDX) =  SPACE)
      *
             MOVE       ZERO     TO       FLG-HANBETU
      *
             PERFORM    VARYING   IDY   FROM   1   BY   1
                              UNTIL  (IDY            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDY)  =  SPACE)
      *
               IF  (WRK-HANBETU2-SKYYM(IDX) 
                                       = WRK-HANBETU-SKYYM(IDY))
               AND (WRK-HANBETU2-HKNCOMBINUM(IDX)
                                       = WRK-HANBETU-HKNCOMBINUM(IDY)) 
      *
                   PERFORM    VARYING   IDZ   FROM   1   BY   1
                                         UNTIL    IDZ    >    31
      *
                     IF    WRK-HANBETU2-DAY(IDX IDZ)   NOT =  SPACE 
                        IF  WRK-HANBETU-DAY(IDY IDZ) = SPACE
                           MOVE   "2"   TO   WRK-HANBETU-DAY(IDY IDZ)
                        END-IF
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE       1      TO       FLG-HANBETU
                   MOVE      10      TO       IDY 
      *
               END-IF
      *
             END-PERFORM
      *
             IF   FLG-HANBETU   =   ZERO
      *
                   MOVE  WRK-HANBETU2-SKYYM(IDX)
                                 TO  WRK-HANBETU-SKYYM(IDY)
                   MOVE  WRK-HANBETU2-HKNCOMBINUM(IDX)
                                 TO  WRK-HANBETU-HKNCOMBINUM(IDY)
                   PERFORM    VARYING   IDZ   FROM   1   BY   1
                                         UNTIL    IDZ    >    31
                      IF    WRK-HANBETU2-DAY(IDX IDZ)   NOT =  SPACE 
                          MOVE  "2"     TO  WRK-HANBETU-DAY(IDY IDZ)
                      END-IF 
                   END-PERFORM
      *
             END-IF
      *
           END-PERFORM.
      *
       SINRYOKAIKEI-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    会計フラグ退避２処理                                         *
      ******************************************************************
       NYUIN-DAY-TAIHI2-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   WRK-HI-ST(IDX)   BY   1
                                    UNTIL    IDY    >    WRK-HI-ED(IDX)
      *
             IF        ACCT-DAY(1 IDY)   NOT =  ZERO 
                 MOVE   ZERO     TO    FLG-HANBETU
                 PERFORM    VARYING   IDZ  FROM   1   BY   1
                              UNTIL  ( IDZ    >    10 )
                                OR   ( WRK-HANBETU2-SKYYM(IDZ) = SPACE )
                   IF    WRK-HANBETU2-SKYYM(IDZ)    = WRK-SKYYM(IDX) 
                   AND   WRK-HANBETU2-HKNCOMBINUM(IDZ)
                                                    = ACCT-HKNCOMBI
                     MOVE   "2"        TO  WRK-HANBETU2-DAY(IDZ IDY)
                     MOVE    1         TO  FLG-HANBETU
                     MOVE    10        TO  IDZ
                   END-IF
                 END-PERFORM
                 IF   FLG-HANBETU   =   ZERO
                     MOVE   WRK-SKYYM(IDX) 
                                       TO  WRK-HANBETU2-SKYYM(IDZ)
                     MOVE   ACCT-HKNCOMBI      
                                       TO  WRK-HANBETU2-HKNCOMBINUM(IDZ)
                     MOVE   "2"        TO  WRK-HANBETU2-DAY(IDZ IDY)
                 END-IF
             END-IF
      *
           END-PERFORM.
      *
       NYUIN-DAY-TAIHI2-EXT.
           EXIT.
      *
      ******************************************************************
      *    データ別開始・終了日セット                                  *
      ******************************************************************
       KAISHISYURYO-SYORI-SEC        SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
              PERFORM    VARYING   IDY   FROM   1   BY   1
                                         UNTIL    IDY    >    31
                IF        WRK-HANBETU-DAY(IDX IDY)   NOT =  SPACE 
                   MOVE   IDY     TO    WRK-HANBETU-KAISHI(IDX)
                   MOVE   31      TO    IDY
                END-IF
              END-PERFORM
      *
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                  MOVE   WRK-HI-ST(1)  TO  WRK-HANBETU-KAISHI2(IDX)
              END-IF
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                  MOVE   WRK-HI-ST(2)  TO  WRK-HANBETU-KAISHI2(IDX)
              END-IF
      *
      ****    PERFORM    VARYING   IDY   FROM  31   BY  -1
      ****                               UNTIL    IDY    <    1
      ****      IF        WRK-HANBETU-DAY(IDX IDY)   NOT =  SPACE 
      ****         MOVE   IDY     TO    WRK-HANBETU-SYURYO(IDX)
      ****         MOVE   1       TO    IDY
      ****      END-IF
      ****    END-PERFORM
      *
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                  MOVE   WRK-HI-ED(1)  TO  WRK-HANBETU-SYURYO(IDX)
              END-IF
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                  MOVE   WRK-HI-ED(2)  TO  WRK-HANBETU-SYURYO(IDX)
              END-IF
      *
           END-PERFORM.
      *
       KAISHISYURYO-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険番号取得                                                *
      ******************************************************************
       HKNNUM-SYUTOKU-SYORI-SEC        SECTION.
      *
           MOVE  WRK-SKYYM(1)    TO     WRK-KOSEI-YYMM(1)
                                        WRK-IKUSEI-YYMM(1)
      *
           MOVE  WRK-SKYYM(2)    TO     WRK-KOSEI-YYMM(2)
                                        WRK-IKUSEI-YYMM(2)
      *
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
      *       保険組合せマスタの検索
              INITIALIZE                           HKNCOMBI-REC
              MOVE    LNK-S02-HOSPID       TO      COMB-HOSPID
              MOVE    LNK-S02-PTID         TO      COMB-PTID
              MOVE    WRK-HANBETU-HKNCOMBINUM(IDX)
                                           TO      COMB-HKNCOMBINUM
              MOVE    HKNCOMBI-REC         TO      MCPDATA-REC
              MOVE    "DBSELECT"           TO      MCP-FUNC
              MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH
              CALL    "ORCMCPSUB"          USING   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
      *
              IF      MCP-RC               =       ZERO
                  MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH 
                  PERFORM   HKNCOMBI-READ-SEC
              END-IF
      *
              MOVE    "DBCLOSE"            TO      MCP-FUNC
              MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH
              CALL    "ORCMCPSUB"           USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
      *
              MOVE COMB-HKNNUM        TO WRK-HANBETU-SYUHKNNUM(IDX)
              MOVE COMB-KOH1HKNNUM    TO WRK-HANBETU-KOH1HKNNUM(IDX)
              MOVE COMB-KOH2HKNNUM    TO WRK-HANBETU-KOH2HKNNUM(IDX)
              MOVE COMB-KOH3HKNNUM    TO WRK-HANBETU-KOH3HKNNUM(IDX)
              MOVE COMB-KOH4HKNNUM    TO WRK-HANBETU-KOH4HKNNUM(IDX)
              MOVE COMB-NYU-KFTNRATE  TO WRK-HANBETU-PTFTNRATE(IDX)
      *
              IF   COMB-HKNNUM        =    "002"                        
                 EVALUATE   COMB-HOJOKBN                                
                    WHEN   "3"                                          
                    WHEN   "C"                                          
                    WHEN   "F"                                          
                        MOVE   "1"    TO WRK-HANBETU-SENINFLG(IDX)      
                 END-EVALUATE                                           
              END-IF                                                    
      *
      *       公費優先に優先番号セット
              IF  WRK-HANBETU-KOH1HKNNUM(IDX) = SPACE OR "027"
                  CONTINUE
              ELSE
      **          IF  WRK-HANBETU-KOH1HKNNUM(IDX)  =  "051"
      **             MOVE   1   TO  WRK-HANBETU-YUSENBAN(IDX)
      **          ELSE
                     MOVE  WRK-HANBETU-KOH1HKNNUM(IDX)
                                TO  WRK-HANBETU-YUSENBAN(IDX)
      **          END-IF
              END-IF
      *
              IF  WRK-HANBETU-KOH2HKNNUM(IDX) = SPACE 
                  CONTINUE
              ELSE
      **          IF  WRK-HANBETU-KOH2HKNNUM(IDX)  =  "051"
      **             MOVE   1   TO  WRK-HANBETU-YUSENBAN(IDX)
      **          ELSE
                    IF (WRK-HANBETU-YUSENBAN(IDX) 
                                       > WRK-HANBETU-KOH2HKNNUM(IDX))
                    OR (WRK-HANBETU-KOH1HKNNUM(IDX)  =  "027"       )
                     MOVE  WRK-HANBETU-KOH2HKNNUM(IDX)
                                      TO WRK-HANBETU-YUSENBAN(IDX)
                    END-IF
      **          END-IF
              END-IF
      *
              IF  WRK-HANBETU-KOH3HKNNUM(IDX) = SPACE 
                  CONTINUE
              ELSE
      **          IF  WRK-HANBETU-KOH3HKNNUM(IDX)  =  "051"
      **             MOVE   1   TO  WRK-HANBETU-YUSENBAN(IDX)
      **          ELSE
                    IF  WRK-HANBETU-YUSENBAN(IDX) 
                                       > WRK-HANBETU-KOH3HKNNUM(IDX)
                     MOVE  WRK-HANBETU-KOH3HKNNUM(IDX)
                                      TO WRK-HANBETU-YUSENBAN(IDX)
                    END-IF
      **          END-IF
              END-IF
      *
              IF  WRK-HANBETU-KOH4HKNNUM(IDX) = SPACE 
                  CONTINUE
              ELSE
      **          IF  WRK-HANBETU-KOH4HKNNUM(IDX)  =  "051"
      **             MOVE   1   TO  WRK-HANBETU-YUSENBAN(IDX)
      **          ELSE
                    IF  WRK-HANBETU-YUSENBAN(IDX) 
                                       > WRK-HANBETU-KOH4HKNNUM(IDX)
                     MOVE  WRK-HANBETU-KOH4HKNNUM(IDX)
                                      TO WRK-HANBETU-YUSENBAN(IDX)
                    END-IF
      **          END-IF
              END-IF
      *
              IF  WRK-HANBETU-YUSENBAN(IDX)  =   ZERO
                     MOVE   999       TO  WRK-HANBETU-YUSENBAN(IDX)
              END-IF
      *
              IF    WRK-HANBETU-KOH1HKNNUM(IDX)  =  "015"
              OR    WRK-HANBETU-KOH2HKNNUM(IDX)  =  "015"
              OR    WRK-HANBETU-KOH3HKNNUM(IDX)  =  "015"
              OR    WRK-HANBETU-KOH4HKNNUM(IDX)  =  "015"
                  IF  WRK-HANBETU-SKYYM(IDX)  =   WRK-KOSEI-YYMM(1)
                    PERFORM  VARYING  KOHFTN-IDX  FROM   1   BY   1
                               UNTIL  KOHFTN-IDX   >    31
                      IF  WRK-HANBETU-DAY(IDX KOHFTN-IDX)  NOT =  SPACE
                          MOVE   WRK-HANBETU-DAY(IDX KOHFTN-IDX)
                                       TO  WRK-KOSEI-DAY(1 KOHFTN-IDX)
                      END-IF
                    END-PERFORM
                  END-IF
                  IF  WRK-HANBETU-SKYYM(IDX)  =   WRK-KOSEI-YYMM(2)
                    PERFORM  VARYING  KOHFTN-IDX  FROM   1   BY   1
                               UNTIL  KOHFTN-IDX   >    31
                      IF  WRK-HANBETU-DAY(IDX KOHFTN-IDX)  NOT =  SPACE
                          MOVE   WRK-HANBETU-DAY(IDX KOHFTN-IDX)
                                       TO  WRK-KOSEI-DAY(2 KOHFTN-IDX)
                      END-IF
                    END-PERFORM
                  END-IF
              END-IF
      *
              IF    WRK-HANBETU-KOH1HKNNUM(IDX)  =  "016"
              OR    WRK-HANBETU-KOH2HKNNUM(IDX)  =  "016"
              OR    WRK-HANBETU-KOH3HKNNUM(IDX)  =  "016"
              OR    WRK-HANBETU-KOH4HKNNUM(IDX)  =  "016"
                  IF  WRK-HANBETU-SKYYM(IDX)  =   WRK-IKUSEI-YYMM(1)
                    PERFORM  VARYING  KOHFTN-IDX  FROM   1   BY   1
                               UNTIL  KOHFTN-IDX   >    31
                      IF  WRK-HANBETU-DAY(IDX KOHFTN-IDX)  NOT =  SPACE
                          MOVE   WRK-HANBETU-DAY(IDX KOHFTN-IDX)
                                       TO  WRK-IKUSEI-DAY(1 KOHFTN-IDX)
                      END-IF
                    END-PERFORM
                  END-IF
                  IF  WRK-HANBETU-SKYYM(IDX)  =   WRK-IKUSEI-YYMM(2)
                    PERFORM  VARYING  KOHFTN-IDX  FROM   1   BY   1
                               UNTIL  KOHFTN-IDX   >    31
                      IF  WRK-HANBETU-DAY(IDX KOHFTN-IDX)  NOT =  SPACE
                          MOVE   WRK-HANBETU-DAY(IDX KOHFTN-IDX)
                                       TO  WRK-IKUSEI-DAY(2 KOHFTN-IDX)
                      END-IF
                    END-PERFORM
                  END-IF
              END-IF
      *
           END-PERFORM.
      *
       HKNNUM-SYUTOKU-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    公費（更正・育成）の請求年月・入院日数引数へセット          *
      ******************************************************************
       KOHFTN-DAY-SYORI-SEC        SECTION.
      *
           PERFORM  VARYING  KOHFTN-IDX  FROM   1   BY   1
                     UNTIL   KOHFTN-IDX   >     2
              PERFORM  VARYING  KOHFTN-IDX2  FROM   1   BY   1
                        UNTIL   KOHFTN-IDX2   >    31
                 IF   WRK-KOSEI-DAY(KOHFTN-IDX KOHFTN-IDX2) NOT = SPACE
                     ADD     1   TO    WRK-KOSEI-NISUU(KOHFTN-IDX)
                 END-IF
                 IF   WRK-IKUSEI-DAY(KOHFTN-IDX KOHFTN-IDX2) NOT = SPACE
                     ADD     1   TO    WRK-IKUSEI-NISUU(KOHFTN-IDX)
                 END-IF
              END-PERFORM
           END-PERFORM
      *
           PERFORM  VARYING  KOHFTN-IDX  FROM   1   BY   1
                     UNTIL   KOHFTN-IDX   >     2
                 MOVE  WRK-KOSEI-YYMM(KOHFTN-IDX)  
                                 TO  LNK-S02-KOSEI-YYMM(KOHFTN-IDX)
                 MOVE  WRK-KOSEI-NISUU(KOHFTN-IDX)  
                                 TO  LNK-S02-KOSEI-NISUU(KOHFTN-IDX)
                 MOVE  WRK-IKUSEI-YYMM(KOHFTN-IDX)  
                                 TO  LNK-S02-IKUSEI-YYMM(KOHFTN-IDX)
                 MOVE  WRK-IKUSEI-NISUU(KOHFTN-IDX)  
                                 TO  LNK-S02-IKUSEI-NISUU(KOHFTN-IDX)
           END-PERFORM.
      *
       KOHFTN-DAY-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    データを請求年月、優先番号順に並び替え                      *
      ******************************************************************
       NARABIKAE-SYORI-SEC        SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
              COMPUTE    IDZ  =  IDX  +  1
      *
              PERFORM    VARYING   IDY   FROM   IDZ   BY   1
                              UNTIL  (IDY            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDY) =  SPACE)
      *
                IF       WRK-HANBETU-KEY(IDX)   >
                                                 WRK-HANBETU-KEY(IDY) 
                   MOVE  WRK-HANBETU-TBL(IDX)  TO  WRK-HANBETU-DATA
                   MOVE  WRK-HANBETU-TBL(IDY)  TO  WRK-HANBETU-TBL(IDX)
                   MOVE  WRK-HANBETU-DATA      TO  WRK-HANBETU-TBL(IDY)
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM.
      *
       NARABIKAE-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ワーク収納レコードセット処理                                *
      ******************************************************************
       SYUNOU-SET-SEC         SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
      *       保険組合せマスタの検索
      *       INITIALIZE                           HKNCOMBI-REC
      *       MOVE    LNK-S02-HOSPID       TO      COMB-HOSPID
      *       MOVE    LNK-S02-PTID         TO      COMB-PTID
      *       MOVE    WRK-HANBETU-HKNCOMBINUM(IDX)
      *                                    TO      COMB-HKNCOMBINUM
      *       MOVE    HKNCOMBI-REC         TO      MCPDATA-REC
      *       MOVE    "DBSELECT"           TO      MCP-FUNC
      *       MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH
      *       CALL    "ORCMCPSUB"          USING   MCPAREA
      *                                            ORCMCPAREA
      *                                            MCPDATA-REC
      *
      *       IF      MCP-RC               =       ZERO
      *          MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH 
      *          PERFORM   HKNCOMBI-READ-SEC
      *       END-IF
      *
      *       MOVE    "DBCLOSE"            TO      MCP-FUNC
      *       MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH
      *       CALL    "ORCMCPSUB"           USING   MCPAREA
      *                                             ORCMCPAREA
      *                                             MCPDATA-REC
      *
              MOVE LNK-S02-HOSPID     TO LNK-SYU-HOSPID(IDX)
              MOVE LNK-S02-NYUGAIKBN  TO LNK-SYU-NYUGAIKBN(IDX)
              MOVE LNK-S02-PTID       TO LNK-SYU-PTID(IDX)
              MOVE WRK-HANBETU-SKYYM(IDX)
                                      TO LNK-SYU-SRYYMD(IDX)(1:6)
              MOVE WRK-HANBETU-KAISHI(IDX)
                                      TO LNK-SYU-SRYYMD(IDX)(7:2)
      ***>    MOVE "2"                TO LNK-SYU-DENPJTIKBN(IDX)
      *       MOVE WRK-SYSYMD         TO LNK-SYU-DENPPRTYMD(IDX)
              MOVE LNK-S02-SYSYMD     TO LNK-SYU-DENPPRTYMD(IDX)
              MOVE WRK-HANBETU-SKYYM(IDX)
                                      TO LNK-SYU-SKYSTYMD(IDX)(1:6)
              MOVE WRK-HANBETU-KAISHI2(IDX)
                                      TO LNK-SYU-SKYSTYMD(IDX)(7:2)
              MOVE WRK-HANBETU-SKYYM(IDX)
                                      TO LNK-SYU-SKYEDYMD(IDX)(1:6)
              MOVE WRK-HANBETU-SYURYO(IDX)
                                      TO LNK-SYU-SKYEDYMD(IDX)(7:2)
              MOVE WRK-HANBETU-HKNCOMBINUM(IDX)
                                      TO LNK-SYU-HKNCOMBINUM(IDX)
      *       MOVE COMB-HKNNUM        TO LNK-SYU-SYUHKNNUM(IDX)
              MOVE WRK-HANBETU-SYUHKNNUM(IDX)
                                      TO LNK-SYU-SYUHKNNUM(IDX)
      *       MOVE COMB-KOH1HKNNUM    TO LNK-SYU-KOH1HKNNUM(IDX)
              MOVE WRK-HANBETU-KOH1HKNNUM(IDX)
                                      TO LNK-SYU-KOH1HKNNUM(IDX)
      *       MOVE COMB-KOH2HKNNUM    TO LNK-SYU-KOH2HKNNUM(IDX)
              MOVE WRK-HANBETU-KOH2HKNNUM(IDX)
                                      TO LNK-SYU-KOH2HKNNUM(IDX)
      *       MOVE COMB-KOH3HKNNUM    TO LNK-SYU-KOH3HKNNUM(IDX)
              MOVE WRK-HANBETU-KOH3HKNNUM(IDX)
                                      TO LNK-SYU-KOH3HKNNUM(IDX)
      *       MOVE COMB-KOH4HKNNUM    TO LNK-SYU-KOH4HKNNUM(IDX)
              MOVE WRK-HANBETU-KOH4HKNNUM(IDX)
                                      TO LNK-SYU-KOH4HKNNUM(IDX)
      *       MOVE COMB-NYU-KFTNRATE  TO LNK-SYU-PTFTNRATE(IDX)
              MOVE WRK-HANBETU-PTFTNRATE(IDX)
                                      TO LNK-SYU-PTFTNRATE(IDX)
              PERFORM    VARYING   IDY    FROM   1   BY   1
                             UNTIL  IDY            >    31
                   MOVE  WRK-HANBETU-DAY(IDX IDY) 
                                      TO LNK-SYU-DAY(IDX IDY)
              END-PERFORM
      *
      *       ワーク日別収納レコードセット処理                         
      *
              MOVE LNK-S02-HOSPID     TO LNK-SDAY-HOSPID(IDX)
              MOVE LNK-S02-NYUGAIKBN  TO LNK-SDAY-NYUGAIKBN(IDX)
              MOVE LNK-S02-PTID       TO LNK-SDAY-PTID(IDX)
              MOVE WRK-HANBETU-SKYYM(IDX)
                                      TO LNK-SDAY-SKYYM(IDX)
      *
           END-PERFORM. 
      *
       SYUNOU-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    入院会計セット処理                                *
      ******************************************************************
       NYUIN-KAIKEI-SET-SEC         SECTION.
      *
           MOVE    LNK-SYUNOU-REC(IDX) TO  SYUNOU-REC
           MOVE    LNK-SYUDAY-REC(IDX) TO  SYUDAY-REC                  
      *
      *外泊情報の取得
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SYU-HOSPID          TO  NACCT-HOSPID
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  NACCT-PTID
           MOVE    SYU-SRYYMD (1:6)  TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY9"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY9"  TO  ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
           ELSE
               MOVE    1               TO      FLG-NYUINACCT
           END-IF
      *
           MOVE     ZERO               TO      FLG-GAIHAKU-SET
           INITIALIZE                          TABLE-GAIHAKU
      *
           PERFORM      GAIHAKU-SET-SEC
               UNTIL   (FLG-NYUINACCT   =       1      )   OR
                       (FLG-GAIHAKU-SET =       1      )
      *
      *
           MOVE     "NYUINACT-KEY9"    TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *選定療養情報の取得
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SYU-HOSPID          TO  NACCT-HOSPID
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  NACCT-PTID
           MOVE    SYU-SRYYMD (1:6)  TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY9"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY9"  TO  ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
           ELSE
               MOVE    1               TO      FLG-NYUINACCT
           END-IF
      *
           MOVE     ZERO               TO      FLG-SENTEI-SET
           INITIALIZE                          TABLE-SENTEI
      *
           PERFORM      SENTEI-SET-SEC
               UNTIL   (FLG-NYUINACCT   =       1      )   OR
                       (FLG-SENTEI-SET =       1      )
      *
      *
           MOVE     "NYUINACT-KEY9"    TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *選定外泊の係数セット
           PERFORM   SENTEI-GAIHAKU-KEISU-SET-SEC
      *
      *入院料関係の取得
      *
           MOVE    ZERO                TO  NYUIN-COUNT
           INITIALIZE                      TABLE-NYUIN
           INITIALIZE                      TABLE-RMSAGAKU
           MOVE    SPACE               TO  HKNTEKGAI-NYUIN
           INITIALIZE                      HKNTEKGAI-NYUIN
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SYU-HOSPID          TO  NACCT-HOSPID
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  NACCT-PTID
           MOVE    SYU-SRYYMD (1:6)  TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY9"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY9"  TO  ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
           ELSE
               MOVE     1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM      NYUINACCT-GET-SEC
               UNTIL   (FLG-NYUINACCT   =       1      )
      *
           MOVE     "NYUINACT-KEY9"    TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           COMPUTE  NYUIN-COUNT = NYUIN-COUNT  -  WRK-GAIHAKU-COUNT
      *
      *医療機関減算の取得（システム管理テーブルより）
      *    月内に複数の医療期間情報がないかチェック
           PERFORM   SYS-5000-CHK-SEC
           DISPLAY "WRK-SYS5000-COUNT = " WRK-SYS5000-COUNT
      *
           IF        WRK-SYS5000-COUNT    =     ZERO
             MOVE    ZERO                TO  FLG-SYSKANRI
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    SYU-SRYYMD        TO  ORC-DBYMD
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
             IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               PERFORM  SYSKANRI-READ-SEC
               PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                                COMPUTE SYU-HKNTEN(11) = SYU-HKNTEN(11)
                                                    - (5 * NYUIN-COUNT)
                                PERFORM VARYING SYU-IDX FROM 1 BY 1    
                                         UNTIL    SYU-IDX  >   31      
                                 IF (TBL-NYUIN(SYU-IDX) NOT = ZERO) AND
                                    (TBL-GAIHAKU(SYU-IDX)   = ZERO)    
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) -     
                                            5                          
                                 END-IF                                
                                END-PERFORM                            
                            WHEN  3
                                COMPUTE SYU-HKNTEN(11) = SYU-HKNTEN(11)
                                                   - (10 * NYUIN-COUNT)
                                PERFORM VARYING SYU-IDX FROM 1 BY 1    
                                         UNTIL    SYU-IDX  >   31      
                                 IF (TBL-NYUIN(SYU-IDX) NOT = ZERO) AND
                                    (TBL-GAIHAKU(SYU-IDX)   = ZERO)    
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) -     
                                            10                         
                                 END-IF                                
                                END-PERFORM                            
                            WHEN  4
                                COMPUTE SYU-HKNTEN(11) = SYU-HKNTEN(11)
                                                    - (5 * NYUIN-COUNT)
                                PERFORM VARYING SYU-IDX FROM 1 BY 1    
                                         UNTIL    SYU-IDX  >   31      
                                 IF (TBL-NYUIN(SYU-IDX) NOT = ZERO) AND
                                    (TBL-GAIHAKU(SYU-IDX)   = ZERO)    
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) -     
                                            5                          
                                 END-IF                                
                                END-PERFORM                            
                        END-EVALUATE
                    END-IF
               END-PERFORM
             END-IF
             MOVE     "SYSKANRI-KEY"   TO  WK-PATH
             PERFORM   CSR-CLOSE-SEC
           ELSE
               PERFORM  SYS-5000-GET-SEC
      *
               MOVE     ZERO   TO  WRK-INNAI-KANSEN-GENTEN
               MOVE     ZERO   TO  WRK-IRYOU-ANZEN-GENTEN
               MOVE     ZERO   TO  WRK-JOKUSOU-GENTEN
      *
               PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                        UNTIL    SYU-IDX  >   31
                   IF (TBL-NYUIN(SYU-IDX) NOT = ZERO) AND
                      (TBL-GAIHAKU(SYU-IDX)   = ZERO)
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =
                               SDAY-HKNTEN(SYU-IDX) -
                               TBL-INNAI-KANSEN(SYU-IDX)
                       IF   TBL-INNAI-KANSEN(SYU-IDX)  NOT = ZERO
                            ADD   5     TO   WRK-INNAI-KANSEN-GENTEN
                       END-IF
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =
                               SDAY-HKNTEN(SYU-IDX) -
                               TBL-IRYOU-ANZEN(SYU-IDX)
                       IF   TBL-IRYOU-ANZEN(SYU-IDX)  NOT = ZERO
                            ADD   10    TO   WRK-IRYOU-ANZEN-GENTEN
                       END-IF
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =
                               SDAY-HKNTEN(SYU-IDX) -
                               TBL-JOKUSOU(SYU-IDX)
                       IF   TBL-JOKUSOU(SYU-IDX)  NOT = ZERO
                            ADD   5     TO   WRK-INNAI-KANSEN-GENTEN
                       END-IF
                   END-IF
               END-PERFORM
               COMPUTE SYU-HKNTEN(11)  =   SYU-HKNTEN(11)
                                       -   WRK-INNAI-KANSEN-GENTEN
               COMPUTE SYU-HKNTEN(11)  =   SYU-HKNTEN(11)
                                       -   WRK-IRYOU-ANZEN-GENTEN
               COMPUTE SYU-HKNTEN(11)  =   SYU-HKNTEN(11)
                                       -   WRK-JOKUSOU-GENTEN
      *
           END-IF
      *
      **     PERFORM   VARYING   IDZ   FROM  1   BY   1
      **               UNTIL     IDZ   >     11
      **               ADD    SYU-HKNTEN (IDZ)  TO  SYU-HKNTEN (12)
      **     END-PERFORM
           MOVE     "SYSKANRI-KEY"     TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    入院保険適用外
           ADD    NYUIN-TGMONEY        TO  SYU-TGMONEY(11)
           ADD    NYUIN-TGMONEY-TAX    TO  SYU-TGMONEY-TAX(11)
           ADD    NYUIN-TGMONEY        TO  SYU-TGMONEY(12)
           ADD    NYUIN-TGMONEY-TAX    TO  SYU-TGMONEY-TAX(12)
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU-REC(IDX)
           MOVE    SYUDAY-REC          TO  LNK-SYUDAY-REC(IDX)         
      *
           .
      *
       NYUIN-KAIKEI-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    入院会計セット処理（労災・自賠責用）                        *
      ******************************************************************
       NYUIN-KAIKEI-SET2-SEC         SECTION.
      *
           MOVE    LNK-SYUNOU-REC(IDX) TO  SYUNOU-REC
           MOVE    LNK-SYUDAY-REC(IDX) TO  SYUDAY-REC                  
      *
      *入院料関係の取得
      *
           INITIALIZE                      TABLE-RMSAGAKU
           MOVE    SPACE               TO  HKNTEKGAI-NYUIN
           INITIALIZE                      HKNTEKGAI-NYUIN
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SYU-HOSPID          TO  NACCT-HOSPID
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  NACCT-PTID
           MOVE    SYU-SRYYMD (1:6)  TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY9"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY9"  TO  ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
           ELSE
               MOVE     1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM      NYUINACCT-GET2-SEC
               UNTIL   (FLG-NYUINACCT   =       1      )
      *
           MOVE     "NYUINACT-KEY9"    TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    入院労災会計作成サブより入院料・外泊を取得
      *
           PERFORM      ROSAI-JIBAI-NYUINRYO-SEC
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU-REC(IDX)
           MOVE    SYUDAY-REC          TO  LNK-SYUDAY-REC(IDX)         
      *
           .
      *
       NYUIN-KAIKEI-SET2-EXT.
           EXIT.
      *
      **************************************************************
      *    外泊情報の取込み
      **************************************************************
       GAIHAKU-SET-SEC           SECTION.
      *
           IF  NACCT-SRYYM      =   SYU-SRYYMD(1:6)
               MOVE     ZERO        TO  WRK-GAIHAKU-COUNT
      *    外泊情報のテーブルセット
               IF   NACCT-ZAISKBKBN   =   "2"
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  (SYU-DAY(SYU-IDX)       =   "1")    AND
                           (NACCT-DAY(SYU-IDX) NOT =  ZERO)
                           MOVE   NACCT-DAY(SYU-IDX)
                                      TO  TBL-GAIHAKU (SYU-IDX)
                           ADD    1   TO  WRK-GAIHAKU-COUNT
                       END-IF
                   END-PERFORM
                   MOVE     1         TO  FLG-GAIHAKU-SET 
               END-IF
           END-IF
           MOVE    "NYUINACCT-KEY9"   TO  ORC-DBPATH
           PERFORM  NYUINACCT-READ-SEC
           .
      *
       GAIHAKU-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    選定療養情報の取込み
      **************************************************************
       SENTEI-SET-SEC           SECTION.
      *
           IF  NACCT-SRYYM      =   SYU-SRYYMD(1:6)
      *    選定療養情報のテーブルセット
               IF  (NACCT-ZAISKBKBN   =   "1")    AND
                   (NACCT-SRYCDTOTAL  =   099999916)
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  (SYU-DAY(SYU-IDX)       =   "1")    AND
                           (NACCT-DAY(SYU-IDX) NOT =  ZERO)
                           MOVE   NACCT-DAY(SYU-IDX)
                                      TO  TBL-SENTEI (SYU-IDX)
                       END-IF
                   END-PERFORM
                   MOVE     1         TO  FLG-SENTEI-SET 
               END-IF
           END-IF
           MOVE    "NYUINACCT-KEY9"   TO  ORC-DBPATH
           PERFORM  NYUINACCT-READ-SEC
           .
      *
       SENTEI-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    選定外泊時の係数セット処理
      **************************************************************
       SENTEI-GAIHAKU-KEISU-SET-SEC           SECTION.
      *
           MOVE      ZERO           TO     WRK-SENTEI-GAIHAKU-KEISU
      *
           IF  SYU-SRYYMD(1:6)    <=    "200303"
               MOVE   0.1           TO     WRK-SENTEI-GAIHAKU-KEISU
           END-IF
      *
           IF (SYU-SRYYMD(1:6)    >=    "200304")   AND
              (SYU-SRYYMD(1:6)    <=    "200403")
               MOVE   0.05           TO    WRK-SENTEI-GAIHAKU-KEISU
           END-IF
      *
           IF  SYU-SRYYMD(1:6)    >=    "200404"
               MOVE   ZERO          TO     WRK-SENTEI-GAIHAKU-KEISU
           END-IF
      *
           .
      *
       SENTEI-GAIHAKU-KEISU-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    入院料関係の取得
      **************************************************************
       NYUINACCT-GET-SEC           SECTION.
      *
           IF    (NACCT-ZAITEN            NOT = ZERO  )           AND
                 (NACCT-SRYYM(1:6)    =   SYU-SRYYMD(1:6))
      *
      *        入院料の集計
               IF  (NACCT-ZAISKBKBN      =   "1")   AND
                   (NACCT-ZAITEN         NOT =  ZERO)
      *            入院基本料と加減算点数の取得
                   PERFORM   NYUINKHN-GET-SEC
                   IF    WRK-GAIHAKU-SRYCD   NOT =   SPACE
                         MOVE  WRK-GAIHAKU-KHNTEN  TO  WRK-KHNTEN
                   END-IF
      *            病棟設定の選定自己負担情報の取得
                   INITIALIZE                  ORCSBTUSENTEIAREA
                   MOVE    SYU-HOSPID          TO  SBTUSENTEI-HOSPID
                   MOVE    SYU-PTID            TO  SBTUSENTEI-PTID
                   MOVE    SYU-SRYYMD (1:6)  TO  SBTUSENTEI-SRYYM
                   CALL    "ORCSBTUSENTEI"     USING
                                               ORCSBTUSENTEIAREA
      *            入院料と外泊を比較して点数算定
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  (SYU-DAY(SYU-IDX)       =   "1")    AND
                           (NACCT-DAY(SYU-IDX) NOT =  ZERO)
                           ADD        1      TO    NYUIN-COUNT
                           MOVE   NACCT-DAY(SYU-IDX)                   
                                      TO  TBL-NYUIN(SYU-IDX)           
                           IF    TBL-SENTEI(SYU-IDX)   =   ZERO
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                     ADD    NACCT-ZAITEN
                                           TO  SYU-HKNTEN(11)
                                               SDAY-HKNTEN(SYU-IDX)    
      *                          外泊（１５％）
                                 WHEN   1
                                     COMPUTE  SYU-HKNTEN(11)  =
                                              SYU-HKNTEN(11)  +
                                             ((WRK-KHNTEN    *  0.15)
                                                             +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                             ((WRK-KHNTEN    *  0.15)  
                                                             +  0.5)   
      *                          外泊（３０％）
                                 WHEN   2
                                     COMPUTE  SYU-HKNTEN(11)  =
                                              SYU-HKNTEN(11)  +
                                            ((WRK-KHNTEN      *  0.3)
                                                              +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                            ((WRK-KHNTEN      *  0.3)  
                                                              +  0.5)  
      *                          外泊（選定療養費）
                                 WHEN   3
                                     COMPUTE  SYU-HKNTEN(11)  =
                                              SYU-HKNTEN(11)  +
                                            ((WRK-KHNTEN      *
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                            ((WRK-KHNTEN      *        
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)  
      *                          他医療機関受診（１５％）平成１６年４月より３０％に変更
                                 WHEN   4
                                     IF  SYU-SRYYMD(1:6) >= "200404"
                                         COMPUTE  SYU-HKNTEN(11)  =
                                                  SYU-HKNTEN(11)  +
                                                ((WRK-KHNTEN  *  0.3)
                                                              +  0.5)
                                         COMPUTE SDAY-HKNTEN(SYU-IDX) =
                                                 SDAY-HKNTEN(SYU-IDX) +
                                               ((WRK-KHNTEN  *  0.3)
                                                              +  0.5)
                                     ELSE
                                         COMPUTE  SYU-HKNTEN(11)  =
                                                  SYU-HKNTEN(11)  +
                                                ((WRK-KHNTEN  *  0.15)
                                                              +  0.5)
                                         COMPUTE SDAY-HKNTEN(SYU-IDX) =
                                                 SDAY-HKNTEN(SYU-IDX) +
                                               ((WRK-KHNTEN  *  0.15)
                                                              +  0.5)
                                     END-IF
                               END-EVALUATE
                           ELSE
      *                        選定期間の点数算定
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                     ADD    WRK-SENTEI-ZAITEN
                                           TO  SYU-HKNTEN(11)
                                               SDAY-HKNTEN(SYU-IDX)    
      *                          外泊
                                 WHEN   1
                                     COMPUTE  SYU-HKNTEN(11)  =
                                              SYU-HKNTEN(11)  +
                                            ((WRK-KHNTEN    *
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                             +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                            ((WRK-KHNTEN    *          
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                             +  0.5)   
      *                          外泊（選定療養費）
                                 WHEN   3
                                     COMPUTE  SYU-HKNTEN(11)  =
                                              SYU-HKNTEN(11)  +
                                            ((WRK-KHNTEN      *
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                            ((WRK-KHNTEN      *        
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)  
      *                          他医療機関受診
                                 WHEN   4
                                     COMPUTE  SYU-HKNTEN(11)  =
                                              SYU-HKNTEN(11)  +
                                            ((WRK-KHNTEN      *
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                            ((WRK-KHNTEN      *        
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)  
                               END-EVALUATE
      *                        選定期間の患者自己負担額算定
                               PERFORM    SENTEI-PTFTN-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        入院料加算の集計
               IF   NACCT-ZAISKBKBN      =   "6"
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  (SYU-DAY(SYU-IDX)       =   "1")    AND
                           (NACCT-DAY(SYU-IDX) NOT =  ZERO)    AND
                           (TBL-GAIHAKU(SYU-IDX)   =  ZERO  OR  4)
                           ADD   NACCT-ZAITEN   TO
                                             SYU-HKNTEN(11)
                                             SDAY-HKNTEN(SYU-IDX)      
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-IF
      *
           PERFORM   NYUINACCT-GET-ETC-SEC
      *
           MOVE    "NYUINACCT-KEY9"         TO  ORC-DBPATH
           PERFORM NYUINACCT-READ-SEC
      *
           .
       NYUINACCT-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    入院料関係の取得（その他）
      **************************************************************
       NYUINACCT-GET-ETC-SEC           SECTION.
      *
           IF     NACCT-SRYYM         =   SYU-SRYYMD(1:6)
      *        室料差額の集計
               IF   NACCT-ZAISKBKBN       =   "3"
      *
      *            システム管理テーブルから設定室料差額の取り込み
                   MOVE    ZERO                TO   FLG-SYSKANRI
                   INITIALIZE                       SYS-5113-REC
                   MOVE    "5113"              TO   SYS-5113-KANRICD
                   MOVE    SYU-SRYYMD        TO   ORC-DBYMD
                   MOVE    SYS-5113-REC        TO   MCPDATA-REC
                   MOVE    "DBSELECT"          TO   MCP-FUNC
                   MOVE    "SYSKANRI-KEY2"     TO   ORC-DBPATH
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                   IF      MCP-RC              =    ZERO
                        MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
                        PERFORM  SYSKANRI-READ-SEC
                        PERFORM   UNTIL   FLG-SYSKANRI   =   1
                            MOVE   SYSKANRI-REC   TO  SYS-5113-REC
                            MOVE   SYS-5113-KBNCD(1:2)  TO  WRK-KBNCD
                            MOVE   SYS-5113-KBNCD(3:6)  TO  WRK-KBNCD2
                            IF     WRK-KBNCD  IS  NUMERIC
                            AND    WRK-KBNCD  NOT = "00"
                            AND    WRK-KBNCD2     =  SPACE
                                MOVE   SYS-5113-BRM-SAGAKU-NM
                                      TO  TBL-RMSAGAKU(WRK-KBNCD)
                            END-IF
                            MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
                            PERFORM  SYSKANRI-READ-SEC
                        END-PERFORM
                   END-IF
      *
                   MOVE     "SYSKANRI-KEY2"    TO    WK-PATH
                   PERFORM   CSR-CLOSE-SEC
      *
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  (SYU-DAY(SYU-IDX)       =   "1")    AND
                           (NACCT-DAY(SYU-IDX) NOT =  ZERO)
                           MOVE   NACCT-DAY(SYU-IDX)  TO  RMSAGAKU-IDX
                           ADD   TBL-RMSAGAKU (RMSAGAKU-IDX)
                                                 TO   SYU-RMSAGAKU
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       NYUINACCT-GET-ETC-EXT.
           EXIT.
      *
      **************************************************************
      *    入院料関係の取得（労災・自賠責用）
      **************************************************************
       NYUINACCT-GET2-SEC           SECTION.
      *
           PERFORM   NYUINACCT-GET-ETC-SEC
      *
           MOVE    "NYUINACCT-KEY9"         TO  ORC-DBPATH
           PERFORM NYUINACCT-READ-SEC
      *
           .
       NYUINACCT-GET2-EXT.
           EXIT.
      *
      **************************************************************
      *    入院労災会計作成サブより入院料・外泊を取得（労災・自賠責用）
      **************************************************************
       ROSAI-JIBAI-NYUINRYO-SEC           SECTION.
      *
      *    入院履歴より初回入院年月日を取得
           PERFORM   ROSAI-JIBAI-NYUINRIREKI-SEC
      *
           MOVE    ZERO                TO  WRK-KOYOMI-YMD
                                           WRK-KOYOMI2-YMD
      *
      *    初回入院年月日から２週間目を算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-NYUINYMD        TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    14                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *
           MOVE    LNK-DAY6-KOYOMI     TO  WRK-KOYOMI-YMD
      *
      *
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    SYU-HOSPID          TO  LNK-SROUNYUIN-HOSPID
           MOVE    SYU-PTID            TO  LNK-SROUNYUIN-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    SYU-HKNCOMBINUM     TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    SPACE               TO  LNK-SROUNYUIN-RYAKUKBN
      *    労災乗数区分セット
           PERFORM    VARYING   SYU-IDX   FROM   1   BY   1
                       UNTIL    SYU-IDX    >    31
      *
              MOVE    SYU-SRYYMD(1:6)  TO  WRK-KOYOMI2-YYMM
              MOVE    SYU-IDX          TO  WRK-KOYOMI2-DD
              IF      WRK-KOYOMI2-YMD  <=  WRK-KOYOMI-YMD
                   MOVE    "1"    TO   LNK-SROUNYUIN-JOUSU-KBN(SYU-IDX)
              ELSE
                   MOVE    "2"    TO   LNK-SROUNYUIN-JOUSU-KBN(SYU-IDX)
              END-IF
           END-PERFORM
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
      *
           IF   LNK-SROUNYUIN-RC   =   ZERO
      *
              PERFORM    VARYING   SYU-IDX   FROM   1   BY   1
                          UNTIL    SYU-IDX    >    31
      *
                IF   LNK-SROUNYUIN-NYUIN-FLG(SYU-IDX)    =  "1"
                AND  SYU-DAY(SYU-IDX)                    =  "1"
      *
                     ADD   LNK-SROUNYUIN-ZAITEN(SYU-IDX)
                                                 TO  SYU-HKNTEN(11)
                     ADD   LNK-SROUNYUIN-ZAITEN-NACCT(SYU-IDX)
                                                 TO  SYU-HKNTEN(11)
                     ADD   LNK-SROUNYUIN-ZAITEN-ACCT(SYU-IDX)
                                                 TO  SYU-HKNTEN(11)
                     ADD   LNK-SROUNYUIN-ZAITEN-ACCTETC(SYU-IDX)
                                                 TO  SYU-HKNTEN(11)
      *
                END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       ROSAI-JIBAI-NYUINRYO-EXT.
           EXIT.
      *
      **************************************************************
      *    入院履歴より初回入院年月日を取得（労災・自賠責用）
      **************************************************************
       ROSAI-JIBAI-NYUINRIREKI-SEC           SECTION.
      *
           MOVE      SPACE      TO      WRK-NYUINYMD
           MOVE      ZERO       TO      WRK-SHONUM
      *
           MOVE      ZERO       TO      WRK-KOYOMI2-YMD
      *
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SYU-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SYU-PTID            TO  PTNYUINRRK-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    SYU-SRYYMD(1:6)     TO  PTNYUINRRK-TAIINYMD (1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
               PERFORM  PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           PERFORM    UNTIL   FLG-PTNYUINRRK   =   1
               IF      PTNYUINRRK-NYUINYMD (1:6)
                                               <   SYU-SRYYMD(1:6)
                   MOVE    1                   TO  IDX4
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-KOYOMI2-YMD
                   MOVE    WRK-KOYOMI2-DD      TO  IDX4
               END-IF 
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                               >   SYU-SRYYMD(1:6)
                   MOVE    31                  TO  IDX5
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-KOYOMI2-YMD
                   MOVE    WRK-KOYOMI2-DD      TO  IDX5
               END-IF
      *        保険組合せのチェック
               PERFORM   VARYING   IDX3   FROM   IDX4    BY  1
                       UNTIL   IDX3    >   IDX5
                   IF    SYU-DAY(IDX3)  =   "1"
                       PERFORM   ROSAI-JIBAI-NYUINRIREKI2-SEC
                       MOVE    IDX5   TO   IDX3
                   END-IF
               END-PERFORM
      *
               MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
               PERFORM  PTNYUINRRK-READ-SEC
           END-PERFORM
      *
           MOVE     "PTNYUINRRK-KEY23" TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       ROSAI-JIBAI-NYUINRIREKI-EXT.
           EXIT.
      *
      **************************************************************
      *    入院履歴より初回入院年月日を取得２（労災・自賠責用）
      **************************************************************
       ROSAI-JIBAI-NYUINRIREKI2-SEC           SECTION.
      *
      *    入院年月日
           MOVE    PTNYUINRRK-NYUINYMD   TO   WRK-NYUINYMD
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM     TO   WRK-SHONUM
      *
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    SYU-HOSPID         TO  PTNYUINRRK-HOSPID
           MOVE    SYU-PTID           TO  PTNYUINRRK-PTID
           MOVE    WRK-SHONUM         TO  PTNYUINRRK-SHONUM
           MOVE    SYU-SRYYMD(1:6)    TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"               TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    PTNYUINRRK-REC     TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY22" TO  ORC-DBPATH
           MOVE    "DBSELECT"         TO  MCP-FUNC
           CALL    "ORCMCPSUB"        USING   MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTNYUINRRK-KEY22"  TO  ORC-DBPATH
               PERFORM  PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           IF  FLG-PTNYUINRRK  =   ZERO
              IF    PTNYUINRRK-NYUINYMD   <    WRK-NYUINYMD
                  MOVE   PTNYUINRRK-NYUINYMD   TO   WRK-NYUINYMD
              END-IF
           END-IF
      *
           MOVE     "PTNYUINRRK-KEY22" TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       ROSAI-JIBAI-NYUINRIREKI2-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と加減算点数の取得
      **************************************************************
       NYUINKHN-GET-SEC           SECTION.
      *
           MOVE    ZERO               TO   WRK-KHNTEN
           MOVE    ZERO               TO   WRK-KSNTEN
           MOVE    ZERO               TO   SENTEI-IDX
      *
           MOVE    SYU-HOSPID         TO   NSRY-HOSPID
           MOVE    "1"                TO   NSRY-NYUGAIKBN
           MOVE    SYU-PTID           TO   NSRY-PTID
           MOVE    "00"               TO   NSRY-SRYKA
           MOVE    SYU-SRYYMD(1:6)  TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    1                  TO   NSRY-RENNUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "NYUINACT-KEY2"    TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC             =    ZERO
               MOVE    "NYUINACT-KEY2"     TO   ORC-DBPATH
               PERFORM NYUINACT-READ-SEC
           ELSE
               MOVE    1              TO   FLG-NYUINACT
           END-IF
           IF      FLG-NYUINACT        =   ZERO
               MOVE      ZERO          TO  FLG-NYUKHN  
               PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                 UNTIL  (FLG-NYUKHN         =   1)    OR
                        (NYUINACT-IDX       >   5)    OR
                        (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
                     MOVE    SPACE         TO   TNS-TENSU-REC
                     INITIALIZE                 TNS-TENSU-REC
                     MOVE    NSRY-SRYCD(NYUINACT-IDX) TO   TNS-SRYCD
                     MOVE    SYU-SRYYMD(1:6)  TO  ORC-DBYMD(1:6)
                     MOVE    "01"               TO  ORC-DBYMD(7:2)
                     PERFORM  TENSU-READ-SEC
                     IF       FLG-TENSU      =    ZERO
                         IF   TNS-DATAKBN    =    1
                              MOVE   TNS-TEN   TO   WRK-KHNTEN
                              MOVE   1         TO   FLG-NYUKHN
                              MOVE   NYUINACT-IDX  TO  SENTEI-IDX
                         END-IF
                     END-IF                  
                     MOVE   "TENSU-KEY"      TO  WK-PATH
                     PERFORM   CSR-CLOSE-SEC
               END-PERFORM
           END-IF
      *
      *    看護比率基準未達時の入院基本点数の取得
           PERFORM  GAIHAKU-NYUINKHN-SET-SEC
      *
      *    初期加算（及び長期減算）の点数取得
           MOVE      ZERO              TO  WRK-KSNTEN
           COMPUTE   WRK-KSNTEN        =   NACCT-ZAITEN   -
                                           WRK-KHNTEN
      *
           MOVE   "NYUINACT-KEY2"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    選定療養の入院剤点数と基本点数の取得
           MOVE      ZERO              TO   WRK-SENTEI-ZAITEN
           MOVE      ZERO              TO   WRK-SENTEI-KHNTEN
           IF        FLG-SENTEI-SET    =    1
               MOVE    ZERO            TO  FLG-SENTEICDCHG
               INITIALIZE  SENTEICDCHG-REC
               MOVE    NSRY-SRYCD(SENTEI-IDX)
                                       TO  SENCHG-IPNSRYCD
               MOVE    SENTEICDCHG-REC TO  MCPDATA-REC
               MOVE    "DBSELECT"      TO  MCP-FUNC
               MOVE    "SENTEICDCHG-KEY2"
                                       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC             =    ZERO
                   MOVE    "SENTEICDCHG-KEY2" 
                                          TO   ORC-DBPATH
                   PERFORM  SENTEICDCHG-READ-SEC
               ELSE
                   MOVE    1              TO   FLG-SENTEICDCHG
               END-IF
      *
               MOVE    "SENTEICDCHG-KEY2" TO   WK-PATH
               PERFORM  CSR-CLOSE-SEC
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               MOVE    SENCHG-SENTEISRYCD TO  TNS-SRYCD
               MOVE    SYU-SRYYMD(1:6)  TO  ORC-DBYMD(1:6)
               MOVE    "01"               TO  ORC-DBYMD(7:2)
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                   MOVE   TNS-TEN      TO   WRK-SENTEI-KHNTEN
                   COMPUTE    WRK-SENTEI-ZAITEN  =  WRK-SENTEI-KHNTEN
                                                 +  WRK-KSNTEN
               END-IF                  
               MOVE   "TENSU-KEY"      TO  WK-PATH
               PERFORM   CSR-CLOSE-SEC
      *
           END-IF
           .
       NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    外泊時の入院基本料取得
      **************************************************************
       GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
           MOVE    ZERO     TO   WRK-GAIHAKU-KHNTEN
      *
           EVALUATE  TNS-SRYCD
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               MOVE    WRK-GAIHAKU-SRYCD  TO  TNS-SRYCD
               MOVE    SYU-SRYYMD(1:6)  TO  ORC-DBYMD(1:6)
               MOVE    "01"               TO  ORC-DBYMD(7:2)
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                        MOVE   TNS-TEN   TO   WRK-GAIHAKU-KHNTEN
               END-IF                  
               MOVE   "TENSU-KEY"      TO  WK-PATH
               PERFORM   CSR-CLOSE-SEC
           END-IF
      *
           .
       GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    選定期間の患者自己負担額算定
      **************************************************************
       SENTEI-PTFTN-SEC           SECTION.
      *
      *    消費税端数区分の取り込み
           MOVE    ZERO                 TO    FLG-SYSKANRI
           INITIALIZE                         SYS-1001-REC
           MOVE    "1001"               TO    SYS-1001-KANRICD
           MOVE    "*"                  TO    SYS-1001-KBNCD
           MOVE    SYU-SRYYMD         TO    ORC-DBYMD
           MOVE    SYS-1001-REC         TO    MCPDATA-REC
           MOVE    "DBSELECT"           TO    MCP-FUNC
           MOVE    "SYSKANRI-KEY"       TO    ORC-DBPATH
           CALL    "ORCMCPSUB"    USING       MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
           IF      MCP-RC               =     ZERO
               MOVE     "SYSKANRI-KEY"  TO    ORC-DBPATH
               PERFORM   SYSKANRI-READ-SEC
               MOVE      SYS-1001-TAX-HASU-KBN       
                                        TO    WRK-TAX1001-KBN
           END-IF
      *
           MOVE     "SYSKANRI-KEY"      TO    WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYU-SRYYMD        TO  ORC-DBYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               PERFORM  SYSKANRI-READ-SEC
               MOVE    "SYSKANRI-KEY"  TO  WK-PATH
               PERFORM   CSR-CLOSE-SEC
               EVALUATE   SYS-5000-SENTEI-FTNKBN
      *            医療機関定率設定
                   WHEN   "1"
                       IF     SYS-5000-SENTEI-TANKA-X   =   SPACE
                           MOVE   10       TO   SYS-5000-SENTEI-TANKA
                       END-IF
                       EVALUATE   SYS-5000-SENTEI-TAXKBN
      *                   消費税なし
                           WHEN   SPACE
                           WHEN   "0"
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                     COMPUTE  NYUIN-TGMONEY =
                                              NYUIN-TGMONEY +
                                            ((WRK-KHNTEN  -  
                                              WRK-SENTEI-KHNTEN) * 
                                              SYS-5000-SENTEI-TANKA)
      *                          外泊（１５％と選定外泊の差額）
                                 WHEN   3
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                     COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                             (WRK-KHNTEN * 0.15) + 0.5
                                     COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                             (WRK-KHNTEN * 
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                                 + 0.5
                                     COMPUTE  NYUIN-TGMONEY  =
                                              NYUIN-TGMONEY  +
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)
                                              *  SYS-5000-SENTEI-TANKA)
                                 WHEN   4
                                     IF  SYU-SRYYMD(1:6) >= "200404"
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                      COMPUTE  WRK-SENTEI-GAIHAKU1 =
      ****                                    (WRK-KHNTEN * 0.30) + 0.5
                                              (WRK-KHNTEN * 0.00) + 0.5
                                      COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                              (WRK-KHNTEN * 
                                               WRK-SENTEI-GAIHAKU-KEISU)
                                                                  + 0.5
                                      COMPUTE  NYUIN-TGMONEY  =
                                               NYUIN-TGMONEY  +
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)
                                               *  SYS-5000-SENTEI-TANKA)
                                     ELSE
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                      COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                              (WRK-KHNTEN * 0.15) + 0.5
                                      COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                              (WRK-KHNTEN * 
                                               WRK-SENTEI-GAIHAKU-KEISU)
                                                                  + 0.5
                                      COMPUTE  NYUIN-TGMONEY  =
                                               NYUIN-TGMONEY  +
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)
                                               *  SYS-5000-SENTEI-TANKA)
                                     END-IF
                               END-EVALUATE
      *                   消費税あり
                           WHEN   "1"
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                     COMPUTE  NYUIN-TGMONEY-TAX =
                                              NYUIN-TGMONEY-TAX +
                                            ((WRK-KHNTEN  -  
                                              WRK-SENTEI-KHNTEN) * 
                                              SYS-5000-SENTEI-TANKA)
      *                          外泊（１５％と選定外泊の差額）
                                 WHEN   3
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                     COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                             (WRK-KHNTEN * 0.15) + 0.5
                                     COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                             (WRK-KHNTEN * 
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                                 + 0.5
                                     COMPUTE  NYUIN-TGMONEY-TAX  =
                                              NYUIN-TGMONEY-TAX  +
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)
                                              *  SYS-5000-SENTEI-TANKA)
                                 WHEN   4
                                     IF  SYU-SRYYMD(1:6) >= "200404"
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                      COMPUTE  WRK-SENTEI-GAIHAKU1 =
      ****                                    (WRK-KHNTEN * 0.30) + 0.5
                                              (WRK-KHNTEN * 0.00) + 0.5
                                      COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                              (WRK-KHNTEN * 
                                               WRK-SENTEI-GAIHAKU-KEISU)
                                                                  + 0.5
                                      COMPUTE  NYUIN-TGMONEY-TAX  =
                                               NYUIN-TGMONEY-TAX  +
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)
                                               *  SYS-5000-SENTEI-TANKA)
                                     ELSE
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                      COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                              (WRK-KHNTEN * 0.15) + 0.5
                                      COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                              (WRK-KHNTEN * 
                                               WRK-SENTEI-GAIHAKU-KEISU)
                                                                  + 0.5
                                      COMPUTE  NYUIN-TGMONEY-TAX  =
                                               NYUIN-TGMONEY-TAX  +
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)
                                               *  SYS-5000-SENTEI-TANKA)
                                     END-IF
                               END-EVALUATE
                       END-EVALUATE
      *            医療機関定額設定
                   WHEN   "2"
                       IF     SYS-5000-SENTEI-KINGAKU-X   =   SPACE
                           MOVE   ZERO    TO  SYS-5000-SENTEI-KINGAKU
                       END-IF
                       EVALUATE   SYS-5000-SENTEI-TAXKBN
      *                   消費税なし
                           WHEN   SPACE
                           WHEN   "0"
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                 WHEN   3
                                 WHEN   4
                                     COMPUTE  NYUIN-TGMONEY =
                                              NYUIN-TGMONEY +
                                              SYS-5000-SENTEI-KINGAKU
                               END-EVALUATE
      *                   消費税あり
                           WHEN   "1"
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                 WHEN   3
                                 WHEN   4
                                     COMPUTE  NYUIN-TGMONEY-TAX =
                                              NYUIN-TGMONEY-TAX +
                                              SYS-5000-SENTEI-KINGAKU
                               END-EVALUATE
                       END-EVALUATE
      *            病棟設定
                   WHEN   "3"
                       PERFORM   SENTEI-PTFTN-BYOTO-SEC
               END-EVALUATE
           END-IF
           .
       SENTEI-PTFTN-EXT.
           EXIT.
      *
      **************************************************************
      *    選定期間の患者自己負担額算定（病棟設定）
      **************************************************************
       SENTEI-PTFTN-BYOTO-SEC           SECTION.
      *
           EVALUATE   SBTUSENTEI-DAY-FTNKBN(SYU-IDX)
      *        病棟定率設定
               WHEN   "1"
                   EVALUATE   SBTUSENTEI-DAY-TAXKBN(SYU-IDX)
      *              消費税なし
                       WHEN   SPACE
                       WHEN   "0"
                           EVALUATE   TBL-GAIHAKU(SYU-IDX)
                             WHEN   ZERO
                                   COMPUTE  NYUIN-TGMONEY =
                                            NYUIN-TGMONEY +
                                          ((WRK-KHNTEN  -  
                                            WRK-SENTEI-KHNTEN) * 
                                         SBTUSENTEI-DAY-TANKA(SYU-IDX))
      *                      外泊（１５％と選定外泊の差額）
                             WHEN   3
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                 COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                         (WRK-KHNTEN * 0.15) + 0.5
                                 COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                         (WRK-KHNTEN * 
                                          WRK-SENTEI-GAIHAKU-KEISU)
                                                             + 0.5
                                 COMPUTE  NYUIN-TGMONEY  =
                                          NYUIN-TGMONEY  +
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)  *
                                         SBTUSENTEI-DAY-TANKA(SYU-IDX))
                             WHEN   4
                                 IF  SYU-SRYYMD(1:6) >= "200404"
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                  COMPUTE  WRK-SENTEI-GAIHAKU1 =
      ****                                (WRK-KHNTEN * 0.30) + 0.5
                                          (WRK-KHNTEN * 0.00) + 0.5
                                  COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                          (WRK-KHNTEN * 
                                           WRK-SENTEI-GAIHAKU-KEISU)
                                                              + 0.5
                                  COMPUTE  NYUIN-TGMONEY  =
                                           NYUIN-TGMONEY  +
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)  *
                                          SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 ELSE
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                  COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                          (WRK-KHNTEN * 0.15) + 0.5
                                  COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                          (WRK-KHNTEN * 
                                           WRK-SENTEI-GAIHAKU-KEISU)
                                                              + 0.5
                                  COMPUTE  NYUIN-TGMONEY  =
                                           NYUIN-TGMONEY  +
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)  *
                                          SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 END-IF
                           END-EVALUATE
      *               消費税あり
                       WHEN   "1"
                           EVALUATE   TBL-GAIHAKU(SYU-IDX)
                             WHEN   ZERO
                                 COMPUTE  NYUIN-TGMONEY-TAX =
                                          NYUIN-TGMONEY-TAX +
                                        ((WRK-KHNTEN  -  
                                          WRK-SENTEI-KHNTEN) * 
                                       SBTUSENTEI-DAY-TANKA(SYU-IDX))
      *                      外泊（１５％と選定外泊の差額）
                             WHEN   3
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                 COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                         (WRK-KHNTEN * 0.15) + 0.5
                                 COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                         (WRK-KHNTEN * 
                                          WRK-SENTEI-GAIHAKU-KEISU)
                                                             + 0.5
                                 COMPUTE  NYUIN-TGMONEY-TAX  =
                                          NYUIN-TGMONEY-TAX  +
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)
                                      *  SBTUSENTEI-DAY-TANKA(SYU-IDX))
                             WHEN   4
                                 IF  SYU-SRYYMD(1:6) >= "200404"
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                  COMPUTE  WRK-SENTEI-GAIHAKU1 =
      ****                                (WRK-KHNTEN * 0.30) + 0.5
                                          (WRK-KHNTEN * 0.00) + 0.5
                                  COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                          (WRK-KHNTEN * 
                                           WRK-SENTEI-GAIHAKU-KEISU)
                                                              + 0.5
                                  COMPUTE  NYUIN-TGMONEY-TAX  =
                                           NYUIN-TGMONEY-TAX  +
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)
                                       *  SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 ELSE
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                  COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                          (WRK-KHNTEN * 0.15) + 0.5
                                  COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                          (WRK-KHNTEN * 
                                           WRK-SENTEI-GAIHAKU-KEISU)
                                                              + 0.5
                                  COMPUTE  NYUIN-TGMONEY-TAX  =
                                           NYUIN-TGMONEY-TAX  +
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)
                                       *  SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 END-IF
                           END-EVALUATE
                   END-EVALUATE
      *        病棟定額設定
               WHEN   "2"
                   EVALUATE   SBTUSENTEI-DAY-TAXKBN(SYU-IDX)
      *               消費税なし
                       WHEN   SPACE
                       WHEN   "0"
                           EVALUATE   TBL-GAIHAKU(SYU-IDX)
                             WHEN   ZERO
                             WHEN   3
                             WHEN   4
                                 COMPUTE  NYUIN-TGMONEY =
                                          NYUIN-TGMONEY +
                                       SBTUSENTEI-DAY-KINGAKU(SYU-IDX)
                           END-EVALUATE
      *               消費税あり
                       WHEN   "1"
                           EVALUATE   TBL-GAIHAKU(SYU-IDX)
                             WHEN   ZERO
                             WHEN   3
                             WHEN   4
                                 COMPUTE  NYUIN-TGMONEY-TAX =
                                          NYUIN-TGMONEY-TAX +
                                       SBTUSENTEI-DAY-KINGAKU(SYU-IDX)
                           END-EVALUATE
                   END-EVALUATE
           END-EVALUATE
           .
       SENTEI-PTFTN-BYOTO-EXT.
           EXIT.
      *
      ******************************************************************
      *    診療会計セット処理                                *
      ******************************************************************
       SINRYO-KAIKEI-SET-SEC         SECTION.
      *
           MOVE    LNK-SYUNOU-REC(IDX) TO  SYUNOU-REC
           MOVE    LNK-SYUDAY-REC(IDX) TO  SYUDAY-REC                  
      *
           MOVE    SPACE               TO  HKNTEKGAI-TABLE
           INITIALIZE                      HKNTEKGAI-TABLE
           MOVE    SPACE               TO  JIHI-TABLE
           INITIALIZE                      JIHI-TABLE
      *
           INITIALIZE                      WRK-ROSAI-AREA
      *
      *診療会計情報の取得
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                      SRYACCT-REC
           MOVE    SYU-HOSPID          TO  ACCT-HOSPID
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  ACCT-PTID
           MOVE    SYU-SRYYMD(1:6)   TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY30"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY30" TO  ORC-DBPATH
               PERFORM    SRYACCT-READ-SEC
      *
               PERFORM    SRYACCT-SET-SEC
                      UNTIL   FLG-SRYACCT  =  1
      *
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE   "SRYACCT-KEY30"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    労災・自賠責転送処理
           IF   SYU-SYUHKNNUM    =    "971"  OR  "973"
               PERFORM   ROSAI-JIBAI-TENSO-SEC
           END-IF
      *
      *    点滴薬剤（器材）点数まとめ処理
           PERFORM   TENTEK-MATOME-SEC
      *
      *    保険点数集計
           PERFORM     VARYING   IDZ    FROM   1    BY    1
                         UNTIL   IDZ    >      11
                  ADD    SYU-HKNTEN(IDZ)      TO  SYU-HKNTEN(12)
           END-PERFORM
      *    保険適用外退避テーブルと自費退避用テーブルから
      *    収納テーブルへのセット
           PERFORM   HKNTEKGAI-SET-SEC
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU-REC(IDX)
           MOVE    SYUDAY-REC          TO  LNK-SYUDAY-REC(IDX)         
      *
           .
      *
       SINRYO-KAIKEI-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    診療会計セット処理                                *
      ******************************************************************
       SRYACCT-SET-SEC         SECTION.
      *
      *    入院日と診療日（診療会計テーブルのカレンダー）を比較して
      *    算定対象日数を求める
           MOVE      ZERO          TO     SINRYO-COUNT
           INITIALIZE                     TABLE-ZAIKAISU               
           IF   ACCT-SRYKBN  =  "90"  OR  "92"
             PERFORM   VARYING   SYU-IDX   FROM   1   BY   1
                     UNTIL      SYU-IDX   >   31
                 IF  (SYU-DAY(SYU-IDX)       =     "1"         )   AND
                     (ACCT-DAY(1 SYU-IDX)    NOT =  ZERO)          AND
                     (SYU-HKNCOMBINUM        =    ACCT-HKNCOMBI)
                     ADD   ACCT-DAY(1 SYU-IDX)
                                      TO   SINRYO-COUNT
                     IF   SYU-SYUHKNNUM  =  "971"  OR  "973"           
                        CONTINUE                                       
                     ELSE                                              
                        MOVE  ACCT-DAY(1 SYU-IDX)                      
                                      TO   TBL-ZAIKAISU(SYU-IDX)       
                     END-IF                                            
                 END-IF
             END-PERFORM
           ELSE
             PERFORM   VARYING   SYU-IDX   FROM   1   BY   1
                     UNTIL      SYU-IDX   >   31
                 IF  (SYU-DAY(SYU-IDX)       =     "1"  OR  "2")   AND
                     (ACCT-DAY(1 SYU-IDX)    NOT =  ZERO)          AND
                     (SYU-HKNCOMBINUM        =    ACCT-HKNCOMBI)
                     ADD   ACCT-DAY(1 SYU-IDX)
                                      TO   SINRYO-COUNT
                     IF   SYU-SYUHKNNUM  =  "971"  OR  "973"           
                        CONTINUE                                       
                     ELSE                                              
                        MOVE  ACCT-DAY(1 SYU-IDX)                      
                                      TO   TBL-ZAIKAISU(SYU-IDX)       
                     END-IF                                            
                 END-IF
             END-PERFORM
           END-IF
      *
           IF  SINRYO-COUNT     NOT =     ZERO
      *
      *        労災・自賠責用の編集処理
               IF   SYU-SYUHKNNUM  =  "971"  OR  "973"
                   PERFORM   ROSAI-JIBAI-HENSYU-SEC
               END-IF
      *
               EVALUATE    ACCT-SRYKBN
      *            診療
                   WHEN    "11"
                   WHEN    "12"
                    COMPUTE SYU-HKNTEN (01) =  SYU-HKNTEN (01)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
                    IF   ACCT-SRYKBN                =  "11"             
                    AND  ACCT-ZAITEN           NOT  =  ZERO             
                    AND  WRK-HANBETU-SENINFLG(IDX)  =   "1"             
                         MOVE   "1"   TO   WRK-HANBETU-SENINFLG2(IDX)   
                    END-IF                                              
      *            指導
                   WHEN    "13"
                    COMPUTE SYU-HKNTEN (02) =  SYU-HKNTEN (02)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            在宅
                   WHEN    "14"
                    COMPUTE SYU-HKNTEN (03) =  SYU-HKNTEN (03)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            投薬
                   WHEN    "21"   THRU  "27"
                   COMPUTE SYU-HKNTEN (04) =  SYU-HKNTEN (04)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            注射
                   WHEN    "31"
                   WHEN    "32"
                    COMPUTE SYU-HKNTEN (05) =  SYU-HKNTEN (05)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            点滴注射又は中心静脈注射
                   WHEN    "33"
                       PERFORM   TENTEK-GET-SEC
                       IF   FLG-SRYACT   =    ZERO
                           EVALUATE  SRY-SRYSYUKBN
                               WHEN  "330"
                               WHEN  "331"
                                  COMPUTE SYU-HKNTEN (05) = 
                                          SYU-HKNTEN (05) + 
                                         (ACCT-SYUTEN1 * SINRYO-COUNT)
                                  PERFORM VARYING SYU-IDX FROM 1 BY 1  
                                          UNTIL    SYU-IDX  >  31      
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) +     
                                           (ACCT-SYUTEN1 *             
                                            TBL-ZAIKAISU(SYU-IDX))     
                                  END-PERFORM                          
                               WHEN  "340"
                                  COMPUTE SYU-HKNTEN (05) = 
                                          SYU-HKNTEN (05) + 
                                         (ACCT-ZAITEN * SINRYO-COUNT)
                                  PERFORM VARYING SYU-IDX FROM 1 BY 1  
                                          UNTIL    SYU-IDX  >  31      
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) +     
                                           (ACCT-ZAITEN  *             
                                            TBL-ZAIKAISU(SYU-IDX))     
                                  END-PERFORM                          
                           END-EVALUATE
                       END-IF
      *            処置
                   WHEN    "40"
                    COMPUTE SYU-HKNTEN (06)   =  SYU-HKNTEN (06)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            手術
                   WHEN    "50"
                   WHEN    "54"
                    COMPUTE SYU-HKNTEN (07)   =  SYU-HKNTEN (07)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            検査
                   WHEN    "60"
                    COMPUTE SYU-HKNTEN (08)   =  SYU-HKNTEN (08)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            Ｘ線
                   WHEN    "70"
                    COMPUTE SYU-HKNTEN (09)   =  SYU-HKNTEN (09)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            その他
                   WHEN    "80"
                    COMPUTE SYU-HKNTEN (10)   =  SYU-HKNTEN (10)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            入院料加算
                   WHEN    "90"
                   WHEN    "92"
                    IF   SYU-SYUHKNNUM  =  "971"  OR  "973"
                      CONTINUE
                    ELSE
                      IF     ACCT-SRYCDTOTAL    =   190080470  OR
                                                    190799670  OR
                                                    190080570  OR
                                                    190799770  OR
                                                    190111690  OR
                                                    190809090  OR
                                                    190111790  OR
                                                    190809190
                        COMPUTE SYU-HKNTEN (11)   =  SYU-HKNTEN (11) -
                               (ACCT-ZAITEN  *   SINRYO-COUNT)
                        PERFORM  VARYING  SYU-IDX  FROM  1  BY  1      
                                   UNTIL    SYU-IDX   >    31          
                           COMPUTE SDAY-HKNTEN(SYU-IDX) =              
                                   SDAY-HKNTEN(SYU-IDX) -              
                                  (ACCT-ZAITEN  *                      
                                   TBL-ZAIKAISU(SYU-IDX))              
                        END-PERFORM                                    
                      ELSE
                        COMPUTE SYU-HKNTEN (11)   =  SYU-HKNTEN (11) +
                               (ACCT-ZAITEN  *   SINRYO-COUNT)
                        PERFORM  VARYING  SYU-IDX  FROM  1  BY  1      
                                   UNTIL    SYU-IDX   >    31          
                           COMPUTE SDAY-HKNTEN(SYU-IDX) =              
                                   SDAY-HKNTEN(SYU-IDX) +              
                                  (ACCT-ZAITEN  *                      
                                   TBL-ZAIKAISU(SYU-IDX))              
                        END-PERFORM                                    
                      END-IF
                    END-IF
      *            保険適用外及び自費
                   WHEN    "95"
                   WHEN    "96"
                       MOVE    ACCT-HOSPID        TO   SRY-HOSPID
                       MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
                       MOVE    ACCT-PTID          TO   SRY-PTID
                       MOVE    ACCT-SRYKA         TO   SRY-SRYKA
                       MOVE    ACCT-SRYYM         TO   SRY-SRYYM
                       MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
                       MOVE    SRYACT-REC         TO   MCPDATA-REC
                       MOVE    "DBSELECT"         TO   MCP-FUNC
                       MOVE    "SRYACT-KEY2"      TO   ORC-DBPATH
                       CALL    "ORCMCPSUB"             USING
                                                       MCPAREA
                                                       ORCMCPAREA
                                                       MCPDATA-REC
                       IF      MCP-RC             =    ZERO
                         MOVE    "SRYACT-KEY2"    TO   ORC-DBPATH
                         PERFORM  SRYACT-READ-SEC
                       ELSE
                         MOVE    1                TO   FLG-SRYACT
                       END-IF
                       IF      FLG-SRYACT          =   ZERO
                           PERFORM  UNTIL
                               (FLG-SRYACT         =  1)            OR
                               (ACCT-HOSPID  NOT = SRY-HOSPID)      OR
                               (ACCT-NYUGAIKBN  NOT = SRY-NYUGAIKBN) OR
                               (ACCT-PTID     NOT = SRY-PTID)       OR
                               (ACCT-SRYKA    NOT = SRY-SRYKA)      OR
                               (ACCT-SRYKBN   NOT = SRY-SRYKBN)     OR
                               (ACCT-ZAINUM   NOT = SRY-ZAINUM)
      *                        保険適用外金額セット
                               PERFORM    HKNTEKGAI-TBL-SEC
                               MOVE      "SRYACT-KEY2"  TO  ORC-DBPATH
                               PERFORM    SRYACT-READ-SEC
                           END-PERFORM
                       END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    "SRYACCT-KEY30" TO  ORC-DBPATH
           PERFORM SRYACCT-READ-SEC
      *
           .
      *
       SRYACCT-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    労災・自賠責用の編集処理
      **************************************************************
       ROSAI-JIBAI-HENSYU-SEC          SECTION.
      *
           IF   ACCT-SRYKBN  =  "90" OR "92" OR "95" OR "96" OR "97"
             CONTINUE
           ELSE
      *
             IF     SYU-SYUHKNNUM   =   "973"
                COMPUTE     WRK-SYUTEN  =  WRK-SYUTEN
                                 +  ( ACCT-SYUTEN1 * SINRYO-COUNT )
                IF     ACCT-SRYKBN   =   "70"
                   COMPUTE  WRK-SYUTEN  =  WRK-SYUTEN
                                 -  ( ACCT-SYUTEN2 * SINRYO-COUNT )
                END-IF
             END-IF
      *
             INITIALIZE                      SRYACT-REC
             MOVE    ACCT-HOSPID        TO   SRY-HOSPID
             MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
             MOVE    ACCT-PTID          TO   SRY-PTID
             MOVE    ACCT-SRYKA         TO   SRY-SRYKA
             MOVE    ACCT-SRYYM         TO   SRY-SRYYM
             MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
             MOVE    SRYACT-REC         TO   MCPDATA-REC
             MOVE    "DBSELECT"         TO   MCP-FUNC
             MOVE    "SRYACT-KEY2"      TO   ORC-DBPATH
             CALL    "ORCMCPSUB"             USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
             IF      MCP-RC             =    ZERO
                MOVE    "SRYACT-KEY2"    TO   ORC-DBPATH
                PERFORM  SRYACT-READ-SEC
                PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                  PERFORM   VARYING   IDZ   FROM   1   BY   1
                       UNTIL    (IDZ            >       5)  OR
                                (SRY-SRYCD(IDZ) =   SPACE)
      *
                    MOVE    SPACE             TO   TNS-TENSU-REC
                    INITIALIZE                     TNS-TENSU-REC
                    MOVE    SRY-SRYCD(IDZ)    TO   TNS-SRYCD
                    MOVE    SYU-SRYYMD(1:6) TO   ORC-DBYMD(1:6)
                    MOVE    "01"              TO   ORC-DBYMD(7:2)
                    PERFORM    TENSU-READ-SEC
      *
                    IF       FLG-TENSU      =    ZERO
      *
                      COMPUTE  WRK-TEN     =   TNS-TEN  /  10
      *
                      IF   SRY-SRYCD(IDZ) =  "101110010"  OR
                                             "101110020"
                           COMPUTE WRK-RSISHOSHIN = WRK-RSISHOSHIN +
                                  (WRK-TEN  *   SINRYO-COUNT)
                      END-IF
                      IF   SRY-SRYCD(IDZ) =  "101130010"  OR
                                             "101130020"  OR
                                             "101130030"  OR
                                             "101130040"  OR
                                             "101130050"  OR
                                             "101130060"  OR
                                             "101130070"  OR
                                             "101130080"  OR
                                             "101130090"  OR
                                             "101130100"
                           COMPUTE WRK-RSISDO = WRK-RSISDO +
                                  (WRK-TEN  *   SINRYO-COUNT)
                      END-IF
                      IF   SRY-SRYCD(IDZ) =  "101910010"  OR
                                             "101910020"  OR
                                             "101910030"  OR
                                             "101910040"  OR
                                             "101910070"  OR
                                             "101910080"  OR
                                             "101910090"  OR
                                             "101910100"
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                      END-IF
      *
                      IF     SYU-SYUHKNNUM   =   "971"
      *
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09593"
                                                    OR    "09594"
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
      *
                      END-IF
      *
                      IF     SYU-SYUHKNNUM   =   "973"
      *
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09591"
                                                    OR    "09592"
                                                    OR    "09593"
                                                    OR    "09594"
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09591"
                           COMPUTE WRK-SINDAN = WRK-SINDAN +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09592"
                           COMPUTE WRK-MEISAI = WRK-MEISAI +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
      *
                         IF    SRY-SRYCD (IDZ)  =   "140037490"
                           COMPUTE WRK-YOUBUKASAN = WRK-YOUBUKASAN +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
      *
                         IF    SRY-SRYCD (IDZ)  =   "199000510"
                           COMPUTE WRK-RYOYOTEATE = WRK-RYOYOTEATE +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
      *
                      END-IF
      *
                    END-IF
      *
                  END-PERFORM
      *
                  MOVE    "SRYACT-KEY2"    TO   ORC-DBPATH
                  PERFORM  SRYACT-READ-SEC
                END-PERFORM
             END-IF
      *
             MOVE     "SRYACT-KEY2"      TO  WK-PATH
             PERFORM   CSR-CLOSE-SEC
      *
           END-IF
      *
           .
       ROSAI-JIBAI-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    労災・自賠責用の転送処理
      **************************************************************
       ROSAI-JIBAI-TENSO-SEC          SECTION.
      *
           IF      WRK-RSISHOSHIN     NOT =    ZERO
                   COMPUTE SYU-HKNTEN (01)  =  SYU-HKNTEN (01)
                                            -  WRK-RSISHOSHIN
                   COMPUTE SYU-RSISHOSHIN-MONEY  =  WRK-RSISHOSHIN
                                                 *  10
           END-IF
           IF      WRK-RSISDO         NOT =    ZERO
                   COMPUTE SYU-HKNTEN (02)  =  SYU-HKNTEN (02)
                                            -  WRK-RSISDO
                   COMPUTE SYU-RSISDO-MONEY      =  WRK-RSISDO
                                                 *  10
           END-IF
           IF      WRK-RSIETC         NOT =    ZERO
                   COMPUTE SYU-HKNTEN (10)  =  SYU-HKNTEN (10)
                                            -  WRK-RSIETC
      ****         COMPUTE SYU-RSIETC-MONEY      =  WRK-RSIETC
      ****                                       *  10
           END-IF
           MOVE    WRK-RSIETC2    TO   SYU-RSIETC-MONEY
      *
           IF  SYU-SYUHKNNUM       =   "973"
      *
             COMPUTE   WRK-JIBAI-SYUTEN(IDX)
                                        =    WRK-SYUTEN
                                        -    WRK-RSISHOSHIN
                                        -    WRK-RSISDO
                                        -    WRK-RSIETC
                                        -    WRK-YOUBUKASAN
                                        -    WRK-RYOYOTEATE
      *
             ADD    SYU-HKNTEN(11)     TO    WRK-JIBAI-SYUTEN(IDX)
      *
             MOVE      WRK-SINDAN      TO    WRK-JIBAI-SINDAN(IDX)
             MOVE      WRK-MEISAI      TO    WRK-JIBAI-MEISAI(IDX)
      *
             MOVE      WRK-RYOYOTEATE  TO    WRK-JIBAI-RYOYO(IDX)
      *
           END-IF
      *
           .
       ROSAI-JIBAI-TENSO-EXT.
           EXIT.
      *
      **************************************************************
      *    保険適用外のテーブル領域セット
      **************************************************************
       HKNTEKGAI-TBL-SEC          SECTION.
      *
           EVALUATE    SRY-SRYKBN
               WHEN    "95"
                   PERFORM     VARYING    IDZ    FROM    1     BY    1
                                 UNTIL   (IDZ         >   5    )   OR
                                         (SRY-SRYCD(IDZ)  =   SPACE)
                       IF    SRY-SRYCD (IDZ) (1:3)   =   "095"
                                                     OR  "096"
                           IF    SRY-SRYCD (IDZ) (4:2)    =   "00"
                                                     OR       "91"     
                                                     OR       "92"     
                                                     OR       "93"     
                                                     OR       "94"     
                               MOVE    SPACE         TO   TNS-TENSU-REC
                               INITIALIZE                 TNS-TENSU-REC
                               MOVE    SRY-SRYCD(IDZ) TO   TNS-SRYCD
                               MOVE    SYU-SRYYMD(1:6)
                                                     TO  ORC-DBYMD(1:6)
                               MOVE    "01"          TO  ORC-DBYMD(7:2)
                               PERFORM    TENSU-READ-SEC
                               IF      MCP-RC         =    ZERO
                                   EVALUATE    TNS-NYUTENTTLKBN
                                       WHEN    1
                                          COMPUTE  TBL-JIHI-TAXNASI(1)
                                               =   TBL-JIHI-TAXNASI(1)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    2
                                          COMPUTE  TBL-JIHI-TAXNASI(2)
                                               =   TBL-JIHI-TAXNASI(2)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    3
                                          COMPUTE  TBL-JIHI-TAXNASI(3)
                                               =   TBL-JIHI-TAXNASI(3)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    4
                                          COMPUTE  TBL-JIHI-TAXNASI(4)
                                               =   TBL-JIHI-TAXNASI(4)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    5
                                          COMPUTE  TBL-JIHI-TAXNASI(5)
                                               =   TBL-JIHI-TAXNASI(5)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                   END-EVALUATE
                               END-IF
                           ELSE
                               EVALUATE   SRY-SRYCD (IDZ) (4:2)
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY(1)  =
                                                TBL-TGMONEY(1)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY(2)  =
                                                TBL-TGMONEY(2)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY(3)  =
                                                TBL-TGMONEY(3)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY(4)  =
                                                TBL-TGMONEY(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY(5)  =
                                                TBL-TGMONEY(5)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY(6)  =
                                                TBL-TGMONEY(6)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "50"
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY(7)  =
                                                TBL-TGMONEY(7)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY(8)  =
                                                TBL-TGMONEY(8)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY(9)  =
                                                TBL-TGMONEY(9)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "80"
                                       COMPUTE  TBL-TGMONEY(10)  =
                                                TBL-TGMONEY(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "90"
                                       COMPUTE  TBL-TGMONEY(11)  =
                                                TBL-TGMONEY(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                               END-EVALUATE
                           END-IF
                       ELSE
                           MOVE    SPACE          TO   TNS-TENSU-REC
                           INITIALIZE                  TNS-TENSU-REC
                           MOVE    SRY-SRYCD(IDZ) TO   TNS-SRYCD
                           MOVE    SYU-SRYYMD(1:6)
                                                  TO   ORC-DBYMD(1:6)
                           MOVE    "01"           TO   ORC-DBYMD(7:2)
                           PERFORM    TENSU-READ-SEC
                           IF      MCP-RC         =    ZERO
                               EVALUATE   TNS-SRYKBN
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY(1)  =
                                                TBL-TGMONEY(1)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY(2)  =
                                                TBL-TGMONEY(2)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY(3)  =
                                                TBL-TGMONEY(3)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY(4)  =
                                                TBL-TGMONEY(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY(5)  =
                                                TBL-TGMONEY(5)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY(6)  =
                                                TBL-TGMONEY(6)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "50"
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY(7)  =
                                                TBL-TGMONEY(7)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY(8)  =
                                                TBL-TGMONEY(8)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY(9)  =
                                                TBL-TGMONEY(9)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "80"
                                       COMPUTE  TBL-TGMONEY(10)  =
                                                TBL-TGMONEY(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "90"
                                       COMPUTE  TBL-TGMONEY(11)  =
                                                TBL-TGMONEY(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   OTHER
                                     IF   TNS-SRYCD(1:1)    =   "6"
                                       IF    TNS-YKZKBN     =    4
                                         COMPUTE  TBL-TGMONEY(5)  =
                                                  TBL-TGMONEY(5)  +
                                                 (SRY-JIHIMONEY(IDZ) *
                                                  SINRYO-COUNT)
                                       ELSE
                                         COMPUTE  TBL-TGMONEY(4)  =
                                                  TBL-TGMONEY(4)  +
                                                 (SRY-JIHIMONEY(IDZ) *
                                                  SINRYO-COUNT)
                                       END-IF
                                     END-IF
                                     IF   TNS-SRYCD(1:1)    =   "7"
                                       COMPUTE  TBL-TGMONEY(10)  =
                                                TBL-TGMONEY(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-PERFORM
               WHEN    "96"
                   PERFORM     VARYING    IDZ    FROM   1    BY   1
                                 UNTIL   (IDZ        >   5    )   OR
                                         (SRY-SRYCD(IDZ)  =   SPACE)
                       IF    SRY-SRYCD (IDZ) (1:3)   =   "095"
                                                     OR  "096"
                           IF    SRY-SRYCD (IDZ) (4:2)    =   "00"
                                                     OR       "91"      
                                                     OR       "92"      
                                                     OR       "93"      
                                                     OR       "94"      
                               MOVE    SPACE         TO   TNS-TENSU-REC
                               INITIALIZE                 TNS-TENSU-REC
                               MOVE    SRY-SRYCD(IDZ) TO   TNS-SRYCD
                               MOVE    SYU-SRYYMD(1:6)
                                                     TO  ORC-DBYMD(1:6)
                               MOVE    "01"          TO  ORC-DBYMD(7:2)
                               PERFORM    TENSU-READ-SEC
                               IF      MCP-RC         =    ZERO
                                   EVALUATE    TNS-NYUTENTTLKBN
                                       WHEN    1
                                          COMPUTE  TBL-JIHI-TAXARI(1)
                                               =   TBL-JIHI-TAXARI(1)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    2
                                          COMPUTE  TBL-JIHI-TAXARI(2)
                                               =   TBL-JIHI-TAXARI(2)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    3
                                          COMPUTE  TBL-JIHI-TAXARI(3)
                                               =   TBL-JIHI-TAXARI(3)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    4
                                          COMPUTE  TBL-JIHI-TAXARI(4)
                                               =   TBL-JIHI-TAXARI(4)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    5
                                          COMPUTE  TBL-JIHI-TAXARI(5)
                                               =   TBL-JIHI-TAXARI(5)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                   END-EVALUATE
                               END-IF
                           ELSE
                               EVALUATE   SRY-SRYCD (IDZ) (4:2)
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY-TAX(1)  =
                                                TBL-TGMONEY-TAX(1)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY-TAX(2)  =
                                                TBL-TGMONEY-TAX(2)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY-TAX(3)  =
                                                TBL-TGMONEY-TAX(3)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY-TAX(4)  =
                                                TBL-TGMONEY-TAX(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY-TAX(5)  =
                                                TBL-TGMONEY-TAX(5)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY-TAX(6)  =
                                                TBL-TGMONEY-TAX(6)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "50"
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY-TAX(7)  =
                                                TBL-TGMONEY-TAX(7)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY-TAX(8)  =
                                                TBL-TGMONEY-TAX(8)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY-TAX(9)  =
                                                TBL-TGMONEY-TAX(9)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "80"
                                       COMPUTE  TBL-TGMONEY-TAX(10)  =
                                                TBL-TGMONEY-TAX(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "90"
                                       COMPUTE  TBL-TGMONEY-TAX(11)  =
                                                TBL-TGMONEY-TAX(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                               END-EVALUATE
                           END-IF
                       ELSE
                           MOVE    SPACE          TO   TNS-TENSU-REC
                           INITIALIZE                  TNS-TENSU-REC
                           MOVE    SRY-SRYCD(IDZ) TO   TNS-SRYCD
                           MOVE    SYU-SRYYMD(1:6)
                                                  TO   ORC-DBYMD(1:6)
                           MOVE    "01"           TO   ORC-DBYMD(7:2)
                           PERFORM    TENSU-READ-SEC
                           IF      MCP-RC         =    ZERO
                               EVALUATE   TNS-SRYKBN
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY-TAX(1)  =
                                                TBL-TGMONEY-TAX(1)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY-TAX(2)  =
                                                TBL-TGMONEY-TAX(2)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY-TAX(3)  =
                                                TBL-TGMONEY-TAX(3)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY-TAX(4)  =
                                                TBL-TGMONEY-TAX(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY-TAX(5)  =
                                                TBL-TGMONEY-TAX(5)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY-TAX(6)  =
                                                TBL-TGMONEY-TAX(6)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "50"
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY-TAX(7)  =
                                                TBL-TGMONEY-TAX(7)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY-TAX(8)  =
                                                TBL-TGMONEY-TAX(8)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY-TAX(9)  =
                                                TBL-TGMONEY-TAX(9)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "80"
                                       COMPUTE  TBL-TGMONEY-TAX(10)  =
                                                TBL-TGMONEY-TAX(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "90"
                                       COMPUTE  TBL-TGMONEY-TAX(11)  =
                                                TBL-TGMONEY-TAX(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   OTHER
                                     IF   TNS-SRYCD(1:1)    =   "6"
                                       IF    TNS-YKZKBN     =    4
                                         COMPUTE  TBL-TGMONEY-TAX(5) =
                                                  TBL-TGMONEY-TAX(5) +
                                                 (SRY-JIHIMONEY(IDZ) *
                                                  SINRYO-COUNT)
                                       ELSE
                                         COMPUTE  TBL-TGMONEY-TAX(4) =
                                                  TBL-TGMONEY-TAX(4) +
                                                 (SRY-JIHIMONEY(IDZ) *
                                                  SINRYO-COUNT)
                                       END-IF
                                     END-IF
                                     IF   TNS-SRYCD(1:1)    =   "7"
                                       COMPUTE  TBL-TGMONEY-TAX(10)  =
                                                TBL-TGMONEY-TAX(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-PERFORM
           END-EVALUATE
           .
       HKNTEKGAI-TBL-EXT.
           EXIT.
      *
      **************************************************************
      *    保険適用外金額の集計テーブルから収納テーブルへのセット
      **************************************************************
       HKNTEKGAI-SET-SEC          SECTION.
      *
      *  保険適用外
           PERFORM     VARYING   IDZ    FROM   1    BY    1
                         UNTIL   IDZ    >      11
                  ADD    TBL-TGMONEY(IDZ)      TO  SYU-TGMONEY(IDZ)
                  ADD    TBL-TGMONEY-TAX(IDZ)  TO  SYU-TGMONEY-TAX(IDZ)
                  ADD    TBL-TGMONEY(IDZ)      TO  SYU-TGMONEY(12)
                  ADD    TBL-TGMONEY-TAX(IDZ)  TO  SYU-TGMONEY-TAX(12)
           END-PERFORM
      *
      *  入院保険適用外
      *    ADD    NYUIN-TGMONEY                TO  SYU-TGMONEY(11)
      *    ADD    NYUIN-TGMONEY-TAX            TO  SYU-TGMONEY-TAX(11)
      *    ADD    NYUIN-TGMONEY                TO  SYU-TGMONEY(12)
      *    ADD    NYUIN-TGMONEY-TAX            TO  SYU-TGMONEY-TAX(12)
      *
      *  自費１から５
           PERFORM     VARYING   IDZ    FROM    1    BY    1
                         UNTIL   IDZ    >       5
                  MOVE TBL-JIHI-TAXNASI(IDZ) TO SYU-JIHI-TAXNASI(IDZ)
                  MOVE TBL-JIHI-TAXARI(IDZ)  TO SYU-JIHI-TAXARI(IDZ)
                  ADD  TBL-JIHI-TAXNASI(IDZ) TO SYU-JIHI-TOTAL
                  ADD  TBL-JIHI-TAXARI(IDZ)  TO SYU-JIHI-TOTAL-TAX
           END-PERFORM
           .
       HKNTEKGAI-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    点滴薬剤点数まとめ用に診療行為テーブルの読み込み            *
      ******************************************************************
       TENTEK-GET-SEC                 SECTION.
      *
      *診療行為情報の取得
           MOVE    ZERO               TO   FLG-SRYACT
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID        TO   SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
           MOVE    ACCT-PTID          TO   SRY-PTID
           MOVE    ACCT-SRYKA         TO   SRY-SRYKA
           MOVE    ACCT-SRYYM         TO   SRY-SRYYM
           MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
           MOVE    SRYACT-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "SRYACT-KEY2"      TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
          IF      MCP-RC             =    ZERO
                MOVE    "SRYACT-KEY2"    TO   ORC-DBPATH
                PERFORM  SRYACT-READ-SEC
          ELSE
                MOVE    1                TO   FLG-SRYACT
          END-IF
      *
           .
      *
       TENTEK-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    点滴薬剤（器材）点数まとめ処理                              *
      ******************************************************************
       TENTEK-MATOME-SEC                 SECTION.
      *
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    SYU-PTID            TO  ORCSTEN-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  ORCSTEN-SRYYM
           MOVE    SYU-HKNCOMBINUM     TO  ORCSTEN-HKNCOMBI
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
      *    薬剤点数と器材点数
           PERFORM  VARYING  IDX1    FROM    1   BY  1
                      UNTIL  IDX1     >       31
      *
               IF  SYU-DAY(IDX1)   =   "1"  OR  "2"
                    IF  ORCSTEN-ZAITEN(IDX1)  NOT =  ZERO
                        ADD    ORCSTEN-ZAITEN(IDX1)
                                      TO   SYU-HKNTEN (05)
                        IF   SYU-SYUHKNNUM  =  "971"  OR  "973"        
                           CONTINUE                                    
                        ELSE                                           
                           ADD    ORCSTEN-ZAITEN(IDX1)                 
                                      TO   SDAY-HKNTEN(IDX1)           
                        END-IF                                         
                    END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM  VARYING  IDX1    FROM    1   BY  1
                      UNTIL  IDX1     >       50
                       OR    ORCSTEN-KIZ-ZAITEN(IDX1)  =  ZERO
      *
                    MOVE    ZERO   TO   WRK-ZAIKAISU
                    PERFORM  VARYING  IDX2    FROM    1   BY  1
                          UNTIL   IDX2     >       31
                      IF  SYU-DAY(IDX2)   =   "1"  OR  "2"
      *
                        IF   SYU-SYUHKNNUM  =  "971"  OR  "973"        
                           CONTINUE                                    
                        ELSE                                           
                           COMPUTE  SDAY-HKNTEN(IDX2)  =               
                                    SDAY-HKNTEN(IDX2)  +               
                                    ( ORCSTEN-KIZ-ZAITEN(IDX1) *       
                                      ORCSTEN-KIZ-DAY(IDX1 IDX2) )     
                        END-IF
      *
                        ADD    ORCSTEN-KIZ-DAY(IDX1 IDX2)
                                      TO   WRK-ZAIKAISU
                      END-IF
                    END-PERFORM
      *
                    COMPUTE  SYU-HKNTEN (05)  =
                             SYU-HKNTEN (05)  +
                             ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                               WRK-ZAIKAISU ) 
      *
           END-PERFORM
      *
           .
      *
       TENTEK-MATOME-EXT.
           EXIT.
      *
      ******************************************************************
      *    月内に複数の医療期間情報がないか検索（医療機関減算の確認）  *
      ******************************************************************
       SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYU-SRYYMD(1:6)   TO  ORC-DBYMD(1:6)
           MOVE    "__"                TO  ORC-DBYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY11"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY11"  TO  ORC-DBPATH
               PERFORM  SYSKANRI-READ-SEC
               MOVE     ZERO     TO        WRK-SYS5000-COUNT
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "SYSKANRI-KEY11"  TO  ORC-DBPATH
                   PERFORM  SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE     "SYSKANRI-KEY11"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
      *
       SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    月内の医療期間情報取得（医療機関減算情報）                  *
      ******************************************************************
       SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      TABLE-GENSAN
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYU-SRYYMD        TO  ORC-DBYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
             MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
             PERFORM  SYSKANRI-READ-SEC
             PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                      UNTIL     GENSAN-IDX   >   4
                  PERFORM   VARYING   SYU-IDX   FROM  1  BY  1
                            UNTIL     SYU-IDX   >  31
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              MOVE   5   TO  TBL-INNAI-KANSEN(SYU-IDX)
                            WHEN  3
                              MOVE   10  TO  TBL-IRYOU-ANZEN(SYU-IDX)
                            WHEN  4
                              MOVE   5   TO  TBL-JOKUSOU(SYU-IDX)
                        END-EVALUATE
                    END-IF
                  END-PERFORM
             END-PERFORM
           END-IF
      *
           MOVE     "SYSKANRI-KEY"   TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYU-SRYYMD(1:6)   TO  ORC-DBYMD(1:6)
           MOVE    "__"                TO  ORC-DBYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY11"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY11"  TO  ORC-DBPATH
               PERFORM  SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   MOVE   SYS-5000-STYUKYMD(7:2)  TO  KIRIKAE-IDXX
                   PERFORM    VARYING  SYU-IDX  FROM KIRIKAE-IDX BY 1
                              UNTIL    SYU-IDX  >   31
                       IF  SYS-5000-GENKBN(2)   =   "2"
                           MOVE   5      TO  TBL-INNAI-KANSEN
                                                       (SYU-IDX)
                       ELSE
                           MOVE   ZERO   TO  TBL-INNAI-KANSEN
                                                       (SYU-IDX)
                       END-IF
                       IF  SYS-5000-GENKBN(3)   =   "2"
                           MOVE   10     TO  TBL-IRYOU-ANZEN
                                                       (SYU-IDX)
                       ELSE
                           MOVE   ZERO   TO  TBL-IRYOU-ANZEN
                                                       (SYU-IDX)
                       END-IF
                       IF  SYS-5000-GENKBN(4)   =   "2"
                           MOVE   5      TO  TBL-JOKUSOU
                                                       (SYU-IDX)
                       ELSE
                           MOVE   ZERO   TO  TBL-JOKUSOU
                                                       (SYU-IDX)
                       END-IF
                   END-PERFORM
                   MOVE    "SYSKANRI-KEY11"  TO  ORC-DBPATH
                   PERFORM  SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
             MOVE     "SYSKANRI-KEY11"      TO  WK-PATH
             PERFORM   CSR-CLOSE-SEC
      *
           .
      *
       SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    入院診療行為マスター読込
      **************************************************************
       NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       NYUINACT-ERAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    選定療養マスター読込
      *****************************************************************
       SENTEICDCHG-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
               MOVE    ZERO            TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
               MOVE    1               TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       SENTEICDCHG-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    システム管理マスタ読み込み処理
      **************************************************************
       SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-5001-REC
                                           SYS-5113-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-5001-REC
                                           SYS-5113-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       TENSU-READ-SEC         SECTION.
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
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込
      *****************************************************************
       PTNYUINRRK-READ-SEC       SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    MCPDATA-REC      TO  PTNYUINRRK-REC
               MOVE    ZERO             TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                   PTNYUINRRK-REC
               MOVE    1                TO  FLG-PTNYUINRRK
           END-IF
           .
       PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
      *    INITIALIZE                  MCPAREA
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WK-PATH             TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       CSR-CLOSE-EXT.
           EXIT.
