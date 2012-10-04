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
       PROGRAM-ID.             ORCBC010.
      *****************************************************************
      *  ★修正時には必ずプログラム識別番号をリセットすること
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データ収集
      *  コンポーネント名  : データ収集用基本情報作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  06/08/17    NACL−伊藤    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.03.01    NACL-藤原    06/10/27  レセプトで生活保護の請求がな
      *                                     い場合記載の対象としない
      *  04.01.01    NACL-小豆沢  07/11/16  公務災害、公害の種別判定追加
      *  04.02.01    NACL-伊藤    08/ 4/22  改定対応
      *  04.02.02    NACL-門脇    08/08/07  精神通院の請求点判定追加
      *
      *  04.03.01    NACL-門脇    08/09/02  請求点判定追加
      *  04.03.02    NACL-伊藤    08/10/31  公費単独の室料差額
      *                                     本人・家族区分の見直し
      *                                     ０件の場合正常終了とする
      *  04.03.03    NACL-門脇    08/12/17  平成２１年１月改正対応
      *
      *  04.04.01    NACL-藤原    09/02/26  平成２０年４月以降に７５歳到
      *                                     達時に海外に住所を有する場合
      *                                     の対応
      *  04.04.02    NACL-門脇    09/04/13  平成２１年５月改正対応
      *  04.04.03    NACL-門脇    10/03/23  改正対応（外泊区分５の対応）
      *  04.04.04    NACL-藤原    10/08/23  後期高齢非該当対応
      *                                     （高齢受給者票がない場合） 
      *
      *  04.05.01    NACL-伊藤    10/01/28  小数点５桁対応COPY句のみ
      *              NACL-小豆沢  10/02/16  治験対応
      *  04.05.02    NACL-藤原    10/03/12  平成２２年４月改正対応
      *                                     平成２２年４月分以降の政府管
      *                                     掌をエラーとする
      *  04.05.03    NACL-小豆沢  10/05/27  剤番号の重複に対処
      *                                     入院料の剤番号は最大剤番号＋１
      *                                     から振り出す
      *  04.05.04    NACL-太田    10/06/04  外泊の剤に入院料を編集する
      *                                     選定入院中の外泊剤を読み飛ばす
      *  04.05.05    NACL-藤原    10/09/14  医療観察法対応
      *                                     公費単独のレセプトとする
      *  04.05.06    NACL-門脇    12/04/02  平成２４年４月改正
      *                                     実日数カウント対応
      *                                     外泊区分６．７の対応
      *  04.05.07    NACL-門脇    12/09/18  改正対応（外泊区分８，９の対応）
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    収集データ（病院基本情報）
           SELECT  COLDAT91-FILE   ASSIGN  COLDAT91
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT91-KEY
                                   FILE    STATUS  IS  STS-COLDAT91.
      *
           SELECT  COLDAT911-FILE  ASSIGN  COLDAT91
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT911-KEY
                                   FILE    STATUS  IS  STS-COLDAT91.
      *
      *    収集データ（患者基本情報）
           SELECT  COLDAT01-FILE   ASSIGN  COLDAT01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT01-KEY
                                   FILE    STATUS  IS  STS-COLDAT01.
      *
           SELECT  COLDAT011-FILE  ASSIGN  COLDAT01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT011-KEY
                                   FILE    STATUS  IS  STS-COLDAT01.
      *
      *    収集データ（診療情報）
           SELECT  COLDAT02-FILE   ASSIGN  COLDAT02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT02-KEY
                                   FILE    STATUS  IS  STS-COLDAT02.
      *
           SELECT  COLDAT021-FILE  ASSIGN  COLDAT02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT021-KEY
                                   FILE    STATUS  IS  STS-COLDAT02.
      *
      *    収集データ（病名情報）
           SELECT  COLDAT03-FILE   ASSIGN  COLDAT03
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT03-KEY
                                   FILE    STATUS  IS  STS-COLDAT03.
      *
           SELECT  COLDAT031-FILE  ASSIGN  COLDAT03
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT031-KEY
                                   FILE    STATUS  IS  STS-COLDAT03.
      *
      *    エラーファイル
           SELECT  COLDATERR-FILE  ASSIGN  WRK-PARA-ERRFILENM
                                   FILE    STATUS  IS  STS-COLDATERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    病院基本情報データ
       FD  COLDAT91-FILE.
       01  COLDAT91-REC.
           COPY    "CPCDF901.INC".
      *
       FD  COLDAT911-FILE.
       01  COLDAT911-REC.
           COPY    "CPCDF901.INC"  REPLACING  //COLDAT91//
                                   BY         //COLDAT911//.
      *
      *    患者基本情報データ
       FD  COLDAT01-FILE.
       01  COLDAT01-REC.
           COPY    "CPCDF001.INC".
      *
       FD  COLDAT011-FILE.
       01  COLDAT011-REC.
           COPY    "CPCDF001.INC"  REPLACING  //COLDAT01//
                                   BY         //COLDAT011//.
      *
      *    診療情報データ
       FD  COLDAT02-FILE.
       01  COLDAT02-REC.
           COPY    "CPCDF002.INC".
      *
       FD  COLDAT021-FILE.
       01  COLDAT021-REC.
           COPY    "CPCDF002.INC"  REPLACING  //COLDAT02//
                                   BY         //COLDAT021//.
      *
      *    病名情報データ
       FD  COLDAT03-FILE.
       01  COLDAT03-REC.
           COPY    "CPCDF003.INC".
      *
       FD  COLDAT031-FILE.
       01  COLDAT031-REC.
           COPY    "CPCDF003.INC"  REPLACING  //COLDAT03//
                                   BY         //COLDAT031//.
      *    エラーファイル
       FD  COLDATERR-FILE.
       01  COLDATERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *01  PRGSKBNO                PIC X(08)  VALUE "20071029".
      *01  PRGSKBNO                PIC X(08)  VALUE "20080428".
      *01  PRGSKBNO                PIC X(08)  VALUE "20080826".
      *01  PRGSKBNO                PIC X(08)  VALUE "20081031".
      *01  PRGSKBNO                PIC X(08)  VALUE "20100128".
      *01  PRGSKBNO                PIC X(08)  VALUE "20100527".
       01  PRGSKBNO                PIC X(08)  VALUE "20100604".
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT91//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT01//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT02//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT03//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COLDAT91                    PIC X(02).
           03  STS-COLDAT01                    PIC X(02).
           03  STS-COLDAT02                    PIC X(02).
           03  STS-COLDAT03                    PIC X(02).
           03  STS-COLDATERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNCOMBI                    PIC 9(01).
           03  FLG-HKNNUM                      PIC 9(01).
           03  FLG-COLDAT01                    PIC 9(01).
           03  FLG-PTHKNINF                    PIC 9(01).
           03  FLG-PTKOHINF                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-PTCOM                       PIC 9(01).
           03  FLG-PTRSIINF                    PIC 9(01).
           03  FLG-ADRS                        PIC 9(01).
           03  FLG-PTBYOMEI                    PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-SRYACCT                     PIC 9(01).
           03  FLG-PTNYUINRRK                  PIC 9(01).
      *
           03  FLG-KOH                         PIC 9(01).
           03  FLG-CONTKBN                     PIC 9(01).
           03  FLG-SET                         PIC 9(01).
           03  FLG-LASTHKNID                   PIC 9(01).
           03  FLG-SHOSAISIN                   PIC 9(01).
           03  FLG-SHOSAISIN-GEN               PIC 9(01).
           03  FLG-CHK                         PIC 9(01).
           03  FLG-EDIT                        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-COLDAT91                    PIC 9(06).
           03  CNT-COLDAT01                    PIC 9(06).
           03  CNT-COLDAT02                    PIC 9(06).
           03  CNT-COLDAT03                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03).
           03  IDX1                            PIC 9(03).
           03  IDX2                            PIC 9(03).
           03  IDX3                            PIC 9(03).
           03  IDX4                            PIC 9(03).
           03  IDX5                            PIC 9(03).
           03  IDX6                            PIC 9(03).
           03  IDY                             PIC 9(03).
           03  IDZ                             PIC 9(03).
           03  IDZ1                            PIC 9(03).
           03  IDZ2                            PIC 9(03).
           03  IDZ3                            PIC 9(03).
           03  IDXZ                            PIC 9(03).
           03  IDXX                            PIC 9(03).
           03  JN-IDX                          PIC 9(03).
           03  HC-IDX                          PIC 9(03).
           03  KH-IDX                          PIC 9(03).
           03  ACCT-IDX                        PIC 9(02).
           03  IDX-DAY                         PIC 9(02).
      **** 03  SYSKAN-IDX                      PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
               05  KEY-N-HKNCOMBI              PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
               05  KEY-O-HKNCOMBI              PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(16).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-PREFNUM            PIC X(02).
               05  WRK-PARA-SRYYM              PIC X(06).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-ERRFILENM          PIC X(100).
      *
           03  WRK-HOSPID                  PIC X(24).
      *
           03  WRK-COLDATERR               PIC X(200).
      *
           03  WRK-PATH                    PIC X(64).
      *
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN              PIC X(01).
               05  WRK-TEISEIKBN           PIC X(01).
      *
           03  WRK-DATAKBN             PIC X(01).
      *
           03  WRK-CHGNUM-P.
               05  WRK-CHGNUM          PIC 9(01).
      *
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-ZAINUM              PIC 9(08).
      *
           03  WRK-PREFKBN             PIC X(01).
           03  WRK-PREFNUM             PIC X(02).
      *
      *    傷病名編集
           03  WRK-BYOMEI          PIC X(150).
           03  WRK-BYOMEIW         PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01).
      *    病名文字数         
           03  WRK-MOJISU          PIC 9(03).
      *
           03  WRK-YM              PIC 9(06).
      *
           03  WRK-MSG-NYUGAI      PIC X(04).
      *
      *    後期高齢者非該当フラグ
           03  WRK-KOUKI-FLG           PIC 9(01).
      *
           03  WRK-TEKSTYMD.
               05  WRK-TEKSTYYMM       PIC 9(06).
               05  WRK-TEKSTDD         PIC 9(02).
      *    レセプト種別判定用領域
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                               PIC X(01).
           03  WK-IHO                                  PIC X(01).
           03  WK-RJN                                  PIC X(01).
           03  WK-KOHI-CNT                             PIC 9(02).
           03  WK-HONKZKKBN                            PIC X(01).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08).
      *
      *    診療回数テーブル（入院会計用  １：基本  ２：組合せ
      *                                  ３：食事  ４：外泊）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  4.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *
      *    ワーク保険組合せ（入院会計用）
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10.
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).
      *
       01  WRK-COLDAT01-TABLE.
           02  WRK-COLDAT01-OCC    OCCURS  100.
           COPY    "CPCDF001.INC"  REPLACING  //COLDAT01//
                                   BY         //WRK-COLDAT01//.
       01  WRK-COLDAT01-TABLE-MAX      PIC 9(03)   VALUE    100.
      *
      *    ワーク患者病名
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO//
                                   BY         //WRK-PTBYO//.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"
                   REPLACING   //SPA-//    BY //WRK-SPA-//.
      *
           COPY    "MCPAREA".
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *    保険番号マスタ退避ワーク
       01  WRK-HKNNUM-AREA.
           03  WRK-HKNNUM-IDX                  PIC 9(05).
           03  WRK-HKNNUM-MAX                  PIC 9(05).
           03  WRK-CHK-HKNNUM                  PIC X(03).
           03  WRK-CHK-PAYKBN                  PIC X(02).
           03  WRK-CHK-TEKSTYMD                PIC X(08).
           03  WRK-CHK-TEKEDYMD                PIC X(08).
       01  TABLE-HKNNUM-AREA.
         02  TBL-HKNNUM-REC                    OCCURS  2000.
           COPY    "CPHKNNUM.INC"  REPLACING   //HKN-//
                                   BY          //TBL-HKN-//.
       01  TBL-HKNNUM-CONST-MAX                PIC 9(05)
                                               VALUE   2000.
      *
      *    実日数カウント用領域
       01  TBL-JNISSU.
           03  TBL-JNISSU-HKN                          OCCURS   5.
               05  TBL-JNISSU-DAY                      PIC X(01)
                                                       OCCURS   31.
      *    実日数カウント用退避領域
       01  TBL-JNISSU-S.
           03  TBL-JNISSU-HKN-S                        OCCURS   5.
               05  TBL-JNISSU-DAY-S                    PIC X(01)
                                                       OCCURS   31.
      *    実日数減カウント用領域
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-HKN-GEN                      OCCURS   5.
               05  TBL-JNISSU-DAY-GEN                  PIC X(01)
                                                       OCCURS   31.
      *    合計点数領域
       01  TBL-TOTALTENSU-INF.
           03  TBL-TOTALTENSU                          PIC 9(08)
                                                       OCCURS   5.
      *    治験減点用点数領域
       01  WRK-TOTALTENSU-INF.
           03  WRK-TOTALTENSU                          PIC 9(08)
                                                       OCCURS   5.
      *    食事療養回数カウント用領域
       01  TBL-SKJNISSU-INF.
           03  TBL-SKJNISSU-OCC                        OCCURS   5.
               05  TBL-SKJNISSU                        PIC 9(02)
                                                       OCCURS   31.
      *    食事療養費領域
       01  TBL-SKJRYOYOHI-INF.
           03  TBL-SKJRYOYOHI                          PIC 9(08)
                                                       OCCURS   5.
      *    食事療養費標準負担額領域
       01  TBL-SKJFTN-INF.
           03  TBL-SKJFTN                              PIC 9(08)
                                                       OCCURS   5.
      *    レセプト種別返却サブ
           COPY    "CPORCSSYUBETUGET.INC".
      *
       01  TBL-NYUINKA-G.
           03  TBL-NYUINKA                 PIC X(02)   OCCURS  31.
           03  TBL-STIDX                   PIC 9(02).
           03  TBL-EDIDX                   PIC 9(02).
      *
      *    標準負担額テーブル
           03  SKJFTN-DAY-AREA.
               05  SKJFTN-MAX              PIC  9(02).
               05  SKJFTN-KAISUINF         OCCURS  31.
                   07  SKJFTN-SRYKA        PIC  X(02).
                   07  SKJFTN-KAISU        PIC  9(02).
      *
       01  COLDATWRK-REC.
           COPY    "CPCDF002.INC"  REPLACING  //COLDAT02//
                                   BY         //COLDATWRK//.
      *
       01  WRK-NYUTAIIN.
           03  WRK-NYUINYMD                PIC  X(08).
           03  WRK-TAIINYMD                PIC  X(08).
      *
       01  WRK-CONS-AREA.
      *
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012".
      *    中国残留邦人等保険番号 
           03  WRK-CONS-ZANRYUU-HKNNUM PIC X(03)   VALUE   "025". 
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980".
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    原爆保険番号 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM PIC X(03)    VALUE   "975".
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".
      *
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
           03  WRK-CONS-SRYYM-201004   PIC 9(06)   VALUE   201004.
      *
      *    標準負担額
           03  WRK-CONS-HYOJYUN-FUTAN-G.
      *        標準負担額（一般）
               05  WRK-CONS-HYOJYUN-FUTAN1
                                      PIC X(09)   VALUE   "197000810".
      *        標準負担額（低所得１）
               05  WRK-CONS-HYOJYUN-FUTAN2
                                      PIC X(09)   VALUE   "197001110".
      *        標準負担額（低所得２・９０日以下）
               05  WRK-CONS-HYOJYUN-FUTAN3
                                      PIC X(09)   VALUE   "197000910".
      *        標準負担額（低所得２・９０日超）
               05  WRK-CONS-HYOJYUN-FUTAN4
                                      PIC X(09)   VALUE   "197001010".
      *        食事療養標準負担額（労災入院中）
               05  WRK-CONS-HYOJYUN-FUTAN5
                                      PIC X(09)   VALUE   "197001210".
      *        生活療養環境療養標準負担額
               05  WRK-CONS-HYOJYUN-FUTAN6
                                      PIC X(09)   VALUE   "197001910".
      *        生活療養（１）食事療養標準負担額（一般）
               05  WRK-CONS-HYOJYUN-FUTAN7
                                      PIC X(09)   VALUE   "197002010".
      *        生活療養（２）食事療養標準負担額（一般）
               05  WRK-CONS-HYOJYUN-FUTAN8
                                      PIC X(09)   VALUE   "197002110".
      *        生活療養食事療養標準負担額（低２）
               05  WRK-CONS-HYOJYUN-FUTAN9
                                      PIC X(09)   VALUE   "197002210".
      *        生活療養食事療養標準負担額（低１）
               05  WRK-CONS-HYOJYUN-FUTAN10
                                      PIC X(09)   VALUE   "197002310".
      *        生活療養食事療養標準負担額（老人低１（老福以外））
               05  WRK-CONS-HYOJYUN-FUTAN11
                                      PIC X(09)   VALUE   "197002410".
      *        生活療養食事療養標準負担額（老人低１（老福））
               05  WRK-CONS-HYOJYUN-FUTAN12
                                      PIC X(09)   VALUE   "197002510".
      *        生活療養食事療養標準負担額（医療区分２・３）（一般）
               05  WRK-CONS-HYOJYUN-FUTAN13
                                      PIC X(09)   VALUE   "197002710".
      *        生活療養食事療養標準負担額（医療区分２・３）（低２・９０日以下）
               05  WRK-CONS-HYOJYUN-FUTAN14
                                      PIC X(09)   VALUE   "197002810".
      *        生活療養食事療養標準負担額（医療区分２・３）（低２・９０日超）
               05  WRK-CONS-HYOJYUN-FUTAN15
                                      PIC X(09)   VALUE   "197002910".
      *        生活療養食事療養標準負担額（医療区分２・３）（低１）
               05  WRK-CONS-HYOJYUN-FUTAN16
                                      PIC X(09)   VALUE   "197003010".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    施設基準情報
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK2005.INC".
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
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *    公費付加情報取得サブ
           COPY    "CPORCSKOHPLUS.INC".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *    患者自己負担金レセプト記載判定
           COPY    "CPORCSFTNJGNCHK.INC".
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *    入院会計作成用パラメタ
           COPY    "CPORCSNYURECEDEN.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       WRK-COLDAT01-TABLE
                                       TTNS-AREA
                                       WRK-HKNNUM-AREA
                                       TABLE-HKNNUM-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-PREFNUM
                                               WRK-PARA-SRYYM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-ERRFILENM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBC010"      TO  JOB-PGID
           IF      WRK-PARA-NYUGAIKBN  =  "1"
               MOVE    "入院"          TO  WRK-MSG-NYUGAI
           ELSE
               MOVE    "外来"          TO  WRK-MSG-NYUGAI
           END-IF
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           STRING  LNK-DAY2-EDTYMD3(5:12)     DELIMITED BY SIZE
                   WRK-MSG-NYUGAI             DELIMITED BY SIZE
                   "分収集データ作成（抽出）" DELIMITED BY SIZE
                                 INTO  JOB-SHELLMSG
           END-STRING
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
      *
           MOVE    WRK-SYMD        TO  WRK-STYMD
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM  TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
           MOVE    WRK-SYMD            TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  WRK-HOSPID
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                           TO  WRK-COLDATERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1009-STYUKYMD
           MOVE    WRK-SYMD            TO  SYS-1009-EDYUKYMD
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-COLDATERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    レセプト編集区分情報
           INITIALIZE                  SYS-2005-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               MOVE    SPACE               TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-RJNGNBKKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
           END-IF
      *
      *    県内扱い保険者情報
           MOVE    SPACE               TO  WRK-1033-TABLE
      *
           MOVE    SPACE               TO  SYS-1033-REC
           INITIALIZE                      SYS-1033-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1033-HOSPNUM
           MOVE    "1033"              TO  SYS-1033-KANRICD
           MOVE    "*"                 TO  SYS-1033-KBNCD
           MOVE    SYS-1033-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               ADD     1                   TO  IDX
               MOVE    SYS-1033-STYUKYMD   TO  WRK-1033-STYUKYMD   (IDX)
               MOVE    SYS-1033-EDYUKYMD   TO  WRK-1033-EDYUKYMD   (IDX)
               MOVE    SYS-1033-TBL        TO  WRK-1033-HKNJANUM-G (IDX)
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT91-HOSPNUM
           MOVE    "COLDT910"          TO  COLDAT91-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT91-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT01-HOSPNUM
           MOVE    "COLDT010"          TO  COLDAT01-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT01-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT02-HOSPNUM
           MOVE    "COLDT020"          TO  COLDAT02-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT02-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT03-HOSPNUM
           MOVE    "COLDT030"          TO  COLDAT03-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT03-TERMID
      *
           OPEN    OUTPUT              COLDAT911-FILE
           CLOSE                       COLDAT911-FILE
           OPEN    OUTPUT              COLDAT011-FILE
           CLOSE                       COLDAT011-FILE
           OPEN    OUTPUT              COLDAT021-FILE
           CLOSE                       COLDAT021-FILE
           OPEN    OUTPUT              COLDAT031-FILE
           CLOSE                       COLDAT031-FILE
      *
           OPEN    I-O                 COLDAT91-FILE
                                       COLDAT01-FILE
                                       COLDAT02-FILE
                                       COLDAT03-FILE
      *
           PERFORM 1001-FILE-SYORI-SEC
           PERFORM 10011-FILE-SYORI-SEC
           PERFORM 10012-FILE-SYORI-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    病院基本ファイル処理
      *****************************************************************
       1001-FILE-SYORI-SEC              SECTION.
      *
      *    医療機関情報処理
           INITIALIZE                      COLDAT91-REC
           MOVE    "1"                 TO  COLDAT91-RECKBN
           MOVE    1                   TO  COLDAT91-RECSEQ
           MOVE    WRK-HOSPID          TO  COLDAT91-HOSPID
           MOVE    SYS-1001-PREFNUM    TO  COLDAT91-PREFNUM
           MOVE    SYS-1001-HOSPSBT    TO  COLDAT91-HOSPSBT
           MOVE    SYS-1001-HOSPSBT1   TO  COLDAT91-HOSPSBT1
           MOVE    SYS-1001-BEDSU      TO  COLDAT91-BEDSU
           MOVE    SYS-1001-BEDSUIPN   TO  COLDAT91-BEDSUIPN
           MOVE    WRK-PARA-NYUGAIKBN  TO  COLDAT91-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM      TO  COLDAT91-DATAYM
           MOVE    PRGSKBNO            TO  COLDAT91-PRGSKBNO
           WRITE   COLDAT91-REC
           ADD     1                   TO  CNT-COLDAT91
           .
       1001-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病院基本ファイル処理（診療科情報）
      *****************************************************************
       10011-FILE-SYORI-SEC        SECTION.
      *
      *    診療科情報
           INITIALIZE                      SYS-1005-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    "*"                 TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ADD     1                   TO  IDX
               INITIALIZE                      COLDAT91-REC
               MOVE    "2"                 TO  COLDAT91-RECKBN
               MOVE    IDX                 TO  COLDAT91-RECSEQ
               MOVE    SYS-1005-KBNCD(1:2) TO  COLDAT91-SRYKA
               MOVE    SYS-1005-RECESRYKA  TO  COLDAT91-RECEKA
               MOVE    SYS-1005-SRYKANAME  TO  COLDAT91-SRYKANAME
               WRITE   COLDAT91-REC
               ADD     1                   TO  CNT-COLDAT91
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       10011-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病院基本ファイル処理（施設基準情報）
      *****************************************************************
       10012-FILE-SYORI-SEC        SECTION.
      *
      *    施設基準情報
           INITIALIZE                      SYS-1006-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "*"                 TO  SYS-1006-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1006-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1006-STYUKYMD(7:2)
           MOVE    SYS-1006-STYUKYMD   TO  SYS-1006-EDYUKYMD
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               MOVE    SYS-1006-KBNCD(1:1) TO  WRK-CHGNUM-P
               PERFORM VARYING  IDX1    FROM  1  BY  1
                                UNTIL   IDX1  >  500
                   IF  SYS-1006-SSTKIJUN (IDX1)  =  1
                       ADD     1        TO  IDX
                       INITIALIZE           COLDAT91-REC
                       MOVE    "3"      TO  COLDAT91-RECKBN
                       MOVE    IDX      TO  COLDAT91-RECSEQ
                       COMPUTE COLDAT91-SSTKIJUNCD
                                        =   (WRK-CHGNUM  -  1)
                                        *   500
                                        +   IDX1
                       WRITE   COLDAT91-REC
                       ADD     1        TO  CNT-COLDAT91
                   END-IF
               END-PERFORM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       10012-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病院基本ファイル処理（保険番号情報）
      *****************************************************************
       10013-FILE-SYORI-SEC        SECTION.
      *
      *    保険番号情報
           MOVE    ZERO                TO  IDX
           PERFORM VARYING WRK-HKNNUM-IDX    FROM    1   BY  1
                   UNTIL   WRK-HKNNUM-IDX    >   WRK-HKNNUM-MAX
               MOVE    TBL-HKNNUM-REC (WRK-HKNNUM-IDX)
                                       TO  HKNNUM-REC
               IF      HKN-HKNNUM      >   "099"
                 ADD     1                 TO  IDX
                 INITIALIZE                    COLDAT91-REC
                 MOVE    "4"               TO  COLDAT91-RECKBN
                 MOVE    IDX               TO  COLDAT91-RECSEQ
                 MOVE    HKN-HKNNUM        TO  COLDAT91-HKNNUM
                 MOVE    HKN-PAYKBN        TO  COLDAT91-PAYKBN
                 MOVE    HKN-HBTNUM        TO  COLDAT91-HBTNUM
                 MOVE    HKN-SEIDONAME     TO  COLDAT91-SEIDONAME
                 MOVE    HKN-HKNKOHSBTKBN  TO  COLDAT91-HKNKOHSBTKBN
                 MOVE    HKN-HKNKOHKBN     TO  COLDAT91-HKNKOHKBN
                 MOVE    HKN-JOKEN-STYEAR  TO  COLDAT91-JOKEN-STYEAR
                 MOVE    HKN-JOKEN-EDYEAR  TO  COLDAT91-JOKEN-EDYEAR
                 MOVE    HKN-TENTNK        TO  COLDAT91-TENTNK
                 MOVE    HKN-RECESKYKBN    TO  COLDAT91-RECESKYKBN
                 WRITE   COLDAT91-REC
                 ADD     1                 TO  CNT-COLDAT91
               END-IF
           END-PERFORM
           .
       10013-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    "1"
      *            入院会計読み込み（患者ＩＤ、診療科、剤識別区分順）
                   INITIALIZE                  NYUINACCT-REC
grpsys             MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
                   MOVE    WRK-PARA-NYUGAIKBN  TO  NACCT-NYUGAIKBN
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key18"             TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key18"         TO  MCP-PATHNAME
                       PERFORM 900-NYUINACCT-READ-SEC
                   ELSE
                       INITIALIZE                  NYUINACCT-REC
                       MOVE    1               TO  FLG-NYUINACCT
                   END-IF
      *
                   PERFORM 2001-NYUIN-HENSYU-SEC
                                       UNTIL   FLG-NYUINACCT   =   1
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key18"             TO  MCP-PATHNAME
                   PERFORM 900-DBCLOSE-SEC
               WHEN    "2"
      *            診療会計読み込み（患者ＩＤ、保険組合せ順）
                   INITIALIZE                  SRYACCT-REC
grpsys             MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  ACCT-SRYYM
                   MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"   TO  MCP-TABLE
                       MOVE    "key8"          TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1               TO  FLG-SRYACCT
                   END-IF
      *
                   PERFORM 2001-NYUINGAI-HENSYU-SEC
                                       UNTIL   FLG-SRYACCT     =   1
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-DBCLOSE-SEC
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院処理
      *****************************************************************
       2001-NYUIN-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  WRK-COLDAT01-TABLE
           INITIALIZE                  TBL-JNISSU
           INITIALIZE                  TBL-JNISSU-S
           INITIALIZE                  TBL-JNISSU-GEN
           INITIALIZE                  TBL-TOTALTENSU-INF
           INITIALIZE                  WRK-TOTALTENSU-INF
           INITIALIZE                  TBL-SKJNISSU-INF
           INITIALIZE                  TBL-SKJRYOYOHI-INF
           INITIALIZE                  TBL-SKJFTN-INF
           MOVE    SPACE               TO  WRK-NYUTAIIN
           MOVE    ZERO                TO  WRK-ZAINUM
      *
      *    テスト患者のときは読み飛ばす
grpsys     MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    NACCT-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           DISPLAY "NYUINACCT PTID=" NACCT-PTID
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF    ( FLG-PTINF           =   ZERO )
           AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      NACCT-PTID  NOT =   PTINF-PTID
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key18"                 TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
               INITIALIZE                      WRK-DAY-AREA
                                               WRK-HKNCOMBI-TABLE
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      NACCT-PTID  NOT =   PTINF-PTID
                 IF    NACCT-ZAISKBKBN    =  "1"  OR  "2"  OR
                                             "4"  OR  "5"
      *            剤識別区分別テーブル編集
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "2"
                           MOVE    4               TO  IDX
                       WHEN    "4"
                           MOVE    3               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       31
                       IF      NACCT-DAY  (IDX1)       >   ZERO
                         IF    IDX         =       3
                           IF  NACCT-SRYYM >       "200603"
                               ADD   1     TO      WRK-DAY (IDX IDX1)
                           ELSE
                               MOVE  1     TO      WRK-DAY (IDX IDX1)
                           END-IF
                         ELSE
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                         END-IF
                       END-IF
                   END-PERFORM
      **             IF      WRK-ZAINUM              <   NACCT-ZAINUM
      **                 MOVE    NACCT-ZAINUM        TO  WRK-ZAINUM
      **             END-IF
                 END-IF
                 MOVE    "tbl_nyuinacct"           TO  MCP-TABLE
                 MOVE    "key18"                   TO  MCP-PATHNAME
                 PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
      *        入院会計の保険組合せをまとめる
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF      WRK-DAY (1 IDX)     >   ZERO    AND
                           WRK-DAY (2 IDX)     >   ZERO
                       PERFORM 20011-NYUINACCT-HENSYU-SEC
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   MOVE    WRK-HKNCOMBI-REC (IDX2)
                                               TO  HKNCOMBI-REC
      *
                   MOVE    "1"                 TO  WRK-DATAKBN
                   PERFORM 2001-FILE-SYORI-SEC
      *
      *            入院基本料等処理
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  NOT =   "1"
                       PERFORM 20013-NYUIN-KIHON-SEC
                   END-IF
      *ito 20080425  END-PERFORM を15行くらい後に移動させた
      *
                   PERFORM 20011-SKJFTN-SEC
                   PERFORM 200111-NYUIN-JNISSU-TAIHI-SEC
                   PERFORM 200112-SKJNISSU-TAIHI-SEC
      *            実日数、点数編集
                   PERFORM 20012-JNISSU-SET-SEC
                   PERFORM 20013-TOTALTENSU-SET-SEC
                   MOVE    TBL-JNISSU      TO  TBL-JNISSU-S
                   INITIALIZE                  TBL-JNISSU
                   INITIALIZE                  TBL-JNISSU-GEN
                   INITIALIZE                  TBL-TOTALTENSU-INF
                   INITIALIZE                  WRK-TOTALTENSU-INF
                   INITIALIZE                  TBL-SKJNISSU-INF
                   INITIALIZE                  TBL-SKJRYOYOHI-INF
                   INITIALIZE                  TBL-SKJFTN-INF
               END-PERFORM
      *        診療会計編集処理
               PERFORM 20012-NYUIN-SRYACCT-HENSYU-SEC
      *
      *        基本ファイル出力処理
               PERFORM 2002-KIHON-WRITE-SEC
           END-IF
           .
       2001-NYUIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       20011-NYUINACCT-HENSYU-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *
      *        保険組合せ追加
               IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                   INITIALIZE                  HKNCOMBI-REC
grpsys             MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
                   MOVE    PTINF-PTID      TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX) TO
                                           COMB-HKNCOMBINUM
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
                   IF      FLG-HKNCOMBI    =   ZERO
                       MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
                   ELSE
                       MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                       MOVE    "1"     TO  WRK-HKNCOMBI-SYOKBN  (IDX1)
                   END-IF
               END-IF
      *
      *        適用範囲かチェック（範囲外のとき２をセット）
               IF      WRK-DAY (2 IDX)     =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)   NOT =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDX             TO  WRK-DD
                       IF      WRK-COMB-TEKSTYMD (IDX1)
                                               <=  WRK-YMD
                       AND     WRK-COMB-TEKEDYMD (IDX1)
                                               >=  WRK-YMD
                           CONTINUE
                       ELSE
                           MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                       END-IF
                   END-IF
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM
           .
       20011-NYUINACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準負担作成
      *****************************************************************
       20011-SKJFTN-SEC            SECTION.
      *
           INITIALIZE                      ORCSNYURECEDEN-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYURECEDEN-HOSPNUM
           MOVE    PTINF-PTID          TO  LNK-NYURECEDEN-PTID
           MOVE    WRK-PARA-SRYYM      TO  LNK-NYURECEDEN-SANTEIYM
           IF      COLDAT01-HKNNUM     =   "060" OR "067" OR
                                           "068" OR "069"
               MOVE    "2"             TO  LNK-NYURECEDEN-HKNKBN
           ELSE
               IF      COLDAT01-HKNNUM     =   "039" OR "040"
                   MOVE    "3"             TO  LNK-NYURECEDEN-HKNKBN
               ELSE
                   MOVE    "1"             TO  LNK-NYURECEDEN-HKNKBN
               END-IF
           END-IF
           CALL    "ORCSNYURECEDEN"    USING   ORCSNYURECEDEN-AREA
                                               SPA-AREA
           IF      LNK-NYURECEDEN-RC   NOT =   ZERO
                   DISPLAY "LNK-NYURECEDEN-RC=" LNK-NYURECEDEN-RC
                   GO    TO    20011-SKJFTN-EXT
           END-IF
      *
           PERFORM VARYING IDZ1    FROM    3   BY  1
                   UNTIL   IDZ1    >       5
             PERFORM VARYING IDZ2    FROM    1   BY  1
                     UNTIL   IDZ2    >       31
                     OR      LNK-NYURECEDEN-HKNCOMBI (IDZ1 IDZ2)
                                               =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               PERFORM VARYING IDXZ    FROM    1   BY  1
                       UNTIL   IDXZ    >       10
                       OR      COLDAT01-HKNCOMBI (IDXZ)  =   ZERO
                       IF      LNK-NYURECEDEN-HKNCOMBI (IDZ1 IDZ2)
                                           =   COLDAT01-HKNCOMBI (IDXZ)
                           MOVE    1           TO  FLG-CHK
                           MOVE    10          TO  IDXZ
                       END-IF
               END-PERFORM
               PERFORM VARYING IDXZ    FROM    1   BY  1
                       UNTIL   IDXZ    >
                                   LNK-NYURECEDEN-MEISAISU (IDZ1 IDZ2)
                       OR      FLG-CHK =       ZERO
                 IF  ( LNK-NYURECEDEN-SRYCD (IDZ1 IDZ2 IDXZ)
                                       =   WRK-CONS-HYOJYUN-FUTAN1
                                       OR  WRK-CONS-HYOJYUN-FUTAN2
                                       OR  WRK-CONS-HYOJYUN-FUTAN3
                                       OR  WRK-CONS-HYOJYUN-FUTAN4
                                       OR  WRK-CONS-HYOJYUN-FUTAN5
                                       OR  WRK-CONS-HYOJYUN-FUTAN6
                                       OR  WRK-CONS-HYOJYUN-FUTAN7
                                       OR  WRK-CONS-HYOJYUN-FUTAN8
                                       OR  WRK-CONS-HYOJYUN-FUTAN9
                                       OR  WRK-CONS-HYOJYUN-FUTAN10
                                       OR  WRK-CONS-HYOJYUN-FUTAN11
                                       OR  WRK-CONS-HYOJYUN-FUTAN12
                                       OR  WRK-CONS-HYOJYUN-FUTAN13
                                       OR  WRK-CONS-HYOJYUN-FUTAN14
                                       OR  WRK-CONS-HYOJYUN-FUTAN15
                                       OR  WRK-CONS-HYOJYUN-FUTAN16 )
                   MOVE    SPACE           TO  COLDAT02-REC
                   INITIALIZE                  COLDAT02-REC
      *            診療会計データ編集
                   MOVE    COLDAT01-KEY    TO  COLDAT02-KEY
                   COMPUTE WRK-ZAINUM      =   WRK-ZAINUM  +  1
                   MOVE    WRK-ZAINUM      TO  COLDAT02-ZAINUM
                   MOVE    1               TO  COLDAT02-SEQNO
                   MOVE    COMB-HKNID      TO  COLDAT02-HKNID
                   PERFORM VARYING IDZ3    FROM    1   BY  1
                           UNTIL   IDZ3    >   4
                     MOVE  COMB-KOHID (IDZ3)
                                           TO  COLDAT02-TKOHID(IDZ3)
                   END-PERFORM
                   MOVE    "97"            TO  COLDAT02-SRYKBN
                   MOVE    "970"           TO  COLDAT02-SRYSYUKBN
                   MOVE    LNK-NYURECEDEN-TEN (IDZ1 IDZ2 IDXZ)
                                           TO  COLDAT02-ZAITEN
                                               COLDAT02-SYUTEN1
                   MOVE    LNK-NYURECEDEN-KAISU (IDZ1 IDZ2 IDXZ)
                                           TO  COLDAT02-ZAIKAISU
                   MOVE    LNK-NYURECEDEN-HKNCOMBI (IDZ1 IDZ2)
                                           TO  COLDAT02-HKNCOMBI
                   PERFORM VARYING IDZ3    FROM    1   BY  1
                           UNTIL   IDZ3    >   31
                     MOVE  SKJFTN-KAISU (IDZ3)
                                           TO  COLDAT02-KAISU (IDZ3)
                   END-PERFORM
                   MOVE    LNK-NYURECEDEN-SRYCD (IDZ1 IDZ2 IDXZ)
                                           TO  COLDAT02-SRYCD
                   MOVE    LNK-NYURECEDEN-SURYO (IDZ1 IDZ2 IDXZ)
                                           TO  COLDAT02-SRYSURYO
                   IF      COLDAT02-SRYSURYO   =   ZERO
                       MOVE  1             TO  COLDAT02-SRYSURYO
                   END-IF
      *
                   INITIALIZE                  TNS-TENSU-REC
grpsys             MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
                   MOVE    COLDAT02-SRYCD  TO  TNS-SRYCD
                   MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
                   PERFORM 900-TENSU-READ-SEC
                   MOVE    TNS-TANICD      TO  COLDAT02-TANICD
                   MOVE    TNS-TANINAME    TO  COLDAT02-TANINAME
                   MOVE    TNS-FORMALNAME  TO  COLDAT02-FORMALNAME
                   MOVE    TNS-TENSIKIBETU TO  COLDAT02-TENSIKIBETU
                   MOVE    TNS-TEN         TO  COLDAT02-TEN
                   IF      TNS-TENSIKIBETU     =   1   OR   3
                       MOVE  TNS-NYUTENTTLKBN
                                           TO  COLDAT02-TENTTLKBN
                   END-IF
                   MOVE    TNS-NAME        TO  COLDAT02-NAME
                   IF   SKJFTN-MAX         =   1
                        MOVE  SKJFTN-SRYKA(1)
                                           TO  COLDAT02-SRYKA
                   ELSE
                     PERFORM VARYING IDZ3  FROM  1  BY  1
                             UNTIL  (IDZ3  >   31)
                             OR     (SKJFTN-SRYKA (IDZ3)
                                           =   SPACE)
                       IF  COLDAT02-ZAIKAISU = SKJFTN-KAISU(IDZ3)
                           MOVE SKJFTN-SRYKA(IDZ3)
                                           TO  COLDAT02-SRYKA
                           MOVE 31         TO  IDZ3
                       END-IF
                     END-PERFORM
                   END-IF
                   IF      COLDAT02-SRYKA  =   SPACE
                       MOVE  SKJFTN-SRYKA(1)
                                           TO  COLDAT02-SRYKA
                   END-IF
                   PERFORM 200313-SKJFTN-TAIHI-SEC
                   WRITE   COLDAT02-REC
                   ADD     1               TO  CNT-COLDAT02
                 END-IF
               END-PERFORM
             END-PERFORM
           END-PERFORM
           .
       20011-SKJFTN-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数退避
      *****************************************************************
       200111-NYUIN-JNISSU-TAIHI-SEC     SECTION.
      *
           PERFORM VARYING    IDX6   FROM  1  BY  1
                   UNTIL      IDX6     >   31
               IF  WRK-DAY (1 IDX6)    >   ZERO    AND
                   WRK-DAY (2 IDX6)    >   ZERO
                 IF  (WRK-DAY(2 IDX6)  =   WRK-COMB-HKNCOMBINUM (IDX2))
      *ito 20080425 IF  (COLDAT01-HKNNUM      NOT =   SPACE)   OR
      *ito              (COLDAT01-RJNHKNNUM   NOT =   SPACE)
                     MOVE    "1"       TO  TBL-JNISSU-DAY(1 IDX6)
                 END-IF
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    10
                     IF  COLDAT01-HKNCOMBI(IDX3) = WRK-DAY(2 IDX6)
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                          MOVE    "1"       TO  TBL-JNISSU-DAY(2 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                          MOVE    "1"       TO  TBL-JNISSU-DAY(3 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                          MOVE    "1"       TO  TBL-JNISSU-DAY(4 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                          MOVE    "1"       TO  TBL-JNISSU-DAY(5 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
               END-IF
           END-PERFORM
           .
       200111-NYUIN-JNISSU-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事日数退避
      *****************************************************************
       200112-SKJNISSU-TAIHI-SEC     SECTION.
      *
           PERFORM VARYING    IDX6   FROM  1  BY  1
                   UNTIL      IDX6     >   31
               IF  (WRK-DAY (3 IDX6)    >   ZERO                    )
               AND (WRK-DAY (2 IDX6)    NOT =   ZERO                )
               AND ((WRK-DAY(4 IDX6)        =   ZERO               ) OR
                    (WRK-DAY(4 IDX6)       >=    4                 ))
                 IF  (WRK-DAY(2 IDX6)  =   WRK-COMB-HKNCOMBINUM (IDX2))
      *ito 20080425 IF  (COLDAT01-HKNNUM      NOT =   SPACE)   OR
      *ito              (COLDAT01-RJNHKNNUM   NOT =   SPACE)
                     MOVE WRK-DAY (3 IDX6)  TO  TBL-SKJNISSU(1 IDX6)
                 END-IF
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    10
                     IF  COLDAT01-HKNCOMBI(IDX3) = WRK-DAY(2 IDX6)
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                          MOVE WRK-DAY (3 IDX6)
                                            TO  TBL-SKJNISSU(2 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                          MOVE WRK-DAY (3 IDX6)
                                            TO  TBL-SKJNISSU(3 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                          MOVE WRK-DAY (3 IDX6)
                                            TO  TBL-SKJNISSU(4 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                          MOVE WRK-DAY (3 IDX6)
                                            TO  TBL-SKJNISSU(5 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
               END-IF
           END-PERFORM
           .
       200112-SKJNISSU-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計編集処理（入院）
      *****************************************************************
       20012-NYUIN-SRYACCT-HENSYU-SEC           SECTION.
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
           MOVE    SPACE               TO  WRK-ERR-AREA
           INITIALIZE                      WRK-ERR-AREA
      *
           INITIALIZE                      SRYACCT-REC
grpsys     MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  ACCT-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    PTINF-PTID          TO  ACCT-PTID
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      KEY-N-HKNCOMBI      =   9999
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    PTINF-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    ZERO                TO  FLG-HKNCOMBI 
               ELSE
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    PTINF-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                        ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   ELSE
                      MOVE    "1"             TO  WRK-TEISEIKBN
                   END-IF
               END-IF
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      KEY-NEW     NOT =   KEY-OLD
                   MOVE    "2"                 TO  WRK-DATAKBN
                   PERFORM 2001-FILE-SYORI-SEC
      *
                   IF      WRK-UPDKBN          =   SPACE
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
                           PERFORM 2003-SRYACCT-HENSYU-SEC
                           MOVE    "tbl_sryacct"      TO  MCP-TABLE
                           MOVE    "key9"             TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-PERFORM
                   ELSE
                       PERFORM 2003-SRYACCT-HENSYU-SEC
                       MOVE    "tbl_sryacct"      TO  MCP-TABLE
                       MOVE    "key9"             TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM
      *        実日数編集
               PERFORM   20012-JNISSU-SET-SEC
               PERFORM   20013-TOTALTENSU-SET-SEC
               INITIALIZE                  TBL-JNISSU
               INITIALIZE                  TBL-JNISSU-GEN
               INITIALIZE                  TBL-TOTALTENSU-INF
               INITIALIZE                  TBL-SKJNISSU-INF
               INITIALIZE                  TBL-SKJRYOYOHI-INF
               INITIALIZE                  TBL-SKJFTN-INF
           END-PERFORM
      *
           MOVE    "tbl_sryacct"      TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       20012-NYUIN-SRYACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院基本料・特定入院料・食事療養編集処理
      *****************************************************************
       20013-NYUIN-KIHON-SEC           SECTION.
      *
           INITIALIZE                      HCM34S01-AREA
                                           WRK-SPA-AREA
                                           SKJFTN-DAY-AREA
      *
           PERFORM 200131-NYUINKA-HEN-SEC
      *
           MOVE    SPA-HOSPNUM         TO  HCM34S01-HOSPNUM
                                           WRK-SPA-HOSPNUM
           MOVE    PTINF-PTID          TO  HCM34S01-PTID
           MOVE    WRK-PARA-SRYYM      TO  HCM34S01-SANTEIYM
           MOVE    "1"                 TO  HCM34S01-EXGAIHAKU
           MOVE    COMB-HKNCOMBINUM    TO  HCM34S01-HKNCOMBI
           MOVE    COMB-HKNNUM         TO  HCM34S01-HKNNUM
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *        労災自賠入院会計編集サブ
               CALL    "ORCHCM34S02"       USING HCM34S01-AREA
                                                 WRK-SPA-AREA
           ELSE
      *        医療入院会計編集サブ
               CALL    "ORCHCM34S01"       USING HCM34S01-AREA
                                                 WRK-SPA-AREA
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL   IDX4    >       HCM34S01-HENKYAKU-MAX
      *
             MOVE    1             TO      FLG-EDIT
      *
             PERFORM VARYING IDX5    FROM    1   BY  1
                     UNTIL ( IDX5                >   20    )
                     OR    ( HCM34S01-SRYCD (IDX4 IDX5)
                                                 =   SPACE )
                     OR    ( FLG-EDIT            =   ZERO  )
      *
                 IF  ( HCM34S01-SRYCD (IDX4 IDX5)    =   "099999907" )
                     MOVE    ZERO  TO      FLG-EDIT
                 END-IF
      *
             END-PERFORM
      *
            IF   ( FLG-EDIT                =   1 )
      *
             COMPUTE WRK-ZAINUM            =   WRK-ZAINUM  +  1
             MOVE    ZERO                  TO  WRK-RENNUM
             PERFORM VARYING IDX5    FROM    1   BY  1
                     UNTIL ( IDX5                >   20    )
                     OR    ( HCM34S01-SRYCD (IDX4 IDX5)
                                                 =   SPACE )
      *
               MOVE    SPACE               TO  COLDAT02-REC
               INITIALIZE                      COLDAT02-REC
      *
      *        診療会計データ編集
               MOVE    COLDAT01-KEY        TO  COLDAT02-KEY
               MOVE    WRK-ZAINUM          TO  COLDAT02-ZAINUM
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  COLDAT02-SEQNO
               MOVE    COMB-HKNID          TO  COLDAT02-HKNID
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       4
                   MOVE    COMB-KOHID (IDX3)   TO  COLDAT02-TKOHID(IDX3)
               END-PERFORM
               MOVE    HCM34S01-SRYKBN  (IDX4) TO  COLDAT02-SRYKBN
               MOVE    HCM34S01-ZAITEN  (IDX4) TO  COLDAT02-ZAITEN
                                                   COLDAT02-SYUTEN1
               MOVE    HCM34S01-ZAIKAISU(IDX4) TO  COLDAT02-ZAIKAISU
               MOVE    HCM34S01-HKNCOMBI       TO  COLDAT02-HKNCOMBI
               MOVE    HCM34S01-DAY-TBL (IDX4) TO  COLDAT02-DAY-AREA
               MOVE    HCM34S01-SRYCD   (IDX4 IDX5)
                                               TO  COLDAT02-SRYCD
               MOVE    HCM34S01-SURYO   (IDX4 IDX5)
                                               TO  COLDAT02-SRYSURYO
      *
               INITIALIZE                      TNS-TENSU-REC
grpsys         MOVE    SPA-HOSPNUM             TO  TNS-HOSPNUM
               MOVE    HCM34S01-SRYCD   (IDX4 IDX5)
                                               TO  TNS-SRYCD
               MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-SRYSYUKBN           TO  COLDAT02-SRYSYUKBN
               MOVE    TNS-TANICD              TO  COLDAT02-TANICD
               MOVE    TNS-TANINAME            TO  COLDAT02-TANINAME
               MOVE    TNS-FORMALNAME          TO  COLDAT02-FORMALNAME
               MOVE    TNS-TENSIKIBETU         TO  COLDAT02-TENSIKIBETU
               MOVE    TNS-TEN                 TO  COLDAT02-TEN
               IF      TNS-TENSIKIBETU     =   1   OR   3
                       MOVE  TNS-NYUTENTTLKBN  TO  COLDAT02-TENTTLKBN
               END-IF
               MOVE    TNS-NAME                TO  COLDAT02-NAME
               MOVE    COLDAT02-REC            TO  COLDATWRK-REC
      *
               IF      COLDAT02-SEQNO          =   1
                 INITIALIZE                        SRYACCT-REC
                 MOVE  COLDAT02-SRYKBN         TO  ACCT-SRYKBN
                 MOVE  ZERO                    TO  ACCT-SRYCDTOTAL
                 MOVE  COLDAT02-ZAITEN         TO  ACCT-ZAITEN
                 MOVE  COLDAT02-ZAIKAISU       TO  ACCT-ZAIKAISU
                 PERFORM 200312-TOTALTEN-TAIHI-SEC
               END-IF
               PERFORM UNTIL   COLDATWRK-ZAIKAISU  =   ZERO
                   MOVE    COLDATWRK-REC       TO  COLDAT02-REC
                   MOVE    ZERO                TO  COLDAT02-ZAIKAISU
                   MOVE    ZERO                TO  COLDAT02-DAY-AREA
                   PERFORM VARYING IDX-DAY   FROM  1  BY  1
                                   UNTIL     IDX-DAY  >   31
                     IF    COLDATWRK-KAISU(IDX-DAY)   >   ZERO
                       IF (COLDAT02-SRYKA   =   SPACE)  OR
                          (COLDAT02-SRYKA   =   TBL-NYUINKA(IDX-DAY))
                           MOVE    TBL-NYUINKA(IDX-DAY)
                                   TO  COLDAT02-SRYKA
                           COMPUTE COLDAT02-ZAIKAISU
                                   =   COLDAT02-ZAIKAISU
                                   +   COLDATWRK-KAISU(IDX-DAY)
                           MOVE    COLDATWRK-KAISU(IDX-DAY)
                                   TO  COLDAT02-KAISU(IDX-DAY)
                           COMPUTE COLDATWRK-ZAIKAISU
                                   =   COLDATWRK-ZAIKAISU
                                   -   COLDATWRK-KAISU(IDX-DAY)
                           MOVE    ZERO
                                   TO  COLDATWRK-KAISU(IDX-DAY)
                       END-IF
                     END-IF
                   END-PERFORM
                   IF   (COLDAT02-SEQNO        =   1)    AND
                        (COLDAT02-SRYKBN       =   "97")
                     PERFORM VARYING IDZ1      FROM    1   BY  1
                             UNTIL   IDZ1      >   31
                       IF    SKJFTN-SRYKA (IDZ1)   =   SPACE
                         MOVE COLDAT02-SRYKA   TO  SKJFTN-SRYKA(IDZ1)
                         MOVE COLDAT02-ZAIKAISU
                                               TO  SKJFTN-KAISU(IDZ1)
                         IF   IDZ1             >   SKJFTN-MAX
                              MOVE  IDZ1       TO  SKJFTN-MAX
                         END-IF
                         MOVE 31               TO  IDZ1
                       ELSE
                         IF  SKJFTN-SRYKA (IDZ1)   =   COLDAT02-SRYKA
                             COMPUTE SKJFTN-KAISU(IDZ1)
                                   = SKJFTN-KAISU(IDZ1)
                                   + COLDAT02-ZAIKAISU
                             IF   IDZ1         >   SKJFTN-MAX
                                  MOVE  IDZ1   TO  SKJFTN-MAX
                             END-IF
                             MOVE 31           TO  IDZ1
                         END-IF
                       END-IF
                     END-PERFORM
                   END-IF
                   WRITE   COLDAT02-REC
                   ADD     1               TO  CNT-COLDAT02
               END-PERFORM
             END-PERFORM
      *
            END-IF
      *
           END-PERFORM
           .
       20013-NYUIN-KIHON-EXT.
           EXIT. 
      *
      *****************************************************************
      *    入院日別診療科編集処理
      *****************************************************************
       200131-NYUINKA-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  TBL-NYUINKA-G
           INITIALIZE                      TBL-NYUINKA-G
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
             IF    ( PTNYUINRRK-TENNYUYMD (1:6)    <=  WRK-PARA-SRYYM )
             AND   ( PTNYUINRRK-TENSTUYMD (1:6)    >=  WRK-PARA-SRYYM )
               IF    ( PTNYUINRRK-TENNYUYMD (1:6)  <   WRK-PARA-SRYYM )
                   MOVE    1           TO  TBL-STIDX
               ELSE
                   MOVE    PTNYUINRRK-TENNYUYMD (7:2)
                                       TO  TBL-STIDX
               END-IF
               IF    ( PTNYUINRRK-TENSTUYMD (1:6)  >   WRK-PARA-SRYYM )
                   MOVE    31          TO  TBL-EDIDX
               ELSE
                   MOVE    PTNYUINRRK-TENSTUYMD (7:2)
                                       TO  TBL-EDIDX
               END-IF
               PERFORM VARYING IDX-DAY     FROM    TBL-STIDX   BY  1
                       UNTIL ( IDX-DAY     >       TBL-EDIDX )
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  TBL-NYUINKA (IDX-DAY)
               END-PERFORM
               MOVE    PTNYUINRRK-NYUINYMD
                                       TO  WRK-NYUINYMD
               MOVE    PTNYUINRRK-TAIINYMD
                                       TO  WRK-TAIINYMD
             END-IF
             PERFORM 900-PTNYUINRRK-KEY41-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       200131-NYUINKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       2001-NYUINGAI-HENSYU-SEC    SECTION.
      *
           INITIALIZE                  WRK-COLDAT01-TABLE
           INITIALIZE                  TBL-JNISSU
           INITIALIZE                  TBL-JNISSU-S
           INITIALIZE                  TBL-JNISSU-GEN
           INITIALIZE                  TBL-TOTALTENSU-INF
           INITIALIZE                  WRK-TOTALTENSU-INF
           INITIALIZE                  TBL-SKJNISSU-INF
           INITIALIZE                  TBL-SKJRYOYOHI-INF
           INITIALIZE                  TBL-SKJFTN-INF
      *
      *    テスト患者のときは読み飛ばす
grpsys     MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"           TO  MCP-TABLE
                   MOVE    "key8"                  TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      KEY-N-HKNCOMBI      =   9999
                       INITIALIZE                  HKNCOMBI-REC
                       MOVE    KEY-N-PTID          TO  COMB-PTID
                       MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM
                       MOVE    ZERO                TO  FLG-HKNCOMBI 
                   ELSE
      *                保険組合せ読込み
                       INITIALIZE                  HKNCOMBI-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                       MOVE    KEY-N-PTID          TO  COMB-PTID
                       MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM
                       MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                       PERFORM 910-HKNCOMBI-INV-SEC
      *
                       IF      FLG-HKNCOMBI    =   ZERO
                           IF      COMB-TEKSTYMD   <=  WRK-STYMD
                           AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                               MOVE    SPACE           TO  WRK-UPDKBN
                            ELSE
                               MOVE    "1"             TO  WRK-UPDKBN
                           END-IF
                       ELSE
                           MOVE    "1"             TO  WRK-TEISEIKBN
                       END-IF
                   END-IF
      *
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       MOVE    "2"                 TO  WRK-DATAKBN
                       PERFORM 2001-FILE-SYORI-SEC
      *
                       IF      WRK-UPDKBN          =   SPACE
      *                    同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1
                                       OR      KEY-NEW NOT =   KEY-OLD
                               PERFORM 2003-SRYACCT-HENSYU-SEC
                               MOVE    "tbl_sryacct"   TO  MCP-TABLE
                               MOVE    "key8"          TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           PERFORM 2003-SRYACCT-HENSYU-SEC
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM
      *            実日数編集
                   PERFORM   20012-JNISSU-SET-SEC
      *            合計点数編集
                   PERFORM   20013-TOTALTENSU-SET-SEC
                   INITIALIZE                  TBL-JNISSU
                   INITIALIZE                  TBL-JNISSU-GEN
                   INITIALIZE                  TBL-TOTALTENSU-INF
                   INITIALIZE                  WRK-TOTALTENSU-INF
                   INITIALIZE                  TBL-SKJNISSU-INF
                   INITIALIZE                  TBL-SKJRYOYOHI-INF
                   INITIALIZE                  TBL-SKJFTN-INF
               END-PERFORM
      *
      *        基本ファイル出力処理
               PERFORM   2002-KIHON-WRITE-SEC
           END-IF
           .
       2001-NYUINGAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       2001-FILE-SYORI-SEC              SECTION.
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "1"
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                       MOVE    "1"         TO  WRK-TEISEIKBN
                   END-IF
               WHEN    "2"
                   IF      WRK-UPDKBN          =   "1"
      *                保険の訂正がされているか
                       IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                           MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX             >=  WRK-DD
                                   OR      WRK-TEISEIKBN   =   "1"
                               IF      ACCT-DAY (1 IDX)    >   ZERO
                                   MOVE    "1"     TO  WRK-TEISEIKBN
                               END-IF
                           END-PERFORM
                       ELSE
                           IF      COMB-TEKSTYMD(1:6)
                                                   >   WRK-STYMD (1:6)
                               MOVE    "1"     TO  WRK-TEISEIKBN
                           END-IF
                       END-IF
                       IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                           MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                           ADD     1                   TO  WRK-DD
                           PERFORM VARYING IDX FROM    WRK-DD  BY  1
                                   UNTIL   IDX             >   31
                                   OR      WRK-TEISEIKBN   =   "1"
                               IF      ACCT-DAY (1 IDX)    >   ZERO
                                   MOVE    "1"     TO  WRK-TEISEIKBN
                               END-IF
                           END-PERFORM
                       ELSE
                           IF      COMB-TEKEDYMD(1:6)
                                                   <   WRK-EDYMD (1:6)
                               MOVE    "1"         TO  WRK-TEISEIKBN
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    COMB-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    COLDAT01-PTID   TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYM  TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
      *    平成２０年４月以降に老人保険使用のとき
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-KOH1HKNNUM     =   "027"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
           END-IF
      *
      *    平成２０年４月以降に退職国保で６５歳以上のとき
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-HKNNUM         =   "067"                 )
               IF      AGECHK-NENREI       >=  65
                   MOVE    "1"                 TO  WRK-TEISEIKBN
               END-IF
          END-IF
      *
      *    平成２２年４月以降に政府管掌を使用のとき
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201004 )
           AND   ( COMB-HKNNUM         =   "001"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
           END-IF
      *
      *   主保険情報読み出し
           INITIALIZE                      PTHKNINF-REC
                                           PTRSIINF-REC
           IF      COMB-HKNID          NOT =   ZERO
               EVALUATE    COMB-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                       INITIALIZE                  PTRSIINF-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
                       MOVE    COMB-HKNID          TO  PTRSI-HKNID
                       MOVE    COMB-PTID           TO  PTRSI-PTID
                       MOVE    PTRSIINF-REC        TO  MCPDATA-REC
                       PERFORM 900-PTRSIINF-INV-SEC
                   WHEN    OTHER
                       INITIALIZE                  PTHKNINF-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
                       MOVE    COMB-PTID           TO  PTHKN-PTID
                       MOVE    COMB-HKNID          TO  PTHKN-HKNID
                       MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                       MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKEDYMD
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       PERFORM 910-PTHKNINF-INV-SEC
               END-EVALUATE
           END-IF
      *
      *    ファイル編集処理
           INITIALIZE                      COLDAT01-REC
           MOVE    WRK-HOSPID          TO  COLDAT01-HOSPID
           MOVE    WRK-PARA-SRYYM      TO  COLDAT01-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  COLDAT01-NYUGAIKBN
           MOVE    COMB-PTID           TO  COLDAT01-PTID
           MOVE    COMB-HKNNUM         TO  COLDAT01-HKNNUM
           MOVE    PTHKN-HONKZKKBN     TO  COLDAT01-HONKZKKBN
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  COLDAT01-KOHID
           ELSE
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE    COMB-KOH1ID         TO  COLDAT01-KOHID
                   MOVE    "0"                 TO  WK-HONKZKKBN
               ELSE
                   MOVE    ZERO                TO  COLDAT01-KOHID
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE    "0" TO  WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE    "0" TO  WK-HONKZKKBN
                             END-EVALUATE
                       WHEN    "060"
                       WHEN    "068"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "0" TO  WK-HONKZKKBN
                              END-EVALUATE
                      WHEN    "040"
                      WHEN    "039"
                          EVALUATE   PTHKN-HOJOKBN
                              WHEN   "1"
                              WHEN   "3"
                                 MOVE    "0"   TO  WK-HONKZKKBN
                          END-EVALUATE
                      WHEN    OTHER
                          EVALUATE   PTHKN-HOJOKBN
                             WHEN   "9"
                                 MOVE    "0" TO  WK-HONKZKKBN
                             WHEN   "8"
                                 MOVE    "0" TO  WK-HONKZKKBN
                             WHEN   "7"
                                 MOVE    "0" TO  WK-HONKZKKBN
                         END-EVALUATE
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    PTHKN-HKNJANUM      TO  COLDAT01-HKNJANUM
           IF    ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOH1HKNNUM     =   "020"
                                       OR  "030" )
               MOVE    COMB-KOH1HKNNUM     TO  COLDAT01-HKNJANUM
           END-IF
           IF      WRK-PARA-SRYYM      <=  "200609"
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    PTHKN-HOJOKBN   TO
                                               COLDAT01-HOJOKBN
                   END-IF
               WHEN    "060"
               WHEN    "068"
                   EVALUATE    WRK-PARA-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0"             TO
                                                   COLDAT01-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1"             TO
                                                   COLDAT01-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2"             TO
                                                   COLDAT01-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1"             TO
                                                   COLDAT01-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2"             TO
                                                   COLDAT01-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3"             TO
                                                   COLDAT01-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    OTHER
                   EVALUATE   PTHKN-HOJOKBN
                      WHEN   "9"
                      WHEN   "8"
                          MOVE    PTHKN-HOJOKBN   TO
                                               COLDAT01-HOJOKBN
                   END-EVALUATE
             END-EVALUATE
           ELSE
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                       OR    ( AGECHK-NENREI <=  74                    )
                           MOVE    PTHKN-HOJOKBN   TO  COLDAT01-HOJOKBN
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                                   MOVE    "1" TO  COLDAT01-HOJOKBN
                               WHEN    "B"
                               WHEN    "H"
                                   MOVE    "2" TO  COLDAT01-HOJOKBN
                               WHEN    "C"
                               WHEN    "I"
                                   MOVE    "3" TO  COLDAT01-HOJOKBN
                               WHEN    OTHER
                                   MOVE    PTHKN-HOJOKBN
                                               TO  COLDAT01-HOJOKBN
                           END-EVALUATE
                       END-IF
                   END-IF
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    PTHKN-HOJOKBN   TO  COLDAT01-HOJOKBN
                   END-IF
               WHEN    "060"
               WHEN    "068"
                   EVALUATE    WRK-PARA-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0" TO  COLDAT01-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1" TO  COLDAT01-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2" TO  COLDAT01-HOJOKBN
                               WHEN   "3"
                                   MOVE    "3" TO  COLDAT01-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1" TO  COLDAT01-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2" TO  COLDAT01-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3" TO  COLDAT01-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    "040"
               WHEN    "039"
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "1"
                       WHEN   "3"
                           MOVE    PTHKN-HOJOKBN   TO  COLDAT01-HOJOKBN
                   END-EVALUATE
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                       WHEN   "7"
                           MOVE    PTHKN-HOJOKBN
                                               TO  COLDAT01-HOJOKBN
                   END-EVALUATE
             END-EVALUATE
           END-IF
      *
           IF      COMB-HKNCOMBINUM    =    9999
               MOVE    "9999999"            TO  COLDAT01-HKNJANUM
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-COLDAT01-TABLE-MAX
               IF      WRK-COLDAT01-HOSPID (IDXX)  =   SPACE
                   PERFORM 20011-COLDAT01-WRITE-SEC
                   MOVE    WRK-COLDAT01-TABLE-MAX
                                              TO  IDXX
               ELSE
                   IF      WRK-COLDAT01-KEY (IDXX) =
                                               COLDAT01-KEY
                       PERFORM 20011-COLDAT01-REWRITE-SEC
                       MOVE    WRK-COLDAT01-TABLE-MAX
                                                   TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
           .
       2001-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       20011-COLDAT01-WRITE-SEC              SECTION.
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
           IF      COLDAT01-HKNNUM NOT =   SPACE
               EVALUATE    COLDAT01-HKNNUM
                 WHEN    WRK-CONS-ROUSAI-HKNNUM
                 WHEN    WRK-CONS-JIBAI-HKNNUM
      *            労災情報 
                   MOVE   PTRSI-HKNID      TO  COLDAT01-HKNID
                   MOVE   PTRSI-HKNKBN     TO  COLDAT01-HKNKBN
                   MOVE   PTRSI-SAIGAIKBN  TO  COLDAT01-SAIGAIKBN
                 WHEN    OTHER
      *            主保険情報 
                   MOVE    PTHKN-HKNID     TO  COLDAT01-HKNID
                                               COLDAT01-THKNID     (1)
                   MOVE    PTHKN-TEKSTYMD  TO  COLDAT01-TTEKSTYMD  (1)
                   MOVE    PTHKN-TEKEDYMD  TO  COLDAT01-TTEKEDYMD  (1)
                   MOVE    PTHKN-CONTKBN   TO  COLDAT01-TCONTKBN   (1)
                   MOVE    PTHKN-HOJOKBN   TO  COLDAT01-THOJOKBN   (1)
                   IF      PTHKN-HKNNUM    =   "002"
                       IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                       OR    ( AGECHK-NENREI <=  74                    )
                           CONTINUE
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                                   MOVE    "1" TO  COLDAT01-THOJOKBN (1)
                               WHEN    "B"
                               WHEN    "H"
                                   MOVE    "2" TO  COLDAT01-THOJOKBN (1)
                               WHEN    "C"
                               WHEN    "I"
                                   MOVE    "3" TO  COLDAT01-THOJOKBN (1)
                           END-EVALUATE
                       END-IF
                   END-IF
                   MOVE    PTHKN-HONKZKKBN TO  COLDAT01-THONKZKKBN (1)
      *            法別番号
                   INITIALIZE                      HKNNUM-REC
grpsys             MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                   MOVE    PTHKN-HKNNUM        TO  HKN-HKNNUM
                   MOVE    ZERO                TO  HKN-PAYKBN
                   MOVE    WRK-SYMD            TO  HKN-TEKSTYMD
                                                   HKN-TEKEDYMD
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       MOVE    HKN-HBTNUM      TO  COLDAT01-HKNHBTNUM
                       MOVE    HKN-HKNKOHSBTKBN
                                               TO  COLDAT01-HKNKOHSBTKBN
                   END-IF
               END-EVALUATE
           END-IF
      *
      *    老人保険情報
           IF      COLDAT01-KOHID  >   ZERO
               MOVE    COMB-KOH1HKNNUM TO  COLDAT01-RJNHKNNUM
      *
grpsys         MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
               MOVE    PTINF-PTID      TO  PTKOH-PTID
               MOVE    COLDAT01-KOHID  TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC    TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               IF       FLG-PTKOHINF       =   ZERO
                   MOVE    PTKOH-PAYKBN        TO  COLDAT01-RJNPAYKBN
                   MOVE    PTKOH-TEKSTYMD      TO  COLDAT01-RJNTEKSTYMD
                   MOVE    PTKOH-TEKEDYMD      TO  COLDAT01-RJNTEKEDYMD
      *            法別番号のセット
                   INITIALIZE                  HKNNUM-REC
grpsys             MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
                   MOVE    PTKOH-KOHNUM    TO  HKN-HKNNUM
                   MOVE    PTKOH-PAYKBN    TO  HKN-PAYKBN
                   MOVE    WRK-SYMD        TO  HKN-TEKSTYMD
                                               HKN-TEKEDYMD
                   MOVE    HKNNUM-REC      TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM      =   ZERO
                       MOVE   HKN-HBTNUM       TO COLDAT01-RJNHBTNUM
                   END-IF
               END-IF
           END-IF
      *
      *    患者情報
           MOVE    PTINF-SEX       TO  COLDAT01-SEX
           MOVE    PTINF-BIRTHDAY  TO  COLDAT01-BIRTHDAY
           MOVE    PTINF-HOME-POST TO  COLDAT01-HOME-POST
      *
      *    年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM     TO  AGECHK-HOSPNUM
           MOVE    COLDAT01-PTID   TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYM  TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC       =   ZERO
               MOVE  AGECHK-NENREI     TO  COLDAT01-AGE
               MOVE  AGECHK-SYUGAKUKBN TO  COLDAT01-SYUGAKUKBN
           END-IF
      *
           MOVE    COMB-HKNCOMBINUM    TO  COLDAT01-HKNCOMBI (1)
           MOVE    COMB-HKNNUM         TO  COLDAT01-HKNNUMHC (1)
           MOVE    COMB-KOHHKNNUM (1)  TO  COLDAT01-KOHNUMHC (1 1)
           MOVE    COMB-KOHID (1)      TO  COLDAT01-KOHIDHC  (1 1)
           MOVE    COMB-KOHHKNNUM (2)  TO  COLDAT01-KOHNUMHC (1 2)
           MOVE    COMB-KOHID (2)      TO  COLDAT01-KOHIDHC  (1 2)
           MOVE    COMB-KOHHKNNUM (3)  TO  COLDAT01-KOHNUMHC (1 3)
           MOVE    COMB-KOHID (3)      TO  COLDAT01-KOHIDHC  (1 3)
           MOVE    COMB-KOHHKNNUM (4)  TO  COLDAT01-KOHNUMHC (1 4)
           MOVE    COMB-KOHID (4)      TO  COLDAT01-KOHIDHC  (1 4)
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "2"
                   MOVE    ACCT-SRYKA      TO  COLDAT01-SRYKA      (1)
           END-EVALUATE
      *
           MOVE    WRK-TEISEIKBN   TO  COLDAT01-TEISEIKBN
      *
           MOVE    COLDAT01-REC    TO  WRK-COLDAT01-OCC (IDXX)
           .
       20011-COLDAT01-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更処理
      *****************************************************************
       20011-COLDAT01-REWRITE-SEC            SECTION.
      *
           MOVE    WRK-COLDAT01-OCC (IDXX) TO  COLDAT01-REC
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
      *    主保険情報 
           IF      COLDAT01-HKNNUM     NOT =   SPACE
             EVALUATE    COLDAT01-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
               WHEN    WRK-CONS-JIBAI-HKNNUM
                 CONTINUE
               WHEN    OTHER
                 MOVE    1                       TO  FLG-LASTHKNID
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       5
                     IF    ( COLDAT01-TTEKSTYMD (IDX)
                                       =   PTHKN-TEKSTYMD )
                     AND   ( COLDAT01-TTEKEDYMD (IDX)
                                       =   PTHKN-TEKEDYMD )
                         MOVE    5              TO  IDX
                     ELSE
                         IF      COLDAT01-TTEKSTYMD (IDX)  =   SPACE
                             MOVE    PTHKN-HKNID
                                           TO  COLDAT01-THKNID    (IDX)
                             MOVE    PTHKN-TEKSTYMD
                                           TO  COLDAT01-TTEKSTYMD (IDX)
                             MOVE    PTHKN-TEKEDYMD
                                           TO  COLDAT01-TTEKEDYMD (IDX)
                             MOVE    PTHKN-CONTKBN
                                           TO  COLDAT01-TCONTKBN  (IDX)
                             MOVE    PTHKN-HOJOKBN
                                           TO  COLDAT01-THOJOKBN  (IDX)
                             IF      PTHKN-HKNNUM    =   "002"
                                 IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                                 OR    ( AGECHK-NENREI <=  74          )
                                     CONTINUE
                                 ELSE
                                     EVALUATE    PTHKN-HOJOKBN
                                       WHEN    "A"
                                       WHEN    "G"
                                         MOVE    "1"
                                           TO  COLDAT01-THOJOKBN (IDX)
                                       WHEN    "B"
                                       WHEN    "H"
                                         MOVE    "2"
                                           TO  COLDAT01-THOJOKBN (IDX)
                                       WHEN    "C"
                                       WHEN    "I"
                                         MOVE    "3"
                                           TO  COLDAT01-THOJOKBN (IDX)
                                     END-EVALUATE
                                 END-IF
                             END-IF
                             MOVE    PTHKN-HONKZKKBN
                                           TO  COLDAT01-THONKZKKBN(IDX)
                             MOVE    5     TO  IDX
                         ELSE
                             IF      COLDAT01-TTEKSTYMD (IDX)
                                                   >=  PTHKN-TEKSTYMD
                                 MOVE    ZERO   TO  FLG-LASTHKNID
                             END-IF
                         END-IF
                     END-IF
                 END-PERFORM
      *
                 IF      FLG-LASTHKNID       =   1
                     MOVE    PTHKN-HKNID         TO  COLDAT01-HKNID
                 END-IF
             END-EVALUATE
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      COLDAT01-HKNCOMBI    (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       COLDAT01-HKNCOMBI   (IDX)
                   MOVE    COMB-HKNNUM         TO
                                       COLDAT01-HKNNUMHC   (IDX)
                   MOVE    COMB-KOHHKNNUM (1)  TO
                                       COLDAT01-KOHNUMHC (IDX 1)
                   MOVE    COMB-KOHID (1)      TO
                                       COLDAT01-KOHIDHC  (IDX 1)
                   MOVE    COMB-KOHHKNNUM (2)  TO
                                       COLDAT01-KOHNUMHC (IDX 2)
                   MOVE    COMB-KOHID (2)      TO
                                       COLDAT01-KOHIDHC  (IDX 2)
                   MOVE    COMB-KOHHKNNUM (3)  TO
                                       COLDAT01-KOHNUMHC (IDX 3)
                   MOVE    COMB-KOHID (3)      TO
                                       COLDAT01-KOHIDHC  (IDX 3)
                   MOVE    COMB-KOHHKNNUM (4)  TO
                                       COLDAT01-KOHNUMHC (IDX 4)
                   MOVE    COMB-KOHID (4)      TO
                                       COLDAT01-KOHIDHC  (IDX 4)
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       COLDAT01-HKNCOMBI   (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "2"
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       20
                       IF      COLDAT01-SRYKA    (IDX) =   SPACE
                           MOVE    ACCT-SRYKA  TO  COLDAT01-SRYKA (IDX)
                           MOVE    20          TO  IDX
                       ELSE
                           IF      COLDAT01-SRYKA  (IDX)
                                                       =   ACCT-SRYKA
                               MOVE    20          TO  IDX
                           END-IF
                       END-IF
                   END-PERFORM
           END-EVALUATE
      *
           IF      WRK-TEISEIKBN           =   "1"
               MOVE    "1"                 TO  COLDAT01-TEISEIKBN
           END-IF
      *
           MOVE    COLDAT01-REC    TO  WRK-COLDAT01-OCC (IDXX)
           .
       20011-COLDAT01-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費番号チェック処理
      *****************************************************************
       20012-KOHID-CHECK-SEC              SECTION.
      *
      *    老人保険以外の公費情報を編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   4
                   OR      COMB-KOHID (IDX)    =   ZERO
               IF      COMB-KOHHKNNUM (IDX)    =   "027"
                   CONTINUE
               ELSE
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY         >   10
                       IF      COMB-KOHID (IDX)
                                           =   COLDAT01-TKOHID (IDY)
                           MOVE    10              TO  IDY
                       ELSE
                           IF      COLDAT01-TKOHID (IDY)   =   ZERO
                               PERFORM 200121-KOHID-HENSYU-SEC
                               MOVE    10              TO  IDY
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20012-KOHID-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費番号編集処理
      *****************************************************************
       200121-KOHID-HENSYU-SEC                SECTION.
      *
           MOVE    COMB-KOHID (IDX)    TO  COLDAT01-TKOHID    (IDY)
           MOVE    COMB-KOHHKNNUM(IDX) TO  COLDAT01-TKOHHKNNUM(IDY)
           MOVE    COMB-KOHPAYKBN(IDX) TO  COLDAT01-TKOHPAYKBN(IDY)
      *
grpsys     MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    COLDAT01-PTID       TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDX)    TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTKOHINF-READ-SEC
           IF       FLG-PTKOHINF       =   ZERO
               MOVE   PTKOH-TEKSTYMD   TO  COLDAT01-TKOHTEKSTYMD (IDY)
               MOVE   PTKOH-TEKEDYMD   TO  COLDAT01-TKOHTEKEDYMD (IDY)
      *        法別番号のセット
               INITIALIZE                 HKNNUM-REC
grpsys         MOVE    SPA-HOSPNUM     TO HKN-HOSPNUM
               MOVE    PTKOH-KOHNUM    TO HKN-HKNNUM
               MOVE    PTKOH-PAYKBN    TO HKN-PAYKBN   
               MOVE    WRK-SYMD        TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
               MOVE    HKNNUM-REC      TO  MCPDATA-REC
               PERFORM 900-HKNNUM-CHK-SEC
               IF      FLG-HKNNUM      =   ZERO
                    MOVE   HKN-HBTNUM      TO  COLDAT01-TKOHHBTNUM (IDY)
               END-IF
      *        地方公費のときレセプト請求区分を取得
               IF    ( COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8" )
               OR    ( COMB-KOHHKNNUM (IDX)(1:2)
                                           =  "99"             )
                   EVALUATE    COMB-HKNNUM
                       WHEN    "060"
                       WHEN    "067"
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                                                               OR  "5"
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                       WHEN    "039"
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                                                               OR  "4"
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                       WHEN    OTHER
                           IF      HKN-RECESKYKBN      =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                   END-EVALUATE
                   IF     COLDAT01-TRECESKYKBN (IDY)   =   1
      *                地方公費レセ記載判定
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    WRK-PARA-NYUGAIKBN
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-PARA-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
                                               SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE       "2"  TO  COLDAT01-TRECESKYKBN(IDY)
                       END-IF
                   END-IF
               ELSE
      *            全国公費のとき（長期）
                   IF      HKN-HKNKOHSBTKBN    =   "2" 
                       GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            全国公費のとき（生保）
      **>          IF   COMB-KOHHKNNUM (IDX)   =   "012" OR "021"
      *                レセ記載判定
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    WRK-PARA-NYUGAIKBN
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-PARA-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
                                               SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE    "2"     TO  COLDAT01-TRECESKYKBN(IDY)
                       ELSE
                           MOVE    "1"     TO  COLDAT01-TRECESKYKBN(IDY)
                       END-IF
      **>              GO  TO  200121-KOHID-HENSYU-EXT
      **>          END-IF
      *            全国公費のとき（老人原爆）
                   IF    ( COLDAT01-RJNHKNNUM
                                       NOT =   SPACE                   )
                   AND   ( COMB-KOHHKNNUM (IDX)
                                           =   WRK-CONS-GENBAKU-HKNNUM )
                   AND   ( COLDAT01-TRECESKYKBN(IDY)  =  "1"           )
                       IF      COLDAT01-HKNNUM =   "060"   OR  "067"
                           IF      SYS-2005-RJNGNBKKBN =   "1" OR  ZERO
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                       ELSE                
                           IF      SYS-2005-RJNGNBKKBN =   "2" OR  ZERO
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                       END-IF
      **>              GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            全国公費のとき（長期・老人原爆・生保・精神以外）
      **>          MOVE    "1"         TO  COLDAT01-TRECESKYKBN(IDY)
               END-IF
           END-IF
           .
       200121-KOHID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    福岡県公費チェック処理
      *****************************************************************
       20011-PREFKBN-40-CHECK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-PREFKBN
                                           WRK-PREFNUM
      *
      *    県外扱いの保険者はレセプトへ記載しない
           IF      PTHKN-HKNJANUM (1:2)    =   "67"
               MOVE    PTHKN-HKNJANUM (3:2)
                                       TO  WRK-PREFNUM
           ELSE
               MOVE    PTHKN-HKNJANUM (1:2)
                                       TO  WRK-PREFNUM
           END-IF
           IF      WRK-PREFNUM         =   SYS-1001-PREFNUM
               MOVE    "1"                 TO  WRK-PREFKBN
           ELSE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       5
                       OR      WRK-1033-STYUKYMD (IDX2) =   SPACE
                   IF      WRK-1033-STYUKYMD   (IDX2)  <=  WRK-SYMD
                   AND     WRK-1033-EDYUKYMD   (IDX2)  >=  WRK-SYMD
                       PERFORM VARYING IDX3 FROM    1   BY  1
                               UNTIL   IDX3 >       20
                               OR      WRK-1033-HKNJANUM (IDX2 IDX3)
                                                   =   SPACE
                               OR      WRK-PREFKBN =   "1"
                           IF      PTHKN-HKNJANUM =
                                           WRK-1033-HKNJANUM (IDX2 IDX3)
                               MOVE    "1"     TO  WRK-PREFKBN
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    県内扱いの場合、公費付加情報よりレセプト請求区分を取得
           IF      WRK-PREFKBN         =   SPACE
               MOVE    "2"                 TO  COLDAT01-TRECESKYKBN(IDY)
           ELSE
               INITIALIZE                  ORCSKOHPLUS-AREA
               MOVE    COMB-HOSPNUM        TO  LNK-SKOHPLUS-HOSPNUM
               MOVE    SYS-1001-PREFNUM    TO  LNK-SKOHPLUS-PREFNUM
               MOVE    COMB-PTID           TO  LNK-SKOHPLUS-PTID
               MOVE    COMB-HKNCOMBINUM    TO  LNK-SKOHPLUS-HKNCOMBI
               MOVE    WRK-SYMD            TO  LNK-SKOHPLUS-SRYYMD
               CALL    "ORCSKOHPLUS"       USING
                                           ORCSKOHPLUS-AREA
                                           SPA-AREA
               IF      LNK-SKOHPLUS-RC     =   ZERO
                   IF      LNK-SKOHPLUS-RECESKYKBN =   "1" OR  "3"
                       MOVE    "2"         TO  COLDAT01-TRECESKYKBN(IDY)
                   ELSE
                       MOVE    "1"         TO  COLDAT01-TRECESKYKBN(IDY)
                   END-IF
               ELSE
                   MOVE    "1"         TO  COLDAT01-TRECESKYKBN (IDY)
               END-IF
           END-IF
      *
           .
       20011-PREFKBN-40-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数編集
      *****************************************************************
       20012-JNISSU-SET-SEC        SECTION.
      *
      *実日数から減する
           PERFORM    VARYING    JN-IDX    FROM   1   BY   1
                       UNTIL     JN-IDX    >      5
             PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                         UNTIL     ACCT-IDX    >      31
                IF  (TBL-JNISSU-DAY(JN-IDX ACCT-IDX)      NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(JN-IDX ACCT-IDX)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU-DAY(JN-IDX ACCT-IDX)
                END-IF
             END-PERFORM
           END-PERFORM
      *
      *実日数編集
      *    主保険
           PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                      UNTIL      ACCT-IDX    >     31
                    IF  ( TBL-JNISSU-DAY(1 ACCT-IDX)
                                        NOT =  SPACE )
                    AND ( TBL-JNISSU-DAY-S(1 ACCT-IDX)
                                            =  SPACE )
                          ADD    1      TO    COLDAT01-HKNJNISSU
                    END-IF
                    ADD   TBL-SKJNISSU(1 ACCT-IDX)
                                        TO    COLDAT01-HKNSKJNISSU
           END-PERFORM
      *    公費
           PERFORM    VARYING    JN-IDX    FROM   2   BY   1
                      UNTIL      JN-IDX    >      5
               PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                          UNTIL      ACCT-IDX    >     31
                    IF  ( TBL-JNISSU-DAY(JN-IDX ACCT-IDX)
                                        NOT =  SPACE )
                    AND ( TBL-JNISSU-DAY-S(JN-IDX ACCT-IDX)
                                            =  SPACE )
                          ADD    1      TO
                                       COLDAT01-TKOHJNISSU(JN-IDX - 1)
                    END-IF
                    ADD   TBL-SKJNISSU(JN-IDX ACCT-IDX)
                                        TO
                                     COLDAT01-TKOHSKJNISSU(JN-IDX - 1)
               END-PERFORM
           END-PERFORM
           .
       20012-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計点数編集
      *****************************************************************
       20013-TOTALTENSU-SET-SEC    SECTION.
      *
      *    主保険
           COMPUTE COLDAT01-HKNTOTALTENSU  =   COLDAT01-HKNTOTALTENSU
                                           +   TBL-TOTALTENSU (1)
           IF    WRK-TOTALTENSU (1)        NOT =   ZERO
               COMPUTE COLDAT01-HKNTOTALTENSU = COLDAT01-HKNTOTALTENSU
                                           -   WRK-TOTALTENSU (1)
           END-IF
           COMPUTE COLDAT01-HKNSKJRYOYOHI  =   COLDAT01-HKNSKJRYOYOHI
                                           +   TBL-SKJRYOYOHI (1)
           COMPUTE COLDAT01-HKNSKJFTN      =   COLDAT01-HKNSKJFTN
                                           +   TBL-SKJFTN     (1)
      *    公費
           PERFORM    VARYING    JN-IDX    FROM   2   BY   1
                      UNTIL      JN-IDX    >      5
               COMPUTE COLDAT01-TKOHTOTALTENSU(JN-IDX - 1) =
                       COLDAT01-TKOHTOTALTENSU(JN-IDX - 1)
                                           +   TBL-TOTALTENSU (JN-IDX)
               IF    WRK-TOTALTENSU (JN-IDX)    NOT =   ZERO
                 COMPUTE COLDAT01-TKOHTOTALTENSU(JN-IDX - 1) =
                         COLDAT01-TKOHTOTALTENSU(JN-IDX - 1)
                                           -  WRK-TOTALTENSU (JN-IDX)
               END-IF
               COMPUTE COLDAT01-TKOHSKJRYOYOHI(JN-IDX - 1) =
                       COLDAT01-TKOHSKJRYOYOHI(JN-IDX - 1)
                                           +   TBL-SKJRYOYOHI (JN-IDX)
               COMPUTE COLDAT01-TKOHSKJFTN(JN-IDX - 1)     =
                       COLDAT01-TKOHSKJFTN(JN-IDX - 1)
                                           +   TBL-SKJFTN (JN-IDX)
           END-PERFORM
      *    テーブルレコード入れ替え
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       100
               IF  WRK-COLDAT01-KEY (IDXX) =
                                           COLDAT01-KEY
                   MOVE    COLDAT01-REC    TO  WRK-COLDAT01-OCC (IDXX)
                   MOVE    100             TO  IDXX
               END-IF
           END-PERFORM
           .
       20013-TOTALTENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本ファイル出力処理
      *****************************************************************
       2002-KIHON-WRITE-SEC              SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-COLDAT01-TABLE-MAX
                   OR      WRK-COLDAT01-HOSPID (IDXX)  =   SPACE
               MOVE    WRK-COLDAT01-OCC (IDXX)     TO  COLDAT01-REC
      *
      *        審査支払機関
               IF      COLDAT01-HKNNUM =  "060" OR "067" OR
                                          "068" OR "069"
                   MOVE    2            TO  COLDAT01-TEISYUTUSAKI
               ELSE
                   IF      COLDAT01-HKNNUM =  "039" OR "040"
                       MOVE    3            TO  COLDAT01-TEISYUTUSAKI
                   ELSE
                       MOVE    1            TO  COLDAT01-TEISYUTUSAKI
                   END-IF
               END-IF
      *        レセプト種別
               PERFORM 20021-RECESYUBETU-SET-SEC
      *
               IF      WRK-PARA-NYUGAIKBN   =  "1"
                   MOVE    WRK-NYUINYMD TO  COLDAT01-NYUINYMD
                   MOVE    WRK-TAIINYMD TO  COLDAT01-TAIINYMD
               ELSE
                   MOVE    SPACE        TO  COLDAT01-NYUINYMD
                   MOVE    SPACE        TO  COLDAT01-TAIINYMD
               END-IF
      *
               WRITE   COLDAT01-REC
               ADD     1                    TO  CNT-COLDAT01
           END-PERFORM
      *
      *    病名ファイル出力処理
           PERFORM 20022-BYOMEI-HENSYU-SEC
           .
       2002-RECESYUBETU-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       20021-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      COLDAT01-TEISEIKBN  =   "1"
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *    包括のとき
           IF      COLDAT01-HKNJANUM   =   "9999999"
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    労災・自賠責のとき
           IF      COLDAT01-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM 
               EVALUATE    COLDAT01-HKNKBN
                                       ALSO    COLDAT01-SAIGAIKBN
                                           ALSO    WRK-PARA-NYUGAIKBN
                   WHEN    "1"         ALSO    "1"     ALSO    "1"
                       MOVE    7081        TO    COLDAT01-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "1"
                       MOVE    7082        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "1"
                       MOVE    7101        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "1"
                       MOVE    7102        TO    COLDAT01-RECESYUBETU
                   WHEN    "1"         ALSO    "1"     ALSO    "2"
                       MOVE    7071        TO    COLDAT01-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "2"
                       MOVE    7072        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "2"
                       MOVE    7091        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "2"
                       MOVE    7092        TO    COLDAT01-RECESYUBETU
                   WHEN    "3"         ALSO    ANY     ALSO    ANY
                       MOVE    7021        TO    COLDAT01-RECESYUBETU
                   WHEN    "4"         ALSO    ANY     ALSO    ANY
                       MOVE    7001        TO    COLDAT01-RECESYUBETU
      *            公務災害
                   WHEN    "5"         ALSO    ANY     ALSO    "1"
                       MOVE    7061        TO    COLDAT01-RECESYUBETU
                   WHEN    "5"         ALSO    ANY     ALSO    "2"
                       MOVE    7051        TO    COLDAT01-RECESYUBETU
               END-EVALUATE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    自費のとき
           IF     COLDAT01-HKNKOHSBTKBN
                                       =   "8"
               MOVE    8888                TO  COLDAT01-RECESYUBETU
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    治験のとき
           IF     (COLDAT01-HKNNUM    >=  "900")    AND
                  (COLDAT01-HKNNUM    <=  "919")
               MOVE    8888                TO  COLDAT01-RECESYUBETU
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    公害保険のとき
           IF      COLDAT01-HKNNUM     =   WRK-CONS-KOUGAI-HKNNUM
               EVALUATE    WRK-PARA-NYUGAIKBN
                   WHEN    "1"
                       MOVE    7210        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"
                       MOVE    7200        TO    COLDAT01-RECESYUBETU
               END-EVALUATE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
           IF      COLDAT01-HKNNUM     NOT =   SPACE
               EVALUATE    COLDAT01-HKNNUM
                   WHEN    "060"
                   WHEN    "068"
                       MOVE   "1"          TO  WK-KOKUHO
                   WHEN    "067"
                   WHEN    "069"
                       MOVE   "2"          TO  WK-KOKUHO
                   WHEN    "039"
                   WHEN    "040"
                       MOVE   "3"          TO  WK-KOKUHO
                   WHEN    OTHER
                       MOVE    "1"         TO  WK-IHO
               END-EVALUATE
           END-IF
      *
           IF      WRK-PARA-SRYYM     <      "200210"
             IF      COLDAT01-RJNHKNNUM     =   "027"
                 MOVE        "1"          TO  WK-RJN
             END-IF
           END-IF
      *
      *****MOVE    COLDAT01-HONKZKKBN  TO  WK-HONKZKKBN
      *
           MOVE    ZERO                TO  WK-KOHI-CNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      COLDAT01-TKOHHKNNUM (IDX)   =   SPACE
               IF      COLDAT01-TRECESKYKBN (IDX)  =   "1"
                   ADD     1               TO  WK-KOHI-CNT
               END-IF
           END-PERFORM
      *
           IF  COLDAT01-HKNNUM     NOT =   SPACE
      *
      *        主保険最新情報
               MOVE    1               TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      COLDAT01-TTEKSTYMD (IDX)  =   SPACE
                   IF      COLDAT01-TTEKSTYMD (IDX)
                                           >   COLDAT01-TTEKSTYMD (IDY)
                       MOVE    IDX             TO  IDY
                   END-IF
               END-PERFORM
               MOVE    COLDAT01-THONKZKKBN (IDY)   TO  WK-HONKZKKBN
      *
               IF    ( WRK-PARA-SRYYM      >=  "200210"              )
               AND   ( WRK-PARA-SRYYM      <   WRK-CONS-SRYYM-200804 )
      *            ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
                   IF      COLDAT01-AGE         <     3
                               MOVE   "3"     TO   WK-HONKZKKBN
                   END-IF
               END-IF
      *        平成２０年４月改正対応
               IF      WRK-PARA-SRYYM      >=  WRK-CONS-SRYYM-200804
                   IF      COLDAT01-SYUGAKUKBN =   1
                       MOVE   "3"          TO  WK-HONKZKKBN
                   END-IF
               END-IF
      *
      *        後期高齢非該当の有無設定
               MOVE    ZERO            TO  WRK-KOUKI-FLG
      * 
               IF    ( COLDAT01-AGE    >=  70                    )
               AND   ( WRK-PARA-SRYYM  >=  WRK-CONS-SRYYM-200804 )
                   IF    ( COLDAT01-HKNNUM     =   SPACE   OR  "040"   
                                               OR  "039"   OR  "067"
                                               OR  "069"             )
                   OR    ( COLDAT01-HKNNUM (1:2)
                                               =   "98"    OR  "90"
                                               OR  "91"              )
                       CONTINUE
                   ELSE
                       IF   (  COLDAT01-HKNNUM =   "060"   OR  "068" )
                       OR   (( COLDAT01-HKNNUM
                                           NOT =   "060"   AND "068" )
                               AND
                             ( COLDAT01-THOJOKBN (IDY)
                                               =   SPACE             ))
                           MOVE    WRK-PARA-SRYYM  TO  WRK-TEKSTYYMM
                           IF      COLDAT01-TTEKSTYMD (IDY) (1:6)  >=
                                               WRK-PARA-SRYYM
                                MOVE  COLDAT01-TTEKSTYMD (IDY)(7:2)
                                                       TO  WRK-TEKSTDD
                           ELSE
                                MOVE  01               TO  WRK-TEKSTDD
                           END-IF
      *
                           INITIALIZE                      PTKOHINF-REC
                           MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
                           MOVE    COLDAT01-PTID       TO  PTKOH-PTID
                           MOVE    "976"               TO  PTKOH-KOHNUM
                           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key4"              TO  MCP-PATHNAME
                           PERFORM 900-DBSELECT-SEC
                           IF      MCP-RC              =   ZERO
                               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                               MOVE    "key4"          TO  MCP-PATHNAME
                               PERFORM 900-PTKOHINF-READ-N-SEC
                               PERFORM UNTIL   FLG-PTKOHINF    =   1
                                   IF    (PTKOH-TEKSTYMD
                                                       <=  WRK-TEKSTYMD)
                                   AND   (WRK-TEKSTYMD <=
                                                       PTKOH-TEKEDYMD  )
                                       MOVE    1       TO  WRK-KOUKI-FLG
                                       MOVE    1       TO  FLG-PTKOHINF
                                   ELSE
                                       MOVE    "tbl_ptkohinf"
                                                        TO  MCP-TABLE
                                       MOVE    "key4"   TO  MCP-PATHNAME
                                       PERFORM  900-PTKOHINF-READ-N-SEC
                                   END-IF
                               END-PERFORM
                           END-IF
                           MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                           MOVE    "key4"               TO  MCP-PATHNAME
                           PERFORM  900-DBCLOSE-SEC
                       END-IF
                   END-IF
               END-IF
      *
               IF      WRK-PARA-SRYYM  >=  WRK-CONS-SRYYM-200804
                   EVALUATE    COLDAT01-HKNNUM
                       WHEN    "002"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "B"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060"
                       WHEN    "068"
      *********************IF  (COLDAT01-AGE  >=   70)  AND
      *********************    (COLDAT01-AGE  <=   74)
                           IF  (COLDAT01-AGE  >=   70)
                           AND (WRK-KOUKI-FLG =    0 )
                             EVALUATE    WRK-PARA-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   COLDAT01-THOJOKBN (IDY)
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   COLDAT01-THOJOKBN (IDY)
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF
                       WHEN    "039"
                       WHEN    "040"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067"
                       WHEN    "069"
                           IF      COLDAT01-AGE  >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                               EVALUATE   COLDAT01-THOJOKBN (IDY)
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "?"   TO   WK-HONKZKKBN
                                   WHEN   "7"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF
                       WHEN    OTHER
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
               END-IF
      *
               IF    ( WRK-PARA-SRYYM      >=  "200610" )
               AND   ( WRK-PARA-SRYYM      <   WRK-CONS-SRYYM-200804 )
                   EVALUATE    COLDAT01-HKNNUM
                       WHEN    "002"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "B"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060"
                       WHEN    "068"
                           IF  (COLDAT01-AGE  >=   70)  AND
                               (COLDAT01-AGE  <=   74)
                             EVALUATE    WRK-PARA-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   COLDAT01-THOJOKBN (IDY)
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   COLDAT01-THOJOKBN (IDY)
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF
                       WHEN    OTHER
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      COLDAT01-RJNHKNNUM      =    "027"
                       EVALUATE   COLDAT01-RJNPAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN
                           WHEN   "02"
                               MOVE   "?"   TO   WK-RJN
                           WHEN   "03"
                               MOVE   "7"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
      *
               IF    ( WRK-PARA-SRYYM      >=  "200210" )
               AND   ( WRK-PARA-SRYYM      <=  "200609" )
      *
      *            １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *            "９"または"８"をセット
      *            老人保険の場合はＷＫ−ＲＪＮにセットする	
                   EVALUATE    COLDAT01-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF  (COLDAT01-AGE  >=   70)  AND
                               (COLDAT01-AGE  <=   74)
                               EVALUATE   COLDAT01-THOJOKBN (IDY)
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      COLDAT01-RJNHKNNUM      =    "027"
                       EVALUATE   COLDAT01-RJNPAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )
           AND   ( COLDAT01-THONKZKKBN (IDY)
                                       =   "1"   )
               IF      COLDAT01-HKNNUM =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *
      *     DISPLAY "HONKZKKBN : " WK-HONKZKKBN 
      *     DISPLAY "NYUGAIKBN : " COLDAT01-NYUGAIKBN 
      *     DISPLAY "IHO       : " WK-IHO 
      *     DISPLAY "KOKUHO    : " WK-KOKUHO 
      *     DISPLAY "RJN       : " WK-RJN 
      *     DISPLAY "KOHI-CNT  : " WK-KOHI-CNT 
      *
           INITIALIZE                  ORCSSYUBETUGETAREA  
           MOVE    WK-KOKUHO           TO  LNK-SYUBETUGET-KOKUHO
           MOVE    WK-IHO              TO  LNK-SYUBETUGET-IHO
           MOVE    WK-RJN              TO  LNK-SYUBETUGET-RJN
           MOVE    WK-KOHI-CNT         TO  LNK-SYUBETUGET-KOHI-CNT
           MOVE    WK-HONKZKKBN        TO  LNK-SYUBETUGET-HONKZKKBN
           MOVE    COLDAT01-NYUGAIKBN  TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM      TO  LNK-SYUBETUGET-SRYYM
           MOVE    COLDAT01-HKNNUM     TO  LNK-SYUBETUGET-HKNNUM
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                       TO  COLDAT01-RECESYUBETU
      *
           .
        20021-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名情報出力処理
      *****************************************************************
       20022-BYOMEI-HENSYU-SEC              SECTION.
      *
      *    患者病名読み込み
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    PTINF-PTID          TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL       FLG-PTBYOMEI     =   1
               PERFORM 200221-BYOMEI-WRITE-SEC
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       20022-BYOMEI-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    患者病名出力処理
      *****************************************************************
       200221-BYOMEI-WRITE-SEC         SECTION.
      *
      *    診療開始日
           IF      PTBYO-SRYYMD(1:6)    =   WRK-PARA-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    <   WRK-PARA-SRYYM
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIYMD(1:6)  >=  WRK-PARA-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  200221-BYOMEI-WRITE-EXT
                       END-IF
                   END-IF
               ELSE
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    入外区分
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   WRK-PARA-NYUGAIKBN
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    "999999"のコードを読み飛ばす
           INITIALIZE                  WRK-PTBYOMEI-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
                   OR      PTBYO-BYOMEICD (IDX)    =   SPACE
               IF      PTBYO-BYOMEICD (IDX)    =   ALL "9"
                   CONTINUE
               ELSE
                   ADD     1               TO  WRK-PTBYO-BYOMEICDSU
                                               IDY
                   MOVE    PTBYO-BYOMEICD (IDX)
                                           TO  WRK-PTBYO-BYOMEICD (IDY)
               END-IF
           END-PERFORM
           MOVE    WRK-PTBYO-BYOMEICD-G
                                       TO  PTBYO-BYOMEICD-G
           MOVE    WRK-PTBYO-BYOMEICDSU
                                       TO  PTBYO-BYOMEICDSU    
      *
      *    疑いフラグより病名を編集
           IF      PTBYO-UTAGAIFLG NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
               MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
               MOVE    PTBYO-BYOMEIHENFLG  TO  SMKBYO-BYOMEIHENFLG
               MOVE    PTBYO-BYOMEICD-G    TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
               COMPUTE PTBYO-BYOMEIMOJI    =   SMKBYO-LENGTH   /   2
               MOVE    SMKBYO-BYOMEI       TO  PTBYO-BYOMEI
               MOVE    SMKBYO-BYOMEIHENFLG TO  PTBYO-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  PTBYO-BYOMEICD-G
           END-IF
      *
           INITIALIZE                      COLDAT03-REC
           MOVE    COLDAT01-HOSPID     TO  COLDAT03-HOSPID
           MOVE    COLDAT01-SRYYM      TO  COLDAT03-SRYYM
           MOVE    COLDAT01-NYUGAIKBN  TO  COLDAT03-NYUGAIKBN
           MOVE    COLDAT01-PTID       TO  COLDAT03-PTID
           MOVE    COLDAT01-RECEKA     TO  COLDAT03-RECEKA
           MOVE    PTBYO-SRYKA         TO  COLDAT03-SRYKA
           MOVE    PTBYO-SRYYMD        TO  COLDAT03-SRYYMD
           MOVE    PTBYO-RENNUM        TO  COLDAT03-RENNUM
           MOVE    PTBYO-BYOMEI        TO  COLDAT03-BYOMEI
           MOVE    PTBYO-BYOMEIMOJI    TO  COLDAT03-BYOMEIMOJI
           MOVE    PTBYO-UTAGAIFLG     TO  COLDAT03-UTAGAIFLG
           MOVE    PTBYO-SYUBYOFLG     TO  COLDAT03-SYUBYOFLG   
           MOVE    PTBYO-MANSEIKBN     TO  COLDAT03-MANSEIKBN
           MOVE    PTBYO-HKNCOMBI      TO  COLDAT03-HKNCOMBI
           MOVE    PTBYO-REZEFLG       TO  COLDAT03-REZEFLG
           IF      WRK-PARA-SRYYM       <   PTBYO-TENKIYMD(1:6)
               CONTINUE
           ELSE
               MOVE    PTBYO-TENKIKBN      TO  COLDAT03-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  COLDAT03-TENKIYMD
           END-IF
           MOVE    PTBYO-HKNBYOMEIFLG  TO  COLDAT03-HKNBYOMEIFLG
           MOVE    PTBYO-BYOMEIHENFLG  TO  COLDAT03-BYOMEIHENFLG
           MOVE    PTBYO-BYOMEICD-G    TO  COLDAT03-BYOMEICD-G
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
                   OR      PTBYO-BYOMEICD (IDX)    =   SPACE
               MOVE    IDX                 TO  COLDAT03-BYOMEICDSU
           END-PERFORM
      *    レセプト診療科
           IF      PTBYO-SRYKA     NOT =   SPACE
               INITIALIZE                      SYS-1005-REC
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE     PTBYO-SRYKA        TO  SYS-1005-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
               MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ELSE
                   INITIALIZE                      SYS-1005-REC
               END-IF
               MOVE    SYS-1005-RECESRYKA  TO  COLDAT03-RECESRYKA
           END-IF
      *
           WRITE   COLDAT03-REC
           ADD     1                   TO  CNT-COLDAT03
           .
       200221-BYOMEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計・診療行為データ編集処理
      *****************************************************************
       2003-SRYACCT-HENSYU-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  FLG-SHOSAISIN
           MOVE    ZERO                TO  FLG-SHOSAISIN-GEN
      *
      *    レセプト診療科
           INITIALIZE                      SYS-1005-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    ACCT-SRYKA          TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
           ELSE
               INITIALIZE                      SYS-1005-REC
           END-IF
      *
      *    診療行為データ読み込み
           INITIALIZE                      SRYACT-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACT    =    1
               PERFORM 20031-SRYACT-HENSYU-SEC
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           PERFORM 200312-TOTALTEN-TAIHI-SEC
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       2003-SRYACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報出力処理
      *****************************************************************
       20031-SRYACT-HENSYU-SEC               SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >   5
                   OR      SRY-SRYCD (IDX2)    =   SPACE
      *
               MOVE    SPACE               TO  COLDAT02-REC
               INITIALIZE                      COLDAT02-REC
      *
      *        診療会計データ編集
               MOVE    COLDAT01-KEY        TO  COLDAT02-KEY
               MOVE    ACCT-ZAINUM         TO  COLDAT02-ZAINUM
      *
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  COLDAT02-SEQNO
               MOVE    COMB-HKNID          TO  COLDAT02-HKNID
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       4
                   MOVE    COMB-KOHID (IDX3)   TO  COLDAT02-TKOHID(IDX3)
               END-PERFORM
               MOVE    ACCT-SRYKA          TO  COLDAT02-SRYKA
               MOVE    SYS-1005-RECESRYKA  TO  COLDAT02-RECESRYKA
               MOVE    ACCT-SRYKBN         TO  COLDAT02-SRYKBN
               MOVE    ACCT-JIHOKBN        TO  COLDAT02-JIHOKBN
               MOVE    ACCT-ZAITEN         TO  COLDAT02-ZAITEN
               MOVE    ACCT-ZAIKAISU       TO  COLDAT02-ZAIKAISU
               MOVE    ACCT-MEISAISU       TO  COLDAT02-MEISAISU
               MOVE    ACCT-SYUTEN1        TO  COLDAT02-SYUTEN1
               MOVE    ACCT-SYUTEN2        TO  COLDAT02-SYUTEN2
               MOVE    ACCT-YKZTEN         TO  COLDAT02-YKZTEN
               MOVE    ACCT-KIZAITEN       TO  COLDAT02-KIZAITEN
               MOVE    ACCT-HKNCOMBI       TO  COLDAT02-HKNCOMBI
               MOVE    ACCT-DAY-TBL (1)    TO  COLDAT02-DAY-AREA
      *
      *        診療行為データ編集
               MOVE    SRY-RENNUM          TO  COLDAT02-RENNUM
               IF      SRY-SRYSYUKBN       =   SPACE
                   MOVE    ACCT-SRYKBN     TO  COLDAT02-SRYSYUKBN (1:2)
                   MOVE    "0"             TO  COLDAT02-SRYSYUKBN (3:1)
               ELSE
                   MOVE    SRY-SRYSYUKBN   TO  COLDAT02-SRYSYUKBN
               END-IF
               MOVE    SRY-JIHIMONEY(IDX2) TO  COLDAT02-JIHIMONEY
               MOVE    SRY-SRYCD    (IDX2) TO  COLDAT02-SRYCD
               MOVE    SRY-SRYSURYO (IDX2) TO  COLDAT02-SRYSURYO
               MOVE    SRY-SRYKAISU (IDX2) TO  COLDAT02-SRYKAISU
               MOVE    SRY-INPUTNUM (IDX2) TO  COLDAT02-INPUTNUM
      *
               INITIALIZE                      TNS-TENSU-REC
grpsys         MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    SRY-SRYCD(IDX2)     TO  TNS-SRYCD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TANICD          TO  COLDAT02-TANICD
               MOVE    TNS-TANINAME        TO  COLDAT02-TANINAME
               MOVE    TNS-FORMALNAME      TO  COLDAT02-FORMALNAME
               MOVE    TNS-TENSIKIBETU     TO  COLDAT02-TENSIKIBETU
               MOVE    TNS-TEN             TO  COLDAT02-TEN
               IF      TNS-SRYCD (1:1)     =   "6"
                   MOVE    TNS-YAKKAKJNCD      TO  COLDAT02-YAKKAKJNCD
               END-IF
               IF      TNS-TENSIKIBETU     =   1   OR   3
                   IF      ACCT-NYUGAIKBN      =   "1"
                       MOVE  TNS-NYUTENTTLKBN  TO  COLDAT02-TENTTLKBN
                   ELSE
                       MOVE  TNS-GAITENTTLKBN  TO  COLDAT02-TENTTLKBN
                   END-IF
               END-IF
      *
               IF (   (SRY-SRYCD    (IDX2)(1:1)
                                               =   "8"  )
                   OR (SRY-SRYCD    (IDX2)(1:3)
                                               =   "008") ) 
               AND (   SRY-INPUTNUM (IDX2) NOT =   ZERO   )
grpsys             MOVE    SPA-HOSPNUM             TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID                TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM              TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD   (IDX2)      TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)      TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT       TO  COLDAT02-NAME
               ELSE
                   MOVE    TNS-NAME                TO  COLDAT02-NAME
               END-IF
      *
               PERFORM 200311-JNISSU-TAIHI-SEC
      *
               WRITE   COLDAT02-REC
               ADD     1               TO  CNT-COLDAT02
           END-PERFORM
           .
       20031-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数退避
      *****************************************************************
       200311-JNISSU-TAIHI-SEC     SECTION.
      *
           IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                "6" OR  "7" OR
                                                "8"
      *        診療実日数のカウントを行える剤であるかの判定
               IF  ((ACCT-ZAITEN    NOT =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                    (ACCT-SRYKBN        =  "11"  OR  "12"  OR
                                           "13"  OR  "14"     ))
               OR  ((ACCT-ZAITEN        =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                    (ACCT-SRYKBN        =  "11"  OR  "12"  OR
                                           "99"               ))
      *
                       IF ( (TNS-JITUDAY    =     2          ) AND
                            (TNS-DAYCNT     =     1   OR   2 )    )
                       OR (  SRY-SRYCD(IDX2)  =  "112700850"
                                              OR "112003350"
                                              OR "099409901"
                                              OR "099999902"
                                              OR "099110001"
                                              OR "099120001"
      *                              同日再診
                                              OR "112010150"
                                              OR "112010250"
                                              OR "112010350"
                                              OR "112010450"
                                              OR "112010550"
                                              OR "112704450"
                                              OR "112704550"
                                              OR "112704650"
                                              OR "112704750"
                                              OR "112010650"
                                              OR "112010750"
                                              OR "112010850"
                                              OR "112010950"
                                              OR "112012850"
                                              OR "112704850"
                                              OR "112704950"
                                              OR "112705050"
                                              OR "112706850"
                                              OR "112008850"
                                              OR "112008950"
                                              OR "112009050"
                                              OR "112009150"
                                              OR "112012550"
                                              OR "112703450"
                                              OR "112703550"
                                              OR "112703650"
                                              OR "112706550"
                                              OR "112008350"
                                              OR "112008450"
                                              OR "112008550"
                                              OR "112008650"
                                              OR "112008750"
                                              OR "112703050"
                                              OR "112703150"
                                              OR "112703250"
                                              OR "112703350"
      *
                                              OR "112011710"
                                              OR "112011810"
                                              OR "112011910"
                                              OR "112012010"
                                              OR "112012210"
                                              OR "112705850"
                                              OR "112705950"
                                              OR "112706050"
      *
                                              OR "111011810"
                                              OR "113013910"
                                              OR "113014010"
                                              OR "113014110"
                                              OR "112015810"
                                              OR "112015950"
                                              OR "112016210"
                                              OR "101110040"
                                              OR "101120100"      )
                             MOVE    1      TO    FLG-SHOSAISIN
                       END-IF
                       IF    SRY-SRYCD(IDX2)  =  "099409903"
                             MOVE    1      TO    FLG-SHOSAISIN-GEN
                       END-IF
               END-IF
           END-IF
      *
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      10)
                      IF   COLDAT01-HKNCOMBI(IDX3) = COLDAT02-HKNCOMBI
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                      END-IF
           END-PERFORM
      *
      *    公費の分離表示用インデックスの確定
           IF    (COLDAT01-HKNNUM        NOT =   SPACE)   OR
                 (COLDAT01-RJNHKNNUM     NOT =   SPACE)
               IF   FLG-SHOSAISIN        =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                      IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                          MOVE    "1"       TO
                                        TBL-JNISSU-DAY(1 ACCT-IDX)
                      END-IF
                    END-PERFORM
               END-IF
               IF   FLG-SHOSAISIN-GEN    =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                      IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                          MOVE    "1"       TO
                                        TBL-JNISSU-DAY-GEN(1 ACCT-IDX)
                      END-IF
                    END-PERFORM
               END-IF
           END-IF
      *    インデックス２
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      4)
                     IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                      AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                           IF   FLG-SHOSAISIN      =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                            TBL-JNISSU-DAY(2 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                           IF   FLG-SHOSAISIN-GEN  =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                        TBL-JNISSU-DAY-GEN(2 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                      END-IF
                     ELSE
                      MOVE  10      TO     IDX3
                     END-IF
           END-PERFORM
      *    インデックス３
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      4)
                     IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                      AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                           IF   FLG-SHOSAISIN      =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                            TBL-JNISSU-DAY(3 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                           IF   FLG-SHOSAISIN-GEN  =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                        TBL-JNISSU-DAY-GEN(3 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                      END-IF
                     ELSE
                      MOVE  10      TO     IDX3
                     END-IF
           END-PERFORM
      *    インデックス４
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      4)
                     IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                      AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                           IF   FLG-SHOSAISIN      =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                            TBL-JNISSU-DAY(4 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                           IF   FLG-SHOSAISIN-GEN  =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                        TBL-JNISSU-DAY-GEN(4 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                      END-IF
                     ELSE
                      MOVE  10      TO     IDX3
                     END-IF
           END-PERFORM
      *    インデックス５
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      4)
                     IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                      AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                           IF   FLG-SHOSAISIN      =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                            TBL-JNISSU-DAY(5 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                           IF   FLG-SHOSAISIN-GEN  =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                        TBL-JNISSU-DAY-GEN(5 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                      END-IF
                     ELSE
                      MOVE  10      TO     IDX3
                     END-IF
           END-PERFORM
           .
       200311-JNISSU-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    トータル点編集
      *****************************************************************
       200312-TOTALTEN-TAIHI-SEC   SECTION.
      *
           IF    (COLDAT01-HKNNUM        NOT =   SPACE)   OR
                 (COLDAT01-RJNHKNNUM     NOT =   SPACE)
             IF          ACCT-SRYKBN         =   "97"
                   COMPUTE  TBL-SKJRYOYOHI (1)
                                             =  TBL-SKJRYOYOHI (1)
                                             + (ACCT-ZAITEN
                                             *  ACCT-ZAIKAISU)
             ELSE
               IF      ( ACCT-SRYKBN         =   "95"   OR  "96" )
                OR     ( COLDAT02-SRYCD      =   "099999912" )
                   CONTINUE
               ELSE
                 IF       (ACCT-SRYCDTOTAL    =    630010002)  OR
                          (ACCT-ZAIKBN        =    2)
                   IF    ACCT-ZAIKBN        =    2
                         COMPUTE    WRK-TOTALTENSU(1)   =
                                                  WRK-TOTALTENSU(1) +
                                                 (ACCT-ZAITEN  *
                                                  ACCT-ZAIKAISU)
                         MOVE     ZERO    TO   ACCT-ZAITEN
                         MOVE     ZERO    TO   ACCT-ZAIKAISU
                   END-IF                                              
                   COMPUTE  TBL-TOTALTENSU(1) =  TBL-TOTALTENSU(1) -
                                                 (ACCT-ZAITEN  *
                                                  ACCT-ZAIKAISU)
                 ELSE
                   COMPUTE  TBL-TOTALTENSU(1) =  TBL-TOTALTENSU(1) +
                                                 (ACCT-ZAITEN  *
                                                  ACCT-ZAIKAISU)
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      10)
                      IF   COLDAT01-HKNCOMBI(IDX3) = COLDAT02-HKNCOMBI
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                      END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX3    FROM   1   BY   1
             UNTIL   (IDX3      >       4)
               MOVE   ZERO               TO        KH-IDX
               IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                  IF        ACCT-SRYKBN      =   "97"
                      COMPUTE  TBL-SKJRYOYOHI (KH-IDX)
                                             =  TBL-SKJRYOYOHI (KH-IDX)
                                             + (ACCT-ZAITEN
                                             *  ACCT-ZAIKAISU)
                  ELSE
                  IF      ( ACCT-SRYKBN         =   "95"   OR  "96" )
                   OR     ( COLDAT02-SRYCD      =   "099999912" )
                     CONTINUE
                  ELSE
                     IF       (ACCT-SRYCDTOTAL    =    630010002)  OR
                              (ACCT-ZAIKBN        =    2)
                         IF    ACCT-ZAIKBN        =    2
                             COMPUTE    WRK-TOTALTENSU(KH-IDX)   =
                                              WRK-TOTALTENSU(KH-IDX) +
                                              (ACCT-ZAITEN  *
                                               ACCT-ZAIKAISU)
                             MOVE     ZERO    TO   ACCT-ZAITEN
                             MOVE     ZERO    TO   ACCT-ZAIKAISU
                         END-IF
                         COMPUTE  TBL-TOTALTENSU(KH-IDX) =
                                        TBL-TOTALTENSU(KH-IDX) -
                                             (ACCT-ZAITEN *
                                              ACCT-ZAIKAISU)
                     ELSE
                         COMPUTE  TBL-TOTALTENSU(KH-IDX) =
                                        TBL-TOTALTENSU(KH-IDX) +
                                             (ACCT-ZAITEN *
                                              ACCT-ZAIKAISU)
                     END-IF
                  END-IF
                  END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       200312-TOTALTEN-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事標準負担額編集
      *****************************************************************
       200313-SKJFTN-TAIHI-SEC     SECTION.
      *
           IF    (COLDAT01-HKNNUM        NOT =   SPACE)   OR
                 (COLDAT01-RJNHKNNUM     NOT =   SPACE)
                 COMPUTE  TBL-SKJFTN(1)      =   TBL-SKJFTN (1)
                                             +  (COLDAT02-ZAITEN
                                             *   COLDAT02-ZAIKAISU)
           END-IF
      *
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      10)
                      IF   COLDAT01-HKNCOMBI(IDX3) = COLDAT02-HKNCOMBI
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                      END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX3    FROM   1   BY   1
             UNTIL   (IDX3      >       4)
               MOVE   ZERO               TO        KH-IDX
               IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                      COMPUTE  TBL-SKJFTN(KH-IDX)
                                             =   TBL-SKJFTN (KH-IDX)
                                             +  (COLDAT02-ZAITEN
                                             *   COLDAT02-ZAIKAISU)
                 END-IF
               END-IF  
           END-PERFORM
           .
       200313-SKJFTN-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   COLDATERR-FILE
           IF      STS-COLDATERR         =   ZERO
               CLOSE   COLDATERR-FILE
           ELSE
               OPEN    OUTPUT              COLDATERR-FILE
      *
               MOVE    WRK-COLDATERR         TO  COLDATERR-REC
               WRITE   COLDATERR-REC
               CLOSE   COLDATERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-COLDATERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           PERFORM 10013-FILE-SYORI-SEC
      *
           CLOSE   COLDAT91-FILE
                   COLDAT01-FILE
                   COLDAT02-FILE
                   COLDAT03-FILE
      *
           IF      CNT-COLDAT01           =   ZERO  
               OPEN    OUTPUT              COLDAT911-FILE
               CLOSE                       COLDAT911-FILE
               MOVE    ZERO            TO  CNT-COLDAT91
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "COLDAT91  CNT-COLDAT91:" CNT-COLDAT91
           DISPLAY "COLDAT01  CNT-COLDAT01:" CNT-COLDAT01
           DISPLAY "COLDAT02  CNT-COLDAT02:" CNT-COLDAT02
           DISPLAY "COLDAT03  CNT-COLDAT03:" CNT-COLDAT03
           DISPLAY "*** ORCBC010 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-SYSKANRI
               ELSE
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ　ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴検索処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
      *
grpsys     MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    PTINF-PTID          TO  PTNYUINRRK-PTID
           MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENNYUYMD
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD
           MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENSTUYMD
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           .
       900-PTNYUINRRK-KEY41-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴FETCH処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-FET-SEC     SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           .
       900-PTNYUINRRK-KEY41-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           IF      COMB-HKNCOMBINUM =   9999
               MOVE    ZERO                TO  FLG-HKNCOMBI
               GO  TO  910-HKNCOMBI-INV-EXT
           END-IF
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC TO  HKNCOMBI-REC
                   IF      COMB-DELKBN =   "1"
                       MOVE    1       TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-HKNCOMBI
                   INITIALIZE              HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN  =   "1"
                       MOVE    1       TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTKOHINF-REC
                   MOVE    ZERO        TO  FLG-PTKOHINF
               ELSE
                   MOVE    1           TO  FLG-PTKOHINF
                   INITIALIZE              PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           MOVE    HKN-HKNNUM          TO  WRK-CHK-HKNNUM
           MOVE    HKN-PAYKBN          TO  WRK-CHK-PAYKBN
           MOVE    HKN-TEKSTYMD        TO  WRK-CHK-TEKSTYMD
           MOVE    HKN-TEKEDYMD        TO  WRK-CHK-TEKEDYMD
      *
           INITIALIZE                      HKNNUM-REC
      *
      *    保険番号マスタ退避ワークチェック
      *
           PERFORM VARYING WRK-HKNNUM-IDX    FROM    1   BY  1
                   UNTIL   WRK-HKNNUM-IDX    >   WRK-HKNNUM-MAX
                   OR      HKN-HKNNUM        NOT =   SPACE
               IF    ( WRK-CHK-HKNNUM  =
                                       TBL-HKN-HKNNUM (WRK-HKNNUM-IDX) )
               AND   ( WRK-CHK-PAYKBN  =
                                       TBL-HKN-PAYKBN (WRK-HKNNUM-IDX) )
               AND   ( TBL-HKN-TEKSTYMD (WRK-HKNNUM-IDX)
                                       <=  WRK-CHK-TEKSTYMD            )
               AND   ( TBL-HKN-TEKEDYMD(WRK-HKNNUM-IDX)
                                       >=  WRK-CHK-TEKEDYMD            )
                   MOVE    TBL-HKNNUM-REC (WRK-HKNNUM-IDX)
                                       TO  HKNNUM-REC
               END-IF
           END-PERFORM
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF      HKN-HKNNUM          =   SPACE
               INITIALIZE                      HKNNUM-REC
grpsys         MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE    WRK-CHK-HKNNUM      TO  HKN-HKNNUM
               MOVE    WRK-CHK-PAYKBN      TO  HKN-PAYKBN
               MOVE    WRK-CHK-TEKSTYMD    TO  HKN-TEKSTYMD
               MOVE    WRK-CHK-TEKEDYMD    TO  HKN-TEKEDYMD
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_hknnum"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-HKNNUM-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
      *
               IF      FLG-HKNNUM          =   ZERO
      *            退避ワークに編集
                   IF      TBL-HKNNUM-CONST-MAX
                                               >   WRK-HKNNUM-MAX
                       COMPUTE WRK-HKNNUM-MAX
                                       =   WRK-HKNNUM-MAX  +   1
                       MOVE    HKNNUM-REC      TO
                                   TBL-HKNNUM-REC (WRK-HKNNUM-MAX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBCLOSE-SEC
           END-IF
           .
       900-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   MOVE    1           TO  FLG-PTINF
                   INITIALIZE              PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptcom"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCOM-REC
                   MOVE    ZERO        TO  FLG-PTCOM
               ELSE
                   MOVE    1           TO  FLG-PTCOM
                   INITIALIZE              PTCOM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       900-TENSU-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-PARA-SRYYM      TO  TTNS-CHK-YMD (1:6)
           MOVE    "01"                TO  TTNS-CHK-YMD (7:2)
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
grpsys         MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    TTNS-CHK-SRYCD  TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOSTYMD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
               MOVE    "DBSELECT"      TO  MCP-FUNC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "DBFETCH"   TO  MCP-FUNC
                   MOVE    "tbl_tensu" TO  MCP-TABLE
                   MOVE    "key"       TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                       MOVE    ZERO            TO  FLG-TENSU
      *
      *                点数マスタ退避ワークに編集する
                       PERFORM 9001-TTNS-HEN-SEC
                   ELSE
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBCLOSE-SEC
           END-IF
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスタ読込
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN =   "1"
                       MOVE    1           TO  FLG-PTRSIINF
                   END-IF
               ELSE
                    INITIALIZE             PTRSIINF-REC
                   MOVE    1           TO  FLG-PTRSIINF
               END-IF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタＲＥＡＤ
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC 
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               IF      PTKOH-SAKJOKBN      =   "1"
                   GO  TO  900-PTKOHINF-READ-N-SEC
               END-IF
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-COLDAT01-READ-SEC             SECTION.
      *
           READ    COLDAT01-FILE
               INVALID
                   MOVE    1           TO  FLG-COLDAT01
               NOT INVALID
                   MOVE    ZERO        TO  FLG-COLDAT01
           END-READ
           .
       900-COLDAT01-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
