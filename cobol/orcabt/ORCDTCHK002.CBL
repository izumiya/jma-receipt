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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCDTCHK002.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 対象患者抽出
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/11/21    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.12    NACL-太田    04/08/09  院内処方の包括入力はチェック
      *                                     の対象外とする
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *    データチェック中間ファイル（対象患者）
           SELECT  DTCHK003-FILE   ASSIGN  DTCHK003PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-DTCHK003.
      *
      *
      *    データチェック中間ファイル（チェック結果：初期化用）
           SELECT  DTCHK004CL-FILE
                                   ASSIGN  DTCHK004PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  DTCHK004CL-KEY
                                   FILE    STATUS  IS  STS-DTCHK004CL.
      *
      *    データチェック中間ファイル（チェック結果）
           SELECT  DTCHK004-FILE
                                   ASSIGN  DTCHK004PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK004-KEY
                                   FILE    STATUS  IS  STS-DTCHK004.
      *
      *    エラーファイル
           SELECT  DTCHKERR-FILE   ASSIGN  DTCHKERRPARA
                                   FILE    STATUS  IS  STS-DTCHKERR.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    データチェック中間ファイル（対象患者情報）
       FD  DTCHK003-FILE.
           COPY    "CPDTCHK001.INC"
                                   REPLACING  //DTCHK001//
                                   BY         //DTCHK003//.
      *
      *    データチェック中間ファイル（チェック結果：初期化用）
       FD  DTCHK004CL-FILE.
           COPY    "CPDTCHK004.INC"
                                   REPLACING  //DTCHK004//
                                   BY         //DTCHK004CL//.
      *
      *    データチェック中間ファイル（チェック結果）
       FD  DTCHK004-FILE.
           COPY    "CPDTCHK004.INC".
      *
      *    エラーファイル
       FD  DTCHKERR-FILE.
       01  DTCHKERR-REC                PIC X(200). 
      *
       WORKING-STORAGE                 SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
           COPY    "HCM401.INC".
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPCOMMONDAT6.INC"
                                   REPLACING  //DTCHK//
                                   BY         //DTCHKERR//.
      *
      *    中間ファイルファイル名称領域 
           COPY    "CPCOMMONDAT6.INC"
                                   REPLACING  //DTCHK//
                                   BY         //DTCHK003//.
      *
           COPY    "CPCOMMONDAT6.INC"
                                   REPLACING  //DTCHK//
                                   BY         //DTCHK004//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-DTCHK003                            PIC X(02).
           03  STS-DTCHK004                            PIC X(02).
           03  STS-DTCHK004CL                          PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-HKNNUM                              PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTKOHINF                            PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-PTNUM                               PIC 9(01).
           03  FLG-CHKMST                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-PTBYOMEI                            PIC 9(01).
           03  FLG-DTCHK003                            PIC 9(01).
           03  FLG-YKZBYO                              PIC 9(01).
           03  FLG-SRYBYO                              PIC 9(01).
           03  FLG-YKZHEI                              PIC 9(01).
           03  FLG-SRYHEI                              PIC 9(01).
           03  FLG-CHKEND-G.
               05  FLG-CHKEND                          PIC 9(01)
                                                       OCCURS  50.
           03  FLG-JIHI                                PIC 9(01).
           03  FLG-RSI-JBI                             PIC 9(01).
           03  FLG-TAISYO-HKN                          PIC 9(01).
           03  FLG-TAISYO-SRY                          PIC 9(01).
           03  FLG-LOOP-END                            PIC 9(01).
           03  FLG-TSRYKA-OVER                         PIC 9(01).
           03  FLG-TKAISU-OVER                         PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-KANACHK                             PIC 9(01).
           03  FLG-SYUSYOKUGO                          PIC 9(01).
           03  FLG-PTBYO-DAY-G.
               05  FLG-PTBYO-DAY                       PIC 9(01)
                                                       OCCURS  31.
           03  FLG-PTBYO-TAISYO                        PIC 9(01).
           03  FLG-PTBYO-ERR                           PIC 9(01).
           03  FLG-PTBYO-MOJI                          PIC 9(01).
           03  FLG-CHK-END                             PIC 9(01).
           03  FLG-MICODSYOBYO                         PIC 9(01).
           03  FLG-PTBYO-LEN-ERR                       PIC 9(01).
           03  FLG-UTAGAI                              PIC 9(01).
           03  FLG-KYUSEI                              PIC 9(01).
      *    0:適応病名なし/1:適応病名あり
           03  FLG-CHKMST-BYOMEI                       PIC 9(01).
           03  FLG-BYOMEI-ERR                          PIC 9(01).
           03  FLG-CHKMST-SRY                          PIC 9(01).
           03  FLG-HIT                                 PIC 9(01).
           03  FLG-INTERACT                            PIC 9(01).
           03  FLG-ERRSET                              PIC 9(01).
           03  FLG-JNISSU-HIT                          PIC 9(01).
           03  FLG-SEIHO                               PIC 9(01).
           03  FLG-RJN                                 PIC 9(01).
           03  FLG-PROC                                PIC 9(01).
           03  FLG-PTBYO-HKN                           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DTCHK003                            PIC 9(06).
           03  CNT-DTCHK004                            PIC 9(06).
           03  CNT-JITUNISSU                           PIC 9(06).
           03  CNT-NISSU                               PIC 9(06).
           03  CNT-CHKERR                              PIC 9(06).
      *    医療機関コード、患者ＩＤ、診療科で抽出した件数
           03  CNT-PTBYOMEI1                           PIC 9(06).
      *    CNT-PTBYOMEI1から、さらに入外区分、適用期間で絞った件数
           03  CNT-PTBYOMEI2                           PIC 9(06).
           03  CNT-BYOMEIMOJI                          PIC 9(06).
           03  CNT-TAISYO                              PIC 9(06).
           03  CNT-STR                                 PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                                PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                              PIC 9(02).
               05  SYS-MM                              PIC 9(02).
               05  SYS-DD                              PIC 9(02).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05). 
           03  IDX1                                    PIC 9(05). 
           03  IDX2                                    PIC 9(05). 
           03  IDX3                                    PIC 9(05). 
           03  IDX4                                    PIC 9(05). 
           03  IDX5                                    PIC 9(05). 
           03  IDX6                                    PIC 9(05). 
           03  IDX7                                    PIC 9(05). 
           03  IDX8                                    PIC 9(05). 
           03  IDX9                                    PIC 9(05). 
           03  IDX10                                   PIC 9(05). 
           03  IDX11                                   PIC 9(05). 
           03  IDX12                                   PIC 9(05). 
           03  IDX13                                   PIC 9(05). 
           03  IDX14                                   PIC 9(05). 
           03  IDX15                                   PIC 9(05). 
           03  IDX16                                   PIC 9(05). 
           03  IDX17                                   PIC 9(05). 
           03  IDX18                                   PIC 9(05). 
           03  IDX19                                   PIC 9(05). 
           03  IDX20                                   PIC 9(05). 
           03  IDX21                                   PIC 9(05). 
           03  IDX22                                   PIC 9(05). 
           03  IDX23                                   PIC 9(05). 
           03  IDX24                                   PIC 9(05). 
           03  IDX25                                   PIC 9(05). 
           03  IDXA                                    PIC 9(05). 
           03  IDXE                                    PIC 9(05). 
           03  IDX-TSRYKA                              PIC 9(05).
           03  IDX-TKAISU                              PIC 9(05).
           03  IDX-ERRCD                               PIC 9(05).
           03  IDX-TSRYCD                              PIC 9(05).
           03  IDX-TTNS                                PIC 9(05).
           03  IDX-COMB                                PIC 9(05).
           03  IDX-DAY                                 PIC 9(02).
      *
       01  WRK-AREA.
           03  WRK-SYMD                                PIC X(08).
           03  WRK-EDTYMD1                             PIC X(10).
           03  WRK-EDTYMD3                             PIC X(22).
           03  WRK-MAE-INPUT                           PIC X(300).
           03  WRK-OUT-INPUT                           PIC X(300).
           03  WRK-OUT-MAX                             PIC 9(05).
           03  WRK-ZOGENPTN                            PIC X(01).
           03  WRK-ZOGEN                               PIC S9(05).
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-KJNYMD                              PIC X(08).
           03  WRK-CHK-CHKKBN                          PIC X(01).
           03  WRK-CHK-CDKBN                           PIC X(01).
           03  WRK-TSRYKA-SRYCD-MAX                    PIC 9(05).
           03  WRK-TKAISU-SRYCD-MAX                    PIC 9(05).
           03  WRK-SRYCD                               PIC X(09).
           03  WRK-TNS-YMD.
               05  WRK-TNS-YY                          PIC 9(04).
               05  WRK-TNS-MM                          PIC 9(02).
               05  WRK-TNS-DD                          PIC 9(02).
           03  WRK-DAY-Z                               PIC ZZ.
           03  WRK-NYUGAIKBN                           PIC X(01).
           03  WRK-HKNID                               PIC 9(10).
           03  WRK-KOHID                               PIC 9(10).
           03  WRK-ERRCD                               PIC X(06).
           03  WRK-ERRMES                              PIC X(200).
           03  WRK-ERRMES-PLUS                         PIC X(200).
           03  WRK-PTBYOMES-PLUS                       PIC X(200).
           03  WRK-BYOMEICD                            PIC X(07).
           03  WRK-HKNNUM                              PIC X(03).
           03  WRK-PTHKN-KIGO                          PIC X(80).
           03  WRK-PTHKN-NUM                           PIC X(80).
           03  WRK-KANACHK-MAE-INPUT                   PIC X(400).
           03  WRK-PTKOH-FTNJANUM                      PIC X(08).
           03  WRK-PTKOH-JKYSNUM                       PIC X(20).
           03  WRK-HEN-PTBYOMEI                        PIC X(100).
           03  WRK-STR                                 PIC X(500).
           03  WRK-STR-TMP                             PIC X(500).
           03  WRK-PNT                                 PIC 9(05).
           03  WRK-UTAIGAI-CHK-YM                      PIC X(06).
           03  WRK-YMD.
               05  WRK-YY                              PIC 9(04).
               05  WRK-MM                              PIC 9(02).
               05  WRK-DD                              PIC 9(02).
           03  WRK-SRYKA                               PIC X(02).
           03  WRK-CHK-CD                              PIC X(09).
           03  WRK-ORCBSD01-CHKKBN                     PIC X(01).
           03  WRK-FLG-CHKEND                          PIC 9(01).
           03  WRK-NYUGAI-MES                          PIC X(10).
           03  WRK-PTINF-NAME                          PIC X(100).
           03  WRK-PTINF-KANANAME                      PIC X(100).
           03  WRK-CHK-SRYCD                           PIC X(09).
           03  WRK-SHO                                 PIC 9(05).
           03  WRK-AMARI                               PIC 9(05).
           03  WRK-STR-LEFT                            PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES           WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99                   PIC Z(9)9.
               05                                      PIC X(490).
           03  WRK-STR-LEFT-TMP                        PIC X(500).
           03  WRK-N.
               05  WRK-KNS-N                           PIC X(10).
               05  WRK-TIME-N                          PIC X(16).
           03  WRK-PTBYO-SRYYMD                        PIC X(08).
           03  WRK-HKNCOMBI                            PIC X(04).
           03  WRK-HKNCOMBI-DAY-G.
               05  WRK-HKNCOMBI-DAY                    PIC 9(03)
                                                       OCCURS  31.
           03  WRK-TKAISU-SRYCD                        PIC X(09).
           03  WRK-MAX                                 PIC 9(05).
           03  WRK-KIGONUM-STR                         PIC X(50).
           03  WRK-STIDX                               PIC  9(02).
           03  WRK-EDIDX                               PIC  9(02).
           03  WRK-NISSU-AREA.
               05  WRK-NISSU-DAY-G.
                   07  WRK-NISSU-DAY                   PIC 9(01)
                                                       OCCURS  31.
               05  WRK-NISSU-KUGIRI-FLG                PIC 9(01).
               05  WRK-NISSU-RENZOKU-CNT               PIC 9(03).
               05  WRK-NISSU-ST-G.
                   07  WRK-NISSU-ST                    PIC 9(02).
                   07  WRK-NISSU-ST-X                  PIC X(02).
                   07  WRK-NISSU-ST-R1 REDEFINES   WRK-NISSU-ST-X.
                       09  WRK-NISSU-ST-Z9             PIC Z9.
                   07  WRK-NISSU-ST-R2 REDEFINES   WRK-NISSU-ST-X.
                       09  WRK-NISSU-ST-9              PIC 9(01).
                       09                              PIC X(01).
               05  WRK-NISSU-ED-G.
                   07  WRK-NISSU-ED                    PIC 9(02).
                   07  WRK-NISSU-ED-X                  PIC X(02).
                   07  WRK-NISSU-ED-R1 REDEFINES   WRK-NISSU-ED-X.
                       09  WRK-NISSU-ED-Z9             PIC Z9.
                   07  WRK-NISSU-ED-R2 REDEFINES   WRK-NISSU-ED-X.
                       09  WRK-NISSU-ED-9              PIC 9(01).
                       09                              PIC X(01).
               05  WRK-NISSU-MES                       PIC X(200).
               05  WRK-NISSU-PTR                       PIC 9(03).
           03  WRK-ORCSHKN-HKNCOMBI                    PIC X(04).
           03  WRK-ORCSHKN-HKNCOMBIMEI                 PIC X(200).
           03  WRK-SYOSIN-CHK-YMD.
               05  WRK-SYOSIN-CHK-YY                   PIC 9(04).
               05  WRK-SYOSIN-CHK-MM                   PIC 9(02).
               05  WRK-SYOSIN-CHK-DD                   PIC 9(02).
      *
      *    保険ＩＤ退避ワーク
       01  THKNID-AREA.
           03  THKNID-MAX                              PIC 9(05).
           03  THKNID-OCC                              OCCURS 20.
               05  THKNID-HKNID                        PIC 9(10).
      *
      *    公費ＩＤ退避ワーク
       01  TKOHID-AREA.
           03  TKOHID-RJNFLG                           PIC 9(01).
           03  TKOHID-SEIHOFLG                         PIC 9(01).
           03  TKOHID-MAX                              PIC 9(05).
           03  TKOHID-OCC                              OCCURS 20.
               05  TKOHID-KOHID                        PIC 9(10).
      *
      *    保険組合せ退避ワーク
      *    （THKNCOMBI-MAXは患者氏名のチェック対象の判定にも使用）
       01  THKNCOMBI-AREA.
           03  THKNCOMBI-MAX                           PIC 9(05).
           03  THKNCOMBI-OCC                           OCCURS 20.
               05  THKNCOMBI-HKNCOMBI                  PIC X(04).
               05  THKNCOMBI-STYMD                     PIC X(08).
               05  THKNCOMBI-EDYMD                     PIC X(08).
      *
      *    入院保険組合せ退避ワーク
      *    （TNACCT-HKNCOMBI-MAXは患者氏名のチェック対象の判定にも使用）
       01  TNACCT-HKNCOMBI-AREA.
           03  TNACCT-HKNCOMBI-DAY                     PIC 9(03)
                                                       OCCURS 31.
           03  TNACCT-HKNCOMBI-MAX                     PIC 9(05).
           03  TNACCT-HKNCOMBI-OCC                     OCCURS 20.
               05  TNACCT-HKNCOMBI-HKNCOMBI            PIC 9(04).
               05  TNACCT-HKNCOMBI-HKNCOMBI-X          REDEFINES
                   TNACCT-HKNCOMBI-HKNCOMBI            PIC X(04).
               05  TNACCT-HKNCOMBI-STYMD               PIC X(08).
               05  TNACCT-HKNCOMBI-EDYMD               PIC X(08).
               05  TNACCT-HKNCOMBI-HKNCOMBI-DAY        PIC 9(03)
                                                       OCCURS 31.
       01  TNACCT-KHNRYO-AREA.
           03  TNACCT-KHNRYO-DAY                       PIC 9(03)
                                                       OCCURS 31.
      *
      *    診療科退避ワーク
       01  TSRYKA-AREA.
           03  TSRYKA-MAX                              PIC 9(05).
           03  TSRYKA-OCC                              OCCURS 30.
               05  TSRYKA-SRYKA                        PIC X(02).
               05  TSRYKA-SHOSIN-DAY                   PIC 9(01)
                                                       OCCURS 31.
               05  TSRYKA-SHOSIN-HKN-YMD-G.
                   07  TSRYKA-SHOSIN-IHO-YMD           PIC X(08).
                   07  TSRYKA-SHOSIN-RSI-YMD           PIC X(08).
                   07  TSRYKA-SHOSIN-JBI-YMD           PIC X(08).
               05  TSRYKA-SHOSIN-HKN-YMD-R
                                   REDEFINES   TSRYKA-SHOSIN-HKN-YMD-G.
                   07  TSRYKA-SHOSIN-HKN-YMD           PIC X(08)
                                                       OCCURS  3.
               05  TSRYKA-SRYCD-MAX                    PIC 9(05).
               05  TSRYKA-SRYCD                        PIC X(09)
                                                       OCCURS 2000.
      *
      *    診療行為退避ワーク
       01  TSRYCD-AREA.
           03  TSRYCD-MAX                              PIC 9(05).
           03  TSRYCD-OCC                              OCCURS 2000.
      *        入外ともに同一の診療行為コードの設定があった場合は
      *        処理パラメタの入外区分を優先的に設定
               05  TSRYCD-NYUGAIKBN                    PIC X(01).
               05  TSRYCD-SRYCD                        PIC X(09).
      *
      *    実日数退避ワーク（実日数チェック用）
       01  TJNISSU-AREA.
           03  TJNISSU-MAX                             PIC 9(05).
           03  TJNISSU-OCC                             OCCURS 20.
               05  TJNISSU-HKNNUM                      PIC X(03).
               05  TJNISSU-DAY-CNT                     PIC 9(03).
               05  TJNISSU-DAY                         PIC 9(01)
                                                       OCCURS 31.
      *
      *    算定回数退避ワーク（実日数チェック用）
       01  TKAISU-AREA.
           03  TKAISU-MAX                              PIC 9(05).
           03  TKAISU-OCC                              OCCURS 20.
               05  TKAISU-HKNNUM                       PIC X(03).
               05  TKAISU-SRYCD-G.
                   07  TKAISU-SRYCD-MAX                PIC 9(05).
                   07  TKAISU-SRYCD-OCC                OCCURS 2000.
                       09  TKAISU-SRYCD                PIC X(09).
                       09  TKAISU-SRYCD-CNT            PIC 9(03).
                       09  TKAISU-SRYCD-DAY            PIC 9(01)
                                                       OCCURS 31.
      *
      *    病名コード退避ワーク
       01  TBYOMEICD-AREA.
           03  TBYOMEICD-IDX1                          PIC 9(05).
           03  TBYOMEICD-IDX2                          PIC 9(05).
           03  TBYOMEICD-STIDX                         PIC 9(05).
           03  TBYOMEICD-MAX                           PIC 9(05).
           03  TBYOMEICD-OCC                           OCCURS 101.
               05  TBYOMEICD-BYOMEICD                  PIC X(07).
               05  TBYOMEICD-KEY.
                   07  TBYOMEICD-ERRFLG                PIC 9(01).
                   07  TBYOMEICD-SRYYMD                PIC X(08).
      *
      *    入院調剤料算定日退避ワーク
       01  TNYUIN-CHOZAI-AREA.
           03  TNYUIN-CHOZAI-DAY                       PIC 9(01)
                                                       OCCURS 31.
      *
      *    入院診療行為日退避ワーク
       01  TNYUIN-ACCT-AREA.
           03  TNYUIN-ACCT-DAY                         PIC 9(01)
                                                       OCCURS 31.
      *
      *    併用禁忌（算定）退避ワーク
       01  THEIYOU-AREA.
           03  THEIYOU-MAX                             PIC 9(05).
           03  THEIYOU-OCC                             OCCURS 4000.
               05  THEIYOU-SRYCD-G.
                   07  THEIYOU-NYUGAIKBN1              PIC X(01).
                   07  THEIYOU-SRYCD1                  PIC X(09).
                   07  THEIYOU-NYUGAIKBN2              PIC X(01).
                   07  THEIYOU-SRYCD2                  PIC X(09).
      *
      *    チェックマスタに存在しない診療行為退避ワーク
       01  TCHK-AREA.
           03  TCHK-FLG                                PIC 9(01).
           03  TCHK-YKZ-BYO-MAX                        PIC 9(05).
           03  TCHK-SRY-BYO-MAX                        PIC 9(05).
           03  TCHK-HEI-YKZ-MAX                        PIC 9(05).
           03  TCHK-HEI-SRY-MAX                        PIC 9(05).
           03  TCHK-SRYCD-OCC                          OCCURS  2000.
               05  TCHK-YKZ-BYO-SRYCD                  PIC X(09).
               05  TCHK-SRY-BYO-SRYCD                  PIC X(09).
               05  TCHK-HEI-YKZ-SRYCD                  PIC X(09).
               05  TCHK-HEI-SRY-SRYCD                  PIC X(09).
      *
      *    減算情報重複チェック用ワーク
       01  TGENZAN-AREA.
           03  TGENZAN-VAL.
               05  TGENZAN-INNAI-G.
                   07  TGENZAN-INNAI-SRYCD         PIC X(09).
                   07  TGENZAN-INNAI-SYS-DAY       PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-INNAI-SRY-DAY       PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-INNAI-ERR-CNT       PIC 9(02).
                   07  TGENZAN-INNAI-ERR-DAY       PIC 9(01)
                                                   OCCURS  31.
               05  TGENZAN-ANZEN-G.
                   07  TGENZAN-ANZEN-SRYCD         PIC X(09).
                   07  TGENZAN-ANZEN-SYS-DAY       PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-ANZEN-SRY-DAY       PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-ANZEN-ERR-CNT       PIC 9(02).
                   07  TGENZAN-ANZEN-ERR-DAY       PIC 9(01)
                                                   OCCURS  31.
               05  TGENZAN-JOKUSO-G.
                   07  TGENZAN-JOKUSO-SRYCD        PIC X(09).
                   07  TGENZAN-JOKUSO-SYS-DAY      PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-JOKUSO-SRY-DAY      PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-JOKUSO-ERR-CNT      PIC 9(02).
                   07  TGENZAN-JOKUSO-ERR-DAY      PIC 9(01)
                                                   OCCURS  31.
           03  TGENZAN-R   REDEFINES   TGENZAN-VAL.
               05  TGENZAN-OCC                     OCCURS  3.
                   07  TGENZAN-SRYCD               PIC X(09).
                   07  TGENZAN-SYS-DAY             PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-SRY-DAY             PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-ERR-CNT             PIC 9(02).
                   07  TGENZAN-ERR-DAY-G.
                       09  TGENZAN-ERR-DAY         PIC 9(01)
                                                   OCCURS  31.
      *
      *    入院料加算保険チェック用ワーク
       01  TNYUKSN-AREA.
           03  TNYUKSN-MAX                         PIC 9(05).
           03  TNYUKSN-OCC                         OCCURS  20.
               05  TNYUKSN-HKNCOMBI                PIC X(04).
               05  TNYUKSN-HKNCOMBI-DAY            PIC 9(01)
                                                   OCCURS  31.
               05  TNYUKSN-ERR-CNT                 PIC 9(02).
               05  TNYUKSN-ERR-DAY-G.
                   07  TNYUKSN-ERR-DAY             PIC 9(01)
                                                   OCCURS  31.
      *
       01  KEY-AREA.
           03  KEY-HKNCOMBI                        PIC X(04).
      *
       01  SRH-AREA.
           03  SRH-IDX                             PIC 9(05).
           03  SRH-HKNNUM-DAT.
               05  SRH-HKNNUM                      PIC X(03).
               05  SRH-HKNNUMMEI                   PIC X(10).
               05  SRH-HKNNUM-MAX                  PIC 9(05).
               05  SRH-HKNNUM-OCC                  OCCURS  50.
                   07  SRH-HKNNUML                 PIC X(03).
                   07  SRH-HKNNUMMEIL              PIC X(10).
           03  SRH-KOHNUM-DAT.
               05  SRH-KOHNUM                      PIC X(03).
               05  SRH-KOHNUMMEI                   PIC X(10).
               05  SRH-PAYKBN                      PIC X(02).
               05  SRH-KOHNUM-MAX                  PIC 9(05).
               05  SRH-KOHNUM-OCC                  OCCURS  150.
                   07  SRH-KOHNUML                 PIC X(03).
                   07  SRH-PAYKBNL                 PIC X(02).
                   07  SRH-KOHNUMMEIL              PIC X(10).
           03  SRH-1005-DAT.
               05  SRH-1005-SRYKA                  PIC X(02).
               05  SRH-1005-SRYKAMEI1              PIC X(20).
               05  SRH-1005-SRYKAMEI2              PIC X(12).
               05  SRH-1005-SRYKAMEI3              PIC X(04).
               05  SRH-1005-MAX                    PIC 9(05).
               05  SRH-1005-OCC                    OCCURS 100.
                   07  SRH-1005-SRYKAL             PIC X(02).
                   07  SRH-1005-SRYKAMEI1L         PIC X(20).
                   07  SRH-1005-SRYKAMEI2L         PIC X(12).
                   07  SRH-1005-SRYKAMEI3L         PIC X(04).
           03  SRH-1040-DAT.
               05  SRH-1040-KOHNUM                 PIC X(03).
               05  SRH-1040-FTNJANUMCHK            PIC X(01).
               05  SRH-1040-JKYSNUMCHK             PIC X(01).
               05  SRH-1040-MAX                    PIC 9(05).
               05  SRH-1040-OCC                    OCCURS  100.
                   07  SRH-1040-KOHNUML            PIC X(03).
                   07  SRH-1040-FTNJANUMCHKL       PIC X(01).
                   07  SRH-1040-JKYSNUMCHKL        PIC X(01).
      *
      *    点数マスタ退避ワーク
      *    薬剤
       01  TTNS-AREA.
           03  TTNS-FLG                            PIC 9(01).
           03  TTNS-YKZ-G.
               05 TTNS-YKZ-MAX                     PIC 9(05).
               05 TTNS-YKZ-KEY-OCC                 OCCURS 2000.
                  07   TTNS-YKZ-KEY-SRYCD          PIC X(09).
                  07   TTNS-YKZ-KEY-YUKOSTYMD      PIC X(08).
                  07   TTNS-YKZ-KEY-YUKOEDYMD      PIC X(08).
               05   TTNS-YKZ-DAT                   PIC X(2000)
                                                   OCCURS 2000.
      *    薬剤以外
           03  TTNS-ETC-G.
               05 TTNS-ETC-MAX                     PIC 9(05).
               05 TTNS-ETC-KEY-OCC                 OCCURS 2000.
                  07   TTNS-ETC-KEY-SRYCD          PIC X(09).
                  07   TTNS-ETC-KEY-YUKOSTYMD      PIC X(08).
                  07   TTNS-ETC-KEY-YUKOEDYMD      PIC X(08).
               05   TTNS-ETC-DAT                   PIC X(2000)
                                                   OCCURS 2000.
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                          PIC 9(07).
           03  WRK-PARA-SHELLID                        PIC X(08).
           03  WRK-PARA-HOSPID                         PIC X(24).
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
           COPY    "MCPAREA".
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-THKNID-MAX                        PIC 9(05)
                                                       VALUE   20.
           03  CONST-TKOHID-MAX                        PIC 9(05)
                                                       VALUE   20.
           03  CONST-THKNCOMBI-MAX                     PIC 9(05)
                                                       VALUE   20.
           03  CONST-TNACCT-HKNCOMBI-MAX               PIC 9(05)
                                                       VALUE   20.
           03  CONST-TSRYKA-MAX                        PIC 9(05)
                                                       VALUE   30.
           03  CONST-TSRYKA-SRYCD-MAX                  PIC 9(05)
                                                       VALUE   2000.
           03  CONST-TSRYCD-MAX                        PIC 9(05)
                                                       VALUE   2000.
           03  CONST-TJNISSU-MAX                       PIC 9(05)
                                                       VALUE   20.
           03  CONST-TKAISU-MAX                        PIC 9(05)
                                                       VALUE   20.
           03  CONST-TKAISU-SRYCD-MAX                  PIC 9(05)
                                                       VALUE   2000.
           03  CONST-THEIYOU-MAX                       PIC 9(05)
                                                       VALUE   2000.
           03  CONST-TBYOMEICD-MAX                     PIC 9(05)
                                                       VALUE   100.
           03  CONST-TNYUKSN-MAX                       PIC 9(05)
                                                       VALUE   20.
           03  CONST-TGENZAN-MAX                       PIC 9(05)
                                                       VALUE   3.
           03  CONST-TCHK-MAX                          PIC 9(05)
                                                       VALUE   2000.
           03  CONST-TTNS-MAX                          PIC 9(05)
                                                       VALUE   2000.
      *
      *    エラーメッセージ
           COPY    "CPDTCHKERRMES.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    チェク情報
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    データチェック機能情報
           COPY    "CPSK1040.INC".
      *
      *    レセプト編集区分
           COPY    "CPSK2005.INC".
      *
      *    入院基本情報
           COPY    "CPSK5000.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    相互作用情報
       01  INTERACT-REC.
           COPY    "CPINTERACT.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
      ****************************************************************
       PROCEDURE                       DIVISION
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
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  STS-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       WRK-PARA
                                       ORCBSD01PARA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPID
                                               DTCHKERRPARA
                                               ORCBSD01-SRYYM
                                               ORCBSD01-NYUGAIKBN
                                               ORCBSD01-CHKKBN-G
                                               ORCBSD01-JIHIPROCKBN
                                               ORCBSD01-INGAIPROCKBN
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCDTCHK002"       TO  JOB-PGID
           MOVE    "データチェック"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
      *    データチェック中間ファイル名
           MOVE    "DTCHK003"          TO  DTCHK003-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK003-TERMID
      *
           MOVE    "DTCHK004"          TO  DTCHK004-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK004-TERMID
      *
      *    初期値設定処理
           PERFORM 1001-INIT-VAL-SET-SEC
      *
      *    チェックマスタチェック処理
           PERFORM 1001-CHKMST-CHK-SEC
      *
      *    保険の種類・公費の種類編集処理
           PERFORM 1001-HKNKOH-HEN-SEC
      *
      *    診療科編集処理
           PERFORM 1001-SRYKA-HEN-SEC
      *
      *    負担者番号受給者番号抜けチェック対象取得処理
           PERFORM 1001-SYS1040-GET-SEC
      *
      *    レセプト編集区分取得処理
           PERFORM 1001-SYS2005-GET-SEC
      *
           OPEN    INPUT               DTCHK003-FILE
      *
           OPEN    OUTPUT              DTCHK004CL-FILE
           CLOSE   DTCHK004CL-FILE
      *
           OPEN    OUTPUT              DTCHK004-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期値設定処理
      *****************************************************************
       1001-INIT-VAL-SET-SEC           SECTION.
      *
           MOVE    ORCBSD01-SRYYM      TO  WRK-KJNYMD
           MOVE    "01"                TO  WRK-KJNYMD(7:2)
      *
           INITIALIZE                      TCHK-AREA
                                           TTNS-AREA

      *
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタチェック処理
      *    （チェック用データの存在チェック）
      *****************************************************************
       1001-CHKMST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-YKZBYO
                                           FLG-SRYBYO
                                           FLG-YKZHEI
                                           FLG-SRYHEI
      *
      *    薬剤と病名
           MOVE    "1"                 TO  WRK-CHK-CHKKBN
           MOVE    "1"                 TO  WRK-CHK-CDKBN
           PERFORM 900-CHK-KEY5-SEL-SEC
           IF    ( FLG-CHKMST          =   ZERO )
               MOVE    1               TO  FLG-YKZBYO
           END-IF
      *
      *    診療行為と病名
           MOVE    "2"                 TO  WRK-CHK-CHKKBN
           MOVE    "1"                 TO  WRK-CHK-CDKBN
           PERFORM 900-CHK-KEY5-SEL-SEC
           IF    ( FLG-CHKMST          =   ZERO )
               MOVE    1               TO  FLG-SRYBYO
           END-IF
      *
      *    併用禁忌の薬剤
           MOVE    "4"                 TO  WRK-CHK-CHKKBN
           MOVE    "2"                 TO  WRK-CHK-CDKBN
           PERFORM 900-CHK-KEY5-SEL-SEC
           IF    ( FLG-CHKMST          =   ZERO )
               MOVE    1               TO  FLG-YKZHEI
           END-IF
      *
      *    併用算定の診療行為
           MOVE    "5"                 TO  WRK-CHK-CHKKBN
           MOVE    "2"                 TO  WRK-CHK-CDKBN
           PERFORM 900-CHK-KEY5-SEL-SEC
           IF    ( FLG-CHKMST          =   ZERO )
               MOVE    1               TO  FLG-SRYHEI
           END-IF
      *
           .
       1001-CHKMST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類・公費の種類編集処理
      *****************************************************************
       1001-HKNKOH-HEN-SEC         SECTION.
      *
           INITIALIZE                  SRH-HKNNUM-DAT
           INITIALIZE                  SRH-KOHNUM-DAT
      *
      *    保険番号検索
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    WRK-PARA-HOSPID     TO  HKN-HOSPID
           MOVE    WRK-KJNYMD          TO  HKN-TEKSTYMD
           MOVE    WRK-KJNYMD          TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    WRK-KJNYMD          TO  ORC-DBYMD
           MOVE    "HKNNUM-KEY3"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-HKNNUM      =   1 )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR  "4"
                                           OR  "8" OR  "9" )
                   MOVE    SRH-HKNNUM-MAX  TO  IDX5
      *            保険
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  NOT =   SRH-HKNNUML (IDX5))
      *                    保険情報編集
                           PERFORM 10011-HKN-HEN-SEC
                       END-IF
                   ELSE
      *                保険情報編集
                       PERFORM 10011-HKN-HEN-SEC
                   END-IF
      *
               ELSE
      *            公費
                   MOVE    SRH-KOHNUM-MAX  TO  IDX5
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  =   SRH-KOHNUML (IDX5))
                           IF    ( HKN-HKNNUM  =   "027"  ) 
                             AND ( HKN-PAYKBN  =   "01" OR "02"  )
                               IF ( HKN-PAYKBN
                                           NOT =   SRH-PAYKBNL (IDX5))
      *                            公費情報編集
                                   PERFORM 10011-KOH-HEN-SEC
                               END-IF
                           END-IF
                       ELSE
      *                    公費情報編集
                           PERFORM 10011-KOH-HEN-SEC
                       END-IF
                   ELSE
      *                公費情報編集
                       PERFORM 10011-KOH-HEN-SEC
                   END-IF
               END-IF
      *
      *        保険番号読み込み
               MOVE    "HKNNUM-KEY3"   TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC             NOT =   ZERO )
                   MOVE    1           TO  FLG-HKNNUM
               ELSE
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
               END-IF
      *
           END-PERFORM
      *
      *    保険番号クローズ
           MOVE    "HKNNUM-KEY3"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       1001-HKNKOH-CMB-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       10011-HKN-HEN-SEC          SECTION.
      *
           IF    ( SRH-HKNNUM-MAX      >=  50 )
               GO  TO  10011-HKN-HEN-EXT
           END-IF
      *
           COMPUTE SRH-HKNNUM-MAX  =   SRH-HKNNUM-MAX   +   1
      *
           MOVE    SRH-HKNNUM-MAX      TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SRH-HKNNUML   (IDX5)
           MOVE    HKN-TANSEIDONAME    TO  SRH-HKNNUMMEIL(IDX5)
      *
           .
       10011-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       10011-KOH-HEN-SEC           SECTION.
      *
           IF    ( SRH-KOHNUM-MAX      >=  150 )
               GO  TO  10011-KOH-HEN-EXT
           END-IF
      *
           COMPUTE SRH-KOHNUM-MAX  =   SRH-KOHNUM-MAX  +   1
      *
           MOVE    SRH-KOHNUM-MAX      TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SRH-KOHNUML (IDX5)
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN      NOT =   "01" AND "02" )
               MOVE    "老人保健"      TO  SRH-KOHNUMMEIL (IDX5)
           ELSE
               MOVE    HKN-TANSEIDONAME
                                       TO  SRH-KOHNUMMEIL (IDX5)
           END-IF
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN          =   "01" OR  "02" )
               MOVE    HKN-PAYKBN
                                       TO  SRH-PAYKBNL (IDX5)
           ELSE
               MOVE    SPACE           TO  SRH-PAYKBNL (IDX5)
           END-IF
      *
           .
       10011-KOH-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       1001-SRYKA-HEN-SEC              SECTION.
      *
           INITIALIZE              SRH-1005-DAT
      *
      *    システム管理マスタ検索処理
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-KJNYMD          TO  ORC-DBYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
               UNTIL     ( IDX5            >   100 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX5            TO  SRH-1005-MAX
               MOVE    SYS-1005-KBNCD (1 : 2)
                                       TO  SRH-1005-SRYKAL (IDX5)
               MOVE    SYS-1005-SRYKANAME1
                                       TO  SRH-1005-SRYKAMEI1L (IDX5)
               MOVE    SYS-1005-SRYKANAME2
                                       TO  SRH-1005-SRYKAMEI2L (IDX5)
               MOVE    SYS-1005-SRYKANAME3
                                       TO  SRH-1005-SRYKAMEI3L (IDX5)
      *
      *        システム管理マスタ読込処理
               MOVE    "SYSKANRI-KEY2" TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC             NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-1005-REC
               END-IF
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ処理
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       1001-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費負担者番号・受給者番号抜けチェック対象取得処理
      *****************************************************************
       1001-SYS1040-GET-SEC            SECTION.
      *
      *
           INITIALIZE              SRH-1040-DAT
      *
      *    システム管理マスタ検索処理
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1040-REC
           MOVE    "1040"              TO  SYS-1040-KANRICD
           MOVE    WRK-KJNYMD          TO  ORC-DBYMD
           MOVE    SYS-1040-REC        TO  MCPDATA-REC
      *
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1040-REC
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
               UNTIL     ( IDX5            >   100 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX5            TO  SRH-1040-MAX
               MOVE    SYS-1040-KBNCD (1 : 3)
                                       TO  SRH-1040-KOHNUML (IDX5)
               MOVE    SYS-1040-FTNJANUMCHK
                                       TO  SRH-1040-FTNJANUMCHKL(IDX5)
               MOVE    SYS-1040-JKYSNUMCHK
                                       TO  SRH-1040-JKYSNUMCHKL (IDX5)
      *
      *        システム管理マスタ読込処理
               MOVE    "SYSKANRI-KEY2" TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC             NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-1040-REC
               END-IF
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ処理
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       1001-SYS1040-GET-EXT.
           EXIT.
      *****************************************************************
      *    レセプト編集区分取得処理
      *****************************************************************
       1001-SYS2005-GET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           MOVE    WRK-KJNYMD          TO  ORC-DBYMD
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-2005-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-2005-REC
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       1001-SYS2005-GET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    データチェック処理
           PERFORM 2001-DATA-CHK-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    データチェック処理
      *****************************************************************
       2001-DATA-CHK-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK003
      *
      *    対象患者情報ファイル読込処理
           PERFORM 900-DTCHK003-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK003    NOT =   ZERO )
      *
      *        チェック項目情報退避処理
               PERFORM 20011-CHK-DATA-TAIHI-SEC
      *
      *        データチェック処理
               PERFORM 20011-DATA-CHK-SEC
      *
      *        処理経過編集処理
               PERFORM 20011-SYORI-KEIKA-HEN-SEC
      *
      *        対象患者情報ファイル読込処理
               PERFORM 900-DTCHK003-READ-SEC
      *
           END-PERFORM
      *
           .
       2001-DATA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目情報退避処理
      *****************************************************************
       20011-CHK-DATA-TAIHI-SEC        SECTION.
      *
      *    退避エリア初期処理
           PERFORM 200111-TAIHI-AREA-INIT-SEC
      *
      *    減算情報ワークシステム管理退避処理
           PERFORM 200111-TGENZAN-SYS-HEN-SEC
      *
      *    診療会計情報退避処理
           PERFORM 200111-SRYACCT-TAIHI-SEC
      *
      *    併用禁忌・併用算定のチェック用に
      *    入力引数とは反対の入外区分で診療会計テーブルを検索する
           IF    ( ORCBSD01-CHKKBN (10)    NOT =   ZERO )
            OR   ( ORCBSD01-CHKKBN (11)    NOT =   ZERO )
               PERFORM 200112-SRYACCT-TAIHI-SEC
           END-IF
      *
      *    入院会計保険組み合わせ退避処理
           IF    ( ORCBSD01-NYUGAIKBN      =   "1" )
      *
               PERFORM 200111-NACCT-KHNRYO-TAIHI-SEC
      *
               PERFORM 200111-NACCT-COMBI-TAIHI-SEC
      *
               PERFORM VARYING IDX18   FROM    1   BY  1
                       UNTIL ( IDX18   >   TNACCT-HKNCOMBI-MAX )
                   MOVE    TNACCT-HKNCOMBI-HKNCOMBI-X (IDX18)
                                       TO  WRK-HKNCOMBI
      *            主保険チェック
                   PERFORM 200111-SYUHOKEN-CHK-SEC
      *
      *            保険ＩＤ退避ワーク編集チェック処理
                   PERFORM 200111-THKNID-HEN-CHK-SEC
      *
      *            公費ＩＤ退避ワーク編集チェック処理
                   PERFORM 200111-TKOHID-HEN-CHK-SEC
      *
                   IF    ( FLG-TAISYO-HKN  =   1 )
                       INITIALIZE WRK-HKNCOMBI-DAY-G
                       PERFORM VARYING IDX0    FROM    1   BY  1
                               UNTIL ( IDX0    >   31 )
                           MOVE    TNACCT-HKNCOMBI-HKNCOMBI-DAY
                                                  (IDX18 IDX0)
                               TO  WRK-HKNCOMBI-DAY (IDX0)
                       END-PERFORM
      *
      *                実日数退避ワーク編集処理（入院）
                       PERFORM 200111-TJNISSU-NYU-HEN-SEC
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       20011-CHK-DATA-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    退避エリア初期処理
      *****************************************************************
       200111-TAIHI-AREA-INIT-SEC      SECTION.
      *
           INITIALIZE                  THKNID-AREA
                                       TKOHID-AREA
                                       THKNCOMBI-AREA
                                       TNACCT-HKNCOMBI-AREA
                                       TNACCT-KHNRYO-AREA
                                       TSRYKA-AREA
                                       TSRYCD-AREA
                                       TJNISSU-AREA
                                       TKAISU-AREA
                                       TBYOMEICD-AREA
                                       TNYUIN-CHOZAI-AREA
                                       TNYUIN-ACCT-AREA
                                       TNYUKSN-AREA
                                       TGENZAN-AREA
      *
           .
       200111-TAIHI-AREA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    減算情報ワークシステム管理退避処理
      *****************************************************************
       200111-TGENZAN-SYS-HEN-SEC      SECTION.
      *
           MOVE    "190080570"         TO  TGENZAN-INNAI-SRYCD
           MOVE    "190111690"         TO  TGENZAN-ANZEN-SRYCD
           MOVE    "190111790"         TO  TGENZAN-JOKUSO-SRYCD
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5000-REC
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY4"     TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5000-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
      *
               IF    ( ORCBSD01-SRYYM  >=  SYS-5000-STYUKYMD (1:6))
                AND  ( ORCBSD01-SRYYM  <=  SYS-5000-EDYUKYMD (1:6))
      *
                   MOVE    01          TO  WRK-STIDX
                   MOVE    31          TO  WRK-EDIDX
      *
                   IF    ( ORCBSD01-SRYYM  =    SYS-5000-STYUKYMD (1:6))
                       MOVE    SYS-5000-STYUKYMD (7:2)
                                       TO  WRK-STIDX
                   END-IF
      *
                   IF    ( ORCBSD01-SRYYM  =    SYS-5000-EDYUKYMD (1:6))
                       MOVE    SYS-5000-EDYUKYMD (7:2)
                                       TO   WRK-EDIDX
                   END-IF
      *
                   PERFORM VARYING IDX0    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX0        >   WRK-EDIDX )
                       IF    ( SYS-5000-GENKBN (2) =   "2" )
                           MOVE    1   TO   TGENZAN-INNAI-SYS-DAY (IDX0)
                       END-IF
                       IF    ( SYS-5000-GENKBN (3) =   "2" )
                           MOVE    1   TO   TGENZAN-ANZEN-SYS-DAY (IDX0)
                       END-IF
                       IF    ( SYS-5000-GENKBN (4) =   "2" )
                           MOVE    1   TO   TGENZAN-JOKUSO-SYS-DAY(IDX0)
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
               MOVE    "SYSKANRI-KEY4" TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-5000-REC
               ELSE
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SYSKANRI-KEY4"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200111-TGENZAN-SYS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報退避処理
      *****************************************************************
       200111-SRYACCT-TAIHI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    DTCHK003-HOSPID     TO  ACCT-HOSPID
           MOVE    ORCBSD01-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    DTCHK003-PTID       TO  ACCT-PTID
           MOVE    ORCBSD01-SRYYM      TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "SRYACCT-KEY28"     TO  ORC-DBPATH
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-SRYACCT-KEY28-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE    SPACE               TO  KEY-HKNCOMBI
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
      *
               IF    ( ACCT-HKNCOMBI   NOT =   KEY-HKNCOMBI )
      *
                   MOVE    ACCT-HKNCOMBI   TO  KEY-HKNCOMBI
      *
      *            主保険チェック
                   MOVE    KEY-HKNCOMBI    TO  WRK-HKNCOMBI
                   PERFORM 200111-SYUHOKEN-CHK-SEC
      *
      *            保険ＩＤ退避ワーク編集チェック処理
                   PERFORM 200111-THKNID-HEN-CHK-SEC
      *
      *            公費ＩＤ退避ワーク編集チェック処理
                   PERFORM 200111-TKOHID-HEN-CHK-SEC
      *
               END-IF
      *
      *        保険組合せ退避ワーク編集処理
               IF    ( FLG-TAISYO-HKN      =   1 )
                   IF    ( THKNCOMBI-MAX   <   CONST-THKNCOMBI-MAX )
                       PERFORM 200111-THKNCOMBI-HEN-SEC
                   END-IF
               END-IF
      *
      *        診療科退避ワーク診療科編集処理
               IF    ( FLG-TAISYO-HKN      =   1 )
                   MOVE    ZERO            TO  IDX-TSRYKA
                   MOVE    ZERO            TO  FLG-TSRYKA-OVER
                   PERFORM 200111-TSRYKA-SRYKA-HEN-SEC
               END-IF
      *
      *        診療行為情報退避処理
               IF    ( FLG-TAISYO-HKN      =   1 )
                   PERFORM 200111-SRYACT-TAIHI-SEC
               END-IF
      *
               IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
      *
                   IF    ( FLG-TAISYO-HKN      =   1 )
      *                入院料加算退避ワーク編集処理
                       IF    ( ACCT-SRYKBN     =   "90"    OR  "92" )
                           PERFORM 200111-TNYUKSN-HEN-SEC
                       END-IF
                   END-IF
      *
      *            入院診療行為日編集処理（実日数チェック用）
                   PERFORM 200111-TNYUIN-ACCT-HEN-SEC
      *
      *            入院調剤料算定日編集処理（実日数チェック用）
                   IF    ( ACCT-SRYKBN     =   "24" )
                       PERFORM 200111-TNYUIN-CHOZAI-HEN-SEC
                   END-IF
      *
               END-IF
      *
               PERFORM 900-SRYACCT-KEY28-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "SRYACCT-KEY28"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200111-SRYACCT-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    主保険チェック処理
      *****************************************************************
       200111-SYUHOKEN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-JIHI
                                           FLG-RSI-JBI
                                           FLG-TAISYO-HKN
      *
      *    保険組合せ取得処理
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
      *    自費・労災・自賠責の判断
           IF    ( COMB-HKNNUM (1:2)   =   "98" )
               MOVE        1           TO  FLG-JIHI
           END-IF
      *
           IF    ( COMB-HKNNUM         =   "971"
                                       OR  "973" )
               MOVE        1           TO  FLG-RSI-JBI
           END-IF
      *
      *    処理対象とする保険か否かを判断（自費かどうか）
           EVALUATE    ORCBSD01-JIHIPROCKBN
      *    自費保険患者を含まない
           WHEN    ZERO
               IF    ( FLG-JIHI        =   ZERO )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
      *    自費保険患者を含む
           WHEN    "1"
                   MOVE    1           TO  FLG-TAISYO-HKN
      *    自費保険患者のみ
           WHEN    "2"
               IF    ( FLG-JIHI        =   1 )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           END-EVALUATE
      *
           .
       200111-SYUHOKEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険ＩＤ退避ワーク編集チェック処理
      *****************************************************************
       200111-THKNID-HEN-CHK-SEC       SECTION.
      *
           IF    ( FLG-RSI-JBI         =   ZERO )
             AND ( FLG-JIHI            =   ZERO )
               IF    ( COMB-HKNID  NOT =   ZERO )
                   IF    ( THKNID-MAX  <   CONST-THKNID-MAX )
                       PERFORM 200111-THKNID-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-THKNID-HEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険ＩＤ退避ワーク編集処理
      *****************************************************************
       200111-THKNID-HEN-SEC           SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   THKNID-MAX )
                    OR   ( IDX0        >   CONST-THKNID-MAX )
                    OR   ( COMB-HKNID  =   THKNID-HKNID (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   THKNID-MAX )
      *
               COMPUTE THKNID-MAX  =   THKNID-MAX  +   1
      *
               IF    ( THKNID-MAX  >   CONST-THKNID-MAX )
                   MOVE    CONST-THKNID-MAX
                                       TO  THKNID-MAX
               ELSE
                   MOVE    COMB-HKNID  TO  THKNID-HKNID (THKNID-MAX)
               END-IF
           END-IF
      *
           .
       200111-THKNID-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費ＩＤ退避ワーク編集処理
      *****************************************************************
       200111-TKOHID-HEN-CHK-SEC       SECTION.
      *
           IF    ( FLG-RSI-JBI         =   ZERO )
             AND ( FLG-JIHI            =   ZERO )
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >  4 )
                       OR    ( COMB-KOHID (IDX3)   =   ZERO   )
                   IF    ( TKOHID-MAX  <   CONST-TKOHID-MAX )
                       PERFORM 200111-TKOHID-HEN-SEC
                   END-IF
                END-PERFORM
           END-IF
      *
           .
       200111-TKOHID-HEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費ＩＤ退避ワーク編集処理
      *****************************************************************
       200111-TKOHID-HEN-SEC           SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   TKOHID-MAX )
                    OR   ( IDX0        >   CONST-TKOHID-MAX )
                    OR   ( COMB-KOHID (IDX3)
                                       =   TKOHID-KOHID (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TKOHID-MAX )
      *
               COMPUTE TKOHID-MAX  =   TKOHID-MAX  +   1
      *
               IF    ( TKOHID-MAX  >   CONST-TKOHID-MAX )
                   MOVE    CONST-TKOHID-MAX
                                       TO  TKOHID-MAX
               ELSE
                   MOVE    COMB-KOHID (IDX3)
                                       TO  TKOHID-KOHID (TKOHID-MAX)
      *
                   EVALUATE    COMB-KOHHKNNUM (IDX3)
                   WHEN    "027"
                       MOVE    1       TO  TKOHID-RJNFLG
                   WHEN    "012"
                       MOVE    1       TO  TKOHID-SEIHOFLG
                   END-EVALUATE
      *
               END-IF
           END-IF
      *
           .
       200111-TKOHID-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ退避ワーク編集処理
      *****************************************************************
       200111-THKNCOMBI-HEN-SEC    SECTION.
      *
           MOVE    ZERO                TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   THKNCOMBI-MAX )
                    OR   ( IDX0        >   CONST-THKNCOMBI-MAX )
                    OR   ( ACCT-HKNCOMBI
                                       =   THKNCOMBI-HKNCOMBI (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0                >   THKNCOMBI-MAX )
      *
               COMPUTE THKNCOMBI-MAX   =   THKNCOMBI-MAX  +   1
      *
               IF    ( THKNCOMBI-MAX   >   CONST-THKNCOMBI-MAX )
                   MOVE    CONST-THKNCOMBI-MAX
                                       TO  THKNCOMBI-MAX
               ELSE
                   MOVE    ACCT-HKNCOMBI
                                       TO  THKNCOMBI-HKNCOMBI
                                                       (THKNCOMBI-MAX)
                   MOVE    "99999999"  TO  THKNCOMBI-STYMD
                                                       (THKNCOMBI-MAX)
                   MOVE    "00000000"  TO  THKNCOMBI-EDYMD
                                                       (THKNCOMBI-MAX)
                   MOVE    THKNCOMBI-MAX   TO  IDXA
               END-IF
           ELSE
                   MOVE    THKNCOMBI-MAX   TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( ACCT-DAY (1 IDX-DAY)    >   ZERO )
                       MOVE    ACCT-SRYYM  TO  WRK-YMD (1:6)
                       MOVE    IDX-DAY     TO  WRK-DD
      *
                       IF    ( THKNCOMBI-STYMD (IDXA)  >   WRK-YMD )
                           MOVE    WRK-YMD TO  THKNCOMBI-STYMD (IDXA)
                       END-IF
      *
                       IF    ( THKNCOMBI-EDYMD (IDXA)  <   WRK-YMD )
                           MOVE    WRK-YMD TO  THKNCOMBI-EDYMD (IDXA)
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200111-THKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報退避処理
      *****************************************************************
       200111-SRYACT-TAIHI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOSAISIN
      *
      *    点数マスタ検索時の基準日を求める
           MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
           MOVE    01                  TO  WRK-TNS-DD
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
               IF    ( ACCT-DAY (1 IDX0 )  >   ZERO )
                   MOVE    IDX0        TO  WRK-TNS-DD
                   MOVE    31          TO  IDX0
               END-IF
           END-PERFORM
      *
      *
           MOVE    ZERO                TO  FLG-SRYACT
      *
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   5 )
                        OR   ( SRY-SRYCD (IDX1)    =   SPACE )
      *
      *            処理対象診療行為チェック処理
                   PERFORM 200111-TAISO-SRY-CHK-SEC
      *
                   IF    ( FLG-TAISYO-SRY  =   1 )
      *
      *                点数マスタ検索処理
                       MOVE    SRY-SRYCD (IDX1)
                                       TO  WRK-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
      *
                       IF    ( FLG-TSRYKA-OVER =   ZERO )
                        AND  ( IDX-TSRYKA      >   ZERO )
      *                    診療科退避ワーク診療行為コード編集処理
                           PERFORM 200111-TSRYKA-SRYCD-HEN-SEC
      *                    診療科退避ワーク初診日編集処理
                           PERFORM 200111-TSRYKA-SHOSIN-HEN-SEC
                       END-IF
      *
      *                診療行為コード退避ワーク編集処理
                       PERFORM 200111-TSRYCD-HEN-SEC
      *
      *                減算情報ワーク診療行為退避処理
                       IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                           PERFORM 200111-TGENZAN-SRY-HEN-SEC
                       END-IF
      *
      *                実日数退避ワーク編集処理
                       IF    ( FLG-SHOSAISIN   =   ZERO )
                               PERFORM 200111-TJNISSU-HEN-SEC
                       END-IF
      *
      *                算定回数退避ワーク編集処理
                       IF    ( TNS-JITUDAY     =   1 )
                        AND  ( TNS-DAYCNT      =   0 )
                           PERFORM 200111-TKAISU-HEN-SEC
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
      *
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SRYACT-REC
               ELSE
                   MOVE    1               TO  FLG-SRYACT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200111-SRYACT-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    処理対象診療行為チェック処理
      *****************************************************************
       200111-TAISO-SRY-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TAISYO-SRY
      *
      *    約束セットのコードは読み飛ばす
           IF    ( SRY-SRYCD (IDX1) (1:1)  =   "S" )
               GO  TO  200111-TAISO-SRY-CHK-EXT
           END-IF
      *
      *    院内処方包括入力分はチェックの対象外とする
           IF    ( SRY-SRYSYUKBN       =   "213" OR "223" OR "233" )
               GO  TO  200111-TAISO-SRY-CHK-EXT
           END-IF
      *
      *    薬剤の場合、院外処方の処理対象チェックを行う
           IF    ( SRY-SRYCD (IDX1) (1:1)  =   "6" )
               EVALUATE    ORCBSD01-INGAIPROCKBN
      *        院外処方を含まない
               WHEN    ZERO
                   IF    ( ACCT-ZAITEN NOT =   ZERO )
                       MOVE    1       TO  FLG-TAISYO-SRY
                   END-IF
      *        院外処方を含む
               WHEN    "1"
                       MOVE    1       TO  FLG-TAISYO-SRY
      *        院外処方のみ
               WHEN    "2"
                   IF    ( ACCT-ZAITEN =   ZERO )
                       MOVE    1       TO  FLG-TAISYO-SRY
                   END-IF
               END-EVALUATE
           ELSE
      *        薬剤以外の場合
                       MOVE    1       TO  FLG-TAISYO-SRY
           END-IF
      *
           .
       200111-TAISO-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク診療科編集処理
      *****************************************************************
       200111-TSRYKA-SRYKA-HEN-SEC     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYKA-MAX )
                    OR   ( IDX0    >   CONST-TSRYKA-MAX )
                    OR   ( ACCT-SRYKA
                                   =   TSRYKA-SRYKA (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TSRYKA-MAX )
      *
               COMPUTE TSRYKA-MAX  =   TSRYKA-MAX  +   1
      *
               IF    ( TSRYKA-MAX  >   CONST-TSRYKA-MAX )
                   MOVE    CONST-TSRYKA-MAX
                                       TO  TSRYKA-MAX
                   MOVE    1           TO  FLG-TSRYKA-OVER
               ELSE
                   MOVE    TSRYKA-MAX  TO  IDX-TSRYKA
                   MOVE    ACCT-SRYKA  TO  TSRYKA-SRYKA
                                                       (TSRYKA-MAX)
               END-IF
           ELSE
               MOVE    IDX0            TO  IDX-TSRYKA
           END-IF
      *
           .
       200111-TSRYKA-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク診療行為コード編集処理
      *****************************************************************
       200111-TSRYKA-SRYCD-HEN-SEC     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYKA-SRYCD-MAX (IDX-TSRYKA) )
                    OR   ( IDX0    >   CONST-TSRYKA-SRYCD-MAX )
                    OR   ( TSRYKA-SRYCD (IDX-TSRYKA IDX0)
                                   =   SRY-SRYCD (IDX1) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TSRYKA-SRYCD-MAX (IDX-TSRYKA) )
      *
               COMPUTE TSRYKA-SRYCD-MAX (IDX-TSRYKA)
                   =   TSRYKA-SRYCD-MAX (IDX-TSRYKA)  +   1
      *
               IF    ( TSRYKA-SRYCD-MAX (IDX-TSRYKA)
                                   >   CONST-TSRYKA-SRYCD-MAX )
                   MOVE    CONST-TSRYKA-SRYCD-MAX
                                   TO  TSRYKA-SRYCD-MAX (IDX-TSRYKA)
               ELSE
                   MOVE    TSRYKA-SRYCD-MAX (IDX-TSRYKA)
                                   TO  WRK-TSRYKA-SRYCD-MAX
                   MOVE    SRY-SRYCD (IDX1)
                                   TO  TSRYKA-SRYCD
                                       (IDX-TSRYKA WRK-TSRYKA-SRYCD-MAX)
               END-IF
           END-IF
      *
           .
       200111-TSRYKA-SRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク初診日編集処理
      *****************************************************************
       200111-TSRYKA-SHOSIN-HEN-SEC    SECTION.
      *
           IF    ( TNS-JITUDAY     =   2 )
            AND  ( TNS-DAYCNT      =   1 )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
      *
                   IF    ( ACCT-DAY (1 IDX2)   >   ZERO )
      *
                       MOVE    1   TO  TSRYKA-SHOSIN-DAY
                                                 (IDX-TSRYKA IDX2)
      *
                       MOVE    ACCT-SRYYM  TO  WRK-SYOSIN-CHK-YMD
                       MOVE    IDX2        TO  WRK-SYOSIN-CHK-DD
      *
                       EVALUATE    COMB-HKNNUM
                       WHEN    "971"
                           MOVE    WRK-SYOSIN-CHK-YMD
                                           TO  TSRYKA-SHOSIN-RSI-YMD
                                                        (IDX-TSRYKA)
                       WHEN    "973"
                           MOVE    WRK-SYOSIN-CHK-YMD
                                           TO  TSRYKA-SHOSIN-JBI-YMD
                                                        (IDX-TSRYKA)
                       WHEN    OTHER
                           MOVE    WRK-SYOSIN-CHK-YMD
                                           TO  TSRYKA-SHOSIN-IHO-YMD
                                                        (IDX-TSRYKA)
                       END-EVALUATE
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200111-TSRYKA-SHOSIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード退避ワーク編集処理
      *****************************************************************
       200111-TSRYCD-HEN-SEC           SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYCD-MAX )
                    OR   ( IDX0    >   CONST-TSRYCD-MAX )
                    OR   ( TSRYCD-SRYCD (IDX0)
                                   =   SRY-SRYCD (IDX1) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TSRYCD-MAX )
      *
               COMPUTE TSRYCD-MAX  =   TSRYCD-MAX  +   1
      *
               IF    ( TSRYCD-MAX  >   CONST-TSRYCD-MAX )
                   MOVE    CONST-TSRYCD-MAX
                                   TO  TSRYCD-MAX
               ELSE
                   MOVE    SRY-NYUGAIKBN
                                   TO  TSRYCD-NYUGAIKBN (TSRYCD-MAX)
                   MOVE    SRY-SRYCD (IDX1)
                                   TO  TSRYCD-SRYCD (TSRYCD-MAX)
               END-IF
           ELSE
      *
      *        処理時の入外区分を優先して設定する
               IF    ( SRY-NYUGAIKBN   =   ORCBSD01-NYUGAIKBN )
                   MOVE    SRY-NYUGAIKBN
                                   TO  TSRYCD-NYUGAIKBN (IDX0)
               END-IF
           END-IF
      *
           .
       200111-TSRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    減算情報ワーク診療行為退避処理
      *****************************************************************
       200111-TGENZAN-SRY-HEN-SEC      SECTION.
      *
           EVALUATE    SRY-SRYCD (IDX1)
      *
      *    院内感染防止対策未実施減算
           WHEN    "190080570"
           WHEN    "190799770"
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( ACCT-DAY (1 IDX0)   >   ZERO )
                       MOVE    1       TO  TGENZAN-INNAI-SRY-DAY (IDX0)
                   END-IF
               END-PERFORM
      *
      *    医療安全管理体制未整備減算
           WHEN    "190111690"
           WHEN    "190809090"
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( ACCT-DAY (1 IDX0)   >   ZERO )
                       MOVE    1       TO  TGENZAN-ANZEN-SRY-DAY (IDX0)
                   END-IF
               END-PERFORM
      *
      *    褥瘡対策未実施減算
           WHEN    "190111790"
           WHEN    "190809190"
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( ACCT-DAY (1 IDX0)   >   ZERO )
                       MOVE    1       TO  TGENZAN-JOKUSO-SRY-DAY (IDX0)
                   END-IF
               END-PERFORM
           END-EVALUATE
      *
           .
       200111-TGENZAN-SRY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワーク編集処理
      *****************************************************************
       200111-TJNISSU-HEN-SEC      SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
      *    実日数算定チェック
           PERFORM 200111-JNISSU-HEN-SEC
           IF    ( FLG-SHOSAISIN       =   ZERO )
               GO  TO  200111-TJNISSU-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TJNISSU-MAX )
                    OR   ( IDX0    >   CONST-TJNISSU-MAX )
                    OR   ( TJNISSU-HKNNUM (IDX0)
                                   =   COMB-HKNNUM )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TJNISSU-MAX )
      *
               COMPUTE TJNISSU-MAX =   TJNISSU-MAX +   1
      *
               IF    ( TJNISSU-MAX >   CONST-TJNISSU-MAX )
                   MOVE    CONST-TJNISSU-MAX
                                   TO  TJNISSU-MAX
               ELSE
                   MOVE    COMB-HKNNUM
                                   TO  TJNISSU-HKNNUM (TJNISSU-MAX)
                   MOVE    TJNISSU-MAX
                                   TO  IDXA
               END-IF
           ELSE
               MOVE    IDX0        TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
                   IF    ( ACCT-DAY (1 IDX2)   >   ZERO )
                       MOVE    1   TO  TJNISSU-DAY (IDXA IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-TJNISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    実日数算定チェック
      *****************************************************************
       200111-JNISSU-HEN-SEC           SECTION.
      *
           IF   (( ACCT-ZAITEN     NOT =   ZERO )
             AND ( ACCT-ZAIKAISU   NOT =   ZERO )
             AND ( ACCT-SRYKBN         =   "11"
                                       OR  "12" 
                                       OR  "13"
                                       OR  "14" ))
           OR   (( ACCT-ZAITEN         =   ZERO )
             AND ( ACCT-ZAIKAISU   NOT =   ZERO )
             AND(  ACCT-SRYKBN         =   "11"
                                      OR   "12"
                                      OR   "99" ))
               IF   (( TNS-JITUDAY     =   2   )
                AND  ( TNS-DAYCNT      =   1   OR  2 ))
                  OR ( SRY-SRYCD(IDX1) =   "112700850"
                                       OR  "112003350"
                                       OR  "099409901"
                                       OR  "099999902"
                                       OR  "099110001"
                                       OR  "099120001"
      *                        同日再診
                                       OR  "112010150"
                                       OR  "112010250"
                                       OR  "112010350"
                                       OR  "112010450"
                                       OR  "112010550"
                                       OR  "112704450"
                                       OR  "112704550"
                                       OR  "112704650"
                                       OR  "112704750"
                                       OR  "112010650"
                                       OR  "112010750"
                                       OR  "112010850"
                                       OR  "112010950"
                                       OR  "112012850"
                                       OR  "112704850"
                                       OR  "112704950"
                                       OR  "112705050"
                                       OR  "112706850"
                                       OR  "112008850"
                                       OR  "112008950"
                                       OR  "112009050"
                                       OR  "112009150"
                                       OR  "112012550"
                                       OR  "112703450"
                                       OR  "112703550"
                                       OR  "112703650"
                                       OR  "112706550"
                                       OR  "112008350"
                                       OR  "112008450"
                                       OR  "112008550"
                                       OR  "112008650"
                                       OR  "112008750"
                                       OR  "112703050"
                                       OR  "112703150"
                                       OR  "112703250"
                                       OR  "112703350"
      *
                                       OR  "112011710"
                                       OR  "112011810"
                                       OR  "112011910"
                                       OR  "112012010"
                                       OR  "112012210"
                                       OR  "112705850"
                                       OR  "112705950"
                                       OR  "112706050" )
      *
                   MOVE    1           TO    FLG-SHOSAISIN
      *
               END-IF
           END-IF
      *
           .
       200111-JNISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワーク編集処理（入院）
      *****************************************************************
       200111-TJNISSU-NYU-HEN-SEC      SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TJNISSU-MAX )
                    OR   ( IDX0    >   CONST-TJNISSU-MAX )
                    OR   ( TJNISSU-HKNNUM (IDX0)
                                   =   COMB-HKNNUM )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TJNISSU-MAX )
      *
               COMPUTE TJNISSU-MAX =   TJNISSU-MAX +   1
      *
               IF    ( TJNISSU-MAX >   CONST-TJNISSU-MAX )
                   MOVE    CONST-TJNISSU-MAX
                                   TO  TJNISSU-MAX
               ELSE
                   MOVE    COMB-HKNNUM
                                   TO  TJNISSU-HKNNUM (TJNISSU-MAX)
                   MOVE    TJNISSU-MAX
                                   TO  IDXA
               END-IF
           ELSE
               MOVE    IDX0        TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
                   IF    ( WRK-HKNCOMBI-DAY (IDX2)       >   ZERO )
                       IF    ( TNACCT-KHNRYO-DAY (IDX2)  >   ZERO )
                           MOVE    1   TO  TJNISSU-DAY (IDXA IDX2)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-TJNISSU-NYU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク編集処理
      *****************************************************************
       200111-TKAISU-HEN-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-TKAISU
           MOVE    ZERO            TO  FLG-TKAISU-OVER
      *
      *    算定回数退避ワーク保険の種類編集処理
           PERFORM 200111-TKAISU-HKNNUM-HEN-SEC
      *
      *    算定回数退避ワーク診療行為コード編集処理
           IF    ( FLG-TKAISU-OVER =   ZERO )
            AND  ( IDX-TKAISU      >   ZERO )
               PERFORM 200111-TKAISU-SRYCD-HEN-SEC
           END-IF
      *
           .
       200111-TKAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク保険の種類編集処理
      *****************************************************************
       200111-TKAISU-HKNNUM-HEN-SEC    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TKAISU-MAX )
                    OR   ( IDX0    >   CONST-TKAISU-MAX )
                    OR   ( COMB-HKNNUM
                                   =   TKAISU-HKNNUM (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TKAISU-MAX )
      *
               COMPUTE TKAISU-MAX  =   TKAISU-MAX  +   1
      *
               IF    ( TKAISU-MAX  >   CONST-TKAISU-MAX )
                   MOVE    CONST-TKAISU-MAX
                                       TO  TKAISU-MAX
                   MOVE    1           TO  FLG-TKAISU-OVER
               ELSE
                   MOVE    TKAISU-MAX  TO  IDX-TKAISU
                   MOVE    COMB-HKNNUM TO  TKAISU-HKNNUM
                                                   (TKAISU-MAX)
               END-IF
           ELSE
               MOVE    IDX0            TO  IDX-TKAISU
           END-IF
      *
           .
       200111-TKAISU-HKNNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク診療行為コード編集処理
      *****************************************************************
       200111-TKAISU-SRYCD-HEN-SEC    SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TKAISU-SRYCD-MAX (IDX-TKAISU) )
                    OR   ( IDX0    >   CONST-TKAISU-SRYCD-MAX )
                    OR   ( TKAISU-SRYCD (IDX-TKAISU IDX0)
                                   =   SRY-SRYCD (IDX1) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TKAISU-SRYCD-MAX (IDX-TKAISU) )
      *
               COMPUTE TKAISU-SRYCD-MAX (IDX-TKAISU)
                   =   TKAISU-SRYCD-MAX (IDX-TKAISU)  +   1
      *
               IF    ( TKAISU-SRYCD-MAX (IDX-TKAISU)
                                   >   CONST-TKAISU-SRYCD-MAX )
                   MOVE    CONST-TKAISU-SRYCD-MAX
                                   TO  TKAISU-SRYCD-MAX (IDX-TKAISU)
               ELSE
                   MOVE    TKAISU-SRYCD-MAX (IDX-TKAISU)
                                   TO  WRK-TKAISU-SRYCD-MAX
                   MOVE    SRY-SRYCD (IDX1)
                                   TO  TKAISU-SRYCD
                                       (IDX-TKAISU WRK-TKAISU-SRYCD-MAX)
                   MOVE    WRK-TKAISU-SRYCD-MAX
                                   TO  IDXA
               END-IF
           ELSE
               MOVE    IDX0        TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
                   IF    ( ACCT-DAY (1 IDX2)   >   ZERO )
                       MOVE    1   TO  TKAISU-SRYCD-DAY
                                               (IDX-TKAISU IDXA IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-TKAISU-SRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算退避ワーク編集処理
      *****************************************************************
       200111-TNYUKSN-HEN-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TNYUKSN-MAX )
                    OR   ( ACCT-HKNCOMBI
                                   =   TNYUKSN-HKNCOMBI (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TNYUKSN-MAX )
      *
               COMPUTE TNYUKSN-MAX
                   =   TNYUKSN-MAX +   1
      *
               IF    ( TNYUKSN-MAX >   CONST-TNYUKSN-MAX )
                   MOVE    CONST-TNYUKSN-MAX
                                   TO  TNYUKSN-MAX
               ELSE
                   MOVE    ACCT-HKNCOMBI
                                   TO  TNYUKSN-HKNCOMBI (TNYUKSN-MAX)
                   MOVE    TNYUKSN-MAX
                                   TO  IDXA
               END-IF
           ELSE
               MOVE    IDX0        TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
                   IF    ( ACCT-DAY (1 IDX2)   >   ZERO )
                       MOVE    1   TO  TNYUKSN-HKNCOMBI-DAY (IDXA IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-TNYUKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク診療行為コード編集処理
      *****************************************************************
       200111-TNYUIN-ACCT-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
               IF    ( ACCT-DAY (1 IDX0 )  >   ZERO )
                   MOVE    1       TO  TNYUIN-ACCT-DAY (IDX0)
               END-IF
      *
           END-PERFORM
      *
           .
       200111-TNYUIN-ACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料算定日退避ワーク編集処理
      *****************************************************************
       200111-TNYUIN-CHOZAI-HEN-SEC    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
               IF    ( ACCT-DAY (1 IDX0 )  >   ZERO )
                   MOVE    1       TO  TNYUIN-CHOZAI-DAY (IDX0)
               END-IF
      *
           END-PERFORM
      *
           .
       200111-TNYUIN-CHOZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報退避処理
      *****************************************************************
       200112-SRYACCT-TAIHI-SEC        SECTION.
      *
           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
               MOVE    "2"             TO  WRK-NYUGAIKBN
           ELSE
               MOVE    "1"             TO  WRK-NYUGAIKBN
           END-IF
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    DTCHK003-HOSPID     TO  ACCT-HOSPID
           MOVE    WRK-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    DTCHK003-PTID       TO  ACCT-PTID
           MOVE    ORCBSD01-SRYYM      TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "SRYACCT-KEY28"     TO  ORC-DBPATH
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-SRYACCT-KEY28-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE    SPACE               TO  KEY-HKNCOMBI
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
      *
      *        主保険チェック
               IF    ( ACCT-HKNCOMBI   NOT =   KEY-HKNCOMBI )
      *
                   MOVE    ACCT-HKNCOMBI   TO  KEY-HKNCOMBI
                   MOVE    KEY-HKNCOMBI    TO  WRK-HKNCOMBI
                   PERFORM 200111-SYUHOKEN-CHK-SEC
               END-IF
      *
      *        診療行為情報退避処理
               IF    ( FLG-TAISYO-HKN      =   1 )
                   PERFORM 200112-SRYACT-TAIHI-SEC
               END-IF
      *
               PERFORM 900-SRYACCT-KEY28-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "SRYACCT-KEY28"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200112-SRYACCT-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報退避処理
      *****************************************************************
       200112-SRYACT-TAIHI-SEC         SECTION.
      *
      *    点数マスタ検索時の基準日を求める
           MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
           MOVE    01                  TO  WRK-TNS-DD
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
               IF    ( ACCT-DAY (1 IDX0 )  >   ZERO )
                   MOVE    IDX0        TO  WRK-DD
                   MOVE    31          TO  IDX0
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-SRYACT
      *
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   5 )
                        OR   ( SRY-SRYCD (IDX1)    =   SPACE )
      *
      *            処理対象診療行為チェック処理
                   PERFORM 200111-TAISO-SRY-CHK-SEC
      *
                   IF    ( FLG-TAISYO-SRY  =   1 )
      *
      *                点数マスタ検索処理
                       MOVE    SRY-SRYCD (IDX1)
                                       TO  WRK-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
      *
      *                診療行為コード退避ワーク編集処理
                       PERFORM 200111-TSRYCD-HEN-SEC
      *
                   END-IF
      *
               END-PERFORM
      *
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SRYACT-REC
               ELSE
                   MOVE    1               TO  FLG-SRYACT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200112-SRYACT-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料退避処理
      *****************************************************************
       200111-NACCT-KHNRYO-TAIHI-SEC   SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    DTCHK003-HOSPID         TO  NACCT-HOSPID
           MOVE    DTCHK003-PTID           TO  NACCT-PTID
           MOVE    ORCBSD01-SRYYM          TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-SRYCDTOTAL    NOT =    099999916 )
      *
      *            入院料を算定している日にちを退避
                   PERFORM VARYING     IDX-DAY FROM    1   BY  1
                           UNTIL     ( IDX-DAY >   31 )
      *
                       IF   ( NACCT-DAY(IDX-DAY)   >   ZERO )
                           MOVE    1   TO  TNACCT-KHNRYO-DAY (IDX-DAY)
                       END-IF
      *
                   END-PERFORM
      *
               END-IF
      *
               MOVE    "NYUINACCT-KEY17"   TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               ELSE
                  MOVE    1                TO  FLG-NYUINACCT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200111-NACCT-KHNRYO-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    入院保険組合せ退避処理
      *****************************************************************
       200111-NACCT-COMBI-TAIHI-SEC    SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    DTCHK003-HOSPID         TO  NACCT-HOSPID
           MOVE    DTCHK003-PTID           TO  NACCT-PTID
           MOVE    ORCBSD01-SRYYM          TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               GO  TO  200111-NACCT-COMBI-TAIHI-EXT
           END-IF
      *
           PERFORM VARYING     IDX-DAY   FROM    1   BY  1
                   UNTIL     ( IDX-DAY   >   31 )
      *
               MOVE    NACCT-DAY (IDX-DAY)
                                   TO  TNACCT-HKNCOMBI-DAY (IDX-DAY)
      *
               IF    ( NACCT-DAY (IDX-DAY) NOT =   ZERO )
      *
                   MOVE   ZERO             TO  IDXA
      *
                   PERFORM VARYING IDX15   FROM    1   BY  1
                       UNTIL ( IDX15   >   TNACCT-HKNCOMBI-MAX )
                        OR   ( IDX15   >   CONST-TNACCT-HKNCOMBI-MAX )
                        OR   ( NACCT-DAY (IDX-DAY)
                               =   TNACCT-HKNCOMBI-HKNCOMBI (IDX15) )
                       CONTINUE
                   END-PERFORM
      *
                   IF    (   IDX15     >   TNACCT-HKNCOMBI-MAX )
                       COMPUTE TNACCT-HKNCOMBI-MAX
                           =   TNACCT-HKNCOMBI-MAX +   1
                       IF  ( TNACCT-HKNCOMBI-MAX
                                       >   CONST-TNACCT-HKNCOMBI-MAX )
                           MOVE    CONST-TNACCT-HKNCOMBI-MAX
                                       TO  TNACCT-HKNCOMBI-MAX
                       ELSE
                           MOVE    NACCT-DAY (IDX-DAY)
                                   TO  TNACCT-HKNCOMBI-HKNCOMBI
                                                  (TNACCT-HKNCOMBI-MAX)
                           MOVE    "99999999"
                                   TO  TNACCT-HKNCOMBI-STYMD
                                                  (TNACCT-HKNCOMBI-MAX)
                           MOVE    "00000000"
                                   TO  TNACCT-HKNCOMBI-EDYMD
                                                  (TNACCT-HKNCOMBI-MAX)
      *
                           MOVE    TNACCT-HKNCOMBI-MAX     TO  IDXA
                       END-IF
                   ELSE
      *
                           MOVE    IDX15                   TO  IDXA
                   END-IF
      *
                   IF    ( IDXA    >   ZERO )
                       MOVE    ORCBSD01-SRYYM  TO  WRK-YMD (1:6)
                       MOVE    IDX-DAY         TO  WRK-DD
                       IF    ( TNACCT-HKNCOMBI-STYMD (IDXA)
                                               >   WRK-YMD )
                           MOVE    WRK-YMD
                                   TO  TNACCT-HKNCOMBI-STYMD (IDXA)
                       END-IF
      *
                       IF    ( TNACCT-HKNCOMBI-EDYMD (IDXA)
                                               <   WRK-YMD )
                           MOVE    WRK-YMD
                                   TO  TNACCT-HKNCOMBI-EDYMD (IDXA)
                       END-IF
                   END-IF
      *
                   MOVE    1       TO  TNACCT-HKNCOMBI-HKNCOMBI-DAY
                                           (IDXA IDX-DAY)
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-NACCT-COMBI-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    データチェック処理
      *****************************************************************
       20011-DATA-CHK-SEC              SECTION.
      *
      *    データチェック初期処理
           PERFORM 20011-DATA-CHK-INIT-SEC
      *
      *    基本チェック処理
           PERFORM 200111-KIHON-CHK-SEC
      *
      *    患者保険情報チェック処理
           IF    ( ORCBSD01-CHKKBN (1) NOT =   ZERO  )
            OR   ( ORCBSD01-CHKKBN (2) NOT =   ZERO  )
               PERFORM 200111-PTHKNINF-CHK-SEC
           END-IF
      *
      *    患者公費情報チェック処理
           IF    ( ORCBSD01-CHKKBN (3) NOT =   ZERO  )
               PERFORM 200111-PTKOHINF-CHK-SEC
           END-IF
      *
      *    保険適応年齢チェック
           IF    ( ORCBSD01-CHKKBN (4) NOT =   ZERO  )
               PERFORM 200111-HKNAGE-CHK-SEC
           END-IF
      *
      *
      *    患者病名情報チェック処理
           IF    ( ORCBSD01-CHKKBN (5) NOT =   ZERO  )
            OR   ( ORCBSD01-CHKKBN (6) NOT =   ZERO  )
            OR   ( ORCBSD01-CHKKBN (7) NOT =   ZERO  )
            OR   ( ORCBSD01-CHKKBN (8) NOT =   ZERO  )
            OR   ( ORCBSD01-CHKKBN (9) NOT =   ZERO  )
               PERFORM 200111-PTBYOMEI-CHK-SEC
           END-IF
      *
      *    併用禁忌の薬剤・併用算定の診療行為チェック処理
           IF    ( ORCBSD01-CHKKBN(10) NOT =   ZERO  )
              OR ( ORCBSD01-CHKKBN(11) NOT =   ZERO  )
               PERFORM 200111-HEIYOU-CHK-SEC
           END-IF
      *
      *    実日数チェック処理
           IF    ( ORCBSD01-CHKKBN(12) NOT =   ZERO  )
               PERFORM 200111-JNISSU-CHK-SEC
           END-IF
      *
      *    患者氏名チェック処理
           IF    ( ORCBSD01-CHKKBN(13) NOT =   ZERO  )
               PERFORM 200111-PTNAME-CHK-SEC
           END-IF
      *
      *    保険組合せチェック処理
           IF    ( ORCBSD01-CHKKBN(14) NOT =   ZERO  )
               PERFORM 200111-HKNCOMBI-CHK-SEC
           END-IF
      *
      *    その他必須チェック処理
           PERFORM 200111-SONOTA-HISSU-CHK-SEC
      *
           .
       20011-DATA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    データチェック初期処理
      *****************************************************************
       20011-DATA-CHK-INIT-SEC         SECTION.
      *
           INITIALIZE                  FLG-CHKEND-G
                                       CNT-CHKERR
      *
      *    患者番号取得処理
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
      *    患者情報取得処理
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    保険組み合わせ
           INITIALIZE                  ORCSHKNSETAREA
           INITIALIZE                  SPA-AREA
      *
           MOVE    PTINF-HOSPID    TO  SPA-HOSPID
           MOVE    PTINF-PTID      TO  SPA-PTID
           MOVE    PTINF-BIRTHDAY  TO  SPA-BIRTHDAY
           MOVE    WRK-KJNYMD      TO  SPA-SRYYMD
      *
           MOVE    ORCBSD01-NYUGAIKBN
                                   TO  SPA-NYUGAIKBN
           MOVE    "2"             TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"    USING   SPA-AREA
                                           ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           .
       20011-DATA-CHK-INIT-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200111-KIHON-CHK-SEC            SECTION.
      *
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "000001"    TO  WRK-ERRCD
               PERFORM 210-ERRMES-HEN-SEC
               MOVE    1           TO  FLG-CHKEND (13)
           END-IF
      *
           .
       200111-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報チェック処理
      *****************************************************************
       200111-PTHKNINF-CHK-SEC         SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (1) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (1)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (2) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (2)
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   THKNID-MAX )
      *
      *        患者保険情報検索処理
               MOVE    THKNID-HKNID (IDX4)
                                       TO  WRK-HKNID
               PERFORM 900-PTHKNINF-KEY-SEL-SEC
      *
      *        保険者番号抜けチェック
               IF    ( FLG-CHKEND (1)          =   ZERO )
                   PERFORM 200111-HKNNUM-CHK-SEC
               END-IF
      *
      *        被保険者記号・番号抜け
               IF    ( FLG-CHKEND (2)          =   ZERO )
                   PERFORM 200111-KIGONUM-CHK-SEC
               END-IF
      *
               IF    ( FLG-CHKEND (1)  =   1 )
                AND  ( FLG-CHKEND (2)  =   1 )
                   MOVE    THKNID-MAX  TO  IDX4
               END-IF
      *
           END-PERFORM
      *
           .
       200111-PTHKNINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号抜けチェック
      *****************************************************************
       200111-HKNNUM-CHK-SEC           SECTION.
      *
           IF    ( PTHKN-HKNJANUM      =   SPACE )
      *
               MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
               MOVE    "010001"        TO  WRK-ERRCD
               PERFORM 211-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (1) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (1)
                   GO  TO  200111-HKNNUM-CHK-EXT
               END-IF
      *
           END-IF
      *
           .
       200111-HKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    被保険者記号・番号抜けチェック
      *****************************************************************
       200111-KIGONUM-CHK-SEC          SECTION.
      *
           MOVE    PTHKN-KIGO          TO  WRK-PTHKN-KIGO
           MOVE    PTHKN-NUM           TO  WRK-PTHKN-NUM
      *
           INSPECT WRK-PTHKN-KIGO      REPLACING   ALL "　"    BY  "  "
           INSPECT WRK-PTHKN-NUM       REPLACING   ALL "　"    BY  "  "
      *
           IF    ( WRK-PTHKN-KIGO      =   SPACE )
            AND  ( WRK-PTHKN-NUM       =   SPACE )
      *
               MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
               MOVE    "020001"        TO  WRK-ERRCD
               PERFORM 211-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (2)
                   GO  TO  200111-KIGONUM-CHK-EXT
               END-IF
      *
           END-IF
      *
           IF    ( PTHKN-KIGO          NOT =   SPACE )
               MOVE    PTHKN-KIGO      TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK       NOT =   2 )
      *
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "020002"        TO  WRK-ERRCD
                   PERFORM 211-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (2)
                       GO  TO  200111-KIGONUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( PTHKN-NUM           NOT =   SPACE )
               MOVE    PTHKN-NUM       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK       NOT =   2 )
      *
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "020003"        TO  WRK-ERRCD
                   PERFORM 211-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (2)
                       GO  TO  200111-KIGONUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    レセ電の場合、文字列長チェック
           IF    ( SYS-2005-KKHRECEKBN =   "1" )
            OR   ( SYS-2005-SYARECEKBN =   "1" )
      *
               MOVE    SPACE               TO  WRK-KIGONUM-STR
               STRING  PTHKN-KIGO              DELIMITED   BY  SPACE
                       PTHKN-NUM               DELIMITED   BY  SPACE
               INTO    WRK-KIGONUM-STR
               END-STRING
      *
               IF    ( WRK-KIGONUM-STR (39:)   NOT =   SPACE )
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "020004"        TO  WRK-ERRCD
                   PERFORM 211-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (2)
                       GO  TO  200111-KIGONUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-KIGONUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報チェック処理
      *****************************************************************
       200111-PTKOHINF-CHK-SEC         SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (3) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (3)
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   TKOHID-MAX )
      *
      *        患者公費情報検索処理
               MOVE    TKOHID-KOHID (IDX4)
                                       TO  WRK-KOHID
               PERFORM 900-PTKOHINF-KEY-SEL-SEC
      *
      *        負担者番号・受給者番号抜け
               IF    ( FLG-CHKEND (3)          =   ZERO )
      *
      *            長期以外を対象とする
                   IF    ( PTKOH-KOHNUM    NOT =   "972" )
                        PERFORM 200111-FTNJKYNUM-CHK-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-CHKEND (3)  =   1 )
                   MOVE    TKOHID-MAX  TO  IDX4
               END-IF
      *
           END-PERFORM
      *
           .
       200111-PTKOHINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号・受給者番号抜けチェック
      *****************************************************************
       200111-FTNJKYNUM-CHK-SEC        SECTION.
      *
           MOVE    PTKOH-KOHNUM        TO  SRH-1040-KOHNUM
           PERFORM 2111-SYS1040-GET-SEC
      *
           MOVE    PTKOH-FTNJANUM      TO  WRK-PTKOH-FTNJANUM
           MOVE    PTKOH-JKYSNUM       TO  WRK-PTKOH-JKYSNUM
      *
           INSPECT WRK-PTKOH-FTNJANUM  REPLACING   ALL "　"    BY  "  "
           INSPECT WRK-PTKOH-JKYSNUM   REPLACING   ALL "　"    BY  "  "
      *
           IF    ( SRH-1040-FTNJANUMCHK    =   "1" )
               CONTINUE
           ELSE
               IF    ( WRK-PTKOH-FTNJANUM  =   SPACE )
      *
                   MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                   MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                   MOVE    "030001"        TO  WRK-ERRCD
                   PERFORM 212-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (3)
                       GO  TO  200111-FTNJKYNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SRH-1040-JKYSNUMCHK =   "1" )
            OR   ( PTKOH-KOHNUM        =   "012" )
               CONTINUE
           ELSE
               IF    ( WRK-PTKOH-JKYSNUM   =   SPACE )
      *
                   MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                   MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                   MOVE    "030002"        TO  WRK-ERRCD
                   PERFORM 212-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (3)
                       GO  TO  200111-FTNJKYNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( PTKOH-FTNJANUM      NOT =   SPACE )
               MOVE    PTKOH-FTNJANUM  TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK       =   9 )
      *
                   MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                   MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                   MOVE    "030003"        TO  WRK-ERRCD
                   PERFORM 212-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (3)
                       GO  TO  200111-FTNJKYNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( PTKOH-JKYSNUM       NOT =   SPACE )
            AND  ( PTKOH-KOHNUM        NOT =   "012" )
               MOVE    PTKOH-JKYSNUM   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK       =   9 )
      *
                   MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                   MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                   MOVE    "030004"        TO  WRK-ERRCD
                   PERFORM 212-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (3)
                       GO  TO  200111-FTNJKYNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-FTNJKYNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険適応年齢チェック処理
      *****************************************************************
       200111-HKNAGE-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (4) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (4)
           END-IF
      *
      *    診療会計の保険組合せをチェック
           MOVE    ZERO                TO  FLG-PROC
           PERFORM VARYING IDX20   FROM    1   BY  1
                   UNTIL ( IDX20   >   THKNCOMBI-MAX )
                    OR   ( FLG-CHKEND (4)  =   1 )
      *
               MOVE    THKNCOMBI-HKNCOMBI (IDX20)
                                       TO  WRK-HKNCOMBI
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
               IF    ( COMB-HKNID      NOT =   ZERO )
                AND  ( COMB-HKNNUM     NOT =   "971" )
                AND  ( COMB-HKNNUM     NOT =   "973" )
                AND  ( COMB-HKNNUM (1:2)
                                       NOT =   "98" )
      *
                   PERFORM 200112-HKNAGE-CHK-SEC
      *
               END-IF
      *
           END-PERFORM
      *
      *    入院会計の保険組合せをチェック
           MOVE    1                   TO  FLG-PROC
           PERFORM VARYING IDX20   FROM    1   BY  1
                   UNTIL ( IDX20   >   TNACCT-HKNCOMBI-MAX )
                    OR   ( FLG-CHKEND (4)  =   1 )
      *
               MOVE    TNACCT-HKNCOMBI-HKNCOMBI (IDX20)
                                       TO  WRK-HKNCOMBI
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
               IF    ( COMB-HKNID      NOT =   ZERO )
                AND  ( COMB-HKNNUM     NOT =   "971" )
                AND  ( COMB-HKNNUM     NOT =   "973" )
                AND  ( COMB-HKNNUM (1:2)
                                       NOT =   "98" )
      *
                   PERFORM 200112-HKNAGE-CHK-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-HKNAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険適応年齢チェック処理
      *****************************************************************
       200112-HKNAGE-CHK-SEC           SECTION.
      *
      *    年齢計算
           INITIALIZE ORCSAGECHKAREA
           MOVE    DTCHK003-HOSPID     TO  AGECHK-HOSPID
           MOVE    DTCHK003-PTID       TO  AGECHK-PTID
           MOVE    WRK-KJNYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    COMB-HKNCOMBINUM    TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
           MOVE    ZERO                TO  FLG-SEIHO
           MOVE    ZERO                TO  FLG-RJN
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       4 )
                    OR   ( COMB-KOHID (IDX0)
                                           =   ZERO )
      *
                IF    ( COMB-KOHHKNNUM (IDX0)
                                           =   "012" )
                    MOVE   1           TO  FLG-SEIHO
                END-IF
      *
                IF    ( COMB-KOHHKNNUM (IDX0)
                                           =   "027" )
                    MOVE   1           TO  FLG-RJN
                END-IF
           END-PERFORM
      *
      *    患者保険情報検索処理
           MOVE    COMB-HKNID          TO  WRK-HKNID
           PERFORM 900-PTHKNINF-KEY-SEL-SEC
      *
           IF    ( AGECHK-NENREI       <   15 )
            AND  ( PTHKN-HONKZKKBN     =   "1" )
               MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
               MOVE    "040001"        TO  WRK-ERRCD
               PERFORM 200112-HKNAGE-ERRMES-HEN-SEC
               PERFORM 215-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (4) =   "1" )
                   MOVE    1       TO  FLG-CHKEND (4)
                   GO  TO  200112-HKNAGE-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( FLG-RJN         =   1 )
               IF    ( AGECHK-ROUJIN   NOT = 1 )
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "040002"        TO  WRK-ERRCD
                   PERFORM 200112-HKNAGE-ERRMES-HEN-SEC
                   PERFORM 215-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (4) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (4)
                       GO  TO  200112-HKNAGE-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-RJN             =   ZERO )
            AND  ( FLG-SEIHO           =   ZERO )
               IF    ( AGECHK-ROUJIN   =   1 )
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "040003"        TO  WRK-ERRCD
                   PERFORM 200112-HKNAGE-ERRMES-HEN-SEC
                   PERFORM 215-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (4) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (4)
                       GO  TO  200112-HKNAGE-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       200112-HKNAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険適応年齢エラーメッセージ編集処理
      *****************************************************************
       200112-HKNAGE-ERRMES-HEN-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMES-PLUS
      *
           IF    ( FLG-PROC            =   ZERO )
               MOVE    "診療会計："    TO  WRK-STR
           ELSE
               MOVE    "入院会計："    TO  WRK-STR
           END-IF
      *
           PERFORM VARYING IDX0   FROM    1   BY  1
                   UNTIL ( IDX0   >   ORCSHKN-HKN-MAX )
                    OR   ( COMB-HKNCOMBINUM
                                       =   ORCSHKN-GMN-HKNCOMBINUM
                                                              (IDX0))
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   ORCSHKN-HKN-MAX )
               MOVE    COMB-HKNCOMBINUM
                                   TO  WRK-MAE-INPUT
           ELSE
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX0)
                                   TO  WRK-MAE-INPUT
           END-IF
      *
           PERFORM 800-ZENKAKU-HEN-SEC
           STRING  WRK-STR         DELIMITED   BY  SPACE
                   WRK-OUT-INPUT   DELIMITED   BY  SPACE
           INTO    WRK-ERRMES-PLUS
           END-STRING
      *
           .
       200112-HKNAGE-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック処理
      *****************************************************************
       200111-PTBYOMEI-CHK-SEC         SECTION.
      *
      *    患者病名情報チェック初期処理
           PERFORM 200111-PTBYOMEI-CHK-INIT-SEC
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   TSRYKA-MAX )
      *
               INITIALIZE              FLG-PTBYO-DAY-G
      *
      *        患者病名情報チェック処理
               PERFORM 200112-PTBYOMEI-CHK-SEC
      *
      *        診療開始日チェック（２）処理
                
               IF    ( FLG-CHKEND (6)      =   ZERO )
                   PERFORM 200112-PTBYO-SRYYMD-CHK-SEC
               END-IF
      *
               IF    ( FLG-CHKEND (5) =   1 )
                AND  ( FLG-CHKEND (6) =   1 )
                AND  ( FLG-CHKEND (7) =   1 )
                AND  ( FLG-CHKEND (8) =   1 )
                AND  ( FLG-CHKEND (9) =   1 )
                    MOVE    TSRYKA-MAX TO  IDX4
               END-IF
      *
           END-PERFORM
      *
      *    薬剤と病名チェック処理
           IF    ( FLG-CHKEND (8)      =   ZERO )
            AND  ( FLG-YKZBYO          =   1 )
               PERFORM 200112-YKZ-BYO-CHK-SEC
           END-IF
      *
      *    診療行為と病名チェック処理
           IF    ( FLG-CHKEND (9)      =   ZERO )
            AND  ( FLG-SRYBYO          =   1 )
               PERFORM 200112-SRY-BYO-CHK-SEC
           END-IF
      *
           .
       200111-PTBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック初期処理
      *****************************************************************
       200111-PTBYOMEI-CHK-INIT-SEC    SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (5) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (5)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (6) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (6)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (7) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (7)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (8) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (8)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (9) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (9)
           END-IF
      *
      *    疑い病名開始日チェック用年月
           MOVE    SPACE               TO  WRK-UTAIGAI-CHK-YM
           MOVE    WRK-KJNYMD          TO  WRK-SYMD
           MOVE    "2"                 TO  WRK-ZOGENPTN
           MOVE    -2                  TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD (1:6)      TO  WRK-UTAIGAI-CHK-YM
      *
           .
       200111-PTBYOMEI-CHK-INIT-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック処理
      *****************************************************************
       200112-PTBYOMEI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  CNT-PTBYOMEI1
                                           CNT-PTBYOMEI2
      *
           MOVE    ZERO                TO  FLG-CHK-END
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
           INITIALIZE                      PTBYOMEI-REC
           MOVE    DTCHK003-HOSPID     TO  PTBYO-HOSPID
           MOVE    DTCHK003-PTID       TO  PTBYO-PTID
           MOVE    TSRYKA-SRYKA (IDX4) TO  PTBYO-SRYKA
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "PTBYOMEI-KEY18"    TO  ORC-DBPATH
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "PTBYOMEI-KEY18"
                                       TO  ORC-DBPATH
               PERFORM 900-PTBYOMEI-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( FLG-CHK-END     NOT =   ZERO )
      *
               COMPUTE CNT-PTBYOMEI1
                   =   CNT-PTBYOMEI1   +   1
      *
      *        患者病名対象チェック
               PERFORM 2001121-PTBYO-TAISYO-CHK-SEC
      *
               IF    ( FLG-PTBYO-TAISYO    =   1 )
      *
                   COMPUTE CNT-PTBYOMEI2
                       =   CNT-PTBYOMEI2   +   1
      *
      *            患者病名エラーメッセージ付加情報編集
                   PERFORM 2001121-PTBYOMES-PLUS-SEC
      *
      *            患者病名チェック
                   IF    ( FLG-CHKEND (5)  =   ZERO )
                       PERFORM 2001121-PTBYO-CHK-SEC
                   END-IF
      *
      *            診療開始日チェック（１）
                   PERFORM VARYING IDX24   FROM    1   BY  1
                           UNTIL ( IDX24   >   3 )
                            OR   ( FLG-CHKEND (6)  NOT =   ZERO )
      *
                       PERFORM 2001121-PTBYO-SRYYMD-CHK-SEC
      *
                   END-PERFORM
      *
      *            疑い病名チェック
                   IF    ( FLG-CHKEND (7)  =   ZERO )
                       PERFORM 2001121-PTBYO-UTAGAI-CHK-SEC
                   END-IF
      *
                   IF    ( PTBYO-SRYYMD (1:6)  =   ORCBSD01-SRYYM )
                       MOVE    PTBYO-SRYYMD    TO  WRK-YMD
                       IF    ( WRK-DD          >=  1 )
                        AND  ( WRK-DD          <=  31 )
                            MOVE    1      TO  FLG-PTBYO-DAY (WRK-DD)
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-CHKEND (5) =   1 )
                 AND ( FLG-CHKEND (6) =   1 )
                 AND ( FLG-CHKEND (7) =   1 )
                 AND ( FLG-CHKEND (8) =   1 )
                 AND ( FLG-CHKEND (9) =   1 )
                   MOVE    1           TO  FLG-CHK-END
               END-IF
      *
               MOVE    "PTBYOMEI-KEY18"    TO  ORC-DBPATH
               PERFORM 900-PTBYOMEI-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "PTBYOMEI-KEY18"    TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
      *    患者病名登録チェック
           IF    ( FLG-CHKEND (5)      =   ZERO )
               MOVE    TSRYKA-SRYKA (IDX4)
                                       TO  WRK-SRYKA
               PERFORM 2001121-PTBYO-TOUROKU-CHK-SEC
           END-IF
      *
           .
       200112-PTBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名エラーメッセージ付加情報編集
      *****************************************************************
       2001121-PTBYOMES-PLUS-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYO-MOJI
           INITIALIZE                      WRK-PTBYOMES-PLUS
      *
           IF    ( PTBYO-BYOMEI        =   SPACE )
               MOVE    1               TO  FLG-PTBYO-MOJI
           ELSE
               MOVE    PTBYO-BYOMEI    TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF    ( FLG-KANACHK NOT =   2 )
                   MOVE    2           TO  FLG-PTBYO-MOJI
               END-IF
           END-IF
      *
           IF    ( FLG-PTBYO-MOJI      =   ZERO )
               MOVE    PTBYO-BYOMEI    TO  WRK-PTBYOMES-PLUS
           ELSE
      *
      *        病名が不正の場合、診療開始日を編集
               MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1     TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
      *
               STRING  "診療開始日："      DELIMITED   BY  SIZE
                       WRK-OUT-INPUT       DELIMITED   BY  SPACE
               INTO    WRK-PTBYOMES-PLUS
               END-STRING
           END-IF
      *
           .
       2001121-PTBYOMES-PLUS-EXT.
           EXIT.
      *****************************************************************
      *    患者病名チェック
      *****************************************************************
       2001121-PTBYO-CHK-SEC           SECTION.
      *
           IF    ( FLG-PTBYO-MOJI      =   1 )
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050004"            TO  WRK-ERRCD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( FLG-PTBYO-MOJI      =   2)
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050005"            TO  WRK-ERRCD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( PTBYO-BYOMEIMOJI    =   ZERO )
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050006"            TO  WRK-ERRCD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( PTBYO-BYOMEI        NOT = SPACE)  
            AND  ( PTBYO-BYOMEICD-G        = SPACE)
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050007"            TO  WRK-ERRCD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF   (( PTBYO-TENKIKBN      =   SPACE)
             AND ( PTBYO-TENKIYMD  NOT =   SPACE))
            OR  (( PTBYO-TENKIKBN  NOT =   SPACE)
             AND ( PTBYO-TENKIYMD      =   SPACE))
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050008"            TO  WRK-ERRCD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( PTBYO-CHARTBYOMEI   NOT =   SPACE )
               MOVE    PTBYO-CHARTBYOMEI   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF    ( FLG-KANACHK NOT =   2 )
                   MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
                   MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
                   MOVE    "050009"            TO  WRK-ERRCD
                   PERFORM 213-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (5)
                       GO  TO  2001121-PTBYO-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SYS-2005-KKHRECEKBN =   "1" )
            OR   ( SYS-2005-SYARECEKBN =   "1" )
      *
               MOVE    ZERO            TO  FLG-PTBYO-LEN-ERR
      *
               IF    ( PTBYO-BYOMEICD (1)  =   "0000999" )
                OR   ( PTBYO-BYOMEIHENFLG  =   "1" )
      *
                   PERFORM 2001121-PTBYO-STRLEN-CHK-SEC
               ELSE
                   PERFORM 2001121-MICODKA-CHK-SEC
                   IF    ( FLG-MICODSYOBYO =   1 )
                       PERFORM 2001121-PTBYO-STRLEN-CHK-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-PTBYO-LEN-ERR   NOT =   ZERO )
                   IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (5)
                       GO  TO  2001121-PTBYO-CHK-EXT
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       2001121-PTBYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名対象チェック
      *****************************************************************
       2001121-PTBYO-TAISYO-CHK-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTBYO-TAISYO
      *
           IF    ( PTBYO-SRYYMD (1:6)      <=   ORCBSD01-SRYYM )
               IF    ( PTBYO-TENKIKBN       =   SPACE )
                   IF    ( PTBYO-NYUGAIKBN  =   SPACE )
                    OR   ( PTBYO-NYUGAIKBN  =   ORCBSD01-NYUGAIKBN )
                           MOVE    1       TO  FLG-PTBYO-TAISYO
                   END-IF
               ELSE
                   IF    ( PTBYO-TENKIYMD (1:6)
                                           >=   ORCBSD01-SRYYM )
                       IF    ( PTBYO-NYUGAIKBN  =   SPACE )
                        OR   ( PTBYO-NYUGAIKBN  =   ORCBSD01-NYUGAIKBN )
                           MOVE    1       TO  FLG-PTBYO-TAISYO
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       2001121-PTBYO-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名文字列長チェック処理
      *****************************************************************
       2001121-PTBYO-STRLEN-CHK-SEC    SECTION.
      *
      *    病名編集処理（疑いフラグを考慮して文字数を求める）
           PERFORM 2001121-PTBYO-BYOMEI-HEN-SEC
      *
           IF    ( SYS-2005-SYUBYOHENFLG   =   "0" )
               IF    ( CNT-BYOMEIMOJI      >   40 )
                   MOVE    2           TO  FLG-PTBYO-LEN-ERR
               END-IF
           ELSE
               IF    ( PTBYO-SYUBYOFLG     =   "1" )
                   IF    ( CNT-BYOMEIMOJI  >   34 )
                       MOVE    1           TO  FLG-PTBYO-LEN-ERR
                   END-IF
               ELSE
                   IF    ( CNT-BYOMEIMOJI  >   40 )
                       MOVE    2           TO  FLG-PTBYO-LEN-ERR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-PTBYO-LEN-ERR       NOT =   ZERO )
      *
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
      *
      *        病名に半角文字が混在していない場合、
      *        病名の先頭５文字まで編集
               MOVE    WRK-HEN-PTBYOMEI
                                       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK       =  2  )
                   MOVE    SPACE               TO  WRK-ERRMES-PLUS
                   STRING  WRK-HEN-PTBYOMEI (1:10)
                                               DELIMITED   BY  SIZE
                           "〜"                DELIMITED   BY  SIZE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
               ELSE
                   MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               END-IF
      *
               IF    ( FLG-PTBYO-LEN-ERR   =   1 )
                   MOVE    "050010"        TO  WRK-ERRCD
               ELSE
                   MOVE    "050011"        TO  WRK-ERRCD
               END-IF
      *
               PERFORM 213-ERRMES-HEN-SEC
      *
           END-IF
      *
           .
      *
       2001-PTBYO-STRLEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    未コード化傷病名コードチェック処理
      *****************************************************************
       2001121-MICODKA-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-MICODSYOBYO
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       21 )
      *
      *        病名のの廃止年月日が診療開始日よりも前かチェック
               IF    ( PTBYO-BYOMEICD(IDX0)    NOT =   SPACE )
                   IF    ( PTBYO-BYOMEICD(IDX0) (1:3)  NOT =   "ZZZ" )
                       MOVE    PTBYO-BYOMEICD(IDX0)
                                           TO  WRK-BYOMEICD
                       PERFORM 900-BYOMEI-KEY-SEL-SEC
                       IF    ( BYO-HAISIYMD    <   PTBYO-SRYYMD )
                           MOVE    1       TO  FLG-MICODSYOBYO
                       END-IF
      *
                       MOVE    21          TO  IDX0
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       2001121-MICODKASYOBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       2001121-PTBYO-BYOMEI-HEN-SEC       SECTION.
      *
           MOVE       ZERO                 TO  FLG-UTAGAI
                                               FLG-KYUSEI
      *
           COMPUTE CNT-BYOMEIMOJI  =   PTBYO-BYOMEIMOJI    *   2
           MOVE    PTBYO-BYOMEI            TO  WRK-HEN-PTBYOMEI
      *
           IF    ( PTBYO-UTAGAIFLG     =   "1" OR  "3" )
               IF    ( CNT-BYOMEIMOJI  <=  8 )
                   MOVE    PTBYO-BYOMEI    TO  WRK-STR
               ELSE
                   MOVE    PTBYO-BYOMEI (CNT-BYOMEIMOJI - 7:)
                                           TO  WRK-STR
               END-IF
      *
               MOVE    ZERO                TO  CNT-STR
               INSPECT WRK-STR             TALLYING
                       CNT-STR FOR ALL "疑"
               IF    ( CNT-STR        <=   ZERO )
                   MOVE    1               TO  FLG-UTAGAI
               END-IF
           END-IF
      *
           IF    ( PTBYO-UTAGAIFLG     =   "2" OR  "3" )
               MOVE    ZERO                TO  CNT-STR
               INSPECT PTBYO-BYOMEI        TALLYING
                       CNT-STR FOR ALL "急性"
               IF    ( CNT-STR        <=   ZERO )
                   MOVE    1               TO  FLG-KYUSEI
               END-IF
           END-IF
      *
           IF    ( FLG-KYUSEI              =   1 )
               MOVE    SPACE               TO  WRK-STR
               MOVE    1                   TO  CNT-BYOMEIMOJI
               STRING  "急性"              DELIMITED   BY  SIZE
                       PTBYO-BYOMEI        DELIMITED   BY  SPACE
               INTO    WRK-STR
               WITH    POINTER             CNT-BYOMEIMOJI
               END-STRING
               MOVE    WRK-STR             TO  WRK-HEN-PTBYOMEI
               COMPUTE CNT-BYOMEIMOJI      =   CNT-BYOMEIMOJI  -   1
           END-IF
      *
           IF    ( FLG-UTAGAI              =   1 )
               MOVE    SPACE               TO  WRK-STR
               MOVE    1                   TO  CNT-BYOMEIMOJI
               STRING  PTBYO-BYOMEI        DELIMITED   BY  SPACE
                       "の疑い"            DELIMITED   BY  SIZE
               INTO    WRK-STR
               WITH    POINTER             CNT-BYOMEIMOJI
               END-STRING
               MOVE    WRK-STR             TO  WRK-HEN-PTBYOMEI
               COMPUTE CNT-BYOMEIMOJI      =   CNT-BYOMEIMOJI  -   1
           END-IF
      *
           .
       2001121-PTBYO-BYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療開始日チェック（１）処理
      *****************************************************************
       2001121-PTBYO-SRYYMD-CHK-SEC    SECTION.
      *
      *    労災自賠をチェック
           IF    ( IDX24               =   2 OR 3 )
               PERFORM 2001121-PTBYO-RSI-JBI-CHK-SEC
               IF    ( FLG-PTBYO-HKN   NOT =   IDX24 )
                   GO  TO  2001121-PTBYO-SRYYMD-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    TSRYKA-SHOSIN-HKN-YMD (IDX4 IDX24)
                                       TO  WRK-SYOSIN-CHK-YMD
      *
           IF    ( WRK-SYOSIN-CHK-YMD  =   SPACE )
               GO  TO  2001121-PTBYO-SRYYMD-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-PTBYO-ERR
           IF    ( PTBYO-SRYYMD        <   WRK-SYOSIN-CHK-YMD )
               IF    ( PTBYO-TENKIKBN      =   SPACE )
                   IF    ( PTBYO-NYUGAIKBN =   SPACE )
                    OR   ( PTBYO-NYUGAIKBN =   ORCBSD01-NYUGAIKBN )
                           MOVE    1   TO  FLG-PTBYO-ERR
                   END-IF
               ELSE
                   IF    ( PTBYO-TENKIYMD  >=  WRK-SYOSIN-CHK-YMD )
                       IF    ( PTBYO-NYUGAIKBN =   SPACE )
                        OR   ( PTBYO-NYUGAIKBN =   ORCBSD01-NYUGAIKBN )
                           MOVE    1   TO  FLG-PTBYO-ERR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-PTBYO-ERR   NOT =   ZERO )
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "060002"            TO  WRK-ERRCD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (6) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (6)
               END-IF
           END-IF
      *
           .
       2001121-PTBYO-SRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名労災自賠責チェック処理
      *****************************************************************
       2001121-PTBYO-RSI-JBI-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYO-HKN
      *
           IF    ( PTBYO-HKNCOMBI      NOT =   ZERO )
               PERFORM VARYING IDX25   FROM    1   BY  1
                       UNTIL ( IDX25           >   ORCSHKN-HKN-MAX )
                        OR   ( FLG-PTBYO-HKN
                                           NOT =   ZERO )
                   IF    ( PTBYO-HKNCOMBI  =   ORCSHKN-GMN-HKNCOMBINUM
                                                                (IDX25))
                       EVALUATE    ORCSHKN-NAI-HKNNUM (IDX25)
                       WHEN    "971"
                           MOVE    2   TO  FLG-PTBYO-HKN
                       WHEN    "973"
                           MOVE    3   TO  FLG-PTBYO-HKN
                       WHEN    OTHER
                           MOVE    1   TO  FLG-PTBYO-HKN
                       END-EVALUATE
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       2001121-PTBYO-RSI-JBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    疑い病名チェック処理
      *****************************************************************
       2001121-PTBYO-UTAGAI-CHK-SEC    SECTION.
      *
           IF    ( PTBYO-UTAGAIFLG     = "1" OR "3")
            AND  ( PTBYO-TENKIKBN      = SPACE)
            AND  ( PTBYO-SRYYMD (1:6) <= WRK-UTAIGAI-CHK-YM )
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "070001"            TO  WRK-ERRCD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (7) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (7)
                   GO  TO  2001121-PTBYO-UTAGAI-CHK-EXT
               END-IF
           END-IF
      *
           .
       2001121-PTBYO-UTAGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名登録チェック
      *****************************************************************
       2001121-PTBYO-TOUROKU-CHK-SEC    SECTION.
      *
           IF    ( CNT-PTBYOMEI1   =   ZERO )
               MOVE    WRK-SRYKA   TO  SRH-1005-SRYKA
               MOVE    "050001"    TO  WRK-ERRCD
               PERFORM 214-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1       TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-TOUROKU-CHK-EXT
               END-IF
           ELSE
               IF    ( CNT-PTBYOMEI2   =   ZERO )
                   MOVE    WRK-SRYKA       TO  SRH-1005-SRYKA
                   IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                       MOVE    "050002"    TO  WRK-ERRCD
                   ELSE
                       MOVE    "050003"    TO  WRK-ERRCD
                   END-IF
                   PERFORM 214-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (5)
                       GO  TO  2001121-PTBYO-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       2001121-PTBYO-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療開始日チェック（２）処理
      *****************************************************************
       200112-PTBYO-SRYYMD-CHK-SEC     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0           >   31 )
                    OR   ( FLG-CHKEND (6) =   1 )
      *
               IF    ( TSRYKA-SHOSIN-DAY (IDX4 IDX0)
                                           =   1 )
                   IF    ( FLG-PTBYO-DAY (IDX0)
                                           =   ZERO )
                       MOVE    IDX0        TO  WRK-DAY-Z
                       MOVE    WRK-DAY-Z   TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    SPACE       TO  WRK-PTBYOMES-PLUS
                       STRING  WRK-OUT-INPUT   DELIMITED   BY  SPACE
                               "日"            DELIMITED   BY  SPACE
                       INTO    WRK-PTBYOMES-PLUS
                       END-STRING
      *
                       MOVE    TSRYKA-SRYKA (IDX4)
                                           TO  SRH-1005-SRYKA
                       MOVE    WRK-PTBYOMES-PLUS
                                           TO  WRK-ERRMES-PLUS
                       MOVE    "060001"    TO  WRK-ERRCD
                       PERFORM 213-ERRMES-HEN-SEC
      *
                       IF    ( ORCBSD01-CHKKBN (6) =   "1" )
                           MOVE    1       TO  FLG-CHKEND (6)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200112-PTBYO-SRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名チェック処理
      *****************************************************************
       200112-YKZ-BYO-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >       TSRYKA-MAX )
                     OR  ( FLG-CHKEND (8)      =   1 )
      *
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   TSRYKA-SRYCD-MAX (IDX6))
                         OR  ( FLG-CHKEND (8)  =   1 )
      *
                   IF    ( TSRYKA-SRYCD (IDX6 IDX7)(1:1)   =   "6" )
                       MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
                       PERFORM 200112-TCHK-YKZ-BYO-CHK-SEC
                       IF    ( TCHK-FLG        =   ZERO )
                           PERFORM 200112-CHKMST-YKZ-BYO-CHK-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       200112-YKZ-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタに存在しない診療行為退避ワークチェック
      *****************************************************************
       200112-TCHK-YKZ-BYO-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  TCHK-FLG
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   TCHK-YKZ-BYO-MAX )
                    OR   ( TCHK-FLG    =   1 )
      *
               IF    ( WRK-CHK-SRYCD   =   TCHK-YKZ-BYO-SRYCD (IDX0) )
                   MOVE    1           TO  TCHK-FLG
               END-IF
      *
           END-PERFORM
      *
           .
       200112-TCHK-YKZ-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名チェック処理（チェックマスタチェック）
      *****************************************************************
       200112-CHKMST-YKZ-BYO-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST-BYOMEI
           INITIALIZE                      TBYOMEICD-AREA
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    "1"                 TO  CHK-CHKKBN
           MOVE    "1"                 TO  CHK-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHK-SRYCD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               MOVE    2               TO  FLG-CHKMST-BYOMEI
               IF    ( CONST-TCHK-MAX  >   TCHK-YKZ-BYO-MAX )
                   COMPUTE TCHK-YKZ-BYO-MAX
                       =   TCHK-YKZ-BYO-MAX    +   1
                   MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  TCHK-YKZ-BYO-SRYCD
                                                (TCHK-YKZ-BYO-MAX)
               END-IF
           END-IF
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                      OR ( FLG-CHKMST-BYOMEI   =  1 )
      *
      *        チェックマスタより患者病名チェック処理
               PERFORM VARYING IDX10   FROM    1   BY  1
                       UNTIL ( IDX10           >   10 )
                          OR ( CHK-CD (IDX10)      =   SPACE )
                          OR ( FLG-CHKMST-BYOMEI   =  1 )
      *
                   MOVE    TSRYKA-SRYKA (IDX6)
                                       TO  WRK-SRYKA
                   MOVE    CHK-CD (IDX10)
                                       TO  WRK-CHK-CD
                   PERFORM 200112-CHKMST-BYO-CHK-SEC
      *
               END-PERFORM
      *
               MOVE    "CHK-KEY3"      TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK-REC
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
           END-PERFORM
      *
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-CHKMST-BYOMEI   =   ZERO )
      *
      *        病名コード退避ワーク並べ替え処理
               PERFORM 200112-TBYOMEICD-SORT-SEC
      *
               MOVE    TSRYKA-SRYKA (IDX6)
                                       TO  WRK-SRYKA
               PERFORM 200112-YKZ-BYO-ERRMES-HEN-SEC
           END-IF
      *
           .
       200112-CHKMST-YKZ-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタより患者病名チェック処理
      *****************************************************************
       200112-CHKMST-BYO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI-ERR
           MOVE    "00000000"          TO  WRK-PTBYO-SRYYMD
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
           INITIALIZE                      PTBYOMEI-REC
           MOVE    DTCHK003-HOSPID     TO  PTBYO-HOSPID
           MOVE    DTCHK003-PTID       TO  PTBYO-PTID
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
      *
           MOVE    WRK-CHK-CD          TO  PTBYO-BYOMEICD (01)
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "PTBYOMEI-KEY19"    TO  ORC-DBPATH
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "PTBYOMEI-KEY19"
                                       TO  ORC-DBPATH
               PERFORM 900-PTBYOMEI-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL ( FLG-PTBYOMEI            NOT =   ZERO )
                    OR   ( FLG-CHKMST-BYOMEI           =   1 )
      *
               MOVE    ZERO            TO  FLG-ERRSET
      *
               PERFORM 2001121-PTBYO-TAISYO-CHK-SEC
               IF    ( FLG-PTBYO-TAISYO    =   1 )
                   MOVE    1           TO  FLG-CHKMST-BYOMEI
               ELSE
      *
      *            適用期間が正しい場合、入外区分の誤りとして
      *            "2"を、適用期間が誤っている場合は適用期間の誤りとして
      *            "1"を設定
                   IF    ( PTBYO-TENKIKBN      =   SPACE )
                       IF    ( PTBYO-SRYYMD (1:6) <=   ORCBSD01-SRYYM )
                           MOVE    2   TO  FLG-BYOMEI-ERR
                           MOVE    1   TO  FLG-ERRSET
                       ELSE
                           IF    ( FLG-BYOMEI-ERR <=   1 )
                               MOVE    1   TO  FLG-BYOMEI-ERR
                               MOVE    1   TO  FLG-ERRSET
                           END-IF
                       END-IF
                   ELSE
                       IF    ( PTBYO-SRYYMD   (1:6)
                                                   <=  ORCBSD01-SRYYM )
                        AND  ( PTBYO-TENKIYMD (1:6)
                                                   >=  ORCBSD01-SRYYM )
                           MOVE    2   TO  FLG-BYOMEI-ERR
                           MOVE    1   TO  FLG-ERRSET
                       ELSE
                           IF    ( FLG-BYOMEI-ERR <=   1 )
                               MOVE    1   TO  FLG-BYOMEI-ERR
                               MOVE    1   TO  FLG-ERRSET
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-ERRSET      =   1 )
                   IF    ( WRK-PTBYO-SRYYMD  < PTBYO-SRYYMD )
                       MOVE    PTBYO-SRYYMD
                                           TO  WRK-PTBYO-SRYYMD
                   END-IF
               END-IF
      *
               MOVE    "PTBYOMEI-KEY19"
                                       TO  ORC-DBPATH
               PERFORM 900-PTBYOMEI-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "PTBYOMEI-KEY19"    TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-CHKMST-BYOMEI   =   ZERO )
               IF    ( FLG-BYOMEI-ERR  =   1 OR 2 )
                   IF    ( CONST-TBYOMEICD-MAX
                                           >   TBYOMEICD-MAX )
                       COMPUTE TBYOMEICD-MAX
                           =   TBYOMEICD-MAX   +   1
      *
                       MOVE    WRK-CHK-CD
                                       TO  TBYOMEICD-BYOMEICD
                                                   (TBYOMEICD-MAX)
                       MOVE    FLG-BYOMEI-ERR
                                       TO  TBYOMEICD-ERRFLG
                                                   (TBYOMEICD-MAX)
                       MOVE    WRK-PTBYO-SRYYMD
                                       TO  TBYOMEICD-SRYYMD
                                                   (TBYOMEICD-MAX)
                   END-IF
               END-IF
           END-IF
      *
           .
       200112-CHKMST-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＳＯＲＴ作業領域ＳＯＲＴ処理
      *****************************************************************
       200112-TBYOMEICD-SORT-SEC       SECTION.
      *
      *    ソート用ワークについて並び替えを行う
           PERFORM VARYING TBYOMEICD-IDX1  FROM    2   BY  1
                   UNTIL ( TBYOMEICD-IDX1  >   TBYOMEICD-MAX )
               COMPUTE TBYOMEICD-STIDX     =   TBYOMEICD-IDX1    -   1
               MOVE    TBYOMEICD-OCC (TBYOMEICD-IDX1)
                                   TO  TBYOMEICD-OCC
                                            (CONST-TBYOMEICD-MAX + 1)
               PERFORM VARYING TBYOMEICD-IDX2
                       FROM    TBYOMEICD-STIDX   BY  -1
                       UNTIL ( TBYOMEICD-IDX2    <   1 )
                          OR ( TBYOMEICD-KEY (CONST-TBYOMEICD-MAX + 1)
                                   <=  TBYOMEICD-KEY (TBYOMEICD-IDX2) )
                       MOVE    TBYOMEICD-OCC (TBYOMEICD-IDX2)
                           TO  TBYOMEICD-OCC (TBYOMEICD-IDX2 + 1)
               END-PERFORM
               MOVE    TBYOMEICD-OCC (CONST-TBYOMEICD-MAX + 1)
                           TO  TBYOMEICD-OCC (TBYOMEICD-IDX2 + 1)
           END-PERFORM
      *
           .
       200112-TBYOMEICD-SORT-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名エラーメッセージ編集処理
      *****************************************************************
       200112-YKZ-BYO-ERRMES-HEN-SEC   SECTION.
      *
           MOVE    WRK-SRYKA           TO  SRH-1005-SRYKA
      *
           MOVE    CHK-SRYCD           TO  WRK-SRYCD
           MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  TNS-NAME
           END-IF
      *
           IF    ( TBYOMEICD-MAX       =   ZERO )
               MOVE    TNS-NAME        TO  WRK-ERRMES-PLUS
               MOVE    "080001"        TO  WRK-ERRCD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (8) =   "1" )
                   MOVE    1       TO  FLG-CHKEND (8)
               END-IF
           ELSE
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >    TBYOMEICD-MAX )
                        OR   ( FLG-CHKEND (8)  =   1 )
      *
                   MOVE    TBYOMEICD-BYOMEICD (IDX0)
                                               TO  WRK-BYOMEICD
                   PERFORM 900-BYOMEI-KEY-SEL-SEC
      *
                   MOVE   SPACE                TO  WRK-ERRMES-PLUS
                   STRING TNS-NAME             DELIMITED BY SPACE
                          "、"                 DELIMITED BY SIZE
                          BYO-BYOMEI           DELIMITED BY SPACE
                   INTO   WRK-ERRMES-PLUS
                   END-STRING
      *
                   IF   ( TBYOMEICD-ERRFLG (IDX0) =   2 )
                       IF    ( ORCBSD01-NYUGAIKBN = "1" )
                           MOVE    "080002"   TO  WRK-ERRCD
                       ELSE
                           MOVE    "080003"   TO  WRK-ERRCD
                       END-IF
                   ELSE
                           MOVE    "080004"   TO  WRK-ERRCD
                   END-IF
      *
                   PERFORM 213-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (8) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (8)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200112-YKZ-BYO-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為と病名チェック処理
      *****************************************************************
       200112-SRY-BYO-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >       TSRYKA-MAX )
                     OR  ( FLG-CHKEND (9)      =   1 )
      *
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   TSRYKA-SRYCD-MAX (IDX6))
                         OR  ( FLG-CHKEND (9)  =   1 )
      *
                   IF    ( TSRYKA-SRYCD (IDX6 IDX7)(1:1)   NOT =   "6" )
                    AND  ( TSRYKA-SRYCD (IDX6 IDX7)(1:1)   NOT =   "7" )
                       MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
                       PERFORM 200112-TCHK-SRY-BYO-CHK-SEC
                       IF    ( TCHK-FLG        =   ZERO )
                           PERFORM 200112-CHKMST-SRY-BYO-CHK-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       200112-SRY-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタに存在しない診療行為退避ワークチェック
      *****************************************************************
       200112-TCHK-SRY-BYO-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  TCHK-FLG
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   TCHK-SRY-BYO-MAX )
                    OR   ( TCHK-FLG    =   1 )
      *
               IF    ( WRK-CHK-SRYCD   =   TCHK-SRY-BYO-SRYCD (IDX0) )
                   MOVE    1           TO  TCHK-FLG
               END-IF
      *
           END-PERFORM
      *
           .
       200112-TCHK-SRY-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為と病名チェック処理（チェックマスタチェック）
      *****************************************************************
       200112-CHKMST-SRY-BYO-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST-BYOMEI
           INITIALIZE                      TBYOMEICD-AREA
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    "2"                 TO  CHK-CHKKBN
           MOVE    "1"                 TO  CHK-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHK-SRYCD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               MOVE    2               TO  FLG-CHKMST-BYOMEI
               IF    ( CONST-TCHK-MAX  >   TCHK-SRY-BYO-MAX )
                   COMPUTE TCHK-SRY-BYO-MAX
                       =   TCHK-SRY-BYO-MAX    +   1
                   MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  TCHK-SRY-BYO-SRYCD
                                                (TCHK-SRY-BYO-MAX)
               END-IF
           END-IF
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                      OR ( FLG-CHKMST-BYOMEI   =  1 )
      *
      *        チェックマスタより患者病名チェック処理
               PERFORM VARYING IDX10   FROM    1   BY  1
                       UNTIL ( IDX10           >   10 )
                          OR ( CHK-CD (IDX10)      =   SPACE )
                          OR ( FLG-CHKMST-BYOMEI   =  1 )
      *
                   MOVE    TSRYKA-SRYKA (IDX6)
                                       TO  WRK-SRYKA
                   MOVE    CHK-CD (IDX10)
                                       TO  WRK-CHK-CD
                   PERFORM 200112-CHKMST-BYO-CHK-SEC
      *
               END-PERFORM
      *
               MOVE    "CHK-KEY3"      TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK-REC
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
           END-PERFORM
      *
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-CHKMST-BYOMEI   =   ZERO )
      *
      *        病名コード退避ワーク並べ替え処理
               PERFORM 200112-TBYOMEICD-SORT-SEC
      *
               MOVE    TSRYKA-SRYKA (IDX6)
                                       TO  WRK-SRYKA
               PERFORM 200112-SRY-BYO-ERRMES-HEN-SEC
           END-IF
      *
           .
       200112-CHKMST-SRY-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為と病名エラーメッセージ編集処理
      *****************************************************************
       200112-SRY-BYO-ERRMES-HEN-SEC   SECTION.
      *
           MOVE    WRK-SRYKA           TO  SRH-1005-SRYKA
      *
           MOVE    CHK-SRYCD           TO  WRK-SRYCD
           MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  TNS-NAME
           END-IF
      *
           IF    ( TBYOMEICD-MAX       =   ZERO )
               MOVE    "090001"        TO  WRK-ERRCD
               MOVE    TNS-NAME        TO  WRK-ERRMES-PLUS
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (9) =   "1" )
                   MOVE    1       TO  FLG-CHKEND (9)
               END-IF
           ELSE
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >    TBYOMEICD-MAX )
                        OR   ( FLG-CHKEND (9)  =   1 )
      *
                   MOVE    TBYOMEICD-BYOMEICD (IDX0)
                                               TO  WRK-BYOMEICD
                   PERFORM 900-BYOMEI-KEY-SEL-SEC
      *
                   MOVE   SPACE                TO  WRK-ERRMES-PLUS
                   STRING TNS-NAME             DELIMITED BY SPACE
                          "、"                 DELIMITED BY SIZE
                          BYO-BYOMEI           DELIMITED BY SPACE
                   INTO   WRK-ERRMES-PLUS
                   END-STRING
      *
                   IF   ( TBYOMEICD-ERRFLG (IDX0) =   2 )
                       IF    ( ORCBSD01-NYUGAIKBN = "1" )
                           MOVE    "090002"   TO  WRK-ERRCD
                       ELSE
                           MOVE    "090003"   TO  WRK-ERRCD
                       END-IF
                   ELSE
                           MOVE    "090004"   TO  WRK-ERRCD
                   END-IF
      *
                   PERFORM 213-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (9) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (9)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200112-SRY-BYO-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤・併用算定の診療行為チェック処理
      *****************************************************************
       200111-HEIYOU-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(10) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (10)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN(11) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (11)
           END-IF
      *
      *    併用禁忌の薬剤チェック処理
           IF    ( FLG-CHKEND (10)     =   ZERO )
            AND  ( FLG-YKZHEI          =   1 )
                   PERFORM 200112-HEI-YKZ-CHK-SEC
           END-IF
      *
      *    併用算定の診療行為チェック処理
           IF    ( FLG-CHKEND (11)     =   ZERO )
            AND  ( FLG-SRYHEI          =   1 )
                   PERFORM 200112-HEI-SRY-CHK-SEC
           END-IF
      *
           .
       200111-HEIYOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤チェック処理
      *****************************************************************
       200112-HEI-YKZ-CHK-SEC          SECTION.
      *
           INITIALIZE                  THEIYOU-AREA
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TSRYCD-MAX )
                    OR   ( FLG-CHKEND (10)     =   1 )
      *
               IF    ( TSRYCD-SRYCD (IDX6)(1:1)    =   "6" )
      *
      *            相互作用テーブルよりチェック
                   IF    ( FLG-CHKEND (10)     =   ZERO )
                       PERFORM 200112-INTERACT-CHK-SEC
                   END-IF
      *
      *            チェックマスタよりチェック
                   IF    ( FLG-CHKEND (10)     =   ZERO )
                       MOVE    TSRYCD-SRYCD (IDX6)
                                               TO  WRK-CHK-SRYCD
                       PERFORM 200112-TCHK-HEI-YKZ-CHK-SEC
                       IF    ( TCHK-FLG        =   ZERO )
                           PERFORM 200112-CHKMST-HEI-YKZ-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    エラーメッセージ編集処理
           IF    ( THEIYOU-MAX         >   ZERO )
               PERFORM 200112-HEI-YKZ-ERRMES-HEN-SEC
           END-IF
      *
           .
       200112-HEI-YKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタに存在しない診療行為退避ワークチェック
      *****************************************************************
       200112-TCHK-HEI-YKZ-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  TCHK-FLG
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   TCHK-HEI-YKZ-MAX )
                    OR   ( TCHK-FLG    =   1 )
      *
               IF    ( WRK-CHK-SRYCD   =   TCHK-HEI-YKZ-SRYCD (IDX0) )
                   MOVE    1           TO  TCHK-FLG
               END-IF
      *
           END-PERFORM
      *
           .
       200112-TCHK-HEI-YKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤チェック処理（チェックマスタ）
      *****************************************************************
       200112-CHKMST-HEI-YKZ-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    "4"                 TO  CHK-CHKKBN
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    TSRYCD-SRYCD (IDX6) TO  CHK-SRYCD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               IF    ( CONST-TCHK-MAX  >   TCHK-HEI-YKZ-MAX )
                   COMPUTE TCHK-HEI-YKZ-MAX
                       =   TCHK-HEI-YKZ-MAX    +   1
                   MOVE    TSRYCD-SRYCD (IDX6)
                                       TO  TCHK-HEI-YKZ-SRYCD
                                                (TCHK-HEI-YKZ-MAX)
               END-IF
           END-IF
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                      OR ( FLG-CHKEND (10) =  1 )
      *
      *        チェックマスタより診療行為チェック処理
               PERFORM VARYING IDX10   FROM    1   BY  1
                       UNTIL ( IDX10           >   10 )
                          OR ( CHK-CD (IDX10)  =  SPACE )
                          OR ( FLG-CHKEND (10) =  1 )
      *
                   MOVE    IDX6            TO  IDX-TSRYCD
                   MOVE    CHK-CD (IDX10)  TO  WRK-CHK-CD
                   MOVE    ORCBSD01-CHKKBN (10)
                                           TO  WRK-ORCBSD01-CHKKBN
                   MOVE    FLG-CHKEND (10) TO  WRK-FLG-CHKEND
                   PERFORM 200112-CHKMST-SRY-CHK-SEC
                   MOVE    WRK-FLG-CHKEND  TO  FLG-CHKEND (10)
      *
               END-PERFORM
      *
               MOVE    "CHK-KEY3"      TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK-REC
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
           END-PERFORM
      *
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200112-CHKMST-HEI-YKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ診療行為チェック処理
      *****************************************************************
       200112-CHKMST-SRY-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-HIT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   THEIYOU-MAX )
                     OR  ( FLG-HIT =   1 )
      *
               IF  (( TSRYCD-SRYCD (IDX-TSRYCD)
                                   =   THEIYOU-SRYCD1(IDX0) )
                 AND( WRK-CHK-CD
                                   =   THEIYOU-SRYCD2(IDX0) ))
                   MOVE    1       TO  FLG-HIT
               END-IF
      *
               IF  (( TSRYCD-SRYCD (IDX-TSRYCD)
                                   =   THEIYOU-SRYCD2(IDX0) )
                 AND( WRK-CHK-CD
                                   =   THEIYOU-SRYCD1(IDX0) ))
                   MOVE    1       TO  FLG-HIT
               END-IF
      *
           END-PERFORM
      *
      *    併用禁忌（算定）退避ワークに存在する場合は、後続の処理は
      *    行わない
           IF    ( FLG-HIT         =   1 )
               GO  TO  200112-CHKMST-SRY-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYCD-MAX )
                    OR   ( WRK-FLG-CHKEND      =   1 )
      *
               IF    ( IDX-TSRYCD  NOT =   IDX0 )
                AND  ( WRK-CHK-CD      =   TSRYCD-SRYCD (IDX0) )
                    IF    ( TSRYCD-NYUGAIKBN(IDX-TSRYCD)
                                       =   ORCBSD01-NYUGAIKBN )
                      OR  ( TSRYCD-NYUGAIKBN(IDX0)
                                       =   ORCBSD01-NYUGAIKBN )
                       IF    ( CONST-THEIYOU-MAX   > THEIYOU-MAX )
                           COMPUTE THEIYOU-MAX =   THEIYOU-MAX +   1
      *
                           MOVE    TSRYCD-NYUGAIKBN (IDX-TSRYCD)
                                       TO  THEIYOU-NYUGAIKBN1
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-SRYCD (IDX-TSRYCD)
                                       TO  THEIYOU-SRYCD1(THEIYOU-MAX)
      *
                           MOVE    TSRYCD-NYUGAIKBN (IDX0)
                                       TO  THEIYOU-NYUGAIKBN2
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-SRYCD (IDX0)
                                       TO  THEIYOU-SRYCD2(THEIYOU-MAX)
      *
                       END-IF
      *
                       IF    ( WRK-ORCBSD01-CHKKBN =   "1" )
                           MOVE    1   TO  WRK-FLG-CHKEND
                       END-IF
      *
                   END-IF
      *
                   MOVE    TSRYCD-MAX      TO  IDX0
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-CHKMST-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤チェック処理（相互作用テーブル）
      *****************************************************************
       200112-INTERACT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-INTERACT
      *
           INITIALIZE                      INTERACT-REC
           MOVE    TSRYCD-SRYCD (IDX6) TO  INTERACT-DRUGCD
           MOVE    INTERACT-REC        TO  MCPDATA-REC
           MOVE    "INTERACT-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  INTERACT-REC
           ELSE
               MOVE    1               TO  FLG-INTERACT
           END-IF
      *
           PERFORM UNTIL ( FLG-INTERACT    NOT =   ZERO )
                    OR   ( FLG-CHKEND (10)     =   1 )
      *
               MOVE    IDX6            TO  IDX-TSRYCD
               PERFORM 200112-INTERACT-YKZ-CHK-SEC
      *
               MOVE    "INTERACT-KEY"  TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF   (  MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  INTERACT-REC
               ELSE
                   MOVE    1           TO  FLG-INTERACT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "INTERACT-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200112-INTERACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    相互作用テーブル薬剤チェック処理
      *****************************************************************
       200112-INTERACT-YKZ-CHK-SEC     SECTION.
      *
      *
           MOVE    ZERO            TO  FLG-HIT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   THEIYOU-MAX )
                    OR   ( FLG-HIT =   1 )
      *
               IF  (( INTERACT-DRUGCD
                                   =   THEIYOU-SRYCD1(IDX0) )
                 AND( INTERACT-DRUGCD2
                                   =   THEIYOU-SRYCD2(IDX0) ))
                   MOVE    1       TO  FLG-HIT
               END-IF
      *
               IF  (( INTERACT-DRUGCD
                                   =   THEIYOU-SRYCD2(IDX0) )
                 AND( INTERACT-DRUGCD2
                                   =   THEIYOU-SRYCD1(IDX0) ))
                   MOVE    1       TO  FLG-HIT
               END-IF
           END-PERFORM
      *
           IF    ( FLG-HIT         =   1 )
               GO  TO  200112-INTERACT-YKZ-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYCD-MAX )
                    OR   ( FLG-CHKEND (10)     =   1 )
      *
               IF    ( IDX-TSRYCD      NOT =   IDX0 )
                AND  ( INTERACT-DRUGCD2    =   TSRYCD-SRYCD (IDX0) )
                   IF    ( TSRYCD-NYUGAIKBN(IDX-TSRYCD)
                                       =   ORCBSD01-NYUGAIKBN )
                     OR  ( TSRYCD-NYUGAIKBN(IDX0)
                                       =   ORCBSD01-NYUGAIKBN )
                       IF    ( CONST-THEIYOU-MAX   > THEIYOU-MAX )
                           COMPUTE THEIYOU-MAX =   THEIYOU-MAX +   1
      *
                           MOVE    TSRYCD-NYUGAIKBN (IDX-TSRYCD)
                                           TO  THEIYOU-NYUGAIKBN1
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-SRYCD (IDX-TSRYCD)
                                           TO  THEIYOU-SRYCD1
                                                         (THEIYOU-MAX)
      *
                           MOVE    TSRYCD-NYUGAIKBN (IDX0)
                                           TO  THEIYOU-NYUGAIKBN2
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-SRYCD (IDX0)
                                           TO  THEIYOU-SRYCD2
                                                         (THEIYOU-MAX)
                       END-IF
      *
                       IF    ( ORCBSD01-CHKKBN (10)    =   "1" )
                           MOVE    1       TO  FLG-CHKEND (10)
                       END-IF
                   END-IF
      *
                   MOVE    TSRYCD-MAX      TO  IDX0
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-INTERACT-YKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤エラーメッセージ編集処理
      *****************************************************************
       200112-HEI-YKZ-ERRMES-HEN-SEC   SECTION.
      *
      *    処理パラメータとは反対の入外を編集
           IF    ( ORCBSD01-NYUGAIKBN   =  "1" )
               MOVE    "（外来）"  TO   WRK-NYUGAI-MES
           ELSE
               MOVE    "（入院）"  TO   WRK-NYUGAI-MES
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN(10)  =  "1" )
               MOVE    1           TO   WRK-MAX
           ELSE
               MOVE    THEIYOU-MAX TO   WRK-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >    WRK-MAX )
      *
               MOVE    SPACE       TO   WRK-ERRMES-PLUS
               MOVE    1           TO   WRK-PNT
      *
               IF    ( THEIYOU-NYUGAIKBN1 (IDX0)
                               NOT =    ORCBSD01-NYUGAIKBN )
                   STRING  WRK-NYUGAI-MES  DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
               END-IF
      *
               MOVE    THEIYOU-SRYCD1 (IDX0)
                                   TO  WRK-SRYCD
               MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  TNS-NAME
               END-IF
      *
               STRING TNS-NAME         DELIMITED   BY  SPACE
                      "、"             DELIMITED   BY  SIZE
               INTO   WRK-ERRMES-PLUS
               WITH    POINTER    WRK-PNT
               END-STRING
      *
               IF    ( THEIYOU-NYUGAIKBN2 (IDX0)
                               NOT =    ORCBSD01-NYUGAIKBN )
                   STRING  WRK-NYUGAI-MES  DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
               END-IF
      *
      *
               MOVE    THEIYOU-SRYCD2 (IDX0)
                                   TO  WRK-SRYCD
               MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  TNS-NAME
               END-IF
      *
               STRING TNS-NAME         DELIMITED   BY  SPACE
               INTO   WRK-ERRMES-PLUS
               WITH    POINTER    WRK-PNT
               END-STRING
      *
               MOVE    "100001"    TO   WRK-ERRCD
               PERFORM 215-ERRMES-HEN-SEC
      *
           END-PERFORM
      *
           .
       200112-HEI-YKZ-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    併用算定の診療行為チェック処理
      *****************************************************************
       200112-HEI-SRY-CHK-SEC          SECTION.
      *
           INITIALIZE                  THEIYOU-AREA
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TSRYCD-MAX )
                    OR   ( FLG-CHKEND (11)     =   1 )
      *
               IF    ( TSRYCD-SRYCD (IDX6)(1:1)    NOT =   "6" )
                AND  ( TSRYCD-SRYCD (IDX6)(1:1)    NOT =   "7" )
      *
      *            チェックマスタよりチェック
                   IF    ( FLG-CHKEND (11)     =   ZERO )
                       MOVE    TSRYCD-SRYCD (IDX6)
                                       TO  WRK-CHK-SRYCD
                       PERFORM 200112-TCHK-HEI-SRY-CHK-SEC
                       IF    ( TCHK-FLG        =   ZERO )
                           PERFORM 200112-CHKMST-HEI-SRY-CHK-SEC
                       END-IF
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
      *    エラーメッセージ編集処理
           IF    ( THEIYOU-MAX         >   ZERO )
               PERFORM 200112-HEI-SRY-ERRMES-HEN-SEC
           END-IF
      *
           .
       200112-HEI-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタに存在しない診療行為退避ワークチェック
      *****************************************************************
       200112-TCHK-HEI-SRY-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  TCHK-FLG
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   TCHK-HEI-SRY-MAX )
                    OR   ( TCHK-FLG    =   1 )
      *
               IF    ( WRK-CHK-SRYCD   =   TCHK-HEI-SRY-SRYCD (IDX0) )
                   MOVE    1           TO  TCHK-FLG
               END-IF
      *
           END-PERFORM
      *
           .
       200112-TCHK-HEI-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定の診療行為チェック処理（チェックマスタ）
      *****************************************************************
       200112-CHKMST-HEI-SRY-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    "5"                 TO  CHK-CHKKBN
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    TSRYCD-SRYCD (IDX6) TO  CHK-SRYCD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               IF    ( CONST-TCHK-MAX  >   TCHK-HEI-SRY-MAX )
                   COMPUTE TCHK-HEI-SRY-MAX
                       =   TCHK-HEI-SRY-MAX    +   1
                   MOVE    TSRYCD-SRYCD (IDX6)
                                       TO  TCHK-HEI-SRY-SRYCD
                                                (TCHK-HEI-SRY-MAX)
               END-IF
           END-IF
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                      OR ( FLG-CHKEND (11) =  1 )
      *
      *        チェックマスタより診療行為チェック処理
               PERFORM VARYING IDX10   FROM    1   BY  1
                       UNTIL ( IDX10           >   10 )
                          OR ( CHK-CD (IDX10)  =  SPACE )
                          OR ( FLG-CHKEND (11) =  1 )
      *
                   MOVE    IDX6            TO  IDX-TSRYCD
                   MOVE    CHK-CD (IDX10)  TO  WRK-CHK-CD
                   MOVE    ORCBSD01-CHKKBN (11)
                                           TO  WRK-ORCBSD01-CHKKBN
                   MOVE    FLG-CHKEND (11) TO  WRK-FLG-CHKEND
                   PERFORM 200112-CHKMST-SRY-CHK-SEC
                   MOVE    WRK-FLG-CHKEND  TO  FLG-CHKEND (11)
      *
               END-PERFORM
      *
               MOVE    "CHK-KEY3"      TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK-REC
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
           END-PERFORM
      *
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200112-CHKMST-HEI-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定の診療行為エラーメッセージ編集処理
      *****************************************************************
       200112-HEI-SRY-ERRMES-HEN-SEC   SECTION.
      *
      *    処理パラメータとは反対の入外を編集
           IF    ( ORCBSD01-NYUGAIKBN   =  "1" )
               MOVE    "（外来）"  TO   WRK-NYUGAI-MES
           ELSE
               MOVE    "（入院）"  TO   WRK-NYUGAI-MES
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN(11)  =  "1" )
               MOVE    1           TO   WRK-MAX
           ELSE
               MOVE    THEIYOU-MAX TO   WRK-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >    WRK-MAX )
      *
               MOVE    SPACE       TO   WRK-ERRMES-PLUS
               MOVE    1           TO   WRK-PNT
      *
               IF    ( THEIYOU-NYUGAIKBN1 (IDX0)
                               NOT =    ORCBSD01-NYUGAIKBN )
                   STRING  WRK-NYUGAI-MES  DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
               END-IF
      *
               MOVE    THEIYOU-SRYCD1 (IDX0)
                                   TO  WRK-SRYCD
               MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  TNS-NAME
               END-IF
      *
               STRING TNS-NAME         DELIMITED   BY  SPACE
                      "、"             DELIMITED   BY  SIZE
               INTO   WRK-ERRMES-PLUS
               WITH    POINTER    WRK-PNT
               END-STRING
      *
               IF    ( THEIYOU-NYUGAIKBN2 (IDX0)
                               NOT =    ORCBSD01-NYUGAIKBN )
                   STRING  WRK-NYUGAI-MES  DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
               END-IF
      *
      *
               MOVE    THEIYOU-SRYCD2 (IDX0)
                                   TO  WRK-SRYCD
               MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  TNS-NAME
               END-IF
      *
               STRING TNS-NAME         DELIMITED   BY  SPACE
               INTO   WRK-ERRMES-PLUS
               WITH    POINTER    WRK-PNT
               END-STRING
      *
      *
               MOVE    "110001"    TO   WRK-ERRCD
               PERFORM 215-ERRMES-HEN-SEC
      *
           END-PERFORM
      *
           .
       200112-HEI-SRY-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    実日数算定チェック処理
      *****************************************************************
       200111-JNISSU-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(12) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (12)
           END-IF
      *
           IF    ( FLG-CHKEND (12)     =   ZERO )
      *
      *        実日数退避ワーク日数カウント
               PERFORM 200112-TJNISSU-DAY-CNT-SEC
      *
      *        算定回数退避ワーク日数カウント
               PERFORM 200112-TKAISU-DAY-CNT-SEC
      *
      *        実日数退避ワークチェック
               PERFORM 200112-TJNISSU-CHK-SEC
           END-IF
      *
           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
               IF    ( FLG-CHKEND (12) =   ZERO )
      *            入院診療行為日チェック処理
                   PERFORM 200112-NACCT-SRY-DAY-CHK-SEC
               END-IF
      *
               IF    ( FLG-CHKEND (12) =   ZERO )
      *            入院調剤料算定日チェック処理
                   PERFORM 200112-NCHOZAI-DAY-CHK-SEC
               END-IF
      *
           END-IF
      *
           .
       200111-JNISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワーク日数カウント処理
      *****************************************************************
       200112-TJNISSU-DAY-CNT-SEC      SECTION.
      *
           PERFORM VARYING IDX13   FROM    1   BY  1
                   UNTIL ( IDX13   >   TJNISSU-MAX )
      *
               MOVE    ZERO        TO  TJNISSU-DAY-CNT (IDX13)
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( TJNISSU-DAY (IDX13 IDX0)    =   1 )
                       COMPUTE TJNISSU-DAY-CNT (IDX13)
                           =   TJNISSU-DAY-CNT (IDX13) +   1
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       200112-TJNISSU-DAY-CNT-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク日数カウント処理
      *****************************************************************
       200112-TKAISU-DAY-CNT-SEC       SECTION.
      *
           PERFORM VARYING IDX12   FROM    1   BY  1
                   UNTIL ( IDX12   >   TKAISU-MAX )
      *
               PERFORM VARYING IDX14   FROM    1   BY  1
                       UNTIL ( IDX14   >   TKAISU-SRYCD-MAX (IDX12))
      *
                   MOVE    ZERO    TO  TKAISU-SRYCD-CNT (IDX12 IDX14)
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   31 )
      *
                       IF    ( TKAISU-SRYCD-DAY (IDX12 IDX14 IDX0 )
                                       =   1 )
                           COMPUTE TKAISU-SRYCD-CNT (IDX12 IDX14)
                               =   TKAISU-SRYCD-CNT (IDX12 IDX14)  +   1
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM
      *
           .
       200112-TKAISU-DAY-CNT-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワークチェック
      *****************************************************************
       200112-TJNISSU-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX12   FROM    1   BY  1
                   UNTIL ( IDX12   >   TKAISU-MAX )
                    OR   ( FLG-CHKEND (12) =   1 )
      *
               MOVE    ZERO            TO  FLG-JNISSU-HIT
      *
               PERFORM VARYING IDX13   FROM    1   BY  1
                       UNTIL ( IDX13   >   TJNISSU-MAX )
                        OR   ( FLG-CHKEND (12) =   1 )
      *
                   IF    ( TJNISSU-HKNNUM (IDX13)
                                       =   TKAISU-HKNNUM (IDX12) )
                       MOVE    1       TO  FLG-JNISSU-HIT
                       PERFORM 200112-TJNISSU-DAY-CHK-SEC
                       MOVE    TJNISSU-MAX      TO  IDX13
                   END-IF
      *
               END-PERFORM
      *
      *        実日数算定元の剤が存在しない場合で、
      *        診療行為の算定回数が設定されている場合はエラーとする
               IF    ( FLG-JNISSU-HIT  =   ZERO )
                   PERFORM VARYING IDX19   FROM    1   BY  1
                           UNTIL ( IDX19   >   TKAISU-SRYCD-MAX (IDX12))
                            OR   ( FLG-CHKEND (12) =   1 )
      *
                       IF    ( TKAISU-SRYCD-CNT (IDX12 IDX19)
                                               >   ZERO )
      *
                           MOVE    TKAISU-HKNNUM (IDX12)
                                           TO  WRK-HKNNUM
                           MOVE    TKAISU-SRYCD (IDX12 IDX19)
                                           TO  WRK-TKAISU-SRYCD
      *
                           PERFORM 200112-JNISSU-ERRMES-HEN-SEC
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       200112-TJNISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワークチェック
      *****************************************************************
       200112-TJNISSU-DAY-CHK-SEC      SECTION.
      *
           PERFORM VARYING IDX14   FROM    1   BY  1
                   UNTIL ( IDX14   >   TKAISU-SRYCD-MAX (IDX12))
                    OR   ( FLG-CHKEND (12) =   1 )
      *
               IF    ( TJNISSU-DAY-CNT (IDX13)
                                   <   TKAISU-SRYCD-CNT (IDX12 IDX14))
                   MOVE    TKAISU-HKNNUM (IDX12)
                                       TO  WRK-HKNNUM
                   MOVE    TKAISU-SRYCD (IDX12 IDX14)
                                       TO  WRK-TKAISU-SRYCD
      *
                   PERFORM 200112-JNISSU-ERRMES-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-TJNISSU-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数エラーメッセージ編集処理
      *****************************************************************
       200112-JNISSU-ERRMES-HEN-SEC    SECTION.
      *
           MOVE    WRK-HKNNUM          TO  SRH-HKNNUM
           MOVE    WRK-TKAISU-SRYCD    TO  WRK-SRYCD
           MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  TNS-NAME
           END-IF
      *
           MOVE    TNS-NAME    TO  WRK-ERRMES-PLUS
           MOVE    "120001"    TO  WRK-ERRCD
           PERFORM 216-ERRMES-HEN-SEC
      *
           IF    ( ORCBSD01-CHKKBN (12)    =   "1" )
               MOVE    1       TO  FLG-CHKEND (12)
           END-IF
      *
           .
       200112-JNISSU-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為日チェック処理
      *****************************************************************
       200112-NACCT-SRY-DAY-CHK-SEC    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
                    OR   ( FLG-CHKEND (12) =   1 )
      *
               IF    ( TNACCT-HKNCOMBI-DAY (IDX0)  =   ZERO )
                   IF    ( TNYUIN-ACCT-DAY (IDX0)  =   1 )
                       MOVE    "120002"    TO  WRK-ERRCD
      *
                       PERFORM 210-ERRMES-HEN-SEC
                       MOVE    1           TO  FLG-CHKEND (12)
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200112-NACCT-SRY-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料算定日チェック処理
      *****************************************************************
       200112-NCHOZAI-DAY-CHK-SEC      SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    DTCHK003-HOSPID         TO  NACCT-HOSPID
           MOVE    DTCHK003-PTID           TO  NACCT-PTID
           MOVE    ORCBSD01-SRYYM          TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               GO  TO  200112-NCHOZAI-DAY-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
                    OR   ( FLG-CHKEND (12) =   1 )
      *
               IF    ( NACCT-DAY (IDX0)    >   ZERO )
                   IF    ( TNYUIN-CHOZAI-DAY (IDX0)    =   1 )
                       MOVE    "120003"    TO  WRK-ERRCD
      *
                       PERFORM 210-ERRMES-HEN-SEC
                       MOVE    1           TO  FLG-CHKEND (12)
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200112-NCHOZAI-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者氏名チェック処理
      *****************************************************************
       200111-PTNAME-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(13) =   ZERO )
               MOVE    1                   TO  FLG-CHKEND (13)
           END-IF
      *
           IF    ( THKNCOMBI-MAX       =   ZERO )
            AND  ( TNACCT-HKNCOMBI-MAX =   ZERO )
               MOVE    1                   TO  FLG-CHKEND (13)
           END-IF
      *
      *    氏名チェック
           IF    ( FLG-CHKEND (13)     =   ZERO )
               PERFORM 200111-NAME-CHK-SEC
           END-IF
      *
      *    カナ氏名チェック
           IF    ( FLG-CHKEND (13)     =   ZERO )
               PERFORM 200111-KANANAME-CHK-SEC
           END-IF
      *
           .
       200111-PTNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者氏名チェック処理
      *****************************************************************
       200111-NAME-CHK-SEC             SECTION.
      *
           MOVE    PTINF-NAME          TO  WRK-PTINF-NAME
      *
           INSPECT WRK-PTINF-NAME      REPLACING   ALL "　"    BY  "  "
      *
           IF    ( WRK-PTINF-NAME      =   SPACE )
      *
               MOVE    "130003"        TO  WRK-ERRCD
               PERFORM 210-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (13) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (13)
                   GO  TO  200111-NAME-CHK-EXT
               END-IF
      *
           END-IF
      *
           IF    ( PTINF-NAME          NOT =   SPACE )
               MOVE    PTINF-NAME      TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK   NOT =   2 )
      *
                   MOVE    "130001"        TO  WRK-ERRCD
                   PERFORM 210-ERRMES-HEN-SEC
               END-IF
      *
               IF    ( ORCBSD01-CHKKBN (13) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (13)
                   GO  TO  200111-NAME-CHK-EXT
               END-IF
           END-IF
      *
           .
       200111-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者カナ氏名チェック処理
      *****************************************************************
       200111-KANANAME-CHK-SEC         SECTION.
      *
           MOVE    PTINF-KANANAME      TO  WRK-PTINF-KANANAME
      *
           INSPECT WRK-PTINF-KANANAME  REPLACING   ALL "　"    BY  "  "
      *
           IF    ( WRK-PTINF-KANANAME  =   SPACE )
      *
               MOVE    "130004"        TO  WRK-ERRCD
               PERFORM 210-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (13) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (13)
                   GO  TO  200111-KANANAME-CHK-EXT
               END-IF
      *
           END-IF
      *
           IF    ( PTINF-KANANAME          NOT =   SPACE )
               MOVE    PTINF-KANANAME      TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK   NOT =   2 )
      *
                   MOVE    "130002"    TO  WRK-ERRCD
                   PERFORM 210-ERRMES-HEN-SEC
               END-IF
      *
               IF    ( ORCBSD01-CHKKBN (13) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (13)
                   GO  TO  200111-KANANAME-CHK-EXT
               END-IF
           END-IF
      *
           .
       200111-KANANAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせチェック処理
      *****************************************************************
       200111-HKNCOMBI-CHK-SEC         SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(14) =   ZERO )
               MOVE    1                   TO  FLG-CHKEND (14)
           END-IF
      *
           IF    ( THKNCOMBI-MAX       =   ZERO )
            AND  ( TNACCT-HKNCOMBI-MAX =   ZERO )
               MOVE    1                   TO  FLG-CHKEND (14)
           END-IF
      *
           IF    ( FLG-CHKEND (14)     =   ZERO )
      *        診療会計保険組み合わせチェック
               PERFORM 200111-ACCT-HKNCOMBI-CHK-SEC
           END-IF
      *
           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
               IF    ( FLG-CHKEND (14)     =   ZERO )
      *            入院診療会計保険組み合わせチェック
                   PERFORM 200111-NACCT-HKNCOMBI-CHK-SEC
               END-IF
           END-IF
      *
           .
       200111-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計保険組み合わせチェック処理
      *****************************************************************
       200111-ACCT-HKNCOMBI-CHK-SEC    SECTION.
      *
           PERFORM VARYING IDX16   FROM    1   BY  1
                   UNTIL ( IDX16   >       THKNCOMBI-MAX )
                    OR   ( FLG-CHKEND (14) =   1 )
               PERFORM VARYING IDX17   FROM    1   BY  1
                       UNTIL ( IDX17   >   ORCSHKN-HKN-MAX )
                        OR   ( THKNCOMBI-HKNCOMBI (IDX16)
                                       =   ORCSHKN-GMN-HKNCOMBINUM
                                                              (IDX17))
                   CONTINUE
      *
               END-PERFORM
      *
               IF    ( IDX17   >   ORCSHKN-HKN-MAX )
                   MOVE    THKNCOMBI-HKNCOMBI (IDX16)
                                       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
                   STRING  "診療会計："    DELIMITED   BY  SIZE
                           WRK-OUT-INPUT   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "140001"    TO  WRK-ERRCD
                   PERFORM 215-ERRMES-HEN-SEC
                   IF    ( ORCBSD01-CHKKBN (14)    =   "1" )
                       MOVE    1       TO  FLG-CHKEND (14)
                   END-IF
               ELSE
                   IF    ( THKNCOMBI-STYMD (IDX16)
                                       >=  ORCSHKN-NAI-TEKSTYMD (IDX17))
                    AND  ( THKNCOMBI-EDYMD (IDX16)
                                       <=  ORCSHKN-NAI-TEKEDYMD (IDX17))
                       CONTINUE
                   ELSE
                       MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX17)
                                       TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    SPACE       TO  WRK-ERRMES-PLUS
                       STRING  "診療会計："    DELIMITED   BY  SIZE
                               WRK-OUT-INPUT   DELIMITED   BY  SPACE
                       INTO    WRK-ERRMES-PLUS
                       END-STRING
      *
                       MOVE    "140002"    TO  WRK-ERRCD
                       PERFORM 215-ERRMES-HEN-SEC
                       IF    ( ORCBSD01-CHKKBN (14)    =   "1" )
                           MOVE    1       TO  FLG-CHKEND (14)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200111-ACCT-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院診療会計保険組み合わせチェック処理
      *****************************************************************
       200111-NACCT-HKNCOMBI-CHK-SEC   SECTION.
      *
           PERFORM VARYING IDX16   FROM    1   BY  1
                   UNTIL ( IDX16   >       TNACCT-HKNCOMBI-MAX )
                    OR   ( FLG-CHKEND (14) =   1 )
      *
               PERFORM VARYING IDX17   FROM    1   BY  1
                       UNTIL ( IDX17   >   ORCSHKN-HKN-MAX )
                        OR   ( TNACCT-HKNCOMBI-HKNCOMBI-X (IDX16)
                                       =   ORCSHKN-GMN-HKNCOMBINUM
                                                              (IDX17))
                   CONTINUE
      *
               END-PERFORM
      *
               IF    ( IDX17   >   ORCSHKN-HKN-MAX )
                   MOVE    TNACCT-HKNCOMBI-HKNCOMBI-X (IDX16)
                                       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
                   STRING  "入院会計："    DELIMITED   BY  SIZE
                           WRK-OUT-INPUT   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "140001"    TO  WRK-ERRCD
                   PERFORM 215-ERRMES-HEN-SEC
                   IF    ( ORCBSD01-CHKKBN (14)    =   "1" )
                       MOVE    1       TO  FLG-CHKEND (14)
                   END-IF
               ELSE
                   IF    ( TNACCT-HKNCOMBI-STYMD (IDX16)
                                       >=  ORCSHKN-NAI-TEKSTYMD (IDX17))
                    AND  ( TNACCT-HKNCOMBI-EDYMD (IDX16)
                                       <=  ORCSHKN-NAI-TEKEDYMD (IDX17))
                       CONTINUE
                   ELSE
                       MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX17)
                                       TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    SPACE       TO  WRK-ERRMES-PLUS
                       STRING  "入院会計："    DELIMITED   BY  SIZE
                               WRK-OUT-INPUT   DELIMITED   BY  SPACE
                       INTO    WRK-ERRMES-PLUS
                       END-STRING
      *
                       MOVE    "140002"    TO  WRK-ERRCD
                       PERFORM 215-ERRMES-HEN-SEC
                       IF    ( ORCBSD01-CHKKBN (14)    =   "1" )
                           MOVE    1       TO  FLG-CHKEND (14)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200111-NACCT-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他必須チェック処理
      *****************************************************************
       200111-SONOTA-HISSU-CHK-SEC     SECTION.
      *
           IF    ( ORCBSD01-NYUGAIKBN    = "1" )
      *
      *        減算情報チェック処理
               PERFORM 200111-GENZAN-CHK-SEC
      *
      *        入院料加算チェック処理
               PERFORM 200111-NYUKSN-CHK-SEC
      *
           END-IF
      *
           .
       200111-SONOTA-HISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    減算情報チェック処理
      *****************************************************************
       200111-GENZAN-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX21   FROM    1   BY  1
                   UNTIL ( IDX21   >   CONST-TGENZAN-MAX )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
      *
                   IF    ( TGENZAN-SYS-DAY (IDX21 IDX0)    =   1 )
                    AND  ( TGENZAN-SRY-DAY (IDX21 IDX0)    =   1 )
      *
                       COMPUTE TGENZAN-ERR-CNT (IDX21)
                           =   TGENZAN-ERR-CNT (IDX21)     +   1
      *
                       MOVE    1       TO  TGENZAN-ERR-DAY (IDX21 IDX0)
      *
                   END-IF
      *
               END-PERFORM
      *
               IF    ( TGENZAN-ERR-CNT (IDX21) >   ZERO )
      *
                   MOVE    TGENZAN-SRYCD (IDX21)
                                               TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    WRK-OUT-INPUT   TO  TNS-NAME
                   END-IF
      *
                   INITIALIZE  WRK-NISSU-AREA
                   MOVE    TGENZAN-ERR-DAY-G (IDX21)
                                               TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
      *
                   STRING  TNS-NAME        DELIMITED   BY  SPACE
                           WRK-NISSU-MES   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "990001"    TO  WRK-ERRCD
                   PERFORM 215-ERRMES-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-GENZAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算チェック処理
      *****************************************************************
       200111-NYUKSN-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TNACCT-HKNCOMBI-MAX )
      *
               PERFORM VARYING IDX22   FROM    1   BY  1
                       UNTIL ( IDX22   >   31 )
      *
                   IF    ( TNACCT-HKNCOMBI-HKNCOMBI-DAY (IDX0 IDX22)
                                       NOT =   ZERO )
                       PERFORM VARYING IDX23   FROM    1   BY  1
                               UNTIL ( IDX23   >   TNYUKSN-MAX )
                           IF    ( TNACCT-HKNCOMBI-HKNCOMBI-X (IDX0)
                                       NOT =   TNYUKSN-HKNCOMBI (IDX23))
                               IF  ( TNYUKSN-HKNCOMBI-DAY (IDX23 IDX22)
                                       NOT =   ZERO )
                                   COMPUTE TNYUKSN-ERR-CNT (IDX23)
                                       =   TNYUKSN-ERR-CNT (IDX23)
                                       +   1
                                   MOVE    1   TO  TNYUKSN-ERR-DAY
                                                          (IDX23 IDX22)
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TNYUKSN-MAX )
      *
               IF    ( TNYUKSN-ERR-CNT (IDX0) >   ZERO )
      *
                   MOVE    TNYUKSN-HKNCOMBI (IDX0)
                                       TO  WRK-ORCSHKN-HKNCOMBI
                   PERFORM 200111-HKNCOMBIMEI-HEN-SEC
      *
                   INITIALIZE  WRK-NISSU-AREA
                   MOVE    TNYUKSN-ERR-DAY-G (IDX0)
                                       TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
      *
                   STRING  WRK-ORCSHKN-HKNCOMBIMEI
                                           DELIMITED   BY  SPACE
                           WRK-NISSU-MES   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "990002"    TO  WRK-ERRCD
                   PERFORM 215-ERRMES-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-NYUKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理
      *****************************************************************
       200111-NISSU-HEN-SEC            SECTION.
      *
           MOVE    1                   TO  WRK-NISSU-PTR
           MOVE    1                   TO  WRK-NISSU-ST
           MOVE    SPACE               TO  WRK-NISSU-MES
           MOVE    ZERO                TO  WRK-NISSU-KUGIRI-FLG
      *
           PERFORM UNTIL ( WRK-NISSU-ST    >   31 )
      *
               IF    ( WRK-NISSU-DAY (WRK-NISSU-ST )
                                       NOT =   ZERO )
      *
                   MOVE    ZERO        TO  WRK-NISSU-RENZOKU-CNT
                   COMPUTE WRK-NISSU-ED    =   WRK-NISSU-ST    +   1
                   PERFORM UNTIL ( WRK-NISSU-ED    >   31 )
                            OR   ( WRK-NISSU-DAY (WRK-NISSU-ED)
                                           =   ZERO )
                       COMPUTE WRK-NISSU-ED
                            =  WRK-NISSU-ED  +   1
      *
                       COMPUTE WRK-NISSU-RENZOKU-CNT
                            =  WRK-NISSU-RENZOKU-CNT   +   1
      *
                   END-PERFORM
      *
                   IF    ( WRK-NISSU-KUGIRI-FLG   =   ZERO )
                       STRING  "　"            DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
      *
                       MOVE    1       TO  WRK-NISSU-KUGIRI-FLG
      *
                   ELSE
                       STRING  "，"            DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                   END-IF
      *
                   IF    ( WRK-NISSU-RENZOKU-CNT  >   ZERO )
                       COMPUTE WRK-NISSU-ED    =   WRK-NISSU-ED    -   1
      *
                       IF    ( WRK-NISSU-ST    <   10 )
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-9
                       ELSE
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-Z9
                       END-IF
      *
                       IF    ( WRK-NISSU-ED    <   10 )
                           MOVE    WRK-NISSU-ED    TO  WRK-NISSU-ED-9
                       ELSE
                           MOVE    WRK-NISSU-ED    TO  WRK-NISSU-ED-Z9
                       END-IF
      *
                       STRING  WRK-NISSU-ST-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                               "〜"                DELIMITED   BY  SIZE
                               WRK-NISSU-ED-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                       COMPUTE WRK-NISSU-ST
                           =   WRK-NISSU-ED      +   1
                   ELSE
      *
                       IF    ( WRK-NISSU-ST    <   10 )
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-9
                       ELSE
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-Z9
                       END-IF
      *
                       STRING  WRK-NISSU-ST-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                       COMPUTE WRK-NISSU-ST
                           =   WRK-NISSU-ST      +   1
                   END-IF
      *
               ELSE
                   COMPUTE WRK-NISSU-ST
                       =   WRK-NISSU-ST      +   1
               END-IF
           END-PERFORM
      *
           MOVE    WRK-NISSU-MES       TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-NISSU-MES
      *
           .
       200111-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       200111-HKNCOMBIMEI-HEN-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-ORCSHKN-HKNCOMBIMEI
      *
           PERFORM VARYING IDX-COMB    FROM    1   BY  1
                   UNTIL ( IDX-COMB    >   ORCSHKN-HKN-MAX )
                    OR   ( WRK-ORCSHKN-HKNCOMBI
                                       =   ORCSHKN-GMN-HKNCOMBINUM
                                                        (IDX-COMB))
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-COMB            >   ORCSHKN-HKN-MAX )
               MOVE    WRK-ORCSHKN-HKNCOMBI
                                   TO  WRK-MAE-INPUT
           ELSE
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX-COMB)
                                   TO  WRK-MAE-INPUT
           END-IF
      *
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-ORCSHKN-HKNCOMBIMEI
      *
           .
       200111-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（メッセージに付加する情報なし）
      *****************************************************************
       210-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           MOVE    WRK-ERRMES      TO  DTCHK004-ERRMSG
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
           DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       210-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（１）
      *****************************************************************
       211-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-HKNNUM-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( SRH-HKNNUMMEI       =   SPACE )
               MOVE    WRK-ERRMES      TO  DTCHK004-ERRMSG
           ELSE
               STRING  "［"            DELIMITED   BY  SIZE
                       SRH-HKNNUMMEI   DELIMITED   BY  SPACE
                       "］"            DELIMITED   BY  SIZE
                       WRK-ERRMES      DELIMITED   BY  SPACE
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
           DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       211-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（２）
      *****************************************************************
       212-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-KOHNUM-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( SRH-KOHNUMMEI       =   SPACE )
               MOVE    WRK-ERRMES      TO  DTCHK004-ERRMSG
           ELSE
               STRING  "［"            DELIMITED   BY  SIZE
                       SRH-KOHNUMMEI   DELIMITED   BY  SPACE
                       "］"            DELIMITED   BY  SIZE
                       WRK-ERRMES      DELIMITED   BY  SPACE
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
           DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       212-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（３）
      *****************************************************************
       213-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-SRYKA-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           STRING  "［"                DELIMITED   BY  SIZE
                   SRH-1005-SRYKAMEI2  DELIMITED   BY  SPACE
                   "］"                DELIMITED   BY  SIZE
                   WRK-ERRMES          DELIMITED   BY  SPACE
                   "【"                DELIMITED   BY  SIZE
                   WRK-ERRMES-PLUS     DELIMITED   BY  SPACE
                   "】"
           INTO    DTCHK004-ERRMSG
           END-STRING
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
           DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       213-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（４）
      *****************************************************************
       214-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-SRYKA-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           STRING  "［"                DELIMITED   BY  SIZE
                   SRH-1005-SRYKAMEI2  DELIMITED   BY  SPACE
                   "］"                DELIMITED   BY  SIZE
                   WRK-ERRMES          DELIMITED   BY  SPACE
           INTO    DTCHK004-ERRMSG
           END-STRING
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
           DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       214-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（５）
      *****************************************************************
       215-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           STRING  WRK-ERRMES          DELIMITED   BY  SPACE
                   "【"                DELIMITED   BY  SIZE
                   WRK-ERRMES-PLUS     DELIMITED   BY  SPACE
                   "】"
           INTO    DTCHK004-ERRMSG
           END-STRING
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
           DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       215-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（６）
      *****************************************************************
       216-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-HKNNUM-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( SRH-HKNNUMMEI       =   SPACE )
               STRING  WRK-ERRMES      DELIMITED   BY  SPACE
                   "【"                DELIMITED   BY  SIZE
                   WRK-ERRMES-PLUS     DELIMITED   BY  SPACE
                   "】"
               INTO    DTCHK004-ERRMSG
               END-STRING
           ELSE
               STRING  "［"            DELIMITED   BY  SIZE
                       SRH-HKNNUMMEI   DELIMITED   BY  SPACE
                       "］"            DELIMITED   BY  SIZE
                       WRK-ERRMES      DELIMITED   BY  SPACE
                       "【"            DELIMITED   BY  SIZE
                       WRK-ERRMES-PLUS DELIMITED   BY  SPACE
                       "】"
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
           DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       216-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ取得処理
      *****************************************************************
       2111-ERRMES-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMES
      *
           PERFORM VARYING IDX-ERRCD   FROM    1   BY  1
                   UNTIL ( IDX-ERRCD   >   ERRMES-MAX )
      *
               IF    ( WRK-ERRCD       =   ERRMES-CD (IDX-ERRCD) )
                   MOVE    ERRMES-MESSAGE (IDX-ERRCD)
                                       TO  WRK-ERRMES
                   MOVE    ERRMES-MAX  TO  IDX-ERRCD
               END-IF
      *
           END-PERFORM
      *
           .
       2111-ERRMES-GET-EXT.
           EXIT.
      *****************************************************************
      *    チェック結果ファイル共通編集項目編集処理
      *****************************************************************
       2111-DTCHK004-KYOTU-HEN-SEC     SECTION.
      *
           INITIALIZE  DTCHK004-REC
           MOVE    PTNUM-PTNUM         TO  DTCHK004-PTNUM
           MOVE    WRK-ERRCD           TO  DTCHK004-ERRCD
      *
           COMPUTE CNT-CHKERR  =   CNT-CHKERR  +   1
           MOVE    CNT-CHKERR          TO  DTCHK004-ERRCD-SEQ
      *
           MOVE    PTINF-PTID          TO  DTCHK004-PTID
           MOVE    PTINF-NAME          TO  DTCHK004-NAME
           MOVE    PTINF-BIRTHDAY      TO  DTCHK004-BIRTHDAY
      *
           .
       2111-DTCHK004-KYOTU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類取得処理
      *****************************************************************
       2111-HKNNUM-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRH-HKNNUMMEI
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-HKNNUM-MAX )
               IF    ( SRH-HKNNUM  =   SRH-HKNNUML (SRH-IDX) )
                   MOVE    SRH-HKNNUMMEIL (SRH-IDX)
                                       TO  SRH-HKNNUMMEI
                   MOVE    SRH-HKNNUM-MAX
                                       TO  SRH-IDX
               END-IF
           END-PERFORM
      *
      *
           .
       2111-HKNNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類取得処理
      *****************************************************************
       2111-KOHNUM-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRH-KOHNUMMEI
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-KOHNUM-MAX )
               IF    ( SRH-KOHNUM  =   "027" )
                AND  ( SRH-PAYKBN  =   "01" OR "02" )
                   IF    ( SRH-KOHNUM  =   SRH-KOHNUML (SRH-IDX) )
                    AND  ( SRH-PAYKBN  =   SRH-PAYKBNL (SRH-IDX) )
                       MOVE    SRH-KOHNUMMEIL (SRH-IDX)
                                       TO  SRH-KOHNUMMEI
                       MOVE    SRH-KOHNUM-MAX
                                       TO  SRH-IDX
                   END-IF
               ELSE
                   IF    ( SRH-KOHNUM  =   SRH-KOHNUML (SRH-IDX) )
                    AND  ( SRH-PAYKBNL (SRH-IDX)   =   SPACE )
                       MOVE    SRH-KOHNUMMEIL (SRH-IDX)
                                       TO  SRH-KOHNUMMEI
                       MOVE    SRH-KOHNUM-MAX
                                       TO  SRH-IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2111-KOHNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       2111-SRYKA-GET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SRH-1005-SRYKAMEI1
                                           SRH-1005-SRYKAMEI2
                                           SRH-1005-SRYKAMEI3
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-1005-MAX )
               IF    ( SRH-1005-SRYKA  =   SRH-1005-SRYKAL (SRH-IDX) )
                   MOVE    SRH-1005-SRYKAMEI1L (SRH-IDX)
                                       TO  SRH-1005-SRYKAMEI1
                   MOVE    SRH-1005-SRYKAMEI2L (SRH-IDX)
                                       TO  SRH-1005-SRYKAMEI2
                   MOVE    SRH-1005-SRYKAMEI3L (SRH-IDX)
                                       TO  SRH-1005-SRYKAMEI3
                   MOVE    SRH-1005-MAX
                                       TO  SRH-IDX
               END-IF
           END-PERFORM
      *
           .
       2111-SRYKA-GET-EXT.
           EXIT.
      *****************************************************************
      *    公費負担者番号受給者番号抜けチェック編集処理
      *****************************************************************
       2111-SYS1040-GET-SEC            SECTION.
      *
           MOVE    SPACE               TO  SRH-1040-FTNJANUMCHK
                                           SRH-1040-JKYSNUMCHK
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-1040-MAX )
               IF    ( SRH-1040-KOHNUM  =   SRH-1040-KOHNUML (SRH-IDX) )
                   MOVE    SRH-1040-FTNJANUMCHKL (SRH-IDX)
                                       TO  SRH-1040-FTNJANUMCHK
                   MOVE    SRH-1040-JKYSNUMCHKL  (SRH-IDX)
                                       TO  SRH-1040-JKYSNUMCHK
                   MOVE    SRH-1040-MAX
                                       TO  SRH-IDX
               END-IF
           END-PERFORM
      *
           .
       2111-SYS1040-GET-EXT.
           EXIT.
      *****************************************************************
      *    処理経過編集処理
      *****************************************************************
       20011-SYORI-KEIKA-HEN-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-SHO
                                       WRK-AMARI
           DIVIDE  50          INTO    CNT-DTCHK003   GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI       NOT =   ZERO )
               GO  TO  20011-SYORI-KEIKA-HEN-EXT
           END-IF
      *
           MOVE    CNT-DTCHK003    TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-KNS-N
           ACCEPT  SYS-TIME        FROM    TIME
           MOVE    SPACE           TO  WRK-MAE-INPUT
           MOVE    SYS-TIME(1:2)   TO  WRK-MAE-INPUT(1:2)
           MOVE    "."             TO  WRK-MAE-INPUT(3:1)
           MOVE    SYS-TIME(3:2)   TO  WRK-MAE-INPUT(4:2)
           MOVE    "."             TO  WRK-MAE-INPUT(6:1)
           MOVE    SYS-TIME(5:2)   TO  WRK-MAE-INPUT(7:2)
      *    PERFORM 800-ZENKAKU-HEN-SEC
      *    MOVE    WRK-OUT-INPUT   TO  WRK-TIME-N
           MOVE    WRK-MAE-INPUT   TO  WRK-TIME-N
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "チェック済患者数："    DELIMITED BY SIZE
                   WRK-KNS-N           DELIMITED BY SPACE
                   "人　"              DELIMITED BY SIZE
                   WRK-TIME-N          DELIMITED BY SPACE
           INTO    JOB-YOBI
           END-STRING
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCDTCHK002"       TO  JOB-PGID
           MOVE    "データチェック"    TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
      *
           .
       20011-SYORI-KEIKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   DTCHKERR-FILE
           IF    ( STS-DTCHKERR        =   ZERO )
               CLOSE   DTCHKERR-FILE
           ELSE
               OPEN    OUTPUT              DTCHKERR-FILE
      *
               MOVE    WRK-DTCHKERR    TO  DTCHKERR-REC
               WRITE   DTCHKERR-REC
               CLOSE   DTCHKERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-DTCHKERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           CLOSE                       DTCHK003-FILE
                                       DTCHK004-FILE
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "*** ORCDTCHK002 END ***"
           DISPLAY "DTCHK004 CNT [" CNT-DTCHK004 "]"
           DISPLAY "***********************"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC      SECTION.
      *
           MOVE    WRK-STR-LEFT    TO  IDXE
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >       500 )
      *
               IF    ( WRK-STR-LEFT (IDXE:1)
                                       NOT =   SPACE )
                   MOVE    WRK-STR-LEFT (IDXE:)
                                           TO  WRK-STR-LEFT-TMP
                   MOVE    WRK-STR-LEFT-TMP
                                           TO  WRK-STR-LEFT
                   MOVE    500             TO  IDXE
               END-IF
      *
           END-PERFORM
      *
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-KANA-CHK-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-KANA-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  ORCSKANACHKAREA
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9               TO  FLG-KANACHK
               GO  TO  800-KANA-CHK-EXT
           END-IF
      *
      *    全角文字の場合、間に半角スペースがなかったかチェック
           IF    ( KANACHK-SYUBETU     =   2  )
            AND  ( KANACHK-RC2         =   8  )
               MOVE    9               TO  FLG-KANACHK
           ELSE
               MOVE    KANACHK-SYUBETU TO  FLG-KANACHK
           END-IF
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE  WRK-MAE-INPUT     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUT-INPUT
               MOVE    KANACHK-MAX TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    対象患者ファイル読込処理
      *****************************************************************
       900-DTCHK003-READ-SEC           SECTION.
      *
           READ    DTCHK003-FILE
           AT  END
               MOVE    1               TO  FLG-DTCHK003
           END-READ
      *
           IF    ( FLG-DTCHK003        =   ZERO )
               COMPUTE CNT-DTCHK003    =   CNT-DTCHK003    +   1
           END-IF
      *
           .
       900-DTCHK003-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理(CHK-KEY5)
      *****************************************************************
       900-CHK-KEY5-SEL-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    WRK-CHK-CHKKBN      TO  CHK-CHKKBN
           MOVE    WRK-CHK-CDKBN       TO  CHK-CDKBN
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    WRK-KJNYMD          TO  ORC-DBYMD
           MOVE    "CHK-KEY5"          TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           MOVE    "CHK-KEY5"          TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
      *
       900-CHK-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計読込処理(KEY28)
      *****************************************************************
       900-SRYACCT-KEY28-FETCH-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-SRYACCT     NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    "SRYACCT-KEY28" TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  SRYACCT-REC
      *
                   MOVE    1           TO  FLG-LOOP-END
      *
                   IF    ( ACCT-ZAIKAISU   =   ZERO )
      *
                        MOVE   ZERO    TO  FLG-LOOP-END
      *
                   END-IF
      *
                   IF    ( ACCT-HKNCOMBI       =   9999 )
                        MOVE   ZERO    TO  FLG-LOOP-END
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-SRYACCT
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-SRYACCT-KEY28-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    患者病名読込処理
      *****************************************************************
       900-PTBYOMEI-FETCH-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  PTBYOMEI-REC
      *
                   MOVE    1           TO  FLG-LOOP-END
      *
      *            レセプト表示期間を過ぎていないかチェック
                   IF    ( PTBYO-REZEMM    >   ZERO )
      *
                       MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
      *
                       IF    ( PTBYO-REZEMM    >   1 )
                           MOVE    "2"         TO  WRK-ZOGENPTN
                           COMPUTE WRK-ZOGEN   =   PTBYO-REZEMM
                                               -   1
                           PERFORM 800-CALENDAR-SEC
                       END-IF
      *
                       IF    ( WRK-KJNYMD(1:6) >   WRK-SYMD(1:6))
                           MOVE    ZERO        TO  FLG-LOOP-END
                       END-IF
      *
                   END-IF
      *
      *            レセプト表示を行うか
                   IF    ( PTBYO-REZEFLG   =   "1" )
                           MOVE    ZERO        TO  FLG-LOOP-END
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-PTBYOMEI
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-PTBYOMEI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY1)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               MOVE    WRK-TNS-YMD         TO  ORC-DBYMD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
      *
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               PERFORM 910-DBCLOSE-SEC
           END-IF
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           IF    ( WRK-SRYCD (1:1)     =   "6" )
               PERFORM VARYING IDX-TTNS    FROM    1   BY  1
                       UNTIL ( IDX-TTNS    >   TTNS-YKZ-MAX )
                        OR   ( TTNS-FLG    =   1 )
                   IF    ( TTNS-YKZ-KEY-SRYCD (IDX-TTNS)
                                           =   WRK-SRYCD )
                     AND ( TTNS-YKZ-KEY-YUKOSTYMD (IDX-TTNS)
                                          <=   WRK-TNS-YMD )
                     AND ( TTNS-YKZ-KEY-YUKOEDYMD (IDX-TTNS)
                                          >=   WRK-TNS-YMD )
                       MOVE    1           TO  TTNS-FLG
                       MOVE    TTNS-YKZ-DAT (IDX-TTNS)
                                           TO  TNS-TENSU-REC
                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING IDX-TTNS    FROM    1   BY  1
                       UNTIL ( IDX-TTNS    >   TTNS-ETC-MAX )
                        OR   ( TTNS-FLG    =   1 )
                   IF    ( TTNS-ETC-KEY-SRYCD (IDX-TTNS)
                                           =   WRK-SRYCD )
                     AND ( TTNS-ETC-KEY-YUKOSTYMD (IDX-TTNS)
                                          <=   WRK-TNS-YMD )
                     AND ( TTNS-ETC-KEY-YUKOEDYMD (IDX-TTNS)
                                          >=   WRK-TNS-YMD )
                       MOVE    1           TO  TTNS-FLG
                       MOVE    TTNS-ETC-DAT (IDX-TTNS)
                                           TO  TNS-TENSU-REC
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TNS-SRYCD (1:1)     =   "6" )
               IF    ( CONST-TTNS-MAX  >   TTNS-YKZ-MAX )
                   COMPUTE TTNS-YKZ-MAX    =   TTNS-YKZ-MAX    +   1
                   MOVE    TNS-SRYCD   TO  TTNS-YKZ-KEY-SRYCD
                                                       (TTNS-YKZ-MAX)
                   MOVE    TNS-YUKOSTYMD   TO  TTNS-YKZ-KEY-YUKOSTYMD
                                                       (TTNS-YKZ-MAX)
                   MOVE    TNS-YUKOEDYMD   TO  TTNS-YKZ-KEY-YUKOEDYMD
                                                       (TTNS-YKZ-MAX)
                   MOVE    TNS-TENSU-REC   TO  TTNS-YKZ-DAT
                                                       (TTNS-YKZ-MAX)
               END-IF
           ELSE
               IF    ( CONST-TTNS-MAX  >   TTNS-ETC-MAX )
                   COMPUTE TTNS-ETC-MAX    =   TTNS-ETC-MAX    +   1
                   MOVE    TNS-SRYCD   TO  TTNS-ETC-KEY-SRYCD
                                                       (TTNS-ETC-MAX)
                   MOVE    TNS-YUKOSTYMD   TO  TTNS-ETC-KEY-YUKOSTYMD
                                                       (TTNS-ETC-MAX)
                   MOVE    TNS-YUKOEDYMD   TO  TTNS-ETC-KEY-YUKOEDYMD
                                                       (TTNS-ETC-MAX)
                   MOVE    TNS-TENSU-REC   TO  TTNS-ETC-DAT
                                                       (TTNS-ETC-MAX)
               END-IF
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
           INITIALIZE                      PTNUM-REC
           MOVE    DTCHK003-HOSPID     TO  PTNUM-HOSPID
           MOVE    DTCHK003-PTID       TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
      *
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE                      PTINF-REC
           MOVE    DTCHK003-HOSPID     TO  PTINF-HOSPID
           MOVE    DTCHK003-PTID       TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
      *    保険組み合わせ
           INITIALIZE                      HKNCOMBI-REC
           MOVE    DTCHK003-HOSPID     TO  COMB-HOSPID
           MOVE    DTCHK003-PTID       TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報検索処理
      *****************************************************************
       900-PTHKNINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTHKNINF
           INITIALIZE                      PTHKNINF-REC
           MOVE    DTCHK003-HOSPID     TO  PTHKN-HOSPID
           MOVE    DTCHK003-PTID       TO  PTHKN-PTID
           MOVE    WRK-HKNID           TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
      *
       900-PTHKNINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理
      *****************************************************************
       900-PTKOHINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKOHINF
           INITIALIZE                      PTKOHINF-REC
           MOVE    DTCHK003-HOSPID     TO  PTKOH-HOSPID
           MOVE    DTCHK003-PTID       TO  PTKOH-PTID
           MOVE    WRK-KOHID           TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
      *
       900-PTKOHINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病名検索処理
      *****************************************************************
       900-BYOMEI-KEY-SEL-SEC          SECTION.
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    WRK-BYOMEICD        TO  BYO-BYOMEICD
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "BYOMEI-KEY"        TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "BYOMEI-KEY"        TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
           .
      *
       900-BYOMEI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       900-DBDISCONNECT-EXT.
           EXIT.
