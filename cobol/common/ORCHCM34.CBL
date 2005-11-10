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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCHCM34.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : オンライン帳票
      *  コンポーネント名  : 会計カード
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/15    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.09.19  自院歴より継続して入院している
      *                                     場合の入院日を修正
      *  01.00.02    NACL-太田    04.01.16  包括分の考慮を追加
      *  01.00.03    NACL-太田    04.01.19  室料差額消費税区分の考慮を追加
      *  01.00.04    NACL-太田    04.02.19  減算時の黒三角編集漏れを修正
      *  01.00.05    NACL-太田    04.04.01  褥瘡患者管理加算の対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC                 PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
           COPY    "HCM341V01.INC"
                   REPLACING   //HCM341V01//  BY //HCM341//.
           COPY    "HCM342V01.INC"
                   REPLACING   //HCM342V01//  BY //HCM342//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA                            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-JYURRK                          PIC 9(01).
           03  FLG-HEAD                            PIC 9(01).
           03  FLG-OVER                            PIC 9(01).
           03  FLG-HKNNUM                          PIC 9(01).
           03  FLG-PTHKNINF                        PIC 9(01).
           03  FLG-PTKOHINF                        PIC 9(01).
           03  FLG-PTBYOMEI                        PIC 9(01).
           03  FLG-ZENSP                           PIC 9(01).
           03  FLG-KYUSEI                          PIC 9(01).
           03  FLG-UTAGAI                          PIC 9(01).
           03  FLG-SRYACCT                         PIC 9(01).
           03  FLG-SRYACT                          PIC 9(01).
           03  FLG-NYUINACCT                       PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-SP                              PIC 9(01).
           03  FLG-LINE                            PIC 9(01).
           03  FLG-MINUS                           PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-NACCTCHK-EXT                    PIC 9(01).
           03  FLG-BRMSAGAKU-HEN                   PIC 9(01).
           03  FLG-PTNYUINRRK                      PIC 9(01).
           03  FLG-LINECHK                         PIC 9(01).
           03  FLG-RSIJBI-AREA-HEN                 PIC 9(01).
           03  FLG-JIBAI                           PIC 9(01).
           03  FLG-ROSAI                           PIC 9(01).
           03  FLG-SRYACT-HEN5                     PIC 9(01).
           03  FLG-COMMENT-CHK                     PIC 9(01).
           03  FLG-PTCOM                           PIC 9(01).
           03  FLG-YAKUSOKU                        PIC 9(01).
           03  FLG-HOUKATU                         PIC 9(01).
           03  FLG-ZAIMATOME                       PIC 9(01).
           03  FLG-EDIT                            PIC 9(01).
           03  FLG-JBI-KZI-OUT                     PIC 9(01).
           03  FLG-SRYSTATUS                      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                            PIC 9(03).
           03  CNT-HKNCOMBI-LINE                   PIC 9(05).
           03  CNT-HKNKOH-LINE                     PIC 9(05).
           03  CNT-BYOMEI-LINE                     PIC 9(05).
           03  CNT-SRYACT-LINE                     PIC 9(05).
           03  CNT-YKZINGAI                        PIC 9(05).
           03  CNT-YKZHKT                          PIC 9(05).
           03  CNT-SPACE                           PIC 9(05).
           03  CNT-STR                             PIC 9(05).
           03  CNT-BYOMEIMOJI                      PIC 9(05).
           03  CNT-HOUSRYACT                       PIC 9(05).
           03  CNT-RENPAKU                         PIC 9(03).
           03  CNT-PTNYUINRRK                      PIC 9(05).
           03  CNT-RSIJBI-KIN                      PIC 9(05).
           03  CNT-RSIJBI-TEN                      PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                          PIC 9(02).
               05  SYS-MM                          PIC 9(02).
               05  SYS-DD                          PIC 9(02).
           03  SYS-TIME                            PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDX6                                PIC 9(05).
           03  IDX7                                PIC 9(05).
           03  IDX8                                PIC 9(05).
           03  IDX9                                PIC 9(05).
           03  IDX10                               PIC 9(05).
           03  IDX11                               PIC 9(05).
           03  IDX12                               PIC 9(05).
           03  IDX13                               PIC 9(05).
           03  IDX14                               PIC 9(05).
           03  IDX15                               PIC 9(05).
           03  IDX16                               PIC 9(05).
           03  IDX17                               PIC 9(05).
           03  IDX18                               PIC 9(05).
           03  IDX19                               PIC 9(05).
           03  IDX20                               PIC 9(05).
           03  IDX21                               PIC 9(05).
           03  IDX22                               PIC 9(05).
           03  IDX23                               PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDXB                                PIC 9(05).
           03  IDXC                                PIC 9(05).
           03  IDXD                                PIC 9(05).
           03  IDXE                                PIC 9(05).
           03  IDXF                                PIC 9(05).
           03  IDXG                                PIC 9(05).
           03  IDXH                                PIC 9(05).
           03  IDXI                                PIC 9(05).
           03  IDXJ                                PIC 9(05).
           03  IDX-S                               PIC 9(05).
           03  IDX-RRK                             PIC 9(05).
           03  IDX-RJ                              PIC 9(05).
      *
      *    同一セクション内のみ使用
           03  IDX0                                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                            PIC X(08).
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
           03  WRK-HCM34-AREA.
               05  WRK-HCM34-SRYYM                 PIC X(16).
               05  WRK-HCM34-SAKYMD                PIC X(22).
               05  WRK-HCM34-PTNUM                 PIC X(10).
               05  WRK-HCM34-PTNUM2                PIC X(20).
               05  WRK-HCM34-BIRTHDAY              PIC X(22).
               05  WRK-HCM34-SRYKA-MAX             PIC 9(05).
               05  WRK-HCM34-SRYKA-G.
                   07  WRK-HCM34-SRYKA             PIC X(16)
                                                   OCCURS 2.
               05  WRK-HCM34-NYUINYMD              PIC X(22).
               05  WRK-HCM34-TAIINYMD              PIC X(22).
               05  WRK-HCM34-LBLSRYKA              PIC X(06).
               05  WRK-HCM34-LBLNYUINYMD           PIC X(06).
               05  WRK-HCM34-LBLTAIINYMD           PIC X(06).
               05  WRK-HCM34-BTUNAME               PIC X(30).
           03  WRK-STR                             PIC X(500).
           03  WRK-STR-LEFT                        PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES       WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99               PIC Z(9)9.
               05                                  PIC X(490).
           03  WRK-STR-LEFT-TMP                    PIC X(500).
           03  WRK-STRLEN                          PIC 9(05).
           03  WRK-STRLEN-MAX                      PIC 9(05).
      *        全角編集用項目
           03  WRK-N.
               05  WRK-HKNCOMBINUM-N               PIC X(08).
               05  WRK-TEKSTYMD-N                  PIC X(18).
               05  WRK-TEKEDYMD-N                  PIC X(18).
           03  WRK-MAE-INPUT                       PIC X(300).
           03  WRK-OUT-INPUT                       PIC X(300).
           03  WRK-OUT-MAX                         PIC 9(05).
           03  WRK-ZZ9                             PIC ZZ9.
           03  WRK-Z9                              PIC Z9.
           03  WRK-WIDTH                           PIC 9(05).
      *    診療行為欄に編集を行う項目を判断する区分
      *    1:保険組合せ／2:保険／3:公費／4:傷病名／5:診療行為
      *    空白:上記以外
           03  WRK-ITEM-KBN                        PIC X(01).
           03  WRK-SHO                             PIC 9(03).
           03  WRK-AMARI                           PIC 9(03).
           03  WRK-PTHKN-HKNNUM                    PIC X(03).
           03  WRK-PTHKN-HONKZKKBN                 PIC X(01).
           03  WRK-PTHKN-HONKZKKBNMEI              PIC X(10).
           03  WRK-PTHKN-HOJOKBN                   PIC X(01).
           03  WRK-PTHKN-HOJOKBNMEI                PIC X(10).
           03  WRK-PTHKN-CONTKBN                   PIC X(01).
           03  WRK-PTHKN-CONTKBNMEI                PIC X(10).
           03  WRK-PTKOH-FTNJANUM-N                PIC X(16).
           03  WRK-PTKOH-JKYSNUM-N                 PIC X(40).
           03  WRK-PTBYO-TENKIKBN                  PIC X(01).
           03  WRK-PTBYO-TENKIKBNMEI               PIC X(04).
           03  WRK-PTBYO-BYOMEI                    PIC X(200).
           03  WRK-PTBYO-BYOMEIMOJI                PIC 9(05).
           03  WRK-PTBYO-UTAGAIFLG                 PIC X(01).
           03  WRK-BASEYMD.
               05  WRK-BYY                         PIC 9(04).
               05  WRK-BMM                         PIC 9(02).
               05  WRK-BDD                         PIC 9(02).
           03  WRK-AGE70-YMD.
               05  WRK-AGE70-YY                    PIC 9(04).
               05  WRK-AGE70-MM                    PIC 9(02).
               05  WRK-AGE70-DD                    PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY                       PIC 9(04).
               05  WRK-SRYMM                       PIC 9(02).
               05  WRK-SRYDD                       PIC 9(02).
           03  WRK-PNT                             PIC 9(05).
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
      *    数量表示用
           03  WRK-SURYO                           PIC 9(05)V9(03).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1                    PIC 9(05).
               05  WRK-SURYO-S2                    PIC 9(03).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1                    PIC ZZZZZ.
               05  WRK-SURYO-Z19       REDEFINES   WRK-SURYO-Z1
                                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2                    PIC X(01).
               05  WRK-SURYO-Z3                    PIC ZZZ.
               05  WRK-SURYO-Z3-X      REDEFINES   WRK-SURYO-Z3
                                                   PIC X(03).
      *    分画数画面編集用
           03  WRK-BUNGSU                          PIC X(10).
           03  WRK-TENSUKAISU                      PIC X(20).
           03  WRK-KEY-HKNCOMBI                    PIC X(04).
           03  WRK-TNS-HOUKNSKBN                   PIC 9(02).
           03  WRK-KUGIRI                          PIC X(02).
           03  WRK-TTL-JIHI                        PIC 9(09).
           03  WRK-TTL-JIHI-TAX                    PIC 9(09).
           03  WRK-TTL-BRMSAGAKU                   PIC 9(09).
           03  WRK-TTL-BRMSAGAKU-TAX               PIC 9(09).
           03  WRK-KINGAKKU-LBL                    PIC X(100).
           03  WRK-KINGAKKU                        PIC 9(09).
           03  WRK-GAIHAKU-ST                      PIC 9(02).
           03  WRK-GAIHAKU-ST-Z9                   PIC Z9.
           03  WRK-GAIHAKU-ED                      PIC 9(02).
           03  WRK-GAIHAKU-ED-Z9                   PIC Z9.
           03  WRK-HKNCOMBI-KEY-MAE                PIC X(04).
           03  WRK-HKNCOMBI-KEY                    PIC X(04).
           03  WRK-HKNCOMBINUM                     PIC 9(04).
           03  WRK-HKNNUM                          PIC X(03).
           03  WRK-NACCT-HKNCOMBI-G.
               05  WRK-NACCT-HKNCOMBI-IDX          PIC 9(05).
               05  WRK-NACCT-HKNCOMBI-STIDX        PIC 9(05).
               05  WRK-NACCT-HKNCOMBI-MAX          PIC 9(05).
               05  WRK-NACCT-HKNCOMBI-OCC          OCCURS  21.
                   07  WRK-NACCT-HKNCOMBI-KEY.
                     09  WRK-NACCT-HKNCOMBI        PIC 9(04).
                     09  WRK-NACCT-HKNCOMBI-X  REDEFINES
                         WRK-NACCT-HKNCOMBI        PIC X(04).
           03  WRK-SHONUM-G.
               05  WRK-SHONUM-MAX                  PIC 9(03).
               05  WRK-SHONUM                      PIC 9(03)
                                                   OCCURS  20.
           03  WRK-RRK-NYUINYMD                    PIC X(18).
           03  WRK-RRK-TAIINYMD                    PIC X(18).
           03  WRK-ZAITEN                          PIC 9(08).
           03  WRK-ZAIKAISU                        PIC 9(07).
           03  WRK-SRYCD                           PIC X(09).
           03  WRK-SRYCD-9                         PIC 9(09).
           03  WRK-SRYSYUKBN                       PIC X(03).
           03  WRK-TANINAME                        PIC X(24).
           03  WRK-JBI-SONOTA-G.
               05  WRK-JBI-SONOTA-KBN              PIC X(01).
               05  WRK-JBI-SONOTA-TEN              PIC 9(08).
               05  WRK-JBI-SONOTA-SRYCD            PIC X(09).
           03  WRK-RSI-SONOTA-G.
               05  WRK-RSI-SONOTA-KBN              PIC X(01).
               05  WRK-RSI-SONOTA-TEN              PIC 9(08).
           03  WRK-JBI-RYOYO-TEATE-KBN             PIC X(01).
           03  WRK-RUIKEITOTAL                     PIC 9(15)V9(03).
      *
       01  COMMENT-CHK-AREA.
           03  COMMENT-IDX                         PIC 9(03).
           03  COMMENT-OCC                         OCCURS 2.
               05  COMMENT-ZAINUM                  PIC 9(08).
               05  COMMENT-MAX                     PIC 9(03).
               05  COMMENT-COMMENT-G.
                   07  COMMENT-COMMENT             PIC X(80)
                                                   OCCURS  10.
      *
       01  WRK-SRYKA-TBL.
           03  WRK-SRYKA-OCC                       OCCURS  20.
               05  WRK-SRYKA                       PIC X(02).
               05  WRK-SRYKA-SRYKAMEI1             PIC X(20).
               05  WRK-SRYKA-SRYKAMEI2             PIC X(12).
               05  WRK-SRYKA-SRYKAMEI3             PIC X(04).
      *
       01  WRK-SRYACT-AREA.
           03  WRK-SRYACT-STR                      PIC X(520).
           03  WRK-SRYACT-PNT                      PIC 9(05).
           03  WRK-SRYACT-MAX                      PIC 9(05).
           03  WRK-SRYACT-TBL.
               05  WRK-SRYACT-OCC                  OCCURS  500.
                   07  WRK-SRYACT                  PIC X(52).
           03  WRK-SRYACT-SRYKBN                   PIC X(02).
           03  WRK-SRYACT-MARK                     PIC X(04).
           03  WRK-SRYACT-HKNCOMBI                 PIC X(04).
           03  WRK-SRYACT-DAY-OCC                  OCCURS  31.
               05  WRK-SRYACT-DAY-X                PIC X(03).
               05  WRK-SRYACT-DAY-Z    REDEFINES   WRK-SRYACT-DAY-X
                                                   PIC Z(03).
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
           03  SRH-SRYKA-DAT.
               05  SRH-SRYKA                       PIC X(02).
               05  SRH-SRYKAMEI1                   PIC X(20).
               05  SRH-SRYKAMEI2                   PIC X(12).
               05  SRH-SRYKAMEI3                   PIC X(04).
               05  SRH-SRYKA-MAX                   PIC 9(05).
               05  SRH-SRYKA-OCC                   OCCURS 100.
                   07  SRH-SRYKAL                  PIC X(02).
                   07  SRH-SRYKAMEI1L              PIC X(20).
                   07  SRH-SRYKAMEI2L              PIC X(12).
                   07  SRH-SRYKAMEI3L              PIC X(04).
      *
       01  SRT-AREA.
           03  SRT-IDX1                            PIC 9(05).
           03  SRT-IDX2                            PIC 9(05).
           03  SRT-STIDX                           PIC 9(05).
           03  SRT-SRYACCT.
               05  SRT-SRYACCT-OCC                 OCCURS  1001.
                   07  SRT-SRYACCT-IDX             PIC 9(05).
                   07  SRT-SRYACCT-HOUKNSKBN       PIC 9(02).
                   07  SRT-SRYACCT-KEY.
                       09 SRT-SRYACCT-HKNCOMBI     PIC X(04).
                       09 SRT-SRYACCT-SRYKBN       PIC X(02).
                       09 SRT-SRYACCT-PRYORITY     PIC 9(01).
                       09 SRT-SRYACCT-YKZHKTKBN    PIC 9(01).
                       09 SRT-SRYACCT-YKZINGAIKBN  PIC 9(01).
                       09 SRT-SRYACCT-KNSJISKBN    PIC 9(01).
                       09 SRT-SRYACCT-CDKBN-SYO-G.
                           11 SRT-SRYACCT-CDKBN-SYO
                                                   PIC 9(01).
                       09 SRT-SRYACCT-CDKBN-BU-G.
                           11 SRT-SRYACCT-CDKBN-BU PIC 9(02).
                       09 SRT-SRYACCT-CDKBN-KBN-G.
                           11 SRT-SRYACCT-CDKBN-KBN
                                                   PIC X(01).
                       09 SRT-SRYACCT-CDKBN-KBNNUM-G.
                           11 SRT-SRYACCT-CDKBN-KBNNUM
                                                   PIC 9(03).
                       09 SRT-SRYACCT-CDKBN-KBNNUM-EDA-G.
                           11 SRT-SRYACCT-CDKBN-KBNNUM-EDA
                                                   PIC 9(02).
                       09 SRT-SRYACCT-CDKBN-KOUBAN-G.
                           11 SRT-SRYACCT-CDKBN-KOUBAN
                                                   PIC 9(02).
                       09 SRT-SRYACCT-KOUHYOJYUNNUM-G.
                          11 SRT-SRYACCT-KOUHYOJYUNNUM
                                                   PIC 9(09).
                       09 SRT-SRYACCT-SRYSYUKBN    PIC X(03).
                       09 SRT-SRYACCT-ZAITEN       PIC 9(08).
                       09 SRT-SRYACCT-ZAINUM       PIC 9(08).
      *
       01  RSIJBI-AREA.
           03  RSIJBI-STIDX                        PIC 9(05).
           03  RSIJBI-IDX1                         PIC 9(05).
           03  RSIJBI-IDX2                         PIC 9(05).
           03  RSIJBI-MAX                          PIC 9(05).
           03  RSIJBI-TEN-MAX                      PIC S9(05).
           03  RSIJBI-TEN-COM-MAX                  PIC S9(05).
           03  RSIJBI-KIN-MAX                      PIC S9(05).
           03  RSIJBI-KIN-COM-MAX                  PIC S9(05).
           03  RSIJBI-TEN                          PIC 9(08).
           03  RSIJBI-KINGAKU                      PIC 9(08).
           03  RSIJBI-ZAIKAISU                     PIC 9(07).
           03  RSIJBI-OCC                          OCCURS  21.
               05  RSIJBI-KEY.
                   07  RSIJBI-TEN-FLG              PIC 9(1).
                   07  RSIJBI-GRP-CNT              PIC 9(3).
               05  RSIJBI-SRYCD                    PIC X(09).
               05  RSIJBI-INPUTNUM                 PIC 9(3).
               05  RSIJBI-NAME                     PIC X(200).
      *
       01  CONST-AREA.
      *    労災自賠編集領域最大行
           03  CONST-RSIJBI-MAX        PIC 9(05)   VALUE 20.
      *    会計カード診療行為最大行
           03  CONST-LINE-MAX          PIC 9(05)   VALUE 28.
      *    会計カード診療行為最大行（続紙）
           03  CONST-ZKSI-LINE-MAX     PIC 9(05)   VALUE 39.
      *    保険組合せ最大行
           03  CONST-HKNCOMBI-LINE-MAX PIC 9(05)   VALUE 6.
      *    03  CONST-HKNCOMBI-LINE-MAX PIC 9(05)   VALUE 0.
      *    保険／公費最大行
           03  CONST-HKNKOH-LINE-MAX   PIC 9(05)   VALUE 6.
      *    03  CONST-HKNKOH-LINE-MAX   PIC 9(05)   VALUE 0.
      *    傷病名最大行
           03  CONST-BYOMEI-LINE-MAX   PIC 9(05)   VALUE 10.
      *    03  CONST-BYOMEI-LINE-MAX   PIC 9(05)   VALUE 0.
      *    診療行為桁数
           03  CONST-SRYACT-LEN        PIC 9(05)   VALUE 52.
      *
           03  CONST-KENSACOMMENT-VAL.
                05  FILLER                 PIC X(50)   VALUE
                "血液化学検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "内分泌学的検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "肝炎ウイルス関連検査".
                05  FILLER                 PIC X(50)   VALUE
                "腫瘍マーカーのうちα−フェトプロテイン等".
                05  FILLER                 PIC X(50)   VALUE
                "腫瘍マーカー".                        
                05  FILLER                 PIC X(50)   VALUE
                "出血・凝固検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "自己抗体検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "内分泌負荷試験".
            03 CONST-KENSACOMMENT-R        REDEFINES
                                           CONST-KENSACOMMENT-VAL.
                05  CONST-KENSACOMMENT     PIC X(50)   OCCURS  8.
      *    薬剤料逓減
           03  CONST-TEIGEN                PIC X(09)   VALUE
               "630010002".
      *    未実施減算
           03  CONST-GENZAN1               PIC X(09)   VALUE
               "190080470".
           03  CONST-GENZAN2               PIC X(09)   VALUE
               "190799670".
           03  CONST-GENZAN3               PIC X(09)   VALUE
               "190080570".
           03  CONST-GENZAN4               PIC X(09)   VALUE
               "190799770".
           03  CONST-GENZAN5               PIC X(09)   VALUE
               "190111690".
           03  CONST-GENZAN6               PIC X(09)   VALUE
               "190809090".
           03  CONST-GENZAN7               PIC X(09)   VALUE
               "190111790".
           03  CONST-GENZAN8               PIC X(09)   VALUE
               "190809190".
           03  CONST-SRYKAMEI-LEN          PIC 9(05)   VALUE   32.
           03  CONST-GAZOU-TANICD          PIC X(03)   VALUE   "006".
           03  CONST-SRYACCT-MAX           PIC 9(05)   VALUE   1000.
           03  CONST-RYOYO-TEATE-GAI       PIC X(09)   VALUE
               "199000610".
           03  CONST-RYOYO-TEATE-NYU       PIC X(09)   VALUE
               "199000510".
      *
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"
                   REPLACING   //SPA-//    BY //WRK-SPA-//.
      *
      *    保険組み合わせワーク
       01  WKHKNCOMBI-AREA.
           02  WKHKNCOMBI-MAX          PIC 9(05).
           02  WKHKNCOMBI-OCC          OCCURS 50.
               03  WKHKNCOMBI-HKNCOMBINUM
                                       PIC  X(04).
               03  WKHKNCOMBI-HKNCOMBIMEI
                                       PIC  X(50).
               03  WKHKNCOMBI-TEKSTYMDS
                                       PIC  X(08).
               03  WKHKNCOMBI-TEKEDYMDS
                                       PIC  X(08).
               03  WKHKNCOMBI-TEKSTYMDW
                                       PIC  X(09).
               03  WKHKNCOMBI-TEKEDYMDW
                                       PIC  X(09).
      *
      *    保険マスタワーク
       01  WKPTHKNINF-REC.
           02  WKPTHKNINF-MAX          PIC 9(05).
           02  WKPTHKNINF-OCC          OCCURS 50.
           COPY    "CPPTHKNINF.INC"
                   REPLACING   //PTHKN-//  BY  //WKPTHKN-//.
      *
      *    公費マスタワーク
       01  WKPTKOHINF-REC.
           02  WKPTKOHINF-MAX          PIC 9(05).
           02  WKPTKOHINF-OCC          OCCURS 50.
           COPY    "CPPTKOHINF.INC"
                   REPLACING   //PTKOH-//  BY  //WKPTKOH-//.
      *
      *    患者病名マスタ−ワーク
       01  WKPTBYOMEI-REC.
           02  WKPTBYOMEI-MAX          PIC 9(05).
           02  WKPTBYOMEI-OCC          OCCURS 300.
           COPY    "CPPTBYOMEI.INC"
                   REPLACING   //PTBYO-//  BY //WKPTBYO-//.
      *
      *    診療会計ワーク
       01  WKSRYACCT-REC.
           02  WKSRYACCT-MAX           PIC 9(05).
           02  WKSRYACCT-OCC           OCCURS 1000
                                       INDEXED WKSRYACCT-IDX.
           COPY    "CPSRYACCT.INC"
                   REPLACING   //ACCT-//  BY //WKACCT-//.
               03 WKACCT-RUIKEITOTAL      PIC 9(15)V9(03).
               03 WKACCT-SRT-SRYCD        PIC X(09).
               03 WKACCT-SRT-SRYSYUKBN    PIC X(03).
               03 WKACCT-SRT-SRYSTATUS    PIC 9(01).
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者病名マスタ
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    労災自賠責医療機関情報設定
           COPY    "CPSK4001.INC".
      *
           COPY    "CPSK4003.INC".
      *
      *    医療機関情報−入院基本
           COPY    "CPSK5000.INC".
      *
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
           COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHCM34.INC".
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION    USING
                                           SPA-AREA
                                           ORCHCM34AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
           .
      *
           EXIT PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SRH-AREA
           INITIALIZE                  TTNS-AREA
           INITIALIZE                  ORCHCM34-OT
      *
      *    医療機関情報取得処理
           PERFORM 1001-SYS1001-GET-SEC
      *
      *    医療機関情報−入院基本取得処理
           PERFORM 1001-SYS5000-GET-SEC
      *
      *    労災自賠責医療機関情報取得処理
           PERFORM 1001-SYS4001-GET-SEC
      *
      *    自賠責保険　特定器材等消費税対象外コード情報取得処理
           PERFORM 1001-SYS4003-GET-SEC
      *
      *    保険の種類・公費の種類編集処理
           PERFORM 1001-HKNKOH-HEN-SEC
      *    診療科編集処理
           PERFORM 1001-SRYKA-HEN-SEC
      *    基本情報編集
           PERFORM 1001-KHN-EDIT-SEC
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    ORCHCM34-NYUGAIKBN  TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
           INITIALIZE                  HCM341
           MOVE    ZERO                TO  FLG-HEAD
           MOVE    ZERO                TO  CNT-PAGE
           MOVE    ZERO                TO  CNT-SRYACT-LINE
           MOVE    ZERO                TO  CNT-YKZINGAI
           MOVE    ZERO                TO  CNT-YKZHKT
           MOVE    LOW-VALUE           TO  WRK-KEY-HKNCOMBI
           MOVE    ZERO                TO  WRK-TTL-JIHI
           MOVE    ZERO                TO  WRK-TTL-JIHI-TAX
           MOVE    ZERO                TO  WRK-TTL-BRMSAGAKU
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報取得処理
      *****************************************************************
       1001-SYS1001-GET-SEC                SECTION.
      *
      *    医療機関ＩＤ編集
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    ORCHCM34-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               MOVE    1               TO  ORCHCM34-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       1001-SYS1001-GET-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報−入院基本取得処理
      *****************************************************************
       1001-SYS5000-GET-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-5000-REC
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    ORCHCM34-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5000-REC
           ELSE
               INITIALIZE                  SYS-5000-REC
           END-IF
      *
           .
       1001-SYS5000-GET-EXT.
           EXIT.
      *****************************************************************
      *    労災医療機関情報
      *****************************************************************
       1001-SYS4001-GET-SEC                SECTION.
      *
      *    医療機関ＩＤ編集
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    ORCHCM34-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-4001-REC
           ELSE
               INITIALIZE                  SYS-4001-REC
           END-IF
      *
           .
       1001-SYS4001-GET-EXT.
           EXIT.
      *****************************************************************
      *    自賠責保険　特定器材等消費税対象外コード
      *****************************************************************
       1001-SYS4003-GET-SEC                SECTION.
      *
      *    医療機関ＩＤ編集
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-4003-REC
           MOVE    "4003"              TO  SYS-4003-KANRICD
           MOVE    "*"                 TO  SYS-4003-KBNCD
           MOVE    ORCHCM34-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-4003-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-4003-REC
           ELSE
               INITIALIZE                  SYS-4003-REC
           END-IF
      *
           .
       1001-SYS4003-GET-EXT.
           EXIT.
      *****************************************************************
      *    基本情報編集
      *****************************************************************
       1001-KHN-EDIT-SEC               SECTION.
      *
           INITIALIZE  WRK-HCM34-AREA
      *
      *    診療年月
           MOVE    ORCHCM34-SRYYM      TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3 (1:16)  TO  WRK-HCM34-SRYYM
      *
      *    作成日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-HCM34-SAKYMD
      *
      *    患者番号
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPID          TO  PTNUM-HOSPID
           MOVE    SPA-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               INITIALIZE                  PTNUM-REC
           ELSE
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               IF    ( PTNUM-PTNUM ( 11 : 1)   =   SPACE )
                   MOVE    PTNUM-PTNUM TO  WRK-HCM34-PTNUM
               ELSE
                   MOVE    PTNUM-PTNUM TO  WRK-HCM34-PTNUM2
               END-IF
           END-IF
      *
      *    患者情報取得
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               INITIALIZE                  PTINF-REC
           ELSE
               MOVE    MCPDATA-REC     TO  PTINF-REC
           END-IF
      *
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-HCM34-BIRTHDAY
      *
      *    保険年齢で７０歳を迎える日を求める
           MOVE    PTINF-BIRTHDAY      TO  WRK-BASEYMD
           IF    ( WRK-BDD         NOT =   01 )
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF    ( WRK-BMM         >   12 )
                   COMPUTE WRK-BYY     =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01                  TO  WRK-BDD
           END-IF
           MOVE    WRK-BASEYMD         TO  WRK-AGE70-YMD
           COMPUTE WRK-AGE70-YY        =   WRK-AGE70-YY    +   70
      *
      *    入退院歴取得
           IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
               INITIALIZE                  ORCSNTYMDAREA
               MOVE    PTINF-HOSPID        TO  SNTYMD-HOSPID
               MOVE    PTINF-PTID          TO  SNTYMD-PTID
               MOVE    "2"                 TO  SNTYMD-KBN
               CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
           END-IF
      *
           EVALUATE    ORCHCM34-NYUGAIKBN
           WHEN    "1"
               MOVE    "入院科"        TO  WRK-HCM34-LBLSRYKA
               MOVE    "入院日"        TO  WRK-HCM34-LBLNYUINYMD
               MOVE    "退院日"        TO  WRK-HCM34-LBLTAIINYMD
      *        入院履歴より、入院科・入院日・退院日・病棟名を取得
               PERFORM 10011-PTNYUINRRK-HEN-SEC
           WHEN    "2"
               MOVE    "診療科"        TO  WRK-HCM34-LBLSRYKA
               MOVE    SPACE           TO  WRK-HCM34-LBLNYUINYMD
               MOVE    SPACE           TO  WRK-HCM34-LBLTAIINYMD
      *        受診履歴より、診療科を取得
               PERFORM 10011-SRYKA-HEN-SEC
               MOVE    SPACE           TO  WRK-HCM34-NYUINYMD
               MOVE    SPACE           TO  WRK-HCM34-TAIINYMD
               MOVE    SPACE           TO  WRK-HCM34-BTUNAME
           END-EVALUATE
      *
           .
       1001-KHN-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報編集処理
      *****************************************************************
       10011-PTNYUINRRK-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-HCM34-SRYKA-G
                                           WRK-HCM34-NYUINYMD
                                           WRK-HCM34-TAIINYMD
                                           WRK-HCM34-BTUNAME
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    ORCHCM34-SRYYM      TO  WRK-SYMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD            TO  PTNYUINRRK-NYUINYMD
           MOVE    ORCHCM34-SRYYM      TO  PTNYUINRRK-TAIINYMD
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY11"  TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "PTNYUINRRK-KEY11"  TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               GO  TO  10011-PTNYUINRRK-HEN-EXT
           END-IF
      *
           MOVE    PTNYUINRRK-NYUINKA  TO  SRH-SRYKA
           PERFORM 200111-SRYKA-HEN-SEC
           MOVE    SRH-SRYKAMEI1       TO  WRK-HCM34-SRYKA(2)
      *
           PERFORM VARYING IDX22   FROM    1   BY  1
                   UNTIL ( IDX22       >   SNTYMD-MAX )
               IF    ( PTNYUINRRK-RRKNUM   = SNTYMD-RRKNUM (IDX22))
      *
                   MOVE    SNTYMD-NYUINYMD (IDX22)
                                               TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD3         TO  WRK-HCM34-NYUINYMD
      *
                   IF    ( SNTYMD-TAIINYMD (IDX22) NOT =   "99999999" )
                    AND  ( SNTYMD-TAIINYMD (IDX22) NOT =   SPACE  )
                       MOVE    SNTYMD-TAIINYMD (IDX22) TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD3     TO  WRK-HCM34-TAIINYMD
                   END-IF
      *
                   MOVE    SNTYMD-MAX      TO  IDX22
      *
               END-IF
      *
           END-PERFORM
      *
      *    病棟名称
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  ORC-DBYMD
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-5001-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           STRING  "病棟名："          DELIMITED   BY  SIZE
                   SYS-5001-BTU-NAME   DELIMITED   BY  SPACE
           INTO    WRK-HCM34-BTUNAME
           END-STRING
      *
           .
       10011-PTNYUINRRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       10011-SRYKA-HEN-SEC             SECTION.
      *
           INITIALIZE                      WRK-SRYKA-TBL
           MOVE    ZERO                TO  WRK-HCM34-SRYKA-MAX
           MOVE    SPACE               TO  WRK-HCM34-SRYKA-G
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    ORCHCM34-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    ORCHCM34-SRYYM      TO  ACCT-SRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "SRYACCT-KEY23"     TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACCT
           ELSE
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           END-IF
      *
           MOVE    ZERO                TO  WKSRYACCT-MAX
           INITIALIZE                      WKSRYACCT-REC
           PERFORM UNTIL     ( FLG-SRYACCT =   1 )
      *
               PERFORM VARYING IDX13   FROM    1   BY  1
                       UNTIL ( IDX13       >   WKSRYACCT-MAX )
                        OR   ( IDX13       >   20 )
                        OR   ( ACCT-SRYKA  =   WRK-SRYKA (IDX13) )
                   CONTINUE
               END-PERFORM
      *
               IF    ( IDX13               >   WKSRYACCT-MAX )
                   COMPUTE WKSRYACCT-MAX   =   WKSRYACCT-MAX   +   1
                   IF    ( WKSRYACCT-MAX   >   20 )
                       MOVE    20          TO  WKSRYACCT-MAX
                   ELSE
                       MOVE    ACCT-SRYKA  TO  WRK-SRYKA (WKSRYACCT-MAX)
                       MOVE    ACCT-SRYKA  TO  SRH-SRYKA
                       PERFORM 200111-SRYKA-HEN-SEC
                       MOVE    SRH-SRYKAMEI1
                                           TO  WRK-SRYKA-SRYKAMEI1
                                                         (WKSRYACCT-MAX)
                       MOVE    SRH-SRYKAMEI2
                                           TO  WRK-SRYKA-SRYKAMEI2
                                                         (WKSRYACCT-MAX)
                       MOVE    SRH-SRYKAMEI3
                                           TO  WRK-SRYKA-SRYKAMEI3
                                                         (WKSRYACCT-MAX)
                   END-IF
               END-IF
      *
               MOVE    "SRYACCT-KEY23"     TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1               TO  FLG-SRYACCT
               ELSE
                   MOVE    MCPDATA-REC     TO  SRYACCT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SRYACCT-KEY23"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           MOVE    1                   TO  WRK-PNT
           MOVE    ZERO                TO  FLG-LOOPEND
           MOVE    ALL "　"            TO  WRK-HCM34-SRYKA-G
           PERFORM VARYING IDX13   FROM    1   BY  1
                   UNTIL ( FLG-LOOPEND NOT =   ZERO )
                    OR   ( IDX13   >   WKSRYACCT-MAX )
               MOVE    WRK-SRYKA-SRYKAMEI2 (IDX13) TO  WRK-STR
               PERFORM 800-STRLEN-CHK-SEC
               IF    ( WRK-STRLEN  +   WRK-PNT - 1
                                   <   CONST-SRYKAMEI-LEN - 2 )
                OR   ( ( WRK-STRLEN  +   WRK-PNT - 1
                                   <=  CONST-SRYKAMEI-LEN - 2 )
                   AND ( IDX13     >=    WKSRYACCT-MAX ) )
                   MOVE    "／"        TO  WRK-KUGIRI
                   IF    ( IDX13       =   1 )
                       MOVE    SPACE   TO  WRK-KUGIRI
                   END-IF
                   STRING  WRK-KUGIRI      DELIMITED   BY  SPACE
                           WRK-SRYKA-SRYKAMEI2 (IDX13)
                                           DELIMITED   BY  SPACE
                   INTO    WRK-HCM34-SRYKA-G
                   WITH    POINTER     WRK-PNT
                   END-STRING
               ELSE
                   MOVE    1           TO  FLG-LOOPEND
               END-IF
           END-PERFORM
      *
           IF    ( FLG-LOOPEND     NOT =   ZERO )
               COMPUTE WRK-PNT     =    CONST-SRYKAMEI-LEN - 1
               MOVE    "他"            TO   WRK-HCM34-SRYKA-G
                                                   (WRK-PNT:2)
           ELSE
               IF    ( WRK-HCM34-SRYKA (2) =    SPACE )
                   MOVE    WRK-HCM34-SRYKA (1)
                                       TO  WRK-HCM34-SRYKA (2)
                   MOVE    SPACE       TO  WRK-HCM34-SRYKA (1)
               END-IF
           END-IF
      *
           .
       10011-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類・公費の種類編集処理
      *****************************************************************
       1001-HKNKOH-HEN-SEC         SECTION.
      *
      *
           INITIALIZE              SRH-HKNNUM-DAT
           INITIALIZE              SRH-KOHNUM-DAT
      *
      *    保険番号検索
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPID          TO  HKN-HOSPID
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    "HKNNUM-KEY3"       TO  ORC-DBPATH
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC           NOT =   ZERO )
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
               IF ( MCP-RC           NOT =   ZERO )
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
       3101-HKNKOH-CMB-EDIT-EXT.
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
           INITIALIZE              SRH-SRYKA-DAT
      *
      *    システム管理マスタ検索処理
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    ORCHCM34-SRYYM      TO  ORC-DBYMD
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
               UNTIL     ( IDX5            >   100 )
                   OR    ( MCP-RC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
      *
               MOVE    IDX5            TO  SRH-SRYKA-MAX
               MOVE    SYS-1005-KBNCD (1 : 2)
                                       TO  SRH-SRYKAL (IDX5)
               MOVE    SYS-1005-SRYKANAME1
                                       TO  SRH-SRYKAMEI1L (IDX5)
               MOVE    SYS-1005-SRYKANAME2
                                       TO  SRH-SRYKAMEI2L (IDX5)
               MOVE    SYS-1005-SRYKANAME3
                                       TO  SRH-SRYKAMEI3L (IDX5)
      *
      *        システム管理マスタ読込処理
               MOVE    "SYSKANRI-KEY2" TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
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
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    保険組合せ編集処理
           PERFORM 2001-HKNCOMBI-HEN-SEC
      *    保険編集処理
           PERFORM 2001-PTHKN-HEN-SEC
      *    公費編集処理
           PERFORM 2001-PTKOH-HEN-SEC
      *    傷病名編集処理
           PERFORM 2001-PTBYOMEI-HEN-SEC
      *    診療行為編集処理
           PERFORM 2001-SRYACT-HEN-SEC
      *
           IF    ( CNT-PAGE        =   ZERO )
            OR   (   ( CNT-PAGE        >   ZERO )
                AND  ( CNT-SRYACT-LINE >   ZERO ))
               PERFORM 290-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       2001-HKNCOMBI-HEN-SEC           SECTION.
      *
           INITIALIZE                  WKHKNCOMBI-AREA
      *
           INITIALIZE                  ORCSHKNSETAREA
           INITIALIZE                  WRK-SPA-AREA
      *
           MOVE    PTINF-HOSPID    TO  WRK-SPA-HOSPID
           MOVE    PTINF-PTID      TO  WRK-SPA-PTID
           MOVE    PTINF-BIRTHDAY  TO  WRK-SPA-BIRTHDAY
           MOVE    ORCHCM34-SRYYM  TO  WRK-SPA-SRYYMD (1:6)
           MOVE    "01"            TO  WRK-SPA-SRYYMD (7:2)
      *
           MOVE    ORCHCM34-NYUGAIKBN
                                   TO  WRK-SPA-NYUGAIKBN
           MOVE    "2"             TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"    USING   WRK-SPA-AREA
                                           ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           MOVE    ZERO            TO  CNT-HKNCOMBI-LINE
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   ORCSHKN-HKN-MAX )
      *
               IF    ( ORCHCM34-SRYYM
                                   >=  ORCSHKN-NAI-TEKSTYMD(IDX1)(1:6))
                AND  ( ORCHCM34-SRYYM
                                   <=  ORCSHKN-NAI-TEKEDYMD(IDX1)(1:6))
                   IF    ( CNT-HKNCOMBI-LINE
                                       <    CONST-HKNCOMBI-LINE-MAX )
      *
                       PERFORM 20011-HKNCOMBI-HEN-SEC
                   ELSE
      *
      *                保険組合せ欄に収まりきらない場合、
      *                ワークエリアに退避する
                       IF    ( WKHKNCOMBI-MAX  <   50 )
                           COMPUTE WKHKNCOMBI-MAX  =   WKHKNCOMBI-MAX
                                                   +   1
      *
                           MOVE    WKHKNCOMBI-MAX  TO  IDXA
      *
                           MOVE    ORCSHKN-GMN-HKNCOMBINUM(IDX1)
                                       TO  WKHKNCOMBI-HKNCOMBINUM(IDXA)
                           MOVE    ORCSHKN-GMN-HKNCOMBIMEI(IDX1)
                                       TO  WRK-MAE-INPUT
                           PERFORM 800-ZENKAKU-HEN-SEC
                           MOVE    WRK-OUT-INPUT
                                       TO  WKHKNCOMBI-HKNCOMBIMEI(IDXA)
                           MOVE    ORCSHKN-NAI-TEKSTYMD(IDX1)
                                       TO  WKHKNCOMBI-TEKSTYMDS (IDXA)
                           MOVE    ORCSHKN-NAI-TEKEDYMD(IDX1)
                                       TO  WKHKNCOMBI-TEKEDYMDS (IDXA)
                           MOVE    ORCSHKN-GMN-TEKSTYMD(IDX1)
                                       TO  WKHKNCOMBI-TEKSTYMDW (IDXA)
                           MOVE    ORCSHKN-GMN-TEKEDYMD(IDX1)
                                       TO  WKHKNCOMBI-TEKEDYMDW (IDXA)
      *
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    保険組み合わせ包括分編集処理
           PERFORM 20011-HOUKATU-HEN-SEC
      *
           .
       2001-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20011-HKNCOMBI-HEN-SEC           SECTION.
      *
           COMPUTE CNT-HKNCOMBI-LINE   =   CNT-HKNCOMBI-LINE   +   1
      *
           MOVE    CNT-HKNCOMBI-LINE   TO  IDXA
      *
           MOVE    ORCSHKN-GMN-HKNCOMBINUM(IDX1)
                                       TO  HCM341-HKNCOMBINUM(IDXA)
           MOVE    ORCSHKN-GMN-HKNCOMBIMEI(IDX1)
                                       TO  HCM341-HKNCOMBIMEI(IDXA)
      *
      *    半角スペースが途中に含まれていた場合の対応
      *    （末尾の全角文字が尻切れにならないようにする）
           MOVE    ZERO                TO  CNT-SPACE
           INSPECT HCM341-HKNCOMBIMEI(IDXA)    TALLYING
                   CNT-SPACE   FOR ALL " "
      *
           DIVIDE  CNT-SPACE   BY  2 GIVING WRK-SHO
           REMAINDER WRK-AMARI
      *
           IF    ( WRK-AMARI   NOT =   ZERO )
               MOVE    1               TO  WRK-STRLEN-MAX
               MOVE    SPACE           TO  WRK-STR
               STRING  HCM341-HKNCOMBIMEI(IDXA)
               DELIMITED   BY  SIZE
               INTO    WRK-STR
               WITH    POINTER WRK-STRLEN-MAX
               END-STRING
               COMPUTE WRK-STRLEN-MAX  =   WRK-STRLEN-MAX
                                       -   1
               MOVE    WRK-STR (1:WRK-STRLEN-MAX - 1)
                                       TO  HCM341-HKNCOMBIMEI(IDXA)
           END-IF
      *
           MOVE    ORCSHKN-GMN-TEKSTYMD(IDX1)
                                       TO  HCM341-TEKSTYMD (IDXA)
           MOVE    ORCSHKN-GMN-TEKEDYMD(IDX1)
                                       TO  HCM341-TEKEDYMD (IDXA)
      *
           .
       20011-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせ包括分編集処理
      *****************************************************************
       20011-HOUKATU-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-HOUKATU
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    ORCHCM34-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    ORCHCM34-SRYYM      TO  ACCT-SRYYM
           MOVE    9999                TO  ACCT-HKNCOMBI
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "SRYACCT-KEY22"     TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACCT
           ELSE
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT = ZERO )
                    OR   ( FLG-HOUKATU NOT = ZERO )
      *
               IF    ( ACCT-ZAITEN     NOT =   ZERO )
                AND  ( ACCT-ZAIKAISU   NOT =   ZERO )
                    MOVE   1           TO  FLG-HOUKATU
               END-IF
      *
               MOVE    "SRYACCT-KEY22" TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1           TO  FLG-SRYACCT
               ELSE
                   MOVE    MCPDATA-REC TO  SRYACCT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SRYACCT-KEY22"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-HOUKATU         =   1 )
               IF    ( CNT-HKNCOMBI-LINE
                                       <    CONST-HKNCOMBI-LINE-MAX )
      *
                   COMPUTE CNT-HKNCOMBI-LINE
                       =   CNT-HKNCOMBI-LINE   +   1
      *
                   MOVE    CNT-HKNCOMBI-LINE
                                       TO  IDXA
                   MOVE    "9999"      TO  HCM341-HKNCOMBINUM(IDXA)
                   MOVE    "包括分"    TO  HCM341-HKNCOMBIMEI(IDXA)
                   MOVE    " 00.00.00" TO  HCM341-TEKSTYMD (IDXA)
                   MOVE    " 99.99.99" TO  HCM341-TEKEDYMD (IDXA)
               ELSE
      *
      *            保険組合せ欄に収まりきらない場合、
      *            ワークエリアに退避する
                   IF    ( WKHKNCOMBI-MAX  <   50 )
                       COMPUTE WKHKNCOMBI-MAX  =   WKHKNCOMBI-MAX
                                               +   1
      *
                       MOVE    WKHKNCOMBI-MAX  TO  IDXA
      *
                       MOVE    "9999"
                                       TO  WKHKNCOMBI-HKNCOMBINUM(IDXA)
                       MOVE    "包括分"
                                       TO  WKHKNCOMBI-HKNCOMBIMEI(IDXA)
                       MOVE    "00000000"
                                       TO  WKHKNCOMBI-TEKSTYMDS (IDXA)
                       MOVE    "99999999"
                                       TO  WKHKNCOMBI-TEKEDYMDS (IDXA)
                       MOVE    " 00.00.00"
                                       TO  WKHKNCOMBI-TEKSTYMDW (IDXA)
                       MOVE    " 99.99.99"
                                       TO  WKHKNCOMBI-TEKEDYMDW (IDXA)
      *
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-HOUKATU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険編集処理
      *****************************************************************
       2001-PTHKN-HEN-SEC              SECTION.
      *
           INITIALIZE  WKPTHKNINF-REC
      *
      *    患者保険マスタ検索
           MOVE    ZERO                TO  FLG-PTHKNINF
           INITIALIZE                  PTHKNINF-REC
           MOVE    PTINF-HOSPID        TO  PTHKN-HOSPID
           MOVE    PTINF-PTID          TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "PTHKNINF-KEY6"     TO  ORC-DBPATH
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC           NOT =   ZERO )
               MOVE    1               TO  FLG-PTHKNINF
           ELSE
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           END-IF
      *
           MOVE    ZERO            TO  CNT-HKNKOH-LINE
           PERFORM UNTIL ( FLG-PTHKNINF    NOT =   ZERO )
      *
               IF    ( PTHKN-SAKJOKBN   =  SPACE )
                AND  ( ORCHCM34-SRYYM  >=  PTHKN-TEKSTYMD (1:6))
                AND  ( ORCHCM34-SRYYM  <=  PTHKN-TEKEDYMD (1:6))
                   IF    ( CNT-HKNKOH-LINE
                                       <    CONST-HKNKOH-LINE-MAX )
                       PERFORM 20011-PTHKN-HEN-SEC
                   ELSE
                       IF    ( WKPTHKNINF-MAX  <   50 )
                           COMPUTE WKPTHKNINF-MAX  =   WKPTHKNINF-MAX
                                                   +   1
                           MOVE    WKPTHKNINF-MAX  TO  IDXA
                           MOVE    PTHKNINF-REC    TO  WKPTHKNINF-OCC
                                                                (IDXA)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "PTHKNINF-KEY6"     TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC           NOT =   ZERO )
                   MOVE    1           TO  FLG-PTHKNINF
               ELSE
                   MOVE    MCPDATA-REC TO  PTHKNINF-REC
               END-IF
           END-PERFORM
      *
           MOVE    "PTHKNINF-KEY6"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
      *
       2001-PTHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険編集処理
      *****************************************************************
       20011-PTHKN-HEN-SEC             SECTION.
      *
           COMPUTE CNT-HKNKOH-LINE =   CNT-HKNKOH-LINE     +   1
      *
           MOVE    CNT-HKNKOH-LINE     TO  IDXA
      *
           MOVE    PTHKN-HKNNUM        TO  SRH-HKNNUM
           PERFORM 200111-PTHKN-HKNNUM-HEN-SEC
           MOVE    SRH-HKNNUMMEI       TO  HCM341-HKNNAME (IDXA)
      *
           MOVE    PTHKN-HKNJANUM      TO  HCM341-HKNJANUM (IDXA)
      *
           MOVE    PTHKN-HONKZKKBN     TO  WRK-PTHKN-HONKZKKBN
           PERFORM 200111-PTHKN-HONKZKKBN-HEN-SEC
           MOVE    WRK-PTHKN-HONKZKKBNMEI
                                       TO  HCM341-HONKZK (IDXA)
      *
           MOVE    PTHKN-HKNNUM        TO  WRK-PTHKN-HKNNUM
           MOVE    PTHKN-HOJOKBN       TO  WRK-PTHKN-HOJOKBN
           PERFORM 200111-PTHKN-HOJOKBN-HEN-SEC
           MOVE    WRK-PTHKN-HOJOKBNMEI
                                       TO  HCM341-FTNRATE (IDXA)
      *
           MOVE    PTHKN-HKNNUM        TO  WRK-PTHKN-HKNNUM
           MOVE    PTHKN-CONTKBN       TO  WRK-PTHKN-CONTKBN
           PERFORM 200111-PTHKN-CONTKBN-HEN-SEC
           MOVE    WRK-PTHKN-CONTKBNMEI
                                       TO  HCM341-CONTKBN (IDXA)
      *
           MOVE    PTHKN-KIGO          TO  HCM341-KIGO  (IDXA)
           MOVE    PTHKN-NUM           TO  HCM341-NUM   (IDXA)
      *
           .
      *
       20011-PTHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類編集処理
      *****************************************************************
       200111-PTHKN-HKNNUM-HEN-SEC     SECTION.
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
           .
       200111-PTHKN-HKNNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    本人家族区分編集処理
      *****************************************************************
       200111-PTHKN-HONKZKKBN-HEN-SEC  SECTION.
      *
           MOVE    SPACE               TO  WRK-PTHKN-HONKZKKBNMEI
      *
           EVALUATE    WRK-PTHKN-HONKZKKBN
           WHEN    "1"
               MOVE    "本人"          TO  WRK-PTHKN-HONKZKKBNMEI
           WHEN    "2"
               MOVE    "家族"          TO  WRK-PTHKN-HONKZKKBNMEI
           END-EVALUATE
      *
           .
       200111-PTHKN-HONKZKKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    補助区分編集処理
      *****************************************************************
       200111-PTHKN-HOJOKBN-HEN-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-PTHKN-HOJOKBNMEI
      *
      *    負担割合
           EVALUATE    WRK-PTHKN-HKNNUM
      *    船員
           WHEN    "002"
               IF    ( PTHKN-TEKSTYMD  >=  "20021001" )
                AND  ( WRK-AGE70-YMD   >   ZERO       )
                AND  ( PTHKN-TEKSTYMD  >=  WRK-AGE70-YMD )
                   EVALUATE    WRK-PTHKN-HOJOKBN
                   WHEN    "8"
                       MOVE    "２割"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "9"
                       MOVE    "１割"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "A" 
                       MOVE    "１割職務"  TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "B" 
                       MOVE    "１割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "C" 
                       MOVE    "１割通勤"  TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "D" 
                       MOVE    "２割職務"  TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "E" 
                       MOVE    "２割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "F" 
                       MOVE    "２割通勤"  TO  WRK-PTHKN-HOJOKBNMEI
                   END-EVALUATE
               ELSE
                   EVALUATE    WRK-PTHKN-HOJOKBN
                   WHEN    "1"
                       MOVE    "職務"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "2"
                       MOVE    "下船"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "3"
                       MOVE    "通勤"      TO  WRK-PTHKN-HOJOKBNMEI
                   END-EVALUATE
               END-IF
      *    国保
           WHEN    "060"
               EVALUATE    WRK-PTHKN-HOJOKBN
               WHEN    "0"
                   MOVE    "無し"          TO  WRK-PTHKN-HOJOKBNMEI
               WHEN    "1"
                   MOVE    "１割"          TO  WRK-PTHKN-HOJOKBNMEI
               WHEN    "2"
                   MOVE    "２割"          TO  WRK-PTHKN-HOJOKBNMEI
               WHEN    "3" 
                   MOVE    "３割"          TO  WRK-PTHKN-HOJOKBNMEI
               END-EVALUATE
      *    共済
           WHEN    "031"
           WHEN    "032"
           WHEN    "033"
           WHEN    "034"
               IF    ( PTHKN-TEKSTYMD  >=  "20021001" )
                AND  ( WRK-AGE70-YMD   >   ZERO       )
                AND  ( PTHKN-TEKSTYMD  >=  WRK-AGE70-YMD )
                   EVALUATE    WRK-PTHKN-HOJOKBN
                   WHEN    "8"
                       MOVE    "２割"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "9"
                       MOVE    "１割"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "B"
                       MOVE    "１割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "E"
                       MOVE    "２割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                   END-EVALUATE
               ELSE
                   EVALUATE    WRK-PTHKN-HOJOKBN
                   WHEN    "2"
                       MOVE    "下船３月"  TO  WRK-PTHKN-HOJOKBNMEI
                   END-EVALUATE
               END-IF
      *    その他
           WHEN    OTHER
      *        自費
               IF    ( PTHKN-HKNNUM (1:2) =   "98" )
                   EVALUATE    WRK-PTHKN-HOJOKBN
                   WHEN    "1"
                       MOVE    "課税"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "2"
                       MOVE    "非課"      TO  WRK-PTHKN-HOJOKBNMEI
                   END-EVALUATE
               ELSE
      *            Ｈ１４．１０ 改正より
                   IF    ( PTHKN-TEKSTYMD        >=    "20021001" )
                    AND  ( WRK-AGE70-YMD         >     ZERO       )
                    AND  ( PTHKN-TEKSTYMD        >=    WRK-AGE70-YMD)
                    AND  ( PTHKN-HKNNUM (1:1) NOT =    "9")
                       EVALUATE    WRK-PTHKN-HOJOKBN
                       WHEN    "8"
                           MOVE    "２割"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "9"
                           MOVE    "１割"  TO  WRK-PTHKN-HOJOKBNMEI
                       END-EVALUATE
                   END-IF
               END-IF
           END-EVALUATE
      *
           .
       200111-PTHKN-HOJOKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    継続区分編集処理
      *****************************************************************
       200111-PTHKN-CONTKBN-HEN-SEC    SECTION.
      *
           MOVE    SPACE                   TO  WRK-PTHKN-CONTKBNMEI
      *
           IF    ( WRK-PTHKN-HKNNUM    =   "003" )
               EVALUATE WRK-PTHKN-CONTKBN
               WHEN    "1"
                   MOVE    "継続"          TO  WRK-PTHKN-CONTKBNMEI
               WHEN    "2"
                   MOVE    "任継"          TO  WRK-PTHKN-CONTKBNMEI
               WHEN    "3"
                   MOVE    "特別"          TO  WRK-PTHKN-CONTKBNMEI
               END-EVALUATE
           ELSE
               EVALUATE WRK-PTHKN-CONTKBN
               WHEN    "1"
                   MOVE    "継続"          TO  WRK-PTHKN-CONTKBNMEI
               WHEN    "2"
                   MOVE    "任継"          TO  WRK-PTHKN-CONTKBNMEI
               END-EVALUATE
           END-IF
      *
           .
       200111-PTHKN-CONTKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       2001-PTKOH-HEN-SEC              SECTION.
      *
           INITIALIZE  WKPTKOHINF-REC
      *
      *    患者公費マスタ検索
           MOVE    ZERO                TO  FLG-PTKOHINF
           INITIALIZE                  PTKOHINF-REC
           MOVE    PTINF-HOSPID        TO  PTKOH-HOSPID
           MOVE    PTINF-PTID          TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "PTKOHINF-KEY2"     TO  ORC-DBPATH
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC           NOT =   ZERO )
               MOVE    1               TO  FLG-PTKOHINF
           ELSE
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTKOHINF    NOT =   ZERO )
      *
               IF    ( PTKOH-SAKJOKBN   =  SPACE )
                AND  ( ORCHCM34-SRYYM  >=  PTKOH-TEKSTYMD (1:6))
                AND  ( ORCHCM34-SRYYM  <=  PTKOH-TEKEDYMD (1:6))
                   IF    ( CNT-HKNKOH-LINE
                                       <    CONST-HKNKOH-LINE-MAX )
                       PERFORM 20011-PTKOH-HEN-SEC
                   ELSE
                       IF    ( WKPTKOHINF-MAX  <   50 )
                           COMPUTE WKPTKOHINF-MAX  =   WKPTKOHINF-MAX
                                                   +   1
                           MOVE    WKPTKOHINF-MAX  TO  IDXA
                           MOVE    PTKOHINF-REC    TO  WKPTKOHINF-OCC
                                                                (IDXA)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "PTKOHINF-KEY2"     TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC           NOT =   ZERO )
                   MOVE    1           TO  FLG-PTKOHINF
               ELSE
                   MOVE    MCPDATA-REC TO  PTKOHINF-REC
               END-IF
           END-PERFORM
      *
           MOVE    "PTKOHINF-KEY2"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       2001-PTKOH-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       20011-PTKOH-HEN-SEC             SECTION.
      *
           COMPUTE CNT-HKNKOH-LINE =   CNT-HKNKOH-LINE     +   1
      *
           MOVE    CNT-HKNKOH-LINE     TO  IDXA
      *
           MOVE    PTKOH-KOHNUM        TO  SRH-KOHNUM
           MOVE    PTKOH-PAYKBN        TO  SRH-PAYKBN
           PERFORM 200111-PTKOH-KOHNUM-HEN-SEC
           MOVE    SRH-KOHNUMMEI       TO  HCM341-HKNNAME (IDXA)
      *
      *
           MOVE    PTKOH-FTNJANUM          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  HCM341-KIGO  (IDXA)
      *
           MOVE    PTKOH-JKYSNUM           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  HCM341-NUM   (IDXA)
      *
           .
      *
       20011-PTKOH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の種類編集処理
      *****************************************************************
       200111-PTKOH-KOHNUM-HEN-SEC     SECTION.
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
       200111-PTKOH-KOHNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    傷病名編集処理
      *****************************************************************
       2001-PTBYOMEI-HEN-SEC           SECTION.
      *
           INITIALIZE  WKPTBYOMEI-REC
      *
      *    患者病名マスタ検索
           MOVE    ZERO                TO  FLG-PTBYOMEI
           INITIALIZE                  PTBYOMEI-REC
           MOVE    PTINF-HOSPID        TO  PTBYO-HOSPID
           MOVE    PTINF-PTID          TO  PTBYO-PTID
           MOVE    ORCHCM34-NYUGAIKBN  TO  PTBYO-NYUGAIKBN
      *
           MOVE    ORCHCM34-SRYYM      TO  PTBYO-TENKIYMD
           MOVE    "01"                TO  PTBYO-TENKIYMD (7:2)
      *
           MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
           MOVE    "2"                 TO  WRK-ZOGENPTN
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD            TO  PTBYO-SRYYMD
           MOVE    SPACE               TO  PTBYO-TENKIKBN
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "PTBYOMEI-KEY11"    TO  ORC-DBPATH
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC           NOT =   ZERO )
               MOVE    1               TO  FLG-PTBYOMEI
           ELSE
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
           END-IF
      *
      *
           MOVE    ZERO            TO  CNT-BYOMEI-LINE
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
      *
               IF    ( CNT-BYOMEI-LINE
                                       <    CONST-BYOMEI-LINE-MAX )
                   PERFORM 20011-PTBYOMEI-HEN-SEC
               ELSE
                   IF    ( WKPTBYOMEI-MAX  <   300 )
                       COMPUTE WKPTBYOMEI-MAX  =   WKPTBYOMEI-MAX
                                               +   1
                       MOVE    WKPTBYOMEI-MAX  TO  IDXA
                       MOVE    PTBYOMEI-REC    TO  WKPTBYOMEI-OCC
                                                            (IDXA)
                   END-IF
               END-IF
      *
               MOVE    "PTBYOMEI-KEY11"    TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC           NOT =   ZERO )
                   MOVE    1           TO  FLG-PTBYOMEI
               ELSE
                   MOVE    MCPDATA-REC TO  PTBYOMEI-REC
               END-IF
           END-PERFORM
      *
           MOVE    "PTBYOMEI-KEY11"    TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       2001-PTBYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    傷病名編集処理
      *****************************************************************
       20011-PTBYOMEI-HEN-SEC          SECTION.
      *
           COMPUTE CNT-BYOMEI-LINE =   CNT-BYOMEI-LINE     +   1
      *
           MOVE    CNT-BYOMEI-LINE         TO  IDXA
      *
           MOVE    PTBYO-BYOMEI            TO  WRK-PTBYO-BYOMEI
           MOVE    PTBYO-BYOMEIMOJI        TO  WRK-PTBYO-BYOMEIMOJI
           MOVE    PTBYO-UTAGAIFLG         TO  WRK-PTBYO-UTAGAIFLG
           PERFORM 20011-PTBYOMEI-BYOMEI-HEN-SEC
           MOVE    WRK-PTBYO-BYOMEI        TO  HCM341-BYOMEI (IDXA)
      *
           MOVE    PTBYO-SRYKA             TO  SRH-SRYKA
           PERFORM 200111-SRYKA-HEN-SEC
           MOVE    SRH-SRYKAMEI2(1:4)      TO  HCM341-BYOSRYKA (IDXA)
      *
           IF    ( PTBYO-SYUBYOFLG         =   "1" )
               MOVE    "主"                TO  HCM341-SYUBYO (IDXA)
           END-IF
      *
           MOVE    PTBYO-SRYYMD            TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1             TO  HCM341-SRYYMD (IDXA)
      *
           MOVE    PTBYO-TENKIKBN          TO  WRK-PTBYO-TENKIKBN
           PERFORM 200111-PTBYO-TENKIKBN-HEN-SEC
           MOVE    WRK-PTBYO-TENKIKBNMEI   TO  HCM341-TENKI  (IDXA)
      *
           .
      *
       20011-PTBYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       20011-PTBYOMEI-BYOMEI-HEN-SEC       SECTION.
      *
           MOVE       ZERO                 TO  FLG-UTAGAI
                                               FLG-KYUSEI
      *
           IF    ( WRK-PTBYO-UTAGAIFLG     =   "1" OR  "3" )
               COMPUTE CNT-BYOMEIMOJI  =   WRK-PTBYO-BYOMEIMOJI    *   2
               IF    ( CNT-BYOMEIMOJI  <=  8 )
                   MOVE    WRK-PTBYO-BYOMEI    TO  WRK-STR
               ELSE
                   MOVE    WRK-PTBYO-BYOMEI (CNT-BYOMEIMOJI - 7:)
                                           TO  WRK-STR
               END-IF
      *
               MOVE    ZERO                TO  CNT-STR
               PERFORM VARYING IDX0    FROM    1   BY  2
                       UNTIL ( IDX0    >   199 )
                        OR   ( WRK-STR (IDX0:) =   SPACE )
      *
                   IF    ( WRK-STR (IDX0:2)    =   "疑" )
                       COMPUTE CNT-STR =   CNT-STR     +   1
                   END-IF
      *
               END-PERFORM
               IF    ( CNT-STR         =   ZERO )
                   MOVE    1           TO  FLG-UTAGAI
               END-IF
           END-IF
      *
           IF    ( WRK-PTBYO-UTAGAIFLG     =   "2" OR  "3" )
               MOVE    ZERO                TO  CNT-STR
               INSPECT WRK-PTBYO-BYOMEI    TALLYING
                       CNT-STR FOR ALL "急性"
               IF    ( CNT-STR        <=   ZERO )
                   MOVE    1               TO  FLG-KYUSEI
               END-IF
           END-IF
      *
           IF    ( FLG-KYUSEI              =   1 )
               MOVE    SPACE               TO  WRK-STR
               STRING  "急性"              DELIMITED   BY  SIZE
                       WRK-PTBYO-BYOMEI    DELIMITED   BY  SPACE
               INTO    WRK-STR
               END-STRING
               MOVE    WRK-STR             TO  WRK-PTBYO-BYOMEI
           END-IF
      *
           IF    ( FLG-UTAGAI              =   1 )
               MOVE    SPACE               TO  WRK-STR
               STRING  WRK-PTBYO-BYOMEI    DELIMITED   BY  SPACE
                       "の疑い"            DELIMITED   BY  SIZE
               INTO    WRK-STR
               END-STRING
               MOVE    WRK-STR             TO  WRK-PTBYO-BYOMEI
           END-IF
      *
           MOVE    WRK-PTBYO-BYOMEI        TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-PTBYO-BYOMEI
      *
           .
       20011-PTBYOMEI-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       200111-SRYKA-HEN-SEC     SECTION.
      *
           MOVE    SPACE                   TO  SRH-SRYKAMEI1
                                               SRH-SRYKAMEI2
                                               SRH-SRYKAMEI3
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-SRYKA-MAX )
               IF    ( SRH-SRYKA   =   SRH-SRYKAL (SRH-IDX) )
                   MOVE    SRH-SRYKAMEI1L (SRH-IDX)
                                           TO  SRH-SRYKAMEI1
                   MOVE    SRH-SRYKAMEI2L (SRH-IDX)
                                           TO  SRH-SRYKAMEI2
                   MOVE    SRH-SRYKAMEI3L (SRH-IDX)
                                           TO  SRH-SRYKAMEI3
                   MOVE    SRH-SRYKA-MAX   TO  SRH-IDX
               END-IF
           END-PERFORM
      *
           .
       200111-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    転帰事由編集処理
      *****************************************************************
       200111-PTBYO-TENKIKBN-HEN-SEC     SECTION.
      *
           MOVE    SPACE                   TO  WRK-PTBYO-TENKIKBNMEI
      *
           EVALUATE    WRK-PTBYO-TENKIKBN
           WHEN    "1"
               MOVE    "治ゆ"              TO  WRK-PTBYO-TENKIKBNMEI
           WHEN    "2"
               MOVE    "死亡"              TO  WRK-PTBYO-TENKIKBNMEI
           WHEN    "3"
               MOVE    "中止"              TO  WRK-PTBYO-TENKIKBNMEI
           WHEN    "8"
               MOVE    "移行"              TO  WRK-PTBYO-TENKIKBNMEI
           END-EVALUATE
      *
           .
       200111-PTBYO-TENKIKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       2001-SRYACT-HEN-SEC             SECTION.
      *
      *    各欄からあふれた情報を診療行為欄に編集を行う
           PERFORM 20011-AFURE-HEN-SEC
      *
      *    入院の場合は入院履歴を編集
      *    また、入院会計の保険組合せを取得
           IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
               PERFORM 2002-NYUINRRK-HEN-SEC
               PERFORM 2002-NACCT-HKNCOMBI-GET-SEC
           END-IF
      *
           MOVE    1                   TO  WRK-NACCT-HKNCOMBI-IDX
           MOVE    SPACE               TO  WRK-HKNCOMBI-KEY
                                           WRK-HKNCOMBI-KEY-MAE
                                           WRK-HKNNUM
      *
      *    診療会計マスターを編集
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                      SRYACCT-REC 
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    ORCHCM34-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    ORCHCM34-SRYYM      TO  ACCT-SRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "SRYACCT-KEY20"     TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACCT
           ELSE
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           END-IF
      *
           MOVE    ZERO                TO  WKSRYACCT-MAX
           INITIALIZE                      WKSRYACCT-REC
           PERFORM UNTIL ( FLG-SRYACCT     =   1 )
                    OR   ( WKSRYACCT-MAX  >=   CONST-SRYACCT-MAX )
      *
      *        診療行為マスタ処理
               PERFORM 200111-SRYACT-SEL-SEC
      *
               IF    ( FLG-YAKUSOKU        =   1 )
                   COMPUTE ACCT-MEISAISU
                       =   ACCT-MEISAISU   -   1
               END-IF
      *
               PERFORM 20011-SRYACT-MATOME-SEC
      *
               MOVE    "SRYACCT-KEY20"     TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1               TO  FLG-SRYACCT
               ELSE
                   MOVE    MCPDATA-REC     TO  SRYACCT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SRYACCT-KEY20"     TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
      *    診療行為並び替え処理
           PERFORM 20011-SRYACT-SORT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKSRYACCT-MAX )
      *
               PERFORM 20011-SRYACT-HEN-SEC
      *
           END-PERFORM
      *
           PERFORM 200111-JIHI-TOTAL-HEN-SEC
      *
           IF    ( ORCHCM34-NYUGAIKBN      =   "1" )
               MOVE    WRK-HKNCOMBI-KEY
                                   TO  WRK-HKNCOMBI-KEY-MAE
               MOVE    HIGH-VALUE  TO  WRK-HKNCOMBI-KEY
                                       WRK-HKNNUM
      *        入院の診療行為をチェック
               PERFORM 2002-NSRYACT-CHK-SEC
           END-IF
      *
           .
       2001-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    同一剤まとめ編集処理
      *****************************************************************
       20011-SRYACT-MATOME-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAIMATOME
      *
           SET     WKSRYACCT-IDX       TO  1
           SEARCH  WKSRYACCT-OCC   VARYING WKSRYACCT-IDX
           AT  END
      *
                   MOVE    ZERO        TO  FLG-ZAIMATOME
      *
           WHEN    WKACCT-HOSPID (WKSRYACCT-IDX)
                                       =   SPACE
      *
                   MOVE    ZERO        TO  FLG-ZAIMATOME
      *
           WHEN    ACCT-HOSPID         =   WKACCT-HOSPID
                                                       (WKSRYACCT-IDX)
            AND    ACCT-NYUGAIKBN      =   WKACCT-NYUGAIKBN
                                                       (WKSRYACCT-IDX)
            AND    ACCT-PTID           =   WKACCT-PTID
                                                       (WKSRYACCT-IDX)
            AND    ACCT-SRYYM          =   WKACCT-SRYYM
                                                       (WKSRYACCT-IDX)
            AND    ACCT-SRYKBN         =   WKACCT-SRYKBN
                                                       (WKSRYACCT-IDX)
            AND    ACCT-HKNCOMBI       =   WKACCT-HKNCOMBI
                                                       (WKSRYACCT-IDX)
            AND    ACCT-ZAITEN         =   WKACCT-ZAITEN 
                                                       (WKSRYACCT-IDX)
            AND    ACCT-SRYCDTOTAL     =   WKACCT-SRYCDTOTAL
                                                       (WKSRYACCT-IDX)
            AND    ACCT-SURYOUTOTAL    =   WKACCT-SURYOUTOTAL
                                                       (WKSRYACCT-IDX)
            AND    ACCT-MEISAISU       =   WKACCT-MEISAISU
                                                       (WKSRYACCT-IDX)
            AND    WRK-RUIKEITOTAL     =   WKACCT-RUIKEITOTAL
                                                       (WKSRYACCT-IDX)
               INITIALIZE  COMMENT-CHK-AREA
               MOVE    ACCT-ZAINUM     TO  COMMENT-ZAINUM (1)
               MOVE    WKACCT-ZAINUM (WKSRYACCT-IDX)
                                       TO  COMMENT-ZAINUM (2)
               PERFORM 200111-PTCOM-CHK-SEC
               IF    ( FLG-COMMENT-CHK =   ZERO )
                   MOVE    1           TO  FLG-ZAIMATOME
               ELSE
                   MOVE    ZERO        TO  FLG-ZAIMATOME
               END-IF
           END-SEARCH
      *
           IF    ( FLG-ZAIMATOME       =   1 )
               COMPUTE WKACCT-ZAIKAISU  (WKSRYACCT-IDX)
                   =   WKACCT-ZAIKAISU  (WKSRYACCT-IDX)
                   +   ACCT-ZAIKAISU
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   31 )
                   COMPUTE WKACCT-DAY (WKSRYACCT-IDX 1 IDX1)
                        =  WKACCT-DAY (WKSRYACCT-IDX 1 IDX1)
                        +   ACCT-DAY (1 IDX1)
               END-PERFORM
           ELSE
               COMPUTE WKSRYACCT-MAX   =   WKSRYACCT-MAX   +   1
               MOVE    SRYACCT-REC     TO  WKSRYACCT-OCC (WKSRYACCT-MAX)
               MOVE    WRK-RUIKEITOTAL TO  WKACCT-RUIKEITOTAL
                                                     (WKSRYACCT-MAX)
               MOVE    WRK-SRYCD       TO  WKACCT-SRT-SRYCD
                                                     (WKSRYACCT-MAX)
               MOVE    WRK-SRYSYUKBN   TO  WKACCT-SRT-SRYSYUKBN
                                                     (WKSRYACCT-MAX)
               MOVE    FLG-SRYSTATUS   TO  WKACCT-SRT-SRYSTATUS
                                                     (WKSRYACCT-MAX)
           END-IF
      *
           .
       20011-SRYACT-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントチェック処理
      *****************************************************************
       200111-PTCOM-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-COMMENT-CHK
      *
           MOVE    1               TO  COMMENT-IDX
           PERFORM 2001111-PTCOM-HEN-SEC
      *
           MOVE    2               TO  COMMENT-IDX
           PERFORM 2001111-PTCOM-HEN-SEC
      *
           IF    ( COMMENT-MAX (1)         =   ZERO )
            AND  ( COMMENT-MAX (2)         =   ZERO )
                CONTINUE
           ELSE
               IF    ( COMMENT-MAX (1)         =   COMMENT-MAX (2) )
                AND  ( COMMENT-COMMENT-G (1)   =   COMMENT-COMMENT-G(2))
                    CONTINUE
               ELSE
                    MOVE   1           TO  FLG-COMMENT-CHK
               END-IF
           END-IF
      *
           .
       200111-PTCOM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者コメント編集処理
      *****************************************************************
       2001111-PTCOM-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTCOM
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SPA-HOSPID          TO  PTCOM-HOSPID
           MOVE    SPA-PTID            TO  PTCOM-PTID
           MOVE    COMMENT-ZAINUM (COMMENT-IDX)
                                       TO  PTCOM-ZAINUM
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "PTCOM-KEY2"        TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-PTCOM
           ELSE
               MOVE    MCPDATA-REC     TO  PTCOM-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTCOM   NOT =   ZERO )
                    OR   ( COMMENT-MAX (COMMENT-IDX)
                                           >   10 )
      *
               COMPUTE COMMENT-MAX (COMMENT-IDX)
                   =   COMMENT-MAX (COMMENT-IDX)   +   1
               MOVE    COMMENT-MAX (COMMENT-IDX)
                                       TO  IDX21
      *
               MOVE    PTCOM-INPUTCOMENT
                                   TO  COMMENT-COMMENT
                                               (COMMENT-IDX IDX21)
      *
               MOVE    "PTCOM-KEY2"    TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1           TO  FLG-PTCOM
               ELSE
                   MOVE    MCPDATA-REC TO  PTCOM-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "PTCOM-KEY2"        TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       2001111-PTCOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為並び替え処理
      *****************************************************************
       20011-SRYACT-SORT-SEC           SECTION.
      *
           INITIALIZE                      SRT-SRYACCT
      *
      *    ソート用ワークにキー項目を退避する
           PERFORM VARYING SRT-IDX1    FROM    1   BY  1
                   UNTIL ( SRT-IDX1    >   WKSRYACCT-MAX )
      *
               MOVE    WKACCT-SRT-SRYCD (SRT-IDX1)
                                       TO  WRK-SRYCD
               MOVE    WKACCT-SRT-SRYSYUKBN (SRT-IDX1)
                                       TO  WRK-SRYSYUKBN
               MOVE    WKACCT-SRT-SRYSTATUS (SRT-IDX1)
                                       TO  FLG-SRYSTATUS
      *
               MOVE    WKSRYACCT-OCC (SRT-IDX1)
                                       TO  SRYACCT-REC
      *
               MOVE    SRT-IDX1        TO  SRT-SRYACCT-IDX (SRT-IDX1)
               MOVE    ACCT-HKNCOMBI   TO  SRT-SRYACCT-HKNCOMBI
                                                           (SRT-IDX1)
               MOVE    ACCT-SRYKBN     TO  SRT-SRYACCT-SRYKBN
                                                           (SRT-IDX1)
               IF    ( WRK-SRYCD   NOT =   SPACE )
                   PERFORM 200111-TENSU-SEL-SEC
                   IF    ( FLG-TENSU   =   ZERO )
      *
                       IF    ( ACCT-SRYKBN =   "60" )
                           PERFORM 200111-KNSJISKBN-HEN-SEC
                       END-IF
      *
                       MOVE   TNS-HOUKNSKBN
                                       TO   SRT-SRYACCT-HOUKNSKBN
                                                          (SRT-IDX1)
      *
                       IF    ( WRK-SRYCD   (1:1) =   "6"  OR  "7" )
                           IF  ( ACCT-SRYKBN     =   "21"
                                                  OR "22"
                                                  OR "23" )
                               CONTINUE
                           ELSE
                               MOVE    ALL "9"
                                   TO  SRT-SRYACCT-CDKBN-SYO-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-BU-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-KBN-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-KBNNUM-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-KBNNUM-EDA-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-KOUBAN-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-KOUHYOJYUNNUM-G
                                                          (SRT-IDX1)
                           END-IF
                       ELSE
      *
                           MOVE   TNS-CDKBN-SYO
                                       TO   SRT-SRYACCT-CDKBN-SYO
                                                          (SRT-IDX1)
                           MOVE   TNS-CDKBN-BU
                                       TO   SRT-SRYACCT-CDKBN-BU
                                                          (SRT-IDX1)
                           MOVE   TNS-CDKBN-KBN
                                       TO   SRT-SRYACCT-CDKBN-KBN
                                                          (SRT-IDX1)
                           MOVE   TNS-CDKBN-KBNNUM
                                       TO   SRT-SRYACCT-CDKBN-KBNNUM
                                                          (SRT-IDX1)
                           MOVE   TNS-CDKBN-KBNNUM-EDA
                                       TO   SRT-SRYACCT-CDKBN-KBNNUM-EDA
                                                          (SRT-IDX1)
                           MOVE   TNS-CDKBN-KOUBAN
                                       TO   SRT-SRYACCT-CDKBN-KOUBAN
                                                          (SRT-IDX1)
                           MOVE   TNS-KOUHYOJYUNNUM
                                       TO   SRT-SRYACCT-KOUHYOJYUNNUM
                                                          (SRT-IDX1)
                       END-IF
                   END-IF
      *
               END-IF
      *
               IF    ( FLG-SRYSTATUS  =  2 OR 3 )
                   MOVE    1               TO  SRT-SRYACCT-PRYORITY
                                                          (SRT-IDX1)
               END-IF
      *
               IF    ( WRK-SRYSYUKBN   =  "213" OR "223" OR "233" )
                           MOVE    1       TO  SRT-SRYACCT-YKZHKTKBN
                                                          (SRT-IDX1)
               ELSE
                   IF    ( ACCT-SRYKBN     =  "21" OR "22" OR "23" )
                    OR   ( WRK-SRYSYUKBN   =  "148" )
                       IF    ( ACCT-ZAITEN =  ZERO )
                           MOVE    1       TO  SRT-SRYACCT-YKZINGAIKBN
                                                          (SRT-IDX1)
                       END-IF
                   END-IF
               END-IF
      *
               IF    (  ACCT-SRYKBN    =  "21" )
                   IF    ( WRK-SRYSYUKBN   =   SPACE )
                       MOVE    "210"
                                       TO  SRT-SRYACCT-SRYSYUKBN
                                                          (SRT-IDX1)
                   ELSE
                       MOVE    WRK-SRYSYUKBN
                                       TO  SRT-SRYACCT-SRYSYUKBN
                                                          (SRT-IDX1)
                   END-IF
               END-IF
      *
               IF    ( ACCT-SRYKBN     =   "21" OR "40" 
                                       OR  "60" OR "80" )
                   CONTINUE
               ELSE
                   MOVE     ACCT-ZAITEN
                                       TO  SRT-SRYACCT-ZAITEN
                                                          (SRT-IDX1)
               END-IF
      *
               MOVE     ACCT-ZAINUM    TO  SRT-SRYACCT-ZAINUM
                                                          (SRT-IDX1)
           END-PERFORM
      *
      *    ソート用ワークについて並び替えを行う
           PERFORM VARYING SRT-IDX1    FROM    2   BY  1
                   UNTIL ( SRT-IDX1    >   WKSRYACCT-MAX )
               COMPUTE SRT-STIDX   =   SRT-IDX1    -   1
               MOVE    SRT-SRYACCT-OCC (SRT-IDX1)
                                   TO  SRT-SRYACCT-OCC (1001)
               PERFORM VARYING SRT-IDX2    FROM    SRT-STIDX   BY  -1
                       UNTIL ( SRT-IDX2    <   1 )
                          OR ( SRT-SRYACCT-KEY (1001)
                                       >=  SRT-SRYACCT-KEY (SRT-IDX2) )
                       MOVE    SRT-SRYACCT-OCC (SRT-IDX2)
                                   TO  SRT-SRYACCT-OCC (SRT-IDX2 + 1)
               END-PERFORM
               MOVE    SRT-SRYACCT-OCC (1001)
                                   TO  SRT-SRYACCT-OCC (SRT-IDX2 + 1)
           END-PERFORM
      *
           .
       20011-SRYACT-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索処理
      *****************************************************************
       200111-SRYACT-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKUSOKU
                                           FLG-SRYSTATUS
      *
           MOVE    SPACE               TO  WRK-SRYCD
                                           WRK-SRYSYUKBN
      *
           MOVE    ZERO                TO  WRK-RUIKEITOTAL
      *
           MOVE    ZERO                TO  FLG-SRYACT
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
           IF    ( MCP-RC            NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACT
           ELSE
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               IF    ( WRK-SRYSYUKBN   =   SPACE )
                   MOVE    SRY-SRYSYUKBN   TO  WRK-SRYSYUKBN
               END-IF
      *
               PERFORM 2001111-SET-SRYSTATUS-SEC
      *
               PERFORM VARYING IDX20   FROM    1   BY  1
                       UNTIL ( IDX20           >   5 )
      *
                   IF    ( SRY-SRYCD (IDX20) (1:1)  =   "S" )
                       MOVE    1           TO  FLG-YAKUSOKU
                   END-IF
      *
                   IF    ( SRY-SRYCD(IDX20)    IS  NUMERIC )
      *
                      MOVE     SRY-SRYCD(IDX20) TO  WRK-SRYCD-9
                      COMPUTE  WRK-RUIKEITOTAL =
                               WRK-RUIKEITOTAL +
                             ( WRK-SRYCD-9     *    SRY-SRYSURYO(IDX20))
      *
                       IF    ( WRK-SRYCD       =   SPACE )
                           IF    ( SRY-SRYCD (IDX20) (1:1)
                                               =   "0" OR "8" )
                               CONTINUE
                           ELSE
                               MOVE    SRY-SRYCD (IDX20)
                                           TO  WRK-SRYCD
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"   TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC            NOT =   ZERO )
                   MOVE    1           TO  FLG-SRYACT
               ELSE
                   MOVE    MCPDATA-REC TO  SRYACT-REC
               END-IF
           END-PERFORM
      *
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200111-SRYACT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為状態フラグ設定処理処理
      *****************************************************************
       2001111-SET-SRYSTATUS-SEC      SECTION.
      *
           PERFORM VARYING IDX23   FROM    1   BY  1
                   UNTIL ( IDX23   >   5 )
      *
               EVALUATE    TRUE
               WHEN  ( SRY-SRYCD (IDX23) (1:1) =   "6" )
                   MOVE    1       TO  FLG-SRYSTATUS
      *
               WHEN  ( SRY-SRYKBN        =   "60" )
                AND  ( SRY-SRYCD (IDX23)  =   "820000053" )
      *
                   MOVE    2       TO  FLG-SRYSTATUS
      *
               WHEN  ( SRY-SRYKBN        =   "80" )
                AND  ( SRY-SRYCD (IDX23)  =   "820000054" )
      *
                   MOVE    2       TO  FLG-SRYSTATUS
      *
               WHEN  ( SRY-SRYKBN        =   "80" )
                AND  ( SRY-SRYCD (IDX23)  =   "820000055" )
      *
                   MOVE    2       TO  FLG-SRYSTATUS
               WHEN  ( SRY-SRYCD (IDX23)  =   "820000046" )
      *
                   MOVE    3       TO  FLG-SRYSTATUS
      *
               END-EVALUATE
      *
           END-PERFORM
      *
           .
       2001111-SET-SRYSTATUS-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       200111-TENSU-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
      *    点数検索用日付編集
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX7    FROM    1   BY  1
                   UNTIL     ( IDX7    >   31 )
               IF    ( ACCT-DAY (1 IDX7)   NOT =   ZERO )
                   MOVE    IDX7        TO  WRK-SRYDD
                   MOVE    31          TO  IDX7
               END-IF
           END-PERFORM
           MOVE    WRK-SRYYMD          TO  ORC-DBYMD
      *
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           .
       200111-TENSU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ検索処理
      *****************************************************************
       200111-KNSJISKBN-HEN-SEC        SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( TNS-KNSJISKBN       =   2 )
               MOVE    5       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
           WHEN  ( TNS-CDKBN-KBN       =   "9" )
               MOVE    4       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
           WHEN    OTHER
               EVALUATE    TNS-CDKBN-KBNNUM
               WHEN  ZERO    THRU    199
                   MOVE    1       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
               WHEN  400     THRU    499
                   MOVE    2       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
               WHEN  200     THRU    399
                   MOVE    3       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
               END-EVALUATE
           END-EVALUATE
      *
           .
       200111-KNSJISKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       20011-SRYACT-HEN-SEC            SECTION.
      *
           MOVE    SRT-SRYACCT-HOUKNSKBN (IDX1)
                                       TO  WRK-TNS-HOUKNSKBN
           MOVE    SRT-SRYACCT-IDX (IDX1)
                                       TO  IDX11
           MOVE    WKSRYACCT-OCC(IDX11)
                                       TO  SRYACCT-REC
      *
           MOVE    1                   TO  FLG-EDIT
           MOVE    ZERO                TO  FLG-SRYSTATUS
      *
           IF    ( ACCT-ZAIKAISU       =   ZERO )
               MOVE    ZERO            TO  FLG-EDIT
           END-IF
      *
      *
           IF    ( ACCT-ZAITEN         =   ZERO )
      *
               IF    ( ACCT-SRYKBN     =   "11" OR "12" OR "99" )
                   MOVE    ZERO        TO  FLG-EDIT
               ELSE
      *
                   PERFORM 200111-SRYACT-CHK-SEC
                   IF    ( FLG-SRYSTATUS  =   ZERO )
                       MOVE    ZERO    TO  FLG-EDIT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-EDIT            =   ZERO )
               GO  TO  20011-SRYACT-HEN-EXT
           END-IF
     *
           MOVE    ZERO                TO  CNT-HOUSRYACT
           MOVE    ZERO                TO  FLG-SRYACT-HEN5
           MOVE    ZERO                TO  FLG-MINUS
           MOVE    ZERO                TO  FLG-RSIJBI-AREA-HEN
           MOVE    ZERO                TO  FLG-JIBAI
           MOVE    ZERO                TO  FLG-ROSAI
           MOVE    SPACE               TO  WRK-JBI-RYOYO-TEATE-KBN
           INITIALIZE                      WRK-JBI-SONOTA-G
                                           WRK-RSI-SONOTA-G
      *
      *    診療行為マスター読み込み
           MOVE    ZERO                TO  FLG-SRYACT
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX7    FROM    1   BY  1
                   UNTIL     ( IDX7    >   31 )
               IF    ( ACCT-DAY (1 IDX7)   NOT =   ZERO )
                   MOVE    IDX7        TO  WRK-SRYDD
                   MOVE    31          TO  IDX7
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACT
           ELSE
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           END-IF
      *
      *    IF    ( FLG-SRYACT          NOT =   ZERO )
      *      OR  ( SRY-SRYSYUKBN       =   "213" OR "223" OR "233" )
      *        GO  TO  20011-SRYACT-HEN-EXT
      *    END-IF
      *
      *    保険組合せキーブレイクチェック処理
           PERFORM 200111-HKNCOMBI-KEY-CHK-SEC
      *
           IF    ( WRK-HKNNUM          =   "971" OR "973" )
      *        労災自賠のとき
      *        未実施減算・日常生活障害加算・痴呆加算・
      *        瘡患者管理加算を除く
               IF    ( ACCT-SRYCDTOTAL =   190080470   OR  190799670
                                       OR  190080570   OR  190799770
                                       OR  190111690   OR  190809090
                                       OR  190111790   OR  190809190
      *
                                       OR  190109170   OR  190807570
                                       OR  190109270   OR  190808670
                                       OR  190116990   OR  190813390 )
                   GO  TO  20011-SRYACT-HEN-EXT
               END-IF
      *
               IF   ( WRK-HKNNUM       =   "973" )
                   MOVE    1           TO  FLG-JIBAI
               END-IF
      *
               IF   ( WRK-HKNNUM       =   "971" )
                   MOVE    1           TO  FLG-ROSAI
               END-IF
      *
               IF   ( ACCT-SRYKBN      =   "11" OR "12" OR "13" )
                   MOVE    1           TO  FLG-RSIJBI-AREA-HEN
               END-IF
      *
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
                    OR   ( WRK-SRYACT-MAX  >   500 )
      *
      *        剤内１行目
               IF   (  SRY-RENNUM      =   1 )
                   PERFORM 200111-GYO1-HEN-SEC
      *
                   IF    ( FLG-RSIJBI-AREA-HEN =   1 )
                       PERFORM 200111-RSIJBI-SYOKI-SEC
                   END-IF
      *
               END-IF
      *
      *        剤明細編集
               PERFORM VARYING IDX9    FROM    1   BY  1
                       UNTIL ( IDX9                >  5    )
                        OR   ( WRK-SRYACT-MAX      >   500 )
                        OR   ( SRY-SRYCD (IDX9)    =   SPACE )
      *
                   IF    ( SRY-SRYCD (IDX9)    =   CONST-TEIGEN )
                       MOVE    1           TO  FLG-MINUS
                   END-IF
      *
                   IF    ( WRK-HKNNUM          NOT =     "971" )
                    AND  ( WRK-HKNNUM          NOT =     "973" )
                       IF    ( SRY-SRYCD (IDX9)    =   CONST-GENZAN1
                                                   OR  CONST-GENZAN2
                                                   OR  CONST-GENZAN3
                                                   OR  CONST-GENZAN4
                                                   OR  CONST-GENZAN5
                                                   OR  CONST-GENZAN6
                                                   OR  CONST-GENZAN7
                                                   OR  CONST-GENZAN8 )
                           MOVE    1       TO  FLG-MINUS
                       END-IF
                   END-IF
      *
                   IF    ( FLG-RSIJBI-AREA-HEN =   1 )
                       IF    ( RSIJBI-MAX      <   CONST-RSIJBI-MAX )
                           PERFORM 200111-RSIJBI-HEN-SEC
                       END-IF
                   ELSE
      *
      *                自費の時、金額を表示
                       IF    ( SRY-SRYKBN  =   "95"  OR  "96" )
                           PERFORM 200111-JIHIMEI-HEN-SEC
                       ELSE
                           PERFORM 200111-ZAIMEI-HEN-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1               TO  FLG-SRYACT
               ELSE
                   MOVE    MCPDATA-REC     TO  SRYACT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SRYACT-KEY2"           TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           EVALUATE    TRUE
      *
           WHEN  ( FLG-RSIJBI-AREA-HEN =   1 )
               PERFORM 200111-RSIJBI-SRYACT-HEN-SEC
      *
           WHEN  ( WRK-JBI-SONOTA-KBN      NOT =   SPACE )
               PERFORM 200111-JBI-SONOTA-HEN-SEC
           WHEN  ( WRK-RSI-SONOTA-KBN      NOT =   SPACE )
               PERFORM 200111-RSI-SONOTA-HEN-SEC
           WHEN  ( WRK-JBI-RYOYO-TEATE-KBN NOT =   SPACE )
               PERFORM 200111-JBI-RYOYO-TEATE-HEN-SEC
      *
           WHEN  ( ACCT-SRYKBN         =   "95"  OR  "96" )
               CONTINUE
      *
           WHEN    OTHER
               IF    ( WRK-TNS-HOUKNSKBN   NOT =   ZERO )
                   IF    ( FLG-SRYACT-HEN5 NOT =   ZERO )
                       PERFORM 210-WRK-SRYACT-TBL-HEN5-SEC
                   END-IF
                   IF    ( CNT-HOUSRYACT   >   1   )
      *                検査数編集処理
                       PERFORM 200111-KENSASU-HEN-SEC
                   END-IF
               END-IF
      *        点数回数編集処理
               MOVE    ACCT-ZAITEN         TO  WRK-ZAITEN
               MOVE    ACCT-ZAIKAISU       TO  WRK-ZAIKAISU
               IF    ( WRK-ZAITEN         >   ZERO )
                   PERFORM 205-TENSUKAISU-HEN-SEC
               ELSE
                   EVALUATE    FLG-SRYSTATUS
                   WHEN    2
                       PERFORM 205-KAISU-HEN-SEC
                   WHEN    3
                       PERFORM 206-KAISU-HEN-SEC
                   WHEN    OTHER
                       PERFORM 205-TENSUKAISU-HEN-SEC
                   END-EVALUATE
               END-IF
           END-EVALUATE
      *
           MOVE    "5"                     TO  WRK-ITEM-KBN
           MOVE    ZERO                    TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       20011-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *   診療行為チェック処理
      *****************************************************************
       200111-SRYACT-CHK-SEC           SECTION.
      *
      *    診療行為マスター読み込み
           MOVE    ZERO                TO  FLG-SRYACT
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   5 )
               MOVE    "%"             TO  SRY-SRYCD (IDX0)
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    PATH-TBL-SRYACT-KEY10
                                       TO  MCP-PATH
           PERFORM 920-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACT
           ELSE
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           END-IF
      *
      *
           PERFORM UNTIL ( FLG-SRYACT      NOT =   ZERO )
                    OR   ( FLG-SRYSTATUS  NOT =   ZERO )
      *
               PERFORM 2001111-SET-SRYSTATUS-SEC
      *
               MOVE    PATH-TBL-SRYACT-KEY10
                                       TO  MCP-PATH
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1               TO  FLG-SRYACT
               ELSE
                   MOVE    MCPDATA-REC     TO  SRYACT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    PATH-TBL-SRYACT-KEY10
                                       TO  MCP-PATH
           PERFORM 920-DBCLOSE-SEC
      *
           .
       200111-SRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せキーブレイクチェック処理
      *****************************************************************
       200111-HKNCOMBI-KEY-CHK-SEC     SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( IDX1                    =     1 )
               MOVE    SPACE               TO  WRK-HKNCOMBI-KEY-MAE
               MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI-KEY
                                               WRK-HKNCOMBINUM
               PERFORM 200111-HKNCOMBI-SEL-SEC
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               IF    ( ORCHCM34-NYUGAIKBN      =   "1" )
      *            入院の診療行為をチェック
                   PERFORM 2002-NSRYACT-CHK-SEC
               END-IF
           WHEN  ( IDX1                >   1 )
               IF    ( ACCT-HKNCOMBI   NOT =   WRK-HKNCOMBI-KEY )
                   PERFORM 200111-JIHI-TOTAL-HEN-SEC
      *
                   MOVE    WRK-HKNCOMBI-KEY
                                           TO  WRK-HKNCOMBI-KEY-MAE
                   MOVE    ACCT-HKNCOMBI   TO  WRK-HKNCOMBI-KEY
                                               WRK-HKNCOMBINUM
                   PERFORM 200111-HKNCOMBI-SEL-SEC
                   MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
      *
                   IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
      *                入院の診療行為をチェック
                       PERFORM 2002-NSRYACT-CHK-SEC
                   END-IF
      *
                   MOVE    1               TO  FLG-LINE
      *
               ELSE
                   MOVE    ZERO            TO  FLG-LINE
               END-IF
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "5"                 TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-EVALUATE
      *
           .
       200111-HKNCOMBI-KEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ検索処理
      *****************************************************************
       200111-HKNCOMBI-SEL-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               INITIALIZE                  HKNCOMBI-REC
           ELSE
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           END-IF
      *
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200111-HKNCOMBI-SEL-EXT.
           EXIT.
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       200111-GYO1-HEN-SEC             SECTION.
      *
           MOVE    SRY-SRYKBN          TO  WRK-SRYACT-SRYKBN
      *
      *    診療区分
           IF    ( ACCT-ZAITEN         =   ZERO )
            AND  ( FLG-SRYSTATUS       =   1 )
               IF    ( SRY-SRYSYUKBN       =   "213" OR "223" OR "233" )
                        MOVE    "○"   TO  WRK-SRYACT-MARK
                        COMPUTE CNT-YKZHKT
                            =   CNT-YKZHKT  +   1
               ELSE
                   IF    ( ACCT-SRYKBN     =   "21" OR "22" OR "23" )
                    OR   ( SRY-SRYSYUKBN   =  "148" )
                        MOVE    "☆"   TO  WRK-SRYACT-MARK
                        COMPUTE CNT-YKZINGAI
                            =   CNT-YKZINGAI    +   1
                   END-IF
               END-IF
           END-IF
      *
      *    保険組合せ
           MOVE    ACCT-HKNCOMBI       TO  WRK-SRYACT-HKNCOMBI
      *    回数
           PERFORM VARYING IDX8    FROM    1   BY  1
                   UNTIL ( IDX8    >   31 )
               MOVE    ACCT-DAY  (1 IDX8)
                                       TO  WRK-SRYACT-DAY-Z (IDX8)
           END-PERFORM
      *
           .
       200111-GYO1-HEN-EXT.
           EXIT.
      *****************************************************************
      *   労災自賠責編集エリア初期処理
      *****************************************************************
       200111-RSIJBI-SYOKI-SEC         SECTION.
      *
           INITIALIZE                  RSIJBI-AREA
      *
           MOVE    ACCT-ZAITEN     TO  RSIJBI-TEN
           MOVE    ACCT-ZAIKAISU   TO  RSIJBI-ZAIKAISU
      *
           .
       200111-RSIJBI-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *   労災自賠責編集エリア編集処理
      *****************************************************************
       200111-RSIJBI-HEN-SEC           SECTION.
      *
           COMPUTE RSIJBI-MAX  =   RSIJBI-MAX  +   1
      *
      *    診療コードより診療行為名称を取得
           INITIALIZE  TNS-TENSU-REC
           MOVE    SRY-SRYCD (IDX9)        TO  TNS-SRYCD
           MOVE    ZERO                    TO  TNS-YUKOSTYMD
           MOVE    ALL "9"                 TO  TNS-YUKOEDYMD
           MOVE    WRK-SRYYMD              TO  ORC-DBYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
               MOVE    SRY-SRYCD (IDX9)    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
           ELSE
      *
               MOVE    TNS-NAME            TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
      *
           END-IF
      *
           MOVE    TNS-SRYCD               TO  RSIJBI-SRYCD(RSIJBI-MAX)
           MOVE    SRY-INPUTNUM (IDX9)     TO  RSIJBI-INPUTNUM
                                                           (RSIJBI-MAX)
           MOVE    TNS-NAME                TO  RSIJBI-NAME (RSIJBI-MAX)
      *
           IF    ( TNS-TENSIKIBETU     =   1 )
               COMPUTE RSIJBI-KINGAKU  =   RSIJBI-KINGAKU + TNS-TEN
               COMPUTE RSIJBI-TEN      =   RSIJBI-TEN - ( TNS-TEN / 10 )
      *
               COMPUTE RSIJBI-KIN-MAX  =   RSIJBI-KIN-MAX  +   1
      *
               IF   (( TNS-SRYCD   (1:1)       =   "8"  )
                 OR  ( TNS-SRYCD   (1:3)       =   "008"  ))
                AND  ( SRY-INPUTNUM(IDX9)  NOT =   ZERO )
                   COMPUTE RSIJBI-KIN-COM-MAX
                       =   RSIJBI-KIN-COM-MAX  +   1
               END-IF
      *
      *
               MOVE    ZERO            TO  RSIJBI-TEN-FLG  (RSIJBI-MAX)
               MOVE    RSIJBI-KIN-MAX  TO  RSIJBI-GRP-CNT  (RSIJBI-MAX)
           ELSE
               COMPUTE RSIJBI-TEN-MAX  =   RSIJBI-TEN-MAX  +   1
      *
               IF   (( TNS-SRYCD   (1:1)       =   "8"  )
                 OR  ( TNS-SRYCD   (1:3)       =   "008"  ))
                AND  ( SRY-INPUTNUM(IDX9)  NOT =   ZERO )
                   COMPUTE RSIJBI-TEN-COM-MAX
                       =   RSIJBI-TEN-COM-MAX  +   1
               END-IF
      *
               MOVE    1               TO  RSIJBI-TEN-FLG  (RSIJBI-MAX)
               MOVE    RSIJBI-TEN-MAX  TO  RSIJBI-GRP-CNT  (RSIJBI-MAX)
           END-IF
      *
           .
       200111-RSIJBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *   労災自賠診療行為編集処理
      *****************************************************************
       200111-RSIJBI-SRYACT-HEN-SEC    SECTION.
      *
      *    労災自賠編集領域をソート
           PERFORM VARYING RSIJBI-IDX1    FROM    2   BY  1
                   UNTIL ( RSIJBI-IDX1    >   RSIJBI-MAX )
               COMPUTE RSIJBI-STIDX   =   RSIJBI-IDX1    -   1
               MOVE    RSIJBI-OCC (RSIJBI-IDX1)
                                   TO  RSIJBI-OCC (21)
               PERFORM VARYING RSIJBI-IDX2 FROM    RSIJBI-STIDX   BY  -1
                       UNTIL ( RSIJBI-IDX2    <   1 )
                          OR ( RSIJBI-KEY (21)
                                       >=  RSIJBI-KEY (RSIJBI-IDX2) )
                       MOVE    RSIJBI-OCC (RSIJBI-IDX2)
                                   TO  RSIJBI-OCC (RSIJBI-IDX2 + 1)
               END-PERFORM
               MOVE    RSIJBI-OCC (21)
                                   TO  RSIJBI-OCC (RSIJBI-IDX2 + 1)
           END-PERFORM
      *
           MOVE    ZERO            TO  CNT-RSIJBI-KIN
                                       CNT-RSIJBI-TEN
      *
           PERFORM VARYING IDX19   FROM    1   BY  1
                   UNTIL ( IDX19   >   RSIJBI-MAX )
      *
      *        コメントの時、内容設定
               IF   (( RSIJBI-SRYCD  (IDX19)(1:1)  =   "8"  )
                 OR  ( RSIJBI-SRYCD  (IDX19)(1:3)  =   "008"  ))
                AND  ( RSIJBI-INPUTNUM(IDX19)  NOT =   ZERO )
      *
                   INITIALIZE                          PTCOM-REC
                   MOVE    ACCT-HOSPID             TO  PTCOM-HOSPID
                   MOVE    ACCT-PTID               TO  PTCOM-PTID
                   MOVE    ACCT-ZAINUM             TO  PTCOM-ZAINUM
                   MOVE    RSIJBI-SRYCD   (IDX19)  TO  PTCOM-SRYCD
                   MOVE    RSIJBI-INPUTNUM(IDX19)  TO  PTCOM-RENNUM
                   MOVE    PTCOM-REC               TO  MCPDATA-REC
                   MOVE    "PTCOM-KEY"             TO  ORC-DBPATH
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC        NOT =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    MCPDATA-REC     TO  PTCOM-REC
                       IF    ( PTCOM-INPUTCOMENT   NOT =   SPACE )
                           MOVE    PTCOM-INPUTCOMENT
                                               TO  WRK-SRYACT-STR
                       END-IF
                   END-IF
      *
                   MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
                   PERFORM 910-DBCLOSE-SEC
      *
                   PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               ELSE
      *
                   MOVE    RSIJBI-NAME (IDX19)     TO  WRK-SRYACT-STR
                   PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
                   IF    ( RSIJBI-TEN-FLG (IDX19)  =   ZERO )
                       COMPUTE CNT-RSIJBI-KIN  =   CNT-RSIJBI-KIN  +   1
                       IF    ( RSIJBI-KIN-MAX  -   RSIJBI-KIN-COM-MAX
                                               =   CNT-RSIJBI-KIN )
                           MOVE    RSIJBI-KINGAKU  TO  WRK-ZAITEN
                           MOVE    RSIJBI-ZAIKAISU TO  WRK-ZAIKAISU
                           PERFORM 205-KINGAKUKAISU-HEN-SEC
                       END-IF
                   ELSE
                       COMPUTE CNT-RSIJBI-TEN  =   CNT-RSIJBI-TEN  +   1
                       IF    ( RSIJBI-TEN-MAX  -   RSIJBI-TEN-COM-MAX
                                               =   CNT-RSIJBI-TEN )
                           MOVE    RSIJBI-TEN      TO  WRK-ZAITEN
                           MOVE    RSIJBI-ZAIKAISU TO  WRK-ZAIKAISU
      *
                           IF    ( WRK-ZAITEN          >   ZERO )
                               PERFORM 205-TENSUKAISU-HEN-SEC
                           ELSE
                               EVALUATE    FLG-SRYSTATUS
                               WHEN    2
                                   PERFORM 205-KAISU-HEN-SEC
                               WHEN    3
                                   PERFORM 206-KAISU-HEN-SEC
                               WHEN    OTHER
                                   PERFORM 205-TENSUKAISU-HEN-SEC
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200111-RSIJBI-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *   労災その他編集処理
      *****************************************************************
       200111-RSI-SONOTA-HEN-SEC       SECTION.
      *
           MOVE    WRK-RSI-SONOTA-TEN  TO  WRK-ZAITEN
      *
           MOVE    ACCT-ZAIKAISU       TO  WRK-ZAIKAISU
      *
           PERFORM 205-KINGAKUKAISU-HEN-SEC
      *
           .
       200111-RSI-SONOTA-HEN-EXT.
           EXIT.
      *****************************************************************
      *   自賠責その他編集処理
      *****************************************************************
       200111-JBI-SONOTA-HEN-SEC       SECTION.
      *
           MOVE    WRK-JBI-SONOTA-TEN  TO  WRK-ZAITEN
      *
           MOVE    ACCT-ZAIKAISU       TO  WRK-ZAIKAISU
      *
           EVALUATE    WRK-JBI-SONOTA-KBN
           WHEN    "1"
           WHEN    "2"
                IF   ( SYS-4001-TAXKBN         =   "2" )
                  IF ( SYS-1001-TAX-HASU-KBN   =   "1" )
                     COMPUTE  WRK-ZAITEN
                         =  ( WRK-ZAITEN   *   1.05)  +  0.5
                  ELSE
                     COMPUTE  WRK-ZAITEN
                         =    WRK-ZAITEN   *   1.05
                  END-IF
                END-IF
           WHEN    "3"
               IF    ( SYS-4001-TAXKBN-KZI     =   "2" )
                   MOVE    ZERO        TO  FLG-JBI-KZI-OUT
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   10 )
                       IF    ( WRK-JBI-SONOTA-SRYCD
                                           =   SYS-4003-SRYCD (IDX0))
                           MOVE    1   TO  FLG-JBI-KZI-OUT
                       END-IF
                   END-PERFORM
      *
                   IF    ( FLG-JBI-KZI-OUT =   ZERO )
                       IF    ( SYS-1001-TAX-HASU-KBN   =   "1" )
                           COMPUTE  WRK-ZAITEN
                               =  ( WRK-ZAITEN   *   1.05)  +  0.5
                       ELSE
                           COMPUTE  WRK-ZAITEN
                               =    WRK-ZAITEN   *   1.05
                       END-IF
                   END-IF
               END-IF
           END-EVALUATE
      *
           PERFORM 205-KINGAKUKAISU-HEN-SEC
      *
           .
       200111-JBI-SONOTA-HEN-EXT.
           EXIT.
      *****************************************************************
      *   自賠責療養担当手当編集処理
      *****************************************************************
       200111-JBI-RYOYO-TEATE-HEN-SEC  SECTION.
      *
           IF    ( WRK-JBI-RYOYO-TEATE-KBN     =   "1" )
               MOVE    120             TO  WRK-ZAITEN
           ELSE
               MOVE    84              TO  WRK-ZAITEN
           END-IF
      *
           MOVE    ACCT-ZAIKAISU       TO  WRK-ZAIKAISU
      *
           PERFORM 205-KINGAKUKAISU-HEN-SEC
      *
           .
       200111-JBI-RYOYO-TEATE-HEN-EXT.
           EXIT.
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       200111-ZAIMEI-HEN-SEC           SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYACT-STR
      *
      *    約束セットのコードは読み飛ばす
           IF    ( SRY-SRYCD (IDX9)(1:1)   =   "S" )
               GO  TO  200111-ZAIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           INITIALIZE  TNS-TENSU-REC
           MOVE    SRY-SRYCD (IDX9)        TO  TNS-SRYCD
           MOVE    ZERO                    TO  TNS-YUKOSTYMD
           MOVE    ALL "9"                 TO  TNS-YUKOEDYMD
           MOVE    WRK-SRYYMD              TO  ORC-DBYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
               MOVE    SRY-SRYCD (IDX9)    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
           ELSE
      *
               MOVE    TNS-NAME            TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
      *
           END-IF
      *
      *    行為名称
           MOVE    TNS-NAME                TO  WRK-SRYACT-STR
      *
      *    薬剤服用コメントの時、再編集
           IF    ( SRY-SRYCD (IDX9)(1:3)   =   "001" )
      *
               STRING "【"                 DELIMITED   BY  SIZE
                      TNS-NAME             DELIMITED   BY  SPACE
                      "】"                 DELIMITED   BY  SIZE
               INTO   WRK-SRYACT-STR
               END-STRING
           END-IF
      *
      *    コメントの時、内容設定
           IF   (( SRY-SRYCD  (IDX9)(1:1)  =   "8"  )
             OR  ( SRY-SRYCD  (IDX9)(1:3)  =   "008"  ))
            AND  ( SRY-INPUTNUM(IDX9)  NOT =   ZERO )
               PERFORM 2001111-PTCOM-SET-SEC
           END-IF
      *
      *    数量（薬剤、器材のとき）
      *    IF    ( SRY-SRYCD  (IDX9)(1:1)  =   "6"  OR  "7" )
      *     AND  ( TNS-TANINAME        NOT =   SPACE )
           IF    ( TNS-TANINAME        NOT =   SPACE )
               IF    ( SRY-SRYCD  (IDX9)(1:1)  =   "1" )
                 AND ( SRY-SRYKBN              =   "70" )
                 AND ( TNS-TANICD              =   CONST-GAZOU-TANICD )
                   MOVE    "回"                TO  WRK-TANINAME
                   PERFORM 2001111-SURYO-HEN-SEC
               ELSE
                   MOVE    TNS-TANINAME        TO  WRK-TANINAME
                   PERFORM 2001111-SURYO-HEN-SEC
               END-IF
           END-IF
      *
      *    分画数
      *    IF    ( SRY-SRYCD  (IDX9)(1:1)  =   "7"  )
      *     AND  ( TNS-DATAKBN             =   3    )
      *     AND  ( SRY-SRYKAISU (IDX9)     >   ZERO )
      *        PERFORM 2001111-BUNGASU-HEN-SEC
      *    END-IF
      *
           IF    ( WRK-TNS-HOUKNSKBN       =   ZERO )
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
           ELSE
               IF    ( SRY-SRYCD  (IDX9)(1:1)  =   "0" OR "8"  )
                   IF    ( FLG-SRYACT-HEN5 NOT =   ZERO )
                       PERFORM 210-WRK-SRYACT-TBL-HEN5-SEC
                   END-IF
                   PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
                   MOVE    ZERO        TO  FLG-SRYACT-HEN5
               ELSE
                   MOVE    SPACE               TO  WRK-STR
                   STRING  WRK-SRYACT-STR      DELIMITED   BY  SPACE
                           "，"                DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   END-STRING
                   MOVE    WRK-STR             TO  WRK-SRYACT-STR
                   PERFORM 210-WRK-SRYACT-TBL-HEN4-SEC
                   COMPUTE CNT-HOUSRYACT   =   CNT-HOUSRYACT   +   1
                   MOVE    1           TO  FLG-SRYACT-HEN5
               END-IF
           END-IF
      *
           IF    ( FLG-JIBAI           =   1 )
            AND  ( SRY-SRYKBN          =   "80" )
      *
               IF    ( SRY-SRYCD (IDX9)(1:5)   >=  "09591" )
                AND  ( SRY-SRYCD (IDX9)(1:5)   <=  "09594" )
                   MOVE    SRY-SRYCD (IDX9)(5:1)
                                           TO  WRK-JBI-SONOTA-KBN
                   MOVE    TNS-TEN         TO  WRK-JBI-SONOTA-TEN
                   MOVE    SRY-SRYCD (IDX9)
                                           TO  WRK-JBI-SONOTA-SRYCD
               ELSE
                   IF    ( SRY-SRYCD (IDX9)
                                       =   CONST-RYOYO-TEATE-GAI )
                       MOVE    "2"     TO  WRK-JBI-RYOYO-TEATE-KBN
                   END-IF
      *
                   IF    ( SRY-SRYCD (IDX9)
                                       =   CONST-RYOYO-TEATE-NYU )
                       MOVE    "1"     TO  WRK-JBI-RYOYO-TEATE-KBN
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( FLG-ROSAI           =   1 )
            AND  ( SRY-SRYKBN          =   "80" )
      *
               IF    ( SRY-SRYCD (IDX9)(1:5)   >=  "09591" )
                AND  ( SRY-SRYCD (IDX9)(1:5)   <=  "09594" )
                   MOVE    SRY-SRYCD (IDX9)(5:1)
                                           TO  WRK-RSI-SONOTA-KBN
                   MOVE    TNS-TEN         TO  WRK-RSI-SONOTA-TEN
               END-IF
           END-IF
      *
           .
       200111-ZAIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       200111-JIHIMEI-HEN-SEC              SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYACT-STR
      *
      *    約束セットのコードは読み飛ばす
           IF    ( SRY-SRYCD (IDX9)(1:1)   =   "S" )
               GO  TO  200111-JIHIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           INITIALIZE  TNS-TENSU-REC
           MOVE    SRY-SRYCD (IDX9)        TO  TNS-SRYCD
           MOVE    ZERO                    TO  TNS-YUKOSTYMD
           MOVE    ALL "9"                 TO  TNS-YUKOEDYMD
           MOVE    WRK-SRYYMD              TO  ORC-DBYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
               MOVE    SRY-SRYCD (IDX9)    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
           ELSE
      *
               MOVE    TNS-NAME            TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
      *
           END-IF
      *
      *    行為名称
           MOVE    TNS-NAME                TO  WRK-SRYACT-STR
      *
      *    コメントの時、内容設定
           IF   (( SRY-SRYCD  (IDX9)(1:1)  =   "8"  )
             OR  ( SRY-SRYCD  (IDX9)(1:3)  =   "008"  ))
            AND  ( SRY-INPUTNUM(IDX9)  NOT =   ZERO )
               PERFORM 2001111-PTCOM-SET-SEC
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
      *    コメント時、金額回数の編集は行わない
           IF   (( SRY-SRYCD  (IDX9)(1:1)  =   "8"  )
             OR  ( SRY-SRYCD  (IDX9)(1:3)  =   "008"  ))
             CONTINUE
           ELSE
      *
      *        計
               MOVE    1               TO  WRK-PNT
               MOVE    SPACE           TO  WRK-TENSUKAISU
               MOVE    SRY-JIHIMONEY (IDX9)
                                       TO  WRK-STR-LEFT-ZX99
      *
               PERFORM 800-STR-HIDARIZUME-HEN-SEC
               STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
                       "円"                DELIMITED   BY  SIZE
               INTO    WRK-TENSUKAISU
               WITH    POINTER WRK-PNT
               END-STRING
      *
               MOVE    ACCT-ZAIKAISU   TO  WRK-ZZ9
               STRING  "×"                DELIMITED   BY  SIZE
                       WRK-ZZ9             DELIMITED   BY  SIZE
               INTO    WRK-TENSUKAISU
               WITH    POINTER WRK-PNT
               END-STRING
      *
               MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
           END-IF
      *
           COMPUTE WRK-TTL-JIHI    =   WRK-TTL-JIHI
                                   + ( SRY-JIHIMONEY (IDX9)
                                   *   ACCT-ZAIKAISU )
      *
           IF    ( ACCT-SRYKBN         =   "96" )
               COMPUTE WRK-TTL-JIHI-TAX    =   WRK-TTL-JIHI-TAX
                                           + ( SRY-JIHIMONEY (IDX9)
                                           * ACCT-ZAIKAISU )
           END-IF
      *
           .
       200111-JIHIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自費計編集処理
      *****************************************************************
       200111-JIHI-TOTAL-HEN-SEC       SECTION.
      *
           IF    ( WRK-TTL-JIHI    >   ZERO )
      *
      *        空行を１行編集
               PERFORM 2010-KARAGYO-HEN-SEC
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               MOVE    "　　　　　自費合計額"
                                           TO  WRK-KINGAKKU-LBL
               MOVE    WRK-TTL-JIHI        TO  WRK-KINGAKKU
               PERFORM 2001111-KINGAKU-HEN-SEC
      *
      *        消費税分がある場合は消費税額を編集
               IF    ( WRK-TTL-JIHI-TAX    >   ZERO )
                   MOVE    "　　　　　自費消費税額"
                                           TO  WRK-KINGAKKU-LBL
                   IF   ( SYS-1001-TAX-HASU-KBN =   "1" )
                       COMPUTE  WRK-KINGAKKU
                            = ( WRK-TTL-JIHI-TAX   *  0.05 )
                            +  0.5
                   ELSE
                       COMPUTE  WRK-KINGAKKU
                            = ( WRK-TTL-JIHI-TAX   *  0.05 )
                   END-IF
                   PERFORM 2001111-KINGAKU-HEN-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  WRK-TTL-JIHI
           MOVE    ZERO                TO  WRK-TTL-JIHI-TAX
      *
           .
      *
       200111-JIHI-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    金額編集処理
      *****************************************************************
       2001111-KINGAKU-HEN-SEC         SECTION.
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    WRK-KINGAKKU-LBL    TO   WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
           MOVE    WRK-KINGAKKU    TO  WRK-STR-LEFT-ZX99
      *
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
                   "円"                DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           MOVE    "5"                     TO  WRK-ITEM-KBN
           MOVE    ZERO                    TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
           .
      *
       2001111-KINGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *   検査数  編集処理
      *****************************************************************
       200111-KENSASU-HEN-SEC          SECTION.
      *
           IF    ( WRK-TNS-HOUKNSKBN   >=  1 )
            AND  ( WRK-TNS-HOUKNSKBN   <=  8 )
               MOVE    CNT-HOUSRYACT   TO  WRK-STR-LEFT-ZX99
               PERFORM 800-STR-HIDARIZUME-HEN-SEC
               MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    SPACE           TO  WRK-SRYACT-STR
               STRING  CONST-KENSACOMMENT (WRK-TNS-HOUKNSKBN)
                                       DELIMITED  BY  SPACE
                       "（"            DELIMITED  BY  SIZE
                       WRK-OUT-INPUT   DELIMITED  BY  SPACE
                       "項目）"        DELIMITED  BY  SIZE
               INTO    WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
           END-IF
      *
           .
       200111-KENSASU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する 
      *****************************************************************
       2001111-PTCOM-SET-SEC               SECTION.
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SRY-HOSPID          TO  PTCOM-HOSPID
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD   (IDX9)  TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX9)  TO  PTCOM-RENNUM
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               CONTINUE
           ELSE
               MOVE    MCPDATA-REC     TO  PTCOM-REC
               IF    ( PTCOM-INPUTCOMENT   NOT =   SPACE )
                   MOVE    PTCOM-INPUTCOMENT
                                       TO  WRK-SRYACT-STR
               END-IF
           END-IF
      *
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       2001111-PTCOM-SET-EXT.
           EXIT.
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       2001111-SURYO-HEN-SEC           SECTION.
      *
           MOVE    SRY-SRYSURYO (IDX9) TO  WRK-SURYO
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           MOVE    ZERO                TO  WRK-SURYO-Z3
           PERFORM VARYING   IDX10     FROM    3  BY  -1
                   UNTIL   ( IDX10     <   1 )
      *
               IF    ( WRK-SURYO-S2(IDX10:1)   NOT =   ZERO )
                   MOVE    1           TO  FLG-SP
               END-IF
      *
               IF    ( FLG-SP          =   1 )
                   MOVE    WRK-SURYO-S2(IDX10:1)
                                       TO  WRK-SURYO-Z3(IDX10:1)
               END-IF
           END-PERFORM
      *
           IF    ( WRK-SURYO-Z3-X      NOT =   SPACE )
               MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z19
               MOVE    "."             TO  WRK-SURYO-Z2
           END-IF
      *
           IF    ( WRK-SURYO-X NOT =   SPACE )
               IF    ( WRK-SURYO-X (1:1)   =   SPACE )
                   MOVE    SPACE       TO  WRK-STR
                   MOVE    1           TO  WRK-PNT
                   UNSTRING    WRK-SURYO-X
                   DELIMITED   BY  ALL SPACE
                   INTO        WRK-STR
                   WITH        POINTER WRK-PNT
                   END-UNSTRING
               ELSE
                   MOVE    1           TO  WRK-PNT
               END-IF
               MOVE    WRK-SURYO-X (WRK-PNT : )
                                       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    SPACE           TO  WRK-STR
               STRING  WRK-SRYACT-STR      DELIMITED   BY  SPACE
                   "　"                    DELIMITED   BY  SIZE
                   WRK-OUT-INPUT           DELIMITED   BY  SPACE
                   WRK-TANINAME            DELIMITED   BY  SPACE
               INTO    WRK-STR
               END-STRING
               MOVE    WRK-STR         TO  WRK-SRYACT-STR
           END-IF
           .
       2001111-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    分画数編集処理
      *****************************************************************
       2001111-BUNGASU-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-BUNGSU
           MOVE    1                   TO  WRK-PNT
           MOVE    SRY-SRYSURYO (IDX9) TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
           INTO    WRK-BUNGSU
           WITH    POINTER WRK-PNT
           END-STRING
           MOVE    SRY-SRYKAISU (IDX9) TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  "/"                 DELIMITED   BY  SIZE
                   WRK-STR-LEFT        DELIMITED   BY  SPACE
           INTO    WRK-BUNGSU
           WITH    POINTER WRK-PNT
           END-STRING
           MOVE    WRK-BUNGSU          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           STRING  WRK-SRYACT-STR      DELIMITED   BY  SPACE
               "　"                    DELIMITED   BY  SIZE
               WRK-OUT-INPUT           DELIMITED   BY  SPACE
           INTO    WRK-STR
           END-STRING
           MOVE    WRK-STR         TO  WRK-SRYACT-STR
      *
           .
       2001111-BUNGASU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    あふれ情報編集処理
      *****************************************************************
       20011-AFURE-HEN-SEC             SECTION.
      *
      *    保険組合せ欄からあふれた分について診療行為欄に編集を行う
           MOVE    ZERO                    TO  FLG-OVER
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKHKNCOMBI-MAX )
               PERFORM 200111-HKNCOMBI-RANGAI-HEN-SEC
           END-PERFORM
      *
      *    保険公費欄からあふれた分について診療行為欄に編集を行う
      *    （保険）
           MOVE    ZERO                    TO  FLG-OVER
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKPTHKNINF-MAX )
               PERFORM 200111-PTHKN-RANGAI-HEN-SEC
           END-PERFORM
      *
      *    保険公費欄からあふれた分について診療行為欄に編集を行う
      *    （公費）
           MOVE    ZERO                    TO  FLG-OVER
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKPTKOHINF-MAX )
               PERFORM 200111-PTKOH-RANGAI-HEN-SEC
           END-PERFORM
      *
      *    傷病名欄からあふれた分について診療行為欄に編集を行う
           MOVE    ZERO                    TO  FLG-OVER
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKPTBYOMEI-MAX )
               PERFORM 200111-PTBYOMEI-RANGAI-HEN-SEC
           END-PERFORM
      *
           .
       20011-AFURE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ欄外編集処理（診療行為欄に編集）
      *****************************************************************
       200111-HKNCOMBI-RANGAI-HEN-SEC   SECTION.
      *
           IF    ( FLG-OVER        =   ZERO )
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "以下保険組合せあふれ分"
                                           TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    1                   TO  FLG-OVER
               MOVE    SPACE               TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
      *    保険組合せ番号全角変換
           MOVE    WKHKNCOMBI-HKNCOMBINUM (IDX1)
                                           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-HKNCOMBINUM-N
      *
      *    適用開始日和暦変換／全角変換
           MOVE    WKHKNCOMBI-TEKSTYMDW(IDX1)
                                           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-TEKSTYMD-N
      *
           MOVE    WKHKNCOMBI-TEKEDYMDW(IDX1)
                                           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-TEKEDYMD-N
      *
           MOVE    WKHKNCOMBI-HKNCOMBIMEI (IDX1)
                                           TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN      <   1 )
               MOVE    1                   TO  WRK-STRLEN
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE        SPACE               TO  WRK-SRYACT-STR
           STRING  WRK-HKNCOMBINUM-N   DELIMITED   BY  SIZE
           "　"                DELIMITED   BY  SIZE
           WKHKNCOMBI-HKNCOMBIMEI (IDX1) (1 : WRK-STRLEN )
                                       DELIMITED   BY  SIZE
           INTO    WRK-SRYACT-STR
           END-STRING
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           MOVE        SPACE               TO  WRK-SRYACT-STR
           STRING  WRK-TEKSTYMD-N      DELIMITED   BY  SIZE
                   "〜"                DELIMITED   BY  SIZE
                   WRK-TEKEDYMD-N      DELIMITED   BY  SIZE
           INTO    WRK-SRYACT-STR
           END-STRING
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           MOVE    "1"                     TO  WRK-ITEM-KBN
           MOVE    IDX1                    TO  IDXD
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       200111-HKNCOMBI-RANGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険欄外編集処理（診療行為欄に編集）
      *****************************************************************
       200111-PTHKN-RANGAI-HEN-SEC     SECTION.
      *
           MOVE    WKPTHKNINF-OCC (IDX1)   TO  PTHKNINF-REC
      *
           IF    ( FLG-OVER        =   ZERO )
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "以下保険あふれ分"
                                           TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    1                   TO  FLG-OVER
               MOVE    SPACE               TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    PTHKN-HKNNUM            TO  SRH-HKNNUM
           PERFORM 200111-PTHKN-HKNNUM-HEN-SEC
           MOVE    SRH-HKNNUMMEI (1:10)    TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    PTHKN-HKNJANUM          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT (1:16)    TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    PTHKN-HONKZKKBN         TO  WRK-PTHKN-HONKZKKBN
           PERFORM 200111-PTHKN-HONKZKKBN-HEN-SEC
           MOVE    WRK-PTHKN-HONKZKKBNMEI (1:4)
                                           TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    PTHKN-CONTKBN           TO  WRK-PTHKN-CONTKBN
           PERFORM 200111-PTHKN-CONTKBN-HEN-SEC
           MOVE    WRK-PTHKN-CONTKBNMEI   (1:4)
                                           TO  WRK-SRYACT-STR
           IF    ( WRK-SRYACT-STR  NOT =   SPACE )
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           IF    ( PTHKN-KIGO      NOT =   SPACE )
               MOVE    SPACE               TO  WRK-SRYACT-STR
               STRING    "記："                DELIMITED   BY  SIZE
                         PTHKN-KIGO            DELIMITED   BY  SPACE
               INTO      WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           IF    ( PTHKN-NUM       NOT =   SPACE )
               MOVE    SPACE               TO  WRK-SRYACT-STR
               STRING    "番："                DELIMITED   BY  SIZE
                         PTHKN-NUM             DELIMITED   BY  SPACE
               INTO      WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           MOVE    "2"                     TO  WRK-ITEM-KBN
           MOVE    IDX1                    TO  IDXD
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       200111-PTHKN-RANGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費欄外編集処理（診療行為欄に編集）
      *****************************************************************
       200111-PTKOH-RANGAI-HEN-SEC     SECTION.
      *
           MOVE    WKPTKOHINF-OCC (IDX1)   TO  PTKOHINF-REC
      *
           IF    ( FLG-OVER        =   ZERO )
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "以下公費あふれ分"
                                           TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    1                   TO  FLG-OVER
               MOVE    SPACE               TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    PTKOH-KOHNUM            TO  SRH-KOHNUM
           MOVE    PTKOH-PAYKBN            TO  SRH-PAYKBN
           PERFORM 200111-PTKOH-KOHNUM-HEN-SEC
           MOVE    SRH-KOHNUMMEI (1:10)
                                           TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    PTKOH-FTNJANUM          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-PTKOH-FTNJANUM-N
      *
           MOVE    PTKOH-JKYSNUM           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-PTKOH-JKYSNUM-N
      *
           IF    ( WRK-PTKOH-FTNJANUM-N    NOT =   SPACE )
               MOVE    SPACE               TO  WRK-SRYACT-STR
               STRING    "負："                DELIMITED   BY  SIZE
                         WRK-PTKOH-FTNJANUM-N  DELIMITED   BY  SPACE
               INTO      WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           IF    ( WRK-PTKOH-JKYSNUM-N     NOT =   SPACE )
               MOVE    SPACE               TO  WRK-SRYACT-STR
               STRING    "受："                DELIMITED   BY  SIZE
                         WRK-PTKOH-JKYSNUM-N   DELIMITED   BY  SPACE
               INTO      WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           MOVE    "3"                     TO  WRK-ITEM-KBN
           MOVE    IDX1                    TO  IDXD
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       200111-PTKOH-RANGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    傷病名欄外編集処理（診療行為欄に編集）
      *****************************************************************
       200111-PTBYOMEI-RANGAI-HEN-SEC  SECTION.
      *
           MOVE    WKPTBYOMEI-OCC (IDX1)   TO  PTBYOMEI-REC
      *
           IF    ( FLG-OVER        =   ZERO )
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "以下傷病名あふれ分"
                                           TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    1                   TO  FLG-OVER
               MOVE    SPACE               TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    PTBYO-BYOMEI            TO  WRK-PTBYO-BYOMEI
           MOVE    PTBYO-BYOMEIMOJI        TO  WRK-PTBYO-BYOMEIMOJI
           MOVE    PTBYO-UTAGAIFLG         TO  WRK-PTBYO-UTAGAIFLG
           PERFORM 20011-PTBYOMEI-BYOMEI-HEN-SEC
           MOVE    WRK-PTBYO-BYOMEI        TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    SPACE                   TO  WRK-SRYACT-STR
           MOVE    1                       TO  WRK-PNT
      *
           MOVE    PTBYO-SRYKA             TO  SRH-SRYKA
           PERFORM 200111-SRYKA-HEN-SEC
           IF    ( SRH-SRYKAMEI2      NOT  =   SPACE )
               STRING  SRH-SRYKAMEI2(1:4)  DELIMITED   BY  SIZE
                       "　"                DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           ELSE
               STRING  "　　　"            DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           IF    ( PTBYO-SYUBYOFLG         =   "1" )
               STRING  "主"                DELIMITED   BY  SIZE
                       "　"                DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           ELSE
               STRING  "　　"              DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           MOVE    PTBYO-SRYYMD            TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1             TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           IF    ( WRK-OUT-INPUT       NOT =   SPACE )
               STRING  WRK-OUT-INPUT (1:18)
                                           DELIMITED   BY  SIZE
                       "　"                DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           ELSE
               STRING  "　　　　　　　　　　　　　　　　　　　"
                                           DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           MOVE    PTBYO-TENKIKBN          TO  WRK-PTBYO-TENKIKBN
           PERFORM 200111-PTBYO-TENKIKBN-HEN-SEC
           IF    ( WRK-PTBYO-TENKIKBNMEI    NOT =   SPACE )
               STRING  WRK-PTBYO-TENKIKBNMEI (1:4)
                                           DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           ELSE
               STRING  "　　"              DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           MOVE    "4"                     TO  WRK-ITEM-KBN
           MOVE    IDX1                    TO  IDXD
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       200111-PTBYOMEI-RANGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴編集処理
      *****************************************************************
       2002-NYUINRRK-HEN-SEC           SECTION.
      *
           PERFORM 20021-SHONUM-GET-SEC
           IF    ( WRK-SHONUM-MAX      =   ZERO )
               GO  TO  2002-NYUINRRK-HEN-EXT
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    ZERO                TO  CNT-PTNYUINRRK
      *
           PERFORM VARYING IDX22   FROM    SNTYMD-MAX   BY  -1
                   UNTIL ( IDX22       <   1 )
                    OR   ( SNTYMD-NYUINYMD (IDX22) (1:6)
                                       >   ORCHCM34-SRYYM )
      *
               PERFORM VARYING IDX18   FROM    1   BY  1
                       UNTIL ( IDX18   >   WRK-SHONUM-MAX )
                   IF    ( SNTYMD-SHONUM (IDX22)
                                       =   WRK-SHONUM (IDX18))
                       PERFORM 20021-NYUINRRK-HEN-SEC
                       MOVE    WRK-SHONUM-MAX  TO  IDX18
                   END-IF
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "6"                     TO  WRK-ITEM-KBN
           MOVE    1                       TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       2002-NYUINRRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回番号取得処理
      *****************************************************************
       20021-SHONUM-GET-SEC            SECTION.
      *
           INITIALIZE                      WRK-SHONUM-G
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    ORCHCM34-SRYYM      TO  WRK-SYMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD            TO  PTNYUINRRK-NYUINYMD
           MOVE    ORCHCM34-SRYYM      TO  PTNYUINRRK-TAIINYMD
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY24"  TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC            NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               PERFORM VARYING IDX18   FROM    1   BY  1
                       UNTIL ( IDX18   >   WRK-SHONUM-MAX )
                         OR  ( IDX18   >   20 )
                         OR  ( PTNYUINRRK-SHONUM
                                       =   WRK-SHONUM (IDX18))
                   CONTINUE
               END-PERFORM
      *
               IF    ( IDX18   >   WRK-SHONUM-MAX )
                   COMPUTE WRK-SHONUM-MAX  =   WRK-SHONUM-MAX  +   1
                   IF    ( WRK-SHONUM-MAX  >   20 )
                       MOVE    20          TO  WRK-SHONUM-MAX
                   ELSE
                       MOVE    PTNYUINRRK-SHONUM
                                           TO  WRK-SHONUM 
                                                       (WRK-SHONUM-MAX)
                   END-IF
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY24"  TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC            NOT =   ZERO )
                   MOVE    1               TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               END-IF
           END-PERFORM
      *
           MOVE    "PTNYUINRRK-KEY24"  TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       20021-SHONUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴編集処理
      *****************************************************************
       20021-NYUINRRK-HEN-SEC          SECTION.
      *
           COMPUTE CNT-PTNYUINRRK  =   CNT-PTNYUINRRK  +   1
      *
           MOVE    SPACE               TO  WRK-RRK-NYUINYMD
           MOVE    SPACE               TO  WRK-RRK-TAIINYMD
      *
           MOVE    SNTYMD-NYUINYMD (IDX22)
                                       TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3 (5:18)  TO  WRK-RRK-NYUINYMD
      *
           IF    ( SNTYMD-TAIINYMD (IDX22) NOT =   "99999999" )
            AND  ( SNTYMD-TAIINYMD (IDX22) NOT =   SPACE )
               MOVE    SNTYMD-TAIINYMD (IDX22)
                                           TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD3 (5:18)  TO  WRK-RRK-TAIINYMD
           END-IF
      *
           MOVE    SPACE                   TO  WRK-SRYACT-STR
           IF    ( CNT-PTNYUINRRK  =   1 )
               STRING  "入退院履歴　"      DELIMITED   BY  SIZE
                       WRK-RRK-NYUINYMD    DELIMITED   BY  SIZE
                       "〜"                DELIMITED   BY  SIZE
                       WRK-RRK-TAIINYMD    DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               END-STRING
           ELSE
               STRING  "　　　　　　"      DELIMITED   BY  SIZE
                       WRK-RRK-NYUINYMD    DELIMITED   BY  SIZE
                       "〜"                DELIMITED   BY  SIZE
                       WRK-RRK-TAIINYMD    DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               END-STRING
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           .
       20021-NYUINRRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計保険組合せ取得処理
      *****************************************************************
       2002-NACCT-HKNCOMBI-GET-SEC     SECTION.
      *
      *    入院会計マスタより保険組み合わせを取得
           INITIALIZE                          WRK-NACCT-HKNCOMBI-G
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    SPA-HOSPID              TO  NACCT-HOSPID
           MOVE    SPA-PTID                TO  NACCT-PTID
           MOVE    ORCHCM34-SRYYM          TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC            NOT =   ZERO )
               MOVE    1                   TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           END-IF
      *
           MOVE    "NYUINACCT-KEY17"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               GO  TO  2002-NACCT-HKNCOMBI-GET-EXT
           END-IF
      *
           PERFORM VARYING     IDX14   FROM    1   BY  1
                   UNTIL     ( IDX14   >   31 )
               IF    ( NACCT-DAY (IDX14)   NOT =   ZERO )
      *
                   PERFORM VARYING IDX15   FROM    1   BY  1
                       UNTIL     ( IDX15   >   WRK-NACCT-HKNCOMBI-MAX )
                        OR       ( IDX15   >   20 )
                        OR       ( NACCT-DAY (IDX14)
                                       =   WRK-NACCT-HKNCOMBI (IDX15) )
                       CONTINUE
                   END-PERFORM
      *
                   IF    (   IDX15         >   WRK-NACCT-HKNCOMBI-MAX )
                       COMPUTE WRK-NACCT-HKNCOMBI-MAX
                           =   WRK-NACCT-HKNCOMBI-MAX  +   1
                       IF  ( WRK-NACCT-HKNCOMBI-MAX    >   20 )
                           MOVE    20      TO  WRK-NACCT-HKNCOMBI-MAX
                       ELSE
                           MOVE    WRK-NACCT-HKNCOMBI-MAX
                                           TO  IDX15
                           MOVE    NACCT-DAY (IDX14)
                                           TO  WRK-NACCT-HKNCOMBI(IDX15)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    入院会計保険組合せワークについて並び替えを行う
           PERFORM VARYING IDX14    FROM    2   BY  1
                   UNTIL ( IDX14    >   WRK-NACCT-HKNCOMBI-MAX )
               COMPUTE WRK-NACCT-HKNCOMBI-STIDX    =   IDX14    -   1
               MOVE    WRK-NACCT-HKNCOMBI-OCC (IDX14)
                                   TO  WRK-NACCT-HKNCOMBI-OCC (21)
               PERFORM VARYING IDX15
                       FROM    WRK-NACCT-HKNCOMBI-STIDX    BY  -1
                       UNTIL ( IDX15    <   1 )
                          OR ( WRK-NACCT-HKNCOMBI-KEY (21)
                                   >=  WRK-NACCT-HKNCOMBI-KEY(IDX15) )
                   MOVE    WRK-NACCT-HKNCOMBI-OCC (IDX15)
                                   TO  WRK-NACCT-HKNCOMBI-OCC(IDX15 + 1)
               END-PERFORM
               MOVE    WRK-NACCT-HKNCOMBI-OCC (21)
                                   TO  WRK-NACCT-HKNCOMBI-OCC(IDX15 + 1)
           END-PERFORM
      *
           .
       2002-NACCT-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為チェック処理
      *****************************************************************
       2002-NSRYACT-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCTCHK-EXT
           PERFORM UNTIL ( WRK-NACCT-HKNCOMBI-IDX
                                       >   WRK-NACCT-HKNCOMBI-MAX )
                     OR  ( FLG-NACCTCHK-EXT
                                   NOT =   ZERO )
      *
               MOVE    WRK-NACCT-HKNCOMBI-IDX  TO  IDXG
      *
               IF    ( WRK-HKNCOMBI-KEY    >   WRK-NACCT-HKNCOMBI-X
                                                               (IDXG))
                   IF    ( WRK-HKNCOMBI-KEY-MAE
                                           =   WRK-NACCT-HKNCOMBI-X
                                                               (IDXG))
      *                空行を１行編集
                       PERFORM 2010-KARAGYO-HEN-SEC
      *
                   ELSE
      *
      *                区切り線“−”を編集
                       MOVE    1           TO  FLG-LINE
                       PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
                       MOVE    "5"         TO  WRK-ITEM-KBN
                       PERFORM 210-SRYACT-HEN-SEC
                   END-IF
      *
      *            入院診療行為編集処理
                   PERFORM 20021-NSRYACT-HEN-SEC
      *
                   COMPUTE WRK-NACCT-HKNCOMBI-IDX
                       =   WRK-NACCT-HKNCOMBI-IDX  +   1
      *
               ELSE
                   MOVE    1               TO  FLG-NACCTCHK-EXT
               END-IF
           END-PERFORM
      *
           .
       2002-NSRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為編集処理
      *****************************************************************
       20021-NSRYACT-HEN-SEC           SECTION.
      *
           INITIALIZE                        HCM34S01-AREA
                                             WRK-SPA-AREA
      *
           MOVE    SPA-HOSPID          TO    HCM34S01-HOSPID
           MOVE    SPA-PTID            TO    HCM34S01-PTID
           MOVE    ORCHCM34-SRYYM      TO    HCM34S01-SANTEIYM
           MOVE    WRK-NACCT-HKNCOMBI-X (IDXG)
                                       TO    HCM34S01-HKNCOMBI
                                             WRK-HKNCOMBINUM
           PERFORM 200111-HKNCOMBI-SEL-SEC
      *
           MOVE    COMB-HKNNUM         TO    HCM34S01-HKNNUM
      *
           IF    ( COMB-HKNNUM         =     "971" OR "973" )
      *
      *        労災自賠入院会計編集サブ
               CALL    "ORCHCM34S02"       USING HCM34S01-AREA
                                                 WRK-SPA-AREA
           ELSE
      *        医療入院会計編集サブ
               CALL    "ORCHCM34S01"       USING HCM34S01-AREA
                                                 WRK-SPA-AREA
           END-IF
      *
           MOVE    ZERO                TO  FLG-BRMSAGAKU-HEN
           PERFORM VARYING IDXH    FROM    1   BY  1
                   UNTIL ( IDXH    >   HCM34S01-HENKYAKU-MAX )
      *
               IF    ( HCM34S01-ZAISKBKBN (IDXH)   =   "3" )
                 AND ( FLG-BRMSAGAKU-HEN           =   ZERO )
      *
      *            空行を１行編集
                   PERFORM 2010-KARAGYO-HEN-SEC
      *
                   MOVE    1           TO  FLG-BRMSAGAKU-HEN
      *
               END-IF
      *
      *        点数検索用日付編集
               MOVE    ORCHCM34-SRYYM  TO  WRK-SRYYMD(1:6)
               MOVE    01              TO  WRK-SRYDD
               PERFORM VARYING     IDX7    FROM    1   BY  1
                   UNTIL     ( IDX7    >   31 )
                   IF    ( HCM34S01-DAY (IDXH IDX7)    NOT =   ZERO )
                       MOVE    IDX7        TO  WRK-SRYDD
                       MOVE    31          TO  IDX7
                   END-IF
               END-PERFORM
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
      *        剤内１行目
               PERFORM 200211-GYO1-HEN-SEC
      *
               MOVE    ZERO            TO  FLG-MINUS
               PERFORM VARYING IDXI    FROM    1   BY  1
                       UNTIL ( IDXI                >   20 )
                        OR   ( WRK-SRYACT-MAX      >   500 )
                        OR   ( HCM34S01-SRYCD (IDXH IDXI)
                                                   =   SPACE )
      *
                   IF    ( COMB-HKNNUM         NOT =     "971" )
                    AND  ( COMB-HKNNUM         NOT =     "973" )
                       IF    ( HCM34S01-SRYCD (IDXH IDXI)
                                                   =   CONST-GENZAN1
                                                   OR  CONST-GENZAN2
                                                   OR  CONST-GENZAN3
                                                   OR  CONST-GENZAN4
                                                   OR  CONST-GENZAN5
                                                   OR  CONST-GENZAN6
                                                   OR  CONST-GENZAN7
                                                   OR  CONST-GENZAN8 )
                           MOVE    1       TO  FLG-MINUS
                       END-IF
                   END-IF
      *
                   PERFORM 200211-ZAIMEI-HEN-SEC
      *
               END-PERFORM
      *
               EVALUATE    HCM34S01-ZAISKBKBN (IDXH)
               WHEN    "1"
               WHEN    "4"
               WHEN    "6"
                   MOVE    HCM34S01-ZAITEN  (IDXH)  TO  WRK-ZAITEN
                   MOVE    HCM34S01-ZAIKAISU(IDXH)  TO  WRK-ZAIKAISU
                   PERFORM 205-TENSUKAISU-HEN-SEC
               WHEN    "2"
                   PERFORM 200211-NISSU-HEN-SEC
                   MOVE    HCM34S01-ZAITEN  (IDXH)  TO  WRK-ZAITEN
                   MOVE    HCM34S01-ZAIKAISU(IDXH)  TO  WRK-ZAIKAISU
                   PERFORM 205-TENSUKAISU-HEN-SEC
               WHEN    "3"
      *
                   COMPUTE WRK-TTL-BRMSAGAKU
                       =   WRK-TTL-BRMSAGAKU
                       + ( HCM34S01-ZAITEN (IDXH)
                       *   HCM34S01-ZAIKAISU (IDXH) )
      *
                   PERFORM 200211-NISSU-HEN-SEC
                   MOVE    HCM34S01-ZAITEN  (IDXH)  TO  WRK-ZAITEN
                   MOVE    HCM34S01-ZAIKAISU(IDXH)  TO  WRK-ZAIKAISU
                   PERFORM 205-KINGAKUKAISU-HEN-SEC
      *
               END-EVALUATE
      *
               MOVE    "5"                 TO  WRK-ITEM-KBN
               MOVE    ZERO                TO  FLG-LINE
               PERFORM 210-SRYACT-HEN-SEC
      *
           END-PERFORM
      *
           PERFORM 200211-BRMSAGAKU-TOTAL-HEN-SEC
      *
           PERFORM 200211-SENTEI-PTFTN-HEN-SEC
      *
           .
       20021-NSRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       200211-GYO1-HEN-SEC             SECTION.
      *
      *    診療区分
           IF    ( HCM34S01-ZAISKBKBN (IDXH)   NOT =   "3" )
               MOVE    HCM34S01-SRYKBN (IDXH)  TO  WRK-SRYACT-SRYKBN
           END-IF
      *
      *    保険組合せ
           MOVE    WRK-NACCT-HKNCOMBI-X (IDXG) TO  WRK-SRYACT-HKNCOMBI
      *    回数
           PERFORM VARYING IDXJ    FROM    1   BY  1
                   UNTIL ( IDXJ    >   31 )
               MOVE    HCM34S01-DAY (IDXH IDXJ)
                                       TO  WRK-SRYACT-DAY-Z (IDXJ)
           END-PERFORM
      *
           .
       200211-GYO1-HEN-EXT.
           EXIT.
      *****************************************************************
      *   剤名編集処理
      *****************************************************************
       200211-ZAIMEI-HEN-SEC           SECTION.
      *
           PERFORM 2002111-ZAIMEI-HEN-SEC
      *
           .
       200211-ZAIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *   剤名編集処理
      *****************************************************************
       2002111-ZAIMEI-HEN-SEC          SECTION.
      *
      *    診療コードより診療行為名称を取得
           INITIALIZE  TNS-TENSU-REC
           MOVE    HCM34S01-SRYCD (IDXH IDXI)
                                           TO  TNS-SRYCD
           MOVE    ZERO                    TO  TNS-YUKOSTYMD
           MOVE    ALL "9"                 TO  TNS-YUKOEDYMD
           MOVE    WRK-SRYYMD              TO  ORC-DBYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
               MOVE    HCM34S01-SRYCD (IDXH IDXI)
                                           TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
           ELSE
      *
               MOVE    TNS-NAME            TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
      *
           END-IF
      *
      *    行為名称
           MOVE    TNS-NAME                TO  WRK-SRYACT-STR
      *
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           .
       2002111-ZAIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *   日数編集処理
      *****************************************************************
       200211-NISSU-HEN-SEC                SECTION.
      *
           MOVE    1                   TO  WRK-GAIHAKU-ST
           PERFORM UNTIL ( WRK-GAIHAKU-ST  >   31 )
               IF    ( HCM34S01-DAY (IDXH WRK-GAIHAKU-ST )
                                       NOT =   ZERO )
      *
                   MOVE    SPACE       TO  WRK-SRYACT-STR
      *
                   MOVE    ZERO        TO  CNT-RENPAKU
                   COMPUTE WRK-GAIHAKU-ED  =  WRK-GAIHAKU-ST   +   1
                   PERFORM UNTIL ( WRK-GAIHAKU-ED  >   31 )
                            OR   ( HCM34S01-DAY (IDXH WRK-GAIHAKU-ED )
                                           =   ZERO )
                       COMPUTE WRK-GAIHAKU-ED  =  WRK-GAIHAKU-ED  +   1
                       COMPUTE CNT-RENPAKU     =  CNT-RENPAKU     +   1
                   END-PERFORM
                   MOVE    SPACE       TO  WRK-SRYACT-STR
                   IF    ( CNT-RENPAKU >   ZERO )
                       COMPUTE WRK-GAIHAKU-ED  =  WRK-GAIHAKU-ED  -   1
                       MOVE    WRK-GAIHAKU-ST  TO WRK-GAIHAKU-ST-Z9
                       MOVE    WRK-GAIHAKU-ED  TO WRK-GAIHAKU-ED-Z9
                       STRING  " "                 DELIMITED   BY  SIZE
                               WRK-GAIHAKU-ST-Z9   DELIMITED   BY  SIZE
                               "日"                DELIMITED   BY  SIZE
                               "〜"                DELIMITED   BY  SIZE
                               WRK-GAIHAKU-ED-Z9   DELIMITED   BY  SIZE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-SRYACT-STR
                       END-STRING
                       COMPUTE WRK-GAIHAKU-ST
                           =   WRK-GAIHAKU-ED      +   1
                   ELSE
                       MOVE    WRK-GAIHAKU-ST  TO WRK-GAIHAKU-ST-Z9
                       STRING  " "                 DELIMITED   BY  SIZE
                               WRK-GAIHAKU-ST-Z9   DELIMITED   BY  SIZE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-SRYACT-STR
                       END-STRING
                       COMPUTE WRK-GAIHAKU-ST
                           =   WRK-GAIHAKU-ST      +   1
                   END-IF
      *
                   MOVE    WRK-SRYACT-STR  TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
      *
                   PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               ELSE
                   COMPUTE WRK-GAIHAKU-ST
                       =   WRK-GAIHAKU-ST      +   1
               END-IF
           END-PERFORM
      *
           .
       200211-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額計編集処理
      *****************************************************************
       200211-BRMSAGAKU-TOTAL-HEN-SEC  SECTION.
      *
           IF    ( WRK-TTL-BRMSAGAKU   >   ZERO )
      *
      *        空行を１行編集
               PERFORM 2010-KARAGYO-HEN-SEC
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               MOVE    "　　　　　室料差額合計額"
                                           TO  WRK-KINGAKKU-LBL
               MOVE    WRK-TTL-BRMSAGAKU   TO  WRK-KINGAKKU
               PERFORM 2001111-KINGAKU-HEN-SEC
      *
               MOVE    ZERO                TO  WRK-TTL-BRMSAGAKU-TAX
      *
      *        消費税表示編集判定
               IF    ( SYS-5000-BRMSGK-TAXKBN  =   "1" )
                   COMPUTE    WRK-TTL-BRMSAGAKU-TAX
                       =    ( WRK-TTL-BRMSAGAKU    *   0.05 )
                   IF    ( WRK-TTL-BRMSAGAKU-TAX   >   ZERO )
                       MOVE    "　　　　　室料差額消費税額"
                                           TO  WRK-KINGAKKU-LBL
                       MOVE    WRK-TTL-BRMSAGAKU-TAX
                                           TO  WRK-KINGAKKU
                       PERFORM 2001111-KINGAKU-HEN-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           MOVE    ZERO                TO  WRK-TTL-BRMSAGAKU
      *
           .
      *
       200211-BRMSAGAKU-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料患者負担額編集処理
      *****************************************************************
       200211-SENTEI-PTFTN-HEN-SEC     SECTION.
      *
           IF    ( HCM34S01-SENTEI-PTFTN   >   ZERO )
      *
      *        空行を１行編集
               PERFORM 2010-KARAGYO-HEN-SEC
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               MOVE    "　　　　　選定入院料自己負担額"
                                           TO  WRK-KINGAKKU-LBL
               MOVE    HCM34S01-SENTEI-PTFTN
                                           TO  WRK-KINGAKKU
               PERFORM 2001111-KINGAKU-HEN-SEC
      *
           END-IF
      *
           .
      *
       200211-SENTEI-PTFTN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    空行編集処理
      *****************************************************************
       2010-KARAGYO-HEN-SEC            SECTION.
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    "　"                TO   WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           MOVE    "5"                 TO  WRK-ITEM-KBN
           MOVE    ZERO                TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       2010-KARAGYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   回数  編集処理
      *****************************************************************
       205-KAISU-HEN-SEC              SECTION.
      *
           IF    ( WRK-ZAIKAISU   NOT =   ZERO )
               CONTINUE
           ELSE
               GO  TO  205-KAISU-HEN-EXT
           END-IF
      *
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
      *
           MOVE    WRK-ZAIKAISU    TO  WRK-ZZ9
           STRING  "　"                DELIMITED   BY  SIZE
                   "×"                DELIMITED   BY  SIZE
                   WRK-ZZ9             DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           .
       205-KAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *   回数  編集処理
      *****************************************************************
       206-KAISU-HEN-SEC              SECTION.
      *
           IF    ( WRK-ZAIKAISU   NOT =   ZERO )
               CONTINUE
           ELSE
               GO  TO  206-KAISU-HEN-EXT
           END-IF
      *
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
      *
           MOVE    WRK-ZAIKAISU    TO  WRK-ZZ9
           STRING  "×"                DELIMITED   BY  SIZE
                   WRK-ZZ9             DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           .
       206-KAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *   点数回数  編集処理
      *****************************************************************
       205-TENSUKAISU-HEN-SEC              SECTION.
      *
           IF    ( WRK-ZAIKAISU   NOT =   ZERO )
               CONTINUE
           ELSE
               GO  TO  205-TENSUKAISU-HEN-EXT
           END-IF
      *
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
           MOVE    WRK-ZAITEN      TO  WRK-STR-LEFT-ZX99
      *
           IF    ( FLG-MINUS       NOT =   ZERO )
               STRING  "▲"            DELIMITED   BY  SIZE
               INTO    WRK-TENSUKAISU
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-ZAIKAISU    TO  WRK-ZZ9
           STRING  "×"                DELIMITED   BY  SIZE
                   WRK-ZZ9             DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           .
       205-TENSUKAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *   金額回数  編集処理
      *****************************************************************
       205-KINGAKUKAISU-HEN-SEC     SECTION.
      *
           IF    ( WRK-ZAIKAISU        NOT =   ZERO )
               CONTINUE
           ELSE
               GO  TO  205-KINGAKUKAISU-HEN-EXT
           END-IF
      *
      *    計
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
           MOVE    WRK-ZAITEN
                                   TO  WRK-STR-LEFT-ZX99
      *
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
                   "円"                DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-ZAIKAISU
                                   TO  WRK-ZZ9
           STRING  "×"                DELIMITED   BY  SIZE
                   WRK-ZZ9             DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           .
       205-KINGAKUKAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル初期処理
      *****************************************************************
       210-WRK-SRYACT-TBL-SYOKI-SEC    SECTION.
      *
           INITIALIZE  WRK-SRYACT-AREA
           MOVE    1                       TO  WRK-SRYACT-MAX
           MOVE    1                       TO  WRK-SRYACT-PNT
           MOVE    ZERO                    TO  FLG-ZENSP
      *
           .
       210-WRK-SRYACT-TBL-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為ワークテーブル編集処理
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN-SEC      SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >=  500 )
               GO  TO  210-WRK-SRYACT-TBL-HEN-EXT
           END-IF
      *
           IF    ( WRK-SRYACT-PNT  <=  1 )
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  -   1
           END-IF
      *
           MOVE    WRK-SRYACT-STR  TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  210-WRK-SRYACT-TBL-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDX6    FROM    1   BY  CONST-SRYACT-LEN
                   UNTIL ( WRK-SRYACT-MAX  >=  500 )
                     OR  ( IDX6    >       WRK-STRLEN )
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
               MOVE    WRK-SRYACT-STR (IDX6:CONST-SRYACT-LEN)
                                   TO  WRK-SRYACT (WRK-SRYACT-MAX)
           END-PERFORM
      *
           MOVE    WRK-SRYACT (WRK-SRYACT-MAX)
                                   TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           COMPUTE WRK-SRYACT-PNT  =   WRK-STRLEN  +   1
      *
           .
       210-WRK-SRYACT-TBL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル編集処理２
      *    （既に編集している項目の後ろに編集）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN2-SEC     SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >=  500 )
               GO  TO  210-WRK-SRYACT-TBL-HEN2-EXT
           END-IF
      *
           MOVE    WRK-SRYACT-STR      TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  210-WRK-SRYACT-TBL-HEN2-EXT
           END-IF
      *
           IF    ( FLG-ZENSP   =   ZERO )
               MOVE    1               TO  FLG-ZENSP
           ELSE
               MOVE    SPACE           TO  WRK-SRYACT-STR
               STRING  "　"                    DELIMITED   BY  SIZE
                       WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               END-STRING
               COMPUTE WRK-STRLEN  =   WRK-STRLEN  +   2
           END-IF
      *
           COMPUTE WRK-PNT     = CONST-SRYACT-LEN
                               * (WRK-SRYACT-MAX   -   1)
                               + WRK-SRYACT-PNT
      *
           STRING  WRK-SRYACT-STR (1:WRK-STRLEN)
                                  DELIMITED   BY  SIZE
           INTO    WRK-SRYACT-TBL
           WITH    POINTER WRK-PNT
           END-STRING
      *
           DIVIDE  WRK-PNT     BY  CONST-SRYACT-LEN
                   GIVING          WRK-SRYACT-MAX
                   REMAINDER       WRK-SRYACT-PNT
      *
           COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
      *
           .
       210-WRK-SRYACT-TBL-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為ワークテーブル編集処理３
      *    （右余白を考慮して編集を行う）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN3-SEC     SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >=  500 )
               GO  TO  210-WRK-SRYACT-TBL-HEN3-EXT
           END-IF
      *
           MOVE    WRK-SRYACT-STR      TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  210-WRK-SRYACT-TBL-HEN3-EXT
           END-IF
      *
           COMPUTE WRK-PNT =   CONST-SRYACT-LEN    -   WRK-STRLEN
           IF    ( CONST-SRYACT-LEN    -  ( WRK-SRYACT-PNT -   1 )
                                       >=  WRK-STRLEN + 2 )
               MOVE    ALL "　"        TO  WRK-STR (1:WRK-PNT)
               STRING  WRK-SRYACT (WRK-SRYACT-MAX)
                                           DELIMITED   BY  SPACE
               INTO    WRK-STR
               END-STRING
               MOVE    1               TO  WRK-SRYACT-PNT
               STRING  WRK-STR (1:WRK-PNT) DELIMITED   BY  SIZE
                       WRK-SRYACT-STR (1:WRK-STRLEN)
                                           DELIMITED   BY  SIZE
               INTO    WRK-SRYACT (WRK-SRYACT-MAX)
               WITH    POINTER WRK-SRYACT-PNT
               END-STRING
           ELSE
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
               MOVE    1               TO  WRK-SRYACT-PNT
               MOVE    ALL "　"        TO  WRK-STR (1:WRK-PNT)
               STRING  WRK-STR (1:WRK-PNT) DELIMITED   BY  SIZE
                       WRK-SRYACT-STR (1:WRK-STRLEN)
                                           DELIMITED   BY  SIZE
               INTO    WRK-SRYACT (WRK-SRYACT-MAX)
               WITH    POINTER WRK-SRYACT-PNT
               END-STRING
           END-IF
      *
           .
       210-WRK-SRYACT-TBL-HEN3-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル編集処理
      *    （検査のまとめ編集）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN4-SEC     SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >=  500 )
               GO  TO  210-WRK-SRYACT-TBL-HEN4-EXT
           END-IF
      *
           IF    ( WRK-SRYACT-PNT  <=  1 )
            AND  ( WRK-SRYACT-MAX   >  1 )
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  -   1
           END-IF
      *
           MOVE    WRK-SRYACT-STR  TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  210-WRK-SRYACT-TBL-HEN4-EXT
           END-IF
      *
           COMPUTE WRK-WIDTH   = ( WRK-SRYACT-PNT  -   1)
                               +   WRK-STRLEN
      *
           EVALUATE    TRUE
           WHEN  ( WRK-STRLEN  >   CONST-SRYACT-LEN)
               PERFORM VARYING IDX6    FROM    1   BY  CONST-SRYACT-LEN
                       UNTIL ( WRK-SRYACT-MAX  >=  500 )
                         OR  ( IDX6    >       WRK-STRLEN )
                   COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
                   MOVE    WRK-SRYACT-STR (IDX6:CONST-SRYACT-LEN)
                                       TO  WRK-SRYACT (WRK-SRYACT-MAX)
               END-PERFORM
               MOVE    WRK-SRYACT (WRK-SRYACT-MAX)
                                       TO  WRK-STR
               PERFORM 800-STRLEN-CHK-SEC
               EVALUATE    TRUE
               WHEN  ( WRK-STRLEN  <=  ZERO )
                   MOVE    1           TO  WRK-SRYACT-PNT
               WHEN
                     ( WRK-STRLEN  >=  CONST-SRYACT-LEN )
                   COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
                   MOVE    1           TO  WRK-SRYACT-PNT
               WHEN    OTHER
                   COMPUTE WRK-SRYACT-PNT  =   WRK-STRLEN      +   1
               END-EVALUATE
           WHEN  ( WRK-WIDTH   >   CONST-SRYACT-LEN )
               COMPUTE WRK-SRYACT-MAX   =   WRK-SRYACT-MAX  +   1
               MOVE    1               TO  WRK-SRYACT-PNT
               STRING  WRK-SRYACT-STR (1:WRK-STRLEN)
                                       DELIMITED   BY  SIZE
               INTO    WRK-SRYACT (WRK-SRYACT-MAX)
               WITH    POINTER WRK-SRYACT-PNT
               END-STRING
           WHEN    OTHER
               STRING  WRK-SRYACT-STR (1:WRK-STRLEN)
                                       DELIMITED   BY  SIZE
               INTO    WRK-SRYACT (WRK-SRYACT-MAX)
               WITH    POINTER WRK-SRYACT-PNT
               END-STRING
           END-EVALUATE
      *
           .
       210-WRK-SRYACT-TBL-HEN4-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル編集処理
      *    （最後のカンマを削る）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN5-SEC     SECTION.
      *
           IF    ( WRK-SRYACT-PNT  <=  1 )
            AND  ( WRK-SRYACT-MAX   >  1 )
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  -   1
           END-IF
      *
           MOVE    WRK-SRYACT (WRK-SRYACT-MAX)
                                       TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  2 )
               MOVE    SPACE           TO  WRK-SRYACT (WRK-SRYACT-MAX)
               MOVE    1               TO  WRK-SRYACT-PNT
               GO  TO  210-WRK-SRYACT-TBL-HEN5-EXT
           END-IF
      *
           MOVE    WRK-SRYACT (WRK-SRYACT-MAX) (1 : WRK-STRLEN - 2)
                                       TO  WRK-STR
           MOVE    WRK-STR             TO  WRK-SRYACT (WRK-SRYACT-MAX)
           COMPUTE WRK-SRYACT-PNT      =   WRK-SRYACT-PNT  -   2
      *
           .
       210-WRK-SRYACT-TBL-HEN5-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       210-SRYACT-HEN-SEC          SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >   ZERO )
               IF    ( WRK-SRYACT-PNT  <=  1 )
                   COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  -   1
               END-IF
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   WRK-SRYACT-MAX )
      *
               COMPUTE CNT-SRYACT-LINE =   CNT-SRYACT-LINE +   1
               MOVE    CNT-SRYACT-LINE     TO  IDXC
               IF    ( FLG-HEAD    =   ZERO )
                   IF    ( WRK-ITEM-KBN            =   "5" )
                    AND  ( WRK-SRYACT-HKNCOMBI NOT =   SPACE )
                       IF    ( IDX4        =   1 )
                           MOVE    WRK-SRYACT-SRYKBN
                                           TO  HCM341-SRYKBN (IDXC)
                           MOVE    WRK-SRYACT-MARK
                                           TO  HCM341-MARK (IDXC)
                       END-IF
                       IF  (   IDXC        =   1 )
                        OR ( ( IDX4        =   1 )
                         AND ( WRK-SRYACT-HKNCOMBI
                                       NOT =   WRK-KEY-HKNCOMBI ) )
                           MOVE    WRK-SRYACT-HKNCOMBI
                                           TO  HCM341-HKNCOMBINUM2
                                                              (IDXC)
                           MOVE    WRK-SRYACT-HKNCOMBI
                                           TO   WRK-KEY-HKNCOMBI
                       END-IF
                       IF  ( IDX4            =   WRK-SRYACT-MAX )
                           PERFORM VARYING IDXF    FROM    1   BY  1
                                   UNTIL ( IDXF    >   31 )
                               MOVE    WRK-SRYACT-DAY-X (IDXF)
                                           TO  HCM341-DAY (IDXC IDXF)
                           END-PERFORM
                       END-IF
                   END-IF
                   MOVE    WRK-SRYACT (IDX4)
                                           TO  HCM341-TEKIYO (IDXC)
               ELSE
                   IF    ( WRK-ITEM-KBN            =   "5" )
                    AND  ( WRK-SRYACT-HKNCOMBI NOT =   SPACE )
                      IF    ( IDX4        =   1 )
                           MOVE    WRK-SRYACT-SRYKBN
                                           TO  HCM342-SRYKBN (IDXC)
                           MOVE    WRK-SRYACT-MARK
                                           TO  HCM342-MARK (IDXC)
                       END-IF
                       IF  (   IDXC        =   1 )
                        OR ( ( IDX4        =   1 )
                         AND ( WRK-SRYACT-HKNCOMBI
                                       NOT =   WRK-KEY-HKNCOMBI ) )
                           MOVE    WRK-SRYACT-HKNCOMBI
                                           TO  HCM342-HKNCOMBINUM2
                                                              (IDXC)
                           MOVE    WRK-SRYACT-HKNCOMBI
                                           TO   WRK-KEY-HKNCOMBI
                       END-IF
                       IF  ( IDX4            =   WRK-SRYACT-MAX )
                           PERFORM VARYING IDXF    FROM    1   BY  1
                                   UNTIL ( IDXF    >   31 )
                               MOVE    WRK-SRYACT-DAY-X (IDXF)
                                           TO  HCM342-DAY (IDXC IDXF)
                           END-PERFORM
                       END-IF
                   END-IF
                   MOVE    WRK-SRYACT (IDX4)
                                           TO  HCM342-TEKIYO (IDXC)
               END-IF
               PERFORM 210-KAIPEGE-CHK-SEC
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-LINECHK
      *
           IF    ( CNT-SRYACT-LINE >   ZERO )
               IF    ( FLG-HEAD        =   ZERO )
                   IF    ( HCM341-TEKIYO (CNT-SRYACT-LINE)
                                               NOT = ALL "−" )
                       MOVE    1       TO  FLG-LINECHK
                   END-IF
               ELSE
                   IF    ( HCM342-TEKIYO (CNT-SRYACT-LINE)
                                               NOT = ALL "−" )
                       MOVE    1       TO  FLG-LINECHK
                   END-IF
               END-IF
            END-IF
      *
           IF    ( FLG-LINECHK         =   1 )
      *
               EVALUATE WRK-ITEM-KBN
                                   ALSO    TRUE
               WHEN     "1"        ALSO  ( IDXD    >=  WKHKNCOMBI-MAX )
               WHEN     "2"        ALSO  ( IDXD    >=  WKPTHKNINF-MAX )
               WHEN     "3"        ALSO  ( IDXD    >=  WKPTKOHINF-MAX )
               WHEN     "4"        ALSO  ( IDXD    >=  WKPTBYOMEI-MAX )
               WHEN     "5"        ALSO  ( FLG-LINE =  1              )
               WHEN     "6"        ALSO  ( FLG-LINE =  1              )
                   COMPUTE CNT-SRYACT-LINE =   CNT-SRYACT-LINE +   1
                   MOVE    CNT-SRYACT-LINE TO  IDXC
                   IF    ( FLG-HEAD    =   ZERO )
                       MOVE    ALL "−"    TO  HCM341-TEKIYO (IDXC)
                   ELSE
                       MOVE    ALL "−"    TO  HCM342-TEKIYO (IDXC)
                   END-IF
                   PERFORM 210-KAIPEGE-CHK-SEC
               END-EVALUATE
      *
           END-IF
      *
           .
       210-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    改ページチェック処理
      *****************************************************************
       210-KAIPEGE-CHK-SEC             SECTION.
      *
           IF   (( FLG-HEAD         =  ZERO )
              AND( CNT-SRYACT-LINE >=  CONST-LINE-MAX ))
            OR  (( FLG-HEAD         =  1 )
              AND( CNT-SRYACT-LINE >=  CONST-ZKSI-LINE-MAX ))
      *
               PERFORM 290-PRINT-SEC
               MOVE    1           TO  FLG-HEAD
               INITIALIZE              HCM342
               MOVE    ZERO        TO  CNT-SRYACT-LINE
               MOVE    ZERO        TO  CNT-YKZINGAI
               MOVE    ZERO        TO  CNT-YKZHKT
           END-IF
      *
           .
       210-KAIPEGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       290-PRINT-SEC               SECTION.
      *
           IF    ( ORCHCM34-KBN   =   "1" )
               IF    ( FLG-HEAD   =   ZERO )
                   PERFORM 2901-HCM341-ONL-PRINT-SEC
               ELSE
                   PERFORM 2901-HCM342-ONL-PRINT-SEC
               END-IF
           ELSE
               IF    ( FLG-HEAD   =   ZERO )
                   PERFORM 2901-HCM341-BAT-PRINT-SEC
               ELSE
                   PERFORM 2901-HCM342-BAT-PRINT-SEC
               END-IF
           END-IF
      *
           .
       290-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    会計カードオンライン印刷処理（頭紙）
      *****************************************************************
       2901-HCM341-ONL-PRINT-SEC       SECTION.
      *
      *    ヘッダー情報編集処理
           PERFORM 29011-HCM341-HEADER-HEN-SEC
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE            TO  HC01PARA-CNT
           COMPUTE HC01PARA-CNT    =   HC01PARA-CNT    *   10
           IF    ( ORCHCM34-KBN        =   "1" )
               COMPUTE HC01PARA-CNT    =   HC01PARA-CNT    +   1
           ELSE
               COMPUTE HC01PARA-CNT    =   HC01PARA-CNT    +   2
           END-IF
           OPEN    OUTPUT              PRT-FILE
      *    印刷用データ出力
           MOVE    HCM341             TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCM341V01.red"     TO  MKPRT-DIA
           MOVE   SPACE               TO  MKPRT-DEF
           MOVE   HC01PARA            TO  MKPRT-DAT
      *    ■暫定
           MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
           CALL   "ORCSMKPRT"         USING
                                      ORCSMKPRTAREA
      *
           .
       2901-HCM341-ONL-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    会計カードオンライン印刷処理（続紙）
      *****************************************************************
       2901-HCM342-ONL-PRINT-SEC       SECTION.
      *
      *    ヘッダー情報編集処理
           PERFORM 29011-HCM342-HEADER-HEN-SEC
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT              PRT-FILE
      *    印刷用データ出力
           MOVE    HCM342             TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCM342V01.red"     TO  MKPRT-DIA
           MOVE   SPACE               TO  MKPRT-DEF
           MOVE   HC01PARA            TO  MKPRT-DAT
      *    ■暫定
           MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
           CALL   "ORCSMKPRT"         USING
                                      ORCSMKPRTAREA
           .
       2901-HCM342-ONL-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    会計カード一括印刷処理（頭紙）
      *****************************************************************
       2901-HCM341-BAT-PRINT-SEC       SECTION.
      *
      *    ヘッダー情報編集処理
           PERFORM 29011-HCM341-HEADER-HEN-SEC
      *
           MOVE    ORCHCM34-PRTAREA    TO  ORCSPRTAREA
      *
           MOVE    SPA-PTID            TO  SPRT-PTID
           MOVE    ORCHCM34-SRYYM      TO  SPRT-SRYYM
           MOVE    "HCM341V01.red"     TO  SPRT-PRTID
           MOVE    HCM341              TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               MOVE    2               TO  ORCHCM34-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2901-HCM341-BAT-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    会計カード一括印刷処理（頭紙）
      *****************************************************************
       2901-HCM342-BAT-PRINT-SEC       SECTION.
      *
      *    ヘッダー情報編集処理
           PERFORM 29011-HCM342-HEADER-HEN-SEC
      *
           MOVE    ORCHCM34-PRTAREA    TO  ORCSPRTAREA
      *
           MOVE    SPA-PTID            TO  SPRT-PTID
           MOVE    ORCHCM34-SRYYM      TO  SPRT-SRYYM
           MOVE   "HCM342V01.red"      TO  SPRT-PRTID
           MOVE    HCM342              TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               MOVE    3               TO  ORCHCM34-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2901-HCM342-BAT-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダー情報編集処理（頭紙）
      *****************************************************************
       29011-HCM341-HEADER-HEN-SEC SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    WRK-HCM34-SRYYM         TO  HCM341-SRYYM
      *
           IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
               MOVE    "入院"              TO  HCM341-NYUGAI
           ELSE
               MOVE    "外来"              TO  HCM341-NYUGAI
           END-IF
      *
           MOVE    WRK-HCM34-SAKYMD        TO  HCM341-SAKYMD
           MOVE    WRK-HCM34-PTNUM         TO  HCM341-PTNUM
           MOVE    WRK-HCM34-PTNUM2        TO  HCM341-PTNUM2
           MOVE    PTINF-NAME              TO  HCM341-NAME
           MOVE    PTINF-KANANAME          TO  HCM341-KANANAME
           MOVE    WRK-HCM34-BIRTHDAY      TO  HCM341-BIRTHDAY
           MOVE    WRK-HCM34-SRYKA(1)      TO  HCM341-SRYKA(1)
           MOVE    WRK-HCM34-SRYKA(2)      TO  HCM341-SRYKA(2)
           MOVE    WRK-HCM34-NYUINYMD      TO  HCM341-NYUINYMD
           MOVE    WRK-HCM34-TAIINYMD      TO  HCM341-TAIINYMD
           MOVE    WRK-HCM34-LBLSRYKA      TO  HCM341-LBLSRYKA
           MOVE    WRK-HCM34-LBLNYUINYMD   TO  HCM341-LBLNYUINYMD
           MOVE    WRK-HCM34-LBLTAIINYMD   TO  HCM341-LBLTAIINYMD
           MOVE    WRK-HCM34-BTUNAME       TO  HCM341-BTUNAME
           MOVE    CNT-PAGE                TO  WRK-ZZ9
           MOVE    WRK-ZZ9                 TO  HCM341-PAGE
      *
           EVALUATE    TRUE
           WHEN      ( CNT-YKZINGAI    >   ZERO )
                 AND ( CNT-YKZHKT      >   ZERO )
               MOVE  "☆：院外処方の薬剤　○：院内処方（包括）分の薬剤"
                                           TO  HCM341-COMMENT
           WHEN      ( CNT-YKZINGAI    >   ZERO )
               MOVE  "☆：院外処方の薬剤"  TO  HCM341-COMMENT
           WHEN      ( CNT-YKZHKT      >   ZERO )
               MOVE  "○：院内処方（包括）分の薬剤"
                                           TO  HCM341-COMMENT
           END-EVALUATE
      *
           .
       29011-HCM341-HEADER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダー情報編集処理（続紙）
      *****************************************************************
       29011-HCM342-HEADER-HEN-SEC SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    WRK-HCM34-SRYYM         TO  HCM342-SRYYM
      *
           IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
               MOVE    "入院"              TO  HCM342-NYUGAI
           ELSE
               MOVE    "外来"              TO  HCM342-NYUGAI
           END-IF
      *
           MOVE    WRK-HCM34-SAKYMD        TO  HCM342-SAKYMD
           MOVE    WRK-HCM34-PTNUM         TO  HCM342-PTNUM
           MOVE    WRK-HCM34-PTNUM2        TO  HCM342-PTNUM2
           MOVE    PTINF-NAME              TO  HCM342-NAME
           MOVE    PTINF-KANANAME          TO  HCM342-KANANAME
           MOVE    WRK-HCM34-BIRTHDAY      TO  HCM342-BIRTHDAY
           MOVE    WRK-HCM34-SRYKA(1)      TO  HCM342-SRYKA(1)
           MOVE    WRK-HCM34-SRYKA(2)      TO  HCM342-SRYKA(2)
           MOVE    WRK-HCM34-NYUINYMD      TO  HCM342-NYUINYMD
           MOVE    WRK-HCM34-TAIINYMD      TO  HCM342-TAIINYMD
           MOVE    WRK-HCM34-LBLSRYKA      TO  HCM342-LBLSRYKA
           MOVE    WRK-HCM34-LBLNYUINYMD   TO  HCM342-LBLNYUINYMD
           MOVE    WRK-HCM34-LBLTAIINYMD   TO  HCM342-LBLTAIINYMD
           MOVE    WRK-HCM34-BTUNAME       TO  HCM342-BTUNAME
           MOVE    CNT-PAGE                TO  WRK-ZZ9
           MOVE    WRK-ZZ9                 TO  HCM342-PAGE
      *
           EVALUATE    TRUE
           WHEN      ( CNT-YKZINGAI    >   ZERO )
                 AND ( CNT-YKZHKT      >   ZERO )
               MOVE  "☆：院外処方の薬剤　○：院内処方（包括）分の薬剤"
                                           TO  HCM342-COMMENT
           WHEN      ( CNT-YKZINGAI    >   ZERO )
               MOVE  "☆：院外処方の薬剤"  TO  HCM342-COMMENT
           WHEN      ( CNT-YKZHKT      >   ZERO )
               MOVE  "○：院内処方（包括）分の薬剤"
                                           TO  HCM342-COMMENT
           END-EVALUATE
      *
           .
       29011-HCM342-HEADER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    CNT-PAGE                TO  ORCHCM34-PAGE
      *
           .
       300-END-EXT.
           EXIT.
      *
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
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    文字列長チェック処理
      *****************************************************************
       800-STRLEN-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-STRLEN
           PERFORM VARYING IDXB    FROM    500 BY  -1
                   UNTIL ( IDXB    <   1 )
               IF    ( WRK-STR (IDXB : 1)  NOT =   SPACE )
                   MOVE    IDXB    TO  WRK-STRLEN
                   MOVE    1       TO  IDXB
               END-IF
           END-PERFORM
      *
           .
       800-STRLEN-CHK-EXT.
           EXIT.
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
      *    点数マスタ検索処理(KEY1)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    ORC-DBYMD           TO  TTNS-CHK-YMD
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
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  ORC-DBYMD
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
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
      *
           .
       920-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       920-DBCLOSE-SEC               SECTION.
      *
      *    暫定的にコメント化
      *    MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
      *    CALL    "MCPSUB"            USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
           .
       920-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "ORCHCM34  ERRCD [" ORCHCM34-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
