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
000100 IDENTIFICATION      DIVISION.
000200 PROGRAM-ID.         ORCSDAY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日付変換サブ
      *  コンポーネント名  : 日付変換
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−    　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.05.07  明治の開始年の修正
      *  01.00.02    MCC-藤原     01.05.07  日のニューメリックチェック追加
      *  01.00.03    NACL-藤原    02.10.21  漢字項目の空白を全角に修正
      *  01.00.04    NACL-多々納  03.04.16  END-IF,END-SERACH追加
      *  01.00.05    NACL-多々納  04.09.22  海の日、敬老の日変更
      *****************************************************************
001400 ENVIRONMENT         DIVISION.
001500 CONFIGURATION       SECTION.
002000 DATA                DIVISION.
002100 WORKING-STORAGE SECTION.
002200 01  WK-YYMMDD                   PIC 9(08).
002300 01  WK-SYYMMDD      REDEFINES   WK-YYMMDD.
002400     03  WK-SYY                  PIC 9(04).
002500     03  WK-SMD                  PIC 9(04).
002600 01  WK-WYYMMDD      REDEFINES   WK-YYMMDD.
002700     03  FILLER                  PIC X(01).
002800     03  WK-WYYMMDD-7.
002900       05  WK-WG                 PIC 9(01).
003000       05  WK-WYY                PIC 9(02).
003100       05  WK-WMD                PIC 9(04).
003200 01  WK-YYMMDD-R     REDEFINES   WK-YYMMDD.
003300     03  WK-YY1.
003400       05  FILLER                PIC 9(01).
003500       05  WK-GYY1               PIC 9(01).
003600     03  WK-YMD.
003700       05  WK-YM.
003800         07  WK-YY               PIC 9(02).
003900         07  WK-MM               PIC 9(02).
004000       05  WK-DD                 PIC 9(02).
004100 01  WK-YYYYMMDD-R   REDEFINES   WK-YYMMDD.
004200     03  WK-YYYYMM               PIC 9(06).
004300     03  FILLER                  PIC 9(02).
004400*
004500 01  WK-NENDO                    PIC 9(04).
004600 01  WK-NISU                     PIC S9(05).
004700 01  WK-SUUTI                    PIC 9(05).
004800 01  WK-SYO                      PIC 9(05).
004900 01  WK-AMARI                    PIC 9(05).
005000 01  WK-SANSYUTS                 PIC S9(05).
005100 01  WK-ST                       PIC 9(08).
005200 01  WK-ST-R         REDEFINES   WK-ST.
005300     03  WK-ST1                  PIC 9(04).
005400     03  WK-ST2                  PIC 9(04).
005500 01  WK-END                      PIC 9(08).
005600 01  SYURYO-YMD.
005700     03  SYURYO-YY               PIC 9(04).
005800     03  SYURYO-MM               PIC 9(02).
005900     03  SYURYO-DD               PIC 9(02).
006000 01  KISAN-YMD.
006100     03  KISAN-YY                PIC 9(04).
006200     03  KISAN-MM                PIC 9(02).
006300     03  KISAN-DD                PIC 9(02).
006400 01  WK-FLG.
006500     03  WK-ERR                  PIC 9(01).
006600     03  WK-UERR                 PIC 9(04).
006700 01  WK-YSKKBN.
006800     03  YSK-YOUBI               PIC 9(01).
006900     03  YSK-YOUBI2              PIC 9(01).
007000     03  YSK-SYUKJTS             PIC 9(01).
007100     03  YSK-KYUJTS              PIC 9(01).
007200 01  WK-YSK-YYMM.
007300     03  WK-YSK-YYYY             PIC 9(04).
007400     03  WK-YSK-MM               PIC 9(02).
007500 01  WK-MNS-YMD.
007600     03  WK-MNS-YY               PIC 9(04).
007700     03  WK-MNS-MM               PIC 9(02).
007800     03  WK-MNS-DD               PIC 9(02).
007900 01  WK-YUKO.
008000     03  WK-YUKO-YY              PIC 9(04).
008100     03  WK-YUKO-MM              PIC 9(02).
008200     03  WK-YUKO-DD              PIC 9(02).
008300 01  CST-AREA.
008400     03  CST1        PIC 999V9(08)   VALUE  365.24217878.
008500     03  CST2        PIC 99V9(08)    VALUE  20.98819444.
008600     03  CST3        PIC 9V9(07)     VALUE  2.4194444.
008700 01  WK-NISSU-A.
008800     03  NISU-A                  PIC 9(03).
008900     03  NISU-B                  PIC 9(05).
009000     03  KIJYUN-A                PIC S9(05).
009100     03  KIJYUN-YY               PIC 9(04).
009200     03  SYU-W                   PIC S9(03)  VALUE  365.
009300 01  WK-SYSZYB.
009400     03  WK-SY                   PIC 9(04).
009500     03  WK-SZ                   PIC 9(04).
009600     03  WK-SSYB                 PIC 9(05)V9(08).
009700     03  WK-WORKA                PIC 9(02).
009800 01  WK-YOUB.
009900     03  FILLER                  PIC 9(04).
010000     03  WYOUB-MMDD              PIC 9(04).
010100 01  WK-SKJYMD.
010200     03  WK-SKJTYY               PIC 9(04).
010300     03  WK-SKJTMM               PIC 9(02).
010400     03  WK-SKJTDD               PIC 9(02).
       01  WK-DYMMY.
010200     03  WK-DUMMYN               PIC 9(10).
010500*
010600 01  GENGO-TBL.
010700     03  FILLER.
010800       05  FILLER    PIC X(10)   VALUE   "4H19890108".
010900       05  FILLER    PIC X(04)   VALUE   "平成".
011000     03  FILLER.
011100       05  FILLER    PIC X(10)   VALUE   "3S19261225".
011200       05  FILLER    PIC X(04)   VALUE   "昭和".
011300     03  FILLER.
011400       05  FILLER    PIC X(10)   VALUE   "2T19120730".
011500       05  FILLER    PIC X(04)   VALUE   "大正".
011600     03  FILLER.
      *---- (01.00.01) LINE MOD START ----------------------------*
      ****   05  FILLER    PIC X(10)   VALUE   "1M18730101".
             05  FILLER    PIC X(10)   VALUE   "1M18680101".
      *---- (01.00.01) LINE MOD END   ----------------------------*
             05  FILLER    PIC X(04)   VALUE   "明治".
       01  GENGO-TBL-R     REDEFINES   GENGO-TBL.
012000     03  GENGO-TBL-R2   OCCURS   4   INDEXED  BY  IX1.
012100       05  GENGO-T1              PIC 9(01).
012200       05  GENGO-T2              PIC X(01).
012300       05  GENGO-YYMMDD-X.
012400         07  GENGO-YYYY.
012500           09  GENGO-YY          PIC 9(02).
012600           09  GENGO-YY2         PIC 9(02).
012700         07  GENGO-YYYY-R        REDEFINES    GENGO-YYYY.
012800           09  GENGO-YYYY-RR     PIC 9(04).
012900         07  GENGO-MMDD          PIC 9(04).
013000       05  GENGO-YYMMDD-R        REDEFINES    GENGO-YYMMDD-X.
013100         07  GENGO-YYMMDD        PIC 9(08).
013200       05  GENMEI                PIC N(2).
013300*
013400*    ２０００年以前の祝日テーブル（固定日）
013500 01  SKJT-TABLE.
013600     03  FILLER      PIC X(12)   VALUE "010101150211".
013700     03  FILLER      PIC X(12)   VALUE "042905030504".
013800     03  FILLER      PIC X(12)   VALUE "050507200915".
013900     03  FILLER      PIC X(12)   VALUE "101011031123".
014000     03  FILLER      PIC X(12)   VALUE "112300000000".
014100 01  SKJT-TABLE-R    REDEFINES   SKJT-TABLE.
014200     03  SKJT-TBL    OCCURS      15  INDEXED  BY  IX2.
014300       05  SKJT-T1               PIC 9(04).
014400*
      *    ２０００年以降の祝日テーブル（固定日）
       01  SKJT-TABLE-NEW.
           03  FILLER      PIC X(12)   VALUE "010102110429".
           03  FILLER      PIC X(12)   VALUE "050305040505".
           03  FILLER      PIC X(12)   VALUE "072009151103".
           03  FILLER      PIC X(12)   VALUE "112312230000".
       01  SKJT-TABLE-RNEW  REDEFINES  SKJT-TABLE-NEW.
           03  SKJT-TBLNEW  OCCURS     12  INDEXED  BY IX6.
             05  SKJT-NEW              PIC 9(04).
      *
      *    ２００３年以降の祝日テーブル（固定日）
      *    (7/20,9/15 を第３月曜へ）
       01  SKJT-TABLE-NEW2.
           03  FILLER      PIC X(12)   VALUE "010102110429".
           03  FILLER      PIC X(12)   VALUE "050305040505".
           03  FILLER      PIC X(12)   VALUE "110311231223".
       01  SKJT-TABLE-RNEW2  REDEFINES  SKJT-TABLE-NEW2.
           03  SKJT-TBLNEW2  OCCURS     9   INDEXED  BY IX7.
             05  SKJT-NEW2              PIC 9(04).
      *
015500*    祝日テーブル２（春分の日・秋分の日）
015600 01  SKJT-TABLE2.
015700     03  SKJT-TBL2   OCCURS      2   INDEXED  BY  IX3.
015800       05  SKJT-M                PIC 9(02).
015900       05  SKJT-D                PIC 9(02).
      *
      *    祝日テーブル３（成人の日・体育の日）
       01  SKJT-TABLE3.
           03  SKJT-TBL3   OCCURS      2   INDEXED  BY  IX5.
             05  SKJT-M3               PIC 9(02).
             05  SKJT-D3               PIC 9(02).
      *
      *    祝日テーブル４（海日・敬老の日）
       01  SKJT-TABLE4.
           03  SKJT-TBL4   OCCURS      2   INDEXED  BY  IX8.
             05  SKJT-M4               PIC 9(02).
             05  SKJT-D4               PIC 9(02).
016600*
016700*    年間祝日管理テーブル  ( 99 => サンシュツ ニヨリ セッテイ )
016800 01  SKJT-KNR-TBL.
016900     03  FILLER      PIC X(14)   VALUE "01019900000002".
017000     03  FILLER      PIC X(14)   VALUE "02110000000001".
017100     03  FILLER      PIC X(14)   VALUE "03990000000001".
017200     03  FILLER      PIC X(14)   VALUE "04290000000001".
017300     03  FILLER      PIC X(14)   VALUE "05030405000003".
017400     03  FILLER      PIC X(14)   VALUE "06000000000000".
017500     03  FILLER      PIC X(14)   VALUE "07200000000001".
017600     03  FILLER      PIC X(14)   VALUE "08000000000000".
017700     03  FILLER      PIC X(14)   VALUE "09159900000002".
017800     03  FILLER      PIC X(14)   VALUE "10990000000001".
017900     03  FILLER      PIC X(14)   VALUE "11032300000002".
018000     03  FILLER      PIC X(14)   VALUE "12230000000001".
018100 01  SKJT-KNRTBL      REDEFINES  SKJT-KNR-TBL.
018200     03  SKJT-KNROC  OCCURS      12.
018300       05  SKJT-KNRMM            PIC 9(02).
018400       05  SKJT-KNRDD            PIC 9(02)    OCCURS   5.
018500       05  SKJT-KNRCNT           PIC 9(02).
018600*
018700*    休日テーブル
018800 01  KYUJT-TABLE.
018900     03  FILLER      PIC X(08)   VALUE "01020103".
019000     03  FILLER      PIC X(12)   VALUE "122912301231".
019100     03  FILLER      PIC X(20)   VALUE "00000000000000000000".
019200 01  KYUJT-TABLE-R   REDEFINES   KYUJT-TABLE.
019300     03  KYUJT-TBL   OCCURS      10  INDEXED  BY  IX4.
019400       05  KYUJT-T2              PIC 9(04).
019500*
019600*    カレンダーテーブル
019700 01  CAL-TABLE.
019800     03  FILLER      PIC X(24)   VALUE
                                       "312831303130313130313031".
019900 01  CAL-TABLE-R     REDEFINES   CAL-TABLE.
020000     03  CAL-TBL     OCCURS      12.
020100       05  CAL-T                 PIC 9(02).
020200*
020300*    日数テーブル
020400 01  NISU-TABLE.
020500     03  FILLER      PIC X(36)   VALUE
020600                    "000031059090120151181212243273304334".
020700 01  NISU-TABLE2     REDEFINES   NISU-TABLE.
020800     03  NISU-TBL    OCCURS      12.
020900       05  NISU-T                PIC 9(03).
021000*
021100*    曜日テーブル
021200 01  YOUBI-TABLE.
021300     03  FILLER.
021400       05  FILLER    PIC X(04)   VALUE    "1MON".
021500       05  FILLER    PIC X(02)   VALUE    "月".
021600     03  FILLER.
021700       05  FILLER    PIC X(04)   VALUE    "2TUE".
021800       05  FILLER    PIC X(02)   VALUE    "火".
021900     03  FILLER.
022000       05  FILLER    PIC X(04)   VALUE    "3WED".
022100       05  FILLER    PIC X(02)   VALUE    "水".
022200     03  FILLER.
022300       05  FILLER    PIC X(04)   VALUE    "4THU".
022400       05  FILLER    PIC X(02)   VALUE    "木".
022500     03  FILLER.
022600       05  FILLER    PIC X(04)   VALUE    "5FRI".
022700       05  FILLER    PIC X(02)   VALUE    "金".
022800     03  FILLER.
022900       05  FILLER    PIC X(04)   VALUE    "6SAT".
023000       05  FILLER    PIC X(02)   VALUE    "土".
023100     03  FILLER.
023200       05  FILLER    PIC X(04)   VALUE    "7SUN".
023300       05  FILLER    PIC X(02)   VALUE    "日".
023400 01  YOUBI-TABLE2    REDEFINES   YOUBI-TABLE.
023500     03  YOUBI-TBL   OCCURS      7   INDEXED  BY  IX-YOUBI.
023600       05  YOUBI-KBN             PIC X(01).
023700       05  YOUBI-KIGO            PIC X(03).
023800       05  YOUBI-MEI             PIC X(02).
023900*
024000*    漢字編集テーブル
024100 01  EDT-GYMD.
024200     03  EDT-GYMD-XX             PIC  9(02).
024300     03  EDT-GYMD-YMD.
024400         05  EDT-GYMD-Y          PIC  9(02).
024500         05  EDT-GYMD-M          PIC  9(02).
024600         05  EDT-GYMD-D          PIC  9(02).
024700 01  EDT-GYMDZ.
024800     03  EDT-GYMDZ-YMD.
024900         05  EDT-GYMDZ-Y         PIC  Z9.
025000         05  EDT-GYMDZ-M         PIC  Z9.
025100         05  EDT-GYMDZ-D         PIC  Z9.
025200 01  EDT-GYMDX.
025300     03  EDT-GYMDX-OC            PIC  X(01) OCCURS   6.
025400*
025500 01  EDT-GYMD1.
025600     03  EDT-GYMD1-G             PIC  X(01).
025700     03  EDT-GYMD1-YMD.
025800         05  EDT-GYMD1-Y         PIC  Z(02).
025900         05  EDT-GYMD1-P1        PIC  X(01).
026000         05  EDT-GYMD1-M         PIC  Z(02).
026100         05  EDT-GYMD1-P2        PIC  X(01).
026200         05  EDT-GYMD1-D         PIC  Z(02).
026300*
026400 01  EDT-GYMD2.
026500     03  EDT-GYMD2-G             PIC  X(04).
026600     03  EDT-GYMD2-YMD.
026700         05  EDT-GYMD2-Y         PIC  X(04).
026800         05  EDT-GYMD2-P1        PIC  X(02).
026900         05  EDT-GYMD2-M         PIC  X(04).
027000         05  EDT-GYMD2-P2        PIC  X(02).
027100         05  EDT-GYMD2-D         PIC  X(04).
027200*
027300 01  EDT-GYMD3.
027400     03  EDT-GYMD3-G             PIC  X(04).
027500     03  EDT-GYMD3-YMD.
027600         05  EDT-GYMD3-Y         PIC  X(04).
027700         05  EDT-GYMD3-NEN       PIC  X(02).
027800         05  EDT-GYMD3-M         PIC  X(04).
027900         05  EDT-GYMD3-TUKI      PIC  X(02).
028000         05  EDT-GYMD3-D         PIC  X(04).
028100         05  EDT-GYMD3-HI        PIC  X(02).
028200*
028300 01  EDT-YMD                     PIC  N(06).
028400 01  EDT-YMDR                    REDEFINES  EDT-YMD.
028500     03  EDT-YMD-OC              PIC  X(02) OCCURS 6.
028600 01  EDT-YMDR1                   REDEFINES  EDT-YMD.
028700     03  EDT-YMD-Y               PIC  X(04).
028800     03  EDT-YMD-M               PIC  X(04).
028900     03  EDT-YMD-D               PIC  X(04).
029000*
029100*    数字テーブル
029200 01  SUUJI-AREA.
029300     03  SUUJIN-TBL.
029400         05  FILLER              PIC  X(22)  VALUE
029500           "  ０１２３４５６７８９".
029600     03  SUUJIN-TBL-R            REDEFINES   SUUJIN-TBL.
029700         05  SUUJIN              PIC  X(02)  OCCURS  11.
029800     03  SUUJI-TBL.
029900         05  FILLER              PIC  X(11)  VALUE
030000         " 0123456789".
030100     03  SUUJI-TBL-R             REDEFINES   SUUJI-TBL.
030200         05  SUUJI               PIC  X(01)  OCCURS  11.
030300*
030400*    コンスタント文字領域
030500 01  CST-MOJI-AREA.
030600     03  CST-SPACE               PIC  X(02)  VALUE
030700           "  ".
030800     03  CST-NEN                 PIC  X(02)  VALUE
030900           "年".
031000     03  CST-TUKI                PIC  X(02)  VALUE
031100           "月".
031200     03  CST-HI                  PIC  X(02)  VALUE
031300           "日".
031400     03  CST-PN                  PIC  X(02)  VALUE
031500           "．".
031600     03  CST-PX                  PIC  X(01)  VALUE
031700           ".".
031800*
031900*    添え字領域
032000 01  IDX-AREA.
032100     03  I                       PIC  9(02)  VALUE  ZERO.
032200     03  J                       PIC  9(02)  VALUE  ZERO.
           03  IDX                     PIC  9(04).
032300/
032400***
032500     COPY  "CPORCSDAY.INC".
032600***
032700 LINKAGE             SECTION.
032800*D   COPY  "CPLNKSTS.INC".
032800     COPY  "CPORCSLNK.INC".
032900 01  LNK-DAYX-AREA.
033000     03  LNK-DAYX-IRAI           PIC X(02).
033100     03  FILLER                  PIC X(78).
033200/
033300 PROCEDURE           DIVISION  USING   STS-AREA-DAY
                                             LNK-DAYX-AREA.
033400******************************************************************
033500*        依頼制御処理
033600******************************************************************
033700 MAIN-RTN            SECTION.
033800 MAIN-000.
033900     MOVE    SPACE           TO  LNK-DAY1-AREA
034000                                 LNK-DAY2-AREA
034100                                 LNK-DAY3-AREA
034200                                 LNK-DAY4-AREA
034300                                 LNK-DAY5-AREA
034400                                 LNK-DAY6-AREA
034500                                 LNK-DAY7-AREA.
034600     MOVE    ZERO            TO  STS-DAY-RC1
                                       STS-DAY-RC2
034700                                 WK-YYMMDD
034800                                 WK-FLG.
034900*
           EVALUATE    LNK-DAYX-IRAI
               WHEN    "11"
               WHEN    "12"
      *            暦変換
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY1-AREA
                   PERFORM  DTCONV-RTN
                   MOVE     LNK-DAY1-AREA      TO  LNK-DAYX-AREA
               WHEN    "21"
               WHEN    "22"
      *            日付編集
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY2-AREA
                   PERFORM  DTEDIT-RTN
                   MOVE     LNK-DAY2-AREA      TO  LNK-DAYX-AREA
               WHEN    "31"
               WHEN    "32"
      *            曜日算出
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY3-AREA
                   PERFORM  DTYOUB-RTN
                   MOVE     LNK-DAY3-AREA      TO  LNK-DAYX-AREA
               WHEN    "41"
               WHEN    "42"
      *            祝日算出
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY4-AREA
                   PERFORM  DTSKJT-RTN
                   MOVE     LNK-DAY4-AREA      TO  LNK-DAYX-AREA
               WHEN    "51"
               WHEN    "52"
      *            期間日数計算
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY5-AREA
                   PERFORM  DTKIKN-RTN
                   MOVE     LNK-DAY5-AREA      TO  LNK-DAYX-AREA
               WHEN    "61"
               WHEN    "62"
      *            該当日算出
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY6-AREA
                   PERFORM  DTPLMN-RTN
                   MOVE     LNK-DAY6-AREA      TO  LNK-DAYX-AREA
               WHEN    "71"
               WHEN    "72"
      *            月末日算出
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY7-AREA
                   PERFORM  DTMEND-RTN
                   MOVE     LNK-DAY7-AREA      TO  LNK-DAYX-AREA
               WHEN    OTHER
                   MOVE     30             TO  STS-DAY-RC1
                   MOVE     01             TO  STS-DAY-RC2
           END-EVALUATE
           .
038700 MAIN-EXT.
038800     EXIT PROGRAM
038900     STOP RUN.
039000******************************************************************
039100*        暦変換処理                         (IRAI = 11 / 12)     *
039200******************************************************************
039300 DTCONV-RTN           SECTION.
039400 DTCONV-000.
039500     EVALUATE    LNK-DAY1-IRAI
039600         WHEN    "11"
039700         WHEN    "21"
039800         WHEN    "61"
039900         WHEN    "71"
040000                 MOVE    LNK-DAY1-YMD    TO  WK-YYMMDD
040100         WHEN    "12"
040200         WHEN    "22"
040300         WHEN    "32"
040400         WHEN    "42"
040500         WHEN    "52"
040600         WHEN    "62"
040700         WHEN    "72"
040800                 MOVE    LNK-DAY1-WYMD   TO  WK-YYMMDD
040900         WHEN    OTHER
041000                 MOVE    30              TO  STS-DAY-RC1
041100                 MOVE    01              TO  STS-DAY-RC2
041200                 GO  TO      DTCONV-EXT
041300     END-EVALUATE.
041400*
041500     IF    LNK-DAY1-IRAI  =  "12"  OR  "22"  OR  "32"  OR
041600                             "42"  OR  "52"  OR  "62"  OR
041700                             "72"
041800           GO  TO      DTCONV-100
           END-IF.
041900*
042000     PERFORM  YMD-CHK-RTN.
042100     IF  WK-ERR  =  1
042200         MOVE    20        TO    STS-DAY-RC1
042300         GO  TO      DTCONV-EXT
           END-IF.
042400*
042500     SET     IX1    TO     1.
042600     SEARCH  GENGO-TBL-R2  VARYING  IX1
042700       AT  END
042800         MOVE    20        TO    STS-DAY-RC1
042900         GO  TO      DTCONV-EXT
043000       WHEN
043100         GENGO-YYMMDD (IX1)  NOT >  WK-YYMMDD
043200         COMPUTE  WK-SYY  =  WK-SYY  -  GENGO-YYYY-RR (IX1)  +  1
043300         MOVE    GENGO-T1 (IX1)    TO    LNK-DAY1-GENGO
043400         MOVE    GENGO-T2 (IX1)    TO    LNK-DAY1-GENKIGO
043500         MOVE    GENMEI   (IX1)    TO    LNK-DAY1-GENMEI
043600         GO  TO      DTCONV-200
           END-SEARCH.
043700************
043800 DTCONV-100.
043900************
044000     MOVE    WK-WG         TO    LNK-DAY1-GENGO.
044100     SET     IX1    TO     1.
044200     SEARCH  GENGO-TBL-R2  VARYING  IX1
044300       AT  END
044400         MOVE    30        TO    STS-DAY-RC1
044500         MOVE    01        TO    STS-DAY-RC2
044600         GO  TO      DTCONV-EXT
044700       WHEN
044800         GENGO-T1 (IX1)  =  LNK-DAY1-GENGO
044900         COMPUTE  WK-SYY  =  WK-YY  +  GENGO-YYYY-RR (IX1)  -  1
045000         MOVE    GENGO-T2 (IX1)    TO    LNK-DAY1-GENKIGO
045100         MOVE    GENMEI   (IX1)    TO    LNK-DAY1-GENMEI 
044700       WHEN
044800         GENGO-T2 (IX1)  =  LNK-DAY1-GENGO
044900         COMPUTE  WK-SYY  =  WK-YY  +  GENGO-YYYY-RR (IX1)  -  1
045000         MOVE    GENGO-T1 (IX1)    TO    LNK-DAY1-GENGO
045000         MOVE    GENGO-T2 (IX1)    TO    LNK-DAY1-GENKIGO
045100         MOVE    GENMEI   (IX1)    TO    LNK-DAY1-GENMEI
           END-SEARCH.
045200     PERFORM  YMD-CHK-RTN.
045300     IF  WK-ERR  =  1
045400         MOVE    20          TO    STS-DAY-RC1
045500         GO  TO      DTCONV-EXT
           END-IF.
045600************
045700 DTCONV-200.
045800************
045900     EVALUATE    LNK-DAY1-IRAI
046000         WHEN    "11"
046100         WHEN    "21"
046200         WHEN    "61"
046300         WHEN    "71"
046400                 MOVE    GENGO-T1 (IX1)  TO  WK-WG
046500                 MOVE    WK-YYMMDD       TO  LNK-DAY1-YMD
046600         WHEN    "12"
046700         WHEN    "22"
046800         WHEN    "32"
046900         WHEN    "42"
047000         WHEN    "52"
047100         WHEN    "62"
047200         WHEN    "72"
047300                 MOVE    WK-YYMMDD       TO  LNK-DAY1-YMD
047400         WHEN    OTHER
047500                 MOVE    30              TO  STS-DAY-RC1
047600                 MOVE    01              TO  STS-DAY-RC2
047700                 GO  TO      DTCONV-EXT
047800     END-EVALUATE.
047900 DTCONV-EXT.
048000     EXIT.
048100******************************************************************
048200*        日付編集処理                       (IRAI = 21 / 22)     *
048300******************************************************************
048400 DTEDIT-RTN           SECTION.
048500 DTEDIT-000.
048600*
048700*    *受取日付の西暦 OR 和暦判定
048800     IF  LNK-DAY2-IRAI   =   "21"
048900*        *西暦日付による場合
049000         MOVE    LNK-DAY2-IRAI     TO    LNK-DAY1-IRAI
049100         MOVE    LNK-DAY2-YMD      TO    LNK-DAY1-YMD
049200         PERFORM  DTCONV-RTN
049300         IF  STS-DAY-RC1  NOT =  ZERO
049400             MOVE    SPACE         TO    LNK-DAY1-AREA
049500             GO  TO      DTEDIT-EXT
049600         END-IF
049700     ELSE
049800*        *和暦日付による場合
049900         MOVE    LNK-DAY2-IRAI     TO    LNK-DAY1-IRAI
050000         MOVE    LNK-DAY2-YMD      TO    LNK-DAY1-YMD
050100         PERFORM  DTCONV-RTN
050200         IF  STS-DAY-RC1  NOT =  ZERO
050300             MOVE    SPACE         TO    LNK-DAY1-AREA
050400             GO  TO      DTEDIT-EXT
050500         END-IF
050600         MOVE    LNK-DAY2-YMD      TO    WK-YYMMDD
050700     END-IF.
050800*
050900     PERFORM  SUUJI-EDT-RTN.
051000*
051100*    *編集１ → X ZZ.ZZ.ZZ
051200     MOVE    LNK-DAY1-GENKIGO         TO    EDT-GYMD1-G.
051300     MOVE    WK-YY                    TO    EDT-GYMD1-Y.
051400     MOVE    CST-PX                   TO    EDT-GYMD1-P1.
051500     MOVE    WK-MM                    TO    EDT-GYMD1-M.
051600     MOVE    CST-PX                   TO    EDT-GYMD1-P2.
051700     MOVE    WK-DD                    TO    EDT-GYMD1-D.
051800     MOVE    EDT-GYMD1                TO    LNK-DAY2-EDTYMD1.
051900*
052000*    *編集２ → NN NN.NN.NN
052100     MOVE    LNK-DAY1-GENMEI          TO    EDT-GYMD2-G.
052200     MOVE    EDT-YMD-Y                TO    EDT-GYMD2-Y.
052300     MOVE    CST-PN                   TO    EDT-GYMD2-P1.
052400     MOVE    EDT-YMD-M                TO    EDT-GYMD2-M.
052500     MOVE    CST-PN                   TO    EDT-GYMD2-P2.
052600     MOVE    EDT-YMD-D                TO    EDT-GYMD2-D.
052700     MOVE    EDT-GYMD2                TO    LNK-DAY2-EDTYMD2.
           INSPECT LNK-DAY2-EDTYMD2         REPLACING   ALL "  "
                                                        BY  "　". 
052800*
052900*    *編集３ → NN NN年NN月NN日
053000     MOVE    LNK-DAY1-GENMEI          TO    EDT-GYMD3-G.
053100     MOVE    EDT-YMD-Y                TO    EDT-GYMD3-Y.
053200     MOVE    CST-NEN                  TO    EDT-GYMD3-NEN.
053300     MOVE    EDT-YMD-M                TO    EDT-GYMD3-M.
053400     MOVE    CST-TUKI                 TO    EDT-GYMD3-TUKI.
053500     MOVE    EDT-YMD-D                TO    EDT-GYMD3-D.
053600     MOVE    CST-HI                   TO    EDT-GYMD3-HI.
053700     MOVE    EDT-GYMD3                TO    LNK-DAY2-EDTYMD3.
           INSPECT LNK-DAY2-EDTYMD3         REPLACING   ALL "  "
                                                        BY  "　". 
053800*
053900     MOVE    SPACE                    TO    LNK-DAY1-AREA.
054000 DTEDIT-EXT.
054100     EXIT.
054200******************************************************************
054300*        曜日算出処理                       (IRAI = 31 / 32)     *
054400******************************************************************
054500 DTYOUB-RTN           SECTION.
054600 DTYOUB-000.
054700*
054800*    *受取日付の西暦 OR 和暦判定
054900     IF  LNK-DAY3-IRAI   =   "31"
055000*        *西暦日付による場合
055100         MOVE    LNK-DAY3-YMD      TO    WK-YYMMDD
055200         PERFORM  YMD-CHK-RTN
055300         IF  WK-ERR  =  1
055400             MOVE    20            TO    STS-DAY-RC1
055500             GO  TO      DTYOUB-EXT
055600         END-IF
055700     ELSE
055800*        *和暦日付による場合
055900         MOVE    LNK-DAY3-IRAI     TO    LNK-DAY1-IRAI
056000         MOVE    LNK-DAY3-YMD      TO    LNK-DAY1-YMD
056100         PERFORM  DTCONV-RTN
056200         MOVE    SPACE             TO    LNK-DAY1-AREA
056300         IF  STS-DAY-RC1  NOT =  ZERO
056400             GO  TO      DTYOUB-EXT
056500         END-IF
056600     END-IF.
056700*
056800     PERFORM  YSK-KBN-RTN.
056900     MOVE    YSK-SYUKJTS              TO    LNK-DAY3-SYUKU.
057000     MOVE    YSK-KYUJTS               TO    LNK-DAY3-YASUMI.
057100     PERFORM  YOUBI-SET-RTN.
057200     MOVE    YOUBI-KBN  (IX-YOUBI)    TO    LNK-DAY3-YOBI.
057300     MOVE    YOUBI-KIGO (IX-YOUBI)    TO    LNK-DAY3-YOBIKIGO.
057400     MOVE    YOUBI-MEI  (IX-YOUBI)    TO    LNK-DAY3-YOBIMEI.
057500 DTYOUB-EXT.
057600     EXIT.
057700******************************************************************
057800*        祝日算出処理                       (IRAI = 41 / 42)     *
057900******************************************************************
058000 DTSKJT-RTN          SECTION.
058100 DTSKJT-000.
058200*
058300*    *受取日付の西暦 OR 和暦判定
058400     IF  LNK-DAY4-IRAI   =   "41"
058500*        *西暦日付による場合
058600         MOVE    LNK-DAY4-YM       TO    WK-YYYYMM
058700         MOVE    01                TO    WK-DD
058800         PERFORM  YMD-CHK-RTN
058900         IF  WK-ERR  =  1
059000             MOVE    20            TO    STS-DAY-RC1
059100             GO  TO      DTSKJT-EXT
059200         END-IF
059300     ELSE
059400*        *和暦日付による場合
059500         MOVE    LNK-DAY4-IRAI     TO    LNK-DAY1-IRAI
059600         MOVE    LNK-DAY4-YM       TO    LNK-DAY1-YMD
059700         COMPUTE  LNK-DAY1-YMD  =  LNK-DAY1-YMD  *  100  +  1
059800         PERFORM  DTCONV-RTN
059900         MOVE    SPACE             TO    LNK-DAY1-AREA
060000         IF  STS-DAY-RC1  NOT =  ZERO
060100             GO  TO      DTSKJT-EXT
060200         END-IF
060300     END-IF.
060400*
060500*    祝日（春分の日，秋分の日）算出
060600     IF  WK-MM   =   3   OR   9
060700         PERFORM SKJT-KEISAN1-RTN
060800     END-IF.
060900*    祝日（成人の日，体育の日）算出
061000     IF  WK-MM   =   1   OR   10
061100         PERFORM SKJT-KEISAN2-RTN
061200     END-IF.
      *    祝日（海の日，敬老の日）算出
           IF  WK-MM   =   7   OR   9
               PERFORM SKJT-KEISAN3-SEC
           END-IF
061300*
061400     PERFORM     VARYING     I       FROM  1  BY  1
061500                   UNTIL     I       >     5
061600         MOVE    SKJT-KNRDD (WK-MM, I) TO  LNK-DAY4-SKJTDD (I)
061700     END-PERFORM.
061800     MOVE    SKJT-KNRCNT (WK-MM)       TO  LNK-DAY4-COUNT.
061900 DTSKJT-EXT.
062000     EXIT.
062100******************************************************************
062200*        期間日数計算処理                   (IRAI = 51 / 52)     *
062300******************************************************************
062400 DTKIKN-RTN           SECTION.
062500 DTKIKN-000.
062600*
062700*    *受取日付の西暦 OR 和暦判定（開始年月日）
062800     IF  LNK-DAY5-IRAI   =   "51"
062900*        *西暦日付による場合
063000         MOVE    LNK-DAY5-START    TO    WK-YYMMDD
063100         PERFORM  YMD-CHK-RTN
063200         IF  WK-ERR  =  1
063300             MOVE    30            TO    STS-DAY-RC1
063400             MOVE    02            TO    STS-DAY-RC2
063500             GO  TO      DTKIKN-EXT
063600         END-IF
063700     ELSE
063800*        *和暦日付による場合
063900         MOVE    LNK-DAY5-IRAI     TO    LNK-DAY1-IRAI
064000         MOVE    LNK-DAY5-START    TO    LNK-DAY1-YMD
064100         PERFORM  DTCONV-RTN
064200         MOVE    SPACE             TO    LNK-DAY1-AREA
064300         IF  STS-DAY-RC1  NOT =  ZERO
064400             MOVE    30            TO    STS-DAY-RC1
064500             MOVE    02            TO    STS-DAY-RC2
064600             GO  TO      DTKIKN-EXT
064700         END-IF
064800     END-IF.
064900     MOVE    WK-YYMMDD             TO    WK-ST.
065000*
065100*    *受取日付の西暦 OR 和暦判定（終了年月日）
065200     IF  LNK-DAY5-IRAI   =   "51"
065300*        *西暦日付による場合
065400         MOVE    LNK-DAY5-END      TO    WK-YYMMDD
065500         PERFORM  YMD-CHK-RTN
065600         IF  WK-ERR  =  1
065700             MOVE    30            TO    STS-DAY-RC1
065800             MOVE    03            TO    STS-DAY-RC2
065900             GO  TO      DTKIKN-EXT
066000         END-IF
066100     ELSE
066200*        *和暦日付による場合
066300         MOVE    LNK-DAY5-IRAI     TO    LNK-DAY1-IRAI
066400         MOVE    LNK-DAY5-END      TO    LNK-DAY1-YMD
066500         PERFORM  DTCONV-RTN
066600         MOVE    SPACE             TO    LNK-DAY1-AREA
066700         IF  STS-DAY-RC1  NOT =  ZERO
066800             MOVE    30            TO    STS-DAY-RC1
066900             MOVE    03            TO    STS-DAY-RC2
067000             GO  TO      DTKIKN-EXT
067100         END-IF
067200     END-IF.
067300     MOVE    WK-YYMMDD             TO    WK-END.
067400*
067500     IF  WK-ST  >  WK-END
067600         MOVE    ZERO              TO    LNK-DAY5-NISSU1
067700         MOVE    ZERO              TO    LNK-DAY5-NISSU2
067800         MOVE    20                TO    STS-DAY-RC1
067900         GO  TO      DTKIKN-EXT
           END-IF.
068000*
068100     MOVE    ZERO                  TO    WK-NISU.
068200*
068300************
068400 DTKIKN-100.
068500************
068600     IF  WK-ST  +  10000  NOT >  WK-END
               IF       WK-ST2      <  0301
068800             MOVE     WK-ST      TO  WK-YYMMDD
               ELSE
068700             COMPUTE  WK-ST1  =  WK-ST1  +  1
068800             MOVE     WK-ST      TO  WK-YYMMDD
               END-IF
068900         PERFORM  URUDOSHI-CHK-RTN
069000         IF  WK-UERR  =  1
069100             COMPUTE  WK-NISU =  WK-NISU +  366
069200           ELSE
069300             COMPUTE  WK-NISU =  WK-NISU +  365
069400         END-IF
               IF       WK-ST2      <  0301
                   COMPUTE  WK-ST1  =  WK-ST1  +  1
               END-IF
069500         GO  TO      DTKIKN-100
           END-IF.
069600     IF  WK-ST  =  WK-END
069700         GO  TO      DTKIKN-200
           END-IF.
069800     MOVE    WK-ST1        TO    KISAN-YY.
069900     MOVE    1             TO    KISAN-MM  KISAN-DD.
070000     MOVE    WK-ST         TO    SYURYO-YMD.
070100     PERFORM  NISSU-SANSYUTS-RTN.
070200     COMPUTE  WK-NISU =  WK-NISU  -  WK-SANSYUTS.
070300     MOVE    WK-END        TO    SYURYO-YMD.
070400     PERFORM  NISSU-SANSYUTS-RTN.
070500     COMPUTE  WK-NISU =  WK-NISU  +  WK-SANSYUTS.
070600************
070700 DTKIKN-200.
070800************
070900     MOVE    WK-NISU       TO    LNK-DAY5-NISSU1.
071000     COMPUTE  WK-NISU =  WK-NISU  +  1.
071100     MOVE    WK-NISU       TO    LNK-DAY5-NISSU2.
071200 DTKIKN-EXT.
071300     EXIT.
071400******************************************************************
071500*        該当日算出処理                     (IRAI = 61 / 62)     *
071600******************************************************************
071700 DTPLMN-RTN          SECTION.
071800 DTPLMN-000.
071900*
072000*    *受取日付の西暦 OR 和暦判定
072100     IF  LNK-DAY6-IRAI   =   "61"
072200*        *西暦日付による場合
072300         MOVE    LNK-DAY6-KIJUN    TO    WK-YYMMDD
072400         PERFORM  YMD-CHK-RTN
072500         IF  WK-ERR  =  1
072600             MOVE    20            TO    STS-DAY-RC1
072700             GO  TO      DTPLMN-EXT
072800         END-IF
072900     ELSE
073000*        *和暦日付による場合
073100         MOVE    LNK-DAY6-IRAI     TO    LNK-DAY1-IRAI
073200         MOVE    LNK-DAY6-KIJUN    TO    LNK-DAY1-YMD
073300         PERFORM  DTCONV-RTN
073400         MOVE    SPACE             TO    LNK-DAY1-AREA
073500         IF  STS-DAY-RC1  NOT =  ZERO
073600             GO  TO      DTPLMN-EXT
073700         END-IF
073800     END-IF.
073900*
074000*    *日数パターン判定
074100     EVALUATE    LNK-DAY6-ZOGENPTN
074200         WHEN    "1"
074300                 PERFORM  DTPLMN-DAY-RTN
074400         WHEN    "2"
074500                 PERFORM  DTPLMN-MON-RTN
074600         WHEN    "3"
074700                 PERFORM  DTPLMN-NEN-RTN
074800         WHEN    OTHER
074900                 MOVE    30        TO    STS-DAY-RC1
075000                 MOVE    03        TO    STS-DAY-RC2
075100                 GO  TO      DTPLMN-EXT
075200     END-EVALUATE.
075300*
075400     IF  LNK-DAY6-IRAI   =   "62"
075500*        *和暦日付による受取りの場合
075600         MOVE    "61"              TO    LNK-DAY1-IRAI
075700         MOVE    LNK-DAY6-KOYOMI   TO    LNK-DAY1-YMD
075800         PERFORM  DTCONV-RTN
075900         MOVE    SPACE             TO    LNK-DAY1-AREA
076000         IF  STS-DAY-RC1  =  ZERO
076100             MOVE  WK-YYMMDD       TO    LNK-DAY6-KOYOMI
076200         ELSE
076300             MOVE  ZERO            TO    LNK-DAY6-KOYOMI
076400             GO  TO      DTPLMN-EXT
076500         END-IF
076600*
076700         MOVE    "61"                TO    LNK-DAY1-IRAI
076800         MOVE    LNK-DAY6-KOYOMIZEN  TO    LNK-DAY1-YMD
076900         PERFORM  DTCONV-RTN
077000         MOVE    SPACE               TO    LNK-DAY1-AREA
077100         IF  STS-DAY-RC1  =  ZERO
077200             MOVE  WK-YYMMDD         TO    LNK-DAY6-KOYOMIZEN
077300         ELSE
077400             MOVE  ZERO              TO    LNK-DAY6-KOYOMIZEN
077500             GO  TO      DTPLMN-EXT
077600         END-IF
077700*
077800         MOVE    "61"                TO    LNK-DAY1-IRAI
077900         MOVE    LNK-DAY6-KOYOMIGO   TO    LNK-DAY1-YMD
078000         PERFORM  DTCONV-RTN
078100         MOVE    SPACE               TO    LNK-DAY1-AREA
078200         IF  STS-DAY-RC1  =  ZERO
078300             MOVE  WK-YYMMDD         TO    LNK-DAY6-KOYOMIGO
078400         ELSE
078500             MOVE  ZERO              TO    LNK-DAY6-KOYOMIGO
078600             GO  TO      DTPLMN-EXT
078700         END-IF
078800*
078900         MOVE    "61"                TO    LNK-DAY1-IRAI
079000         MOVE    LNK-DAY6-KEISAN     TO    LNK-DAY1-YMD
079100         PERFORM  DTCONV-RTN
079200         MOVE    SPACE               TO    LNK-DAY1-AREA
079300         IF  STS-DAY-RC1  =  ZERO
079400             MOVE  WK-YYMMDD         TO    LNK-DAY6-KEISAN
079500         ELSE
079600             MOVE  ZERO              TO    LNK-DAY6-KEISAN
079700             GO  TO      DTPLMN-EXT
079800         END-IF
079900*
080000         MOVE    "61"                TO    LNK-DAY1-IRAI
080100         MOVE    LNK-DAY6-KEISANZEN  TO    LNK-DAY1-YMD
080200         PERFORM  DTCONV-RTN
080300         MOVE    SPACE               TO    LNK-DAY1-AREA
080400         IF  STS-DAY-RC1  =  ZERO
080500             MOVE  WK-YYMMDD         TO    LNK-DAY6-KEISANZEN
080600         ELSE
080700             MOVE  ZERO              TO    LNK-DAY6-KEISANZEN
080800             GO  TO      DTPLMN-EXT
080900         END-IF
081000*
081100         MOVE    "61"                TO    LNK-DAY1-IRAI
081200         MOVE    LNK-DAY6-KEISANGO   TO    LNK-DAY1-YMD
081300         PERFORM  DTCONV-RTN
081400         MOVE    SPACE               TO    LNK-DAY1-AREA
081500         IF  STS-DAY-RC1  =  ZERO
081600             MOVE  WK-YYMMDD         TO    LNK-DAY6-KEISANGO
081700         ELSE
081800             MOVE  ZERO              TO    LNK-DAY6-KEISANGO
081900             GO  TO      DTPLMN-EXT
082000         END-IF
082100     END-IF.
082200 DTPLMN-EXT.
082300     EXIT.
082400******************************************************************
082500*        日数による該当日算出処理
082600******************************************************************
082700 DTPLMN-DAY-RTN      SECTION.
082800 DTPLMN-DAY-000.
082900*    *計算日
083000     MOVE     LNK-DAY6-ZOGEN        TO    WK-NISU.
083100     PERFORM  NISSU-KANSAN-RTN.
083200     MOVE     WK-YYMMDD             TO    LNK-DAY6-KEISAN.
083300     PERFORM  YSK-KBN-RTN.
083400     IF  YSK-KYUJTS   NOT =   1
083500         MOVE     WK-YYMMDD         TO    LNK-DAY6-KEISANZEN
083600                                          LNK-DAY6-KEISANGO
083700         GO  TO    DTPLMN-DAY-100
           END-IF.
083800*
083900     PERFORM  DT-YUKOMNS-RTN.
084000     MOVE     WK-YUKO               TO    LNK-DAY6-KEISANZEN.
084100     PERFORM  DT-YUKOPLS-RTN.
084200     MOVE     WK-YUKO               TO    LNK-DAY6-KEISANGO.
084300****************
084400 DTPLMN-DAY-100.
084500****************
084600*    *暦上日
084700     MOVE     LNK-DAY6-KEISAN       TO    WK-MNS-YMD.
084800     PERFORM  DT-MINUS-RTN.
084900     MOVE     WK-MNS-YMD            TO    LNK-DAY6-KOYOMI
085000                                          WK-YYMMDD.
085100     PERFORM  YSK-KBN-RTN.
085200     IF  YSK-KYUJTS   NOT =   1
085300         MOVE     WK-YYMMDD         TO    LNK-DAY6-KOYOMIZEN
085400                                          LNK-DAY6-KOYOMIGO
085500         GO  TO    DTPLMN-DAY-EXT
           END-IF.
085600*
085700     PERFORM  DT-YUKOMNS-RTN.
085800     MOVE     WK-YUKO               TO    LNK-DAY6-KOYOMIZEN.
085900     PERFORM  DT-YUKOPLS-RTN.
086000     MOVE     WK-YUKO               TO    LNK-DAY6-KOYOMIGO.
086100 DTPLMN-DAY-EXT.
086200     EXIT.
086300******************************************************************
086400*        月数による該当日算出処理
086500******************************************************************
086600 DTPLMN-MON-RTN      SECTION.
086700 DTPLMN-MON-000.
086800     MOVE    LNK-DAY6-ZOGEN      TO    WK-NISU.
086900     MOVE    WK-NISU       TO    WK-SUUTI.
087000     DIVIDE  WK-SUUTI  BY  12    GIVING      WK-SYO
087100                                 REMAINDER   WK-AMARI.
087200     IF  WK-NISU  <  ZERO
087300         IF  WK-AMARI  <  WK-MM
087400             COMPUTE   WK-MM     =  WK-MM    - WK-AMARI
087500         ELSE
087600             COMPUTE   WK-AMARI  =  WK-AMARI - WK-MM
087700             COMPUTE   WK-MM     =  12       - WK-AMARI
087800             COMPUTE   WK-SYO    =  WK-SYO   + 1
087900         END-IF
088000         COMPUTE   WK-SYY   =    WK-SYY    -   WK-SYO
088100     ELSE
088200         ADD       WK-MM         TO    WK-AMARI
088300         IF  WK-AMARI  >  12
088400             COMPUTE   WK-AMARI  =  WK-AMARI - 12
088500             MOVE      WK-AMARI  TO    WK-MM
088600             COMPUTE   WK-SYO    =  WK-SYO   + 1
088700         ELSE
088800             MOVE      WK-AMARI  TO    WK-MM
088900         END-IF
089000         COMPUTE   WK-SYY   =    WK-SYY    +   WK-SYO
089100     END-IF.
089200*
089300     MOVE    WK-YYMMDD           TO    WK-YUKO.
089400     PERFORM  DT-MMLAST-RTN.
089500     IF  WK-DD  >  WK-YUKO-DD
089600         MOVE     WK-YUKO-DD     TO    WK-DD
           END-IF.
089700****************
089800 DTPLMN-MON-050.
089900****************
090000*    *計算日
090100     MOVE     WK-YYMMDD         TO    LNK-DAY6-KEISAN.
090200     PERFORM  YSK-KBN-RTN.
090300     IF  YSK-KYUJTS   NOT =   1
090400         MOVE     WK-YYMMDD     TO    LNK-DAY6-KEISANZEN
090500                                      LNK-DAY6-KEISANGO
090600         GO  TO    DTPLMN-MON-100
           END-IF.
090700*
090800     PERFORM  DT-YUKOMNS-RTN.
090900     MOVE     WK-YUKO           TO    LNK-DAY6-KEISANZEN.
091000     PERFORM  DT-YUKOPLS-RTN.
091100     MOVE     WK-YUKO           TO    LNK-DAY6-KEISANGO.
091200****************
091300 DTPLMN-MON-100.
091400****************
091500*    *暦上日
091600     MOVE     LNK-DAY6-KEISAN   TO    WK-MNS-YMD.
091700     PERFORM  DT-MINUS-RTN.
091800     MOVE     WK-MNS-YMD        TO    LNK-DAY6-KOYOMI
091900                                      WK-YYMMDD.
092000     PERFORM  YSK-KBN-RTN.
092100     IF  YSK-KYUJTS   NOT =   1
092200         MOVE     WK-YYMMDD     TO    LNK-DAY6-KOYOMIZEN
092300                                      LNK-DAY6-KOYOMIGO
092400         GO  TO    DTPLMN-MON-EXT
           END-IF.
092500*
092600     PERFORM  DT-YUKOMNS-RTN.
092700     MOVE     WK-YUKO           TO    LNK-DAY6-KOYOMIZEN.
092800     PERFORM  DT-YUKOPLS-RTN.
092900     MOVE     WK-YUKO           TO    LNK-DAY6-KOYOMIGO.
093000 DTPLMN-MON-EXT.
093100     EXIT.
093200******************************************************************
093300*        年数による該当日算出処理
093400******************************************************************
093500 DTPLMN-NEN-RTN      SECTION.
093600 DTPLMN-NEN-000.
093700     MOVE    LNK-DAY6-ZOGEN      TO    WK-NISU.
093800     COMPUTE    WK-SYY      =    WK-SYY    +   LNK-DAY6-ZOGEN
093900*
094000     MOVE    WK-YYMMDD           TO    WK-YUKO.
094100     PERFORM  DT-MMLAST-RTN.
094200     IF  WK-DD  >  WK-YUKO-DD
094300         MOVE     WK-YUKO-DD     TO    WK-DD
           END-IF.
094400****************
094500 DTPLMN-NEN-050.
094600****************
094700*    *計算日
094800     MOVE     WK-YYMMDD          TO    LNK-DAY6-KEISAN.
094900     PERFORM  YSK-KBN-RTN.
095000     IF  YSK-KYUJTS   NOT =   1
095100         MOVE     WK-YYMMDD      TO    LNK-DAY6-KEISANZEN
095200                                       LNK-DAY6-KEISANGO
095300         GO  TO    DTPLMN-NEN-100
           END-IF.
095400*
095500     PERFORM  DT-YUKOMNS-RTN.
095600     MOVE     WK-YUKO            TO    LNK-DAY6-KEISANZEN.
095700     PERFORM  DT-YUKOPLS-RTN.
095800     MOVE     WK-YUKO            TO    LNK-DAY6-KEISANGO.
095900****************
096000 DTPLMN-NEN-100.
096100****************
096200*    *暦上日
096300     MOVE     LNK-DAY6-KEISAN    TO    WK-MNS-YMD.
096400     PERFORM  DT-MINUS-RTN.
096500     MOVE     WK-MNS-YMD         TO    LNK-DAY6-KOYOMI
096600                                       WK-YYMMDD.
096700     PERFORM  YSK-KBN-RTN.
096800     IF  YSK-KYUJTS   NOT =   1
096900         MOVE     WK-YYMMDD      TO    LNK-DAY6-KOYOMIZEN
097000                                       LNK-DAY6-KOYOMIGO
097100         GO  TO    DTPLMN-NEN-EXT
           END-IF.
097200*
097300     PERFORM  DT-YUKOMNS-RTN.
097400     MOVE     WK-YUKO            TO    LNK-DAY6-KOYOMIZEN.
097500     PERFORM  DT-YUKOPLS-RTN.
097600     MOVE     WK-YUKO            TO    LNK-DAY6-KOYOMIGO.
097700 DTPLMN-NEN-EXT.
097800     EXIT.
097900******************************************************************
098000*        月末日算出処理                     (IRAI = 71 / 72)     *
098100******************************************************************
098200 DTMEND-RTN          SECTION.
098300 DTMEND-000.
098400*
098500*    *受取日付の西暦 OR 和暦判定
098600     IF  LNK-DAY7-IRAI   =   "71"
098700*        *西暦日付による場合
098800         MOVE    LNK-DAY7-YM       TO    WK-YYYYMM
098900         MOVE    01                TO    WK-DD
099000         PERFORM  YMD-CHK-RTN
099100         IF  WK-ERR  =  1
099200             MOVE    20            TO    STS-DAY-RC1
099300             GO  TO      DTMEND-EXT
099400         END-IF
099500     ELSE
099600*        *和暦日付による場合
099700         MOVE    LNK-DAY7-IRAI     TO    LNK-DAY1-IRAI
099800         MOVE    LNK-DAY7-YM       TO    LNK-DAY1-YMD
099900         COMPUTE  LNK-DAY1-YMD   =  LNK-DAY1-YMD  *  100  +  1
100000         PERFORM  DTCONV-RTN
100100         MOVE    SPACE             TO    LNK-DAY1-AREA
100200         IF  STS-DAY-RC1  NOT =  ZERO
100300             GO  TO      DTMEND-EXT
100400         END-IF
100500     END-IF.
100600*
100700     IF  WK-MM  =  4  OR  6  OR  9  OR  11
100800         MOVE    30              TO    WK-DD
100900     ELSE
101000         IF  WK-MM  =  2
101100             MOVE    29              TO    WK-DD
101200         ELSE
101300             MOVE    31              TO    WK-DD
               END-IF
           END-IF.
101400     PERFORM  YMD-CHK-RTN.
101500     IF  WK-ERR  =  1
101600         IF  WK-MM  =  2
101700             MOVE    28              TO    WK-DD
101800             PERFORM  YMD-CHK-RTN
101900             IF  WK-ERR  =  1
102000                 MOVE    20          TO    STS-DAY-RC1
102100                 GO  TO  DTMEND-EXT
102200             ELSE
102300                 CONTINUE
                   END-IF
102400         ELSE
102500             MOVE    20              TO    STS-DAY-RC1
102600             GO  TO  DTMEND-EXT
               END-IF
           END-IF.
102700*
102800     PERFORM  YSK-KBN-RTN.
102900     MOVE    WK-YYMMDD       TO    LNK-DAY7-KOYOMI.
103000     IF  YSK-KYUJTS  =  1
103100         PERFORM  DT-YUKOMNS-RTN
103200         MOVE     WK-YUKO    TO    WK-YYMMDD
           END-IF.
103300     MOVE    WK-YYMMDD       TO    LNK-DAY7-EIGYO.
103400*
103500     IF  LNK-DAY7-IRAI   =   "72"
103600*        *和暦日付による受取りの場合
103700         MOVE    "71"              TO    LNK-DAY1-IRAI
103800         MOVE    LNK-DAY7-KOYOMI   TO    LNK-DAY1-YMD
103900         PERFORM  DTCONV-RTN
104000         MOVE    SPACE             TO    LNK-DAY1-AREA
104100         IF      STS-DAY-RC1  =  ZERO
104200             MOVE  WK-YYMMDD       TO    LNK-DAY7-KOYOMI
104300         ELSE
104400             MOVE  ZERO            TO    LNK-DAY7-KOYOMI
104500             GO  TO      DTMEND-EXT
104600         END-IF
104700*
104800         MOVE    "71"              TO    LNK-DAY1-IRAI
104900         MOVE    LNK-DAY7-EIGYO    TO    LNK-DAY1-YMD
105000         PERFORM  DTCONV-RTN
105100         MOVE    SPACE             TO    LNK-DAY1-AREA
105200         IF      STS-DAY-RC1  =  ZERO
105300             MOVE  WK-YYMMDD       TO    LNK-DAY7-EIGYO
105400         ELSE
105500             MOVE  ZERO            TO    LNK-DAY7-EIGYO
105600             GO  TO      DTMEND-EXT
105700         END-IF
105800     END-IF.
105900 DTMEND-EXT.
106000     EXIT.
106100******************************************************************
106200*        日付チェック処理
106300******************************************************************
106400 YMD-CHK-RTN         SECTION.
106500 YMD-CHK-000.
106600*
106700     MOVE    ZERO                TO    WK-ERR.
106800*
106900     IF  WK-MM   IS  NOT  NUMERIC
107000         MOVE    1               TO    WK-ERR
107100         GO  TO    YMD-CHK-EXT
           END-IF.
      *---- (01.00.02) LINE ADD START ----------------------------------
106900     IF  WK-DD   IS  NOT  NUMERIC
107000         MOVE    1               TO    WK-ERR
107100         GO  TO    YMD-CHK-EXT
           END-IF.
      *---- (01.00.02) LINE ADD END ------------------------------------
107200     IF  (WK-MM  <  1)  OR  (WK-MM  >  12)
107300         MOVE    1               TO    WK-ERR
107400         GO  TO    YMD-CHK-EXT
           END-IF.
107500*
107600     IF  WK-MM  =  2
107700         GO  TO    YMD-CHK-100
           END-IF.
107800*
107900     IF  WK-MM  =  4  OR  6  OR  9  OR  11
108000         IF  (WK-DD  <  1)  OR  (WK-DD  >  30)
108100             GO  TO    YMD-CHK-200
108200         ELSE
108300             GO  TO    YMD-CHK-EXT
               END-IF
108400     ELSE
108500         IF  (WK-DD  <  1)  OR  (WK-DD  >  31)
108600             GO  TO    YMD-CHK-200
108700         ELSE
108800             GO  TO    YMD-CHK-EXT
               END-IF
           END-IF.
108900*************
109000 YMD-CHK-100.
109100*************
109200     PERFORM  URUDOSHI-CHK-RTN.
109300     IF  WK-UERR  =  ZERO
109400         IF  (WK-DD  <  1)  OR  (WK-DD  >  28)
109500             GO  TO    YMD-CHK-200
109600         ELSE
109700             GO  TO    YMD-CHK-EXT
               END-IF
109800     ELSE
109900         IF  (WK-DD  <  1)  OR  (WK-DD  >  29)
110000             GO  TO    YMD-CHK-200
110100         ELSE
110200             GO  TO    YMD-CHK-EXT
               END-IF
           END-IF.
110300*************
110400 YMD-CHK-200.
110500*************
110600     MOVE    1              TO    WK-ERR.
110700 YMD-CHK-EXT.
110800     EXIT.
110900******************************************************************
111000*        潤年判定処理
111100******************************************************************
111200 URUDOSHI-CHK-RTN    SECTION.
111300 URUDOSHI-CHK-000.
111400     MOVE    ZERO           TO    WK-UERR.
111500*
111600*     COMPUTE  WK-UERR  =  WK-SYY  -  (WK-SYY  /  4  *  4).
           DIVIDE  4  INTO  WK-SYY  GIVING WK-DUMMYN
                      REMAINDER WK-UERR.
111700     IF  WK-UERR  =  ZERO
111800*         COMPUTE  WK-UERR  =  WK-SYY  -  (WK-SYY  /  100  *  100)
               DIVIDE  100  INTO  WK-SYY  GIVING WK-DUMMYN
                         REMAINDER WK-UERR
111900         IF  WK-UERR  =  ZERO
112000*             COMPUTE  WK-UERR  =
112100*                      WK-SYY  -  (WK-SYY  /  400  *  400)
                   DIVIDE  400  INTO  WK-SYY  GIVING WK-DUMMYN
                             REMAINDER WK-UERR
112200             IF  WK-UERR  NOT =  ZERO
112300                 MOVE    ZERO              TO  WK-UERR
112400             ELSE
112500                 MOVE     1                TO  WK-UERR
112600             END-IF
112700         ELSE
112800             MOVE     1          TO  WK-UERR
112900         END-IF
113000     ELSE
113100         MOVE        ZERO        TO  WK-UERR
113200     END-IF.
113300 URUDOSHI-CHK-EXT.
113400     EXIT.
113500******************************************************************
113600*        曜日・祝日区分・休日区分  算出処理
113700******************************************************************
113800 YSK-KBN-RTN         SECTION.
113900 YSK-KBN-000.
114000*    祝日（春分の日，秋分の日）算出
114100     PERFORM SKJT-KEISAN1-RTN.
114200*    祝日（成人の日，体育の日）算出
114300     PERFORM SKJT-KEISAN2-RTN.
      *    祝日（海の日，敬老の日）算出
           PERFORM SKJT-KEISAN3-SEC
114400*
114500     MOVE    ZERO          TO    YSK-SYUKJTS   YSK-KYUJTS.
114600     MOVE    WK-YYMMDD     TO    WK-YOUB.
114700     PERFORM  TABLE-SRH-RTN.
114800     IF  YSK-SYUKJTS  =  ZERO
114900         IF  YSK-YOUBI  =  1
115000             MOVE    WK-YOUB       TO    WK-MNS-YMD
115100             PERFORM  DT-MINUS-RTN
115200             MOVE    WK-MNS-YMD    TO    WK-YOUB
115300             PERFORM  TABLE-SRH-RTN
               END-IF
           END-IF.
115400     IF  YSK-YOUBI  =  ZERO
115500         MOVE    1                 TO    YSK-KYUJTS
115600     ELSE
115700         IF  YSK-SYUKJTS  =  1
115800             MOVE    1             TO    YSK-KYUJTS
115900         ELSE
116000             SET     IX4    TO     1
116100             SEARCH  KYUJT-TBL     VARYING  IX4
116200               AT  END
116300                 MOVE    ZERO      TO    YSK-KYUJTS
116400               WHEN
116500                 KYUJT-T2 (IX4)  =  WK-SMD
116600                 MOVE    1         TO  YSK-KYUJTS
                   END-SEARCH
               END-IF
           END-IF.
116700*
116800*    IF  WK-SYYMMDD  >  "19830731"
116900*        IF  YSK-YOUBI  =  6
117000*            IF  (WK-DD  >  7)  AND  (WK-DD  <  15)
117100*                MOVE    1             TO    YSK-KYUJTS.
117200*    IF  WK-SYYMMDD  >  "19860731"
117300*        IF  YSK-YOUBI  =  6
117400*            IF  (WK-DD  >  7)  AND  (WK-DD  <  22)
117500*                MOVE    1             TO    YSK-KYUJTS.
117600*    IF  WK-SYYMMDD  >  "19890131"
117700*        IF  YSK-YOUBI  =  6
117800*            MOVE    1             TO    YSK-KYUJTS.
117900 YSK-KBN-EXT.
118000     EXIT.
118100******************************************************************
118200*        テーブル検索処理
118300******************************************************************
118400 TABLE-SRH-RTN       SECTION.
118500 TABLE-SRH-000.
      *
           EVALUATE    TRUE
               WHEN    WK-SYY  <  2000
                   SET    IX2    TO     1
                   SEARCH  SKJT-TBL     VARYING  IX2
                     AT  END
                         MOVE    ZERO      TO    YSK-SYUKJTS
                     WHEN
                         SKJT-T1 (IX2)  =  WYOUB-MMDD
                         MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
      *
               WHEN   (WK-SYY  >= 2000   )  AND
                      (WK-SYY  <  2003   )
                   SET    IX6    TO     1
                   SEARCH  SKJT-TBLNEW   VARYING   IX6
                     AT  END
                         MOVE    ZERO      TO    YSK-SYUKJTS
                     WHEN
                         SKJT-NEW(IX6)     =   WYOUB-MMDD
                         MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
      *
               WHEN   (WK-SYY  >= 2003   )
                   SET    IX7    TO     1
                   SEARCH  SKJT-TBLNEW2  VARYING   IX7
                     AT  END
                         MOVE    ZERO      TO    YSK-SYUKJTS
                     WHEN
                         SKJT-NEW2(IX7)    =   WYOUB-MMDD
                         MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
           END-EVALUATE
      *
      *    春分の日・秋分の日
           IF  YSK-SYUKJTS  =  ZERO
               SET     IX3    TO     1
               SEARCH  SKJT-TBL2     VARYING  IX3
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL2 (IX3)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
           END-IF.
      *    成人の日・体育の日
           IF  (YSK-SYUKJTS  =   ZERO ) AND
               (WK-SYY       >=  2000 )
               SET     IX5    TO     1
               SEARCH  SKJT-TBL3     VARYING  IX5
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL3 (IX5)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
                END-SEARCH
           END-IF.
      *    海の日・敬老の日
           IF  (YSK-SYUKJTS  =   ZERO ) AND
               (WK-SYY       >=  2003 )
               SET     IX8    TO     1
               SEARCH  SKJT-TBL4     VARYING  IX8
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL4 (IX8)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
                END-SEARCH
           END-IF.
           .
123400**
123500 TABLE-SRH-EXT.
123600     EXIT.
123700******************************************************************
123800*        前日算出処理
123900******************************************************************
124000 DT-MINUS-RTN        SECTION.
124100 DT-MINUS-000.
124200     IF  WK-MNS-DD  >  1
124300         COMPUTE  WK-MNS-DD  =  WK-MNS-DD  -  1
124400     ELSE
124500         IF  WK-MNS-MM  =  1
124600             MOVE    12            TO    WK-MNS-MM
124700             COMPUTE  WK-MNS-YY  =  WK-MNS-YY  -  1
124800             MOVE    31            TO    WK-MNS-DD
124900         ELSE
125000             COMPUTE  WK-MNS-MM  =  WK-MNS-MM  -  1
125100             MOVE    WK-MNS-YMD    TO    WK-YUKO
125200             PERFORM  DT-MMLAST-RTN
125300             MOVE    WK-YUKO-DD    TO    WK-MNS-DD
               END-IF
           END-IF.
125400 DT-MINUS-EXT.
125500     EXIT.
125600******************************************************************
125700*        月末日算出処理
125800******************************************************************
125900 DT-MMLAST-RTN       SECTION.
126000 DT-MMLAST-000.
126100     IF  WK-YUKO-MM  =  2
126200         PERFORM  URUDOSHI-CHK-RTN
126300         IF  WK-UERR  =  1
126400             MOVE    29            TO    CAL-T (2)
126500         ELSE
126600             MOVE    28            TO    CAL-T (2)
               END-IF
           END-IF.
126700     MOVE    CAL-T (WK-YUKO-MM)    TO    WK-YUKO-DD.
126800 DT-MMLAST-EXT.
126900     EXIT.
127000******************************************************************
127100*        前日有効日算出処理
127200******************************************************************
127300 DT-YUKOMNS-RTN      SECTION.
127400 DT-YUKOMNS-000.
127500     IF  WK-DD  =  1
127600         IF  WK-MM  =  1
127700             MOVE    12            TO    WK-MM
127800             MOVE    31            TO    WK-DD
127900             COMPUTE  WK-SYY =  WK-SYY -  1
128000         ELSE
128100             COMPUTE  WK-MM  =  WK-MM  -  1
128200             MOVE    WK-YYMMDD     TO    WK-YUKO
128300             PERFORM  DT-MMLAST-RTN
128400             MOVE    WK-YUKO-DD    TO    WK-DD
               END-IF
128500     ELSE
128600         COMPUTE  WK-DD  =  WK-DD  -  1
           END-IF.
128700     PERFORM  YSK-KBN-RTN.
128800     IF  YSK-KYUJTS  =  1
128900         GO  TO    DT-YUKOMNS-000
           END-IF.
129000     MOVE    WK-YYMMDD     TO    WK-YUKO.
129100 DT-YUKOMNS-EXT.
129200     EXIT.
129300******************************************************************
129400*        翌日有効日算出処理
129500******************************************************************
129600 DT-YUKOPLS-RTN      SECTION.
129700 DT-YUKOPLS-000.
129800     MOVE    WK-YYMMDD     TO    WK-YUKO.
129900     PERFORM  DT-MMLAST-RTN.
130000     IF  WK-DD  =  WK-YUKO-DD
130100         IF  WK-MM  =  12
130200             MOVE    1             TO    WK-MM  WK-DD
130300             COMPUTE  WK-SYY =  WK-SYY +  1
130400         ELSE
130500             COMPUTE  WK-MM  =  WK-MM  +  1
130600             MOVE    1             TO    WK-DD
               END-IF
130700     ELSE
130800         COMPUTE  WK-DD  =  WK-DD  +  1
           END-IF.
130900     PERFORM  YSK-KBN-RTN.
131000     IF  YSK-KYUJTS  =  1
131100         GO  TO    DT-YUKOPLS-000
           END-IF.
131200     MOVE    WK-YYMMDD     TO    WK-YUKO.
131300 DT-YUKOPLS-EXT.
131400     EXIT.
131500******************************************************************
131600*        日数算出処理
131700******************************************************************
131800 NISSU-SANSYUTS-RTN  SECTION.
131900 NISSU-SANSYUTS-000.
132000     MOVE    NISU-T (KISAN-MM)   TO    NISU-A.
132100     IF  KISAN-MM  >  2
132200         MOVE    KISAN-YMD       TO    WK-YYMMDD
132300         PERFORM  URUDOSHI-CHK-RTN
132400         IF  WK-UERR  =  1
132500             COMPUTE  NISU-A  =  NISU-A  +  1
               END-IF
           END-IF.
132600     COMPUTE  NISU-A  =  NISU-A  +  KISAN-DD.
132700     MOVE    ZERO            TO  NISU-B.
132800     MOVE    KISAN-YMD       TO  WK-YYMMDD.
132900********************
133000 NISSU-SANSYUTS-100.
133100********************
133200     IF  WK-SYY  =  SYURYO-YY
133300         GO  TO    NISSU-SANSYUTS-200
           END-IF.
133400     PERFORM  URUDOSHI-CHK-RTN.
133500     IF  WK-UERR  =  1
133600         COMPUTE  NISU-B  =  NISU-B  +  366
133700     ELSE
133800         COMPUTE  NISU-B  =  NISU-B  +  365
           END-IF.
133900     COMPUTE  WK-SYY  =  WK-SYY  +  1.
134000     GO  TO    NISSU-SANSYUTS-100.
134100********************
134200 NISSU-SANSYUTS-200.
134300********************
134400     COMPUTE  NISU-B  =  NISU-B  +  NISU-T (SYURYO-MM)
134500     IF  SYURYO-MM  >  2
134600         PERFORM  URUDOSHI-CHK-RTN
134700         IF  WK-UERR  =  1
134800             COMPUTE  NISU-B  =  NISU-B  +  1
               END-IF
           END-IF.
134900     COMPUTE  NISU-B  =  NISU-B  +  SYURYO-DD.
135000     COMPUTE  WK-SANSYUTS  =  NISU-B  -  NISU-A.
135100 NISSU-SANSYUTS-EXT.
135200     EXIT.
135300******************************************************************
135400*        日数換算処理
135500******************************************************************
135600 NISSU-KANSAN-RTN    SECTION.
135700 NISSU-KANSAN-000.
135800     IF  WK-NISU  <  ZERO
135900         COMPUTE  KIJYUN-A  =  WK-NISU  /  365
136000         COMPUTE  KIJYUN-YY =  WK-SYY   +  KIJYUN-A  -  1
136100     ELSE
136200         MOVE    WK-SYY          TO    KIJYUN-YY
           END-IF.
136300     MOVE    WK-YYMMDD           TO  SYURYO-YMD.
136400     MOVE    KIJYUN-YY           TO  KISAN-YY.
136500     MOVE    1                   TO  KISAN-MM.
136600     MOVE    ZERO                TO  KISAN-DD.
136700     PERFORM  NISSU-SANSYUTS-RTN.
136800     COMPUTE  WK-SANSYUTS  =  WK-SANSYUTS  +  WK-NISU.
136900******************
137000 NISSU-KANSAN-100.
137100******************
137200     MOVE    KIJYUN-YY       TO  WK-SYY.
137300     PERFORM  URUDOSHI-CHK-RTN.
137400     IF  WK-UERR  =  1
137500         MOVE    366             TO    SYU-W
137600     ELSE
137700         MOVE    365             TO    SYU-W
           END-IF.
137800     IF  WK-SANSYUTS  >  SYU-W
137900         COMPUTE  WK-SANSYUTS   =  WK-SANSYUTS   -  SYU-W
138000         COMPUTE  KIJYUN-YY     =  KIJYUN-YY  +  1
138100         GO  TO    NISSU-KANSAN-100
           END-IF.
138200     IF  WK-UERR  =  1
138300         MOVE    29              TO    CAL-T (2)
138400     ELSE
138500         MOVE    28              TO    CAL-T (2)
           END-IF.
138600     MOVE    1                   TO  WK-MM.
138700******************
138800 NISSU-KANSAN-200.
138900******************
139000     IF  CAL-T (WK-MM)  <  WK-SANSYUTS
139100         COMPUTE  WK-SANSYUTS  =  WK-SANSYUTS  -  CAL-T (WK-MM)
139200         COMPUTE  WK-MM  =  WK-MM  +  1
139300         IF  WK-MM  >  12
139400             MOVE    1           TO   WK-MM
139500             MOVE    1           TO   WK-DD
139600             GO  TO    NISSU-KANSAN-200
139700         ELSE
139800             GO  TO    NISSU-KANSAN-200
               END-IF
139900     ELSE
140000         MOVE    WK-SANSYUTS     TO  WK-DD
           END-IF.
140100 NISSU-KANSAN-EXT.
140200     EXIT.
140300******************************************************************
140400*    春分の日・秋分の日を算出する                                *
140500******************************************************************
140600 SKJT-KEISAN1-RTN  SECTION.
140700 SKJT-KEISAN1-000.
140800     IF  WK-MM  <  3
140900         COMPUTE  WK-YSK-YYYY  =  WK-SYY   -  1
141000         COMPUTE  WK-YSK-MM    =  WK-MM  +  10
141100     ELSE
141200         MOVE    WK-SYY          TO    WK-YSK-YYYY
141300         COMPUTE  WK-YSK-MM    =  WK-MM  -  2
           END-IF.
141400     COMPUTE  WK-SZ       =  2.6  *  WK-YSK-MM  -  0.2.
141500     COMPUTE  WK-SY       =  (WK-YSK-YYYY  -  1900)  *  0.25.
141600     COMPUTE  WK-SY       =  WK-DD  +  WK-SZ  +  WK-SY  +
141700                             (WK-YSK-YYYY  -  1934).
141800*    COMPUTE  YSK-YOUBI   =  WK-SY  -  WK-SY  /  7  *  7.
           DIVIDE  7  INTO  WK-SY  GIVING  WK-DUMMYN
                      REMAINDER  YSK-YOUBI.
141900     COMPUTE  WK-WORKA    =  WK-SYY  -  1960.
142000     COMPUTE  WK-SSYB     =  WK-WORKA  *  CST1  +  CST2.
142100     COMPUTE  WK-SY       =  WK-WORKA  /  4.
142200*     COMPUTE  WK-SZ       =  (WK-WORKA  -  WK-WORKA  /  4  *  4).
           DIVIDE  4  INTO  WK-WORKA  GIVING  WK-DUMMYN
                      REMAINDER  WK-SZ.
142300     COMPUTE  WK-SSYB     =  WK-SSYB  -  (WK-SY  *  1461)  -
142400                             (WK-SZ  *  365).
142500     SET     IX3           TO    1.
142600     MOVE    3             TO    SKJT-M (IX3).
142700     MOVE    WK-SSYB       TO    SKJT-D (IX3)
142800                                 SKJT-KNRDD (3, 1).
142900*
143000     SET     IX3           TO    2.
143100     MOVE    9             TO    SKJT-M (IX3).
143200     COMPUTE  WK-SSYB     =  WK-SSYB  +  CST3.
           IF  WK-SYY  =  2012 
               MOVE  22  TO  WK-SSYB
           END-IF.
143300     MOVE    WK-SSYB       TO    SKJT-D (IX3)
143400                                 SKJT-KNRDD (9, 2).
143500 SKJT-KEISAN1-EXT.
143600     EXIT.
143700******************************************************************
143800*    成人の日  体育の日を算出する                                *
143900******************************************************************
144000 SKJT-KEISAN2-RTN  SECTION.
144100 SKJT-KEISAN2-000.
144200     SET     IX5              TO  1.
144300     MOVE    WK-SYY           TO  WK-SKJTYY.
144400     MOVE    1                TO  WK-SKJTDD.
144500 SKJT-KEISAN2-100.
144600     IF      IX5          =   1
144700          MOVE    1           TO  WK-SKJTMM
144800     ELSE
144900          MOVE    10          TO  WK-SKJTMM
145000     END-IF.
145100*
145200     IF    WK-SKJTMM      <   3
145300         COMPUTE  WK-YSK-YYYY     =   WK-SKJTYY   -   1
145400         COMPUTE  WK-YSK-MM       =   WK-SKJTMM   +   10
145500     ELSE
145600         MOVE    WK-SKJTYY            TO  WK-YSK-YYYY
145700         COMPUTE WK-YSK-MM        =   WK-SKJTMM   -   2
145800     END-IF.
145900     COMPUTE WK-SZ   =  2.6 * WK-YSK-MM  -  0.2.
146000     COMPUTE WK-SY   =  (WK-YSK-YYYY -  1900)  *  0.25.
146100     COMPUTE WK-SY   =  WK-SKJTDD  +  WK-SZ  +  WK-SY  +
146200                        (WK-YSK-YYYY    -   1934).
146300*     COMPUTE YSK-YOUBI2 =  WK-SY   -  WK-SY  /   7  *  7.
           DIVIDE  7  INTO  WK-SY  GIVING  WK-DUMMYN
                      REMAINDER  YSK-YOUBI2.
146400*
146500     MOVE    WK-SKJTMM          TO  SKJT-M3(IX5).
146600     EVALUATE     YSK-YOUBI2
146700       WHEN       0   MOVE  9   TO  SKJT-D3(IX5)
146800       WHEN       1   MOVE  8   TO  SKJT-D3(IX5)
146900       WHEN       2   MOVE 14   TO  SKJT-D3(IX5)
147000       WHEN       3   MOVE 13   TO  SKJT-D3(IX5)
147100       WHEN       4   MOVE 12   TO  SKJT-D3(IX5)
147200       WHEN       5   MOVE 11   TO  SKJT-D3(IX5)
147300       WHEN       6   MOVE 10   TO  SKJT-D3(IX5)
147400     END-EVALUATE.
147500*
147600     IF    WK-SKJTMM  =   1
147700           MOVE  SKJT-D3(IX5)   TO  SKJT-KNRDD (1, 2)
147800     END-IF.
147900     IF    WK-SKJTMM  =   10
148000           MOVE  SKJT-D3(IX5)   TO  SKJT-KNRDD (10, 1)
148100     END-IF.
148200*
148300     IF    IX5        =   2
148400         GO  TO  SKJT-KEISAN2-EXT
148500     ELSE
148600         SET     IX5      TO  2
148700         GO  TO  SKJT-KEISAN2-100
148800     END-IF.
148900 SKJT-KEISAN2-EXT.
149000     EXIT.
143700******************************************************************
143800*    海の日  敬老の日を算出する                                *
143900******************************************************************
       SKJT-KEISAN3-SEC  SECTION.
           SET     IX8              TO  1.
           MOVE    WK-SYY           TO  WK-SKJTYY
           MOVE    1                TO  WK-SKJTDD
           PERFORM VARYING  IDX     FROM    1   BY  1
                   UNTIL    IDX     >   2
               IF      IDX          =   1
                   MOVE    7            TO  WK-SKJTMM
               ELSE
                   MOVE    9            TO  WK-SKJTMM
               END-IF
      *
               SET     IX8              TO  IDX
               IF    WK-SKJTMM      <   3
                   COMPUTE  WK-YSK-YYYY     =   WK-SKJTYY   -   1
                   COMPUTE  WK-YSK-MM       =   WK-SKJTMM   +   10
               ELSE
                   MOVE    WK-SKJTYY            TO  WK-YSK-YYYY
                   COMPUTE WK-YSK-MM        =   WK-SKJTMM   -   2
               END-IF
               COMPUTE WK-SZ   =  2.6 * WK-YSK-MM  -  0.2
               COMPUTE WK-SY   =  (WK-YSK-YYYY -  1900)  *  0.25
               COMPUTE WK-SY   =  WK-SKJTDD  +  WK-SZ  +  WK-SY  +
                                  (WK-YSK-YYYY    -   1934)
               DIVIDE  7  INTO  WK-SY  GIVING  WK-DUMMYN
                          REMAINDER  YSK-YOUBI2
      *
               MOVE    WK-SKJTMM          TO  SKJT-M4(IX8)
               EVALUATE    YSK-YOUBI2
                   WHEN    0       MOVE  16   TO  SKJT-D4(IX8)
                   WHEN    1       MOVE  15   TO  SKJT-D4(IX8)
                   WHEN    2       MOVE  21   TO  SKJT-D4(IX8)
                   WHEN    3       MOVE  20   TO  SKJT-D4(IX8)
                   WHEN    4       MOVE  19   TO  SKJT-D4(IX8)
                   WHEN    5       MOVE  18   TO  SKJT-D4(IX8)
                   WHEN    6       MOVE  17   TO  SKJT-D4(IX8)
               END-EVALUATE
      *
               IF      WK-SKJTMM  =   7
                   MOVE    SKJT-D4(IX8)    TO  SKJT-KNRDD (7, 1)
               END-IF
               IF      WK-SKJTMM  =   9
                   MOVE    SKJT-D4(IX8)    TO  SKJT-KNRDD (9, 1)
               END-IF
           END-PERFORM
           .
       SKJT-KEISAN3-EXT.
           EXIT.
149100******************************************************************
149200*        数字変換処理
149300******************************************************************
149400 SUUJI-EDT-RTN    SECTION.
149500 SUUJI-EDT-000.
149600     MOVE    WK-YYMMDD       TO      EDT-GYMD.
149700     MOVE    EDT-GYMD-Y      TO      EDT-GYMDZ-Y.
149800     MOVE    EDT-GYMD-M      TO      EDT-GYMDZ-M.
149900     MOVE    EDT-GYMD-D      TO      EDT-GYMDZ-D.
150000     MOVE    EDT-GYMDZ       TO      EDT-GYMDX.
150100*
150200     MOVE    SPACE           TO      EDT-YMD.
150300     PERFORM     VARYING     I       FROM  1  BY  1
150400                   UNTIL     I       >     6
150500         EVALUATE  EDT-GYMDX-OC ( I )
150600             WHEN  " "   MOVE  SUUJIN(1)   TO  EDT-YMD-OC (I)
150700             WHEN  "0"   MOVE  SUUJIN(2)   TO  EDT-YMD-OC (I)
150800             WHEN  "1"   MOVE  SUUJIN(3)   TO  EDT-YMD-OC (I)
150900             WHEN  "2"   MOVE  SUUJIN(4)   TO  EDT-YMD-OC (I)
151000             WHEN  "3"   MOVE  SUUJIN(5)   TO  EDT-YMD-OC (I)
151100             WHEN  "4"   MOVE  SUUJIN(6)   TO  EDT-YMD-OC (I)
151200             WHEN  "5"   MOVE  SUUJIN(7)   TO  EDT-YMD-OC (I)
151300             WHEN  "6"   MOVE  SUUJIN(8)   TO  EDT-YMD-OC (I)
151400             WHEN  "7"   MOVE  SUUJIN(9)   TO  EDT-YMD-OC (I)
151500             WHEN  "8"   MOVE  SUUJIN(10)  TO  EDT-YMD-OC (I)
151600             WHEN  "9"   MOVE  SUUJIN(11)  TO  EDT-YMD-OC (I)
151700         END-EVALUATE
151800     END-PERFORM.
151900 SUUJI-EDT-EXT.
152000     EXIT.
152100******************************************************************
152200*        曜日変換処理
152300******************************************************************
152400 YOUBI-SET-RTN    SECTION.
152500 YOUBI-SET-000.
152600     SET   IX-YOUBI       TO  1.
152700     EVALUATE    YSK-YOUBI
152800         WHEN    1        SET   IX-YOUBI   TO   1
152900         WHEN    2        SET   IX-YOUBI   TO   2
153000         WHEN    3        SET   IX-YOUBI   TO   3
153100         WHEN    4        SET   IX-YOUBI   TO   4
153200         WHEN    5        SET   IX-YOUBI   TO   5
153300         WHEN    6        SET   IX-YOUBI   TO   6
153400         WHEN    0        SET   IX-YOUBI   TO   7
153500     END-EVALUATE.
153600 YOUBI-SET-EXT.
153700     EXIT.
