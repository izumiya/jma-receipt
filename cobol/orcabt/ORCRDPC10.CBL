       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCRDPC10.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : ＤＰＣファイル作成
      *                    : Ｅ、Ｆファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/01/23    NACL−伊藤    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *              NACL-小豆沢  xx/xx/xx  グループ診療対応
      *  平成20年度仕様対応
      *              NACL-伊藤    08/06/20  出来高・包括フラグ
      *                                       特定入院料１セット廃止
      *                                       診療区分21,22,23の場合
      *                                       退院日であれば１をセット
      *              NACL-伊藤    08/07/08  診療行為名称
      *                                       入力側の有効桁数が2->3に
      *                                       変更されたため作業領域の
      *                                       有効桁数を変更
      *              NACL-小豆沢  08/07/17  データ識別番号
      *                                       患者IDから患者番号へ変更
      *              NACL-伊藤    08/07/18  出来高点数
      *                                       手術の自動吻合器加算など
      *                                       時間外加算の所定点数に該
      *                                       当しない点数が含まれてい
      *                                       た
      *              NACL-伊藤    08/07/28  入院日
      *                                       自院入院歴を作成されてい
      *                                       た場合の考慮を行った
      *              NACL-伊藤    08/07/29  出来高点数
      *                                       内視鏡検査＋検査逓減＋乳
      *                                       幼児加算のようなケースで
      *                                       乳幼児加算が０点
      *              NACL-伊藤    08/07/30  出来高点数
      *                                       ２以上の手術の５０％併施
      *                                       加算の点数計算
      *              NACL-伊藤    08/08/06  出来高点数
      *                                       手術の通則加算の点数は所
      *                                       定点数に含めない
      *              NACL-小豆沢  08/08/21  入退院日
      *                                       退院同日入院の場合入退院
      *                                       歴が同日入院であったため
      *                                       退院の歴として記録する
      *              NACL-伊藤    08/10/06  出来高点数
      *                                       閉鎖循環式全身麻酔計算
      *              NACL-伊藤    09/01/13  出来高点数
      *                                       通則加算対象外できざみ
      *                                       加算のマスタの出来高点数
      *              NACL-伊藤    09/01/21  出来高・包括フラグ
      *                                       退院時処方の判断を退院日
      *                                       からコメントコードに変更
      *              NACL-伊藤    10/08/19  特定入院料包括対応
      *              NACL-伊藤    11/01/06  退院日
      *                                       当該月末時点での入院状態
      *                                       により退院日を決定する
      *                                     退院時投薬のコメント
      *                                       ユーザーが作成したコメン
      *                                       トコードに対応
      *              NACL-伊藤    14/04/18  データ区分
      *                                       9999包括保険で病理診断を
      *                                       算定した場合64で記録され
      *                                       ていた
      *              NACL-伊藤    14/04/21  出来高点数
      *                                       検査で包括区分が０でない
      *                                       場合薬剤器材の点数を引く
      *              NACL-伊藤    14/05/12  出来高・包括フラグ
      *                                       平成26年 4月診療分からは
      *                                       点数マスタのDPCKBNを設定
      *              NACL-伊藤    14/07/16  行為明細点数、出来高点数
      *                                       端数処理
      *              NACL-小豆沢  14/09/09  Ｅファイル編集処理
      *                                     平成２６年１０月診療分からの
      *                                     包括分は行為点数、行為薬剤料、
      *                                     行為材料料にゼロ点をセット
      *                                     する
      *              NACL-小豆沢  14/09/19  Ｆファイル編集処理
      *                                     栄養管理体制減算をマイナス
      *                                     記録する
      *              NACL-伊藤    14/10/17  レセプト科区分クリア漏れ
      *              NACL-小豆沢  14/10/20  Ｅファイル編集処理
      *                                     包括分は行為点数、行為薬剤料、
      *                                     行為材料料にゼロ点をセット
      *                                     する
      *              NACL-小豆沢  14/10/20  Ｆファイル編集処理
      *                                     包括分は出来高点数にゼロ点を
      *                                     セットする
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *
      *    レセプト０４データ
           SELECT  RECE641-FILE    ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE641-KEY
                                   FILE    STATUS  IS  STS-RECE64.
      *    レセプト０５データ
           SELECT  RECE65-FILE     ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE65-KEY
                                   FILE    STATUS  IS  STS-RECE65.
      *
           SELECT  RECE69-FILE     ASSIGN  RECE69PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE69-KEY
                                   FILE    STATUS  IS  STS-RECE69.
      *    入退院履歴情報
           SELECT  RECE68-FILE     ASSIGN  RECE68PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE68-KEY
                                   FILE    STATUS  IS  STS-RECE68.
      *    順序番号情報
           SELECT  RECE900-FILE    ASSIGN  RECE900PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE900-KEY
                                   FILE    STATUS  IS  STS-RECE900.
      *    ＤＰＣ（Ｅ）
           SELECT  DPCE-FILE       ASSIGN  DPCE
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-DPCE.
      *    ＤＰＣ（Ｆ）
           SELECT  DPCF-FILE       ASSIGN  DPCF
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-DPCF.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
      *
      *    レセプト０４データ
       FD  RECE641-FILE.
       01  RECE641-REC.
           COPY    "CPRCF0641.INC".
      *
      *    レセプト０５データ
       FD  RECE65-FILE.
       01  RECE65-REC.
           COPY    "CPRCF065.INC".
      *
      *    レセプト０９データ
       FD  RECE69-FILE.
       01  RECE69-REC. 
           COPY    "CPRCF069.INC".
      *
       FD  RECE68-FILE.
       01  RECE68-REC. 
           COPY    "CPRCF068.INC".
      *
      *    順序番号データ
       FD  RECE900-FILE.
       01  RECE900-REC. 
           COPY    "CPRCF900.INC".
      *
      *    ＤＰＣ（Ｅ）
       FD  DPCE-FILE.
       01  DPCE-REC                PIC X(2500).
      *
      *    ＤＰＣ（Ｆ）
       FD  DPCF-FILE.
       01  DPCF-REC                PIC X(2500).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE62//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE64//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE65//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE69//.
      *     入退院履歴情報ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE68//.
      *     順序番号情報ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE900//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"
                                   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE35              PIC X(02).
           03  STS-RECE62              PIC X(02).
           03  STS-RECE64              PIC X(02).
           03  STS-RECE65              PIC X(02).
           03  STS-RECE68              PIC X(02).
           03  STS-RECE69              PIC X(02).
           03  STS-RECE900             PIC X(02).
           03  STS-DPCE                PIC X(02).
           03  STS-DPCF                PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PTCOM               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-RECE35              PIC 9(01).
           03  FLG-RECE62              PIC 9(01).
           03  FLG-RECE64              PIC 9(01).
           03  FLG-RECE65              PIC 9(01).
           03  FLG-RECE69              PIC 9(01).
           03  FLG-RECE68              PIC 9(01).
           03  FLG-RECE900             PIC 9(01).
      *
           03  FLG-READ                PIC 9(01).
      *
           03  FLG-TEIGEN              PIC 9(01).
           03  FLG-YKZ20               PIC 9(01).
           03  FLG-TOTALTEN            PIC 9(01).
           03  FLG-KOKUJISKBKBN1       PIC 9(01).
           03  FLG-GAITENTTLKBN        PIC 9(03).
           03  FLG-TBLMAX              PIC 9(01).
           03  FLG-SRYSYUKBN721        PIC 9(01).
           03  FLG-SRYKBN70-TEIGEN     PIC 9(01).
           03  FLG-KENSA-KASAN         PIC 9(01).
           03  FLG-KENSA-TEIGEN        PIC 9(01).
           03  FLG-SET                 PIC 9(01).
           03  FLG-NYUTAI-RIREKI       PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
           03  FLG-COMMENT             PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-DAYSET              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-HOUKATU-RECE04      PIC 9(01).
           03  FLG-TOKUTEINYUIN        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-OUT-E               PIC 9(06).
           03  CNT-OUT-F               PIC 9(06).
           03  CNT-RECE64              PIC 9(06).
           03  CNT-RECE69              PIC 9(06).
           03  CNT-RECE900             PIC 9(06).
           03  CNT-COMMIT              PIC 9(03).
      *
       01  INDEX-AREA.
           03  IDW0                    PIC 9(03).
           03  IDW                     PIC 9(03).
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
           03  IDZ                     PIC 9(03).
           03  IDY1                    PIC 9(03).
           03  IDY2                    PIC 9(03).
           03  IDY3                    PIC 9(03).
           03  IDXW                    PIC 9(03).
           03  IDXX                    PIC 9(03).
           03  IDXY                    PIC 9(03).
           03  IDXZ                    PIC 9(03).
           03  IDW1                    PIC 9(03).
           03  IDW2                    PIC 9(04).
           03  IDZ1                    PIC 9(03).
           03  IDZ2                    PIC 9(03).
           03  IDZ3                    PIC 9(03).
           03  IDZ4                    PIC 9(03).
           03  IDZ5                    PIC 9(03).
           03  IDZ6                    PIC 9(03).
           03  SYSKAN-IDX              PIC 9(03).
           03  SYSKAN-IDX2             PIC 9(03).
           03  II                      PIC 9(01).
           03  IJ                      PIC 9(02).
           03  IDX-HOUKATU             PIC 9(02).
      *
       01  KEY-AREA                          VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-NEW1.
                   07  KEY-N-HOSPNUM   PIC 9(02).
                   07  KEY-N-SRYYM     PIC 9(06).
                   07  KEY-N-NYUGAIKBN PIC X(01).
                   07  KEY-N-PTID      PIC 9(10).
                   07  KEY-N-RECEKA    PIC X(02).
               05  KEY-N-TEISYUTUSAKI  PIC 9(01).
           03  KEY-OLD.
               05  KEY-OLD1.
                   07  KEY-O-HOSPNUM   PIC 9(02).
                   07  KEY-O-SRYYM     PIC 9(06).
                   07  KEY-O-NYUGAIKBN PIC X(01).
                   07  KEY-O-PTID      PIC 9(10).
                   07  KEY-O-RECEKA    PIC X(02).
               05  KEY-O-TEISYUTUSAKI  PIC 9(01).
      *
       01  SUM-AREA                                VALUE   ZERO.
           03  SUM-TOTALKENSU          PIC 9(06).
           03  SUM-TOTALTEN            PIC 9(10).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  10.
               05  TBL-SYS1001         PIC  X(610).
      *
      *    診療科情報退避用領域
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-OCC           OCCURS  100.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-RECESRYKA       PIC X(02).
           03  TBL-SRYKA-MAX           PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
      *    パラメタエリア
           03  WRK-PARA.
               05  WRK-PARA1.
                   07  WRK-PARA-SRYYM  PIC X(06).
                   07  WRK-PARA-TERMID PIC X(16).
                   07  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-TEISYUTUSAKI
                                       PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-SYARECEKBN PIC X(01).
               05  WRK-PARA-KKHRECEKBN PIC X(01).
               05  WRK-PARA-CREYMD     PIC X(08).
               05  WRK-PARA-CREHMS     PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-KIZAI-SRYCD         PIC X(09).
      *     右ずめ編集用
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-COM-SRYKBN          PIC X(02).
           03  WRK-REC                 PIC X(2500).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NUM-X.
                   07  WRK-NUM         PIC 9999999999.999.
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02).
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *    傷病名レコード編集用
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEICD        PIC X(07).
               05  WRK-RECECD-G.
                   07  WRK-RECECD      PIC X(04)   OCCURS  20.
      *    給付割合 
           03  WRK-KYURATE             PIC 9(03).
      *****レセプト番号
      *****03  WRK-RECENUM             PIC 9(06).
      *
           03  WRK-RENNUM-E            PIC 9(04).
           03  WRK-RENNUM-F            PIC 9(04).
      *  WRK-RENNUM と WRK-MEISAI-RENNUM は桁数変更不可
           03  WRK-RENNUM              PIC 9(04).
           03  WRK-MEISAI-RENNUM       PIC 9(03).
           03  WRK-COM-RENNUM          PIC 9(04).
      *
           03  WRK-COM-FTNKBN          PIC X(01).
           03  WRK-TAIHI-FTNKBN        PIC X(01).
           03  WRK-SJ-KBN              PIC X(02).
           03  WRK-RECECOM-SJKBN       PIC X(02).
           03  WRK-SJ-CHK              PIC 9(01).
      *
      *    1桁目:生保 2桁目:結核 3桁目:精神
           03  WRK-3SYU-KBN.
               05  WRK-3SYU            PIC X(03).
      *    生保
           03  SEHIDX                  PIC 9(01).
      *    精神
           03  SESIDX                  PIC 9(01).
      *    結核
           03  KEKIDX                  PIC 9(01).
      *
           03  IDX1                    PIC 9(01).
           03  IDX2                    PIC 9(01).
           03  IDX3                    PIC 9(01).
      *
      *    摘要レコード編集用
           03  WRK-ZAIWMAX             PIC 9(03).
           03  WRK-TBLMAX1             PIC 9(03).
           03  WRK-TBLMAX6             PIC 9(03).
           03  WRK-TBLMAX7             PIC 9(03).
           03  WRK-TBLMAX8             PIC 9(03).
           03  WRK-SURYO               PIC 9(10).
           03  WRK-TENSU               PIC 9(07).
           03  WRK-KAISU               PIC 9(03).
           03  WRK-KAISU-70            PIC 9(03)V9(01).
           03  WRK-KAISU-70T           PIC 9(03)V9(01).
           03  WRK-TANKA               PIC 9(07)V9(02).
           03  WRK-SRYSURYO            PIC 9(05)V9(05).
           03  WRK-FTNKBN-AREA.
               05  WRK-FTNKBN          PIC X(01).
               05  WRK-KOHNUM          PIC 9(01)
                                           OCCURS  3.
               05  WRK-SEHFLG          PIC 9(01).
           03  WRK-SRYCD1-MAX          PIC 9(03).
           03  WRK-SRYCD71-MAX         PIC 9(03).
           03  WRK-SRYCD72-MAX         PIC 9(03).
           03  WRK-KOKUJISKBKBN1-KOUI-TEN
                                       PIC 9(09)V9(02).
           03  WRK-RECE64-ZAIKAISU     PIC 9(04).
           03  WRK-KENSASU             PIC 9(04).
      *    日付変換用
           03  WRK-SYMD                PIC X(08).
      *
           03  WRK-HENYMDG             PIC 9(08).
           03  WRK-HENYMDGR            REDEFINES   WRK-HENYMDG.
               05  FILLER              PIC 9(01).
               05  WRK-HENYMD          PIC 9(07).
      *
      *    診療年月の末日
           03  WRK-EDYMD               PIC 9(08).
           03  WRK-EDYMD-R             REDEFINES   WRK-EDYMD.
               05  WRK-EDYM            PIC X(06).
               05  WRK-EDDD            PIC 9(02).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
      *    きざみ計算用
           03  FLG-KZM                 PIC 9(01).
           03  WRK-KZM-TEN             PIC 9(08)V9(3).
           03  WRK-KZM-TEN1            PIC 9(08)V9(3).
           03  WRK-KZM-TEN2            PIC 9(08).
           03  WRK-KZM-SURYO           PIC 9(05)V9(5).
           03  WRK-KZM-SURYO-R         REDEFINES  WRK-KZM-SURYO.
               05  WRK-KZM-SURYO-S1    PIC 9(05).
               05  WRK-KZM-SURYO-S2    PIC 9(05).
      *
      *    保険履歴編集（適用欄）
           03  WRK-HKN-RIREKI-AREA.
               05  WRK-HKN-RIREKI-TBL          OCCURS  50.
                   07  WRK-HKN-RIREKI          PIC X(52).
                   07  WRK-HKN-RIREKI-W   REDEFINES    WRK-HKN-RIREKI.
                       09  WRK-HKN-MEISYO      PIC X(04).
                       09  WRK-HKN-TEKSTYMD.
                           11  WRK-HKN-TEKSTYY PIC X(08).
                           11  WRK-HKN-TEKSTMD PIC X(12).
                       09  WRK-HKN-FL.
                           11  WRK-HKN-FL1     PIC X(04).
                           11  WRK-HKN-TEKEDYMD.
                               13  WRK-HKN-TEKEDYY
                                               PIC X(08).
                               13  WRK-HKN-TEKEDMD
                                               PIC X(12).
                           11  WRK-HKN-FL2     PIC X(04).
           03  WRK-RIREKI-MAX                  PIC 9(02).
           03  WRK-HKN-RIREKI-X                PIC X(60).
           03  WRK-HKN-HKNJANUM                PIC X(16).
           03  WRK-HKN-NUM                     PIC X(50).
           03  WRK-HKN-KIGO                    PIC X(50).
           03  WRK-HKN-TEISYUTUSAKI            PIC X(04).
           03  WRK-SRYYMST                     PIC X(12).
           03  WRK-SRYYMED                     PIC X(12).
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT                 PIC X(50)
                                               OCCURS  16.
           03  WRK-COMMENT-R               REDEFINES   WRK-COMMENT-G.
               05  WRK-COMMENT1                PIC X(100)
                                               OCCURS  8.
           03  WRK-COMM                        PIC X(800).
           03  WRK-COMM1                       PIC X(800).
           03  WRK-COL                         PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
           03  WRK-SJKBN-X.
               05  WRK-SJKBN       PIC 9(02).
      *
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN                 PIC X(01).
      *
      *    区分番号編集（点数表）
           03  WRK-KBNNUM-EDIT.
               05  WRK-KBNNUM-KBN              PIC X(01).
               05  WRK-KBNNUM-KBNNUM           PIC 9(03).
               05  WRK-KBNNUM-HAIFUN           PIC X(01).
               05  WRK-KBNNUM-KBNNUM-EDA       PIC 9(02).
      *
      *    データ識別番号への患者番号編集用
           03  WRK-ZEROKETA                    PIC 9(02).
           03  WRK-PTNUMKETA                   PIC 9(02).
      *
      *    医薬品・特定器材用テーブル
       01  TBL-ZAIW-AREA.
           03  TBL-ZAIW-TABLE          OCCURS  2.
               05  TBL-ZAIW-REC        OCCURS 100.
                   07  TBL-ZAIW-SRYACTINF.
      *                診療コード
                       09  TBL-ZAIW-SRYCD           PIC  X(09).
      *                診療種別区分
                       09  TBL-ZAIW-SRYSYUKBN       PIC  X(03).
      *                数量
                       09  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(05).
      *                回数
                       09  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *                単位コード
                       09  TBL-ZAIW-TANICD          PIC  9(03).
      *                単位名称有効桁数
                       09  TBL-ZAIW-TANIMOJISU      PIC  9(01).
      *                単位名称
                       09  TBL-ZAIW-TANINAME        PIC  X(24).
      *                点数識別区分
                       09  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
      *                点数
                       09  TBL-ZAIW-TEN             PIC  9(09)V9(02).
      *                明細請求フラグ
                       09  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *                自動算定区分
                       09  TBL-ZAIW-AUTOKBN         PIC  X(01).
      *                名称入力番号
                       09  TBL-ZAIW-INPUTNUM        PIC  9(03).
      *                点数欄集計先識別
                       09  TBL-ZAIW-TENTTLKBN       PIC  9(03).
      *                データ区分
                       09  TBL-ZAIW-DATAKBN         PIC  9(01).
      *                包括対象検査
                       09  TBL-ZAIW-HOUKNSKBN       PIC  9(02).
      *                検査等実施判断
                       09  TBL-ZAIW-KNSJISKBN       PIC  9(01).
      *                検査等実施判断グループ
                       09  TBL-ZAIW-KNSJISGRPKBN    PIC  9(02).
      *                各種番号等
                       09  TBL-ZAIW-CDKBNINF.
                           11  TBL-ZAIW-CDKBN-KBN   PIC  X(01).
                           11  TBL-ZAIW-CDKBN-SYO   PIC  9(01).
                           11  TBL-ZAIW-CDKBN-BU    PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KBNNUM     PIC  9(03).
                           11  TBL-ZAIW-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KOUBAN     PIC  9(02).
                           11  TBL-ZAIW-KOUHYOJYUNNUM    PIC  9(09).
      *                告示等識別区分１
                       09  TBL-ZAIW-KOKUJISKBKBN1   PIC  9(01).
      *                告示等識別区分２
                       09  TBL-ZAIW-KOKUJISKBKBN2   PIC  9(01).
      *                通則加算対象外区分
                       09  TBL-ZAIW-TUSOKUGAIKBN    PIC  9(01).
      *                きざみ値計算識別
                       09  TBL-ZAIW-KZMCOMPSIKIBETU PIC  9(01).
      *                注加算コード
                       09  TBL-ZAIW-CHUKSNINF.
      *                    コード
                           11  TBL-ZAIW-CHUKSNCD    PIC  X(04).
      *                    通番
                           11  TBL-ZAIW-CHUKSNTUBAN PIC  X(01).
      *                検体検査コメント
                       09  TBL-ZAIW-KENTAICOMMENT   PIC  9(01).
      *                入力コメント
                       09  TBL-ZAIW-INPUTCOMENT     PIC  X(80).
      *            名称情報
                   07  TBL-ZAIW-NAMEINF.
      *                漢字有効桁数
                       09  TBL-ZAIW-YUKOKETA        PIC  9(03).
      *                漢字名称
                       09  TBL-ZAIW-NAME            PIC  X(200).
      *            データ識別(１：フィルム ２：特定機材)
                   07  TBL-ZAIW-DATSKB              PIC 9(01).
      *
      *    診療行為・コメント用テーブル
       01  TBL-KOUI-AREA.
           03  TBL-KOUI-REC                     OCCURS  100.
               05  TBL-KOUI-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KOUI-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                   07  TBL-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KOUI-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KOUI-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KOUI-CDKBNINF.
                       09  TBL-KOUI-CDKBN-KBN   PIC  X(01).
                       09  TBL-KOUI-CDKBN-SYO   PIC  9(01).
                       09  TBL-KOUI-CDKBN-BU    PIC  9(02).
                       09  TBL-KOUI-CDKBN-KBNNUM     PIC  9(03).
                       09  TBL-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  TBL-KOUI-CDKBN-KOUBAN     PIC  9(02).
                       09  TBL-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                   07  TBL-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KOUI-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KOUI-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KOUI-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                   07  TBL-KOUI-CHUKSNINF.
      *                コード
                       09  TBL-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KOUI-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KOUI-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KOUI-INPUTCOMENT     PIC  X(80). 
      *        名称情報
               05  TBL-KOUI-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KOUI-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KOUI-NAME            PIC  X(200).
      *
      *    コメントデータ用テーブル
       01  TBL-COMMENT-AREA.
           03  TBL-COMMENT-REC                  OCCURS  100.
               05  TBL-COMMENT-SRYCD            PIC  X(09).
               05  TBL-COMMENT-NAME             PIC  X(200).
      *
      *    閉麻計算用
       01  TBL-HEIMA-AREA.
           03  WRK-HEIMA-IDX                    PIC 9(02).
           03  WRK-HEIMA-SURYO                  PIC 9(05).
           03  WRK-HEIMA-SURYO2                 PIC 9(05).
           03  TBL-HEIMA-MAX                    PIC 9(02).
           03  TBL-HEIMA-TOT-SURYO              PIC 9(05)V9(5).
           03  TBL-HEIMA-TBL                    OCCURS  10.
               05  TBL-HEIMA-TEN                PIC 9(07).
               05  TBL-HEIMA-KZMKGN             PIC 9(08)V9(3).
               05  TBL-HEIMA-KZMJGN             PIC 9(08)V9(3).
               05  TBL-HEIMA-KZMTEN             PIC 9(07).
               05  TBL-HEIMA-KZMERR             PIC 9(01).
               05  TBL-HEIMA-SURYO              PIC 9(08)V9(5).
               05  TBL-HEIMA-ZAN-SURYO          PIC 9(08)V9(5).
               05  TBL-HEIMA-COM-SURYO          PIC 9(08)V9(5).
      *
      *    診療行為データ用テーブル（告示等識別区分による記録順）
       01  WRK-KOUI-TAIHI-AREA.
           03  WRK-KOUI-TAIHI-TABLE             OCCURS  100.
               05  WRK-KOUI-TAIHI-REC           PIC X(500).
      *        診療コード
               05  WRK-KOUI-TAIHI-SRYCD         PIC  X(09).
      *        告示等識別区分１
               05  WRK-KOUI-TAIHI-KOKUJISKBKBN1 PIC  9(01).
      *        注加算コード
               05  WRK-KOUI-TAIHI-CHUKSNCD      PIC  X(04).
      *        点数識別区分
               05  WRK-KOUI-TAIHI-TENSIKIBETU   PIC  9(01).
      *
       01  WRK-KOUI-AREA.
           03  WRK-KOUI-TABLE                   OCCURS  4.
               05  WRK-KOUI-TBLMAX              PIC 9(03).
               05  WRK-KOUI-REC                     OCCURS  100.
                   07  WRK-KOUI-SRYACTINF.
      *            診療コード
                       09  WRK-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                       09  WRK-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                       09  WRK-KOUI-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                       09  WRK-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                       09  WRK-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                       09  WRK-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                       09  WRK-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                       09  WRK-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                       09  WRK-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                       09  WRK-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                       09  WRK-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                       09  WRK-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                       09  WRK-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                       09  WRK-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                       09  WRK-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                       09  WRK-KOUI-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                       09  WRK-KOUI-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                       09  WRK-KOUI-CDKBNINF.
                           11  WRK-KOUI-CDKBN-KBN   PIC  X(01).
                           11  WRK-KOUI-CDKBN-SYO   PIC  9(01).
                           11  WRK-KOUI-CDKBN-BU    PIC  9(02).
                           11  WRK-KOUI-CDKBN-KBNNUM     PIC  9(03).
                           11  WRK-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  WRK-KOUI-CDKBN-KOUBAN     PIC  9(02).
                           11  WRK-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                       09  WRK-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                       09  WRK-KOUI-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                       09  WRK-KOUI-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                       09  WRK-KOUI-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                       09  WRK-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                       09  WRK-KOUI-CHUKSNINF.
      *                コード
                           11  WRK-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                           11  WRK-KOUI-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                       09  WRK-KOUI-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                       09  WRK-KOUI-INPUTCOMENT     PIC  X(80). 
      *        名称情報
                   07  WRK-KOUI-NAMEINF.
      *            漢字有効桁数
                       09  WRK-KOUI-YUKOKETA        PIC  9(03).
      *            漢字名称
                       09  WRK-KOUI-NAME            PIC  X(200).
      *
      *    並び替え用ワーク
       01  TBL-KOUI-SORT                        PIC X(500).
      *
      *    診療行為データ用テーブル（再診 時間外）
       01  TBL-KOUI-SRYKBN12-AREA.
           03  TBL-KOUI-SRYKBN12-REC                     OCCURS  3.
               05  TBL-KOUI-SRYKBN12-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYKBN12-SRYCD  PIC  X(09).
      *            回数
                   07  TBL-KOUI-SRYKBN12-KAISU  PIC  9(04).
      *            点数
                   07  TBL-KOUI-SRYKBN12-TEN    PIC  9(09)V9(02).
      *
      *    負担区分編集用エリア
       01  WRK-RECE641-REC.
           COPY    "CPRCF0641.INC" REPLACING  //RECE641//
                                   BY         //WRK-RECE641//.
      *
       01  WRK-RECE65-REC.
           COPY    "CPRCF065.INC"  REPLACING  //RECE65//
                                   BY         //WRK-RECE65//.
      *
       01  WRK-TAIHI-RECE641-REC.
           COPY    "CPRCF0641.INC"  REPLACING //RECE641//
                                   BY         //WRK-TAIHI-RECE641//.
      *
      *    画像診断用退避エリア
       01  WRK-721-RECE65-REC.
           COPY    "CPRCF065.INC"  REPLACING  //RECE65//
                                   BY         //WRK-721-RECE65//.
       01  WRK-700-RECE65-AREA.
           02  WRK-SRYSYUKBN700-MAX           PIC 9(01).
           02  FLG-SRYSYUKBN700               PIC 9(01)  OCCURS  5.
           02  WRK-700-RECE65-REC                        OCCURS  5.
           COPY    "CPRCF065.INC"  REPLACING  //RECE65//
                                   BY         //WRK-700-RECE65//.
       01  WRK-SRYKBN70-RECE65-REC.
           COPY    "CPRCF065.INC"  REPLACING  //RECE65//
                                   BY         //WRK-SRYKBN70-RECE65//.
      *    生体検査・加算用退避エリア
       01  WRK-KASAN-RECE65-REC.
           COPY    "CPRCF065.INC"  REPLACING  //RECE65//
                                   BY         //WRK-KASAN-RECE65//.
      *    生体検査・逓減用退避エリア
       01  WRK-TEIGEN-RECE65-REC.
           COPY    "CPRCF065.INC"  REPLACING  //RECE65//
                                   BY         //WRK-TEIGEN-RECE65//.
      *
      *    検体検査用退避エリア
       01  TBL-KENTAI-RECE64-AREA.
         02  TBL-KENTAI-RECE64-REC  OCCURS  100.
      *
           COPY    "CPRCF0641.INC"  REPLACING  //RECE641//
                                    BY          //TBL-KENTAI-RECE64//.
      *
           03  TBL-KENTAI-RECE65-REC        OCCURS  50.
               05  TBL-KENTAI-RECE65-SRYACTINF.
      *            診療コード
                   07  TBL-KENTAI-RECE65-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KENTAI-RECE65-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KENTAI-RECE65-SRYSURYO      PIC  9(05)V9(05).
      *            回数
                   07  TBL-KENTAI-RECE65-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KENTAI-RECE65-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KENTAI-RECE65-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KENTAI-RECE65-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KENTAI-RECE65-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KENTAI-RECE65-TEN            PIC 9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KENTAI-RECE65-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KENTAI-RECE65-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KENTAI-RECE65-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KENTAI-RECE65-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KENTAI-RECE65-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KENTAI-RECE65-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KENTAI-RECE65-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KENTAI-RECE65-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KENTAI-RECE65-CDKBNINF.
                       09  TBL-KENTAI-RECE65-CDKBN-KBN   PIC  X(01).
                       09  TBL-KENTAI-RECE65-CDKBN-SYO   PIC  9(01).
                       09  TBL-KENTAI-RECE65-CDKBN-BU    PIC  9(02).
                       09  TBL-KENTAI-RECE65-CDKBN-KBNNUM     PIC 9(03).
                       09  TBL-KENTAI-RECE65-CDKBN-KBNNUM-EDA PIC 9(02).
                       09  TBL-KENTAI-RECE65-CDKBN-KOUBAN     PIC 9(02).
                       09  TBL-KENTAI-RECE65-KOUHYOJYUNNUM    PIC 9(09).
      *            告示等識別区分１
                   07  TBL-KENTAI-RECE65-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KENTAI-RECE65-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KENTAI-RECE65-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KENTAI-RECE65-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KENTAI-RECE65-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                   07  TBL-KENTAI-RECE65-CHUKSNINF.
      *                コード
                       09  TBL-KENTAI-RECE65-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KENTAI-RECE65-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KENTAI-RECE65-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KENTAI-RECE65-INPUTCOMENT     PIC  X(80). 
      *        名称情報
               05  TBL-KENTAI-RECE65-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KENTAI-RECE65-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KENTAI-RECE65-NAME            PIC  X(200).
      *
           03  TBL-KENTAI-SRYCD-MAX
                                   PIC 9(02).
      *
         02  TBL-KENTAI-RECE64-MAX PIC 9(03).
         02  TBL-KENTAI-RECE65-MAX PIC 9(03).
      *
      *    検査（判断料）用退避エリア
       01  TBL-KENSA-RECE64-AREA.
         02  TBL-KENSA-RECE64-REC  OCCURS  100.
      *
           COPY    "CPRCF0641.INC"  REPLACING  //RECE641//
                                    BY          //TBL-KENSA-RECE64//.
      *
           03  TBL-KENSA-RECE65-REC        OCCURS  50.
               05  TBL-KENSA-RECE65-SRYACTINF.
      *            診療コード
                   07  TBL-KENSA-RECE65-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KENSA-RECE65-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KENSA-RECE65-SRYSURYO      PIC  9(05)V9(05).
      *            回数
                   07  TBL-KENSA-RECE65-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KENSA-RECE65-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KENSA-RECE65-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KENSA-RECE65-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KENSA-RECE65-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KENSA-RECE65-TEN            PIC 9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KENSA-RECE65-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KENSA-RECE65-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KENSA-RECE65-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KENSA-RECE65-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KENSA-RECE65-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KENSA-RECE65-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KENSA-RECE65-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KENSA-RECE65-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KENSA-RECE65-CDKBNINF.
                       09  TBL-KENSA-RECE65-CDKBN-KBN   PIC  X(01).
                       09  TBL-KENSA-RECE65-CDKBN-SYO   PIC  9(01).
                       09  TBL-KENSA-RECE65-CDKBN-BU    PIC  9(02).
                       09  TBL-KENSA-RECE65-CDKBN-KBNNUM     PIC 9(03).
                       09  TBL-KENSA-RECE65-CDKBN-KBNNUM-EDA PIC 9(02).
                       09  TBL-KENSA-RECE65-CDKBN-KOUBAN     PIC 9(02).
                       09  TBL-KENSA-RECE65-KOUHYOJYUNNUM    PIC 9(09).
      *            告示等識別区分１
                   07  TBL-KENSA-RECE65-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KENSA-RECE65-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KENSA-RECE65-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KENSA-RECE65-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KENSA-RECE65-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                   07  TBL-KENSA-RECE65-CHUKSNINF.
      *                コード
                       09  TBL-KENSA-RECE65-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KENSA-RECE65-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KENSA-RECE65-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KENSA-RECE65-INPUTCOMENT     PIC  X(80). 
      *        名称情報
               05  TBL-KENSA-RECE65-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KENSA-RECE65-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KENSA-RECE65-NAME            PIC  X(200).
      *
           03  TBL-KENSA-SRYCD-MAX
                                  PIC 9(02).
      *
         02  TBL-KENSA-RECE64-MAX PIC 9(03).
         02  TBL-KENSA-RECE65-MAX PIC 9(03).
      *
       01  WRK-CONS-AREA.
      *    日計表レコード記載の条件点数
           03  WRK-CONS-JYOKEN-TENSU   PIC 9(08)   VALUE   350000.
      *    外来迅速検体検査加算
           03  WRK-CONS-SRYCD          PIC X(09)   VALUE   "160177770".
      *    検査日　日
           03  WRK-CONS-COMMENT        PIC X(09)   VALUE   "840000099".
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    ＴＡＢ
           03  WRK-KUGIRI-TAB         PIC X(01)   VALUE   X"09".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
      *    退職者保険番号
      *        組合
           03  WRK-CONS-HKNNUM1       PIC X(03)   VALUE  "063".
      *        国公
           03  WRK-CONS-HKNNUM2       PIC X(03)   VALUE  "072".
      *        地公
           03  WRK-CONS-HKNNUM3       PIC X(03)   VALUE  "073".
      *        警察
           03  WRK-CONS-HKNNUM4       PIC X(03)   VALUE  "074".
      *        公学・私学
           03  WRK-CONS-HKNNUM5       PIC X(03)   VALUE  "075".
      *        国保
           03  WRK-CONS-HKNNUM6       PIC X(03)   VALUE  "067".
      *    薬剤料逓減
           03  WRK-CONS-TEIGEN        PIC X(09)   VALUE   "630010002".
      *****退院時投薬
      *****03  WRK-CONS-TAITOYAKU     PIC X(09)   VALUE   "840000006".
      *****03  WRK-CONS-TAITOYAKU1    PIC X(09)   VALUE   "008400002".
      *    内服薬２０５円以下
      *****03  WRK-CONS-NAI205        PIC X(09)   VALUE   "610010002".
      *    注射２０５円以下
      *****03  WRK-CONS-CHU205        PIC X(09)   VALUE   "640010002".
      *    外服薬２０５円以下
      *****03  WRK-CONS-GAI205        PIC X(09)   VALUE   "660010002".
      *
      *    再診の時間加算
           03  WRK-CONS-JIKANGAI-TABLE. 
      *    一般
      *        時間外（再診）
               05  FILLER             PIC X(09)    VALUE   "112001110".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112001210".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112001310".
      *        時間外特例（再診）
               05  FILLER             PIC X(09)    VALUE   "112001410".
      *        時間外（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006470".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006570".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006670".
      *        時間外特例（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006770".
      *    老人
      *        時間外（再診）
               05  FILLER             PIC X(09)    VALUE   "112701010".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112701110".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112701210".
      *        時間外特例（再診）
               05  FILLER             PIC X(09)    VALUE   "112701310".
      *        時間外（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701710".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701810".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701910".
      *        時間外特例（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112702010".
      *    乳幼児
      *        夜間（再診）
               05  FILLER             PIC X(09)    VALUE   "112014170".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112014270".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112014370".
      *        夜間（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014470".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014570".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014670".
      *        再診（乳幼児時間外）加算
               05  FILLER             PIC X(09)    VALUE   "112014770".
      *        再診（乳幼児休日）加算
               05  FILLER             PIC X(09)    VALUE   "112014870".
      *        再診（乳幼児深夜）加算
               05  FILLER             PIC X(09)    VALUE   "112014970".
      *        再診（乳幼児時間外特例医療機関）加算
               05  FILLER             PIC X(09)    VALUE   "112015070".
      *        外来診療科（乳幼児時間外）加算
               05  FILLER             PIC X(09)    VALUE   "112015170".
      *        外来診療科（乳幼児休日）加算
               05  FILLER             PIC X(09)    VALUE   "112015270".
      *        外来診療科（乳幼児深夜）加算
               05  FILLER             PIC X(09)    VALUE   "112015370".
      *        外来診療科（乳幼児時間外特例医療機関）加算
               05  FILLER             PIC X(09)    VALUE   "112015470".
      *
           03  WRK-CONS-JIKANGAI-TABLE-R  REDEFINES   
                                      WRK-CONS-JIKANGAI-TABLE.
               05  WRK-CONS-JIKANGAI-SRYCD-G       OCCURS  30.
                   07  WRK-CONS-JIKANGAI-SRYCD
                                      PIC X(09).
           03  WRK-CONS-JIKANGAI-TEN-TABLE.
               05  WRK-CONS-JIKANGAI-TEN-G         OCCURS  30.
                   07  WRK-CONS-JIKANGAI-TEN
                                      PIC 9(09).
           03  WRK-CONS-JIKANGAI-MAX  PIC 9(02)    VALUE   30.    
      *
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）
           03  WRK-CONS-SANSO1        PIC X(09)   VALUE   "739210000".
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）（離島）
           03  WRK-CONS-SANSO2        PIC X(09)   VALUE   "739250000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）
           03  WRK-CONS-SANSO3        PIC X(09)   VALUE   "739200000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）（離島）
           03  WRK-CONS-SANSO4        PIC X(09)   VALUE   "739240000".
      *    酸素ボンベ・大型
           03  WRK-CONS-SANSO5        PIC X(09)   VALUE   "739220000".
      *    酸素ボンベ・大型（離島）
           03  WRK-CONS-SANSO6        PIC X(09)   VALUE   "739260000".
      *    酸素ボンベ・小型
           03  WRK-CONS-SANSO7        PIC X(09)   VALUE   "739230000".
      *    酸素ボンベ・小型（離島）
           03  WRK-CONS-SANSO8        PIC X(09)   VALUE   "739270000".
      *
      *    新生児加算（生体検査）
           03  WRK-CONS-KENSA-KASAN1  PIC X(09)   VALUE   "160155290".
      *    乳幼児加算（生体検査）
           03  WRK-CONS-KENSA-KASAN2  PIC X(09)   VALUE   "160155390".
      *
      *    検査逓減
           03  WRK-CONS-KENSA-TEIGEN  PIC X(09)   VALUE   "160000190".
      *    間接撮影の５０％逓減
           03  WRK-CONS-SRYKBN70-TEIGEN
                                      PIC X(09)   VALUE   "170020570".
      *
      *    超音波内視鏡加算
           03  WRK-CONS-CHOONPA-KASAN
                                      PIC X(09)   VALUE   "160148670".
      *
      *    施設基準不適合（検査）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-KENSA
                                      PIC X(09)   VALUE   "160174470".
      *
      *    施設基準不適合（画像）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-GAZOU
                                      PIC X(09)   VALUE   "170025170".
      *
      *    施設基準不適合（放射線）（１００分の７０）
           03  WRK-CONS-SISETUKIJYUN-HOUSYASEN
                                      PIC X(09)   VALUE   "180025270".
      *
      *    酸素補正率１．３（１気圧）
           03  WRK-CONS-SANSO-HOSEI   PIC X(09)   VALUE   "770020070".
      *
      *    出力対象のコメント
           03  WRK-CONS-COMMENT-G.
      *        複数診療科受診
               05  WRK-CONS-COMMENT1  PIC X(09)   VALUE   "099999901".
      *        他保険にて診察料算定済
               05  WRK-CONS-COMMENT2  PIC X(09)   VALUE   "099999902".
      *********（粉砕）
      *********05  WRK-CONS-COMMENT3  PIC X(09)   VALUE   "099209901".
      *********（残量廃棄）
      *********05  WRK-CONS-COMMENT4  PIC X(09)   VALUE   "099309901".
      *
      *    再入院年月日
           03  WRK-CONS-NYUIN-SRYCD   PIC X(09)   VALUE   "840000014".
      *    退院年月日
           03  WRK-CONS-TAIIN-SRYCD   PIC X(09)   VALUE   "840000013".
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
      *        生活療養・標準負担額
               05  WRK-CONS-HYOJYUN-FUTAN5
                                      PIC X(09)   VALUE   "197001910".
      *        生活療養（１）・標準負担額（一般）
               05  WRK-CONS-HYOJYUN-FUTAN6
                                      PIC X(09)   VALUE   "197002010".
      *        生活療養（２）・標準負担額（一般）
               05  WRK-CONS-HYOJYUN-FUTAN7
                                      PIC X(09)   VALUE   "197002110".
      *        生活療養・標準負担額（低２）
               05  WRK-CONS-HYOJYUN-FUTAN8
                                      PIC X(09)   VALUE   "197002210".
      *        生活療養・標準負担額（低１）
               05  WRK-CONS-HYOJYUN-FUTAN9
                                      PIC X(09)   VALUE   "197002310".
      *        生活療養・標準負担額（老人低１（老福以外））
               05  WRK-CONS-HYOJYUN-FUTAN10
                                      PIC X(09)   VALUE   "197002410".
      *        生活療養・標準負担額（老人低１（老福））
               05  WRK-CONS-HYOJYUN-FUTAN11
                                      PIC X(09)   VALUE   "197002510".
      *        生活療養・標準負担額（医療区分２・３）（一般）
               05  WRK-CONS-HYOJYUN-FUTAN12
                                      PIC X(09)   VALUE   "197002710".
      *        生活療養・標準負担額（医療区分２・３）（低２・９０日以下）
               05  WRK-CONS-HYOJYUN-FUTAN13
                                      PIC X(09)   VALUE   "197002810".
      *        生活療養・標準負担額（医療区分２・３）（低２・９０日超）
               05  WRK-CONS-HYOJYUN-FUTAN14
                                      PIC X(09)   VALUE   "197002910".
      *        生活療養・標準負担額（医療区分２・３）（低１）
               05  WRK-CONS-HYOJYUN-FUTAN15
                                      PIC X(09)   VALUE   "197003010".
      *
           03  WRK-CONS-SRYYM-200609   PIC 9(06)   VALUE   200609.
      *
      *    改行コード
       01  WRK-CONS-RETURN          PIC X(01)   VALUE   X"0A".
      *
       01  WRK-NYUIN-RIREKI.
           03  WRK-NYUIN-RIREKI-OCC    OCCURS   10.
               05  WRK-NYUIN-RIREKI-NYUINYMD    PIC X(08).
               05  WRK-NYUIN-RIREKI-TAIINYMD    PIC X(08).
      *
       01  WRK-FEDT-AREA.
           03  WRK-FEDT-1ST-FLG        PIC 9(01).
           03  WRK-FEDT-REC-FLG        PIC X(01).
           03  WRK-FEDT-NYUINYMD       PIC X(08).
           03  WRK-FEDT-TAIINYMD       PIC X(08).
           03  WRK-FEDT-SRYCD          PIC X(09).
           03  WRK-FEDT-KBNNUM-FLG     PIC 9(01).
           03  WRK-FEDT-NAME           PIC X(200).
           03  WRK-FEDT-SRYKBN-SAVE    PIC X(02).
           03  WRK-FEDT-SRYKBN         PIC X(02).
           03  WRK-FEDT-ZAITEN         PIC 9(08).
           03  WRK-FEDT-YKZTEN         PIC 9(07).
           03  WRK-FEDT-KIZAITEN       PIC 9(07).
           03  WRK-FEDT-TENKBN         PIC X(01).
           03  WRK-FEDT-ZAIKAISU       PIC 9(04).
           03  WRK-FEDT-HKNJANUM       PIC X(08).
           03  WRK-FEDT-RECESYUBETU    PIC 9(04).
           03  WRK-FEDT-YMD.
               05  WRK-FEDT-SRYYM      PIC 9(06).
               05  WRK-FEDT-DD         PIC 9(02).
           03  WRK-FEDT-RECESRYKA      PIC X(02).
           03  WRK-FEDT-SRYKA          PIC X(03).
           03  WRK-FEDT-DRCD           PIC X(05).
           03  WRK-FEDT-BYOTOCD        PIC X(02).
           03  WRK-FEDT-BYOTOKBN       PIC X(01).
           03  WRK-FEDT-NYUGAIKBN      PIC X(01).
           03  WRK-FEDT-SRYSURYO       PIC 9(05)V9(05).
           03  WRK-FEDT-TANICD         PIC 9(03).
           03  WRK-FEDT-MEISAI-TEN     PIC 9(08).
           03  WRK-FEDT-MEISAI-YKZ     PIC 9(09)V9(03).
           03  WRK-FEDT-MEISAI-KIZ     PIC 9(09)V9(03).
           03  WRK-FEDT-DEKIDAKA-TEN   PIC S9(08).
           03  WRK-FEDT-DEKIDAKA-SUM   PIC 9(08).
           03  WRK-FEDT-DEKIDAKA-SUB   PIC 9(01).
           03  WRK-FEDT-HOUKATU-FLG    PIC X(01).
           03  WRK-FEDT-PREV-TEN       PIC 9(08).
           03  WRK-FEDT-PREV-TEN50     PIC 9(08).
           03  WRK-FEDT-PREV-TEN721    PIC 9(08).
      *
      *    保険組み合わせ退避ワーク
       01  THKNCOMBI-AREA.
           02      TCOMB-MAX        PIC 9(05).
           02      TCOMB-OCC        OCCURS     30.
           COPY    "CPHKNCOMBI.INC" REPLACING   //COMB-//
                                    BY          //TCOMB-//.
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
           COPY    "CPDPCCONST.INC".
      *
      *    退院時投薬コメント
      **     COPY    "CPDPCTTOYAKU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1009.INC".
      *
           COPY    "CPSK2005.INC".
      *
           COPY    "CPSK2009.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK9102.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院会計作成用パラメタ
           COPY    "CPORCSDPCNYUIN.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
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
                                       TTNS-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA1
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-SYARECEKBN
                                               WRK-PARA-KKHRECEKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-CREYMD
                                               WRK-PARA-CREHMS
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ＥＦファイル作成"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCRDPC10"     TO  JOB-PGID
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *
           PERFORM 110-PTNUM-KETA-SEC
      *
           MOVE    "RECE620"           TO  RECE62PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE62PARA-HOSPNUM
           MOVE    "RECE649"           TO  RECE64PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE64PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE64PARA-HOSPNUM
           MOVE    "RECE651"           TO  RECE65PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE65PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE65PARA-HOSPNUM
           MOVE    "RECE690"           TO  RECE69PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE69PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE69PARA-HOSPNUM
           MOVE    "RECE680"           TO  RECE68PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE68PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE68PARA-HOSPNUM
           MOVE    "RECE900"           TO  RECE900PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE900PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE900PARA-HOSPNUM
      *
           OPEN    I-O                 RECE62-FILE
           IF      STS-RECE62          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE62PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE62          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE641-FILE
           IF      STS-RECE64          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE64PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE64          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE65-FILE
           IF      STS-RECE65          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE65PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE65          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE69-FILE
           OPEN    INPUT               RECE68-FILE
           OPEN    OUTPUT              DPCE-FILE
           OPEN    OUTPUT              DPCF-FILE
      *
           OPEN    OUTPUT              RECE900-FILE
           CLOSE                       RECE900-FILE
           OPEN    I-O                 RECE900-FILE
      *
           IF      FLG-END             =   ZERO
      *        ファイル読み込み
               PERFORM 900-RECE62-READ-SEC
      *
      *        時間外加算点数取得
               INITIALIZE                  WRK-CONS-JIKANGAI-TEN-TABLE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-JIKANGAI-MAX
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    WRK-CONS-JIKANGAI-SRYCD (IDX)
                                           TO  TNS-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   MOVE    TNS-TEN         TO
                                           WRK-CONS-JIKANGAI-TEN (IDX)
               END-PERFORM
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ取得
      *****************************************************************
       110-SYSKANRI-HENSYU-SEC      SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関ＩＤ編集
              INITIALIZE                  SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD(1:6)
              MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
              MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
              MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC         TO  SYS-1001-REC
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL   FLG-SYSKANRI  =    1
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
              IF      SYSKAN-IDX          =   ZERO
                   MOVE    "医療機関情報が取得できませんでした。"
                                          TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
              END-IF
           END-IF
      *
      *    診療科情報
           MOVE    SPACE               TO  TBL-SRYKA-AREA
           INITIALIZE                      TBL-SRYKA-AREA
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    ZERO                TO  SYSKAN-IDX2
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL   FLG-SYSKANRI  =    1
                   MOVE    MCPDATA-REC TO  SYS-1005-REC
                   ADD     1           TO  SYSKAN-IDX2
                   MOVE    SYS-1005-KBNCD (1:2)
                                       TO  TBL-SRYKA (SYSKAN-IDX2)
                   MOVE    SYS-1005-RECESRYKA
                                       TO  TBL-RECESRYKA (SYSKAN-IDX2)
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    SYSKAN-IDX2         TO  TBL-SRYKA-MAX
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           IF      SYSKAN-IDX2         =   ZERO
                   MOVE    "診療科情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    レセプト編集区分情報
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD(7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
               IF      SYS-2005-NI-JYOKEN (3)  NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                               TO  SYS-2005-NI-JYOKEN(3)
               END-IF
               IF      SYS-2005-NI-JYOKEN (4)  NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                               TO  SYS-2005-NI-JYOKEN(4)
               END-IF
           ELSE
               MOVE    SPACE            TO  SYS-2005-REC
               INITIALIZE                   SYS-2005-REC
               MOVE    WRK-CONS-JYOKEN-TENSU
                                        TO  SYS-2005-NI-JYOKEN(3)
                                            SYS-2005-NI-JYOKEN(4)
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
      *
      *    レセプト編集区分情報
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2009-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD(7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
           ELSE
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-SYAHO
                                   =   SPACE )
               MOVE    "1"             TO  SYS-2009-SYUKA-SYAHO
           END-IF      
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOKUHO
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOKUHO
           END-IF
      *
           MOVE    SPACE               TO  SYS-9102-REC
           INITIALIZE                      SYS-9102-REC
           MOVE    "9102"              TO  SYS-9102-KANRICD
           MOVE    "*"                 TO  SYS-9102-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-9102-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-9102-STYUKYMD(7:2)
           MOVE    SYS-9102-STYUKYMD   TO  SYS-9102-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9102-HOSPNUM
           MOVE    SYS-9102-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9102-REC
           ELSE
               INITIALIZE                      SYS-9102-REC
           END-IF
           IF      SYS-9102-NUMBER     =   SPACE
               MOVE    "0"                 TO  SYS-9102-NUMBER
           END-IF
           .
       110-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理より患者番号桁数取得
      *****************************************************************
       110-PTNUM-KETA-SEC      SECTION.
      *
      *
           INITIALIZE                      SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF       FLG-SYSKANRI   =   ZERO
                 MOVE    MCPDATA-REC   TO  SYS-1009-REC
                 DISPLAY "SYS-1009-PTNUMKSIKBN = "
                          SYS-1009-PTNUMKSIKBN
                 EVALUATE  SYS-1009-PTNUMKSIKBN
                     WHEN  "1"
                         DISPLAY "SYS-1009-JIYKSIKETA = "
                                  SYS-1009-JIYKSIKETA
                         COMPUTE  WRK-ZEROKETA  =   10  -
                                  SYS-1009-JIYKSIKETA
                     WHEN  "2"
                         DISPLAY "SYS-1009-HJNKSIRENNUMKETA = "
                                  SYS-1009-HJNKSIRENNUMKETA
                         COMPUTE  WRK-ZEROKETA  =   10  -
                                  SYS-1009-HJNKSIRENNUMKETA
                     WHEN  "3"
                         DISPLAY "SYS-1009-KKCKSIMAEKETA = "
                                  SYS-1009-KKCKSIMAEKETA
                         DISPLAY "SYS-1009-KKCKSIRENNUMKETA = "
                                  SYS-1009-KKCKSIRENNUMKETA
                         DISPLAY "SYS-1009-KKCKSIATOKETA = "
                                  SYS-1009-KKCKSIATOKETA
                         COMPUTE  WRK-ZEROKETA  =   10  -
                                  SYS-1009-KKCKSIMAEKETA  -
                                  SYS-1009-KKCKSIRENNUMKETA -
                                  SYS-1009-KKCKSIATOKETA
                 END-EVALUATE
                 DISPLAY "WRK-ZEROKETA = " WRK-ZEROKETA
                 COMPUTE  WRK-PTNUMKETA  = WRK-ZEROKETA  +  1
                 DISPLAY "WRK-PTNUMKETA = " WRK-PTNUMKETA
               ELSE
                 MOVE    "患者番号構成が取得できませんでした。"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       110-PTNUM-KETA--EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療年月の末日の取得
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE62-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
      *
      *    患者保険組み合わせ取得処理
           PERFORM 2001-HKNCOMBI-GET-SEC
      *
      *    診療年月・患者ID・レセ科別処理
           MOVE    KEY-NEW         TO  KEY-OLD
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      KEY-NEW1
                                                   NOT =   KEY-OLD1
      *
      *        入院会計取得処理（提出先別）
               PERFORM 2001-NYUINRYO-GET-SEC
      *
      *        診療年月・患者ID・レセ科・提出先別処理
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM                 UNTIL   FLG-END     =   1
                                       OR      KEY-NEW NOT =   KEY-OLD
      *
                   MOVE    ZERO                TO  WRK-RENNUM
                   MOVE    ZERO                TO  WRK-MEISAI-RENNUM
                   MOVE    SPACE               TO  WRK-FEDT-SRYKBN-SAVE
                   MOVE    ZERO                TO  WRK-COM-RENNUM
                                                   FLG-COMMENT
                                                   FLG-TOKUTEINYUIN
                   MOVE    SPACE               TO  WRK-COM-SRYKBN
                                                   WRK-COM-FTNKBN
                   INITIALIZE                 TBL-KENTAI-RECE64-AREA
                                              TBL-KENSA-RECE64-AREA
      *
                   MOVE    ZERO               TO  WRK-RENNUM-E
                                                  WRK-RENNUM-F 
      *
      *            実日数・合計点数等のＩＮＤＥＸ
                   MOVE    1                   TO  IDX1
                   MOVE    2                   TO  IDX2
                   MOVE    3                   TO  IDX3
      *
      *            包括分データ作成
                   PERFORM     9999-HOUKATU-SEC
      *            摘要レコード
                   PERFORM     2007-HENSYU-SEC
      *
                   ADD     1           TO  CNT-COMMIT
                   IF      CNT-COMMIT      =    300
                       MOVE  ZERO      TO      CNT-COMMIT
                       PERFORM 900-DBCOMMIT-SEC
                      PERFORM 900-DBSTART-SEC
                   END-IF
      *
                   PERFORM     900-RECE62-READ-SEC
               END-PERFORM
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       2001-HKNCOMBI-GET-SEC    SECTION.
      *
           INITIALIZE                  THKNCOMBI-AREA
      *
           MOVE    ZERO            TO  FLG-HKNCOMBI
           INITIALIZE                  HKNCOMBI-REC
           MOVE    RECE62-PTID     TO  COMB-PTID
           MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-KEY2-SEL-SEC
           IF    ( FLG-HKNCOMBI    =   ZERO )
               PERFORM 910-HKNCOMBI-KEY2-FETCH-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-HKNCOMBI    =   1 )
                    OR   ( TCOMB-MAX      >=   30 )
      *
               COMPUTE TCOMB-MAX   =   TCOMB-MAX   +   1
               MOVE    HKNCOMBI-REC    TO  TCOMB-OCC (TCOMB-MAX)
      *
               PERFORM 910-HKNCOMBI-KEY2-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2001-HKNCOMBI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号別保険履歴情報編集処理
      *****************************************************************
       2001-RIREKI-HEN-SEC      SECTION.
      *
      *    診療年月編集
           MOVE    RECE62-SRYYM        TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMST
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE62-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD 
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMED
      *
           MOVE    SPACE               TO  WRK-HKN-RIREKI-AREA
           MOVE    ZERO                TO  WRK-RIREKI-MAX
                                           CNT-RECE69
      *
           INITIALIZE                      RECE69-REC
           MOVE    RECE62-HOSPNUM      TO  RECE69-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE69-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE69-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE69-PTID
           MOVE    RECE62-RECEKA       TO  RECE69-RECEKA
      *
           START   RECE69-FILE
               KEY IS    >=   RECE69-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE69
               NOT INVALID
                   PERFORM 990-RECE69-NEXT-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE69           =   1                )
                   OR    ( RECE62-HOSPNUM   NOT =   RECE69-HOSPNUM   )
                   OR    ( RECE62-SRYYM     NOT =   RECE69-SRYYM     )
                   OR    ( RECE62-NYUGAIKBN NOT =   RECE69-NYUGAIKBN )
                   OR    ( RECE62-PTID      NOT =   RECE69-PTID      )
                   OR    ( RECE62-RECEKA    NOT =   RECE69-RECEKA    )
      *
               ADD     1                  TO  CNT-RECE69
      *
               ADD     1                  TO  WRK-RIREKI-MAX
               IF      WRK-RIREKI-MAX     >   50
                   MOVE    "保険履歴情報がテーブルオーバーしました"
                                              TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-RIREKI-HEN-EXT
               END-IF    
      *
               EVALUATE    RECE69-TEISYUTUSAKI
                   WHEN    "1"
                       MOVE    "社保"     TO  WRK-HKN-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE      TO  WRK-HKN-TEISYUTUSAKI
               END-EVALUATE
               IF    ( RECE69-NUM         =   SPACE )
               AND   ( RECE69-KIGO        =   SPACE )
               AND   ( RECE69-HKNJANUM    =   SPACE )
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE69-TANSEIDONAME    DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  WRK-HKN-HKNJANUM
                   IF      RECE69-HKNJANUM     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE69-HKNJANUM TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-HKNJANUM
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-KIGO
                   IF      RECE69-KIGO     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE69-KIGO     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-KIGO
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-NUM
                   IF      RECE69-NUM      NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE69-NUM      TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO  WRK-HKN-NUM
                       END-IF
                   END-IF
                   MOVE    SPACE                  TO  WRK-HKN-RIREKI-X
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE69-TANSEIDONAME    DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-HKNJANUM       DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-KIGO           DELIMITED  BY  SPACE
                           "−"                   DELIMITED  BY  SIZE
                           WRK-HKN-NUM            DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI-X
                   END-STRING
                   IF      WRK-HKN-RIREKI-X (53:) NOT =   SPACE
                       STRING  WRK-HKN-TEISYUTUSAKI
                                                  DELIMITED  BY  SPACE
                               RECE69-TANSEIDONAME
                                                  DELIMITED  BY  SPACE
                               "　"               DELIMITED  BY  SIZE
                               WRK-HKN-HKNJANUM   DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                       END-STRING
                       ADD     1                   TO  WRK-RIREKI-MAX
                       IF      WRK-RIREKI-MAX     >   50
                           MOVE
                               "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                           GO  TO  2001-RIREKI-HEN-EXT
                       END-IF
                       STRING  WRK-HKN-KIGO       DELIMITED  BY  SPACE
                               "−"               DELIMITED  BY  SIZE
                               WRK-HKN-NUM        DELIMITED  BY  SPACE
                               INTO
                                   WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
                       END-STRING
                   ELSE
                       MOVE    WRK-HKN-RIREKI-X   TO
                                       WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                   END-IF
               END-IF
               IF      WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
                                          NOT =   SPACE
                   ADD     1                   TO  WRK-RIREKI-MAX
                   IF      WRK-RIREKI-MAX     >   50
                       MOVE    "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  2001-RIREKI-HEN-EXT
                   END-IF
               END-IF
      *
               MOVE    RECE69-TEKSTYMDX   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
      *********IF      RECE69-TEKSTYMDX(1:6)  =   RECE62-SRYYM
      *            MOVE    LNK-DAY2-EDTYMD3(11:12)    TO
      *                           WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(9:)
      *********ELSE
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)     TO
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(3:)
      *********END-IF
      *
               IF      RECE69-TEKEDYMDX(1:6)  =   RECE62-SRYYM
                   MOVE    RECE69-TEKEDYMDX   TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
      *********    MOVE    LNK-DAY2-EDTYMD3(11:12)    TO
      *********                       WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)(3:)
                   MOVE    "から"             TO
                                      WRK-HKN-FL1     (WRK-RIREKI-MAX)
                   MOVE    "まで"             TO
                                      WRK-HKN-FL2     (WRK-RIREKI-MAX)
               ELSE
                   MOVE    "から有効"         TO
                                      WRK-HKN-FL      (WRK-RIREKI-MAX)
               END-IF
      *
               PERFORM 990-RECE69-NEXT-SEC
           END-PERFORM
      *
           IF      CNT-RECE69             =   1
               MOVE    SPACE                  TO  WRK-HKN-RIREKI-AREA
               MOVE    ZERO                   TO  WRK-RIREKI-MAX
           END-IF
           .
       2001-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計取得処理
      *****************************************************************
       2001-NYUINRYO-GET-SEC           SECTION.
      *
           INITIALIZE                      ORCSDPCNYUIN-AREA
           MOVE    RECE62-HOSPNUM      TO  LNK-DPCNYUIN-HOSPNUM
           MOVE    RECE62-PTID         TO  LNK-DPCNYUIN-PTID
           MOVE    RECE62-SRYYM        TO  LNK-DPCNYUIN-SANTEIYM
           CALL    "ORCSDPCNYUIN"      USING   ORCSDPCNYUIN-AREA
                                               SPA-AREA
           IF      LNK-DPCNYUIN-RC     NOT =   ZERO
               DISPLAY "ORCSDPCNYUIN ERROR PTID= " RECE62-PTID
                       " LNK-DPCNYUIN-RC=" LNK-DPCNYUIN-RC
           END-IF
           .
       2001-NYUINRYO-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要レコード編集処理
      *****************************************************************
       2007-HENSYU-SEC              SECTION.
      *
      *    入退院履歴編集処理
           PERFORM 20079-HC101-NYUTAI-RIREKI-HEN-SEC
      *
      *    摘要情報
           INITIALIZE                      RECE641-REC
           MOVE    RECE62-HOSPNUM      TO  RECE641-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE641-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE641-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE641-PTID
           MOVE    RECE62-RECEKA       TO  RECE641-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE641-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE641-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE641-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE641-HOJOKBN
           MOVE    RECE62-KOHID-KEY    TO  RECE641-KOHID
           MOVE    RECE62-TEKSTYMD     TO  RECE641-TEKSTYMD
           MOVE    RECE62-RECEIPT-KBN  TO  RECE641-RECEIPT-KBN
      *
           START   RECE641-FILE
               KEY IS    >=   RECE641-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE64
               NOT INVALID
                   PERFORM 990-RECE641-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1                   )
               OR     ( FLG-RECE64           =   1                   )
               OR     ( RECE62-HOSPNUM   NOT =   RECE641-HOSPNUM      )
               OR     ( RECE62-SRYYM     NOT =   RECE641-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE641-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE641-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE641-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE641-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE641-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE641-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE641-HOJOKBN      )
               OR     ( RECE62-KOHID-KEY NOT =   RECE641-KOHID        )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE641-TEKSTYMD     )
               OR     ( RECE62-RECEIPT-KBN
                                         NOT =   RECE641-RECEIPT-KBN  )
               OR     ( RECE641-SRYKBN       >=  "90"                 )
      *
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
      *        摘要データ編集処理
               IF      FLG-END           =   ZERO
                   PERFORM 20074-ZAI-KOUI-SORT-SEC
                   PERFORM 20072-ZAI-HENSYU-SEC
      *
                   PERFORM 990-RECE641-NEXT-SEC
               END-IF
           END-PERFORM
      *
      *    入院会計編集処理
           PERFORM 20077-NYUIN-HENSYU-SEC
           .
       2007-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    入退院履歴編集処理
      *****************************************************************
       20079-HC101-NYUTAI-RIREKI-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUTAI-RIREKI
           MOVE    ZERO                TO  WRK-NYUINCHUKBN
           MOVE    SPACE               TO  WRK-NYUIN-RIREKI
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      RECE68-REC
           MOVE    RECE62-HOSPNUM      TO  RECE68-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE68-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE68-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE68-PTID
           MOVE    RECE62-RECEKA       TO  RECE68-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE68-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE68-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE68-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE68-HOJOKBN
           MOVE    RECE62-KOHID-KEY    TO  RECE68-KOHID
           MOVE    RECE62-TEKSTYMD     TO  RECE68-TEKSTYMD
           MOVE    RECE62-RECEIPT-KBN  TO  RECE68-RECEIPT-KBN
      *
           START   RECE68-FILE
               KEY IS    >=   RECE68-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE68
               NOT INVALID
                   PERFORM 900-RECE68-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE68           =  1                   )
                   OR    ( RECE62-HOSPNUM   NOT =  RECE68-HOSPNUM      )
                   OR    ( RECE62-SRYYM     NOT =  RECE68-SRYYM        )
                   OR    ( RECE62-NYUGAIKBN NOT =  RECE68-NYUGAIKBN    )
                   OR    ( RECE62-PTID      NOT =  RECE68-PTID         )
                   OR    ( RECE62-RECEKA    NOT =  RECE68-RECEKA       )
                   OR    ( RECE62-TEISYUTUSAKI
                                             NOT =  RECE68-TEISYUTUSAKI)
                   OR    ( RECE62-RECESYUBETU
                                             NOT =  RECE68-RECESYUBETU )
                   OR    ( RECE62-HKNJANUM-KEY
                                             NOT =  RECE68-HKNJANUM    )
                   OR    ( RECE62-HOJOKBN-KEY
                                             NOT =  RECE68-HOJOKBN     )
                   OR    ( RECE62-KOHID-KEY  NOT =  RECE68-KOHID       )
                   OR    ( RECE62-TEKSTYMD   NOT =  RECE68-TEKSTYMD    )
                   OR    ( RECE62-RECEIPT-KBN
                                             NOT =  RECE68-RECEIPT-KBN )
      *
               IF   (( RECE68-NYUINYMD (1:6)  <=  RECE62-SRYYM ) AND
                     (( RECE68-TAIINYMD (1:6) >=  RECE62-SRYYM ) OR
                      ( RECE68-TAIINYMD       =   SPACE        )))
                 OR ( RECE68-NYUINCHUKBN      =   "1"            )
                 OR ( WRK-NYUINCHUKBN         =   "1"            )
                   IF   WRK-NYUINCHUKBN  =   "1"
                        CONTINUE
                   ELSE
                        ADD  1                TO  IDX
                        IF   IDX              >   10
                             DISPLAY "テーブルオーバー WRK-NYUIN-RIREKI"
                             GO  TO   20079-HC101-NYUTAI-RIREKI-HEN-EXT
                        END-IF
                        MOVE RECE68-NYUINYMD  TO
                                        WRK-NYUIN-RIREKI-NYUINYMD (IDX)
                   END-IF
                   IF   RECE68-TAIINYMD  =   SPACE
                        MOVE  "99999999" TO
                                   WRK-NYUIN-RIREKI-TAIINYMD (IDX)
                   ELSE
                        MOVE  RECE68-TAIINYMD TO
                                   WRK-NYUIN-RIREKI-TAIINYMD (IDX)
                   END-IF
               END-IF
      *
               MOVE    RECE68-NYUINCHUKBN  TO  WRK-NYUINCHUKBN
      *
               PERFORM 900-RECE68-NEXT-SEC
           END-PERFORM
           .
       20079-HC101-NYUTAI-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤　明細テーブル退避処理
      *****************************************************************
       20071-ZAI-TAIHI-SEC    SECTION.
      *
           INITIALIZE                      TBL-ZAIW-AREA
                                           TBL-KOUI-AREA
                                           TBL-COMMENT-AREA
           INITIALIZE                      WRK-RECE65-REC
                                           WRK-KASAN-RECE65-REC
                                           WRK-TEIGEN-RECE65-REC
                                           WRK-700-RECE65-AREA
                                           WRK-721-RECE65-REC
                                           WRK-SRYKBN70-RECE65-REC
      *
           MOVE    ZERO                TO  WRK-ZAIWMAX
           MOVE    ZERO                TO  WRK-TBLMAX1
                                           WRK-TBLMAX6
                                           WRK-TBLMAX7
                                           WRK-TBLMAX8
                                           FLG-TEIGEN
                                           FLG-YKZ20
                                           FLG-KOKUJISKBKBN1
                                           FLG-GAITENTTLKBN
                                           FLG-SRYSYUKBN721
      *************************************FLG-SRYSYUKBN700
                                           FLG-SRYKBN70-TEIGEN
                                           FLG-KENSA-KASAN
                                           FLG-KENSA-TEIGEN
                                           WRK-SRYCD1-MAX
                                           WRK-SRYCD71-MAX
                                           WRK-SRYCD72-MAX
                                           WRK-KOKUJISKBKBN1-KOUI-TEN
      *
           MOVE    RECE641-SRYKBN      TO  WRK-SRYKBN
      *
           INITIALIZE                       RECE65-REC
           MOVE    RECE641-HOSPNUM      TO  RECE65-HOSPNUM
           MOVE    RECE641-SRYYM        TO  RECE65-SRYYM
           MOVE    RECE641-NYUGAIKBN    TO  RECE65-NYUGAIKBN
           MOVE    RECE641-PTID         TO  RECE65-PTID
           MOVE    RECE641-RECEKA       TO  RECE65-RECEKA
           MOVE    RECE641-TEISYUTUSAKI TO  RECE65-TEISYUTUSAKI
           MOVE    RECE641-RECESYUBETU  TO  RECE65-RECESYUBETU
           MOVE    RECE641-HKNJANUM     TO  RECE65-HKNJANUM
           MOVE    RECE641-HOJOKBN      TO  RECE65-HOJOKBN
           MOVE    RECE641-KOHID        TO  RECE65-KOHID
           MOVE    RECE641-TEKSTYMD     TO  RECE65-TEKSTYMD
           MOVE    RECE641-RECEIPT-KBN  TO  RECE65-RECEIPT-KBN
           MOVE    RECE641-ZAINUM       TO  RECE65-ZAINUM
           START   RECE65-FILE
               KEY IS    >=   RECE65-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE65
               NOT INVALID
                   PERFORM 990-RECE65-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END                =   1                   )
               OR     ( FLG-RECE65             =   1                   )
               OR     ( RECE641-HOSPNUM    NOT =   RECE65-HOSPNUM      )
               OR     ( RECE641-SRYYM      NOT =   RECE65-SRYYM        )
               OR     ( RECE641-NYUGAIKBN  NOT =   RECE65-NYUGAIKBN    )
               OR     ( RECE641-PTID       NOT =   RECE65-PTID         )
               OR     ( RECE641-RECEKA     NOT =   RECE65-RECEKA       )
               OR     ( RECE641-TEISYUTUSAKI
                                           NOT =   RECE65-TEISYUTUSAKI )
               OR     ( RECE641-RECESYUBETU
                                           NOT =   RECE65-RECESYUBETU  )
               OR     ( RECE641-HKNJANUM   NOT =   RECE65-HKNJANUM     )
               OR     ( RECE641-HOJOKBN    NOT =   RECE65-HOJOKBN      )
               OR     ( RECE641-KOHID      NOT =   RECE65-KOHID        )
               OR     ( RECE641-TEKSTYMD   NOT =   RECE65-TEKSTYMD     )
               OR     ( RECE641-RECEIPT-KBN
                                           NOT =   RECE65-RECEIPT-KBN  )
               OR     ( RECE641-ZAINUM     NOT =   RECE65-ZAINUM       )
      *
      *        9999包括分剤は自保区分に１がセットされている
               IF       RECE641-JIHOKBN        =   "1"
                   MOVE      "6"               TO  WRK-FEDT-HOUKATU-FLG
               ELSE
                   MOVE      "0"               TO  WRK-FEDT-HOUKATU-FLG
               END-IF
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
                         OR      ( FLG-END           =   1     )
      *
      *         入院料はコメントのみ対象とする
                IF    ( RECE641-SRYKBN               >=  "90"      )
                AND   ( RECE65-SRYCD (IDX) (1:1) NOT =   "8"   AND
                        RECE65-SRYCD (IDX) (1:3) NOT =   "008" AND
                        RECE65-SRYCD (IDX)       NOT =  "190128310" AND
                        RECE65-SRYCD (IDX)       NOT =  "190128410" AND
                        RECE65-SRYCD (IDX)       NOT =  "190128570")
                 CONTINUE
                ELSE
      *
                 IF    (WRK-PARA-SRYYM          >=  "201004")   AND
                       (RECE65-SRYCD (IDX)       =  "190128310" OR
                                                    "190128410" OR
                                                    "190128570")
                     CONTINUE
                 ELSE
      *
                 IF      RECE65-RECEDENFLG (IDX)     =   SPACE
                   MOVE    SPACE                TO  WRK-KIZAI-SRYCD
                   IF      RECE65-SRYCD (IDX) (1:3) = "059"
                       MOVE    RECE65-SRYCD (IDX)
                                                TO  WRK-KIZAI-SRYCD
                       MOVE    "777770000"      TO  RECE65-SRYCD (IDX)
                   END-IF
      *
                   EVALUATE    RECE65-SRYCD (IDX) (1:1)
                       WHEN    "0"
                       WHEN    "8"
                           IF   (RECE65-SRYKBN = "21" OR "22" OR "23" OR
                                                 "14")
                           AND ((RECE65-SRYCD(IDX)
                                               = "840000006")
                           OR   (RECE65-SRYCD(IDX)
                                               = SYS-9102-SRYCD(1))
                           OR   (RECE65-SRYCD(IDX)
                                               = SYS-9102-SRYCD(2)))
                               MOVE  "1"      TO WRK-FEDT-HOUKATU-FLG
      *            テーブルが３件なので繰り返しは保留にしIF文のみとした 
      *                        PERFORM VARYING TBL-TAITOYAKU-IDX
      *                            FROM  1  BY  1
      *                            UNTIL (TBL-TAITOYAKU-IDX
      *                                  >  TBL-TAITOYAKU-MAX)
      *                              OR  (TBL-TAITOYAKU
      *                                   (TBL-TAITOYAKU-IDX) 
      *                                  =  "000000000")
      *                            IF  RECE65-SRYCD(IDX)
      *                                  =  TBL-TAITOYAKU
      *                                      (TBL-TAITOYAKU-IDX) 
      *                                MOVE "1" TO WRK-FEDT-HOUKATU-FLG
      *                            END-IF
      *                        END-PERFORM
                           END-IF
                       WHEN    "1"
                           PERFORM 200711-KOUI-TAIHI-SEC
                       WHEN    "6"
                           PERFORM 200711-YKZ-TAIHI-SEC
                       WHEN    "7"
                           PERFORM 200711-KIZAI-TAIHI-SEC
                   END-EVALUATE
                 END-IF
                 END-IF
                END-IF
               END-PERFORM
      *
               PERFORM 990-RECE65-NEXT-SEC
           END-PERFORM
      *
      *    画像診断のとき順番を変える
      *
           IF      FLG-SRYSYUKBN721    >   ZERO
               MOVE     FLG-SRYSYUKBN721   TO  IDX
               MOVE    WRK-721-RECE65-REC  TO  RECE65-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX
                   MOVE    WRK-SRYKBN70-RECE65-REC
                                               TO  RECE65-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX
                       MOVE     FLG-SRYSYUKBN700  (IDXY)  TO  IDX
                       MOVE    WRK-700-RECE65-REC (IDXY)  TO  RECE65-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-PERFORM
               END-IF
           END-IF
      *
      *    生体検査とき順番を変える
           IF      FLG-KENSA-TEIGEN    >   ZERO
               MOVE    FLG-KENSA-TEIGEN        TO  IDX
               MOVE    WRK-TEIGEN-RECE65-REC   TO  RECE65-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
           END-IF
           IF      FLG-KENSA-KASAN     >   ZERO
               MOVE     FLG-KENSA-KASAN        TO  IDX
               MOVE    WRK-KASAN-RECE65-REC    TO  RECE65-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
           END-IF
           .
       20071-TBL-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ退避処理
      *****************************************************************
       200711-KOUI-TAIHI-SEC          SECTION.
      *
      *    画像診断・生体検査のとき順番を変える
           IF      RECE65-SRYSYUKBN-X (IDX)
                                       =   "721"
               MOVE    RECE65-REC          TO  WRK-RECE65-REC
               MOVE    IDX                 TO  IDW1
      *
               IF      FLG-SRYSYUKBN721    >   ZERO
                   MOVE     FLG-SRYSYUKBN721   TO  IDX
                   MOVE    WRK-721-RECE65-REC  TO  RECE65-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO                TO  FLG-SRYSYUKBN721
                   INITIALIZE                  WRK-721-RECE65-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX
                       MOVE    WRK-SRYKBN70-RECE65-REC
                                               TO  RECE65-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       MOVE    ZERO            TO  FLG-SRYKBN70-TEIGEN
                       INITIALIZE              WRK-SRYKBN70-RECE65-REC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX
                           MOVE     FLG-SRYSYUKBN700  (IDXY)
                                                           TO  IDX
                           MOVE    WRK-700-RECE65-REC (IDXY)
                                                           TO RECE65-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM
      *****************MOVE    ZERO                TO  FLG-SRYSYUKBN700
                       INITIALIZE                  WRK-700-RECE65-AREA
                   END-IF
               END-IF
      *
               MOVE    WRK-RECE65-REC      TO  RECE65-REC
               MOVE    IDW1                TO  IDX
      *
               MOVE    IDX                 TO  FLG-SRYSYUKBN721
               MOVE    RECE65-REC          TO WRK-721-RECE65-REC
           ELSE
               IF    ( RECE65-SRYCD (IDX)  =  WRK-CONS-SRYKBN70-TEIGEN )
               AND   ( FLG-SRYSYUKBN721    >  ZERO                     )
                   MOVE    IDX         TO  FLG-SRYKBN70-TEIGEN
                   MOVE    RECE65-REC  TO  WRK-SRYKBN70-RECE65-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               ELSE
                   IF    ( RECE65-SRYSYUKBN-X (IDX)
                                               =   "700" )
                   AND   ( RECE65-DATAKBN   (IDX)
                                               =   "2"   )
                   AND   ( FLG-SRYSYUKBN721    >   ZERO  )
                       ADD     1               TO  WRK-SRYSYUKBN700-MAX
                       MOVE    IDX             TO  FLG-SRYSYUKBN700
                                               (WRK-SRYSYUKBN700-MAX)
                       MOVE    RECE65-REC      TO WRK-700-RECE65-REC
                                               (WRK-SRYSYUKBN700-MAX)
                   ELSE
      *****************IF    ( RECE65-SRYCD (IDX)
      *****************                    =   WRK-CONS-KENSA-KASAN1
      *****************                    OR  WRK-CONS-KENSA-KASAN2 )
      *****************    MOVE    IDX         TO  FLG-KENSA-KASAN
      *****************    MOVE    RECE65-REC  TO  WRK-KASAN-RECE65-REC
      *****************ELSE
      *****************    IF      RECE65-SRYCD (IDX)
      *****************                        =   WRK-CONS-KENSA-TEIGEN
      *****************        MOVE    IDX         TO  FLG-KENSA-TEIGEN
      *****************        MOVE    RECE65-REC  TO
      *****************                            WRK-TEIGEN-RECE65-REC
      *****************    ELSE
                               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *****************    END-IF
      *****************END-IF
                   END-IF
               END-IF
           END-IF
           .
       200711-KOUI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）退避編集処理
      *****************************************************************
       2007111-KOUI-TAIHI-HENSYU-SEC SECTION.
      *
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCRDPC10 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    WRK-TBLMAX1         TO  WRK-SRYCD1-MAX
      *
               MOVE    RECE65-SRYACTINF (IDX)  TO
                                     TBL-KOUI-SRYACTINF (WRK-TBLMAX1)
               MOVE    RECE65-NAMEINF (IDX)    TO
                                     TBL-KOUI-NAMEINF   (WRK-TBLMAX1)
      *
               IF    ( RECE641-SRYKBN      =   "13"  OR  "14" )
                   IF      TBL-KOUI-KOKUJISKBKBN1(WRK-TBLMAX1)
                                           NOT =   "1"
                       MOVE    IDX             TO  FLG-KOKUJISKBKBN1
                   END-IF
                   IF    ( TBL-KOUI-KOKUJISKBKBN1(WRK-TBLMAX1)
                                                   =   "1" )
                   AND   ( TBL-KOUI-DATAKBN (WRK-TBLMAX1)
                                                   =   2 )
                       ADD     TBL-KOUI-TEN (WRK-TBLMAX1)
                                       TO  WRK-KOKUJISKBKBN1-KOUI-TEN
                   END-IF
               END-IF
           END-IF
           .
       200711-KOUI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（医薬品）退避処理
      *****************************************************************
       200711-YKZ-TAIHI-SEC          SECTION.
      *
           IF      RECE65-SRYCD (IDX)  =   WRK-CONS-TEIGEN
               CONTINUE
           ELSE
      *         IF      RECE641-YKZTEN    =   ZERO
      *             GO  TO  200711-YKZ-TAIHI-EXT
      *         END-IF
      *         退院時の院外処方も記録する為、上記の判定文を削除
                CONTINUE
           END-IF
      *
           ADD     1                   TO  WRK-TBLMAX6
      *
           IF      WRK-TBLMAX6         =   1
               IF      RECE65-SRYCD (IDX)
                                       =   WRK-CONS-TEIGEN
                   MOVE    1               TO  FLG-TEIGEN
               END-IF
           END-IF
      *
           IF      WRK-TBLMAX6         >   100
               MOVE    "*** ORCRDPC10 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE65-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (1 WRK-TBLMAX6)
               MOVE    RECE65-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (1 WRK-TBLMAX6)
               MOVE    RECE65-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (1 WRK-TBLMAX6)
               MOVE    RECE65-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(1 WRK-TBLMAX6)
               MOVE    RECE65-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (1 WRK-TBLMAX6)
               MOVE    RECE65-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (1 WRK-TBLMAX6)
               MOVE    RECE65-NAME       (IDX)  TO
                                   TBL-ZAIW-NAME       (1 WRK-TBLMAX6)
               MOVE    RECE65-YUKOKETA   (IDX)  TO
                                   TBL-ZAIW-YUKOKETA   (1 WRK-TBLMAX6)
           END-IF
           .
       200711-YKZ-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（その他）退避処理
      *****************************************************************
       200711-KIZAI-TAIHI-SEC          SECTION.
      *
           IF    ( RECE641-SRYKBN  NOT =   "70" )
           AND   ( RECE641-KIZAITEN    =   ZERO )
               GO  TO  200711-KIZAI-TAIHI-EXT
           END-IF
      *
      *    酸素補正率１．３（１気圧）のとき
           IF     RECE65-SRYCD (IDX)    =   WRK-CONS-SANSO-HOSEI
               GO  TO  200711-KIZAI-TAIHI-EXT
           END-IF
      *
           ADD     1                   TO  WRK-TBLMAX7
      *
           IF      WRK-TBLMAX7         >   100
               MOVE    "*** ORCRDPC10 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE65-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
               MOVE    RECE65-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
               MOVE    RECE65-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (2 WRK-TBLMAX7)
               MOVE    RECE65-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(2 WRK-TBLMAX7)
               MOVE    RECE65-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (2 WRK-TBLMAX7)
               MOVE    RECE65-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (2 WRK-TBLMAX7)
               MOVE    RECE65-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF    (2 WRK-TBLMAX7)
      *
               IF    ( RECE65-DATAKBN (IDX)
                                           =   "3"  )
               AND   ( RECE641-SRYKBN      =   "70" )
                   MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX
               ELSE
                   IF      RECE65-SRYCD (IDX)
                                              =   "799990070"
                       MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX
                   ELSE
                       MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX
                   END-IF
               END-IF
           END-IF
      *
      *    酸素のとき補正率を編集
           IF    ( RECE65-SRYCD (IDX)  =   WRK-CONS-SANSO1
                                       OR  WRK-CONS-SANSO2
                                       OR  WRK-CONS-SANSO3
                                       OR  WRK-CONS-SANSO4
                                       OR  WRK-CONS-SANSO5
                                       OR  WRK-CONS-SANSO6
                                       OR  WRK-CONS-SANSO7
                                       OR  WRK-CONS-SANSO8 )
               ADD     1                   TO  WRK-TBLMAX7
      *
               IF      WRK-TBLMAX7         >   100
                   MOVE    "*** ORCRDPC10 TABLE 100 OVER  ***"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
               ELSE
                   MOVE    WRK-CONS-SANSO-HOSEI TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
                   MOVE    1                   TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
                   MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX
               END-IF
           END-IF
           .
       200711-KIZAI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）並び替え処理
      *****************************************************************
       20074-ZAI-KOUI-SORT-SEC          SECTION.
      *
           INITIALIZE                   WRK-KOUI-AREA
                                        WRK-KOUI-TAIHI-AREA
      *
           IF      RECE641-SRYKBN       =   "50"    OR  "54"    OR
                                            "60"    OR  "40"    OR
                                            "80"
               CONTINUE
           ELSE
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           IF      WRK-TBLMAX1          =   1
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-TBLMAX1
               MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-TAIHI-REC          (IDX)
               MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-TAIHI-SRYCD        (IDX)
               MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-TAIHI-TENSIKIBETU  (IDX)
               MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-TAIHI-CHUKSNCD     (IDX)
               MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDX)
           END-PERFORM
      *
           INITIALIZE                      TBL-KOUI-AREA
           MOVE    ZERO                TO  WRK-TBLMAX1
      *
           MOVE    1                   TO  IDX
           PERFORM UNTIL   IDX >=      100
                   OR      WRK-KOUI-TAIHI-SRYCD (IDX)  =   SPACE
               EVALUATE    WRK-KOUI-TAIHI-SRYCD (IDX) (1:1)
                   WHEN    "0"
                   WHEN    "8"
                       IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                           PERFORM 200741-KASAN-HENSYU-SEC
                       END-IF
                       ADD     1           TO  WRK-TBLMAX1
                       MOVE    WRK-KOUI-TAIHI-REC (IDX)
                                           TO  TBL-KOUI-REC(WRK-TBLMAX1)
                       COMPUTE IDX     =   IDX     +   1
                   WHEN    OTHER
      *                基本項目・加算項目・通則加算項目に分ける
                       PERFORM UNTIL   IDX >=      100
                               OR      WRK-KOUI-TAIHI-SRYCD (IDX)
                                                       =   SPACE
                               OR      WRK-KOUI-TAIHI-SRYCD (IDX) (1:1)
                                                   NOT =   "1"
                           IF  WRK-KOUI-TAIHI-SRYCD (IDX) = "150268970"
                              MOVE    1      TO
                                      WRK-KOUI-TAIHI-KOKUJISKBKBN1 (IDX)
                           END-IF
                           EVALUATE    WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDX)
                               WHEN    "1"
                               WHEN    "3"
                               WHEN    "5"
                                   IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                                       PERFORM 200741-KASAN-HENSYU-SEC
                                   END-IF
                                   MOVE    1               TO  IDZ
                               WHEN    "7"
                                   IF      WRK-KOUI-TAIHI-SRYCD (IDX)
                                                           =
                                                 WRK-CONS-CHOONPA-KASAN
                                     MOVE    4               TO  IDZ
                                   ELSE
                                     IF      WRK-KOUI-TAIHI-SRYCD (IDX)
                                                           =
                                         WRK-CONS-SISETUKIJYUN-KENSA  OR
                                         WRK-CONS-SISETUKIJYUN-GAZOU  OR
                                         WRK-CONS-SISETUKIJYUN-HOUSYASEN
                                       MOVE    2               TO  IDZ
                                     ELSE
                                      IF    ( WRK-KOUI-TAIHI-TENSIKIBETU
                                                               (IDX)
                                                         =   5   OR  6 )
                                      AND   ( WRK-KOUI-TAIHI-CHUKSNCD
                                                                 (IDX)
                                                         =   "0000"    )
                                      AND   ( RECE641-SRYKBN
                                                     NOT =   "54"      )
                                        MOVE    3               TO  IDZ
                                      ELSE
                                        MOVE    2               TO  IDZ
                                      END-IF
                                     END-IF
                                   END-IF
                               WHEN    "9"
                                   MOVE    4               TO  IDZ
                           END-EVALUATE
                           ADD     1           TO  WRK-KOUI-TBLMAX (IDZ)
                           MOVE    WRK-KOUI-TBLMAX (IDZ)
                                               TO  IDY
                           MOVE    WRK-KOUI-TAIHI-REC (IDX)
                                               TO
                                               WRK-KOUI-REC (IDZ IDY)
                           COMPUTE IDX     =   IDX     +   1
                       END-PERFORM
               END-EVALUATE
           END-PERFORM
      *
           IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
               PERFORM 200741-KASAN-HENSYU-SEC
           END-IF
           .
       20074-ZAI-KOUI-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       200741-KASAN-HENSYU-SEC          SECTION.
      *
      *    加算項目の記録順
           IF      WRK-KOUI-TBLMAX (2)         >   ZERO
               COMPUTE IDY     =   WRK-KOUI-TBLMAX (2) -   1
               PERFORM VARYING IDY1    FROM    1   BY  1
                       UNTIL   IDY1    >       IDY     
                   COMPUTE IDZ     =   IDY1    +   1
                   PERFORM VARYING IDY2    FROM   IDZ  BY  1
                           UNTIL   IDY2    >   WRK-KOUI-TBLMAX (2)
                       IF      WRK-KOUI-CHUKSNINF (2 IDY1)
                                       > WRK-KOUI-CHUKSNINF (2 IDY2)
                           MOVE    WRK-KOUI-REC (2 IDY1)  TO
                                                 TBL-KOUI-SORT
                           MOVE    WRK-KOUI-REC (2 IDY2)  TO
                                                 WRK-KOUI-REC (2 IDY1)
                           MOVE    TBL-KOUI-SORT         TO
                                                 WRK-KOUI-REC (2 IDY2)
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
      *    基本項目・加算項目・通則加算項目の順に編集
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       4
               MOVE    WRK-KOUI-TBLMAX (IDY1)  TO  IDY
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       IDY
                   ADD     1               TO  WRK-TBLMAX1
                   MOVE    WRK-KOUI-REC (IDY1 IDY2)
                                           TO  TBL-KOUI-REC(WRK-TBLMAX1)
               END-PERFORM
           END-PERFORM
      *
           INITIALIZE                   WRK-KOUI-AREA
           .
       200741-KASAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       20072-ZAI-HENSYU-SEC          SECTION.
      *
           IF      RECE641-SRYKBN   >=   "90"
              GO  TO   20072-ZAI-HENSYU-EXT
           END-IF
      *
           IF      RECE641-SRYKBN  NOT =   WRK-FEDT-SRYKBN-SAVE
      *    順序番号リセット
      *            順序番号の取り込み
                   INITIALIZE                        RECE900-REC
                   MOVE    RECE641-HOSPNUM       TO  RECE900-HOSPNUM
                   MOVE    RECE641-SRYYM         TO  RECE900-SRYYM
                   MOVE    RECE641-PTID          TO  RECE900-PTID
                   MOVE    RECE641-SRYKBN        TO  RECE900-SRYKBN
                   PERFORM   900-RECE900-READ-SEC
                   IF      FLG-RECE900         =   1
                       MOVE    ZERO        TO  WRK-RENNUM
                   ELSE
                       MOVE    RECE900-RENNUM   TO   WRK-RENNUM
                   END-IF
                   MOVE    RECE641-SRYKBN
                                       TO  WRK-FEDT-SRYKBN-SAVE
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       31
      *    診療行為データ・コメントデータの編集
               IF      RECE641-KAISU (1 IDXX)  >   ZERO
                   MOVE    ZERO        TO  WRK-FEDT-1ST-FLG
      *    順序番号
                   ADD     1           TO  WRK-RENNUM
      *    順序番号ファイルの更新
                   INITIALIZE                        RECE900-REC
                   MOVE    RECE641-HOSPNUM       TO  RECE900-HOSPNUM
                   MOVE    RECE641-SRYYM         TO  RECE900-SRYYM
                   MOVE    RECE641-PTID          TO  RECE900-PTID
                   MOVE    RECE641-SRYKBN        TO  RECE900-SRYKBN
                   PERFORM   900-RECE900-READ-SEC
                   IF    FLG-RECE900   =    1
                         MOVE    RECE641-HOSPNUM TO  RECE900-HOSPNUM
                         MOVE    RECE641-SRYYM   TO  RECE900-SRYYM
                         MOVE    RECE641-PTID    TO  RECE900-PTID
                         MOVE    RECE641-SRYKBN  TO  RECE900-SRYKBN
                         MOVE       WRK-RENNUM   TO  RECE900-RENNUM
                         WRITE      RECE900-REC
                   ELSE
                         MOVE       WRK-RENNUM   TO  RECE900-RENNUM
                         REWRITE    RECE900-REC
                   END-IF
      *    行為明細番号リセット
                   MOVE    ZERO        TO  WRK-MEISAI-RENNUM
                   MOVE    IDXX        TO  WRK-FEDT-DD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-TBLMAX1
      *
      *****    診療行為最終データ又は次がコメントデータの場合
      *****    MOVE    ZERO            TO  FLG-TBLMAX
      *****    IF      IDX             =   WRK-SRYCD1-MAX
      *****        CONTINUE
      *****    ELSE
      *****        COMPUTE IDY1    =   IDX     +   1
      *****        IF      TBL-KOUI-SRYCD (IDY1) (1:1)
      *****                                =   "0" OR  "8"
      *****            MOVE    1               TO  FLG-TBLMAX
      *****        END-IF
      *****    END-IF
      *
                       EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                           WHEN    "0"
                           WHEN    "8"
                               CONTINUE
                           WHEN    OTHER
                               IF    ((IDX               =  1)
                                AND   (WRK-FEDT-1ST-FLG  =  ZERO))
                               OR    ((RECE641-SRYKBN    =  "70")
                                AND   (IDX               =  2)
                                AND   (WRK-FEDT-1ST-FLG  =  ZERO))
                                   MOVE    1    TO    WRK-FEDT-1ST-FLG
                                   PERFORM 200721-ZAI-E-HENSYU-SEC
                                   PERFORM 5001-FILE-WRITE-SEC
                               END-IF
                               PERFORM 200721-ZAI-F-HENSYU-SEC
                               PERFORM 5001-FILE-F-WRITE-SEC
                       END-EVALUATE
                   END-PERFORM
               END-IF
      *
      *    医薬品データの編集
               IF      RECE641-KAISU (1 IDXX)  >   ZERO
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-TBLMAX6
      *ito.TAITOYAKU   IF    IDX               =  1
                       IF   (WRK-FEDT-1ST-FLG  =  ZERO)
                       AND  (TBL-ZAIW-SRYCD (1 IDX)
                                           NOT =  "840000006")
                       AND  (TBL-ZAIW-SRYCD (1 IDX)
                                           NOT =  SYS-9102-SRYCD(1))
                       AND  (TBL-ZAIW-SRYCD (1 IDX)
                                           NOT =  SYS-9102-SRYCD(2))
                             MOVE    1    TO    WRK-FEDT-1ST-FLG
                             PERFORM 200722-YKZ-E-HENSYU-SEC
                             PERFORM 5001-FILE-WRITE-SEC
                       END-IF
                       PERFORM 200722-YKZ-F-HENSYU-SEC
                       PERFORM 5001-FILE-F-WRITE-SEC
                   END-PERFORM
               END-IF
      *
      *    特定器材データの編集
               IF      RECE641-KAISU (1 IDXX)  >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >       2
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >       WRK-TBLMAX7
                       IF      IDXY        =   TBL-ZAIW-DATSKB (2 IDX)
                         IF    IDX               =  1
                         AND   WRK-FEDT-1ST-FLG  =  ZERO
                               MOVE    1     TO    WRK-FEDT-1ST-FLG
                               PERFORM 200723-KIZAI-E-HENSYU-SEC
                               PERFORM 5001-FILE-WRITE-SEC
                         END-IF
                         PERFORM 200723-KIZAI-F-HENSYU-SEC
                         PERFORM 5001-FILE-F-WRITE-SEC
                       END-IF
                     END-PERFORM
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20072-ZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル＜診療明細情報＞：診療行為  編集処理
      *****************************************************************
       200721-ZAI-E-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-FEDT-PREV-TEN
           MOVE    ZERO                TO  WRK-FEDT-PREV-TEN50
           MOVE    ZERO                TO  WRK-FEDT-PREV-TEN721
           MOVE    "E"                 TO  WRK-FEDT-REC-FLG
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUM
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-TEN
           MOVE    ZERO                TO  TBL-HEIMA-AREA
      *退院年月日、入院年月日のセット
           PERFORM 200799-ZAI-HENSYU-0-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-KOUI-SRYCD(IDX) TO  WRK-FEDT-SRYCD
           MOVE    TBL-KOUI-NAME(IDX)  TO  WRK-FEDT-NAME
           MOVE    1                   TO  WRK-FEDT-KBNNUM-FLG
      *順序番号
      *****ADD     1                   TO  WRK-RENNUM
      *データ区分
           MOVE    RECE641-SRYKBN      TO  WRK-FEDT-SRYKBN
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *病院点数マスタコードから診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *行為点数
           MOVE    RECE641-ZAITEN      TO  WRK-FEDT-ZAITEN
      *行為薬剤料
           MOVE    RECE641-YKZTEN      TO  WRK-FEDT-YKZTEN
      *行為材料料
           MOVE    RECE641-KIZAITEN    TO  WRK-FEDT-KIZAITEN
           IF      RECE641-SRYKBN      =   "70"
             COMPUTE  WRK-FEDT-KIZAITEN    =   RECE641-KIZAITEN
                                           +   RECE641-SYUTEN2
           END-IF
      *円・点区分
           MOVE    "0"                 TO  WRK-FEDT-TENKBN
      *行為回数
           MOVE    RECE641-KAISU (1 IDXX)
                                       TO  WRK-FEDT-ZAIKAISU
      *****MOVE    RECE641-ZAIKAISU    TO  WRK-FEDT-ZAIKAISU
      *保険者番号
           MOVE    SPACE               TO  WRK-FEDT-HKNJANUM
           IF      RECE641-HKNJANUM (5:4)  =  SPACE
                   MOVE    RECE641-HKNJANUM (1:4)
                                       TO  WRK-FEDT-HKNJANUM (5:)
           ELSE
             IF    RECE641-HKNJANUM (7:2)  =  SPACE
                   MOVE    RECE641-HKNJANUM (1:6)
                                       TO  WRK-FEDT-HKNJANUM (3:)
             ELSE
                   MOVE    RECE641-HKNJANUM
                                       TO  WRK-FEDT-HKNJANUM
             END-IF
           END-IF
      *レセプト種別
           MOVE    RECE641-RECESYUBETU TO  WRK-FEDT-RECESYUBETU
      *実施年月日
           MOVE    RECE641-SRYYM       TO  WRK-FEDT-SRYYM
      *    WRK-FEDT-DD はすでにセット済み
      *レセプト科区分
           MOVE    SPACE               TO  WRK-FEDT-RECESRYKA
           PERFORM VARYING SYSKAN-IDX2 FROM  1  BY  1
                   UNTIL   SYSKAN-IDX2 >   TBL-SRYKA-MAX
             IF    RECE641-SRYKA       =   TBL-SRYKA (SYSKAN-IDX2)
               MOVE    TBL-RECESRYKA (SYSKAN-IDX2)
                                       TO  WRK-FEDT-RECESRYKA
               MOVE    TBL-SRYKA-MAX   TO  SYSKAN-IDX2
             END-IF
           END-PERFORM
      *診療科区分
           PERFORM 200721-SRYKAKBN-HENSYU-SEC
      *医師コード
           IF      LNK-DPCNYUIN-DRCD (1 IDXX)
                                   NOT =   SPACE
             MOVE  LNK-DPCNYUIN-DRCD (1 IDXX)
                                       TO  WRK-FEDT-DRCD
           ELSE
             IF    LNK-DPCNYUIN-DRCD (2 IDXX)
                                   NOT =   SPACE
               MOVE  LNK-DPCNYUIN-DRCD (2 IDXX)
                                       TO  WRK-FEDT-DRCD
             ELSE
               MOVE  SPACE             TO  WRK-FEDT-DRCD
             END-IF
           END-IF
      *****MOVE    RECE641-DRCD        TO  WRK-FEDT-DRCD
      *病棟コード
           IF      LNK-DPCNYUIN-BTUNUM (1 IDXX)
                                   NOT =   ZERO
               MOVE    LNK-DPCNYUIN-BTUNUM (1 IDXX)
                                       TO  WRK-FEDT-BYOTOCD
           ELSE
               MOVE    LNK-DPCNYUIN-BTUNUM (2 IDXX)
                                       TO  WRK-FEDT-BYOTOCD
           END-IF
      *病棟区分
           IF      LNK-DPCNYUIN-BTUKBN (1 IDXX)
                                   NOT =   SPACE
             IF    LNK-DPCNYUIN-BTUKBN (1 IDXX)
                                       =   "01"
               MOVE    "0"             TO  WRK-FEDT-BYOTOKBN
             ELSE
               MOVE    "1"             TO  WRK-FEDT-BYOTOKBN
             END-IF
           ELSE
             IF    LNK-DPCNYUIN-BTUKBN (2 IDXX)
                                       =   "01"
               MOVE    "0"             TO  WRK-FEDT-BYOTOKBN
             ELSE
               MOVE    "1"             TO  WRK-FEDT-BYOTOKBN
             END-IF
           END-IF
      *入外区分
           MOVE    RECE641-NYUGAIKBN   TO  WRK-FEDT-NYUGAIKBN
      *行為点数から施設タイプまで
           PERFORM 200799-ZAI-HENSYU-E-SEC
           .
       200721-ZAI-E-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科区分  編集処理
      *****************************************************************
       200721-SRYKAKBN-HENSYU-SEC      SECTION.
      *
           SET     TBL-DPCSRYKACD-IDX  TO  1
           SEARCH  TBL-DPCSRYKACD-OCC
                               VARYING TBL-DPCSRYKACD-IDX
               AT  END
                   MOVE  "000"         TO  WRK-FEDT-SRYKA
               WHEN  WRK-FEDT-RECESRYKA =  TBL-RECEDEN-SRYKACD
                                           (TBL-DPCSRYKACD-IDX)
                   MOVE  TBL-DPC-SRYKACD (TBL-DPCSRYKACD-IDX)
                                       TO  WRK-FEDT-SRYKA
           END-SEARCH
           .
       200721-SRYKAKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル＜診療明細情報＞：診療行為  編集処理
      *****************************************************************
       200721-ZAI-F-HENSYU-SEC       SECTION.
      *
           MOVE    "F"                 TO  WRK-FEDT-REC-FLG
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TBL-KOUI-SRYCD(IDX) TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-KOUI-SRYCD(IDX) TO  WRK-FEDT-SRYCD
           MOVE    TBL-KOUI-NAME(IDX)  TO  WRK-FEDT-NAME
           MOVE    1                   TO  WRK-FEDT-KBNNUM-FLG
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *行為明細番号から診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *使用量
           MOVE    TBL-KOUI-SRYSURYO (IDX)
                                       TO  WRK-FEDT-SRYSURYO
      *基準単位
           MOVE    TBL-KOUI-TANICD (IDX)
                                       TO  WRK-FEDT-TANICD
      *行為明細点数
           IF      TBL-KOUI-TENSIKIBETU (IDX)  =   3
             COMPUTE WRK-FEDT-MEISAI-TEN
                                       =   TBL-KOUI-TEN (IDX)
                                       +   0.5
           ELSE
             MOVE  ZERO                TO  WRK-FEDT-MEISAI-TEN
           END-IF
      *行為明細薬剤料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-YKZ
      *行為明細材料料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-KIZ
      *円・点区分
           MOVE    "0"                 TO  WRK-FEDT-TENKBN
      *出来高実績点数
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUB
           IF      WRK-FEDT-SRYKBN     =   "60"
           AND     TBL-KOUI-HOUKNSKBN (IDX)
                                   NOT =   ZERO
             IF    IDX                 =   WRK-TBLMAX1
      **           MOVE    WRK-FEDT-ZAITEN
      **                               TO  WRK-FEDT-DEKIDAKA-TEN
                   COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   WRK-FEDT-ZAITEN
                                       -   WRK-FEDT-YKZTEN
                                       -   WRK-FEDT-KIZAITEN
             ELSE
                   MOVE    ZERO        TO  WRK-FEDT-DEKIDAKA-TEN
             END-IF
           ELSE
             IF    WRK-FEDT-SRYKBN     =   "70"
             AND   WRK-FEDT-SRYSURYO   >   1
                   MOVE  WRK-FEDT-SRYSURYO    TO  WRK-KAISU-70T
                   IF    WRK-KAISU-70T >  5
                     MOVE  5           TO  WRK-KAISU-70T
                   END-IF
                   COMPUTE WRK-KAISU-70 =  (WRK-KAISU-70T - 1)
                                       *   0.5
                                       +   1
                   IF  (TBL-KOUI-TEN (IDX)               =  ZERO)
                   AND (TBL-KOUI-KZMCOMPSIKIBETU (IDX)  =   1)
                       PERFORM 200799-KZM-DEKIDAKA-SEC
                   ELSE
                       COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   TBL-KOUI-TEN (IDX)
                                       *   WRK-KAISU-70
                                       +   0.5
                   END-IF
                   COMPUTE WRK-FEDT-DEKIDAKA-SUM
                                       =   WRK-FEDT-DEKIDAKA-SUM
                                       +   WRK-FEDT-DEKIDAKA-TEN
             ELSE
              IF   TBL-KOUI-TENSIKIBETU (IDX)  =  5
                IF    WRK-FEDT-SRYKBN          =  "70"
                AND   WRK-FEDT-DEKIDAKA-SUM    >  ZERO
                AND  (TBL-KOUI-SRYCD(IDX)  NOT =  "170017170"
                                           AND    "170017270")
                   COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   RECE641-ZAITEN
                                       -   WRK-FEDT-DEKIDAKA-SUM
                ELSE
      *!           COMPUTE WRK-FEDT-DEKIDAKA-TEN
      *!                               =   RECE641-ZAITEN
      *!                               -   TBL-KOUI-TEN (IDX - 1)
                   IF  (TBL-KOUI-SRYCD(IDX) =   "170017170"
                                            OR  "170017270")
                   AND (WRK-FEDT-PREV-TEN721 >  ZERO)
                       MOVE WRK-FEDT-PREV-TEN721
                                       TO   WRK-FEDT-PREV-TEN
                   END-IF
                   COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   WRK-FEDT-PREV-TEN
                                       *   TBL-KOUI-TEN (IDX)
                                       /   100
                                       +   0.5
                   IF      WRK-FEDT-SRYKBN     NOT =  "50"
                       MOVE    ZERO        TO  WRK-FEDT-PREV-TEN
                   END-IF
                   MOVE    ZERO        TO  WRK-FEDT-PREV-TEN50
                   MOVE    ZERO        TO  WRK-FEDT-PREV-TEN721
                END-IF
              ELSE
               IF  TBL-KOUI-TENSIKIBETU (IDX)  =  6
                   MOVE    1           TO  WRK-FEDT-DEKIDAKA-SUB
      *!           COMPUTE WRK-FEDT-DEKIDAKA-TEN
      *!                               =   TBL-KOUI-TEN (IDX - 1)
      *!                               -   RECE641-ZAITEN
                   IF  TBL-KOUI-SRYCD (IDX) = "150268970"
                       COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   WRK-FEDT-PREV-TEN50
                                       *   TBL-KOUI-TEN (IDX)
                                       /   100
                       COMPUTE WRK-FEDT-PREV-TEN
                                       =   WRK-FEDT-PREV-TEN
                                       -   WRK-FEDT-DEKIDAKA-TEN
                   ELSE
                       COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   WRK-FEDT-PREV-TEN
                                       *   TBL-KOUI-TEN (IDX)
                                       /   100
                       IF  TNS-TEIGENKBN   NOT =   1
                           MOVE    ZERO        TO  WRK-FEDT-PREV-TEN
                           MOVE    ZERO        TO  WRK-FEDT-PREV-TEN50
                           MOVE    ZERO        TO  WRK-FEDT-PREV-TEN721
                       END-IF
                   END-IF
               ELSE
      *         きざみ値計算識別判定
                 IF  ((TBL-KOUI-DATAKBN (IDX)          =   1)    AND
                      (TBL-KOUI-CDKBN-KBN (IDX)        =   "L")  AND
                      (TBL-KOUI-CDKBN-KBNNUM (IDX)     =   8)    AND
                      (TBL-KOUI-CDKBN-KBNNUM-EDA (IDX) =   0))
                       IF  WRK-MEISAI-RENNUM   =   1
                           PERFORM 200721-HEIMA-SET-SEC
                           PERFORM 200721-HEIMA-COM1-SEC
                       ELSE
                           PERFORM 200721-HEIMA-COM2-SEC
                       END-IF
                 ELSE
                   IF    TBL-KOUI-KZMCOMPSIKIBETU (IDX)  =   1
                         PERFORM 200799-KZM-DEKIDAKA-SEC
                   ELSE
                     COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   TBL-KOUI-TEN (IDX)
                                       +   0.5
                   END-IF
                 END-IF
               END-IF
              END-IF
             END-IF
           END-IF
      *出来高・包括フラグ
           IF      WRK-FEDT-HOUKATU-FLG    NOT =   "6"
      * 2014.5 MOVE    "0"                 TO  WRK-FEDT-HOUKATU-FLG
               MOVE    TNS-DPCKBN          TO  WRK-FEDT-HOUKATU-FLG
           END-IF
      *使用量から出来高・包括フラグまで
           PERFORM 200799-ZAI-HENSYU-F-SEC
      *加算・減算のため行為点数を累積する
      *    手術で通則加算は通則加算対象外とする
           IF     (WRK-FEDT-SRYKBN             =   "50")  AND
                  (TBL-KOUI-TENSIKIBETU (IDX)  =   3)  AND
                  (TNS-KOKUJISKBKBN2           =   "9")  AND
                  (TNS-TUSOKUGAIKBN            =   "0")
               MOVE     1                      TO  TNS-TUSOKUGAIKBN
           END-IF
           IF     (TBL-KOUI-TENSIKIBETU (IDX)  =   3)
           OR     (TNS-TEIGENKBN               =   1)
             IF    TNS-TUSOKUGAIKBN      NOT =   "1"
               IF    TBL-KOUI-KZMCOMPSIKIBETU (IDX)  =   1
                   COMPUTE WRK-FEDT-PREV-TEN   =   WRK-FEDT-PREV-TEN
                                               +   WRK-FEDT-DEKIDAKA-TEN
                   IF    WRK-FEDT-SRYKBN       =  "70"
                   AND   TBL-KOUI-SRYSYUKBN(IDX)   =  "721"
                       COMPUTE WRK-FEDT-PREV-TEN721
                                               =   WRK-FEDT-PREV-TEN721
                                               +   WRK-FEDT-DEKIDAKA-TEN
                   END-IF
               ELSE
                   IF  TBL-KOUI-TENSIKIBETU (IDX)  =  6
                       COMPUTE WRK-FEDT-PREV-TEN
                                        =   WRK-FEDT-PREV-TEN
                                        *  (100  -  TBL-KOUI-TEN (IDX))
                                        /   100
                                        +   0.5
                   ELSE
                       COMPUTE WRK-FEDT-PREV-TEN =  WRK-FEDT-PREV-TEN
                                                 +  TBL-KOUI-TEN (IDX)
                       MOVE TBL-KOUI-TEN (IDX) TO  WRK-FEDT-PREV-TEN50
                   END-IF
               END-IF
             END-IF
           END-IF
           .
       200721-ZAI-F-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    閉麻テーブルセット処理
      *****************************************************************
       200721-HEIMA-SET-SEC            SECTION.
      *
           PERFORM VARYING IDXW FROM IDX  BY  1
                   UNTIL   TBL-KOUI-SRYCD (IDXW)  =  SPACE
             IF  ((TBL-KOUI-DATAKBN (IDXW)          =  1)    AND
                  (TBL-KOUI-CDKBN-KBN (IDXW)        =  "L")  AND
                  (TBL-KOUI-CDKBN-KBNNUM (IDXW)     =  8)    AND
                  (TBL-KOUI-CDKBN-KBNNUM-EDA (IDXW) =  0))
                   COMPUTE TBL-HEIMA-MAX  =  TBL-HEIMA-MAX + 1
                   MOVE  TBL-KOUI-SRYSURYO (IDXW)  TO
                                  TBL-HEIMA-SURYO (TBL-HEIMA-MAX)
                   MOVE  TBL-KOUI-SRYSURYO (IDXW)  TO
                                  TBL-HEIMA-ZAN-SURYO (TBL-HEIMA-MAX)
                   COMPUTE TBL-HEIMA-TOT-SURYO
                                       =  TBL-HEIMA-TOT-SURYO
                                       +  TBL-KOUI-SRYSURYO (IDXW)
             END-IF
           END-PERFORM
      *    基本となる数量計算
           IF      TBL-HEIMA-TOT-SURYO     >=  120
               IF  TBL-HEIMA-SURYO (1)     >=  120
                   COMPUTE WRK-HEIMA-SURYO  =  TBL-HEIMA-SURYO(1) - 120
                   COMPUTE WRK-HEIMA-SURYO2 =  WRK-HEIMA-SURYO / 30
                   COMPUTE TBL-HEIMA-ZAN-SURYO (1)
                                            =  WRK-HEIMA-SURYO
                                            - (WRK-HEIMA-SURYO2 * 30)
                   COMPUTE WRK-KZM-SURYO    =  TBL-HEIMA-SURYO (1)
                                            -  TBL-HEIMA-ZAN-SURYO (1)
               ELSE
                   MOVE    TBL-HEIMA-SURYO (1)  TO  WRK-KZM-SURYO
                   MOVE    ZERO            TO  TBL-HEIMA-ZAN-SURYO (1)
                   PERFORM VARYING IDXW FROM 2  BY  1
                           UNTIL   ( IDXW > TBL-HEIMA-MAX )
                                OR ( WRK-KZM-SURYO = 120 )
                     COMPUTE WRK-HEIMA-SURYO  =  120  -  WRK-KZM-SURYO
                     IF      TBL-HEIMA-ZAN-SURYO (IDXW)
                                              >=   WRK-HEIMA-SURYO
                         COMPUTE WRK-KZM-SURYO  =  WRK-KZM-SURYO
                                                +  WRK-HEIMA-SURYO
                         COMPUTE TBL-HEIMA-ZAN-SURYO (IDXW)
                                      =  TBL-HEIMA-ZAN-SURYO (IDXW)
                                      -  WRK-HEIMA-SURYO
                     ELSE
                         COMPUTE WRK-KZM-SURYO  =  WRK-KZM-SURYO
                                      +  TBL-HEIMA-ZAN-SURYO (IDXW)
                         MOVE    ZERO    TO  TBL-HEIMA-ZAN-SURYO (IDXW)
                     END-IF
                   END-PERFORM
               END-IF
           ELSE
               MOVE    TBL-HEIMA-TOT-SURYO TO  WRK-KZM-SURYO
               PERFORM VARYING IDXW FROM 1  BY  1
                       UNTIL   IDXW >    TBL-HEIMA-MAX
                 MOVE    ZERO       TO  TBL-HEIMA-ZAN-SURYO(IDXW)
               END-PERFORM
           END-IF
           MOVE    WRK-KZM-SURYO    TO  TBL-HEIMA-COM-SURYO (1)
           COMPUTE TBL-HEIMA-TOT-SURYO =   TBL-HEIMA-TOT-SURYO
                                       -   WRK-KZM-SURYO
      *    30分以上があるか？
           PERFORM VARYING IDXW FROM 1  BY  1
                   UNTIL   ( IDXW > TBL-HEIMA-MAX )
             IF      TBL-HEIMA-ZAN-SURYO (IDXW)  >=  30
               MOVE    TBL-HEIMA-ZAN-SURYO (IDXW)
                                           TO  WRK-HEIMA-SURYO
               COMPUTE WRK-HEIMA-SURYO2
                       =  TBL-HEIMA-ZAN-SURYO (IDXW) / 30
               COMPUTE WRK-KZM-SURYO        =  WRK-HEIMA-SURYO2 * 30
               COMPUTE TBL-HEIMA-ZAN-SURYO (IDXW)
                                            =  WRK-HEIMA-SURYO
                                            -  WRK-KZM-SURYO
               COMPUTE TBL-HEIMA-COM-SURYO (IDXW)
                                          =  TBL-HEIMA-COM-SURYO (IDXW)
                                          +   WRK-KZM-SURYO
               COMPUTE TBL-HEIMA-TOT-SURYO  =  TBL-HEIMA-TOT-SURYO
                                            -  WRK-KZM-SURYO
             END-IF
           END-PERFORM
      *    30分以下を寄せ集め
           PERFORM UNTIL   ( TBL-HEIMA-TOT-SURYO = ZERO )
             MOVE    ZERO              TO  WRK-HEIMA-IDX
             MOVE    ZERO              TO  WRK-KZM-SURYO
             PERFORM VARYING IDXW FROM 1  BY  1
                     UNTIL   ( IDXW > TBL-HEIMA-MAX )
               IF    TBL-HEIMA-ZAN-SURYO (IDXW) >  WRK-KZM-SURYO
                 MOVE    IDXW              TO  WRK-HEIMA-IDX
                 MOVE    TBL-HEIMA-ZAN-SURYO (IDXW)
                                           TO  WRK-KZM-SURYO
               END-IF
             END-PERFORM
             IF      WRK-HEIMA-IDX   NOT =   ZERO
                   MOVE    TBL-HEIMA-ZAN-SURYO (WRK-HEIMA-IDX)
                                          TO  WRK-KZM-SURYO
                   MOVE    ZERO  TO  TBL-HEIMA-ZAN-SURYO (WRK-HEIMA-IDX)
             END-IF
      **     DISPLAY " "
      **     DISPLAY "WRK-HEIMA-IDX = " WRK-HEIMA-IDX
             PERFORM VARYING IDXW FROM 1  BY  1
                     UNTIL   ( IDXW > TBL-HEIMA-MAX )
                        OR   ( WRK-HEIMA-IDX = ZERO )
               IF      IDXW          NOT =   WRK-HEIMA-IDX
                   MOVE    IDXW            TO  IDW
                   PERFORM VARYING IDW0 FROM TBL-HEIMA-MAX  BY  -1
                           UNTIL   ( IDW0 = 0 )
                     IF    ( IDW0      NOT =   WRK-HEIMA-IDX ) AND
                           ( IDW0      NOT =   IDXW )
                       IF      TBL-HEIMA-ZAN-SURYO (IDW0)
                                       =   TBL-HEIMA-ZAN-SURYO (IDXW)
                         MOVE  IDW0        TO  IDW
                         MOVE  1           TO  IDW0
                       END-IF
                     END-IF
                   END-PERFORM
      **     DISPLAY "IDW = " IDW
                   COMPUTE WRK-HEIMA-SURYO =  30  -  WRK-KZM-SURYO
                   IF      TBL-HEIMA-ZAN-SURYO (IDW) < WRK-HEIMA-SURYO
                       COMPUTE WRK-KZM-SURYO =  WRK-KZM-SURYO
                                         +  TBL-HEIMA-ZAN-SURYO (IDW)
                       MOVE    ZERO      TO TBL-HEIMA-ZAN-SURYO (IDW)
                   ELSE
                       COMPUTE TBL-HEIMA-ZAN-SURYO (IDW)
                                         =  TBL-HEIMA-ZAN-SURYO (IDW)
                                         -  WRK-HEIMA-SURYO
                       MOVE    30            TO WRK-KZM-SURYO
                       MOVE    TBL-HEIMA-MAX TO IDXW
                   END-IF
               END-IF
             END-PERFORM
             COMPUTE TBL-HEIMA-COM-SURYO (WRK-HEIMA-IDX)
                                 =  TBL-HEIMA-COM-SURYO (WRK-HEIMA-IDX)
                                 +  WRK-KZM-SURYO
             COMPUTE TBL-HEIMA-TOT-SURYO  =  TBL-HEIMA-TOT-SURYO
                                          -   WRK-KZM-SURYO
      **     DISPLAY "ZAN:COM(1) " TBL-HEIMA-ZAN-SURYO(1)
      **                           TBL-HEIMA-COM-SURYO(1)
      **     DISPLAY "ZAN:COM(2) " TBL-HEIMA-ZAN-SURYO(2)
      **                           TBL-HEIMA-COM-SURYO(2)
      **     DISPLAY "ZAN:COM(3) " TBL-HEIMA-ZAN-SURYO(3)
      **                           TBL-HEIMA-COM-SURYO(3)
           END-PERFORM
           MOVE    ZERO                TO  WRK-HEIMA-IDX
           .
       200721-HEIMA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     閉麻計算１
      *****************************************************************
       200721-HEIMA-COM1-SEC           SECTION.
      *
           ADD     1             TO  WRK-HEIMA-IDX
           MOVE    TNS-TEN       TO  TBL-HEIMA-TEN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMKGN    TO  TBL-HEIMA-KZMKGN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMJGN    TO  TBL-HEIMA-KZMJGN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMTEN    TO  TBL-HEIMA-KZMTEN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMERR    TO  TBL-HEIMA-KZMERR (WRK-HEIMA-IDX)
      *
           MOVE    TBL-HEIMA-COM-SURYO (1) TO  WRK-KZM-SURYO
           MOVE    ZERO                    TO  WRK-KZM-TEN2
           MOVE    ZERO                    TO  FLG-KZM
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-KZM-SURYO       <=  (TNS-KZMKGN - TNS-KZM )
                   IF      TNS-KZMERR      =   2  OR  3
                       MOVE  1             TO  FLG-KZM
                   END-IF
      *
      *            下限値＜データ≦上限値
               WHEN     (TNS-KZMKGN        <  WRK-KZM-SURYO ) AND
                        (TNS-KZMJGN        >= WRK-KZM-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KZM-TEN1    =  (WRK-KZM-SURYO
                                           -   TNS-KZMKGN)
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                   COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                           + ( WRK-KZM-TEN2
                                           *   TNS-KZMTEN)
                   COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    TNS-KZMJGN          <   WRK-KZM-SURYO
                   IF      TNS-KZMERR      =   0  OR  2
      *            警告エラーとする
                       MOVE    2           TO  FLG-KZM
      *            点数計算式１
                       COMPUTE WRK-KZM-TEN1    =  (WRK-KZM-SURYO
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                               + ( WRK-KZM-TEN2
                                               *   TNS-KZMTEN)
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KZM-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                               + ( WRK-KZM-TEN2
                                               *   TNS-KZMTEN)
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF    ((FLG-KZM             =   ZERO )  OR
                  (FLG-KZM             =   2   )) AND
                  (WRK-KZM-TEN2        >   ZERO )
               MOVE    WRK-KZM-TEN2    TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
               MOVE    TNS-TEN         TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
           .
       200721-HEIMA-COM1-EXT.
           EXIT.
      *
      *****************************************************************
      *     閉麻計算２
      *****************************************************************
       200721-HEIMA-COM2-SEC           SECTION.
      *
           ADD     1             TO  WRK-HEIMA-IDX
           MOVE    TNS-TEN       TO  TBL-HEIMA-TEN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMKGN    TO  TBL-HEIMA-KZMKGN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMJGN    TO  TBL-HEIMA-KZMJGN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMTEN    TO  TBL-HEIMA-KZMTEN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMERR    TO  TBL-HEIMA-KZMERR (WRK-HEIMA-IDX)
      *
           MOVE    TBL-HEIMA-COM-SURYO (WRK-HEIMA-IDX)
                                           TO  WRK-KZM-SURYO
           MOVE    ZERO                    TO  WRK-KZM-TEN2
      *    きざみ値判定
           EVALUATE    TRUE
               WHEN    WRK-KZM-SURYO       =   ZERO
                   CONTINUE
      *
      *            データ≦上限値
               WHEN    (TNS-KZMJGN        >= WRK-KZM-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KZM-TEN1    =   WRK-KZM-SURYO
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                   COMPUTE WRK-KZM-TEN     =   WRK-KZM-TEN2
                                           *   TNS-KZMTEN
                   COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    TNS-KZMJGN          <   WRK-KZM-SURYO
                   IF      TNS-KZMERR      =   0  OR  2
      *            点数計算式１
                       COMPUTE WRK-KZM-TEN1    =   WRK-KZM-SURYO
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   WRK-KZM-TEN2
                                               *   TNS-KZMTEN
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KZM-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   WRK-KZM-TEN2
                                               *   TNS-KZMTEN
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF      WRK-KZM-TEN2        >   ZERO
               MOVE    WRK-KZM-TEN2    TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
               MOVE    ZERO            TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
           .
       200721-HEIMA-COM2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル＜診療明細情報＞：医薬品  編集処理
      *****************************************************************
       200722-YKZ-E-HENSYU-SEC         SECTION.
      *
           MOVE    "E"                 TO  WRK-FEDT-REC-FLG
      *退院年月日、入院年月日のセット
           PERFORM 200799-ZAI-HENSYU-0-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-ZAIW-SRYCD (1 IDX)
                                       TO  WRK-FEDT-SRYCD
           MOVE    TBL-ZAIW-NAME (1 IDX)
                                       TO  WRK-FEDT-NAME
           MOVE    ZERO                TO  WRK-FEDT-KBNNUM-FLG
      *順序番号
      *****ADD     1                   TO  WRK-RENNUM
      *データ区分
           MOVE    RECE641-SRYKBN      TO  WRK-FEDT-SRYKBN
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *病院点数マスタコードから診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *行為点数
           IF      IDX                 =   WRK-TBLMAX6
             IF    TBL-ZAIW-SRYCD (1 IDX)
                                       =   WRK-CONS-TEIGEN
               MOVE  RECE641-ZAITEN    TO  WRK-FEDT-ZAITEN
             ELSE
               MOVE  RECE641-YKZTEN    TO  WRK-FEDT-ZAITEN
               ADD   RECE641-KIZAITEN  TO  WRK-FEDT-ZAITEN
             END-IF
           ELSE
             MOVE  RECE641-ZAITEN      TO  WRK-FEDT-ZAITEN
           END-IF
      *行為薬剤料
           MOVE    RECE641-YKZTEN      TO  WRK-FEDT-YKZTEN
      *行為材料料
           MOVE    RECE641-KIZAITEN    TO  WRK-FEDT-KIZAITEN
      *円・点区分
           MOVE    "0"                 TO  WRK-FEDT-TENKBN
      *行為回数
           MOVE    RECE641-KAISU (1 IDXX)
                                       TO  WRK-FEDT-ZAIKAISU
      *****MOVE    RECE641-ZAIKAISU    TO  WRK-FEDT-ZAIKAISU
      *保険者番号
           MOVE    SPACE               TO  WRK-FEDT-HKNJANUM
           IF      RECE641-HKNJANUM (5:4)  =  SPACE
                   MOVE    RECE641-HKNJANUM (1:4)
                                       TO  WRK-FEDT-HKNJANUM (5:)
           ELSE
             IF    RECE641-HKNJANUM (7:2)  =  SPACE
                   MOVE    RECE641-HKNJANUM (1:6)
                                       TO  WRK-FEDT-HKNJANUM (3:)
             ELSE
                   MOVE    RECE641-HKNJANUM
                                       TO  WRK-FEDT-HKNJANUM
             END-IF
           END-IF
      *レセプト種別
           MOVE    RECE641-RECESYUBETU TO  WRK-FEDT-RECESYUBETU
      *実施年月日
           MOVE    RECE641-SRYYM       TO  WRK-FEDT-SRYYM
      *    WRK-FEDT-DD はすでにセット済み
      *    退院時処方の出来高・包括フラグセット
      *      退院日を無条件にセットは廃止
      *      明細データ退避処理時にフラグをセットするようにした
      **** IF     (WRK-FEDT-SRYKBN     =   "21" OR "22" OR "23")
      ****    AND (WRK-FEDT-YMD        =   WRK-FEDT-TAIINYMD)
      ****      MOVE    "1"                TO  WRK-FEDT-HOUKATU-FLG
      **** ELSE
      ****      MOVE    "0"                TO  WRK-FEDT-HOUKATU-FLG
      **** END-IF
      *レセプト科区分
           MOVE    SPACE               TO  WRK-FEDT-RECESRYKA
           PERFORM VARYING SYSKAN-IDX2 FROM  1  BY  1
                   UNTIL   SYSKAN-IDX2 >   TBL-SRYKA-MAX
             IF    RECE641-SRYKA       =   TBL-SRYKA (SYSKAN-IDX2)
               MOVE    TBL-RECESRYKA (SYSKAN-IDX2)
                                       TO  WRK-FEDT-RECESRYKA
               MOVE    TBL-SRYKA-MAX   TO  SYSKAN-IDX2
             END-IF
           END-PERFORM
      *診療科区分
           PERFORM 200721-SRYKAKBN-HENSYU-SEC
      *医師コード
           IF      LNK-DPCNYUIN-DRCD (1 IDXX)
                                   NOT =   SPACE
             MOVE  LNK-DPCNYUIN-DRCD (1 IDXX)
                                       TO  WRK-FEDT-DRCD
           ELSE
             IF    LNK-DPCNYUIN-DRCD (2 IDXX)
                                   NOT =   SPACE
               MOVE  LNK-DPCNYUIN-DRCD (2 IDXX)
                                       TO  WRK-FEDT-DRCD
             ELSE
               MOVE  SPACE             TO  WRK-FEDT-DRCD
             END-IF
           END-IF
      *****MOVE    RECE641-DRCD        TO  WRK-FEDT-DRCD
      *病棟コード
           IF      LNK-DPCNYUIN-BTUNUM (1 IDXX)
                                   NOT =   ZERO
               MOVE    LNK-DPCNYUIN-BTUNUM (1 IDXX)
                                       TO  WRK-FEDT-BYOTOCD
           ELSE
               MOVE    LNK-DPCNYUIN-BTUNUM (2 IDXX)
                                       TO  WRK-FEDT-BYOTOCD
           END-IF
      *病棟区分
           IF      LNK-DPCNYUIN-BTUKBN (1 IDXX)
                                   NOT =   SPACE
             IF    LNK-DPCNYUIN-BTUKBN (1 IDXX)
                                       =   "01"
               MOVE    "0"             TO  WRK-FEDT-BYOTOKBN
             ELSE
               MOVE    "1"             TO  WRK-FEDT-BYOTOKBN
             END-IF
           ELSE
             IF    LNK-DPCNYUIN-BTUKBN (2 IDXX)
                                       =   "01"
               MOVE    "0"             TO  WRK-FEDT-BYOTOKBN
             ELSE
               MOVE    "1"             TO  WRK-FEDT-BYOTOKBN
             END-IF
           END-IF
      *入外区分
           MOVE    RECE641-NYUGAIKBN   TO  WRK-FEDT-NYUGAIKBN
      *行為点数から施設タイプまで
           PERFORM 200799-ZAI-HENSYU-E-SEC
           .
       200722-YKZ-E-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル＜行為明細情報＞：医薬品  編集処理
      *****************************************************************
       200722-YKZ-F-HENSYU-SEC         SECTION.
      *
           MOVE    "F"                 TO  WRK-FEDT-REC-FLG
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-ZAIW-SRYCD (1 IDX)
                                       TO  WRK-FEDT-SRYCD
           MOVE    TBL-ZAIW-NAME (1 IDX)
                                       TO  WRK-FEDT-NAME
           MOVE    ZERO                TO  WRK-FEDT-KBNNUM-FLG
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *行為明細番号から診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *使用量
           IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =   WRK-CONS-TEIGEN
                   MOVE    ZERO        TO  WRK-FEDT-SRYSURYO
           ELSE
                   MOVE    TBL-ZAIW-SRYSURYO (1 IDX)
                                       TO  WRK-FEDT-SRYSURYO
           END-IF
      *基準単位
           MOVE    TBL-ZAIW-TANICD (1 IDX)
                                       TO  WRK-FEDT-TANICD
      *行為明細点数
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-TEN
      *行為明細薬剤料
           COMPUTE WRK-FEDT-MEISAI-YKZ =   TBL-ZAIW-TEN (1 IDX)
                                       *   WRK-FEDT-SRYSURYO
      *行為明細材料料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-KIZ
      *円・点区分
           MOVE    "1"                 TO  WRK-FEDT-TENKBN
      *出来高実績点数
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUB
           IF      IDX                 =   WRK-TBLMAX6
                   MOVE    WRK-FEDT-YKZTEN
                                       TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
                   MOVE    ZERO        TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
      *出来高・包括フラグ
      *    退院日処方の出来高・包括フラグセット
      *      退院日を無条件にセットは廃止
      **** IF     (WRK-FEDT-SRYKBN     =   "21" OR "22" OR "23")
      ****    AND (WRK-FEDT-YMD        =   WRK-FEDT-TAIINYMD)
      ****      MOVE    "1"                TO  WRK-FEDT-HOUKATU-FLG
      **** ELSE
      ****      MOVE    "0"                TO  WRK-FEDT-HOUKATU-FLG
      **** END-IF
      *使用量から出来高・包括フラグまで
           PERFORM 200799-ZAI-HENSYU-F-SEC
           .
       200722-YKZ-F-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル＜診療明細情報＞：特定器材  編集処理
      *****************************************************************
       200723-KIZAI-E-HENSYU-SEC       SECTION.
      *
           MOVE    "E"                 TO  WRK-FEDT-REC-FLG
      *退院年月日、入院年月日のセット
           PERFORM 200799-ZAI-HENSYU-0-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-ZAIW-SRYCD (2 IDX)
                                       TO  WRK-FEDT-SRYCD
           MOVE    TBL-ZAIW-NAME (2 IDX)
                                       TO  WRK-FEDT-NAME
           MOVE    ZERO                TO  WRK-FEDT-KBNNUM-FLG
      *順序番号
      *****ADD     1                   TO  WRK-RENNUM
      *データ区分
           MOVE    RECE641-SRYKBN      TO  WRK-FEDT-SRYKBN
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *病院点数マスタコードから診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *行為点数
           MOVE    ZERO                TO  WRK-FEDT-ZAITEN
           IF    ( IDXY                =   1               )
           AND   ( RECE641-SRYKBN      =   "70"            )
               MOVE  RECE641-SYUTEN2   TO  WRK-FEDT-ZAITEN
           END-IF
           IF    ( IDXY                =   2               )
               MOVE  RECE641-KIZAITEN  TO  WRK-FEDT-ZAITEN
           END-IF
      *行為薬剤料
           MOVE    RECE641-YKZTEN      TO  WRK-FEDT-YKZTEN
      *行為材料料
           MOVE    WRK-FEDT-ZAITEN     TO  WRK-FEDT-KIZAITEN
      *****MOVE    RECE641-KIZAITEN    TO  WRK-FEDT-KIZAITEN
      *円・点区分
           MOVE    "0"                 TO  WRK-FEDT-TENKBN
      *行為回数
           MOVE    RECE641-KAISU (1 IDXX)
                                       TO  WRK-FEDT-ZAIKAISU
      *保険者番号
           MOVE    SPACE               TO  WRK-FEDT-HKNJANUM
           IF      RECE641-HKNJANUM (5:4)  =  SPACE
                   MOVE    RECE641-HKNJANUM (1:4)
                                       TO  WRK-FEDT-HKNJANUM (5:)
           ELSE
             IF    RECE641-HKNJANUM (7:2)  =  SPACE
                   MOVE    RECE641-HKNJANUM (1:6)
                                       TO  WRK-FEDT-HKNJANUM (3:)
             ELSE
                   MOVE    RECE641-HKNJANUM
                                       TO  WRK-FEDT-HKNJANUM
             END-IF
           END-IF
      *レセプト種別
           MOVE    RECE641-RECESYUBETU TO  WRK-FEDT-RECESYUBETU
      *実施年月日
           MOVE    RECE641-SRYYM       TO  WRK-FEDT-SRYYM
      *    WRK-FEDT-DD はすでにセット済み
      *レセプト科区分
           MOVE    SPACE               TO  WRK-FEDT-RECESRYKA
           PERFORM VARYING SYSKAN-IDX2 FROM  1  BY  1
                   UNTIL   SYSKAN-IDX2 >   TBL-SRYKA-MAX
             IF    RECE641-SRYKA       =   TBL-SRYKA (SYSKAN-IDX2)
               MOVE    TBL-RECESRYKA (SYSKAN-IDX2)
                                       TO  WRK-FEDT-RECESRYKA
               MOVE    TBL-SRYKA-MAX   TO  SYSKAN-IDX2
             END-IF
           END-PERFORM
      *診療科区分
           PERFORM 200721-SRYKAKBN-HENSYU-SEC
      *医師コード
           IF      LNK-DPCNYUIN-DRCD (1 IDXX)
                                   NOT =   SPACE
             MOVE  LNK-DPCNYUIN-DRCD (1 IDXX)
                                       TO  WRK-FEDT-DRCD
           ELSE
             IF    LNK-DPCNYUIN-DRCD (2 IDXX)
                                   NOT =   SPACE
               MOVE  LNK-DPCNYUIN-DRCD (2 IDXX)
                                       TO  WRK-FEDT-DRCD
             ELSE
               MOVE  SPACE             TO  WRK-FEDT-DRCD
             END-IF
           END-IF
      *****MOVE    RECE641-DRCD        TO  WRK-FEDT-DRCD
      *病棟コード
           IF      LNK-DPCNYUIN-BTUNUM (1 IDXX)
                                   NOT =   ZERO
               MOVE    LNK-DPCNYUIN-BTUNUM (1 IDXX)
                                       TO  WRK-FEDT-BYOTOCD
           ELSE
               MOVE    LNK-DPCNYUIN-BTUNUM (2 IDXX)
                                       TO  WRK-FEDT-BYOTOCD
           END-IF
      *病棟区分
           IF      LNK-DPCNYUIN-BTUKBN (1 IDXX)
                                   NOT =   SPACE
             IF    LNK-DPCNYUIN-BTUKBN (1 IDXX)
                                       =   "01"
               MOVE    "0"             TO  WRK-FEDT-BYOTOKBN
             ELSE
               MOVE    "1"             TO  WRK-FEDT-BYOTOKBN
             END-IF
           ELSE
             IF    LNK-DPCNYUIN-BTUKBN (2 IDXX)
                                       =   "01"
               MOVE    "0"             TO  WRK-FEDT-BYOTOKBN
             ELSE
               MOVE    "1"             TO  WRK-FEDT-BYOTOKBN
             END-IF
           END-IF
      *入外区分
           MOVE    RECE641-NYUGAIKBN   TO  WRK-FEDT-NYUGAIKBN
      *行為点数から施設タイプまで
           PERFORM 200799-ZAI-HENSYU-E-SEC
           .
       200723-KIZAI-E-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル＜行為明細情報＞：特定器材  編集処理
      *****************************************************************
       200723-KIZAI-F-HENSYU-SEC       SECTION.
      *
           MOVE    "F"                 TO  WRK-FEDT-REC-FLG
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-ZAIW-SRYCD (2 IDX)
                                       TO  WRK-FEDT-SRYCD
           MOVE    TBL-ZAIW-NAME (2 IDX)
                                       TO  WRK-FEDT-NAME
           IF      WRK-FEDT-SRYCD      =   WRK-CONS-SANSO-HOSEI
             MOVE  "酸素補正率１．３（１気圧）"
                                       TO  WRK-FEDT-NAME
           END-IF
           MOVE    ZERO                TO  WRK-FEDT-KBNNUM-FLG
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *行為明細番号から診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *使用量
           MOVE    TBL-ZAIW-SRYSURYO (2 IDX)
                                       TO  WRK-FEDT-SRYSURYO
      *基準単位
           MOVE    TBL-ZAIW-TANICD (2 IDX)
                                       TO  WRK-FEDT-TANICD
      *行為明細点数
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-TEN
      *行為明細薬剤料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-YKZ
      *行為明細材料料
      *****IF    ( TBL-ZAIW-TENSIKIBETU (2 IDX)    =   2               )
      *****OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   "777770000"     )
      *****OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   WRK-CONS-SANSO1
      *****                                        OR  WRK-CONS-SANSO2
      *****                                        OR  WRK-CONS-SANSO3
      *****                                        OR  WRK-CONS-SANSO4
      *****                                        OR  WRK-CONS-SANSO5
      *****                                        OR  WRK-CONS-SANSO6
      *****                                        OR  WRK-CONS-SANSO7
      *****                                        OR  WRK-CONS-SANSO8 )
             COMPUTE WRK-FEDT-MEISAI-KIZ   =   TBL-ZAIW-TEN (2 IDX)
                                           *   WRK-FEDT-SRYSURYO
      *****ELSE
      *****  MOVE  ZERO                TO  WRK-FEDT-MEISAI-KIZ
      *****END-IF
      *円・点区分
           MOVE    "1"                 TO  WRK-FEDT-TENKBN
      *出来高実績点数
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUB
           IF      IDX                 =   WRK-TBLMAX7
                   MOVE    WRK-FEDT-KIZAITEN
                                       TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
                   MOVE    ZERO        TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
      *出来高・包括フラグ
           IF      WRK-FEDT-HOUKATU-FLG    NOT =  "6"
               MOVE    "0"                 TO  WRK-FEDT-HOUKATU-FLG
           END-IF
      *使用量から出来高・包括フラグまで
           PERFORM 200799-ZAI-HENSYU-F-SEC
           .
       200723-KIZAI-F-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院基本料・特定入院料・食事療養編集処理
      *****************************************************************
       20077-NYUIN-HENSYU-SEC           SECTION.
      *
      *    入院料
           PERFORM VARYING IDZ1    FROM    1   BY  1
                   UNTIL   IDZ1    >       5
               PERFORM VARYING IDZ2    FROM    1   BY  1
                       UNTIL   IDZ2    >       31
      *****            OR      LNK-DPCNYUIN-HKNCOMBI (IDZ1 IDZ2)
      *****                                    =   ZERO
      *
                   MOVE    ZERO                    TO  FLG-CHK
      *
                   PERFORM VARYING IDXZ    FROM    1   BY  1
                           UNTIL   IDXZ    >       10
                           OR      RECE62-HKNCOMBI (IDXZ)  =   ZERO
                       IF      LNK-DPCNYUIN-HKNCOMBI (IDZ1 IDZ2)
                                           =   RECE62-HKNCOMBI (IDXZ)
                           MOVE    1                   TO  FLG-CHK
                           MOVE    10                  TO  IDXZ
                       END-IF
                   END-PERFORM
      *
                   PERFORM VARYING IDXZ    FROM    1   BY  1
                           UNTIL   IDXZ    >
                                   LNK-DPCNYUIN-MEISAISU (IDZ1 IDZ2)
                           OR      FLG-CHK =       ZERO
      *
                       MOVE    SPACE               TO  WRK-REC
                       MOVE    ZERO                TO  WRK-REC-LENGTH
      *
                       EVALUATE    LNK-DPCNYUIN-RECSKB (IDZ1 IDZ2 IDXZ)
                           WHEN    "CO"
                               CONTINUE
                           WHEN    OTHER
                               IF    (  LNK-DPCNYUIN-SRYCD
                                                       (IDZ1 IDZ2 IDXZ)
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
                                           OR  WRK-CONS-HYOJYUN-FUTAN15)
                                   CONTINUE
                               ELSE
                                    IF    IDXZ       =  1
                                    OR    LNK-DPCNYUIN-SRYSKB
                                                       (IDZ1 IDZ2 IDXZ)
                                                     = "97"
      *                              外泊の場合(2行目のレコード)
                                       IF  LNK-DPCNYUIN-SRYCD
                                                       (IDZ1 IDZ2 2)
                                                     = "190107290" OR
                                                       "190107390" OR
                                                       "190107490" OR
                                                       "190107590"
                                           MOVE    2   TO  IDXZ
                                       END-IF
                                       MOVE  IDZ2  TO  WRK-FEDT-DD
                                       PERFORM
                                         200771-NYUIN-ZAI-E-HENSYU-SEC
                                       IF  WRK-REC-LENGTH  >  ZERO
                                         PERFORM 5001-FILE-WRITE-SEC
                                       END-IF
                                    END-IF
                                    PERFORM
                                         200771-NYUIN-ZAI-F-HENSYU-SEC
                                    IF   WRK-REC-LENGTH  >  ZERO
                                         PERFORM 5001-FILE-F-WRITE-SEC
                                    END-IF
                                  END-IF
                       END-EVALUATE
                   END-PERFORM
               END-PERFORM
      *
      *        差し込みコメント編集
               IF      IDZ1            <   3
                  PERFORM 200771-NYUIN-COMMENT-HENSYU-SEC
               END-IF
           END-PERFORM
           .
       20077-NYUIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *        差し込みコメント編集処理
      *****************************************************************
       200771-NYUIN-COMMENT-HENSYU-SEC      SECTION.
      *
           PERFORM
               UNTIL  ( FLG-END              =   1                   )
               OR     ( FLG-RECE64           =   1                   )
               OR     ( RECE62-HOSPNUM   NOT =   RECE641-HOSPNUM      )
               OR     ( RECE62-SRYYM     NOT =   RECE641-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE641-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE641-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE641-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE641-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE641-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE641-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE641-HOJOKBN      )
               OR     ( RECE62-KOHID-KEY NOT =   RECE641-KOHID        )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE641-TEKSTYMD     )
               OR     ( RECE62-RECEIPT-KBN
                                         NOT =   RECE641-RECEIPT-KBN  )
                   EVALUATE    IDZ1
                       WHEN    1
                           IF      RECE641-SRYKBN       >  "90"
                               GO  TO  200771-NYUIN-COMMENT-HENSYU-EXT
                           END-IF
                       WHEN    2
                           IF      RECE641-SRYKBN       >  "92"
                               GO  TO  200771-NYUIN-COMMENT-HENSYU-EXT
                           END-IF
                   END-EVALUATE
      *
      *            明細テーブル退避処理
                   PERFORM 20071-ZAI-TAIHI-SEC
      *            摘要データ編集処理
                   IF      FLG-END           =   ZERO
                       PERFORM 20072-ZAI-HENSYU-SEC
      *
                       PERFORM 990-RECE641-NEXT-SEC
                   END-IF
           END-PERFORM
           .
       200771-NYUIN-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル＜行為明細情報＞：入院料  編集処理
      *****************************************************************
       200771-NYUIN-ZAI-E-HENSYU-SEC   SECTION.
      *
           MOVE    ZERO                TO  WRK-FEDT-PREV-TEN
           IF      LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
                                   NOT =   WRK-FEDT-SRYKBN-SAVE
      *            順序番号の取り込み
                   INITIALIZE                         RECE900-REC
                   MOVE    LNK-DPCNYUIN-HOSPNUM   TO  RECE900-HOSPNUM
                   MOVE    LNK-DPCNYUIN-SANTEIYM  TO  RECE900-SRYYM
                   MOVE    LNK-DPCNYUIN-PTID      TO  RECE900-PTID
                   MOVE    LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
                                                  TO  RECE900-SRYKBN
                   DISPLAY   "LNK-DPCNYUIN-HOSPNUM = "
                              LNK-DPCNYUIN-HOSPNUM
                   DISPLAY   "LNK-DPCNYUIN-SANTEIYM = "
                              LNK-DPCNYUIN-SANTEIYM
                   DISPLAY   "LNK-DPCNYUIN-PTID = "
                              LNK-DPCNYUIN-PTID
                   DISPLAY   "LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ) = "
                              LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
                   DISPLAY   "900-RECE900-READ-SEC"
                   PERFORM   900-RECE900-READ-SEC
                   DISPLAY "FLG-RECE900 = " FLG-RECE900
                   IF      FLG-RECE900         =   1
                       MOVE    ZERO             TO  WRK-RENNUM
                   ELSE
                       MOVE    RECE900-RENNUM   TO  WRK-RENNUM                       
                   END-IF
                   DISPLAY "WRK-RENNUM = " WRK-RENNUM
                   MOVE    LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
                                       TO  WRK-FEDT-SRYKBN-SAVE
           END-IF
      *
           MOVE    "E"                 TO  WRK-FEDT-REC-FLG
      *退院年月日、入院年月日のセット
           PERFORM 200799-ZAI-HENSYU-0-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    LNK-DPCNYUIN-SRYCD (IDZ1 IDZ2 IDXZ)
                                       TO  WRK-FEDT-SRYCD
           INITIALIZE                      TNS-TENSU-REC
           MOVE    LNK-DPCNYUIN-SRYCD (IDZ1 IDZ2 IDXZ)
                                       TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           MOVE    TNS-NAME            TO  WRK-FEDT-NAME
           MOVE    2                   TO  WRK-FEDT-KBNNUM-FLG
           IF      LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
                                       =   "97"
               MOVE    SPACE           TO  WRK-KBNNUM-EDIT
               MOVE    3               TO  WRK-FEDT-KBNNUM-FLG
           END-IF
      *順序番号
           ADD     1                   TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-MEISAI-RENNUM
      *順序番号ファイルの更新
           INITIALIZE                         RECE900-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM   TO  RECE900-HOSPNUM
           MOVE    LNK-DPCNYUIN-SANTEIYM  TO  RECE900-SRYYM
           MOVE    LNK-DPCNYUIN-PTID      TO  RECE900-PTID
           MOVE    LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
                                          TO  RECE900-SRYKBN
           PERFORM   900-RECE900-READ-SEC
           IF      FLG-RECE900         =   1
               MOVE    LNK-DPCNYUIN-HOSPNUM   TO  RECE900-HOSPNUM
               MOVE    LNK-DPCNYUIN-SANTEIYM  TO  RECE900-SRYYM
               MOVE    LNK-DPCNYUIN-PTID      TO  RECE900-PTID
               MOVE    LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
                                          TO  RECE900-SRYKBN
               MOVE    WRK-RENNUM  TO  RECE900-RENNUM
               WRITE      RECE900-REC
           ELSE
               MOVE    WRK-RENNUM  TO  RECE900-RENNUM
               REWRITE    RECE900-REC
           END-IF
      *データ区分（診療識別）
           MOVE    LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
                                       TO  WRK-FEDT-SRYKBN
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *病院点数マスタコードから診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *行為点数
           IF      WRK-FEDT-SRYKBN     =   "90"  OR  "92"
             MOVE  LNK-DPCNYUIN-MEISAISU (IDZ1 IDZ2)
                                       TO  IDW
             MOVE  LNK-DPCNYUIN-TEN (IDZ1 IDZ2 IDW)
                                       TO  WRK-FEDT-ZAITEN
           ELSE
             MOVE  LNK-DPCNYUIN-TEN (IDZ1 IDZ2 IDXZ)
                                       TO  WRK-FEDT-ZAITEN
           END-IF
      *行為薬剤料
           MOVE    ZERO                TO  WRK-FEDT-YKZTEN
      *行為材料料
           MOVE    ZERO                TO  WRK-FEDT-KIZAITEN
      *円・点区分
           IF      WRK-FEDT-SRYKBN     =   "97"
             MOVE  "1"                 TO  WRK-FEDT-TENKBN
           ELSE
             MOVE  "0"                 TO  WRK-FEDT-TENKBN
           END-IF
      *行為回数
           MOVE    1                   TO  WRK-FEDT-ZAIKAISU
      *****IF      WRK-FEDT-SRYKBN     =   "90"
      *****  MOVE  LNK-DPCNYUIN-MEISAISU (IDZ1 IDZ2)
      *****                            TO  IDW
      *****  MOVE  LNK-DPCNYUIN-KAISU (IDZ1 IDZ2 IDW)
      *****                            TO  WRK-FEDT-ZAIKAISU
      *****ELSE
      *****  MOVE  1                   TO  WRK-FEDT-ZAIKAISU
      *****END-IF
      *保険者番号
           MOVE    SPACE               TO  WRK-FEDT-HKNJANUM
           IF      RECE62-HKNJANUM (5:4)  =  SPACE
                   MOVE    RECE62-HKNJANUM (1:4)
                                       TO  WRK-FEDT-HKNJANUM (5:)
           ELSE
             IF    RECE62-HKNJANUM (7:2)  =  SPACE
                   MOVE    RECE62-HKNJANUM (1:6)
                                       TO  WRK-FEDT-HKNJANUM (3:)
             ELSE
                   MOVE    RECE62-HKNJANUM
                                       TO  WRK-FEDT-HKNJANUM
             END-IF
           END-IF
      *レセプト種別
           MOVE    RECE62-RECESYUBETU TO  WRK-FEDT-RECESYUBETU
      *実施年月日
           MOVE    RECE62-SRYYM       TO  WRK-FEDT-SRYYM
      *    WRK-FEDT-DD はすでにセット済み
      *レセプト科区分
           MOVE    SPACE               TO  WRK-FEDT-RECESRYKA
           PERFORM VARYING SYSKAN-IDX2 FROM  1  BY  1
                   UNTIL   SYSKAN-IDX2 >   TBL-SRYKA-MAX
             IF    LNK-DPCNYUIN-SRYKA (IDZ1 IDZ2)
                                       =   TBL-SRYKA (SYSKAN-IDX2)
               MOVE    TBL-RECESRYKA (SYSKAN-IDX2)
                                       TO  WRK-FEDT-RECESRYKA
               MOVE    TBL-SRYKA-MAX   TO  SYSKAN-IDX2
             END-IF
           END-PERFORM
      *診療科区分
           PERFORM 200721-SRYKAKBN-HENSYU-SEC
      *医師コード
           MOVE    LNK-DPCNYUIN-DRCD (IDZ1 IDZ2)
                                       TO  WRK-FEDT-DRCD
      *病棟コード
           MOVE    LNK-DPCNYUIN-BTUNUM (IDZ1 IDZ2)
                                       TO  WRK-FEDT-BYOTOCD
      *病棟区分
           IF      LNK-DPCNYUIN-BTUKBN (IDZ1 IDZ2)
                                       =   "01"
               MOVE    "0"             TO  WRK-FEDT-BYOTOKBN
           ELSE
               MOVE    "1"             TO  WRK-FEDT-BYOTOKBN
           END-IF
      *入外区分
           MOVE    RECE62-NYUGAIKBN   TO  WRK-FEDT-NYUGAIKBN
      *行為点数から施設タイプまで
           PERFORM 200799-ZAI-HENSYU-E-SEC
           .
       200771-NYUIN-ZAI-E-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル＜行為明細情報＞：入院料  編集処理
      *****************************************************************
       200771-NYUIN-ZAI-F-HENSYU-SEC   SECTION.
      *
           MOVE    "F"                 TO  WRK-FEDT-REC-FLG
      *診療コード、名称、解釈番号編集のセット
           MOVE    LNK-DPCNYUIN-SRYCD (IDZ1 IDZ2 IDXZ)
                                       TO  WRK-FEDT-SRYCD
           INITIALIZE                      TNS-TENSU-REC
           MOVE    LNK-DPCNYUIN-SRYCD (IDZ1 IDZ2 IDXZ)
                                       TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           MOVE    TNS-NAME            TO  WRK-FEDT-NAME
           MOVE    2                   TO  WRK-FEDT-KBNNUM-FLG
      **** IF      LNK-DPCNYUIN-SRYSKB (IDZ1 IDZ2 IDXZ)
      ****                             =   "97"
      ****     MOVE    SPACE           TO  WRK-KBNNUM-EDIT
               MOVE    3               TO  WRK-FEDT-KBNNUM-FLG
      **** END-IF
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *行為明細番号から診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *使用量
           IF      TNS-TANICD          =   "000"
             MOVE  ZERO                TO  WRK-FEDT-SRYSURYO
           ELSE
             MOVE  LNK-DPCNYUIN-SURYO (IDZ1 IDZ2 IDXZ)
                                       TO  WRK-FEDT-SRYSURYO
           END-IF
      *基準単位
           MOVE    TNS-TANICD          TO  WRK-FEDT-TANICD
      *****MOVE    ZERO                TO  WRK-FEDT-TANICD
      *行為明細点数
           IF      TNS-TENSIKIBETU     =   3
             COMPUTE WRK-FEDT-MEISAI-TEN
                                       =   TNS-TEN
                                       +   0.5
           ELSE
            IF     TNS-TENSIKIBETU     =   6
              MOVE WRK-FEDT-ZAITEN     TO  WRK-FEDT-MEISAI-TEN
            ELSE
             IF    TNS-SRYKBN          =   "97"
               IF  WRK-FEDT-SRYSURYO   =   ZERO
                   COMPUTE WRK-FEDT-MEISAI-TEN
                                       =   TNS-TEN
                                       +   0.5
               ELSE
                   COMPUTE WRK-FEDT-MEISAI-TEN
                                       =   TNS-TEN
                                       *   WRK-FEDT-SRYSURYO
                                       +   0.5
               END-IF
             ELSE
               MOVE  ZERO              TO  WRK-FEDT-MEISAI-TEN
             END-IF
            END-IF
           END-IF
      *****MOVE    LNK-DPCNYUIN-TEN (IDZ1 IDZ2 IDXZ)
      *****                            TO  WRK-FEDT-MEISAI-TEN
      *行為明細薬剤料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-YKZ
      *行為明細材料料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-KIZ
      *円・点区分
           IF      TNS-SRYKBN          =   "97"
             MOVE  "1"                 TO  WRK-FEDT-TENKBN
           ELSE
             MOVE  "0"                 TO  WRK-FEDT-TENKBN
           END-IF
      *出来高実績点数
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUB
           MOVE    WRK-FEDT-MEISAI-TEN TO  WRK-FEDT-DEKIDAKA-TEN
           IF      TNS-TENSIKIBETU     =  6
      *                              外泊の場合は対象外
               IF  TNS-SRYCD           = "190107290" OR
                                         "190107390" OR
                                         "190107490" OR
                                         "190107590"
                   CONTINUE
               ELSE
                   MOVE    1           TO  WRK-FEDT-DEKIDAKA-SUB
                   COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =  (WRK-FEDT-PREV-TEN
                                       *  (100 -  TNS-TEN)
                                       /   100 )
                                       +   0.5
                   COMPUTE  WRK-FEDT-DEKIDAKA-TEN
                                       = WRK-FEDT-PREV-TEN -
                                         WRK-FEDT-DEKIDAKA-TEN
                   MOVE    ZERO        TO  WRK-FEDT-PREV-TEN
               END-IF
           END-IF
      *    栄養管理体制減算規定該当（入院基本料）
      *    栄養管理体制減算規定該当（特定入院料及び短期滞在手術等基本料２）
      *    は出来高実績点数をマイナス記録
           IF   TNS-SRYCD              =   "190182590"   OR
                                           "190182690"
               MOVE    -40             TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
      *出来高・包括フラグ
      * 2014.5 MOVE    "0"                 TO  WRK-FEDT-HOUKATU-FLG
           MOVE    TNS-DPCKBN          TO  WRK-FEDT-HOUKATU-FLG
      **   SET     TBL-DPCDEKIDAKA-IDX TO  1
      **   SEARCH  TBL-DPCDEKIDAKA-OCC
      **                       VARYING TBL-DPCDEKIDAKA-IDX
      **       AT  END
      **           MOVE  "0"           TO  WRK-FEDT-HOUKATU-FLG
      **       WHEN  WRK-FEDT-SRYCD    =   TBL-DPCDEKIDAKA-SRYCD
      **                                   (TBL-DPCDEKIDAKA-IDX)
      **           MOVE  "1"           TO  WRK-FEDT-HOUKATU-FLG
      **   END-SEARCH
      *使用量から出来高・包括フラグまで
           PERFORM 200799-ZAI-HENSYU-F-SEC
      *入院料減算のため行為点数を累積する
           IF      TNS-TENSIKIBETU     =   3
                   COMPUTE WRK-FEDT-PREV-TEN   =   WRK-FEDT-PREV-TEN
                                               +   TNS-TEN
           END-IF
           .
       200771-NYUIN-ZAI-F-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル編集処理（退院日、入院日セット）
      *****************************************************************
       200799-ZAI-HENSYU-0-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-DAYSET
           MOVE    RECE62-SRYYM        TO  WRK-EDYM
           MOVE    WRK-FEDT-DD         TO  WRK-EDDD
           PERFORM VARYING IDW         FROM    1  BY  1
                   UNTIL  (IDW         >   10)
                      OR  (WRK-NYUIN-RIREKI-NYUINYMD (IDW)
                                       =   SPACE
                      AND  WRK-NYUIN-RIREKI-TAIINYMD (IDW)
                                       =   SPACE)
                      OR  (FLG-DAYSET  =   1)
             IF    WRK-NYUIN-RIREKI-NYUINYMD (IDW)
                                       <=  WRK-EDYMD
              AND  WRK-NYUIN-RIREKI-TAIINYMD (IDW)
                                       >=  WRK-EDYMD
      *退院年月日
               IF (WRK-NYUIN-RIREKI-TAIINYMD (IDW)
                                       =   "99999999")  OR
                  (WRK-NYUIN-RIREKI-TAIINYMD (IDW) (1:6)
                                       >   RECE62-SRYYM)
                   MOVE  "00000000"    TO  WRK-FEDT-TAIINYMD
               ELSE
                   MOVE  WRK-NYUIN-RIREKI-TAIINYMD (IDW)
                                       TO  WRK-FEDT-TAIINYMD
               END-IF
      *入院年月日
                   MOVE  WRK-NYUIN-RIREKI-NYUINYMD (IDW)
                                       TO  WRK-FEDT-NYUINYMD
               MOVE     1      TO     FLG-DAYSET
             END-IF
           END-PERFORM
           .
       200799-ZAI-HENSYU-0-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル編集処理（施設コードから順序番号）
      *****************************************************************
       200799-ZAI-HENSYU-1-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
      *施設コード
           STRING  SYS-1001-PREFNUM    DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD     DELIMITED  BY  SIZE
                   WRK-KUGIRI-TAB      DELIMITED  BY  SIZE
                   INTO                    WRK-REC
           END-STRING
           MOVE    10                  TO  WRK-REC-LENGTH
      *データ識別番号
           IF      SYS-9102-NUMBER     =   "0"
               MOVE    ALL"0"              TO  WRK-DATA(1:WRK-ZEROKETA)
               MOVE    RECE62-PTNUM        TO  WRK-DATA
                                                   (WRK-PTNUMKETA:10)
           ELSE
               MOVE    RECE62-PTID         TO  WRK-DATA
           END-IF
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *退院年月日
           MOVE    WRK-FEDT-TAIINYMD   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *入院年月日
           MOVE    WRK-FEDT-NYUINYMD   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *データ区分（診療識別）
           MOVE    WRK-FEDT-SRYKBN     TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *順序番号
           MOVE    WRK-RENNUM          TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           .
       200799-ZAI-HENSYU-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル編集処理（行為明細番号から診療行為名称）
      *****************************************************************
       200799-ZAI-HENSYU-2-SEC     SECTION.
      *
      *行為明細番号（Ｆレコードのみ）
           IF      WRK-FEDT-REC-FLG    =   "F"
             ADD   1                   TO  WRK-MEISAI-RENNUM
             MOVE  WRK-MEISAI-RENNUM   TO  WRK-DATA
             MOVE  3                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           END-IF
      *病院点数マスタコード
           MOVE    WRK-FEDT-SRYCD      TO  WRK-DATA
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *レセプト電算処理システム用コード
           MOVE    WRK-FEDT-SRYCD      TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *解釈番号（基本）
           IF      WRK-FEDT-KBNNUM-FLG NOT =  3
             MOVE  SPACE               TO  WRK-KBNNUM-EDIT
           END-IF
           IF      WRK-FEDT-KBNNUM-FLG =   1
             MOVE  TBL-KOUI-CDKBN-KBN (IDX)
                                       TO  WRK-KBNNUM-KBN
             MOVE  TBL-KOUI-CDKBN-KBNNUM (IDX)
                                       TO  WRK-KBNNUM-KBNNUM
             IF    TBL-KOUI-CDKBN-KBNNUM-EDA (IDX)
                                   NOT =   ZERO
               MOVE    "-"             TO  WRK-KBNNUM-HAIFUN
               MOVE    TBL-KOUI-CDKBN-KBNNUM-EDA (IDX)
                                       TO  WRK-KBNNUM-KBNNUM-EDA
             END-IF
           END-IF
           IF      WRK-FEDT-KBNNUM-FLG =   2
             MOVE  TNS-CDKBN-KBN       TO  WRK-KBNNUM-KBN
             MOVE  TNS-CDKBN-KBNNUM    TO  WRK-KBNNUM-KBNNUM
             IF    TNS-CDKBN-KBNNUM-EDA
                                   NOT =   ZERO
               MOVE    "-"             TO  WRK-KBNNUM-HAIFUN
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  WRK-KBNNUM-KBNNUM-EDA
             END-IF
           END-IF
           MOVE    WRK-KBNNUM-EDIT     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *診療行為名称
           MOVE    WRK-FEDT-NAME       TO  WRK-DATA
           MOVE    254                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           .
       200799-ZAI-HENSYU-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル編集処理（行為点数から施設タイプ）
      *    包括分は行為点数、行為薬剤料、行為材料料にゼロ点をセットする
      *****************************************************************
       200799-ZAI-HENSYU-E-SEC     SECTION.
      *
      *行為点数
           IF      (RECE641-JIHOKBN    =  "1")
               MOVE    ZERO            TO  WRK-TENSU
           ELSE
               MOVE    WRK-FEDT-ZAITEN TO  WRK-TENSU
           END-IF
           MOVE    WRK-TENSU           TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *行為薬剤料
           IF      (RECE641-JIHOKBN    =  "1")
               MOVE    ZERO            TO  WRK-TENSU
           ELSE
               MOVE    WRK-FEDT-YKZTEN TO  WRK-TENSU
           END-IF
           MOVE    WRK-TENSU           TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *行為材料料
           IF      (RECE641-JIHOKBN    =  "1")
               MOVE    ZERO            TO  WRK-TENSU
           ELSE
               MOVE    WRK-FEDT-KIZAITEN
                                       TO  WRK-TENSU
           END-IF
           MOVE    WRK-TENSU           TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *円・点区分
           MOVE    WRK-FEDT-TENKBN     TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *行為回数
           MOVE    WRK-FEDT-ZAIKAISU   TO  WRK-KAISU
           MOVE    WRK-KAISU           TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *保険者番号
           MOVE    WRK-FEDT-HKNJANUM   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *レセプト種別
           MOVE    WRK-FEDT-RECESYUBETU
                                       TO  WRK-NUM
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *実施年月日
           MOVE    WRK-FEDT-YMD        TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *レセプト科区分
           MOVE    WRK-FEDT-RECESRYKA  TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *診療科区分
           MOVE    WRK-FEDT-SRYKA      TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *医師コード
           MOVE    WRK-FEDT-DRCD       TO  WRK-DATA
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *病棟コード
           MOVE    WRK-FEDT-BYOTOCD    TO  WRK-DATA
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *病棟区分
           MOVE    WRK-FEDT-BYOTOKBN   TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *入外区分
           IF      WRK-FEDT-NYUGAIKBN  =   "2"
             MOVE  "1"                 TO  WRK-DATA
           ELSE
             MOVE  "0"                 TO  WRK-DATA
           END-IF
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *施設タイプ
           MOVE    SPACE               TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
           .
       200799-ZAI-HENSYU-E-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル編集処理（使用量から出来高・包括フラグ）
      *****************************************************************
       200799-ZAI-HENSYU-F-SEC     SECTION.
      *
      *使用量
           IF      WRK-FEDT-TANICD     =   "000"
             MOVE  ZERO                TO  WRK-NUM
           ELSE
             MOVE  WRK-FEDT-SRYSURYO   TO  WRK-SRYSURYO
             MOVE  WRK-SRYSURYO        TO  WRK-NUM
           END-IF
           MOVE    11                  TO  WRK-NAGASA
           MOVE    3                   TO  WRK-SYOSUU
           PERFORM 5005-NUM-HENSYU-SEC
      *基準単位
           MOVE    WRK-FEDT-TANICD     TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *行為明細点数
           MOVE    WRK-FEDT-MEISAI-TEN TO  WRK-TENSU
           MOVE    WRK-TENSU           TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *行為明細薬剤料
           MOVE    WRK-FEDT-MEISAI-YKZ TO  WRK-NUM
           MOVE    WRK-NUM-X (3:12)    TO  WRK-DATA
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *行為明細材料料
           MOVE    WRK-FEDT-MEISAI-KIZ TO  WRK-NUM
           MOVE    WRK-NUM-X (3:12)    TO  WRK-DATA
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *円・点区分
           MOVE    WRK-FEDT-TENKBN     TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *出来高実績点数
           IF      WRK-FEDT-HOUKATU-FLG   =   "6"
                MOVE    ZERO           TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
           MOVE    WRK-FEDT-DEKIDAKA-TEN
                                       TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           IF     (WRK-FEDT-DEKIDAKA-SUB   =  1)   OR
                  (WRK-FEDT-DEKIDAKA-TEN   <  0)
               MOVE    WRK-NUM-X (3:8) TO  WRK-DATA
               MOVE    "-"             TO  WRK-DATA (1:1)
               PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
               PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *出来高・包括フラグ
           MOVE    WRK-FEDT-HOUKATU-FLG
                                       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
           .
       200799-ZAI-HENSYU-F-EXT.
           EXIT.
      *
      *****************************************************************
      *     きざみ値計算・編集処理
      *****************************************************************
       200799-KZM-DEKIDAKA-SEC         SECTION.
      *
      *    きざみ値計算
           MOVE    WRK-FEDT-SRYSURYO       TO  WRK-KZM-SURYO
           MOVE    ZERO                    TO  WRK-KZM-TEN2
           MOVE    ZERO                    TO  FLG-KZM
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-FEDT-SRYCD          TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-KZM-SURYO       <=  (TNS-KZMKGN - TNS-KZM )
                   IF      TNS-KZMERR      =   2  OR  3
                       MOVE  1             TO  FLG-KZM
                   END-IF
      *
      *            下限値＜データ≦上限値
               WHEN     (TNS-KZMKGN        <  WRK-KZM-SURYO ) AND
                        (TNS-KZMJGN        >= WRK-KZM-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KZM-TEN1    =  (WRK-KZM-SURYO
                                           -   TNS-KZMKGN)
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                   COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                           + ( WRK-KZM-TEN2
                                           *   TNS-KZMTEN)
                   COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    TNS-KZMJGN          <   WRK-KZM-SURYO
                   IF      TNS-KZMERR      =   0  OR  2
      *            警告エラーとする
                       MOVE    2           TO  FLG-KZM
      *            点数計算式１
                       COMPUTE WRK-KZM-TEN1    =  (WRK-KZM-SURYO
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                               + ( WRK-KZM-TEN2
                                               *   TNS-KZMTEN)
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KZM-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                               + ( WRK-KZM-TEN2
                                               *   TNS-KZMTEN)
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
      *    数量は整数であること
           IF      WRK-KZM-SURYO-S2            >   ZERO
                   MOVE    1                   TO  FLG-KZM
           END-IF
      *
           IF    ((FLG-KZM             =   ZERO )  OR
                  (FLG-KZM             =   2   )) AND
                  (WRK-KZM-TEN2        >   ZERO )
               MOVE    WRK-KZM-TEN2    TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
               MOVE    TNS-TEN         TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
           .
       200799-KZM-DEKIDAKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE62-FILE
                   RECE641-FILE
                   RECE65-FILE
                   RECE69-FILE
                   RECE68-FILE
                   DPCE-FILE
                   DPCF-FILE
                   RECE900-FILE
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE62  CNT-IN  :" CNT-IN
           DISPLAY "RECE64  CNT-IN  :" CNT-RECE64
           DISPLAY "DB      CNT-OUT :" CNT-OUT
           DISPLAY "DB         E    :" CNT-OUT-E
           DISPLAY "DB         F    :" CNT-OUT-F
           DISPLAY "*** ORCRDPC10 END ***"
      *     ACCEPT  WRK-IN FROM CONSOLE
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM   900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM-FILE-ID DELIMITED   BY  SIZE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCRDPC10"             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM         DELIMITED   BY  SIZE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理（Ｅファイル）
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           ADD     1                   TO  WRK-RENNUM-E
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
      *
      *    Ｅファイルの区分設定
           MOVE    "E"                 TO  RECEDEN-RECKBN
           MOVE    WRK-RENNUM-E        TO  RECEDEN-RENNUM
           MOVE    WRK-REC             TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
           ADD     1                   TO  CNT-OUT-E
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理（Ｆファイル）
      *****************************************************************
       5001-FILE-F-WRITE-SEC       SECTION.
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           ADD     1                   TO  WRK-RENNUM-F
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
      *
      *    Ｅファイルの区分設定
           MOVE    "F"                 TO  RECEDEN-RECKBN
           MOVE    WRK-RENNUM-F        TO  RECEDEN-RENNUM
           MOVE    WRK-REC             TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
           ADD     1                   TO  CNT-OUT-F
           .
       5001-FILE-F-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー編集処理
      *****************************************************************
       50011-KEY-HENSYU-SEC        SECTION.
      *
           INITIALIZE                  RECEDEN-REC
           MOVE    RECE62-HOSPNUM      TO  RECEDEN-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECEDEN-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECEDEN-PTID
           IF    ( SYS-2009-SYUKA-KBN  =   "1" )
               IF  (     ( RECE62-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE62-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
               OR  (     ( RECE62-TEISYUTUSAKI   =   "6" )
                     AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
                   MOVE    RECE62-SYUKA        TO  RECEDEN-RECEKA
               ELSE
                   IF      RECE62-RECEKA       =   SPACE
                       MOVE    ZERO            TO  RECEDEN-RECEKA
                   ELSE
                       MOVE    RECE62-RECEKA   TO  RECEDEN-RECEKA
                   END-IF
               END-IF
           ELSE
               IF      RECE62-RECEKA       =   SPACE
                   MOVE    ZERO            TO  RECEDEN-RECEKA
               ELSE
                    MOVE    RECE62-RECEKA  TO  RECEDEN-RECEKA
               END-IF
           END-IF
           MOVE    RECE62-TEISYUTUSAKI TO  RECEDEN-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECEDEN-RECESYUBETU
           IF      RECE62-HKNJANUM-KEY =   SPACE
               MOVE    ZERO                TO  RECEDEN-HKNJANUM
           ELSE
               MOVE    RECE62-HKNJANUM-KEY TO  RECEDEN-HKNJANUM
           END-IF
           IF      RECE62-HOJOKBN-KEY      =   SPACE
               MOVE    ZERO                TO  RECEDEN-HOJOKBN-KEY
           ELSE
               MOVE    RECE62-HOJOKBN-KEY  TO  RECEDEN-HOJOKBN-KEY
           END-IF
           MOVE    RECE62-KOHID-KEY    TO  RECEDEN-KOHID-KEY
           IF      RECE62-TEKSTYMD     =   SPACE
               MOVE    ZERO            TO  RECEDEN-TEKSTYMD
           ELSE
               MOVE    RECE62-TEKSTYMD TO  RECEDEN-TEKSTYMD
           END-IF
           MOVE    RECE62-RECEIPT-KBN  TO  RECEDEN-RECEIPT-KBN
           .
       50011-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       50012-DBINSERT-SEC          SECTION.
      *
           MOVE    WRK-PARA-CREYMD     TO  RECEDEN-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECEDEN-CREHMS
      *
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "tbl_efout"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBINSERT-SEC
      *
           ADD     1                   TO  CNT-OUT
           .
       50012-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    右ずめ編集処理
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *              
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           .
       5003-DATA-RIGHT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW2)          DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW2                TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI-TAB            DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF
      *
               PERFORM VARYING IDW2    FROM    WRK-ST    BY  1
                       UNTIL   IDW2    >       10
                   IF      WRK-NUM (IDW2:1)  NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW2
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW2:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ
                       MOVE    10                TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI-TAB            DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *????
           IF      FLG-HKNCOMBI        =   1
               DISPLAY "HKNCOMBI INV " COMB-HOSPNUM " " COMB-PTID " "
                                       COMB-HKNCOMBINUM " "
           END-IF
      *????
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ検索処理(KEY2)
      *****************************************************************
       910-HKNCOMBI-KEY2-SEL-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           .
       910-HKNCOMBI-KEY2-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込処理(KEY2)
      *****************************************************************
       910-HKNCOMBI-KEY2-FETCH-SEC     SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-HKNCOMBI
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           .
       910-HKNCOMBI-KEY2-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           PERFORM 900-CLOSE-SEC
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-READ-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    RECE62-SRYYM        TO  TTNS-CHK-YMD (1:6)
           MOVE    "01"                TO  TTNS-CHK-YMD (7:2)
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD  TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOSTYMD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM   900-DBFETCH-SEC
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
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
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
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
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
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE62-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-READ
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      FLG-READ    =   1
               READ    RECE62-FILE         NEXT
                   AT  END
                       MOVE    1           TO  FLG-END
                       MOVE    HIGH-VALUE  TO  KEY-NEW
                   NOT AT  END
                       IF      STS-RECE62          =   "00"
                           CONTINUE
                       ELSE
                           MOVE    SPACE           TO  WRK-ERR-AREA
                           INITIALIZE                  WRK-ERR-AREA
                           MOVE    2               TO  WRK-ERR-FLG
                           MOVE    RECE62PARA      TO  WRK-ERR-FILE-NM
                           MOVE    STS-RECE62      TO  WRK-ERR-FILE-STS
                           PERFORM 500-FILE-ERR-ABORT-SEC
                       END-IF
      *
      **               IF  ( ( WRK-PARA-SYARECEKBN =   ZERO
      **                                           OR  "1" )   AND
      **                     ( RECE62-TEISYUTUSAKI
      **                                           =   "1"  )      )
      **               OR  ( ( WRK-PARA-KKHRECEKBN =   ZERO
      **                                           OR  "1"  )  AND
      **                     ( RECE62-TEISYUTUSAKI
      **                                           =   "2"  )      )
                       IF  (   RECE62-PTNUM        =   SPACE       )
                       OR  (   RECE62-TSTPTNUMKBN  =   "1"         )
                       OR  (   RECE62-RECESYUBETU  =   ZERO   OR 
                                                       8888   OR 
                                                       6666        )
      *??              OR  (   RECE62-SRYYM    NOT =   WRK-PARA-SRYYM)
                           MOVE    2               TO  FLG-READ
                       ELSE
      *                    主科設定のとき未設定なら更新しない
                           IF    ( SYS-2009-SYUKA-KBN  =   "1"  )
                           AND   ( RECE62-SYUKA        =   "XX" )
                             IF  (     ( RECE62-TEISYUTUSAKI   = "1" )
                                   AND ( SYS-2009-SYUKA-SYAHO  = "1" ) )
                             OR  (     ( RECE62-TEISYUTUSAKI   = "2" )
                                   AND ( SYS-2009-SYUKA-KOKUHO = "1" ) )
                               MOVE    2               TO  FLG-READ
                             END-IF
                           END-IF
                       END-IF
      *
                       IF      FLG-READ            =   ZERO
      *                    医療機関情報取得
                           IF      WRK-PARA-SRYYM  =   ZERO
                               MOVE  RECE62-SRYYM      TO
                                                   WRK-SYS-SRYYMD(1:6)
                               MOVE  "01"              TO
                                                   WRK-SYS-SRYYMD(7:2)
                               PERFORM 900-SYSKANRI-HENSYU-SEC
                           END-IF
                           MOVE    1               TO  FLG-READ
                       END-IF
      *
                       IF      FLG-READ            =   1
                           ADD     1               TO  CNT-IN
                           MOVE    RECE62-HOSPNUM  TO  KEY-N-HOSPNUM
                           MOVE    RECE62-SRYYM    TO  KEY-N-SRYYM
                           MOVE    RECE62-NYUGAIKBN
                                                   TO  KEY-N-NYUGAIKBN
                           MOVE    RECE62-PTID     TO  KEY-N-PTID
                           MOVE    RECE62-RECEKA   TO  KEY-N-RECEKA
                           MOVE    RECE62-TEISYUTUSAKI
                                                   TO KEY-N-TEISYUTUSAKI
      *
                           DISPLAY "RECE62  PTNUM = " RECE62-PTNUM
                                   "  PTID = " RECE62-PTID
                                   "  KEY = " RECE62-KEY
                       ELSE
                           MOVE    ZERO        TO  FLG-READ
                       END-IF
               END-READ
           END-PERFORM
           .
       910-RECE62-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
      *    医療機関情報（基本情報）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  10   )
                   OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1001(SYSKAN-IDX) TO  SYS-1001-REC
               IF    ( SYS-1001-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING  "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE641-NEXT-SEC           SECTION.
      *
           READ    RECE641-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE64
               NOT AT  END
                   IF      STS-RECE64          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE64PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE64          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE64
                   ADD     1         TO  CNT-RECE64
           END-READ
           .
       990-RECE641-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE65-NEXT-SEC           SECTION.
      *
           READ    RECE65-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE65
               NOT AT  END
                   IF      STS-RECE65          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE65PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE65          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE65
           END-READ
           .
       990-RECE65-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０９データ読み
      ****************************************************************
       990-RECE69-NEXT-SEC           SECTION.
      *
           READ    RECE69-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE69
               NOT AT  END
                   IF      STS-RECE69          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE69PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE69          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE69
           END-READ
           .
       990-RECE69-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    入退院履歴情報データ読み
      ****************************************************************
       900-RECE68-NEXT-SEC           SECTION.
      *
           READ    RECE68-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE68
               NOT AT  END
                   IF      STS-RECE68          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE68PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE68          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE68
           END-READ
           .
       900-RECE68-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    順序番号情報データ読み
      ****************************************************************
       900-RECE900-READ-SEC           SECTION.
      *
           MOVE    ZERO              TO  FLG-RECE900
      *
           READ    RECE900-FILE
               INVALID
                   MOVE    1         TO  FLG-RECE900
               NOT INVALID
                   MOVE    ZERO      TO  FLG-RECE900
           END-READ
           .
       900-RECE900-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括分データ作成処理
      *****************************************************************
       9999-HOUKATU-SEC              SECTION.
      *
           INITIALIZE                      RECE641-REC
           MOVE    RECE62-HOSPNUM      TO  RECE641-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE641-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE641-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE641-PTID
           MOVE    RECE62-RECEKA       TO  RECE641-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE641-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE641-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE641-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE641-HOJOKBN
           MOVE    RECE62-KOHID-KEY    TO  RECE641-KOHID
           MOVE    RECE62-TEKSTYMD     TO  RECE641-TEKSTYMD
           MOVE    RECE62-RECEIPT-KBN  TO  RECE641-RECEIPT-KBN
           MOVE    "90"                TO  RECE641-SRYKBN
      *
           START   RECE641-FILE
               KEY IS    >=   RECE641-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE64
               NOT INVALID
                   PERFORM 990-RECE641-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE64           =   1                    )
               OR     ( RECE62-HOSPNUM   NOT =   RECE641-HOSPNUM      )
               OR     ( RECE62-SRYYM     NOT =   RECE641-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE641-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE641-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE641-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE641-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE641-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE641-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE641-HOJOKBN      )
               OR     ( RECE62-KOHID-KEY NOT =   RECE641-KOHID        )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE641-TEKSTYMD     )
               OR     ( RECE62-RECEIPT-KBN
                                         NOT =   RECE641-RECEIPT-KBN  )
               OR     ( RECE641-SRYKBN       >=  "93"                 )
      *
               PERFORM 9999-HOUKATU-1-SEC
      *
               IF      FLG-TOKUTEINYUIN    =   ZERO
                   PERFORM 990-RECE641-NEXT-SEC
               ELSE
                   MOVE    1               TO  FLG-RECE64
               END-IF
           END-PERFORM
      *
           IF      FLG-TOKUTEINYUIN    =   1
                   PERFORM 9999-HOUKATU-SAKUSEI-SEC
           END-IF
           .
       9999-HOUKATU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    包括分データ作成処理
      *****************************************************************
       9999-HOUKATU-1-SEC              SECTION.
      *
           INITIALIZE                       RECE65-REC
           MOVE    RECE641-HOSPNUM      TO  RECE65-HOSPNUM
           MOVE    RECE641-SRYYM        TO  RECE65-SRYYM
           MOVE    RECE641-NYUGAIKBN    TO  RECE65-NYUGAIKBN
           MOVE    RECE641-PTID         TO  RECE65-PTID
           MOVE    RECE641-RECEKA       TO  RECE65-RECEKA
           MOVE    RECE641-TEISYUTUSAKI TO  RECE65-TEISYUTUSAKI
           MOVE    RECE641-RECESYUBETU  TO  RECE65-RECESYUBETU
           MOVE    RECE641-HKNJANUM     TO  RECE65-HKNJANUM
           MOVE    RECE641-HOJOKBN      TO  RECE65-HOJOKBN
           MOVE    RECE641-KOHID        TO  RECE65-KOHID
           MOVE    RECE641-TEKSTYMD     TO  RECE65-TEKSTYMD
           MOVE    RECE641-RECEIPT-KBN  TO  RECE65-RECEIPT-KBN
           MOVE    RECE641-ZAINUM       TO  RECE65-ZAINUM
           START   RECE65-FILE
               KEY IS    >=   RECE65-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE65
               NOT INVALID
                   PERFORM 990-RECE65-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE65             =   1                   )
               OR     ( RECE641-HOSPNUM    NOT =   RECE65-HOSPNUM      )
               OR     ( RECE641-SRYYM      NOT =   RECE65-SRYYM        )
               OR     ( RECE641-NYUGAIKBN  NOT =   RECE65-NYUGAIKBN    )
               OR     ( RECE641-PTID       NOT =   RECE65-PTID         )
               OR     ( RECE641-RECEKA     NOT =   RECE65-RECEKA       )
               OR     ( RECE641-TEISYUTUSAKI
                                           NOT =   RECE65-TEISYUTUSAKI )
               OR     ( RECE641-RECESYUBETU
                                           NOT =   RECE65-RECESYUBETU  )
               OR     ( RECE641-HKNJANUM   NOT =   RECE65-HKNJANUM     )
               OR     ( RECE641-HOJOKBN    NOT =   RECE65-HOJOKBN      )
               OR     ( RECE641-KOHID      NOT =   RECE65-KOHID        )
               OR     ( RECE641-TEKSTYMD   NOT =   RECE65-TEKSTYMD     )
               OR     ( RECE641-RECEIPT-KBN
                                           NOT =   RECE65-RECEIPT-KBN  )
               OR     ( RECE641-ZAINUM     NOT =   RECE65-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
      *
                   IF    RECE65-SRYYM   <=   "201403"
                       PERFORM 9999-TOKUTEINYUIN-22-CHK-SEC
                   ELSE
                       PERFORM 9999-TOKUTEINYUIN-26-CHK-SEC
                   END-IF
                   IF      FLG-TOKUTEINYUIN    =   1
                        DISPLAY "## target srycd exist!! "
                                RECE65-SRYCD(IDX)
                        MOVE    5                   TO  IDX
                   END-IF
               END-PERFORM
      *
               IF      FLG-TOKUTEINYUIN    =   ZERO
                   PERFORM 990-RECE65-NEXT-SEC
               ELSE
                   MOVE    1               TO  FLG-RECE65
               END-IF
           END-PERFORM
           .
       9999-HOUKATU-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理（該当特定入院料チェック）平成２６年３月まで
      *****************************************************************
       9999-TOKUTEINYUIN-22-CHK-SEC       SECTION.
      *
           SET     TBL-TOKUTEINYUIN22-IDX    TO    1
           SEARCH  TBL-TOKUTEINYUIN22-OCC
                               VARYING TBL-TOKUTEINYUIN22-IDX
               AT  END
                   MOVE    ZERO                TO  FLG-TOKUTEINYUIN
               WHEN        RECE65-SRYCD(IDX)
                                       =   TBL-TOKUTEINYUIN22-SRYCD
                                            (TBL-TOKUTEINYUIN22-IDX)
                   MOVE    1                   TO  FLG-TOKUTEINYUIN
           END-SEARCH
           .
       9999-TOKUTEINYUIN-22-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理（該当特定入院料チェック）平成２６年４月から
      *****************************************************************
       9999-TOKUTEINYUIN-26-CHK-SEC       SECTION.
      *
           SET     TBL-TOKUTEINYUIN26-IDX    TO    1
           SEARCH  TBL-TOKUTEINYUIN26-OCC
                               VARYING TBL-TOKUTEINYUIN26-IDX
               AT  END
                   MOVE    ZERO                TO  FLG-TOKUTEINYUIN
               WHEN        RECE65-SRYCD(IDX)
                                       =   TBL-TOKUTEINYUIN26-SRYCD
                                            (TBL-TOKUTEINYUIN26-IDX)
                   MOVE    1                   TO  FLG-TOKUTEINYUIN
           END-SEARCH
           .
       9999-TOKUTEINYUIN-26-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理
      *****************************************************************
       9999-HOUKATU-SAKUSEI-SEC       SECTION.
      *
           PERFORM     VARYING   IDX-HOUKATU  FROM   1   BY  1
             UNTIL    (IDX-HOUKATU  >   20)   OR
                      (RECE62-SRYKA(IDX-HOUKATU)   =   SPACE)
      *
             INITIALIZE                      SRYACCT-REC
             MOVE    RECE62-HOSPNUM      TO  ACCT-HOSPNUM
             MOVE    RECE62-SRYYM        TO  ACCT-SRYYM
             MOVE    RECE62-NYUGAIKBN    TO  ACCT-NYUGAIKBN
             MOVE    RECE62-PTID         TO  ACCT-PTID
             DISPLAY "RECE62-SRYKA = " RECE62-SRYKA(IDX-HOUKATU)
             MOVE    RECE62-SRYKA(IDX-HOUKATU)
                                         TO  ACCT-SRYKA
             MOVE    9999                TO  ACCT-HKNCOMBI
             MOVE    SRYACCT-REC         TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_sryacct"       TO  MCP-TABLE
             MOVE    "key24"             TO  MCP-PATHNAME
             PERFORM 900-ORCDBMAIN-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key24"         TO  MCP-PATHNAME
                 PERFORM 900-SRYACCT-READ-SEC
             ELSE
                 INITIALIZE                  SRYACCT-REC
                 MOVE    1               TO  FLG-SRYACCT
             END-IF
      *
             PERFORM UNTIL       FLG-SRYACCT     =   1
                 PERFORM 9999-SRYACCT-HENSYU-SEC
                 MOVE    "tbl_sryacct"      TO  MCP-TABLE
                 MOVE    "key24"            TO  MCP-PATHNAME
                 PERFORM 900-SRYACCT-READ-SEC
             END-PERFORM
      *
             MOVE    "tbl_sryacct"      TO  MCP-TABLE
             MOVE    "key24"            TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
           END-PERFORM
           .
       9999-HOUKATU-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成　診療会計・診療行為データ編集処理
      *****************************************************************
       9999-SRYACCT-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING IJ  FROM  1  BY  1
                   UNTIL   IJ  >  31
               IF      RECE62-JNISSUTBL(IJ)    =   SPACE
                   COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                               -   ACCT-DAY(1 IJ)
                   MOVE    ZERO                TO  ACCT-DAY(1 IJ)
               END-IF
           END-PERFORM
           IF      ACCT-ZAIKAISU       =   ZERO
                   GO      TO        9999-SRYACCT-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-HOUKATU-RECE04
      *
      *    診療行為データ読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACT    =    1
               PERFORM 9999-SRYACT-HENSYU-SEC
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       9999-SRYACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成　診療会計・診療行為データ編集処理
      *****************************************************************
       9999-SRYACT-HENSYU-SEC               SECTION.
      *
           PERFORM VARYING II      FROM    1   BY  1
                   UNTIL   II      >   5
                   OR      SRY-SRYCD (II)      =   SPACE
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    SRY-SRYCD(II)       TO  TNS-SRYCD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
      *
               IF    ( FLG-HOUKATU-RECE04      =   ZERO )  AND
                     ( II                      =   1 )
                   PERFORM 9999-HOUKATU-SAKUSEI-RECE04-SEC
                   WRITE   RECE641-REC
                   IF      STS-RECE64      NOT =   ZERO
                       DISPLAY "RECE64 WRITE ERROR STS= "
                               STS-RECE64
                   END-IF
                   MOVE    1                   TO  FLG-HOUKATU-RECE04
               END-IF
               PERFORM 9999-HOUKATU-SAKUSEI-RECE05-SEC
           END-PERFORM
      *
           WRITE   RECE65-REC
           IF      STS-RECE65      NOT =   ZERO
                       DISPLAY "RECE65 WRITE ERROR STS= "
                               STS-RECE65
           END-IF
           .
       9999-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理（レセプト０４）
      *****************************************************************
       9999-HOUKATU-SAKUSEI-RECE04-SEC SECTION.
      *
           INITIALIZE                      RECE641-REC
      *医療機関ＩＤ
           MOVE    RECE62-HOSPNUM      TO  RECE641-HOSPNUM
      *診療年月
           MOVE    RECE62-SRYYM        TO  RECE641-SRYYM
      *入外区分
           MOVE    RECE62-NYUGAIKBN    TO  RECE641-NYUGAIKBN
      *患者ＩＤ
           MOVE    RECE62-PTID         TO  RECE641-PTID
      *レセプト科
           MOVE    RECE62-RECEKA       TO  RECE641-RECEKA
      *審査支払機関
           MOVE    RECE62-TEISYUTUSAKI TO  RECE641-TEISYUTUSAKI
      *レセプト種別
           MOVE    RECE62-RECESYUBETU  TO  RECE641-RECESYUBETU
      *保険者番号
           MOVE    RECE62-HKNJANUM-KEY TO  RECE641-HKNJANUM
      *補助区分
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE641-HOJOKBN
      *公費ＩＤ
           MOVE    RECE62-KOHID-KEY    TO  RECE641-KOHID
      *適用開始年月日
           MOVE    RECE62-TEKSTYMD     TO  RECE641-TEKSTYMD
      *分割区分
           MOVE    RECE62-RECEIPT-KBN  TO  RECE641-RECEIPT-KBN
      *診療区分
           IF      ACCT-SRYKBN         =   "64"
               MOVE    "60"                TO  RECE641-SRYKBN
           ELSE
               MOVE    ACCT-SRYKBN         TO  RECE641-SRYKBN
           END-IF
      *コード表用番号−区分番号
           MOVE    TNS-CDKBN-KBNNUM    TO  RECE641-CDKBN-KBNNUM
      *剤番号
           MOVE    ACCT-ZAINUM         TO  RECE641-ZAINUM
      *連番号
           MOVE    1                   TO  RECE641-ZAIREN
      *公費分離区分(???)
           MOVE    RECE62-KOHBUNRI-MIN TO  RECE641-KOHBUNRI
      *患者番号
           MOVE    RECE62-PTNUM        TO  RECE641-PTNUM
      *診療種別区分
           IF      ACCT-SRYKBN         =   "64"
               MOVE    "600"               TO  RECE641-SRYSYUKBN
           ELSE
               MOVE    SRY-SRYSYUKBN       TO  RECE641-SRYSYUKBN
           END-IF
      *診療科
           MOVE    RECE62-SRYKA(1)        TO  RECE641-SRYKA
      *保険組合せ番号(???)
           MOVE    RECE62-HKNCOMBI(1)  TO  RECE641-HKNCOMBI
      *自保区分
           MOVE    ACCT-JIHOKBN        TO  RECE641-JIHOKBN
           IF      ACCT-JIHOKBN        =   SPACE
               MOVE    "1"                 TO  RECE641-JIHOKBN
           ELSE
               DISPLAY "JIHOKBN ? PTNUM= " RECE62-PTNUM
                       "JIHOKBN= " ACCT-JIHOKBN
           END-IF
      *剤点数
           MOVE    ACCT-ZAITEN         TO  RECE641-ZAITEN
      *診療コード計
           MOVE    ACCT-SRYCDTOTAL     TO  RECE641-SRYCDTOTAL
      *数量計
           MOVE    ACCT-SURYOUTOTAL    TO  RECE641-SURYOUTOTAL
      *診療コード＊数量の累計(???)
      *     MOVE                        TO  RECE641-RUIKEITOTAL
      *明細数
           MOVE    ACCT-MEISAISU       TO  RECE641-MEISAISU
      *剤回数
           MOVE    ACCT-ZAIKAISU       TO  RECE641-ZAIKAISU
      *手技点数１
           MOVE    ACCT-SYUTEN1        TO  RECE641-SYUTEN1
      *手技点数２
           MOVE    ACCT-SYUTEN2        TO  RECE641-SYUTEN2
      *薬剤点数
           MOVE    ACCT-YKZTEN         TO  RECE641-YKZTEN
      *器材点数
           MOVE    ACCT-KIZAITEN       TO  RECE641-KIZAITEN
      *ドクターコード
           MOVE    ACCT-DRCD           TO  RECE641-DRCD
      *剤請求フラグ
           MOVE    ACCT-ZAIREQFLG      TO  RECE641-ZAIREQFLG
      *逓減率
           MOVE    ACCT-TEIGENRATE     TO  RECE641-TEIGENRATE
      *投薬剤点０（１又は２（コメントのみ））(???)
      *     MOVE                        TO  RECE641-TOYAKUFLG
      *関係コメント存在フラグ(???)
      *     MOVE                        TO  RECE641-SETCOMFLG
      *剤区分
            MOVE    ACCT-ZAIKBN        TO  RECE641-ZAIKBN
      *検査等実施判断
           MOVE    TNS-KNSJISKBN       TO  RECE641-KNSJISKBN
      *検査等実施判断グループ
           MOVE    TNS-KNSJISGRPKBN    TO  RECE641-KNSJISGRPKBN
      *レセ電用入院料加算フラグ(???)
      *      MOVE                       TO  RECE641-NYUKSNFLG
      *日数回数テーブル
           MOVE    ACCT-DAY-AREA       TO  RECE641-DAY-AREA
           .
       9999-HOUKATU-SAKUSEI-RECE04-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理（レセプト０５）
      *****************************************************************
       9999-HOUKATU-SAKUSEI-RECE05-SEC SECTION.
      *
           IF      II                  =   1
      *
           INITIALIZE                      RECE65-REC
      *医療機関ＩＤ
           MOVE    RECE62-HOSPNUM      TO  RECE65-HOSPNUM
      *診療年月
           MOVE    RECE62-SRYYM        TO  RECE65-SRYYM
      *入外区分
           MOVE    RECE62-NYUGAIKBN    TO  RECE65-NYUGAIKBN
      *患者ＩＤ
           MOVE    RECE62-PTID         TO  RECE65-PTID
      *レセプト科
           MOVE    RECE62-RECEKA       TO  RECE65-RECEKA
      *審査支払機関
           MOVE    RECE62-TEISYUTUSAKI TO  RECE65-TEISYUTUSAKI
      *レセプト種別
           MOVE    RECE62-RECESYUBETU  TO  RECE65-RECESYUBETU
      *保険者番号
           MOVE    RECE62-HKNJANUM-KEY TO  RECE65-HKNJANUM
      *補助区分
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE65-HOJOKBN
      *公費ＩＤ
           MOVE    RECE62-KOHID-KEY    TO  RECE65-KOHID
      *適用開始年月日
           MOVE    RECE62-TEKSTYMD     TO  RECE65-TEKSTYMD
      *分割区分
           MOVE    RECE62-RECEIPT-KBN  TO  RECE65-RECEIPT-KBN
      *剤番号
           MOVE    SRY-ZAINUM          TO  RECE65-ZAINUM
      *剤連番
           MOVE    1                   TO  RECE65-ZAIREN
      *連番号
           MOVE    SRY-RENNUM          TO  RECE65-RENNUM
      *公費分離区分(???)
           MOVE    RECE62-KOHBUNRI-MIN TO  RECE65-KOHBUNRI
      *患者番号
           MOVE    RECE62-PTNUM        TO  RECE65-PTNUM
      *診療科
           MOVE    RECE62-SRYKA(1)        TO  RECE65-SRYKA
      *診療区分
           IF      SRY-SRYKBN          =   "64"
               MOVE    "60"                TO  RECE65-SRYKBN
           ELSE
               MOVE    SRY-SRYKBN          TO  RECE65-SRYKBN
           END-IF
      *診療種別区分
           MOVE    SRY-SRYSYUKBN       TO  RECE65-SRYSYUKBN
      *自費金額
           MOVE    SRY-JIHIMONEYTOTAL  TO  RECE65-JIHIMONEY
      *
           END-IF
      *
      *診療コード
           MOVE    SRY-SRYCD (II)      TO  RECE65-SRYCD (II)
      *診療種別区分
           IF      SRY-SRYKBN          =   "64"
               MOVE    "600"               TO  RECE65-SRYSYUKBN-X (II)
           ELSE
               MOVE    SRY-SRYSYUKBN       TO  RECE65-SRYSYUKBN-X (II)
           END-IF
      *数量
           MOVE    SRY-SRYSURYO (II)   TO  RECE65-SRYSURYO (II)
      *回数
           MOVE    SRY-SRYKAISU (II)   TO  RECE65-SRYKAISU (II)
      *単位コード
           MOVE    TNS-TANICD          TO  RECE65-TANICD (II)
      *単位名称有効桁数
           MOVE    TNS-TANIMOJISU      TO  RECE65-TANIMOJISU (II)
      *単位名称
           MOVE    TNS-TANINAME        TO  RECE65-TANINAME (II)
      *点数識別区分
           MOVE    TNS-TENSIKIBETU     TO  RECE65-TENSIKIBETU (II)
      *点数
           MOVE    TNS-TEN             TO  RECE65-TEN (II)
      *明細請求フラグ
           MOVE    SRY-MEISKYFLG (II)  TO  RECE65-MEISKYFLG (II)
      *自動算定区分
           MOVE    SRY-AUTOKBN (II)    TO  RECE65-AUTOKBN (II)
      *名称入力番号
           MOVE    SRY-INPUTNUM (II)   TO  RECE65-INPUTNUM (II)
      *点数欄集計先識別
           MOVE    TNS-NYUTENTTLKBN    TO  RECE65-TENTTLKBN (II)
      *データ区分
           MOVE    TNS-DATAKBN         TO  RECE65-DATAKBN (II)
      *包括対象検査
           MOVE    TNS-HOUKNSKBN       TO  RECE65-HOUKNSKBN (II)
      *検査等実施判断
           MOVE    TNS-KNSJISKBN       TO  RECE65-KNSJISKBN (II)
      *検査等実施判断グループ
           MOVE    TNS-KNSJISGRPKBN    TO  RECE65-KNSJISGRPKBN (II)
      *各種番号等
           MOVE    TNS-CDKBN-KBN       TO  RECE65-CDKBN-KBN (II)
           MOVE    TNS-CDKBN-SYO       TO  RECE65-CDKBN-SYO (II)
           MOVE    TNS-CDKBN-BU        TO  RECE65-CDKBN-BU (II)
           MOVE    TNS-CDKBN-KBNNUM    TO  RECE65-CDKBN-KBNNUM (II)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  RECE65-CDKBN-KBNNUM-EDA (II)
           MOVE    TNS-CDKBN-KOUBAN    TO  RECE65-CDKBN-KOUBAN (II)
           MOVE    TNS-KOUHYOJYUNNUM   TO  RECE65-KOUHYOJYUNNUM (II)
      *告示等識別区分１
           MOVE    TNS-KOKUJISKBKBN1   TO  RECE65-KOKUJISKBKBN1 (II)
      *告示等識別区分２
           MOVE    TNS-KOKUJISKBKBN2   TO  RECE65-KOKUJISKBKBN2 (II)
      *通則加算対象外区分
           MOVE    TNS-TUSOKUGAIKBN    TO  RECE65-TUSOKUGAIKBN (II)
      *時間外加算区分
           MOVE    TNS-TIMEKSNKBN      TO  RECE65-TIMEKSNKBN (II)
      *きざみ値計算識別
           MOVE    TNS-KZMCOMPSIKIBETU TO  RECE65-KZMCOMPSIKIBETU (II)
      *注加算コード
           MOVE    TNS-CHUKSNCD        TO  RECE65-CHUKSNCD (II)
      *通番
           MOVE    TNS-CHUKSNTUBAN     TO  RECE65-CHUKSNTUBAN (II)
      *検体検査コメント
           MOVE    TNS-KENTAICOMMENT   TO  RECE65-KENTAICOMMENT (II)
      *入力コメント
           MOVE    SPACE               TO  RECE65-INPUTCOMENT (II)
      *漢字有効桁数
           MOVE    TNS-YUKOKETA        TO  RECE65-YUKOKETA (II)
      *漢字名称
           MOVE    TNS-NAME            TO  RECE65-NAME (II)
      *特定器材種別１
           MOVE    TNS-TOKUKIZAISBT1   TO  RECE65-TOKUKIZAISBT1 (II)
      *酸素等区分
           MOVE    TNS-SANSOKBN        TO  RECE65-SANSOKBN (II)
      *レセプト電算対象外フラグ(???)
      *     MOVE                        TO  RECE65-RECEDENFLG(II)
      *レセプト対象外フラグ(???)
      *     MOVE                        TO  RECE65-RECEPTFLG(II)
      *レセ電用特定器材商品名(???)
      *     MOVE                        TO  RECE65-RECEDENNAME(II)
      *レセ用特定器材商品名編集フラグ(???)
      *     MOVE                        TO  RECE65-RECEHENFLG(II)
      *関係コメント関連設定フラグ(???)
      *     MOVE                        TO  RECE65-SETCOMATFLG(II)
           .
       9999-HOUKATU-SAKUSEI-RECE05-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       900-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ更新処理エラー　TABLE="
                                           DELIMITED  BY  SIZE
                       MCP-TABLE           DELIMITED  BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCRDPC10 insert err TABLE="
                                           DELIMITED   BY  SIZE
                      MCP-TABLE            DELIMITED   BY  SPACE
                      " PATHNAME="         DELIMITED   BY  SIZE
                      MCP-PATHNAME         DELIMITED   BY  SPACE
                      " RC="               DELIMITED   BY  SIZE
                      WRK-MCP-RC           DELIMITED   BY  SIZE
                      LOW-VALUE            DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCRDPC10 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
