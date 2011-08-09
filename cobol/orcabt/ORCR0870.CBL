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
       PROGRAM-ID.             ORCR0870.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      コメント編集（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/01    NACL-門脇　    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT5→31
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.01  リハ言語聴覚療法（３）追加対応
      *  01.01.02    NACL-門脇    04.04.05  在宅血液透析頻回指導管理料の
      *                                     コメント対応
      *  01.01.03    NACL-門脇    04.04.15  脳血管疾患急性発症日のコメント
      *                                     対応
      *  01.01.04    NACL-門脇    04.06.07  コピー句０８５変更に伴う修正
      *  中止処理追加
      *  01.02.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.02.02    NACL-門脇    04/12/09  脳血管疾患急性発症終了日対応
      *  01.02.03    NACL-門脇    05/01/18  酸素計算式コメント対応
      *  01.02.04    NACL-門脇    05/05/10  脳血管疾患急性発症病名記載対応
      *  01.02.05    NACL-門脇    05/08/22  リハビリ実施回数差し込み修正対応
      *  01.02.06    NACL-門脇    05/08/31  自賠責従来様式対応
      *  01.02.07    NACL-門脇    05/11/01  （検選）等コメント対応
      *  01.02.08    NACL-門脇    05/11/15  傷病手当金、療養費同意書の自動
      *                                     記載編集修正
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  02.09.02    NACL-門脇    06/04/28  リハ検索条件修正
      *  02.09.03    NACL-門脇    06/05/01  リハ発症日当月分記載修正
      *  02.09.04    NACL-門脇    06/05/30  リハ発症日記載修正
      *  03.01.01    NACL-門脇    06/06/22  往診、在宅患者訪問診療料の
      *                                     コメント自動記載対応
      *  03.01.02    NACL-門脇    06/07/10  在支援コメント自動記載対応
      *  03.01.03    NACL-門脇    06/07/14  診療区分置き換えフラグの編集
      *  03.02.01    NACL-門脇    06/08/01  リハ発症日記載修正
      *  03.03.01    NACL-門脇    06/10/19  療養病棟レセコメント記載対応
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *  03.04.02    NACL-門脇    07/01/12  リハビリ発症終了日対応
      *  03.04.03    NACL-門脇    07/01/15  リハビリ手術日・急性増悪日記載対応
      *  03.04.04    NACL-門脇    07/03/05  酸素計算式修正
      *  03.04.05    NACL-門脇    07/04/13  リハビリ対応
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/06/26  自賠責平成１９年４月改正様式対応
      *  03.05.04    NACL-門脇    07/08/07  算定履歴に関わる記載修正
      *  04.01.01    NACL-門脇    07/11/01  公務災害対応
      *  04.01.02    NACL-門脇    07/11/26  特定器材商品名記載対応
      *  04.01.03    NACL-門脇    07/11/30  退院時服薬指導加算算定日記載対応
      *  04.01.04    NACL-門脇    08/01/15  公害対応
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （薬剤管理指導料コード追加）
      *                                     （診療区分６４：病理診断対応）
      *                                     （在支病自動記載対応）
      *  04.02.02    NACL-門脇    08/04/03  ○○にて入院料算定済編集修正
      *  04.02.03    NACL-小豆沢  08/04/08  医療区分コメント改定対応
      *  04.02.04    NACL-門脇    08/05/07  退院時服薬指導加算算定日記載修正
      *
      *  04.03.01    NACL-門脇    08/08/19  入院料未算定時のコメント対応
      *  04.03.02    NACL-門脇    08/11/04  リハビリ発症日年月日対応
      *
      *  04.04.01    NACL-門脇    09/02/04  検査のコメント自動記載対応
      *  04.04.02    NACL-門脇    09/03/11  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *  04.04.03    NACL-門脇    10/03/03  療養レセコメント編集修正
      *  04.04.04    NACL-門脇    10/04/06  改正対応
      *                                     （コメント自動記載）
      *  04.04.05    NACL-門脇    10/04/23  在宅血液透析頻回指導管理料の
      *                                     コメント自動記載修正
      *  04.04.06    NACL-門脇    10/05/10  入院期間コメント修正
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.02    NACL-門脇    09/11/25  リハビリコメント対応
      *  04.05.03    NACL-門脇    11/08/08  外泊（他医受診）の日付コメント
      *                                     の差し込みを止める
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *
           SELECT  RECE84-FILE     ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE84-KEY
                                   FILE    STATUS  IS  STS-RECE84.
      *
           SELECT  RECE85-FILE     ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE85-KEY
                                   FILE    STATUS  IS  STS-RECE85.
      *
           SELECT  RECE841-FILE    ASSIGN  RECE841PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE841-KEY
                                   FILE    STATUS  IS  STS-RECE841.
           SELECT  RECE8441-FILE   ASSIGN  RECE841PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8441-KEY
                                   FILE    STATUS  IS  STS-RECE841.
      *
           SELECT  RECE851-FILE    ASSIGN  RECE851PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE851-KEY
                                   FILE    STATUS  IS  STS-RECE851.
           SELECT  RECE8551-FILE   ASSIGN  RECE851PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8551-KEY
                                   FILE    STATUS  IS  STS-RECE851.
      *ワーク
           SELECT  RECE849-FILE    ASSIGN  RECE849PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE849-KEY
                                   FILE    STATUS  IS  STS-RECE849.
           SELECT  RECE8449-FILE   ASSIGN  RECE849PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8449-KEY
                                   FILE    STATUS  IS  STS-RECE849.
      *
           SELECT  RECE859-FILE    ASSIGN  RECE859PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE859-KEY
                                   FILE    STATUS  IS  STS-RECE859.
           SELECT  RECE8559-FILE   ASSIGN  RECE859PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8559-KEY
                                   FILE    STATUS  IS  STS-RECE859.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC.
           COPY    "CPRCF082.INC".
      *
       FD  RECE84-FILE.
       01  RECE84-REC.
           COPY    "CPRCF084.INC".
      *
       FD  RECE85-FILE.
       01  RECE85-REC.
           COPY    "CPRCF085.INC".
      *
       FD  RECE841-FILE.
       01  RECE841-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //RECE841//.
      *
       FD  RECE851-FILE.
       01  RECE851-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //RECE851//.
      *
       FD  RECE8441-FILE.
       01  RECE8441-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //RECE8441//.
      *
       FD  RECE8551-FILE.
       01  RECE8551-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //RECE8551//.
      *ワーク
       FD  RECE849-FILE.
       01  RECE849-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //RECE849//.
      *
       FD  RECE859-FILE.
       01  RECE859-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //RECE859//.
      *
       FD  RECE8449-FILE.
       01  RECE8449-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //RECE8449//.
      *
       FD  RECE8559-FILE.
       01  RECE8559-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //RECE8559//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE82//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE84//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE85//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE841//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE851//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE849//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE859//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82          PIC X(02).
           03  STS-RECE84          PIC X(02).
           03  STS-RECE841         PIC X(02).
           03  STS-RECE85          PIC X(02).
           03  STS-RECE851         PIC X(02).
           03  STS-RECE849         PIC X(02).
           03  STS-RECE859         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE84          PIC 9(01).
           03  FLG-RECE849         PIC 9(01).
           03  FLG-RECE85          PIC 9(01).
           03  FLG-RECE859         PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
      *
           03  FLG-YAKUKAN-COMMENT PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT2 PIC 9(01).
           03  FLG-WRKRECE85       PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-NYU-RENZOKU     PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-KYUSEI-OK       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-RIHA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-RYOCOM          PIC 9(01).
           03  FLG-RYOCOM2         PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-COMOKIKAE       PIC 9(01).
           03  FLG-COMOKIKAE2      PIC 9(01).
           03  FLG-HANTEI          PIC 9(01).
           03  FLG-COMARI          PIC 9(01).
           03  FLG-COMARI2         PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-KAISIEN         PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-NG              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE82          PIC 9(06).
           03  CNT-RECE84          PIC 9(06).
           03  CNT-RECE85          PIC 9(06).
           03  CNT-RECE849         PIC 9(06).
           03  CNT-RECE859         PIC 9(06).
           03  CNT-RIHA            PIC 9(06).
           03  CNT-IN-COMMIT       PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX12               PIC 9(04).
           03  IDX13               PIC 9(04).
           03  IDX15               PIC 9(01).
           03  IDX16               PIC 9(02).
           03  IDX17               PIC 9(02).
           03  IDX18               PIC 9(02).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  DAY-IDX3            PIC 9(01).
           03  IDX-RECE85          PIC 9(04).
           03  IDV                 PIC 9(04).
           03  RYO-IDX             PIC 9(03).
           03  RYO-IDX2            PIC 9(03).
           03  RYO-IDX3            PIC 9(03).
           03  IDW                 PIC 9(04).
           03  IDC1                PIC 9(02).
           03  IDC2                PIC 9(02).
           03  IDC3                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-RECEKBN            PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN            PIC  X(01).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-JYUNBAN         PIC 9(04).
           03  WRK-GYOSUU          PIC 9(04).
           03  WRK-GYOSUU2         PIC 9(04).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CNT-MEISAI      PIC 9(04).
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA-TAIHI      PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-FSANTEIYMD      PIC 9(08).
           03  WRK-AFTER2          PIC 9(08).
           03  WRK-AFTER180        PIC 9(08).
           03  WRK-TOSEKIYMD       PIC 9(08).
           03  WRK-KYUSEIYMD       PIC 9(08).
           03  WRK-HINKAIYMD       PIC 9(08).
           03  WRK-DAY             PIC 9(02).
      *
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-HENSYU-KAISU    PIC X(02).
           03  WRK-HENSYU-KAISU2   PIC X(04).
      *
           03  WRK-HENSYU-KASANRITU   PIC X(01).
           03  WRK-HENSYU-KASANRITU2  PIC X(02).
      *
           03  WRK-TUKI-YAKUKAN    PIC 9(02).
           03  WRK-DAY-YAKUKAN     PIC 9(02).
      *
           03  TBL-SANTEI-DAY.
               05  TB-SANTEI-DAY   PIC 9(02)   OCCURS  31.
      *
      *    該当診療月分
           03  TBL-RIHASANTEI-DAY.
               05  TB-RIHASANTEI-DAY PIC 9(01) OCCURS  31.
      *
      *    直近分
           03  WRK-RIHASANTEI-YMD.
               05  WRK-RIHASANTEI-YM PIC 9(06).
               05  WRK-RIHASANTEI-DD PIC 9(02).
      *
      *    直近分退避用
           03  WRK-RIHASANTEI-YMD2   PIC X(08).
      *
      *    前月以前の直近終了日
           03  WRK-RIHASANTEI-END    PIC X(08).
      *
      *    ２００６年４月１日に算定日があるか
           03  WRK-RIHASANTEI41-FLG  PIC 9(01).
      *
           03  TBL-NYU-DAY.
               05  TBL-NYU-DAY-OCC             OCCURS  31.
                 07  TBL-NYU-DAY-ST  PIC 9(02).
                 07  TBL-NYU-DAY-ED  PIC 9(02).
                 07  TBL-NYU-DAY-CD  PIC X(09).
      *
           03  WRK-ZENYYMM           PIC 9(06).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09).
           03  WRK-KEISAN4           PIC 9(09).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
           03  WRK-KYUSEI-NYUGAIKBN  PIC X(01).
           03  WRK-KYUSEI-SRYKA      PIC X(02).
           03  WRK-KYUSEI-ZAINUM     PIC 9(08).
      *
           03  WRK-HENYMD            PIC X(08).
           03  WRK-TAIHI-XX          PIC X(02).
      *
           03  WRK-OKIKAEFLG         PIC X(01).
      *
           03  WRK-RIHAIGAKU-SRYCD           PIC X(09).
           03  WRK-RIHAIGAKU-SRYCD2          PIC X(09).
           03  WRK-RIHAIGAKU-FSANTEIYMD      PIC X(08).
           03  WRK-RIHAIGAKU-SANTEI-AREA.
               05  WRK-RIHAIGAKU-SANTEI-FLG  PIC X(01).
               05  WRK-RIHAIGAKU-SANTEI-DAY  PIC 9(03)
                                             OCCURS 31.
      *
           03  WRK-MCP-TABLE         PIC X(64).
           03  WRK-MCP-PATHNAME      PIC X(64).
      *
           03  WRK-YAKUKAN-SRYCD     PIC X(09).
      *
           03  WRK-KAISIEN-FSANTEIYMD        PIC X(08).
           03  WRK-KAISIEN-YMD               PIC X(08).
           03  WRK-KAISIEN-CHECKYMD          PIC X(08).
           03  WRK-KAISIEN-RRKNUM            PIC 9(03).
           03  WRK-KAISIEN-RRKNUM-X          PIC 9(03).
           03  WRK-KAISIEN-RRKNUM-Y          PIC 9(03).
      *
       01  WRK-RECE84-REC. 
           COPY    "CPRCF084.INC"  REPLACING   //RECE84//
                                   BY          //WRK-RECE84//.
      *
       01  WRK-RECE85-REC. 
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //WRK-RECE85//.
      *
       01  WRK-RECE85XX-REC.
         02 WRK-RECE85XX-TBL       OCCURS 50.
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //WRK-RECE85XX//.
      *
       01  WRK-RECE85XX2-REC.
         02 WRK-RECE85XX2-TBL      OCCURS 300.
           COPY    "CPRCF085.INC"  REPLACING   //RECE85//
                                   BY          //WRK-RECE85XX2//.
      *
       01  WRK-FREE-AREA.
           03  WRK-FREEINF                     OCCURS 49.         
               05  WK-FREE-YUKOKETA          PIC  9(03).
               05  WK-FREE-NAME              PIC  X(200).
      *
       01  WRK-FREE-AREA2.                                              
           03  WRK-FREEINF2                    OCCURS 50.               
               05  WK-FREE-YUKOKETA2         PIC  9(03).                
               05  WK-FREE-NAME2             PIC  X(200).               
      *
       01  WRK-FREE-AREA3.
           03  WRK-FREEINF3.
               05  WK-FREE-NAME3             PIC  X(1000).
               05  WK-FREE-NAME3-TAIHI       PIC  X(1000).
      *
      *    日付全角変換領域
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "　１".
           03  FILLER              PIC X(04)  VALUE "　２".
           03  FILLER              PIC X(04)  VALUE "　３".
           03  FILLER              PIC X(04)  VALUE "　４".
           03  FILLER              PIC X(04)  VALUE "　５".
           03  FILLER              PIC X(04)  VALUE "　６".
           03  FILLER              PIC X(04)  VALUE "　７".
           03  FILLER              PIC X(04)  VALUE "　８".
           03  FILLER              PIC X(04)  VALUE "　９".
           03  FILLER              PIC X(04)  VALUE "１０".
           03  FILLER              PIC X(04)  VALUE "１１".
           03  FILLER              PIC X(04)  VALUE "１２".
           03  FILLER              PIC X(04)  VALUE "１３".
           03  FILLER              PIC X(04)  VALUE "１４".
           03  FILLER              PIC X(04)  VALUE "１５".
           03  FILLER              PIC X(04)  VALUE "１６".
           03  FILLER              PIC X(04)  VALUE "１７".
           03  FILLER              PIC X(04)  VALUE "１８".
           03  FILLER              PIC X(04)  VALUE "１９".
           03  FILLER              PIC X(04)  VALUE "２０".
           03  FILLER              PIC X(04)  VALUE "２１".
           03  FILLER              PIC X(04)  VALUE "２２".
           03  FILLER              PIC X(04)  VALUE "２３".
           03  FILLER              PIC X(04)  VALUE "２４".
           03  FILLER              PIC X(04)  VALUE "２５".
           03  FILLER              PIC X(04)  VALUE "２６".
           03  FILLER              PIC X(04)  VALUE "２７".
           03  FILLER              PIC X(04)  VALUE "２８".
           03  FILLER              PIC X(04)  VALUE "２９".
           03  FILLER              PIC X(04)  VALUE "３０".
           03  FILLER              PIC X(04)  VALUE "３１".
       01  TBL-DAY-AREA-R          REDEFINES   TBL-DAY-AREA.
           03  TBL-DAY             PIC X(04)   OCCURS   31.
      *
       01  MAX-MEISAISU            PIC 9(07).
      *
      *    固定値
       01  CONST-AREA.
      *    薬剤管理指導料
           03  CONS-YAKUKAN-COMMENT  PIC X(09) VALUE "120001610".
      *    算定日コメント
           03  CONS-SANTEI-COMMENT1  PIC X(09) VALUE "840000058".
           03  CONS-SANTEI-COMMENT2  PIC X(09) VALUE "840000065".
      *    在宅血液透析指導管理料
           03  CONS-TOSEKI           PIC X(09) VALUE "114009310".
      *    脳血管疾患急性発症日
           03  CONS-KYUSEI           PIC X(09) VALUE "099800101".
      *    心大リハ開始日
           03  CONS-SINDAI          PIC X(09) VALUE "099800111".
      *    呼吸器リハ開始日
           03  CONS-KOKYUKI         PIC X(09) VALUE "099800141".
      *    脳血管疾患等リハ開始日
           03  CONS-NOUKE           PIC X(09) VALUE "099800121".
      *    運動器リハ開始日
           03  CONS-UNDOKI          PIC X(09) VALUE "099800131".
      *    難病患者リハ開始日
           03  CONS-NANBYO          PIC X(09) VALUE "099800161".
      *    障害児（者）リハ開始日
           03  CONS-SYOGAI          PIC X(09) VALUE "099800171".
      *    摂食療法開始日
           03  CONS-SESSYOKU        PIC X(09) VALUE "099800151".
      *
      *    心大リハ終了日
           03  CONS-SINDAI-E        PIC X(09) VALUE "099800112".
      *    呼吸器リハ終了日
           03  CONS-KOKYUKI-E       PIC X(09) VALUE "099800142".
      *    脳血管疾患等リハ終了日
           03  CONS-NOUKE-E         PIC X(09) VALUE "099800122".
      *    運動器リハ終了日
           03  CONS-UNDOKI-E        PIC X(09) VALUE "099800132".
      *    難病患者リハ終了日
           03  CONS-NANBYO-E        PIC X(09) VALUE "099800162".
      *    障害児（者）リハ終了日
           03  CONS-SYOGAI-E        PIC X(09) VALUE "099800172".
      *    摂食療法終了日
           03  CONS-SESSYOKU-E      PIC X(09) VALUE "099800152".
      *
      *    心大血管疾患リハビリテーション医学管理料（１）
           03  CONS-SINDAI-IGAKU1   PIC X(09) VALUE "180029910".
      *    心大血管疾患リハビリテーション医学管理料（２）
           03  CONS-SINDAI-IGAKU2   PIC X(09) VALUE "180030010".
      *    呼吸器リハビリテーション医学管理料（１）
           03  CONS-KOKYUKI-IGAKU1  PIC X(09) VALUE "180030510".
      *    呼吸器リハビリテーション医学管理料（２）
           03  CONS-KOKYUKI-IGAKU2  PIC X(09) VALUE "180030610".
      *    脳血管疾患等リハビリテーション医学管理料（１）
           03  CONS-NOUKE-IGAKU1    PIC X(09) VALUE "180030110".
      *    脳血管疾患等リハビリテーション医学管理料（２）
           03  CONS-NOUKE-IGAKU2    PIC X(09) VALUE "180030210".
      *    運動器リハビリテーション医学管理料（１）
           03  CONS-UNDOKI-IGAKU1   PIC X(09) VALUE "180030310".
      *    運動器リハビリテーション医学管理料（２）
           03  CONS-UNDOKI-IGAKU2   PIC X(09) VALUE "180030410".
      *
      *    心大血管疾患リハ実施
           03  CONS-SINDAI-DUMMY    PIC X(09) VALUE "099800311".
      *    呼吸器リハ実施
           03  CONS-KOKYUKI-DUMMY   PIC X(09) VALUE "099800341".
      *    脳血管疾患リハ実施
           03  CONS-NOUKE-DUMMY     PIC X(09) VALUE "099800321".
      *    運動器リハ実施
           03  CONS-UNDOKI-DUMMY    PIC X(09) VALUE "099800331".
      *
      *    介護支援連携指導料
           03  CONS-KAISIEN         PIC X(09) VALUE "113011710".
      *
      *    薬剤管理指導料テーブル（２００８年４月診療分〜）
       01  TBL-YAKUKAN-AREA.
           03  FILLER               PIC X(09) VALUE "113010710".
           03  FILLER               PIC X(09) VALUE "113010810".
           03  FILLER               PIC X(09) VALUE "120001610".
      *
       01  TBL-YAKUKAN-R       REDEFINES   TBL-YAKUKAN-AREA.
            03  TBL-YAKUKAN    PIC X(09)   OCCURS   3.
      *
       01  TBL-YAKUKAN-MAX     PIC 9(02)   VALUE    3.
      *
      *
       01  TBL-HASSYO-INF.
           03  TBL-HASSYO-KAITATU-COMMENT   PIC 9(01).
           03  TBL-HASSYO-SINDAI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-COMMENT   PIC 9(01).
           03  TBL-HASSYO-NOUKE-COMMENT     PIC 9(01).
           03  TBL-HASSYO-UNDOKI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-NANBYO-COMMENT    PIC 9(01).
           03  TBL-HASSYO-SYOGAI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-COMMENT  PIC 9(01).
      *
       01  TBL-ZAISIEN-INF.
           03  TBL-ZAISIEN-COMMENT          PIC 9(01).
      *
      *    酸素（金額・数量）情報
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN          PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO     PIC 9(05)V9(05).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO     PIC 9(05)V9(05).
      *
      *    療養レセコメント情報
       01  TBL-RYOCOM-INF.
           03  TBL-RYOCOM-INF2                 OCCURS  31.
               05  TBL-RYOCOM-NYUINKBN         PIC  X(01).
               05  TBL-RYOCOM-AREA.
                 06  TBL-RYOCOM-MEISAISU       PIC  9(03).
                 06  TBL-RYOCOM-SRYCDTOTAL     PIC  9(14).
                 06  TBL-RYOCOM-INF3           OCCURS  30.
                   07  TBL-RYOCOM-IRYO-SRYCD   PIC  X(09).
                 06  TBL-RYOCOM-ADL-SRYCD      PIC  X(09).
                 06  TBL-RYOCOM-ADL-TEN        PIC  9(02).
                 06  TBL-RYOCOM-ADL-TEN-MOJI   PIC  X(04).
                 06  TBL-RYOCOM-INF4           OCCURS   4.
                   07  TBL-RYOCOM-ADL-SRYCD-H22      PIC  X(09).
                   07  TBL-RYOCOM-ADL-TEN-H22        PIC  9(02).
                   07  TBL-RYOCOM-ADL-TEN-MOJI-H22   PIC  X(04).
      *
      *    療養レセコメント情報（該当分のまとめ）
       01  TBL-RYOCOM2-INF.
           03  TBL-RYOCOM2-INF2                OCCURS  31.
               05  TBL-RYOCOM2-AREA.
                 06  TBL-RYOCOM2-MEISAISU      PIC  9(03).
                 06  TBL-RYOCOM2-SRYCDTOTAL    PIC  9(14).
                 06  TBL-RYOCOM2-INF3          OCCURS  30.
                   07  TBL-RYOCOM2-IRYO-SRYCD  PIC  X(09).
                 06  TBL-RYOCOM2-ADL-SRYCD     PIC  X(09).
                 06  TBL-RYOCOM2-ADL-TEN       PIC  9(02).
                 06  TBL-RYOCOM2-ADL-TEN-MOJI  PIC  X(04).
                 06  TBL-RYOCOM2-INF4          OCCURS   4.
                   07  TBL-RYOCOM2-ADL-SRYCD-H22     PIC  X(09).
                   07  TBL-RYOCOM2-ADL-TEN-H22       PIC  9(02).
                   07  TBL-RYOCOM2-ADL-TEN-MOJI-H22  PIC  X(04).
               05  TBL-RYOCOM2-INF4            OCCURS  31.
                 06  TBL-RYOCOM2-DAY           PIC  9(01).
      *
      *    酸素
           COPY    "CMSANSOTBL.INC".
      *
           COPY    "MCPAREA".
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
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    算定履歴付加
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    療養レセコメント取得サブ
           COPY    "CPORCSRYOCOM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END  =  1
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0870"          TO  JOB-PGID
               MOVE    "コメント編集（労災・自賠責）"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           PERFORM 990-RECE82-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
      *
           MOVE    "RECE84W"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE84PARA-HOSPNUM
      *
           MOVE    "RECE850"           TO  RECE85PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE85PARA-HOSPNUM
      *
           MOVE    "RECE84A"           TO  RECE841PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE841PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE841PARA-HOSPNUM
      *
           MOVE    "RECE85A"           TO  RECE851PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE851PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE851PARA-HOSPNUM
      *
           MOVE    "RECE84Y"           TO  RECE849PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE849PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE849PARA-HOSPNUM
      *
           MOVE    "RECE85Y"           TO  RECE859PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE859PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE859PARA-HOSPNUM
      *
           OPEN    INPUT               RECE82-FILE
      *
           OPEN    INPUT               RECE84-FILE
      *
           OPEN    INPUT               RECE85-FILE
      *
           OPEN    OUTPUT              RECE8441-FILE
           CLOSE                       RECE8441-FILE
      *
           OPEN    OUTPUT              RECE8551-FILE
           CLOSE                       RECE8551-FILE
      *
           OPEN    OUTPUT              RECE8449-FILE
           CLOSE                       RECE8449-FILE
      *
           OPEN    OUTPUT              RECE8559-FILE
           CLOSE                       RECE8559-FILE
      *
           OPEN    OUTPUT              RECE841-FILE
      *
           OPEN    OUTPUT              RECE851-FILE
      *
           OPEN    I-O                 RECE849-FILE
      *
           OPEN    I-O                 RECE859-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                        TBL-HASSYO-INF
           INITIALIZE                        TBL-ZAISIEN-INF
           INITIALIZE                        TBL-RYOCOM-INF
      *
      *    療養レセコメント取得
      *
           MOVE      ZERO     TO      FLG-RYOCOM
      *
           IF    RECE82-SRYYM   >=  "200610"
             INITIALIZE                   ORCSRYOCOM-AREA
             MOVE    RECE82-HOSPNUM   TO  LNK-RYOCOM-HOSPNUM
             MOVE    RECE82-PTID      TO  LNK-RYOCOM-PTID
             MOVE    RECE82-SRYYM     TO  LNK-RYOCOM-SANTEIYM
             CALL    "ORCSRYOCOM"         USING
                                          ORCSRYOCOM-AREA
                                          SPA-AREA
             IF      LNK-RYOCOM-RC    =   ZERO
               MOVE       1           TO  FLG-RYOCOM
               PERFORM  VARYING  RYO-IDX  FROM  1  BY  1
                         UNTIL   RYO-IDX   >   31
                 MOVE  LNK-RYOCOM-NYUINKBN(RYO-IDX)
                                 TO  TBL-RYOCOM-NYUINKBN(RYO-IDX)
                 MOVE  LNK-RYOCOM-MEISAISU(RYO-IDX)
                                 TO  TBL-RYOCOM-MEISAISU(RYO-IDX)
                 MOVE  LNK-RYOCOM-SRYCDTOTAL(RYO-IDX)
                                 TO  TBL-RYOCOM-SRYCDTOTAL(RYO-IDX)
                 PERFORM  VARYING  RYO-IDX2  FROM  1  BY  1
                           UNTIL   RYO-IDX2   >   30
                   MOVE  LNK-RYOCOM-IRYO-SRYCD(RYO-IDX RYO-IDX2)
                           TO  TBL-RYOCOM-IRYO-SRYCD(RYO-IDX RYO-IDX2)
                 END-PERFORM
                 MOVE  LNK-RYOCOM-ADL-SRYCD(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-SRYCD(RYO-IDX)
                 MOVE  LNK-RYOCOM-ADL-TEN(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-TEN(RYO-IDX)
                 MOVE  LNK-RYOCOM-ADL-TEN-MOJI(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-TEN-MOJI(RYO-IDX)
                 PERFORM  VARYING  RYO-IDX2  FROM  1  BY  1
                           UNTIL   RYO-IDX2   >    4
                   MOVE  LNK-RYOCOM-ADL-SRYCD-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-SRYCD-H22(RYO-IDX RYO-IDX2)
                   MOVE  LNK-RYOCOM-ADL-TEN-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-TEN-H22(RYO-IDX RYO-IDX2)
                   MOVE  LNK-RYOCOM-ADL-TEN-MOJI-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-TEN-MOJI-H22(RYO-IDX RYO-IDX2)
                 END-PERFORM
               END-PERFORM
             END-IF
           END-IF
      *
           IF   RECE82-COMCNTFLG   =   "1"
             PERFORM 800-RECE82-SET84-SEC
             MOVE    ZERO                TO  RECE84-ZAINUM
      *
             START   RECE84-FILE
                 KEY IS    >=   RECE84-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE84
                 NOT INVALID
                     PERFORM 990-RECE84-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE84           =   1                   )
               OR     ( RECE82-HOSPNUM   NOT =   RECE84-HOSPNUM      )
               OR     ( RECE82-SRYYM     NOT =   RECE84-SRYYM        )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE84-NYUGAIKBN    )
               OR     ( RECE82-PTID      NOT =   RECE84-PTID         )
               OR     ( RECE82-RECEKA    NOT =   RECE84-RECEKA       )
               OR     ( RECE82-HKNID     NOT =   RECE84-HKNID        )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE84-TEKSTYMD     )
      *
               IF   RECE82-HKNKBN    NOT  =  "4"                        
                  EVALUATE      RECE84-SRYKBN
                      WHEN      "14"
      *               （在支援に関する診療のカウント）
                         IF  RECE84-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                            MOVE  RECE84-REC  TO  WRK-RECE84-REC
                            EVALUATE  RECE84-FLG-ZAISIEN-COMMENT
                              WHEN      "1"
                                MOVE  1  TO  TBL-ZAISIEN-COMMENT
                              WHEN      "2"
                                MOVE  2  TO  TBL-ZAISIEN-COMMENT
                            END-EVALUATE
                         END-IF
                      WHEN      "40"
      *               （介達牽引あり）
                         IF  RECE84-KAITATU-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KAITATU-COMMENT
                         END-IF
                      WHEN      "80"
      *               （リハビリあり）
                         IF  RECE84-SINDAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SINDAI-COMMENT
                         END-IF
                         IF  RECE84-KOKYUKI-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KOKYUKI-COMMENT
                         END-IF
                         IF  RECE84-NOUKE-COMMENT        =  "1"
                            MOVE   1   TO  TBL-HASSYO-NOUKE-COMMENT
                         END-IF
                         IF  RECE84-UNDOKI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-UNDOKI-COMMENT
                         END-IF
                         IF  RECE84-NANBYO-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-NANBYO-COMMENT
                         END-IF
                         IF  RECE84-SYOGAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SYOGAI-COMMENT
                         END-IF
                         IF  RECE84-SESSYOKU-COMMENT     =  "1"
                            MOVE   1   TO  TBL-HASSYO-SESSYOKU-COMMENT
                         END-IF
                  END-EVALUATE
               ELSE                                                     
                  EVALUATE      RECE84-SRYKBN
                      WHEN      "40"
                      WHEN      "80"
      *               （介達牽引あり）
                         IF  RECE84-KAITATU-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KAITATU-COMMENT
                         END-IF
      *               （リハビリあり）
                         IF  RECE84-SINDAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SINDAI-COMMENT
                         END-IF
                         IF  RECE84-KOKYUKI-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KOKYUKI-COMMENT
                         END-IF
                         IF  RECE84-NOUKE-COMMENT        =  "1"
                            MOVE   1   TO  TBL-HASSYO-NOUKE-COMMENT
                         END-IF
                         IF  RECE84-UNDOKI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-UNDOKI-COMMENT
                         END-IF
                         IF  RECE84-NANBYO-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-NANBYO-COMMENT
                         END-IF
                         IF  RECE84-SYOGAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SYOGAI-COMMENT
                         END-IF
                         IF  RECE84-SESSYOKU-COMMENT     =  "1"
                            MOVE   1   TO  TBL-HASSYO-SESSYOKU-COMMENT
                         END-IF
      *               （在支援に関する診療のカウント）
                         IF  RECE84-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                            MOVE  RECE84-REC  TO  WRK-RECE84-REC
                            EVALUATE  RECE84-FLG-ZAISIEN-COMMENT
                              WHEN      "1"
                                MOVE  1  TO  TBL-ZAISIEN-COMMENT
                              WHEN      "2"
                                MOVE  2  TO  TBL-ZAISIEN-COMMENT
                            END-EVALUATE
                         END-IF
                  END-EVALUATE                                          
               END-IF                                                   
               PERFORM 990-RECE84-NEXT-SEC
             END-PERFORM
           END-IF
      *
      *    他保険にて入院料算定済みのコメント
      *
           IF   RECE82-NYUINRYOFLG   NOT =   SPACE
      *
              PERFORM   2001-NYUINRYO-COMMENT-SEC
      *
           END-IF
      *
      *    発症日のコメント
           IF   RECE82-SRYYM      >=    "200604"
               PERFORM   2001-HASSYO-HANTEI-SEC
           ELSE
               MOVE      ZERO            TO   FLG-COMOKIKAE
               MOVE      CONS-KYUSEI     TO   WRK-SRYCD
               PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      *
      *    自賠責従来様式：加算率 
           IF   RECE82-HKNKBN     =   "4"
           AND  WRK-PARA-RECEKBN  =   "5"
             EVALUATE  RECE82-JIBAIKBN
      *        加算率
               WHEN     "1"
                  PERFORM    2001-KASANRITU-COMMENT-SEC
      *        薬剤等点単価
               WHEN     "2"
                  IF    RECE82-SYUTENTANKA  =  RECE82-YKZTENTANKA
                     CONTINUE
                  ELSE
                     PERFORM    2001-YKZTENTANKA-COMMENT-SEC
                  END-IF
             END-EVALUATE
           END-IF
      *
      *    在支援コメント
           IF     TBL-ZAISIEN-COMMENT   NOT =  ZERO
                PERFORM    2001-ZAISIEN-COMMENT-SEC
           END-IF
      *
      *    特別のコメント（特別な関係にある他院）
      *
           IF     RECE82-TAINFLG            =  "1"
                PERFORM    2001-TAIN-TOKUBETU-COMMENT-SEC
           END-IF
      *
           PERFORM 800-RECE82-SET84-SEC
           MOVE    ZERO                TO  RECE84-ZAINUM
      *
           START   RECE84-FILE
               KEY IS    >=   RECE84-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE84
               NOT INVALID
                   PERFORM 990-RECE84-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE84           =   1                   )
               OR     ( RECE82-HOSPNUM   NOT =   RECE84-HOSPNUM      )
               OR     ( RECE82-SRYYM     NOT =   RECE84-SRYYM        )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE84-NYUGAIKBN    )
               OR     ( RECE82-PTID      NOT =   RECE84-PTID         )
               OR     ( RECE82-RECEKA    NOT =   RECE84-RECEKA       )
               OR     ( RECE82-HKNID     NOT =   RECE84-HKNID        )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE84-TEKSTYMD     )
      *
               MOVE    ZERO   TO   FLG-KAISIEN
      *
               IF   RECE82-HKNKBN    NOT  =  "4"                        
                 EVALUATE      RECE84-SRYKBN                   
                   WHEN      "11"
                      IF   RECE82-HKNKBN     =   "5"
                      AND  RECE82-KOMUKBN    =   "2"
                          MOVE   ZERO   TO  FLG-COMMENT
      *                （初診（同日複数診療科）算定科のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-SYODOU-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2099-RECE84-WRITE-SEC
                          END-IF
                      ELSE
                            PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "13"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （薬剤管理指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-YAKUKANSIDO-HANTEI-SEC
                      END-IF
      *            （入院栄養食事指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUEIYOSIDO-HANTEI-SEC
                      END-IF
      *            （退院前訪問指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-HANTEI-SEC
                      END-IF
      *            （傷病手当金意見書交付料交付日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
      *            （療養費同意書交付料交付日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
      *            （退院時服薬指導加算算定時の
      *              前月薬剤管理指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TAIIN-HANTEI-SEC
                      END-IF
      *            （介護支援連携指導料の算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KAISIEN-HANTEI-SEC
                      END-IF
      *            （集団栄養食事指導料の算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYUEIYO-HANTEI-SEC
                      END-IF
      *
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "14"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （訪問看護指示料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUSIJI-HANTEI-SEC
                      END-IF
      *            （在宅血液透析頻回指導管理料算定時の
      *              在宅血液透析指導管理料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HINKAI-HANTEI-SEC
                      END-IF
      *            （往診・特別往診算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-OSIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "40"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （人工腎臓等の算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINZOUETC-HANTEI-SEC
                      END-IF
      *            （局所陰圧閉鎖処置の算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KYOKUIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "50"
      *            （手術日のセット）
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "54"
      *            （麻酔日のセット）
                      PERFORM    2001-MASUI-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （（検選）日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSEN-HANTEI-SEC
                      END-IF
      *            （実施日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "70"
      *            （ＣＴ・ＭＲＩ実施日のセット）
                      PERFORM    2001-CTMRI-HANTEI-SEC
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （精神科退院前訪問指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-SEISIN-HANTEI-SEC
                      END-IF
      *            （リハビリのセット）
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE84-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE84-RIHABIRI-COMMENT      =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                          ELSE
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHAIGAKU-COMMENT-SEC
                          END-IF
                        END-IF
                      END-IF
      *            （（リハ選）日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-RIHASEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "90"                                       
                   WHEN      "92"
                      EVALUATE    RECE84-ZAISKBKBN
                        WHEN   "2"
                          PERFORM   2001-NYU-COMMENT-SEC
                        WHEN   "6"
                          MOVE   ZERO   TO  FLG-COMMENT
      *                  （精神科隔離病室管理加算の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2001-KAKURI-HANTEI-SEC
                          END-IF
      *                  （労災治療計画加算の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2001-CHIRYO-HANTEI-SEC
                          END-IF
      *                  （栄養サポートチーム加算の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-EISAPO-HANTEI-SEC
                          END-IF
      *                  （呼吸ケアチーム加算の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-KOKKEA-HANTEI-SEC
                          END-IF
      *                  （重症者加算（精神療養病棟入院料）の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-JYUSYO-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2099-RECE84-WRITE-SEC
                          END-IF
                        WHEN   OTHER
                         PERFORM    2099-RECE84-WRITE-SEC
                      END-EVALUATE
                   WHEN   OTHER
                         PERFORM    2099-RECE84-WRITE-SEC
                 END-EVALUATE
               ELSE                                                     
                 EVALUATE      RECE84-SRYKBN                            
                   WHEN      "11"
                      EVALUATE   RECE82-JIBAIKBN
                        WHEN       "1"
                          PERFORM      2099-RECE84-WRITE-SEC
                        WHEN       "2"
                          MOVE   ZERO   TO  FLG-COMMENT
      *                （初診（同日複数診療科）算定科のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-SYODOU-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2099-RECE84-WRITE-SEC
                          END-IF
                      END-EVALUATE
                   WHEN      "13"                                       
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （薬剤管理指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-YAKUKANSIDO-HANTEI-SEC
                      END-IF
      *            （入院栄養食事指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUEIYOSIDO-HANTEI-SEC
                      END-IF
      *            （退院前訪問指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-HANTEI-SEC
                      END-IF
      *            （傷病手当金意見書交付料交付日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
      *            （療養費同意書交付料交付日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
      *            （退院時服薬指導加算算定時の
      *              前月薬剤管理指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TAIIN-HANTEI-SEC
                      END-IF
      *            （介護支援連携指導料の算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KAISIEN-HANTEI-SEC
                      END-IF
      *            （集団栄養食事指導料の算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYUEIYO-HANTEI-SEC
                      END-IF
      *
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "40"          
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （精神科退院前訪問指導料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-SEISIN-HANTEI-SEC
                      END-IF
      *            （リハビリのセット）
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE84-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE84-RIHABIRI-COMMENT      =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                          ELSE
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHAIGAKU-COMMENT-SEC
                          END-IF
                        END-IF
                      END-IF
      *            （（リハ選）日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-RIHASEN-HANTEI-SEC
                      END-IF
      *
      *            （訪問看護指示料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-HOUSIJI-HANTEI-SEC
                      END-IF
      *            （在宅血液透析頻回指導管理料算定時の
      *              在宅血液透析指導管理料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-HINKAI-HANTEI-SEC
                      END-IF
      *            （往診・特別往診算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-OSIN-HANTEI-SEC
                      END-IF
      *            （人工腎臓等の算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-JINZOUETC-HANTEI-SEC
                      END-IF
      *            （局所陰圧閉鎖処置の算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2001-KYOKUIN-HANTEI-SEC
                      END-IF
      *
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM       2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "50"                                       
      *            （手術日のセット）                                   
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC               
                   WHEN      "54"
      *            （麻酔日のセット）
                      PERFORM    2001-MASUI-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （（検選）日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSEN-HANTEI-SEC
                      END-IF
      *            （実施日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE84-WRITE-SEC
                      END-IF
                   WHEN      "70"
      *            （ＣＴ・ＭＲＩ実施日のセット）
                      PERFORM    2001-CTMRI-HANTEI-SEC
                   WHEN      "90"                                       
                   WHEN      "92"
                      EVALUATE    RECE84-ZAISKBKBN
                        WHEN   "2"
                          PERFORM   2001-NYU-COMMENT-SEC
                        WHEN   "6"
                          MOVE   ZERO   TO  FLG-COMMENT
      *                  （精神科隔離病室管理加算の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2001-KAKURI-HANTEI-SEC
                          END-IF
      *                  （労災治療計画加算の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2001-CHIRYO-HANTEI-SEC
                          END-IF
      *                  （栄養サポートチーム加算の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-EISAPO-HANTEI-SEC
                          END-IF
      *                  （呼吸ケアチーム加算の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-KOKKEA-HANTEI-SEC
                          END-IF
      *                  （重症者加算（精神療養病棟入院料）の算定日のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-JYUSYO-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                             PERFORM   2099-RECE84-WRITE-SEC
                          END-IF
                        WHEN   OTHER
                          PERFORM   2099-RECE84-WRITE-SEC
                      END-EVALUATE
                   WHEN   OTHER                                         
                         PERFORM    2099-RECE84-WRITE-SEC
                 END-EVALUATE                                           
               END-IF                                                   
      *
               PERFORM 990-RECE84-NEXT-SEC
      *
           END-PERFORM
      *
      *****差込編集２********************
           PERFORM     250-SASHIKOMI2-SEC
      ***********************************
      *
           PERFORM 990-RECE82-READ-SEC
      *
           IF  CNT-IN-COMMIT   =    300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
           END-IF
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    他保険にて入院料算定済みのコメント編集
      *****************************************************************
       2001-NYUINRYO-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE82-SET849-SEC
           MOVE    99999999            TO  RECE849-ZAINUM
           MOVE    "90"                TO  RECE849-SRYKBN
      *
           MOVE    1                   TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           PERFORM 800-RECE82-SET859-SEC
           MOVE    99999999            TO  RECE859-ZAINUM
           MOVE    "90"                TO  RECE859-SRYKBN
      *    コメントの挿入
           MOVE    SPACE               TO  WRK-RECE85XX-REC
           INITIALIZE                      WRK-RECE85XX-REC
           MOVE    "810000001"         TO  WRK-RECE85XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE85XX-SRYSURYO(1 1)
           EVALUATE   RECE82-NYUINRYOFLG
             WHEN     "1"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "自費にて入院料算定済" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "2"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "労災にて入院料算定済" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "3"
               MOVE     11             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "自賠責にて入院料算定済" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "4"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "国保にて入院料算定済" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "5"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "社保にて入院料算定済" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "6"
               MOVE     11             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "他保険にて入院料算定済" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "7"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "公害にて入院料算定済" 
                                       TO  WRK-RECE85XX-NAME(1 1)
             WHEN     "8"
               MOVE     10             TO  WRK-RECE85XX-YUKOKETA(1 1)
               MOVE    "後保にて入院料算定済" 
                                       TO  WRK-RECE85XX-NAME(1 1)
           END-EVALUATE
           MOVE     "1"                TO  WRK-RECE85XX-SIKFLG(1 1)
      *
           MOVE    WRK-RECE85XX-SRYACT-TBL(1 1)
                                       TO  RECE859-SRYACT-TBL(1)
           WRITE   RECE859-REC
           ADD     1                   TO  CNT-RECE859
      *
           .
       2001-NYUINRYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日があるか判定
      *****************************************************************
       2001-HASSYO-HANTEI-SEC          SECTION.
      *
           IF   TBL-HASSYO-KAITATU-COMMENT   =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-KYUSEI      TO   WRK-SRYCD
                PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SINDAI-COMMENT    =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-SINDAI      TO   WRK-SRYCD
                MOVE      CONS-SINDAI-E    TO   WRK-SRYCD2
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-KOKYUKI-COMMENT   =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-KOKYUKI     TO   WRK-SRYCD
                MOVE      CONS-KOKYUKI-E   TO   WRK-SRYCD2
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-NOUKE-COMMENT     =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-NOUKE       TO   WRK-SRYCD
                MOVE      CONS-NOUKE-E     TO   WRK-SRYCD2
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-UNDOKI-COMMENT    =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-UNDOKI      TO   WRK-SRYCD
                MOVE      CONS-UNDOKI-E    TO   WRK-SRYCD2
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-NANBYO-COMMENT    =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-NANBYO      TO   WRK-SRYCD
                MOVE      CONS-NANBYO-E    TO   WRK-SRYCD2
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SYOGAI-COMMENT    =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-SYOGAI      TO   WRK-SRYCD
                MOVE      CONS-SYOGAI-E    TO   WRK-SRYCD2
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SESSYOKU-COMMENT  =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-SESSYOKU    TO   WRK-SRYCD
                MOVE      CONS-SESSYOKU-E  TO   WRK-SRYCD2
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      *
           .
       2001-HASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日があるか判定
      *    脳血管疾患急性発症日の１８０日以内か判定
      *****************************************************************
       2001-KYUSEIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     WRK-KYUSEIYMD
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE82-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE82-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE82-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"   TO  MCP-TABLE
                MOVE    "key3"         TO  MCP-PATHNAME
                PERFORM  900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                 ( FLG-KYUSEI-COMMENT =   1 )
      *
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3               <    1 )
                               OR    ( FLG-KYUSEI-COMMENT =    1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                         MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                         MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                         INITIALIZE                  STS-AREA-DAY
                         INITIALIZE                  LNK-DAY6-AREA
                         MOVE    "61"            TO  LNK-DAY6-IRAI
                         MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                         MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                         MOVE     179            TO  LNK-DAY6-ZOGEN
                         CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                         LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER180
      *
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE82-SRYYM
                         AND  RECE82-SRYYM        <=  WRK-AFTER180(1:6)
                               MOVE    1    TO   FLG-KYUSEI-COMMENT
                               MOVE  WRK-FSANTEIYMD
                                            TO   WRK-KYUSEIYMD
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_santei"   TO  MCP-TABLE
                   MOVE    "key3"         TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-KYUSEI-COMMENT    =      1
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE82-SRYYM      >=    "200604"
                   MOVE       WRK-KYUSEIYMD(7:2)  TO  IDX16
                   PERFORM    20011-HASSYO-COMMENT-SEC
               ELSE
                   PERFORM    20011-KYUSEIHASSYO-COMMENT-SEC
               END-IF
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日等の検索
      *****************************************************************
       20011-KYUSEIHASSYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-KYUSEI-OK
           MOVE    SPACE                 TO  WRK-KYUSEI-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-KYUSEI-SRYKA
           MOVE    ZERO                  TO  WRK-KYUSEI-ZAINUM
           MOVE    ZERO                  TO  FLG-COMOKIKAE2
           MOVE    ZERO                  TO  FLG-COMARI
           MOVE    ZERO                  TO  FLG-COMARI2
      *
           MOVE    ZERO                  TO  IDZ
           INITIALIZE                        WRK-FREE-AREA
      *++++
           PERFORM   VARYING   IDX15    FROM   1   BY   1
                      UNTIL   (IDX15          >    2)
                       OR     (FLG-KYUSEI-OK  =    1)
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE82-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    IDX15                 TO  SRY-NYUGAIKBN
           MOVE    RECE82-PTID           TO  SRY-PTID
           MOVE    WRK-KYUSEIYMD(1:6)    TO  SRY-SRYYM
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (1)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (2)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (3)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (4)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key7"                TO  MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"      TO  MCP-TABLE
               MOVE    "key7"            TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-KYUSEI-OK  =   1
                 INITIALIZE                     SRYACCT-REC
                 MOVE    SRY-HOSPNUM      TO    ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN    TO    ACCT-NYUGAIKBN
                 MOVE    SRY-PTID         TO    ACCT-PTID
                 MOVE    SRY-SRYKA        TO    ACCT-SRYKA
                 MOVE    SRY-SRYYM        TO    ACCT-SRYYM
                 MOVE    SRY-ZAINUM       TO    ACCT-ZAINUM
                 MOVE    SRYACCT-REC      TO    MCPDATA-REC
                 MOVE    "tbl_sryacct"    TO    MCP-TABLE
                 MOVE    "key31"          TO    MCP-PATHNAME
grpsys           PERFORM  900-DBSELECT-SEC
                 IF     MCP-RC            =  ZERO
                    MOVE    "tbl_sryacct" TO    MCP-TABLE
                    MOVE    "key31"       TO    MCP-PATHNAME
grpsys              PERFORM  900-DBFETCH-SEC
                    IF      MCP-RC        =       ZERO
                        MOVE    MCPDATA-REC  TO  SRYACCT-REC
      *
                        MOVE    WRK-KYUSEIYMD(7:2)  TO  DAY-IDX2
                        IF  ( ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO )
                        AND ( ACCT-HKNCOMBI  =  RECE82-HKNCOMBI )
                            MOVE   1   TO  FLG-KYUSEI-OK
                            MOVE   ACCT-NYUGAIKBN
                                       TO  WRK-KYUSEI-NYUGAIKBN
                            MOVE   ACCT-SRYKA
                                       TO  WRK-KYUSEI-SRYKA
                            MOVE   ACCT-ZAINUM
                                       TO  WRK-KYUSEI-ZAINUM
                        END-IF
                    END-IF
                 END-IF
                 MOVE    "tbl_sryacct"    TO   MCP-TABLE
                 MOVE    "key31"          TO   MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"     TO   MCP-TABLE
                 MOVE    "key7"           TO   MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           END-PERFORM
      *++++
           IF      WRK-KYUSEI-ZAINUM    NOT =    ZERO
              INITIALIZE                           SRYACT-REC
              MOVE    RECE82-HOSPNUM         TO    SRY-HOSPNUM
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO    SRY-NYUGAIKBN
              MOVE    RECE82-PTID            TO    SRY-PTID
              MOVE    WRK-KYUSEI-SRYKA       TO    SRY-SRYKA
              MOVE    WRK-KYUSEIYMD(1:6)     TO    SRY-SRYYM
              MOVE    WRK-KYUSEI-ZAINUM      TO    SRY-ZAINUM
              MOVE    SRYACT-REC             TO    MCPDATA-REC
              MOVE    "tbl_sryact"           TO    MCP-TABLE
              MOVE    "key2"                 TO    MCP-PATHNAME
grpsys        PERFORM  900-DBSELECT-SEC
              IF     MCP-RC            =  ZERO
                 MOVE    "tbl_sryact"        TO    MCP-TABLE
                 MOVE    "key2"              TO    MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL   FLG-SRYACT     =   1
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )  OR
                                    (SRY-SRYCD(IDX12)  =   SPACE)
                      IF   (SRY-SRYCD(IDX12)(1:1)     =   "8"  )  OR
                           (SRY-SRYCD(IDX12)(1:3)     =   "008")
                       IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
                         INITIALIZE                      PTCOM-REC
                         MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
                         MOVE    SRY-PTID            TO  PTCOM-PTID
                         MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
                         MOVE    SRY-SRYCD(IDX12)    TO  PTCOM-SRYCD
                         MOVE    SRY-INPUTNUM(IDX12) TO  PTCOM-RENNUM
                         PERFORM 900-PTCOM-READ-SEC
                         ADD     1     TO    IDZ
                         MOVE    PTCOM-INPUTCOMENT
                                                TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       ELSE
                         PERFORM      900-TENSU-TBL-SEC
                         ADD     1           TO  IDZ
                         MOVE    TNS-NAME    TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          EVALUATE  SRY-SRYCD(IDX12)
                            WHEN     "840000042"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    1    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000101"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    2    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                          END-EVALUATE
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          IF  SRY-SRYCD(IDX12) = "840000042" OR
                                                 "840000101"
                            MOVE    1    TO   FLG-COMARI2
                          ELSE
                            MOVE    1    TO   FLG-COMARI
                          END-IF
                       ELSE
                            MOVE    1    TO   FLG-COMARI
                       END-IF
                      END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"       TO  MCP-TABLE
                   MOVE    "key2"             TO  MCP-PATHNAME
                   PERFORM  900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_sryact"            TO  MCP-TABLE
              MOVE    "key2"                  TO  MCP-PATHNAME
              PERFORM  900-CLOSE-SEC
           END-IF
      *++++
           IF      FLG-COMARI    =    ZERO
              INITIALIZE                      SANTEIPLUS-REC
              MOVE    RECE82-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
              MOVE    RECE82-PTID         TO  SANTEIPLUS-PTID
              MOVE    WRK-KYUSEIYMD(1:6)  TO  SANTEIPLUS-SRYYM
              MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
              MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
              MOVE    ZERO                TO  SANTEIPLUS-SRYKA
              MOVE    RECE82-HKNCOMBI     TO  SANTEIPLUS-HKNCOMBINUM
              MOVE    WRK-KYUSEIYMD(7:2)  TO  SANTEIPLUS-DAYKEY
              MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
              MOVE    "tbl_santeiplus"    TO  MCP-TABLE
              MOVE    "key2"              TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC               =       ZERO
                 MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                 MOVE    "key2"               TO  MCP-PATHNAME
                 PERFORM  900-SANTEIPLUS-READ-SEC
                 PERFORM  UNTIL   FLG-SANTEIPLUS     =   1
                   IF  SANTEIPLUS-COMMENT  NOT =  SPACE
                      ADD     1                 TO  IDZ
                      MOVE  SANTEIPLUS-COMMENT  TO  WK-FREE-NAME(IDZ)
                      PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                 UNTIL    IDX13    <     1
                        IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                          COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                   IDX13    /    2
                          MOVE     1     TO      IDX13
                        END-IF
                      END-PERFORM
                   END-IF
                   IF   FLG-COMOKIKAE  =    1
                   AND  FLG-COMARI2    =   ZERO
                      IF   SANTEIPLUS-COMMENT(1:8)  = "手術　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    1    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:12) = "急性増悪　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    2    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                   END-IF
                   MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                   MOVE    "key2"               TO  MCP-PATHNAME
                   PERFORM  900-SANTEIPLUS-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_santeiplus"     TO  MCP-TABLE
              MOVE    "key2"               TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-KYUSEIHASSYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日のコメント編集
      *****************************************************************
       20011-KYUSEIHASSYO-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO         TO  IDX-RECE85
           MOVE    SPACE        TO  WRK-RECE85XX-REC
           INITIALIZE               WRK-RECE85XX-REC
      *
           ADD     1            TO  IDX-RECE85
           MOVE    "810000001"  TO  WRK-RECE85XX-SRYCD(IDX-RECE85 1)
           MOVE    ZERO         TO  WRK-RECE85XX-SRYSURYO(IDX-RECE85 1)
           MOVE    9            TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
           MOVE    "発症　　　月　　日"
                                TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                              TO  WRK-RECE85XX-NAME(IDX-RECE85 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                              TO  WRK-RECE85XX-NAME(IDX-RECE85 1)(13:4)
           MOVE    "1"        TO  WRK-RECE85XX-SIKFLG(IDX-RECE85 1)
      *
           PERFORM   VARYING   IDX12   FROM    1   BY  1
                      UNTIL  ( IDX12               >   49    )
                        OR   ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1        TO  IDX-RECE85
                MOVE   "810000001"
                                TO  WRK-RECE85XX-SRYCD(IDX-RECE85 1)
                MOVE   ZERO     TO  WRK-RECE85XX-SRYSURYO(IDX-RECE85 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                                TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                MOVE   WK-FREE-NAME(IDX12)
                                TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                MOVE   "1"      TO  WRK-RECE85XX-SIKFLG(IDX-RECE85 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
                PERFORM 800-RECE82-SET859-SEC
                MOVE    99999996            TO  RECE859-ZAINUM
                MOVE    "99"                TO  RECE859-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE859-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE85   =    1)
                   ADD         1            TO  IDX-RECE85
                   IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                       MOVE    1            TO  FLG-WRKRECE85
                   ELSE
                       MOVE  WRK-RECE85XX-SRYACT-TBL (IDX-RECE85 1)
                                        TO   RECE859-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE859-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE859-REC
                   ADD     1            TO   CNT-RECE859
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE82-SET849-SEC
           MOVE    99999996            TO  RECE849-ZAINUM
           MOVE    "99"                TO  RECE849-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           .
       20011-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集
      *****************************************************************
       20011-HASSYO-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO         TO  IDX-RECE85
           MOVE    SPACE        TO  WRK-RECE85XX-REC
           INITIALIZE               WRK-RECE85XX-REC
      *
           ADD     1            TO  IDX-RECE85
           MOVE    "810000001"  TO  WRK-RECE85XX-SRYCD(IDX-RECE85 1)
           MOVE    ZERO         TO  WRK-RECE85XX-SRYSURYO(IDX-RECE85 1)
      *
           MOVE    SPACE        TO  WRK-OKIKAEFLG
      *
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    19   TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
               MOVE    "脳血管疾患急性発症　　　年　　月　　日"
                            TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
               MOVE    21   TO  IDC1
               MOVE    27   TO  IDC2
               MOVE    33   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               COMPUTE WRK-ZAINUM    =    99998000
                                     +   (IDX16 + 31)
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    23   TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
               MOVE    "心大血管疾患リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               IF      FLG-RIHA      =  1
                 MOVE    99998201      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998200
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE82-HKNKBN    NOT  =  "4"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    21  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE    "脳血管疾患等リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       1
                   MOVE    21  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE    "脳血管疾患等リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       2
                   MOVE    23  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE
                       "脳血管疾患等リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99998401      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998400
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE82-HKNKBN    NOT  =  "4"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    18  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE    "運動器リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       1
                   MOVE    18  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE    "運動器リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       2
                   MOVE    20  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE    "運動器リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99998601      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998600
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE82-HKNKBN    NOT  =  "4"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-KOKYUKI
               MOVE    20   TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
               MOVE    "呼吸器リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
               MOVE    23   TO  IDC1
               MOVE    29   TO  IDC2
               MOVE    35   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               IF      FLG-RIHA      =  1
                 MOVE    99998801      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998800
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE82-HKNKBN    NOT  =  "4"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-SESSYOKU
               MOVE    21   TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
               MOVE    "摂食機能療法：治療開始　　　年　　月　　日"
                            TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               IF      FLG-RIHA      =  1
                 MOVE    99999001      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999000
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE82-HKNKBN    NOT  =  "4"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    19  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE    "難病患者リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       1
                   MOVE    19  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE    "難病患者リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       2
                   MOVE    21  TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                   MOVE    "難病患者リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99999201      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999200
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE82-HKNKBN    NOT  =  "4"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-SYOGAI
               MOVE    21   TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
               MOVE    "障害児（者）リハ：発症　　　年　　月　　日"
                            TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               IF      FLG-RIHA      =  1
                 MOVE    99999401      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999400
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE82-HKNKBN    NOT  =  "4"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           MOVE    "1"        TO  WRK-RECE85XX-SIKFLG(IDX-RECE85 1)
      *
           PERFORM   VARYING   IDX12   FROM    1   BY  1
                      UNTIL  ( IDX12               >   49    )
                        OR   ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1        TO  IDX-RECE85
                MOVE   "810000001"
                                TO  WRK-RECE85XX-SRYCD(IDX-RECE85 1)
                MOVE   ZERO     TO  WRK-RECE85XX-SRYSURYO(IDX-RECE85 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                                TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                MOVE   WK-FREE-NAME(IDX12)
                                TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                MOVE   "1"      TO  WRK-RECE85XX-SIKFLG(IDX-RECE85 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
                PERFORM 800-RECE82-SET859-SEC
                MOVE    WRK-ZAINUM          TO  RECE859-ZAINUM
                MOVE    WRK-SRYKBN          TO  RECE859-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE859-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE85   =    1)
                   ADD         1            TO  IDX-RECE85
                   IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                       MOVE    1            TO  FLG-WRKRECE85
                   ELSE
                       MOVE  WRK-RECE85XX-SRYACT-TBL (IDX-RECE85 1)
                                        TO   RECE859-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE859-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE859-REC
                   ADD     1            TO   CNT-RECE859
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE82-SET849-SEC
           MOVE    WRK-ZAINUM          TO  RECE849-ZAINUM
           MOVE    WRK-SRYKBN          TO  RECE849-SRYKBN
           MOVE    WRK-OKIKAEFLG       TO  RECE849-OKIKAEFLG
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           .
       20011-HASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集（年月日）
      *****************************************************************
       200111-HASSYO-COMMENT-HIZUKE-SEC          SECTION.
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-KYUSEIYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                          LNK-DAY2-AREA
           IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE85XX-NAME(IDX-RECE85 1)(IDC1:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE85XX-NAME(IDX-RECE85 1)(IDC2:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE85XX-NAME(IDX-RECE85 1)(IDC3:4)
              END-IF
           END-IF
      *
           .
       200111-HASSYO-COMMENT-HIZUKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ発症日があるか判定
      *****************************************************************
       20011-RIHABIRIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     SPACE    TO     WRK-RIHASANTEI-END
      *
      *    リハ終了日
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE82-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE82-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD2          TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE82-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"   TO  MCP-TABLE
                MOVE    "key3"         TO  MCP-PATHNAME
                PERFORM  900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF    SANTEI-SRYYM  <  RECE82-SRYYM
                   PERFORM   VARYING   IDX3   FROM  31  BY  -1
                              UNTIL  ( IDX3                <   1 )
                                OR   ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE  SANTEI-SRYYM  TO  WRK-RIHASANTEI-END(1:6)
                        MOVE  IDX3          TO  WRK-RIHASANTEI-END(7:2)
      *
                        MOVE  1             TO  FLG-KYUSEI-COMMENT2
      *
                     END-IF
      *
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_santei"   TO  MCP-TABLE
                 MOVE    "key3"         TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      ********
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
           INITIALIZE               TBL-RIHASANTEI-DAY
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD2
           MOVE     ZERO     TO     WRK-RIHASANTEI41-FLG
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE82-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE82-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE82-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"   TO  MCP-TABLE
                MOVE    "key3"         TO  MCP-PATHNAME
                PERFORM  900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE82-SRYYM
                  IF   SANTEI-SRYYM  =  RECE82-SRYYM
                   PERFORM  VARYING   IDX3   FROM   1  BY  1
                             UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
      ****               MOVE   SANTEI-SRYYM   TO  WRK-KYUSEIYMD(1:6)
      ****               MOVE   "00"           TO  WRK-KYUSEIYMD(7:2)
                         MOVE   1     TO  TB-RIHASANTEI-DAY(IDX3)
      *
                         MOVE   1     TO  FLG-KYUSEI-COMMENT
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE    1    TO   WRK-RIHASANTEI41-FLG
                         END-IF
                         IF    (SANTEI-SRYYM  NOT = "200604")
                         AND   (IDX3              =  1      )
                            MOVE   1     TO   FLG-KYUSEI-COMMENT2
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3                <   1 )
                               OR    ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
      ****               MOVE   SANTEI-SRYYM   TO  WRK-KYUSEIYMD(1:6)
      ****               MOVE   IDX3           TO  WRK-KYUSEIYMD(7:2)
      *
                       MOVE   SANTEI-SRYYM
                                        TO  WRK-RIHASANTEI-YMD2(1:6)
                       MOVE   IDX3      TO  WRK-RIHASANTEI-YMD2(7:2)
      *
                       IF  (WRK-RIHASANTEI-END  = SPACE              )
                       OR  (WRK-RIHASANTEI-END <= WRK-RIHASANTEI-YMD2)
      *
                         MOVE   SANTEI-SRYYM
                                          TO  WRK-RIHASANTEI-YMD(1:6)
                         MOVE   IDX3      TO  WRK-RIHASANTEI-YMD(7:2)
      *
                         MOVE   1         TO  FLG-KYUSEI-COMMENT
      *
                       END-IF
      *
                       MOVE     1         TO  FLG-KYUSEI-COMMENT2
      *
                       IF  (WRK-RIHASANTEI-END  = SPACE              )
                       OR  (WRK-RIHASANTEI-END <= WRK-RIHASANTEI-YMD2)
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE   ZERO  TO   FLG-KYUSEI-COMMENT2
                         END-IF
                         IF     WRK-RIHASANTEI41-FLG  =  1
                            MOVE   ZERO  TO   TB-RIHASANTEI-DAY(1)
                         END-IF
      *
                       END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"   TO  MCP-TABLE
                 MOVE    "key3"         TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-KYUSEI-COMMENT    =      1
               IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                     MOVE   1      TO  FLG-RIHA
                     MOVE   WRK-RIHASANTEI-YMD
                                   TO  WRK-KYUSEIYMD
                     PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                     PERFORM    20011-HASSYO-COMMENT-SEC
               END-IF
               PERFORM   VARYING   IDX16    FROM   1   BY   1
                          UNTIL    IDX16      >   31
                 IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                     MOVE   2      TO  FLG-RIHA
                     MOVE   RECE82-SRYYM
                                   TO  WRK-KYUSEIYMD(1:6)
                     MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                     PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                     PERFORM    20011-HASSYO-COMMENT-SEC
                 END-IF
               END-PERFORM
           END-IF
      *
           .
       20011-RIHABIRIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責従来様式：加算率 のコメント編集
      *****************************************************************
       2001-KASANRITU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE82-SET849-SEC
           MOVE    99999998            TO  RECE849-ZAINUM
           MOVE    "99"                TO  RECE849-SRYKBN
           MOVE    1                   TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           PERFORM 800-RECE82-SET859-SEC
           MOVE    99999998            TO  RECE859-ZAINUM
           MOVE    "99"                TO  RECE859-SRYKBN
      *    加算率のコメントの挿入
           MOVE    SPACE               TO  WRK-RECE85XX-REC
           INITIALIZE                      WRK-RECE85XX-REC
           MOVE   "810000001"          TO  WRK-RECE85XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE85XX-SRYSURYO(1 1)
           MOVE    9                   TO  WRK-RECE85XX-YUKOKETA(1 1)
           MOVE   "加算率（　．　　）"
                                       TO  WRK-RECE85XX-NAME(1 1)
      *
           MOVE    RECE82-KSNRATE(1:1) TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE85XX-NAME(1 1)(9:2)
           MOVE    RECE82-KSNRATE(2:1) TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE85XX-NAME(1 1)(13:2)
           MOVE    RECE82-KSNRATE(3:1) TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE85XX-NAME(1 1)(15:2)
      *
           MOVE     "1"                TO  WRK-RECE85XX-SIKFLG(1 1)
           MOVE    WRK-RECE85XX-SRYACT-TBL(1 1)
                                       TO  RECE859-SRYACT-TBL(1)
           WRITE   RECE859-REC
           ADD     1                   TO  CNT-RECE859
      *
           .
       2001-KASANRITU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算率編集
      *****************************************************************
       20011-KASANRITU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KASANRITU
           MOVE    SPACE                TO  WRK-HENSYU-KASANRITU2
      *
           MOVE    DAY-IDX3             TO  WRK-HENSYU-KASANRITU
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KASANRITU TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:2)
                                        TO  WRK-HENSYU-KASANRITU2
           END-IF
      *
           .
       20011-KASANRITU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責従来様式：薬剤等点単価 のコメント編集
      *****************************************************************
       2001-YKZTENTANKA-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE82-SET849-SEC
           MOVE    99999998            TO  RECE849-ZAINUM
           MOVE    "99"                TO  RECE849-SRYKBN
           MOVE    1                   TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           PERFORM 800-RECE82-SET859-SEC
           MOVE    99999998            TO  RECE859-ZAINUM
           MOVE    "99"                TO  RECE859-SRYKBN
      *    薬剤等点単価のコメントの挿入
           MOVE    SPACE               TO  WRK-RECE85XX-REC
           INITIALIZE                      WRK-RECE85XX-REC
           MOVE   "810000001"          TO  WRK-RECE85XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE85XX-SRYSURYO(1 1)
           MOVE    13                  TO  WRK-RECE85XX-YUKOKETA(1 1)
           MOVE    "薬剤等（１点単価　　　円）"
                                       TO  WRK-RECE85XX-NAME(1 1)
      *
           MOVE    RECE82-YKZTENTANKA(1:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           IF      WRK-HENSYU-KASANRITU2   =   "０"
              MOVE   "　"   TO   WRK-HENSYU-KASANRITU2
           END-IF
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE85XX-NAME(1 1)(19:2)
           MOVE    RECE82-YKZTENTANKA(2:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE85XX-NAME(1 1)(21:2)
      *
           MOVE     "1"                TO  WRK-RECE85XX-SIKFLG(1 1)
           MOVE    WRK-RECE85XX-SRYACT-TBL(1 1)
                                       TO  RECE859-SRYACT-TBL(1)
           WRITE   RECE859-REC
           ADD     1                   TO  CNT-RECE859
      *
           .
       2001-YKZTENTANKA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    在支援のコメント編集
      *****************************************************************
       2001-ZAISIEN-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE82-SET849-SEC
           MOVE    99999965            TO  RECE849-ZAINUM
           MOVE    WRK-RECE84-SRYKBN   TO  RECE849-SRYKBN
      *
           MOVE    1                   TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           PERFORM 800-RECE82-SET859-SEC
           MOVE    99999965            TO  RECE859-ZAINUM
           MOVE    WRK-RECE84-SRYKBN   TO  RECE859-SRYKBN
      *    在支援コメントの挿入
           MOVE    SPACE               TO  WRK-RECE85XX-REC
           INITIALIZE                      WRK-RECE85XX-REC
           MOVE    "810000001"         TO  WRK-RECE85XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE85XX-SRYSURYO(1 1)
           MOVE     3                  TO  WRK-RECE85XX-YUKOKETA(1 1)
           EVALUATE  TBL-ZAISIEN-COMMENT
             WHEN      1
               MOVE    "在支援"        TO  WRK-RECE85XX-NAME(1 1)
             WHEN      2
               MOVE    "在支病"        TO  WRK-RECE85XX-NAME(1 1)
           END-EVALUATE
           MOVE     "1"                TO  WRK-RECE85XX-SIKFLG(1 1)
      *
           MOVE    WRK-RECE85XX-SRYACT-TBL(1 1)
                                       TO  RECE859-SRYACT-TBL(1)
           WRITE   RECE859-REC
           ADD     1                   TO  CNT-RECE859
      *
           .
       2001-ZAISIEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特別のコメント（特別な関係にある他院）
      *****************************************************************
       2001-TAIN-TOKUBETU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE82-SET849-SEC
           MOVE    99999990            TO  RECE849-ZAINUM
           MOVE    "99"                TO  RECE849-SRYKBN
           MOVE    1                   TO  RECE849-MEISAISU
      *
           WRITE   RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           PERFORM 800-RECE82-SET859-SEC
           MOVE    99999990            TO  RECE859-ZAINUM
           MOVE    "99"                TO  RECE859-SRYKBN
      *
           MOVE    SPACE               TO  WRK-RECE85XX-REC
           INITIALIZE                      WRK-RECE85XX-REC
           MOVE   "810000001"          TO  WRK-RECE85XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE85XX-SRYSURYO(1 1)
           MOVE    4                   TO  WRK-RECE85XX-YUKOKETA(1 1)
           MOVE   "（特別）"           TO  WRK-RECE85XX-NAME(1 1)
      *
           MOVE     "1"                TO  WRK-RECE85XX-SIKFLG(1 1)
           MOVE    WRK-RECE85XX-SRYACT-TBL(1 1)
                                       TO  RECE859-SRYACT-TBL(1)
           WRITE   RECE859-REC
           ADD     1                   TO  CNT-RECE859
      *
           .
       2001-TAIN-TOKUBETU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診（同日複数診療科）があるか判定
      *****************************************************************
       2001-SYODOU-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-SYODOU-COMMENT   =    "1"
                MOVE       99       TO     FLG-COMMENT
                PERFORM    20011-SYODOU-COMMENT-SEC
           END-IF
      *
           .
       2001-SYODOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診（同日複数診療科）のコメント編集
      *****************************************************************
       20011-SYODOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE84-SYODOU-SRYKA(IDX)  =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "複初算定診療科："  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      診療科の取得
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE84-SYODOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE82-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key10"           TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             PERFORM   VARYING   IDW   FROM   200   BY  -1
                        UNTIL    IDW   <   1
                IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                     COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                     MOVE    1   TO    IDW
                END-IF
             END-PERFORM
      *
           END-PERFORM
      *
           PERFORM   200111-RECE85-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                TO  IDX-RECE85
           MOVE    SPACE               TO  WRK-RECE85XX-REC
           INITIALIZE                      WRK-RECE85XX-REC
      *
           MOVE    RECE84-MEISAISU     TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SYODOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤管理指導料があるか判定
      *****************************************************************
       2001-YAKUKANSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-YAKUKAN-COMMENT   =    "1"
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-YAKUKANSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院栄養食事指導料があるか判定
      *****************************************************************
       2001-NYUEIYOSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-NYUEIYO-COMMENT   =    "1"
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-NYUEIYOSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院前訪問指導料があるか判定
      *****************************************************************
       2001-HOUMONSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-HOUMON-COMMENT   =    "1"
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUMONSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    傷病手当金意見書交付料があるか判定
      *****************************************************************
       2001-SYOBYO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-SYOBYO-COMMENT   =    "1"
               MOVE    4                 TO  FLG-COMMENT
               PERFORM 20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYOBYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養費同意書交付料があるか判定
      *****************************************************************
       2001-RYOYO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-RYOYO-COMMENT    =    "1"
               MOVE    4                 TO     FLG-COMMENT
               PERFORM 20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-RYOYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院時服薬指導加算を算定時、当月に薬剤管理指導料がなく、前月
      *    に薬剤管理指導料があるか判定
      *****************************************************************
       2001-TAIIN-HANTEI-SEC          SECTION.
      *
           MOVE      ZERO            TO      WRK-TUKI-YAKUKAN
           MOVE      ZERO            TO      WRK-DAY-YAKUKAN
      *
           IF      RECE84-FLG-TAIIN-COMMENT   =    "1"
      *
             MOVE      ZERO            TO      FLG-YAKUKAN-COMMENT
      *
             IF    RECE82-SRYYM    >=  "200804"
               PERFORM   VARYING   IDX6   FROM    1   BY    1
                          UNTIL  ( IDX6   >   TBL-YAKUKAN-MAX       )
                            OR   ( FLG-YAKUKAN-COMMENT   =    1     )
                  MOVE   TBL-YAKUKAN(IDX6)     TO  WRK-YAKUKAN-SRYCD
                  PERFORM  2001X-TAIIN-HANTEI-TOUGETU-SEC
               END-PERFORM
             ELSE
                  MOVE   CONS-YAKUKAN-COMMENT  TO  WRK-YAKUKAN-SRYCD
                  PERFORM  2001X-TAIIN-HANTEI-TOUGETU-SEC
             END-IF
      *
             IF      FLG-YAKUKAN-COMMENT   =   ZERO
      *
               MOVE      ZERO            TO      WRK-ZENYYMM
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    RECE84-SRYYM        TO  LNK-DAY6-KIJUN(1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN(7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE     -1                 TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-ZENYYMM
      *
               IF    RECE82-SRYYM    >=  "200804"
                 PERFORM   VARYING   IDX6   FROM    1   BY    1
                            UNTIL    IDX6   >   TBL-YAKUKAN-MAX
                    MOVE   TBL-YAKUKAN(IDX6)     TO  WRK-YAKUKAN-SRYCD
                    PERFORM  2001Y-TAIIN-HANTEI-ZENGETU-SEC
                 END-PERFORM
               ELSE
                    MOVE   CONS-YAKUKAN-COMMENT  TO  WRK-YAKUKAN-SRYCD
                    PERFORM  2001Y-TAIIN-HANTEI-ZENGETU-SEC
               END-IF
      *
             END-IF
      *
             MOVE       99                TO     FLG-COMMENT
             IF    WRK-DAY-YAKUKAN    NOT   =   ZERO
                MOVE    2                 TO     FLG-HANTEI
                MOVE    WRK-ZENYYMM(5:2)  TO     WRK-TUKI-YAKUKAN
             ELSE
                MOVE    1                 TO     FLG-HANTEI
             END-IF
             PERFORM    20011-TAIIN-COMMENT-SEC
      *
           END-IF
      *
           .
       2001-TAIIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    当月に薬剤管理指導料があるかチェック
      *****************************************************************
       2001X-TAIIN-HANTEI-TOUGETU-SEC          SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    RECE84-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    RECE84-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    RECE84-PTID       TO  SRY-PTID
           MOVE    RECE84-SRYYM      TO  SRY-SRYYM
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (1)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (2)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (3)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (4)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
      *
           IF      MCP-RC               =       ZERO
             MOVE    "tbl_sryact"    TO  MCP-TABLE
             MOVE    "key7"          TO  MCP-PATHNAME
             PERFORM  900-SRYACT-READ-SEC
             IF      FLG-SRYACT  =  ZERO
                   MOVE    1         TO  FLG-YAKUKAN-COMMENT
             END-IF
           END-IF
      *
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2001X-TAIIN-HANTEI-TOUGETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月に薬剤管理指導料があるかチェック
      *****************************************************************
       2001Y-TAIIN-HANTEI-ZENGETU-SEC          SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    RECE84-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    RECE84-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    RECE84-PTID       TO  SRY-PTID
           MOVE    WRK-ZENYYMM       TO  SRY-SRYYM
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (1)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (2)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (3)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (4)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO   MCP-TABLE
               MOVE    "key7"           TO   MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
      *
               PERFORM  UNTIL   FLG-SRYACT   =  1
      *
                 INITIALIZE                  SRYACCT-REC
                 MOVE    SRY-HOSPNUM    TO   ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN  TO   ACCT-NYUGAIKBN
                 MOVE    SRY-PTID       TO   ACCT-PTID
                 MOVE    SRY-SRYKA      TO   ACCT-SRYKA
                 MOVE    SRY-SRYYM      TO   ACCT-SRYYM
                 MOVE    SRY-ZAINUM     TO   ACCT-ZAINUM
                 MOVE    SRYACCT-REC    TO   MCPDATA-REC
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key31"        TO   MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
      *
                 IF  MCP-RC               =  ZERO
                     MOVE    "tbl_sryacct"  TO  MCP-TABLE
                     MOVE    "key31"        TO  MCP-PATHNAME
                     PERFORM   900-DBFETCH-SEC
                     IF      MCP-RC       =  ZERO
                       MOVE    MCPDATA-REC  TO  SRYACCT-REC
                       PERFORM   VARYING   IDX  FROM   1   BY   1
                                  UNTIL    IDX    >   31
                         IF    ACCT-DAY(1 IDX)  NOT =  ZERO
                            IF    IDX   >   WRK-DAY-YAKUKAN
                                MOVE  IDX   TO  WRK-DAY-YAKUKAN
                            END-IF
                         END-IF
                       END-PERFORM
                     END-IF
                 END-IF
      *
                 INITIALIZE                  MCPAREA
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key31"        TO   MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"   TO   MCP-TABLE
                 MOVE    "key7"         TO   MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"      TO      MCP-TABLE
           MOVE    "key7"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2001Y-TAIIN-HANTEI-ZENGETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護支援連携指導料があるか判定
      *****************************************************************
       2001-KAISIEN-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-KAISIEN-COMMENT   =    "1"
               MOVE    ZERO                TO  FLG-KAISIEN
               MOVE    SPACE               TO  WRK-KAISIEN-FSANTEIYMD
               INITIALIZE                      SANTEI-REC
               MOVE    RECE82-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE82-PTID         TO  SANTEI-PTID
               MOVE    CONS-KAISIEN        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI   =  1 )
                            OR    ( FLG-KAISIEN  =  1 )
                   IF   SANTEI-SRYYM  <  RECE84-SRYYM
                     PERFORM  VARYING   IDX3   FROM  31  BY  -1
                               UNTIL  ( IDX3        <   1 )
                                OR    ( FLG-KAISIEN =   1 )
                       IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                         MOVE    1    TO  FLG-KAISIEN
                         MOVE   SANTEI-SRYYM
                                      TO  WRK-KAISIEN-FSANTEIYMD(1:6)
                         MOVE   IDX3  TO  WRK-KAISIEN-FSANTEIYMD(7:2)
                       END-IF
                     END-PERFORM
                   END-IF
                   MOVE    "tbl_santei"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
      *
               IF   FLG-KAISIEN  =   1
      *           剤の最初の日検索
                  MOVE       SPACE         TO  WRK-KAISIEN-YMD
                  PERFORM  VARYING   IDX3   FROM  1  BY  1
                            UNTIL    IDX3  >   31
                    IF   RECE84-KAISU(1 IDX3)  NOT =  ZERO
                       MOVE  RECE84-SRYYM  TO  WRK-KAISIEN-YMD(1:6)
                       MOVE  IDX3          TO  WRK-KAISIEN-YMD(7:2)
                       MOVE  31            TO  IDX3
                    END-IF
                  END-PERFORM
      *
                  IF   WRK-KAISIEN-YMD   NOT =  SPACE
                    MOVE  WRK-KAISIEN-YMD     TO  WRK-KAISIEN-CHECKYMD
                    PERFORM  2001-KAISIEN-PTNYUINRRK-CHECK-SEC
                    MOVE  WRK-KAISIEN-RRKNUM  TO  WRK-KAISIEN-RRKNUM-X
      *
                    MOVE  WRK-KAISIEN-FSANTEIYMD
                                              TO  WRK-KAISIEN-CHECKYMD
                    PERFORM  2001-KAISIEN-PTNYUINRRK-CHECK-SEC
                    MOVE  WRK-KAISIEN-RRKNUM  TO  WRK-KAISIEN-RRKNUM-Y
      *
                    IF  (WRK-KAISIEN-RRKNUM-X  NOT = ZERO           )
                    AND (WRK-KAISIEN-RRKNUM-Y  NOT = ZERO           )
                    AND (WRK-KAISIEN-RRKNUM-X = WRK-KAISIEN-RRKNUM-Y)
                       CONTINUE
                    ELSE
                       MOVE   ZERO   TO   FLG-KAISIEN
                    END-IF
                  ELSE
                       MOVE   ZERO   TO   FLG-KAISIEN
                  END-IF
               END-IF
      *
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KAISIEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴チェック
      *****************************************************************
       2001-KAISIEN-PTNYUINRRK-CHECK-SEC          SECTION.
      *
           MOVE    ZERO                  TO  WRK-KAISIEN-RRKNUM
           INITIALIZE                        PTNYUINRRK-REC
           MOVE    RECE82-HOSPNUM        TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE82-PTID           TO  PTNYUINRRK-PTID
           MOVE    WRK-KAISIEN-CHECKYMD  TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-KAISIEN-CHECKYMD  TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key11"               TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
             MOVE    "key11"             TO  MCP-PATHNAME
             PERFORM  900-PTNYUINRRK-READ-SEC
             IF    FLG-PTNYUINRRK  =  ZERO
                MOVE  PTNYUINRRK-RRKNUM  TO  WRK-KAISIEN-RRKNUM
             END-IF
           END-IF
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key11"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2001-KAISIEN-PTNYUINRRK-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    集団栄養食事指導料があるか判定
      *****************************************************************
       2001-SYUEIYO-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-SYUEIYO-COMMENT   =    "1"
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYUEIYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訪問看護指示料があるか判定
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-HOUSIJI-COMMENT   =    "1"
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUSIJI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅血液透析頻回指導管理料があるか判定
      *    在宅血液透析指導管理料算定日の歴月で２月以内か判定
      *****************************************************************
       2001-HINKAI-HANTEI-SEC          SECTION.
      *
           IF   RECE84-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE84-KAISU(1 IDX3)  NOT =  ZERO
                        MOVE  IDX3  TO  WRK-DAY
                        MOVE  31    TO  IDX3
                 END-IF
             END-PERFORM
      *
             MOVE     ZERO     TO     FLG-HINKAI-COMMENT
             MOVE     ZERO     TO     WRK-TOSEKIYMD
      *
      *      算定履歴検索
             INITIALIZE                  SANTEI-REC
             MOVE    RECE82-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE82-PTID         TO  SANTEI-PTID
             MOVE    CONS-TOSEKI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                    MOVE    "tbl_santei" TO  MCP-TABLE
                    MOVE    "key3"       TO  MCP-PATHNAME
                    PERFORM 900-SANTEI-READ-SEC
                    PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                     ( FLG-HINKAI-COMMENT =   1 )
      *
                      IF   SANTEI-SRYYM  <=  RECE84-SRYYM
      ***              MOVE   SANTEI-SRYYM     TO  WRK-FSANTEIYMD(1:6)
      ***              MOVE   SANTEI-MSANTEID  TO  WRK-FSANTEIYMD(7:2)
                       MOVE   SANTEI-FSANTEIYMD
                                               TO  WRK-FSANTEIYMD
      *
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     2              TO  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER2
      *
                       MOVE    RECE84-SRYYM    TO  WRK-HINKAIYMD(1:6)
                       MOVE    WRK-DAY         TO  WRK-HINKAIYMD(7:2)
      *
                       IF     WRK-DAY      NOT  =  ZERO
                       AND    WRK-FSANTEIYMD   <=  WRK-HINKAIYMD
                         IF ((WRK-HINKAIYMD    <   WRK-AFTER2) AND
                             (RECE84-SRYYM     <= "201003"   ))
                         OR ( RECE84-SRYYM     >= "201004"    )
                             MOVE    1    TO   FLG-HINKAI-COMMENT
                             MOVE  WRK-FSANTEIYMD
                                          TO   WRK-TOSEKIYMD
                         END-IF
                       END-IF
                      END-IF
      * 
                      MOVE    "tbl_santei"     TO  MCP-TABLE
                      MOVE    "key3"           TO  MCP-PATHNAME
                      PERFORM 900-SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF      FLG-HINKAI-COMMENT    =      1
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-HINKAI-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-HINKAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅血液透析頻回指導管理料のコメント編集
      *****************************************************************
       20011-HINKAI-COMMENT-SEC          SECTION.
      *
             PERFORM   200111-RECE85-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE85
             MOVE    SPACE                TO  WRK-RECE85XX-REC
             INITIALIZE                       WRK-RECE85XX-REC
      *      コメントの挿入
             COMPUTE  MAX-MEISAISU        =   RECE84-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16     TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "初回指導管理算定日　　　月　　日"
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(27:4)
             MOVE    "1"    TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    往診・特別往診があるか判定
      *****************************************************************
       2001-OSIN-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-OSIN-COMMENT   =    "1"
               MOVE       6       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-OSIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    人工腎臓等のコメントがあるか判定
      *****************************************************************
       2001-JINZOUETC-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-JINZOU-COMMENT   =    "1"
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           END-IF
      *
           .
       2001-JINZOUETC-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    局所陰圧閉鎖処置のコメントがあるか判定
      *****************************************************************
       2001-KYOKUIN-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-KYOKUIN-COMMENT   =    "1"
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           END-IF
      *
           .
       2001-KYOKUIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日のコメントがあるか判定
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-SHUJUTU-COMMENT   =    SPACE
               MOVE       1       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE84-WRITE-SEC
           END-IF
      *
           .
       2001-SHUJUTSU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻酔日のコメントがあるか判定
      *****************************************************************
       2001-MASUI-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-MASUI-COMMENT   =    SPACE
               MOVE       8       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE84-WRITE-SEC
           END-IF
      *
           .
       2001-MASUI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （検選）のコメントがあるか判定
      *****************************************************************
       2001-KENSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE84-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （検選）等のコメント編集
      *****************************************************************
       20011-KENSENTOU-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-RECE85-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           PERFORM
             UNTIL  ( FLG-RECE85           =   1                   )
             OR     ( RECE84-HOSPNUM   NOT =   RECE85-HOSPNUM      )
             OR     ( RECE84-SRYYM     NOT =   RECE85-SRYYM        )
             OR     ( RECE84-NYUGAIKBN NOT =   RECE85-NYUGAIKBN    )
             OR     ( RECE84-PTID      NOT =   RECE85-PTID         )
             OR     ( RECE84-RECEKA    NOT =   RECE85-RECEKA       )
             OR     ( RECE84-HKNID     NOT =   RECE85-HKNID        )
             OR     ( RECE84-TEKSTYMD  NOT =   RECE85-TEKSTYMD     )
             OR     ( RECE84-ZAINUM    NOT =   RECE85-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE85-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE85
                  MOVE   RECE85-SRYACT-TBL(IDX)
                             TO   WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
      *++++++
                  IF    IDX-RECE85  =  RECE84-MEISAISU
      *
                    IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = "810000001"
                    OR  WRK-RECE85XX-SRYCD(IDX-RECE85 1)(1:3)  = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) * 2) + 1
      *
                        IF   WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) <=  96
                          MOVE    "　×"   TO
                           WRK-RECE85XX-NAME(IDX-RECE85 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
      *
                        MOVE    RECE84-ZAIKAISU   TO   DAY-IDX2
                        PERFORM 20011-KAISU-HENSYU-SEC
                        IF   WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) <=  96
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE85XX-NAME(IDX-RECE85 1)(IDV:4)
                          COMPUTE WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) =
                                  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1) +
                                  4
                        END-IF
      *
                    END-IF
      *
                  END-IF
      *++++++
             END-PERFORM 
      *
             PERFORM 990-RECE85-NEXT-SEC
      *
           END-PERFORM
      *
      *    手術日コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
      *
                  INITIALIZE                        RECE859-REC
                  MOVE    WRK-RECE85-KEY       TO   RECE859-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE859-RENNUM
                  MOVE    WRK-RECE85-PTNUM     TO   RECE859-PTNUM
                  MOVE    WRK-RECE85-SRYKA     TO   RECE859-SRYKA
                  MOVE    WRK-RECE85-SRYKBN    TO   RECE859-SRYKBN
                  MOVE    WRK-RECE85-SRYSYUKBN TO   RECE859-SRYSYUKBN
                  MOVE    WRK-RECE85-JIHIMONEY TO   RECE859-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE85   =   1)
                     ADD         1             TO    IDX-RECE85
                     IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE85
                     ELSE
                         MOVE  WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
                                          TO   RECE859-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE85XX-SIKFLG(IDX-RECE85 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE859-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE859-REC
                     ADD     1            TO   CNT-RECE859
                  END-IF
           END-PERFORM
      *
           MOVE    RECE84-REC        TO      RECE849-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE849-MEISAISU
           WRITE   RECE849-REC
           ADD     1                 TO      CNT-RECE849
      *
           .
       20011-KENSENTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集
      *****************************************************************
       20011-KAISU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KAISU
           MOVE    SPACE                TO  WRK-HENSYU-KAISU2
      *
           MOVE    DAY-IDX2             TO  WRK-HENSYU-KAISU
           IF      WRK-HENSYU-KAISU(1:1) =  "0"
               MOVE      SPACE          TO  WRK-HENSYU-KAISU(1:1)
           END-IF
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KAISU     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WRK-HENSYU-KAISU2
           END-IF
      *
           .
       20011-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）があるか判定
      *****************************************************************
       2001-KENSA-HANTEI-SEC          SECTION.
      *
           IF     RECE84-FLG-KENSA-COMMENT    =    "1"
               MOVE       7       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＴ・ＭＲＩがあるか判定
      *****************************************************************
       2001-CTMRI-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-CTMRI-COMMENT   =    "1"
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE84-WRITE-SEC
           END-IF
      *
           .
       2001-CTMRI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神科退院前訪問指導料があるか判定
      *****************************************************************
       2001-HOUMONSIDO-SEISIN-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-HOUSEI-COMMENT   =    "1"
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUMONSIDO-SEISIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院時服薬指導加算を算定時の前月薬剤管理指導料のコメント編集
      *****************************************************************
       20011-TAIIN-COMMENT-SEC          SECTION.
      *
           MOVE       RECE84-REC       TO  RECE849-REC
           EVALUATE   FLG-HANTEI
             WHEN       1
                ADD     1              TO  RECE849-MEISAISU
             WHEN       2
                ADD     2              TO  RECE849-MEISAISU
           END-EVALUATE
           WRITE      RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           PERFORM 800-RECE84-SET85-SEC
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                 KEY IS    >=   RECE85-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE85
                 NOT INVALID
                     PERFORM 990-RECE85-NEXT-SEC
           END-START
      *
           MOVE    RECE85-REC           TO  RECE859-REC
           EVALUATE   FLG-HANTEI
             WHEN       1
               MOVE    RECE85-SRYACT-TBL(1) TO  RECE859-SRYACT-TBL(1)
               MOVE    RECE85-SRYACT-TBL(2) TO  RECE859-SRYACT-TBL(3)
               MOVE    RECE85-SRYACT-TBL(3) TO  RECE859-SRYACT-TBL(4)
               MOVE    RECE85-SRYACT-TBL(4) TO  RECE859-SRYACT-TBL(5)
      *
               MOVE    SPACE                TO  RECE859-SRYACT-TBL(2)
               INITIALIZE                       RECE859-SRYACT-TBL(2)
               MOVE    "810000001"          TO  RECE859-SRYCD(2)
               MOVE    ZERO                 TO  RECE859-SRYSURYO(2)
               PERFORM  200111-SANTEI-DAY-SEC
               PERFORM  200111-TAIIN-COMMENT-HENSYU-SEC
               MOVE     "1"                 TO  RECE859-SIKFLG(2)
             WHEN       2
               MOVE    RECE85-SRYACT-TBL(1) TO  RECE859-SRYACT-TBL(1)
               MOVE    RECE85-SRYACT-TBL(2) TO  RECE859-SRYACT-TBL(4)
               MOVE    RECE85-SRYACT-TBL(3) TO  RECE859-SRYACT-TBL(5)
      *
               MOVE    SPACE                TO  RECE859-SRYACT-TBL(2)
               INITIALIZE                       RECE859-SRYACT-TBL(2)
               MOVE    "810000001"          TO  RECE859-SRYCD(2)
               MOVE    ZERO                 TO  RECE859-SRYSURYO(2)
               PERFORM  200111-SANTEI-DAY-SEC
               PERFORM  200111-TAIIN-COMMENT-HENSYU-SEC
               MOVE     "1"                 TO  RECE859-SIKFLG(2)
      *
               MOVE    SPACE                TO  RECE859-SRYACT-TBL(3)
               INITIALIZE                       RECE859-SRYACT-TBL(3)
               MOVE    "810000001"          TO  RECE859-SRYCD(3)
               MOVE    ZERO                 TO  RECE859-SRYSURYO(3)
               MOVE    16                   TO  RECE859-YUKOKETA(3)
               MOVE    "薬剤管理指導料算定　　　月　　日"
                                            TO  RECE859-NAME(3)
               MOVE     WRK-TUKI-YAKUKAN    TO  DAY-IDX
               MOVE     TBL-DAY(DAY-IDX)    TO  RECE859-NAME(3)(21:4)
               MOVE     WRK-DAY-YAKUKAN     TO  DAY-IDX
               MOVE     TBL-DAY(DAY-IDX)    TO  RECE859-NAME(3)(27:4)
               MOVE     "1"                 TO  RECE859-SIKFLG(3)
           END-EVALUATE
      *
           WRITE    RECE859-REC
           ADD      1                   TO  CNT-RECE859
      *
           .
       20011-TAIIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日編集
      *****************************************************************
       200111-TAIIN-COMMENT-HENSYU-SEC          SECTION.
      *
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX
           IF       DAY-IDX  <  10
             MOVE    5                  TO  RECE859-YUKOKETA(2)
             MOVE    "算定　　日"       TO  RECE859-NAME(2)(1:10)
             MOVE   TBL-DAY(DAY-IDX)    TO  RECE859-NAME(2)(5:4)
             MOVE   11                  TO  IDX17
           ELSE
             MOVE    6                  TO  RECE859-YUKOKETA(2)
             MOVE    "算定　　　日"     TO  RECE859-NAME(2)(1:12)
             MOVE   TBL-DAY(DAY-IDX)    TO  RECE859-NAME(2)(7:4)
             MOVE   13                  TO  IDX17
           END-IF
           IF       TB-SANTEI-DAY(2)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(2)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE859-YUKOKETA(2)
               MOVE  "、　日"       TO  RECE859-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE859-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE859-YUKOKETA(2)
               MOVE  "、　　日"     TO  RECE859-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE859-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(3)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(3)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE859-YUKOKETA(2)
               MOVE  "、　日"       TO  RECE859-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE859-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE859-YUKOKETA(2)
               MOVE  "、　　日"     TO  RECE859-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE859-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(4)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(4)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE859-YUKOKETA(2)
               MOVE  "、　日"       TO  RECE859-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE859-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE859-YUKOKETA(2)
               MOVE  "、　　日"     TO  RECE859-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE859-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(5)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(5)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE859-YUKOKETA(2)
               MOVE  "、　日"       TO  RECE859-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE859-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE859-YUKOKETA(2)
               MOVE  "、　　日"     TO  RECE859-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE859-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
      *
           .
       200111-TAIIN-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集（差込：上）
      *****************************************************************
       20011-SHUJUTSU-ETC-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  200111-FREE-COMMENT2-SEC                              
      *
           PERFORM  200111-RECE85-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1       TO  IDX-RECE85
                MOVE   "810000001"
                               TO  WRK-RECE85XX-SRYCD(IDX-RECE85 1)
                MOVE   ZERO    TO  WRK-RECE85XX-SRYSURYO(IDX-RECE85 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                               TO  WRK-RECE85XX-YUKOKETA(IDX-RECE85 1)
                MOVE   WK-FREE-NAME2(IDX)
                               TO  WRK-RECE85XX-NAME(IDX-RECE85 1)
                MOVE    "1"    TO  WRK-RECE85XX-SIKFLG(IDX-RECE85 1)
           END-PERFORM 
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集２（差込：下）
      *****************************************************************
       20011-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM   200111-FREE-COMMENT2-SEC                              
      *
           PERFORM   200111-RECE85-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           MOVE    RECE84-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                              TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"   TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM 
      *
           IF    FLG-KAISIEN  =  1
              COMPUTE  MAX-MEISAISU  =   MAX-MEISAISU  +  1
              MOVE   "810000001"
                            TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
              MOVE   ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
              MOVE   12     TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE   "初回算定日　　　月　　日"
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
              MOVE   WRK-KAISIEN-FSANTEIYMD(5:2)   TO  DAY-IDX2
              MOVE   TBL-DAY(DAY-IDX2)
                       TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(13:4)
              MOVE   WRK-KAISIEN-FSANTEIYMD(7:2)   TO  DAY-IDX2
              MOVE   TBL-DAY(DAY-IDX2)
                       TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(19:4)
              MOVE   "1"    TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
      *
              MOVE   ZERO   TO  FLG-KAISIEN
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC2-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日の確定
      *****************************************************************
       200111-SANTEI-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-SANTEI-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
               IF    RECE84-KAISU(1 IDX)  NOT =  ZERO
                   ADD   1     TO   DAY-IDX
                   MOVE  IDX   TO  TB-SANTEI-DAY(DAY-IDX)
               END-IF
           END-PERFORM
           .
       200111-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付の編集
      *****************************************************************
       200111-FREE-COMMENT2-SEC          SECTION.                         
      *
           ADD      1          TO    IDZ
      *
           EVALUATE   FLG-COMMENT
              WHEN    1
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "手術"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    2
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "実施"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    3
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "算定"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    4
                 MOVE     16         TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     8          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "交付　　年　　月"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
                 INITIALIZE                      STS-AREA-DAY
                 INITIALIZE                      LNK-DAY2-AREA
                 MOVE    "21"                TO  LNK-DAY2-IRAI
                 MOVE    RECE82-SRYYM        TO  WRK-HENYMD(1:6)
                 MOVE    "01"                TO  WRK-HENYMD(7:2)
                 MOVE    WRK-HENYMD          TO  LNK-DAY2-YMD
                 CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                             LNK-DAY2-AREA
                 IF  STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(5:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(11:4)
                   END-IF
                 END-IF
              WHEN    6
                 MOVE     0          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     0          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
              WHEN    7
                 MOVE     8          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     4          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "検査実施"
                             TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    8
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "麻酔"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
           END-EVALUATE
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX                 >    31)
                           OR     (TB-SANTEI-DAY(IDX)  =  ZERO)
      *
                 IF  FLG-FIRST  =  ZERO
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "　"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "日"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF   FLG-COMMENT   =   4 OR 6
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       ELSE
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    "　"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       END-IF
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "日"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  2001111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  24 OR 25
                          PERFORM  2001111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  2001111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  23 OR 24 OR 25
                          PERFORM  2001111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
      *
           END-PERFORM
           .
       200111-FREE-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付の編集（改行）
      *****************************************************************
       2001111-FREE-COMMENT2-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "　"        TO  WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
           .
       2001111-FREE-COMMENT2-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリのコメント編集
      *****************************************************************
       2001-RIHA-COMMENT-SEC          SECTION.
      *
             PERFORM   200111-RECE85-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE85
             MOVE    SPACE                TO  WRK-RECE85XX-REC
             INITIALIZE                       WRK-RECE85XX-REC
      *      リハビリコメントの挿入
             COMPUTE  MAX-MEISAISU        =   RECE84-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "実施日数　　日"
                            TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
             MOVE    "1"    TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             PERFORM  VARYING   IDX  FROM  1  BY  1
                       UNTIL    IDX   >   31
                 IF   RECE84-KAISU(1 IDX)  NOT = ZERO
                        ADD      1        TO  CNT-RIHA
                 END-IF
             END-PERFORM
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                          TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       2001-RIHA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ医学管理料のコメント編集
      *****************************************************************
       2001-RIHAIGAKU-COMMENT-SEC          SECTION.
      *
           EVALUATE      RECE84-RIHABIRI-COMMENT
              WHEN           "2"
                  MOVE   CONS-SINDAI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "3"
                  MOVE   CONS-SINDAI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "4"
                  MOVE   CONS-KOKYUKI-IGAKU1  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "5"
                  MOVE   CONS-KOKYUKI-IGAKU2  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "6"
                  MOVE   CONS-NOUKE-IGAKU1    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "7"
                  MOVE   CONS-NOUKE-IGAKU2    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "8"
                  MOVE   CONS-UNDOKI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "9"
                  MOVE   CONS-UNDOKI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
           END-EVALUATE
      ***
           MOVE          SPACE          TO  WRK-RIHAIGAKU-FSANTEIYMD
      *    医学管理：算定履歴検索
           INITIALIZE                       SANTEI-REC
           MOVE    RECE84-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE84-PTID          TO  SANTEI-PTID
           MOVE    RECE84-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD  TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    RECE84-HKNCOMBI      TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
                MOVE  SANTEI-FSANTEIYMD TO  WRK-RIHAIGAKU-FSANTEIYMD
           END-IF
      ***
           INITIALIZE      WRK-RIHAIGAKU-SANTEI-AREA
      *    リハ実施ダミー：算定履歴検索
           INITIALIZE                       SANTEI-REC
           MOVE    RECE84-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE84-PTID          TO  SANTEI-PTID
           MOVE    RECE84-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD2 TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    RECE84-HKNCOMBI      TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF   SANTEI-DAY(IDX)   NOT =  ZERO
                    MOVE  "1"  TO  WRK-RIHAIGAKU-SANTEI-FLG
                    MOVE  SANTEI-DAY(IDX)
                               TO  WRK-RIHAIGAKU-SANTEI-DAY(IDX)
                 END-IF
               END-PERFORM
           END-IF
      *
      ***
      *
           PERFORM  200111-RECE85-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           COMPUTE  MAX-MEISAISU        =   RECE84-MEISAISU  +  1
      *
           MOVE    "810000001"
                         TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
           MOVE    9     TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
           MOVE    "開始　　　月　　日"
                         TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
           MOVE    "1"   TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
           IF      WRK-RIHAIGAKU-FSANTEIYMD  NOT = SPACE
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(7:4)
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(13:4)
           END-IF
      *
           IF      WRK-RIHAIGAKU-SANTEI-FLG  NOT = SPACE
             PERFORM    20011-RIHAIGAKU-DUMMY-COMMENT-SEC
           ELSE
             PERFORM    200111-SRYACTINF-HENSYU-SEC
           END-IF
      *
           .
       2001-RIHAIGAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ医学管理料のコメント編集（リハ実施日）
      *****************************************************************
       20011-RIHAIGAKU-DUMMY-COMMENT-SEC          SECTION.
      *
           INITIALIZE             TBL-SANTEI-DAY
      *
           MOVE     ZERO      TO   DAY-IDX
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX    >   31
             IF  WRK-RIHAIGAKU-SANTEI-DAY(IDX)  NOT = ZERO
                 ADD    1     TO   DAY-IDX
                 MOVE   IDX   TO   TB-SANTEI-DAY(DAY-IDX)
             END-IF
           END-PERFORM
      *
           MOVE     ZERO      TO   IDZ
           INITIALIZE         WRK-FREE-AREA2
      *
           MOVE     2         TO   FLG-COMMENT
           PERFORM  200111-FREE-COMMENT2-SEC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-RIHAIGAKU-DUMMY-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    （リハ選）のコメントがあるか判定
      *****************************************************************
       2001-RIHASEN-HANTEI-SEC          SECTION.
      *
           IF     RECE84-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-RIHASEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊日のコメント編集
      *    精神科隔離病室管理加算（入院料加算）のコメント編集
      *****************************************************************
       2001-NYU-COMMENT-SEC          SECTION.
      *
           PERFORM     20011-NYU-DAY-SEC
      *
           PERFORM     200111-RECE85-TAIHI-SEC
      *
           MOVE     ZERO    TO    FLG-NG
           IF   (RECE84-ZAISKBKBN  =  "2"                          )
           AND  (FLG-RECE85        =  ZERO                         )
           AND  (WRK-RECE85-SRYCD(1)  =  "190108890" OR "190142190"
                                      OR "190108790" OR "190142090"
                                      OR "190108990" OR "190109090"
                                      OR "190142290" OR "190141590")
              MOVE    1     TO    FLG-NG
           END-IF
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
      *
           MOVE    RECE84-MEISAISU      TO  MAX-MEISAISU
      *
           IF    FLG-NG  =  ZERO
      *      算定日コメントの挿入
             PERFORM  VARYING      IDX    FROM  1   BY   1
                      UNTIL       (IDX    >     31)      OR
                                  (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
      ***      MOVE    TBL-NYU-DAY-CD(IDX)
               MOVE    "810000001"
                             TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
               MOVE    ZERO  TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
               IF    TBL-NYU-DAY-CD(IDX)  =  CONS-SANTEI-COMMENT1
                 MOVE    3   TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "　　日"
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(1:4)
               ELSE
                 MOVE    7   TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "　　日〜　　日"
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX) 
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(1:4)
                 MOVE    TBL-NYU-DAY-ED(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)(9:4)
               END-IF
               MOVE    "1"   TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
             END-PERFORM
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       2001-NYU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊日の確定
      *    精神科隔離病室管理加算日（入院料加算）の確定
      *****************************************************************
       20011-NYU-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-NYU-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF    RECE84-KAISU(1 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                  IF  (RECE84-ZAISKBKBN         =  "2")
                  AND (FLG-RYOCOM               =   1 )
                  AND (TBL-RYOCOM-NYUINKBN(IDX) =  "1")
                  AND (RECE82-SRYYM       >=  "201004")
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                  ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                  END-IF
                END-IF
           END-PERFORM
      *
           PERFORM    VARYING    DAY-IDX  FROM   1   BY   1
                      UNTIL  (DAY-IDX                      >   31)  OR
                             (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  CONS-SANTEI-COMMENT1
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  CONS-SANTEI-COMMENT2
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
           END-PERFORM
      *
           .
       20011-NYU-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神科隔離病室管理加算（入院料加算）があるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-KAKURI-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-KAKURI-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-KAKURI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災治療計画加算（入院料加算）があるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-CHIRYO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-CHIRYO-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   20011-CHIRYO-COMMENT-SEC
           END-IF
      *
           .
       2001-CHIRYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    栄養サポートチーム加算（入院料加算）のコメントがあるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-EISAPO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-EISAPO-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-EISAPO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    呼吸ケアチーム加算（入院料加算）のコメントがあるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-KOKKEA-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-KOKKEA-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-KOKKEA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    重症者加算（精神療養病棟入院料）（入院料加算）のコメントがあるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-JYUSYO-HANTEI-SEC          SECTION.
      *
           IF      RECE84-FLG-JYUSYO-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-JYUSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災治療計画加算のコメント編集
      *****************************************************************
       20011-CHIRYO-COMMENT-SEC          SECTION.
      *
           PERFORM    200111-SANTEI-DAY-SEC
      *
           MOVE       RECE84-REC       TO  RECE849-REC
           ADD     1                   TO  RECE849-MEISAISU
           WRITE      RECE849-REC
           ADD     1                   TO  CNT-RECE849
      *
           PERFORM 800-RECE84-SET85-SEC
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                 KEY IS    >=   RECE85-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE85
                 NOT INVALID
                     PERFORM 990-RECE85-NEXT-SEC
           END-START
      *
           MOVE    RECE85-REC           TO  RECE859-REC
           MOVE    RECE85-SRYACT-TBL(1) TO  RECE859-SRYACT-TBL(1)
           MOVE    RECE85-SRYACT-TBL(2) TO  RECE859-SRYACT-TBL(3)
           MOVE    RECE85-SRYACT-TBL(3) TO  RECE859-SRYACT-TBL(4)
           MOVE    RECE85-SRYACT-TBL(4) TO  RECE859-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE859-SRYACT-TBL(2)
           INITIALIZE                       RECE859-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE859-SRYCD(2)
           MOVE    ZERO                 TO  RECE859-SRYSURYO(2)
           MOVE    9                    TO  RECE859-YUKOKETA(2)
           MOVE    "算定　　　月　　日" TO  RECE859-NAME(2)
           MOVE     RECE82-SRYYM(5:2)   TO  DAY-IDX2
           MOVE     TBL-DAY(DAY-IDX2)   TO  RECE859-NAME(2)(7:4)
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX2
           MOVE     TBL-DAY(DAY-IDX2)   TO  RECE859-NAME(2)(13:4)
           MOVE     "1"                 TO  RECE859-SIKFLG(2)
      *
           WRITE    RECE859-REC
           ADD      1                   TO  CNT-RECE859
      *
           .
       20011-CHIRYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       2099-RECE84-WRITE-SEC      SECTION.
      *
           MOVE    RECE84-REC      TO  RECE849-REC
           WRITE   RECE849-REC
           ADD     1               TO  CNT-RECE849
      *
           PERFORM 20991-RECE85-TENSOU-SEC
           .
       2099-RECE84-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０５出力処理
      *****************************************************************
       20991-RECE85-TENSOU-SEC     SECTION.
      *
           PERFORM 800-RECE84-SET85-SEC
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
               KEY IS    >=   RECE85-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE85
               NOT INVALID
                   PERFORM 990-RECE85-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE85           =   1                   )
               OR     ( RECE84-HOSPNUM   NOT =   RECE85-HOSPNUM      )
               OR     ( RECE84-SRYYM     NOT =   RECE85-SRYYM        )
               OR     ( RECE84-NYUGAIKBN NOT =   RECE85-NYUGAIKBN    )
               OR     ( RECE84-PTID      NOT =   RECE85-PTID         )
               OR     ( RECE84-RECEKA    NOT =   RECE85-RECEKA       )
               OR     ( RECE84-HKNID     NOT =   RECE85-HKNID        )
               OR     ( RECE84-TEKSTYMD  NOT =   RECE85-TEKSTYMD     )
               OR     ( RECE84-ZAINUM    NOT =   RECE85-ZAINUM       )
      *
      *
               MOVE          RECE85-REC       TO      RECE859-REC
               WRITE         RECE859-REC
               ADD     1                 TO  CNT-RECE859
      *
               PERFORM 990-RECE85-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE85-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　退避処理
      *****************************************************************
       200111-RECE85-TAIHI-SEC     SECTION.
      *
           PERFORM 800-RECE84-SET85-SEC
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                 KEY IS    >=   RECE85-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE85
                 NOT INVALID
                     PERFORM 990-RECE85-NEXT-SEC
                     MOVE    RECE85-REC  TO  WRK-RECE85-REC
           END-START
      *
           .
       200111-RECE85-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE85           =   1                   )
             OR     ( RECE84-HOSPNUM   NOT =   RECE85-HOSPNUM      )
             OR     ( RECE84-SRYYM     NOT =   RECE85-SRYYM        )
             OR     ( RECE84-NYUGAIKBN NOT =   RECE85-NYUGAIKBN    )
             OR     ( RECE84-PTID      NOT =   RECE85-PTID         )
             OR     ( RECE84-RECEKA    NOT =   RECE85-RECEKA       )
             OR     ( RECE84-HKNID     NOT =   RECE85-HKNID        )
             OR     ( RECE84-TEKSTYMD  NOT =   RECE85-TEKSTYMD     )
             OR     ( RECE84-ZAINUM    NOT =   RECE85-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE85-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE85
                  MOVE   RECE85-SRYACT-TBL(IDX)
                             TO   WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
             END-PERFORM 
      *
             PERFORM 990-RECE85-NEXT-SEC
      *
           END-PERFORM
      *
      *    手術日コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
      *
                  INITIALIZE                        RECE859-REC
                  MOVE    WRK-RECE85-KEY       TO   RECE859-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE859-RENNUM
                  MOVE    WRK-RECE85-PTNUM     TO   RECE859-PTNUM
                  MOVE    WRK-RECE85-SRYKA     TO   RECE859-SRYKA
                  MOVE    WRK-RECE85-SRYKBN    TO   RECE859-SRYKBN
                  MOVE    WRK-RECE85-SRYSYUKBN TO   RECE859-SRYSYUKBN
                  MOVE    WRK-RECE85-JIHIMONEY TO   RECE859-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE85   =   1)
                     ADD         1             TO    IDX-RECE85
                     IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE85
                     ELSE
                         MOVE  WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
                                          TO   RECE859-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE85XX-SIKFLG(IDX-RECE85 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE859-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE859-REC
                     ADD     1            TO   CNT-RECE859
                  END-IF
           END-PERFORM
      *
           MOVE    RECE84-REC        TO      RECE849-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE849-MEISAISU
           WRITE   RECE849-REC
           ADD     1                 TO      CNT-RECE849
      *
           .
       200111-SRYACTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    差込編集２
      *****************************************************************
       250-SASHIKOMI2-SEC     SECTION.
      *
      *    パラメータ８４９ファイルより編集
           PERFORM 800-RECE82-SET849-SEC
           MOVE    ZERO                TO  RECE849-ZAINUM
      *
           START   RECE849-FILE
               KEY IS    >=   RECE849-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE849
               NOT INVALID
                   PERFORM 990-RECE849-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE849          =   1                    )
               OR     ( RECE82-HOSPNUM   NOT =   RECE849-HOSPNUM      )
               OR     ( RECE82-SRYYM     NOT =   RECE849-SRYYM        )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE849-NYUGAIKBN    )
               OR     ( RECE82-PTID      NOT =   RECE849-PTID         )
               OR     ( RECE82-RECEKA    NOT =   RECE849-RECEKA       )
               OR     ( RECE82-HKNID     NOT =   RECE849-HKNID        )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE849-TEKSTYMD     )
      *
               EVALUATE  RECE849-SRYKBN
                 WHEN      "90"
                 WHEN      "92"
                    IF    FLG-RYOCOM  =  ZERO
                       PERFORM       2599-RECE849-WRITE-SEC
                    ELSE
                       EVALUATE  RECE849-ZAISKBKBN
                         WHEN     "1"
                         WHEN     "2"
                           PERFORM   2502-RYOCOM-HANTEI-SEC
                         WHEN   OTHER
                           PERFORM   2599-RECE849-WRITE-SEC
                       END-EVALUATE
                    END-IF
                 WHEN      OTHER
                    MOVE   ZERO   TO  FLG-COMMENT
      *             （酸素計算式のコメントのセット）
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2501-SANSO-HANTEI-SEC
                    END-IF
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2599-RECE849-WRITE-SEC
                    END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE849-NEXT-SEC
      *
           END-PERFORM
      *
           .
       250-SASHIKOMI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素があるか判定
      *****************************************************************
       2501-SANSO-HANTEI-SEC          SECTION.
      *
           IF     RECE849-FLG-SANSO-COMMENT   =    "1"
               MOVE       99      TO     FLG-COMMENT
      *
               PERFORM    25011-SANSO-TAIHI-SEC
      *
               PERFORM    25011-SANSO-COMMENT-SEC
      *
           END-IF
      *
           .
       2501-SANSO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素の数量・単価退避
      *****************************************************************
       25011-SANSO-TAIHI-SEC          SECTION.
      *
           MOVE       ZERO    TO     IDX4
           INITIALIZE                TBL-SANSO-INF
      *
           PERFORM 800-RECE849-SET859-SEC
           MOVE    RECE849-ZAINUM       TO  RECE859-ZAINUM
           START   RECE859-FILE
               KEY IS    >=   RECE859-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE859
               NOT INVALID
                   PERFORM 990-RECE859-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE859           =   1                 )
               OR     ( RECE849-HOSPNUM   NOT =   RECE859-HOSPNUM   )
               OR     ( RECE849-SRYYM     NOT =   RECE859-SRYYM     )
               OR     ( RECE849-NYUGAIKBN NOT =   RECE859-NYUGAIKBN )
               OR     ( RECE849-PTID      NOT =   RECE859-PTID      )
               OR     ( RECE849-RECEKA    NOT =   RECE859-RECEKA    )
               OR     ( RECE849-HKNID     NOT =   RECE859-HKNID     )
               OR     ( RECE849-TEKSTYMD  NOT =   RECE859-TEKSTYMD  )
               OR     ( RECE849-ZAINUM    NOT =   RECE859-ZAINUM    )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE859-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE859-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE859-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE859-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE859-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE859-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE859-SRYSURYO(IDX)  
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE859-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25011-SANSO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素のコメント編集（差込：下）
      *****************************************************************
       25011-SANSO-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
           INITIALIZE            WRK-FREE-AREA3
      *
           PERFORM   250111-SANSO-FREE-COMMENT-SEC
      *
           PERFORM   250111-RECE859-TAIHI-SEC
      *
      *    ＲＥＣＥ８５９ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX-REC
           INITIALIZE                       WRK-RECE85XX-REC
           MOVE    ZERO                 TO  WRK-JYUNBAN
                                            WRK-GYOSUU
                                            WRK-GYOSUU2
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD       1      TO     WRK-GYOSUU
      *
           END-PERFORM 
      *
           PERFORM    250111-SRYACTINF2-HENSYU-SEC
      *
           .
       25011-SANSO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素計算式編集
      *****************************************************************
       250111-SANSO-FREE-COMMENT-SEC          SECTION.                 
      *
           MOVE     1          TO    WRK-KETA
           MOVE    ZERO        TO    WRK-KEISAN4
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                 >    10)
                       OR     (TBL-SANSO-TEN(IDX)  =  ZERO)
      *
             IF   IDX  >  1
                MOVE    "＋"   TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
             END-IF
      *
             MOVE    "（"   TO  WK-FREE-NAME3(WRK-KETA:2)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *単価
             MOVE     TBL-SANSO-TEN(IDX)   TO   WRK-KEISAN
             COMPUTE  WRK-KEISAN2  = (TBL-SANSO-TEN(IDX) -  WRK-KEISAN)
                                   *   1000
      *
             PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
             MOVE    "円×" TO  WK-FREE-NAME3(WRK-KETA:4)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  4
      *数量
             MOVE     TBL-SANSO-SRYSURYO(IDX)  TO  WRK-KEISAN
             COMPUTE  WRK-KEISAN2  
                             = (TBL-SANSO-SRYSURYO(IDX) -  WRK-KEISAN)
                             *   1000
      *
             PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
             IF    TBL-KASAN-SRYSURYO  =  ZERO
                MOVE    "Ｌ×１．３）" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                                                                + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             ELSE
                MOVE    "Ｌ×１．３×" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
                MOVE     TBL-KASAN-SRYSURYO  TO  WRK-KEISAN
                COMPUTE  WRK-KEISAN2  
                             = (TBL-KASAN-SRYSURYO -  WRK-KEISAN)
                             *   1000
      *
                PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
                MOVE    "）" TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
      ***                             * TBL-KASAN-SRYSURYO)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
                                      * TBL-KASAN-SRYSURYO)     + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             END-IF
      *
           END-PERFORM
      *
           IF   IDX  =  2
              MOVE    "÷１０＝"  TO  WK-FREE-NAME3(WRK-KETA:8)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  8
      *
      ***     COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***     COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
              COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
              MOVE     ZERO       TO   WRK-KEISAN2
      *
              PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
              MOVE    "点"   TO  WK-FREE-NAME3(WRK-KETA:2)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           ELSE
              IF   IDX  >  2
                  MOVE   WK-FREE-NAME3  TO  WK-FREE-NAME3-TAIHI
                  MOVE    "（"    TO   WK-FREE-NAME3(1:2)
                  MOVE   WK-FREE-NAME3-TAIHI
                                  TO   WK-FREE-NAME3(3:)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                  MOVE    "）÷１０＝"  TO  WK-FREE-NAME3(WRK-KETA:10)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  10
      *
      ***         COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***         COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
                  COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
                  MOVE     ZERO       TO   WRK-KEISAN2
      *
                  PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
                  MOVE    "点"   TO  WK-FREE-NAME3(WRK-KETA:2)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
              END-IF
           END-IF
      *
           IF     WRK-KETA    >   1
               PERFORM   VARYING   IDX4    FROM   1   BY   50
                          UNTIL   (IDX4     >    1000)
                            OR    (WK-FREE-NAME3(IDX4:1) = SPACE)
                   ADD     1      TO    IDZ
                   MOVE    ZERO   TO    WRK-KETA2
                   PERFORM   VARYING   IDX5    FROM   1   BY   2
                              UNTIL   (IDX5     >    50)
                      IF   WK-FREE-NAME3(IDX4:IDX5) = SPACE
                          MOVE   IDX5   TO   WRK-KETA2
                          MOVE   50     TO   IDX5
                      END-IF
                   END-PERFORM
                   IF   WRK-KETA2 =  ZERO
                        MOVE   25   TO  WK-FREE-YUKOKETA2(IDZ)
                   ELSE
                        COMPUTE   WK-FREE-YUKOKETA2(IDZ)  =
                                       ( WRK-KETA2 - 1 )  /  2
                   END-IF
                   MOVE   WK-FREE-NAME3(IDX4:50)
                                   TO    WK-FREE-NAME2(IDZ)
               END-PERFORM
           END-IF
      *
           .
       250111-SANSO-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素計算式・数値編集
      *****************************************************************
       2501111-SANSO-FREE-COMMENT-SUCHI-SEC          SECTION.          
      *
           IF    WRK-KEISAN  NOT  =  ZERO
               PERFORM   VARYING   IDX4    FROM   1   BY   1
                          UNTIL   (IDX4               >    9)
                     IF   WRK-KEISAN(IDX4:1) =   "0"
                        CONTINUE
                     ELSE
                        MOVE  IDX4   TO  IDX5
                        MOVE   9     TO  IDX4
                     END-IF
               END-PERFORM
               MOVE  SPACE             TO  WRK-KEISAN-TAIHI
               MOVE  WRK-KEISAN(IDX5:) TO  WRK-KEISAN-TAIHI
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
               CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                               TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
               END-IF
           END-IF
      *
           IF    WRK-KEISAN2  NOT  =  ZERO
                PERFORM   VARYING   IDX4    FROM   3   BY  -1
                           UNTIL   (IDX4               <    1)
                      IF   WRK-KEISAN2(IDX4:1) =   "0"
                         CONTINUE
                      ELSE
                         MOVE  IDX4   TO  IDX5
                         MOVE   1     TO  IDX4
                      END-IF
                END-PERFORM
                MOVE  SPACE               TO  WRK-KEISAN-TAIHI
                MOVE  WRK-KEISAN2(1:IDX5) TO  WRK-KEISAN-TAIHI
      *
                IF    WRK-KEISAN  NOT  =  ZERO
                   MOVE    "．"     TO  WK-FREE-NAME3(WRK-KETA:2)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                ELSE
                   MOVE    "０．"   TO  WK-FREE-NAME3(WRK-KETA:4)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                END-IF
      *
                INITIALIZE                  ORCSKANACHKAREA
                MOVE    "2"             TO  KANACHK-SYORI
                MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
                CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                IF      KANACHK-RC      =   ZERO
                    MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                    COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
                END-IF
           END-IF
      *
           IF    WRK-KEISAN    =  ZERO
           AND   WRK-KEISAN2   =  ZERO
               MOVE    "０"   TO  WK-FREE-NAME3(WRK-KETA:2)
               COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           END-IF
      *
           .
       2501111-SANSO-FREE-COMMENT-SUCHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ８５９　退避処理
      *****************************************************************
       250111-RECE859-TAIHI-SEC     SECTION.
      *
           PERFORM 800-RECE849-SET859-SEC
           MOVE    RECE849-ZAINUM       TO  RECE859-ZAINUM
           START   RECE859-FILE
                 KEY IS    >=   RECE859-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE859
                 NOT INVALID
                     PERFORM 990-RECE859-NEXT-SEC
                     MOVE   RECE859-REC TO  WRK-RECE85-REC
           END-START
      *
           .
       250111-RECE859-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ８５９　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE859           =   1                    )
             OR     ( RECE849-HOSPNUM   NOT =   RECE859-HOSPNUM      )
             OR     ( RECE849-SRYYM     NOT =   RECE859-SRYYM        )
             OR     ( RECE849-NYUGAIKBN NOT =   RECE859-NYUGAIKBN    )
             OR     ( RECE849-PTID      NOT =   RECE859-PTID         )
             OR     ( RECE849-RECEKA    NOT =   RECE859-RECEKA       )
             OR     ( RECE849-HKNID     NOT =   RECE859-HKNID        )
             OR     ( RECE849-TEKSTYMD  NOT =   RECE859-TEKSTYMD     )
             OR     ( RECE849-ZAINUM    NOT =   RECE859-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   5     )
                       OR      ( RECE859-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE85
                  MOVE   RECE859-SRYACT-TBL(IDX)
                             TO   WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
      *
                  IF   RECE859-SRYCD(IDX) = "770020070" OR "770030070"
                       MOVE   IDX-RECE85   TO   WRK-JYUNBAN
                  END-IF
      *
             END-PERFORM 
      *
             PERFORM 990-RECE859-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      補正率又は高気圧酸素加算以降の明細をずらす
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE85   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE85XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE85XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE85XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      計算式の差し込み
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE85     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1      TO  MAX-MEISAISU
                MOVE   "810000001"
                              TO  WRK-RECE85XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO   TO  WRK-RECE85XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE85XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE85XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"   TO  WRK-RECE85XX-SIKFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    手術日コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
      *
                  INITIALIZE                        RECE851-REC
                  MOVE    WRK-RECE85-KEY       TO   RECE851-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE851-RENNUM
                  MOVE    WRK-RECE85-PTNUM     TO   RECE851-PTNUM
                  MOVE    WRK-RECE85-SRYKA     TO   RECE851-SRYKA
                  MOVE    WRK-RECE85-SRYKBN    TO   RECE851-SRYKBN
                  MOVE    WRK-RECE85-SRYSYUKBN TO   RECE851-SRYSYUKBN
                  MOVE    WRK-RECE85-JIHIMONEY TO   RECE851-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE85   =   1)
                     ADD         1             TO    IDX-RECE85
                     IF  WRK-RECE85XX-SRYCD(IDX-RECE85 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE85
                     ELSE
                         MOVE  WRK-RECE85XX-SRYACT-TBL(IDX-RECE85 1)
                                          TO   RECE851-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE85XX-SIKFLG(IDX-RECE85 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE851-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE851-REC
                     ADD     1            TO   CNT-RECE85
                  END-IF
           END-PERFORM
      *
           MOVE    RECE849-REC       TO      RECE841-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE841-MEISAISU
           WRITE   RECE841-REC
           ADD     1                 TO      CNT-RECE84
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟コメントがあるか判定
      *****************************************************************
       2502-RYOCOM-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-RYOCOM2
           INITIALIZE               TBL-RYOCOM2-INF
      *
           PERFORM  VARYING  RYO-IDX  FROM  1  BY  1
                     UNTIL   RYO-IDX   >   31
      *
               IF    (RECE849-KAISU(1 RYO-IDX)   NOT =  ZERO)
               AND   (TBL-RYOCOM-NYUINKBN(RYO-IDX)   =  "1" )
      *
                  MOVE     ZERO     TO    FLG-ARI
                  PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                            UNTIL  (RYO-IDX3   >   31)
                             OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3)
                                               = ZERO)
                             OR    (FLG-ARI    =    1)
                   IF   RECE82-SRYYM  >=  "201004"
                     IF  (TBL-RYOCOM2-MEISAISU  (RYO-IDX3) =
                                    TBL-RYOCOM-MEISAISU  (RYO-IDX))
                     AND (TBL-RYOCOM2-SRYCDTOTAL(RYO-IDX3) =
                                    TBL-RYOCOM-SRYCDTOTAL(RYO-IDX))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 1) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 1))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 2) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 2))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 3) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 3))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 4) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 4))
                        MOVE   1   TO  TBL-RYOCOM2-DAY(RYO-IDX3 RYO-IDX)
                        MOVE   1   TO  FLG-ARI
                     END-IF
                   ELSE
                     IF  (TBL-RYOCOM2-MEISAISU  (RYO-IDX3) =
                                    TBL-RYOCOM-MEISAISU  (RYO-IDX))
                     AND (TBL-RYOCOM2-SRYCDTOTAL(RYO-IDX3) =
                                    TBL-RYOCOM-SRYCDTOTAL(RYO-IDX))
                     AND (TBL-RYOCOM2-ADL-TEN   (RYO-IDX3) =
                                    TBL-RYOCOM-ADL-TEN   (RYO-IDX))
                        MOVE   1   TO  TBL-RYOCOM2-DAY(RYO-IDX3 RYO-IDX)
                        MOVE   1   TO  FLG-ARI
                     END-IF
                   END-IF
                  END-PERFORM
                  IF  FLG-ARI  =  ZERO
                     MOVE  TBL-RYOCOM-AREA(RYO-IDX)
                               TO  TBL-RYOCOM2-AREA(RYO-IDX3)
                     MOVE  1   TO  TBL-RYOCOM2-DAY (RYO-IDX3 RYO-IDX)
                  END-IF
      *
                  MOVE      1      TO     FLG-RYOCOM2
      *
               END-IF
      *
           END-PERFORM
      *
           IF  FLG-RYOCOM2   =   ZERO
              PERFORM   2599-RECE849-WRITE-SEC
           ELSE
            IF   RECE82-SRYYM  >=  "201004"
              PERFORM   25021-RYOCOM-HENSYU-H22-SEC
            ELSE
              PERFORM   25021-RYOCOM-HENSYU-SEC
            END-IF
           END-IF
      *
           .
       2502-RYOCOM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟コメント編集（平成２２年４月診療〜）
      *****************************************************************
       25021-RYOCOM-HENSYU-H22-SEC          SECTION.
      *
           PERFORM  250111-RECE859-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX2-REC
           INITIALIZE                       WRK-RECE85XX2-REC
      *
           MOVE    RECE849-MEISAISU     TO  MAX-MEISAISU
      *
      *
           PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                     UNTIL  (RYO-IDX3   >   31                    )
                      OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3) = ZERO)
      *
              INITIALIZE                TBL-NYU-DAY
      *
              MOVE    ZERO          TO  DAY-IDX
              MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
              PERFORM    VARYING    IDX  FROM   1   BY   1
                          UNTIL    (IDX      >   31)  OR
                                   (DAY-IDX  >   31)
                IF    TBL-RYOCOM2-DAY(RYO-IDX3 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                END-IF
              END-PERFORM
      *
      ***     PERFORM  VARYING  DAY-IDX  FROM   1   BY   1
      ***               UNTIL  (DAY-IDX                  >   31)
      ***                OR    (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
      ***       IF    TBL-NYU-DAY-ST(DAY-IDX) 
      ***                           =   TBL-NYU-DAY-ED(DAY-IDX)
      ***             MOVE  "840000059"
      ***                           TO  TBL-NYU-DAY-CD(DAY-IDX)
      ***       ELSE
      ***             MOVE  "810000001"
      ***                           TO  TBL-NYU-DAY-CD(DAY-IDX)
      ***       END-IF
      ***     END-PERFORM
      *
      *       算定日コメントの挿入
              PERFORM  VARYING   IDX    FROM  1   BY   1
                        UNTIL   (IDX                  >    31)
                         OR     (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU   =   MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                 MOVE  "810000001"
                             TO  WRK-RECE85XX2-SRYCD(MAX-MEISAISU 1)
                 MOVE   ZERO TO  WRK-RECE85XX2-SRYSURYO(MAX-MEISAISU 1)
      ***        IF    TBL-NYU-DAY-CD(IDX)  =  "840000059"
      ***          MOVE   16 TO  WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
      ***          MOVE   "療養病棟入院期間　　　日〜　　日"
      ***                  TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
      ***          MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
      ***          MOVE   TBL-DAY(DAY-IDX2)
      ***                  TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(19:4)
      ***          MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
      ***          MOVE   TBL-DAY(DAY-IDX2)
      ***                  TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(27:4)
      ***        ELSE
                   MOVE   16 TO  WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                   EVALUATE RECE82-HOSPSBT
                     WHEN    "1"
                     WHEN    "3"
                       MOVE   "療養病棟入院期間　　　日〜　　日"
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                     WHEN    "2"
                     WHEN    "4"
                       MOVE   "療養病床入院期間　　　日〜　　日"
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                   END-EVALUATE
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(19:4)
                   MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(27:4)
      ***        END-IF
                 MOVE  "1" TO  WRK-RECE85XX2-SIKFLG(MAX-MEISAISU 1)
               END-IF
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                  UNTIL  (IDX                                  >  30  )
                   OR    (TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX) = SPACE)
      *
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                   MOVE "810000001"
                              TO WRK-RECE85XX2-SRYCD(MAX-MEISAISU 1)
                   MOVE  ZERO TO WRK-RECE85XX2-SRYSURYO(MAX-MEISAISU 1)
                   MOVE  4    TO WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE  "1"  TO WRK-RECE85XX2-SIKFLG(MAX-MEISAISU 1)
                   PERFORM       250212-RYOCOM-NAME-HENSYU-SEC
               END-IF
      *
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                UNTIL (IDX   >   4)
      *
                IF  TBL-RYOCOM2-ADL-SRYCD-H22(RYO-IDX3 IDX) NOT = SPACE
                  COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
                  IF  MAX-MEISAISU <= 300
                    MOVE   "810000001"
                             TO WRK-RECE85XX2-SRYCD(MAX-MEISAISU 1)
                    MOVE   ZERO
                             TO WRK-RECE85XX2-SRYSURYO(MAX-MEISAISU 1)
                    EVALUATE  IDX
                      WHEN     1
                        MOVE 17
                           TO WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "ＡＤＬａ　ベッド上の可動性　　　点"
                           TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(29:4)
                      WHEN     2
                        MOVE 11
                           TO WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "ＡＤＬｂ　移乗　　　点"
                           TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(17:4)
                      WHEN     3
                        MOVE 11
                           TO WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "ＡＤＬｃ　食事　　　点"
                           TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(17:4)
                      WHEN     4
                        MOVE 15
                           TO WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "ＡＤＬｄ　トイレの使用　　　点"
                           TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(25:4)
                    END-EVALUATE
                    MOVE "1" TO WRK-RECE85XX2-SIKFLG(MAX-MEISAISU 1)
                  END-IF
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *
           PERFORM    250211-SRYACTINF3-HENSYU-SEC
      *
           .
       25021-RYOCOM-HENSYU-H22-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟コメント編集
      *****************************************************************
       25021-RYOCOM-HENSYU-SEC          SECTION.
      *
           PERFORM  250111-RECE859-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE85
           MOVE    SPACE                TO  WRK-RECE85XX2-REC
           INITIALIZE                       WRK-RECE85XX2-REC
      *
           MOVE    RECE849-MEISAISU     TO  MAX-MEISAISU
      *
      *
           PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                     UNTIL  (RYO-IDX3   >   31                    )
                      OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3) = ZERO)
      *
              INITIALIZE                TBL-NYU-DAY
      *
              MOVE    ZERO          TO  DAY-IDX
              MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
              PERFORM    VARYING    IDX  FROM   1   BY   1
                          UNTIL    (IDX      >   31)  OR
                                   (DAY-IDX  >   31)
                IF    TBL-RYOCOM2-DAY(RYO-IDX3 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                END-IF
              END-PERFORM
      *
              PERFORM  VARYING  DAY-IDX  FROM   1   BY   1
                        UNTIL  (DAY-IDX                  >   31)
                         OR    (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  "840000059"
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  "810000001"
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
              END-PERFORM
      *
      *       算定日コメントの挿入
              PERFORM  VARYING   IDX    FROM  1   BY   1
                        UNTIL   (IDX                  >    31)
                         OR     (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU   =   MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                 MOVE  "810000001"
                             TO  WRK-RECE85XX2-SRYCD(MAX-MEISAISU 1)
                 MOVE   ZERO TO  WRK-RECE85XX2-SRYSURYO(MAX-MEISAISU 1)
                 IF    TBL-NYU-DAY-CD(IDX)  =  "840000059"
                   MOVE   6  TO  WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE   "　　月　　日"
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                   MOVE   RECE82-SRYYM(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(1:4)
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(7:4)
                 ELSE
                   MOVE   13 TO  WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE   "　　月　　日〜　　月　　日"
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                   MOVE   RECE82-SRYYM(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(1:4)
                               WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(15:4)
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(7:4)
                   MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(21:4)
                 END-IF
                 MOVE  "1" TO  WRK-RECE85XX2-SIKFLG(MAX-MEISAISU 1)
               END-IF
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                  UNTIL  (IDX                                  >  30  )
                   OR    (TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX) = SPACE)
      *
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                   MOVE "810000001"
                              TO WRK-RECE85XX2-SRYCD(MAX-MEISAISU 1)
                   MOVE  ZERO TO WRK-RECE85XX2-SRYSURYO(MAX-MEISAISU 1)
                   MOVE  4    TO WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE  "1"  TO WRK-RECE85XX2-SIKFLG(MAX-MEISAISU 1)
                   PERFORM       250212-RYOCOM-NAME-HENSYU-SEC
               END-IF
      *
              END-PERFORM
      *
              IF   TBL-RYOCOM2-ADL-SRYCD(RYO-IDX3)  NOT =  SPACE
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                MOVE   "810000001"
                             TO WRK-RECE85XX2-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO WRK-RECE85XX2-SRYSURYO(MAX-MEISAISU 1)
                MOVE   7     TO WRK-RECE85XX2-YUKOKETA(MAX-MEISAISU 1)
                MOVE   "ＡＤＬ　　　点"
                             TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
                MOVE   TBL-RYOCOM2-ADL-TEN-MOJI(RYO-IDX3)
                             TO WRK-RECE85XX2-NAME(MAX-MEISAISU 1)(9:4)
                MOVE   "1"   TO WRK-RECE85XX2-SIKFLG(MAX-MEISAISU 1)
               END-IF
              END-IF
      *
           END-PERFORM
      *
      *
           PERFORM    250211-SRYACTINF3-HENSYU-SEC
      *
           .
       25021-RYOCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ８５９　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       250211-SRYACTINF3-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE859           =   1                    )
             OR     ( RECE849-HOSPNUM   NOT =   RECE859-HOSPNUM      )
             OR     ( RECE849-SRYYM     NOT =   RECE859-SRYYM        )
             OR     ( RECE849-NYUGAIKBN NOT =   RECE859-NYUGAIKBN    )
             OR     ( RECE849-PTID      NOT =   RECE859-PTID         )
             OR     ( RECE849-RECEKA    NOT =   RECE859-RECEKA       )
             OR     ( RECE849-HKNID     NOT =   RECE859-HKNID        )
             OR     ( RECE849-TEKSTYMD  NOT =   RECE859-TEKSTYMD     )
             OR     ( RECE849-ZAINUM    NOT =   RECE859-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   5     )
                         OR    ( RECE859-SRYCD(IDX) =   SPACE )
                  ADD    1              TO      IDX-RECE85
                  MOVE   RECE859-SRYACT-TBL(IDX)
                           TO   WRK-RECE85XX2-SRYACT-TBL(IDX-RECE85 1)
             END-PERFORM
      *
             PERFORM 990-RECE859-NEXT-SEC
      *
           END-PERFORM
      *
           MOVE       ZERO             TO      FLG-WRKRECE85
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE85
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE85  =    1
      *
                  INITIALIZE                        RECE851-REC
                  MOVE    WRK-RECE85-KEY       TO   RECE851-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE851-RENNUM
                  MOVE    WRK-RECE85-PTNUM     TO   RECE851-PTNUM
                  MOVE    WRK-RECE85-SRYKA     TO   RECE851-SRYKA
                  MOVE    WRK-RECE85-SRYKBN    TO   RECE851-SRYKBN
                  MOVE    WRK-RECE85-SRYSYUKBN TO   RECE851-SRYSYUKBN
                  MOVE    WRK-RECE85-JIHIMONEY TO   RECE851-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                            UNTIL   (IDX             >   5)
                             OR     (FLG-WRKRECE85   =   1)
                     ADD         1             TO    IDX-RECE85
                     IF  WRK-RECE85XX2-SRYCD(IDX-RECE85 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE85
                     ELSE
                         MOVE  WRK-RECE85XX2-SRYACT-TBL(IDX-RECE85 1)
                                         TO   RECE851-SRYACT-TBL(IDX)
                         IF WRK-RECE85XX2-SIKFLG(IDX-RECE85 1) =  "1"
                              ADD   1    TO   WRK-CNT-MEISAI
                         END-IF
                     END-IF
                     IF   IDX-RECE85   >= 300
                         MOVE    1       TO   FLG-WRKRECE85
                     END-IF
                  END-PERFORM
                  IF   RECE851-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE851-REC
                     ADD     1            TO   CNT-RECE85
                  END-IF
           END-PERFORM
      *
           MOVE    RECE849-REC       TO      RECE841-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE841-MEISAISU
      *
           MOVE    RECE849-MEISAISU  TO      RECE841-RYOTENKISAISU
      *
           WRITE   RECE841-REC
           ADD     1                 TO      CNT-RECE84
      *
           .
       250211-SRYACTINF3-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟コメント名称編集
      *****************************************************************
       250212-RYOCOM-NAME-HENSYU-SEC     SECTION.
      *
           EVALUATE  TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX)
             WHEN      "820000057"
             WHEN      "820000096"
                MOVE "「０１」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000058"
             WHEN      "820000097"
                MOVE "「０２」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000059"
             WHEN      "820000098"
                MOVE "「０３」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000060"
             WHEN      "820000099"
                MOVE "「０４」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000061"
             WHEN      "820000100"
                MOVE "「０５」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000062"
             WHEN      "820000101"
                MOVE "「０６」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000063"
             WHEN      "820000102"
                MOVE "「０７」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000064"
             WHEN      "820000103"
                MOVE "「０８」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000065"
             WHEN      "820000104"
                MOVE "「０９」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000066"
             WHEN      "820000105"
                MOVE "「１０」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000067"
             WHEN      "820000106"
                MOVE "「１１」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000068"
             WHEN      "820000107"
                MOVE "「１２」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000069"
             WHEN      "820000108"
                MOVE "「１３」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000070"
             WHEN      "820000109"
                MOVE "「１４」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000071"
             WHEN      "820000110"
                MOVE "「１５」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000072"
             WHEN      "820000111"
                MOVE "「１６」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000073"
             WHEN      "820000112"
                MOVE "「１７」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000074"
             WHEN      "820000113"
                MOVE "「１８」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000075"
             WHEN      "820000114"
                MOVE "「１９」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000076"
             WHEN      "820000115"
                MOVE "「２０」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000077"
             WHEN      "820000116"
                MOVE "「２１」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000078"
             WHEN      "820000117"
                MOVE "「２２」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000079"
             WHEN      "820000118"
                MOVE "「２３」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000080"
             WHEN      "820000119"
                MOVE "「２４」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000081"
             WHEN      "820000120"
                MOVE "「２５」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000082"
             WHEN      "820000121"
                MOVE "「２６」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000083"
             WHEN      "820000122"
                MOVE "「２７」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000084"
             WHEN      "820000123"
                MOVE "「２８」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000085"
             WHEN      "820000124"
                MOVE "「２９」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000086"
             WHEN      "820000125"
                MOVE "「３０」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000087"
             WHEN      "820000126"
                MOVE "「３１」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000088"
             WHEN      "820000127"
                MOVE "「３２」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000089"
             WHEN      "820000128"
                MOVE "「３３」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000090"
             WHEN      "820000129"
                MOVE "「３４」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000091"
             WHEN      "820000130"
                MOVE "「３５」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000092"
             WHEN      "820000131"
                MOVE "「３６」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000093"
             WHEN      "820000132"
                MOVE "「３７」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000158"
                MOVE "「９１」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000159"
                MOVE "「９２」"
                       TO  WRK-RECE85XX2-NAME(MAX-MEISAISU 1)
           END-EVALUATE
      *
           .
       250212-RYOCOM-NAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル８４９出力処理
      *****************************************************************
       2599-RECE849-WRITE-SEC      SECTION.
      *
           MOVE    RECE849-REC      TO  RECE841-REC
           WRITE   RECE841-REC
           ADD     1                TO  CNT-RECE84
      *
           PERFORM 25991-RECE859-TENSOU-SEC
           .
       2599-RECE849-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト８５９出力処理
      *****************************************************************
       25991-RECE859-TENSOU-SEC     SECTION.
      *
           PERFORM 800-RECE849-SET859-SEC
           MOVE    RECE849-ZAINUM       TO  RECE859-ZAINUM
           START   RECE859-FILE
               KEY IS    >=   RECE859-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE859
               NOT INVALID
                   PERFORM 990-RECE859-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE859           =   1                    )
               OR     ( RECE849-HOSPNUM   NOT =   RECE859-HOSPNUM      )
               OR     ( RECE849-SRYYM     NOT =   RECE859-SRYYM        )
               OR     ( RECE849-NYUGAIKBN NOT =   RECE859-NYUGAIKBN    )
               OR     ( RECE849-PTID      NOT =   RECE859-PTID         )
               OR     ( RECE849-RECEKA    NOT =   RECE859-RECEKA       )
               OR     ( RECE849-HKNID     NOT =   RECE859-HKNID        )
               OR     ( RECE849-TEKSTYMD  NOT =   RECE859-TEKSTYMD     )
               OR     ( RECE849-ZAINUM    NOT =   RECE859-ZAINUM       )
      *
               MOVE    RECE859-REC       TO   RECE851-REC
               WRITE   RECE851-REC
               ADD     1                 TO   CNT-RECE85
      *
               PERFORM 990-RECE859-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25991-RECE859-TENSOU-EXT.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイルＣＬＯＳＥ処理
           PERFORM 310-FILE-CLOSE-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE82  CNT-IN  :" CNT-RECE82  
           DISPLAY "RECE849 CNT-WK  :" CNT-RECE849 
           DISPLAY "RECE859 CNT-WK  :" CNT-RECE859 
           DISPLAY "RECE84  CNT-OUT :" CNT-RECE84  
           DISPLAY "RECE85  CNT-OUT :" CNT-RECE85  
           DISPLAY "*** ORCR0870 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＣＬＯＳＥ処理
      *****************************************************************
       310-FILE-CLOSE-SEC              SECTION.
      *
           CLOSE                       RECE82-FILE
      *
           CLOSE                       RECE84-FILE
      *
           CLOSE                       RECE85-FILE
      *
           CLOSE                       RECE841-FILE     
           CLOSE                       RECE851-FILE     
      *
           CLOSE                       RECE849-FILE
           CLOSE                       RECE859-FILE
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト８２データ編集処理（８４）
      *****************************************************************
       800-RECE82-SET84-SEC           SECTION.
      *
           INITIALIZE                      RECE84-REC
           MOVE    RECE82-HOSPNUM      TO  RECE84-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE84-PTID
           MOVE    RECE82-RECEKA       TO  RECE84-RECEKA
           MOVE    RECE82-HKNID        TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE84-TEKSTYMD
      *
           .
       800-RECE82-SET84-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト８２データ編集処理（８４９）
      *****************************************************************
       800-RECE82-SET849-SEC           SECTION.
      *
           INITIALIZE                      RECE849-REC
           MOVE    RECE82-HOSPNUM      TO  RECE849-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE849-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE849-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE849-PTID
           MOVE    RECE82-RECEKA       TO  RECE849-RECEKA
           MOVE    RECE82-HKNID        TO  RECE849-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE849-TEKSTYMD
      *
           .
       800-RECE82-SET849-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト８２データ編集処理（８５９）
      *****************************************************************
       800-RECE82-SET859-SEC           SECTION.
      *
           INITIALIZE                      RECE859-REC
           MOVE    RECE82-HOSPNUM      TO  RECE859-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE859-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE859-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE859-PTID
           MOVE    RECE82-RECEKA       TO  RECE859-RECEKA
           MOVE    RECE82-HKNID        TO  RECE859-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE859-TEKSTYMD
      *
           .
       800-RECE82-SET859-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト８４データ編集処理（８５）
      *****************************************************************
       800-RECE84-SET85-SEC           SECTION.
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPNUM      TO  RECE85-HOSPNUM
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID        TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
      *
           .
       800-RECE84-SET85-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト８４９データ編集処理（８５９）
      *****************************************************************
       800-RECE849-SET859-SEC           SECTION.
      *
           INITIALIZE                       RECE859-REC
           MOVE    RECE849-HOSPNUM      TO  RECE859-HOSPNUM
           MOVE    RECE849-SRYYM        TO  RECE859-SRYYM
           MOVE    RECE849-NYUGAIKBN    TO  RECE859-NYUGAIKBN
           MOVE    RECE849-PTID         TO  RECE859-PTID
           MOVE    RECE849-RECEKA       TO  RECE859-RECEKA
           MOVE    RECE849-HKNID        TO  RECE859-HKNID
           MOVE    RECE849-TEKSTYMD     TO  RECE859-TEKSTYMD
      *
           .
       800-RECE849-SET859-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE82
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
           END-READ
      *
           .
       990-RECE82-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE84-NEXT-SEC           SECTION.
      *
           READ    RECE84-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE84
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE84
           END-READ
      *
           .
       990-RECE84-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト８４９データ読み
      ****************************************************************
       990-RECE849-NEXT-SEC           SECTION.
      *
           READ    RECE849-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE849
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE849
           END-READ
      *
           .
       990-RECE849-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE85-NEXT-SEC           SECTION.
      *
           READ    RECE85-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE85
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE85
           END-READ
      *
           .
       990-RECE85-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト８５９データ読み
      ****************************************************************
       990-RECE859-NEXT-SEC           SECTION.
      *
           READ    RECE859-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE859
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE859
           END-READ
      *
           .
       990-RECE859-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴マスタ読み込み
      *****************************************************************
       900-SANTEI-INV-SEC             SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SANTEI-REC
                   MOVE    ZERO                TO  FLG-SANTEI
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
                   INITIALIZE                      SANTEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                      SANTEI-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SANTEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       900-SANTEI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       900-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスタ読み込み
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ読み込み
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴付加読込
      *****************************************************************
       900-SANTEIPLUS-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEIPLUS-REC
               MOVE    ZERO            TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                  SANTEIPLUS-REC
               MOVE    1               TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       900-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
      *
           MOVE     PTCOM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                       TNS-TENSU-REC
grpsys     MOVE    SPA-HOSPNUM      TO      TNS-HOSPNUM
           MOVE    SRY-SRYCD(IDX12) TO      TNS-SRYCD
           MOVE    SRY-SRYYM        TO      TNS-YUKOSTYMD(1:6)
           MOVE    "01"             TO      TNS-YUKOSTYMD(7:2)
           MOVE    SRY-SRYYM        TO      TNS-YUKOEDYMD(1:6)
           MOVE    "01"             TO      TNS-YUKOEDYMD(7:2)
           MOVE    TNS-TENSU-REC    TO      MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-TBL-EXT.
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
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
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
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
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
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
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
       120-DBOPEN-EXT.
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
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBSTART-EXT.
           EXIT.
