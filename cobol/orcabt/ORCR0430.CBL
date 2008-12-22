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
       PROGRAM-ID.             ORCR0430.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      コメント編集（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/01    MCC-門脇　    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    02.09.18  自賠責対応　　　　　　
      *  01.00.02    NACL-門脇    02.11.29  複数診療科受診の
      *                                     コメント差込み
      *  01.00.03    NACL-門脇    03.02.04  剤まとめ対応等
      *  01.00.04    NACL-門脇    03.02.06  コメントカウントフラグでの
      *                                     判定追加
      *  01.00.05    NACL-門脇    03.03.03  剤番号桁修正対応
      *  01.00.06    NACL-門脇    03.03.12  ＣＴ・ＭＲＩの実施日追加
      *  01.00.07    NACL-門脇    03.04.04  傷病手当金意見書交付料の交付日編集
      *                                     療養費同意書交付料の交付日編集
      *  01.00.08    NACL-門脇    03.04.24  輸血はコメント編集対象外とする
      *  01.00.09    NACL-門脇    03.05.28  受診履歴キーの変更
      *  01.00.10    NACL-門脇    03.06.19  複数診療科受診の編集修正
      *  01.00.11    NACL-門脇    03.09.24  コメント編集修正
      *  01.00.12    NACL-門脇    03.09.29  コメント編集修正
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.01  リハ言語聴覚療法（３）追加対応
      *  01.01.02    NACL-門脇    04.04.05  在宅血液透析頻回指導管理料の
      *                                     コメント対応
      *  01.01.03    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指導料
      *                                     算定時の点滴注射のコメント対応
      *  01.01.04    NACL-門脇    04.04.15  脳血管疾患急性発症日のコメント
      *                                     対応
      *  01.01.05    NACL-門脇    04.06.07  コピー句０４５変更に伴う修正
      *  01.01.06    NACL-門脇    04.07.30  同日複数再診のコメント修正
      *  中止処理追加
      *  01.02.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.02.02    NACL-門脇    04/12/09  脳血管疾患急性発症終了日対応
      *  01.02.03    NACL-門脇    05/01/18  酸素計算式コメント対応
      *  01.02.04    NACL-門脇    05/04/20  内服薬剤逓減編集対応
      *  01.02.05    NACL-門脇    05/05/10  脳血管疾患急性発症病名記載対応
      *  01.02.06    NACL-門脇    05/08/22  リハビリ実施回数差し込み修正対応
      *  01.02.07    NACL-門脇    05/08/31  自賠責従来様式対応
      *  01.02.08    NACL-門脇    05/11/01  （検選）等コメント対応
      *  01.02.09    NACL-門脇    05/11/15  傷病手当金、療養費同意書の自動
      *                                     記載編集修正
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  02.09.02    NACL-門脇    06/04/28  リハ検索条件修正
      *  02.09.03    NACL-門脇    06/05/01  リハ発症日当月分記載修正
      *  02.09.04    NACL-門脇    05/05/10  内服薬剤逓減編集修正
      *  02.09.05    NACL-門脇    06/05/30  リハ発症日記載修正
      *  03.01.01    NACL-門脇    06/06/22  往診、在宅患者訪問診療料の
      *                                     コメント自動記載対応
      *  03.01.02    NACL-門脇    06/07/10  在支援コメント自動記載対応
      *  03.01.03    NACL-門脇    06/07/14  診療区分置き換えフラグの編集
      *  03.02.01    NACL-門脇    06/08/01  リハ発症日記載修正
      *  03.02.02    NACL-門脇    06/09/07  内服薬剤逓減編集修正
      *  03.03.01    NACL-門脇    06/11/10  往診のコメント自動記載条件修正
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *  03.04.02    NACL-門脇    07/01/12  リハビリ発症終了日対応
      *  03.04.03    NACL-門脇    07/01/15  リハビリ手術日・急性増悪日記載対応
      *  03.04.04    NACL-門脇    07/03/05  酸素計算式修正
      *  03.04.05    NACL-門脇    07/04/13  リハビリ対応
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/06/26  自賠責平成１９年４月改正様式対応
      *  03.05.04    NACL-門脇    07/08/07  算定履歴に関わる記載修正
      *  04.01.01    NACL-門脇    07/10/31  公務災害対応
      *  04.01.02    NACL-門脇    07/11/26  特定器材商品名記載対応
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *                                     （小児特定疾患カウンセリング料
      *                                       の自動記載対応）
      *                                     （在支病自動記載対応）
      *  04.02.02    NACL-門脇    08/10/14  内服薬剤逓減編集修正
      *  04.02.03    NACL-門脇    08/12/22  内服薬剤逓減編集修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
      *
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
      *
           SELECT  RECE441-FILE    ASSIGN  RECE441PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE441-KEY
                                   FILE    STATUS  IS  STS-RECE441.
           SELECT  RECE4441-FILE   ASSIGN  RECE441PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4441-KEY
                                   FILE    STATUS  IS  STS-RECE441.
      *
           SELECT  RECE451-FILE    ASSIGN  RECE451PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE451-KEY
                                   FILE    STATUS  IS  STS-RECE451.
           SELECT  RECE4551-FILE   ASSIGN  RECE451PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4551-KEY
                                   FILE    STATUS  IS  STS-RECE451.
      *ワーク
           SELECT  RECE449-FILE    ASSIGN  RECE449PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE449-KEY
                                   FILE    STATUS  IS  STS-RECE449.
           SELECT  RECE4449-FILE   ASSIGN  RECE449PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4449-KEY
                                   FILE    STATUS  IS  STS-RECE449.
      *
           SELECT  RECE459-FILE    ASSIGN  RECE459PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE459-KEY
                                   FILE    STATUS  IS  STS-RECE459.
           SELECT  RECE4559-FILE   ASSIGN  RECE459PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4559-KEY
                                   FILE    STATUS  IS  STS-RECE459.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC.
           COPY    "CPRCF044.INC".
      *
       FD  RECE45-FILE.
       01  RECE45-REC.
           COPY    "CPRCF045.INC".
      *
       FD  RECE441-FILE.
       01  RECE441-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE441//.
      *
       FD  RECE451-FILE.
       01  RECE451-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE451//.
      *
       FD  RECE4441-FILE.
       01  RECE4441-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE4441//.
      *
       FD  RECE4551-FILE.
       01  RECE4551-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE4551//.
      *ワーク
       FD  RECE449-FILE.
       01  RECE449-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE449//.
      *
       FD  RECE459-FILE.
       01  RECE459-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE459//.
      *
       FD  RECE4449-FILE.
       01  RECE4449-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //RECE4449//.
      *
       FD  RECE4559-FILE.
       01  RECE4559-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE4559//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE441//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE451//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE449//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE459//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE41          PIC X(02).
           03  STS-RECE44          PIC X(02).
           03  STS-RECE441         PIC X(02).
           03  STS-RECE45          PIC X(02).
           03  STS-RECE451         PIC X(02).
           03  STS-RECE449         PIC X(02).
           03  STS-RECE459         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE44          PIC 9(01).
           03  FLG-RECE449         PIC 9(01).
           03  FLG-RECE45          PIC 9(01).
           03  FLG-RECE459         PIC 9(01).
           03  FLG-WRKRECE45       PIC 9(01).
           03  FLG-SHIDOU-COMMENT  PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT2 PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-YAKUTEI         PIC 9(01).
           03  FLG-YAKUTEI2        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-KYUSEI-OK       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-NIKOCHINTOU-COMMENT
                                   PIC 9(01).
           03  FLG-RIHA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-OSIN-HENSYU     PIC 9(01).
           03  FLG-COMOKIKAE       PIC 9(01).
           03  FLG-COMOKIKAE2      PIC 9(01).
           03  FLG-SYOUNI-COMMENT  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE44          PIC 9(06).
           03  CNT-RECE45          PIC 9(06).
           03  CNT-RECE449         PIC 9(06).
           03  CNT-RECE459         PIC 9(06).
           03  CNT-RIHA            PIC 9(06).
           03  CNT-JUSHIN          PIC 9(06).
           03  CNT-SANTEIKA        PIC 9(06).
           03  CNT-DOUJITU         PIC 9(06).
           03  CNT-DENWA           PIC 9(06).
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
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(04).
           03  IDX9                PIC 9(04).
           03  IDX10               PIC 9(04).
           03  IDX11               PIC 9(04).
           03  IDX12               PIC 9(04).
           03  IDX13               PIC 9(04).
           03  IDX15               PIC 9(01).
           03  IDX16               PIC 9(02).
           03  IDX17               PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  DAY-IDX3            PIC 9(01).
           03  IDX-RECE45          PIC 9(04).
           03  IDV                 PIC 9(04).
           03  IDW                 PIC 9(04).
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
           03  WRK-ZAINUM          PIC 9(08).
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
           03  WRK-NIKOCHINTOU-YMD PIC X(08).
      *
           03  WRK-HENSYU-KAISU    PIC X(02).
           03  WRK-HENSYU-KAISU2   PIC X(04).
      *
           03  WRK-HENSYU-KASANRITU   PIC X(01).
           03  WRK-HENSYU-KASANRITU2  PIC X(02).
      *
           03  WRK-RUIKEITOTAL     PIC 9(15)V9(03).
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
           03  WRK-HKNCOMBINUMX      PIC X(04).
           03  WRK-HKNCOMBINUM9      REDEFINES  WRK-HKNCOMBINUMX
                                     PIC 9(04).
      *
           03  WRK-SRYYMDX.
               05  WRK-SRYYM         PIC 9(06).
               05  WRK-SRYDD         PIC 9(02).
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
           03  WRK-SYOUNI-FSANTEIYMD         PIC X(08).
      *
           03  WRK-MCP-TABLE         PIC X(64).
           03  WRK-MCP-PATHNAME      PIC X(64).
      *
       01  WRK-RECE44-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //WRK-RECE44//.
      *
       01  WRK-RECE45-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45//.
      *
       01  WRK-RECE45XX-REC.
         02 WRK-RECE45XX-TBL       OCCURS 50.
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45XX//.
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
      *    慢性とう痛疾患
           03  CONS-MANSEI          PIC X(09) VALUE "113006510".
      *    在宅血液透析指導管理料
           03  CONS-TOSEKI          PIC X(09) VALUE "114009310".
      *    脳血管疾患急性発症日
           03  CONS-KYUSEI          PIC X(09) VALUE "099800101".
      *    ニコチン依存症管理料（初回）
           03  CONS-NIKOCHIN        PIC X(09) VALUE "113008310".
      *    重度喘息患者治療管理加算（１月目）
           03  CONS-ZENSOKU         PIC X(09) VALUE "113008070".
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
      *    小児特定疾患カウンセリング料（１回目）
           03  CONS-SYOUNI          PIC X(09) VALUE "113000810".
      *
      *    複数診療科受診の退避テーブル
       01  TBL-JUSHIN-INF.
           03  TBL-JUSHIN-INF2      OCCURS    50.
               05  TBL-JUSHIN-SRYKA       PIC X(02).
               05  TBL-JUSHIN-SRYKANAME   PIC X(80).
               05  TBL-JUSHIN-ARI         PIC X(01).
               05  TBL-JUSHIN-DAY         PIC X(01)   OCCURS  31.
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
               05  TBL-SANSO-SRYSURYO     PIC 9(05)V9(03).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO     PIC 9(05)V9(03).
      *
      *    薬剤逓減情報
       01  TBL-YAKUTEI-INF.
           03  TBL-YT1-OCC                      OCCURS 100.
             05  TBL-YT1-KEY.
               07  TBL-YT1-SRYKA                PIC X(02).
             05  TBL-YT1-RECE44-REC             PIC X(300).
             05  TBL-YT1-HIKAKU.
               07  TBL-YT1-ZAITEN               PIC 9(08).
               07  TBL-YT1-SRYCDTOTAL           PIC 9(14).
               07  TBL-YT1-SURYOUTOTAL          PIC 9(07)V9(03).
               07  TBL-YT1-RUIKEITOTAL          PIC 9(15)V9(03).
               07  TBL-YT1-MEISAISU             PIC 9(04).
             05  TBL-YT1-RECE44-DAY-AREA.
               07  TBL-YT1-OCC2                 OCCURS  31.
                 09  TBL-YT1-OCC3               OCCURS   3.
                   11  TBL-YT1-KAISU            PIC 9(03).
             05  TBL-YT1-FLG                    PIC 9(01).
      *
      *    並び替え用
       01  WRK-YAKUTEI-DATA                     PIC X(636).
      *
       01  WRK-TAIHI-YT1-KEY                    PIC X(02).
       01  WRK-YAKUTEI-ZAINUM                   PIC 9(08).
       01  WRK-YAKUTEI-ZAINUM2                  PIC 9(08).
      *
      *    薬剤逓減情報（キー単位の編集テーブル）
       01  TBL-YAKUTEI-INF2.
           03  TBL-YT2-DAY                    OCCURS      31.
             05  TBL-YT2X-KAIT                  OCCURS     3.
               06  TBL-YT2X1-OCC                  OCCURS  25.
                 07  TBL-YT2X1-RECE44-REC         PIC X(300).
                 07  TBL-YT2X1-KAISU              PIC 9(03).
                 07  TBL-YT2X1-DAY-AREA.
                   09  TBL-YT2X1-OCC2               OCCURS  31.
                       11  TBL-YT2X1-DAY            PIC 9(03).
               06  TBL-YT2X2-OCC                  OCCURS   3.
                 07  TBL-YT2X2-RECE44-REC         PIC X(300).
                 07  TBL-YT2X2-KAISU              PIC 9(03).
                 07  TBL-YT2X2-DAY-AREA.
                   09  TBL-YT2X2-OCC2               OCCURS  31.
                       11  TBL-YT2X2-DAY            PIC 9(03).
             05  TBL-YT2Y-KAIT                  OCCURS     3.
               06  TBL-YT2Y1-OCC                  OCCURS  26.
                 07  TBL-YT2Y1-HIKAKU.
                   09  TBL-YT2Y1-ZAITEN           PIC 9(08).
                   09  TBL-YT2Y1-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT2Y1-SURYOUTOTAL      PIC 9(07)V9(03).
                   09  TBL-YT2Y1-RUIKEITOTAL      PIC 9(15)V9(03).
                   09  TBL-YT2Y1-MEISAISU         PIC 9(04).
               06  TBL-YT2Y2-OCC                  OCCURS   4.
                 07  TBL-YT2Y2-HIKAKU.
                   09  TBL-YT2Y2-ZAITEN           PIC 9(08).
                   09  TBL-YT2Y2-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT2Y2-SURYOUTOTAL      PIC 9(07)V9(03).
                   09  TBL-YT2Y2-RUIKEITOTAL      PIC 9(15)V9(03).
                   09  TBL-YT2Y2-MEISAISU         PIC 9(04).
      *
      *    薬剤逓減情報（まとめ用の編集テーブル）
       01  TBL-YAKUTEI-INF3.
           03  TBL-YT3-XXX                    OCCURS     100.
             05  TBL-YT3X-KAIT.
               06  TBL-YT3X1-OCC                  OCCURS  25.
                 07  TBL-YT3X1-RECE44-REC         PIC X(300).
                 07  TBL-YT3X1-KAISU              PIC 9(03).
                 07  TBL-YT3X1-DAY-AREA.
                   09  TBL-YT3X1-OCC2               OCCURS  31.
                       11  TBL-YT3X1-DAY            PIC 9(03).
               06  TBL-YT3X2-OCC                  OCCURS   3.
                 07  TBL-YT3X2-RECE44-REC         PIC X(300).
                 07  TBL-YT3X2-KAISU              PIC 9(03).
                 07  TBL-YT3X2-DAY-AREA.
                   09  TBL-YT3X2-OCC2               OCCURS  31.
                       11  TBL-YT3X2-DAY            PIC 9(03).
             05  TBL-YT3Y-KAIT.
               06  TBL-YT3Y1-OCC                  OCCURS  26.
                 07  TBL-YT3Y1-HIKAKU.
                   09  TBL-YT3Y1-ZAITEN           PIC 9(08).
                   09  TBL-YT3Y1-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT3Y1-SURYOUTOTAL      PIC 9(07)V9(03).
                   09  TBL-YT3Y1-RUIKEITOTAL      PIC 9(15)V9(03).
                   09  TBL-YT3Y1-MEISAISU         PIC 9(04).
               06  TBL-YT3Y2-OCC                  OCCURS   4.
                 07  TBL-YT3Y2-HIKAKU.
                   09  TBL-YT3Y2-ZAITEN           PIC 9(08).
                   09  TBL-YT3Y2-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT3Y2-SURYOUTOTAL      PIC 9(07)V9(03).
                   09  TBL-YT3Y2-RUIKEITOTAL      PIC 9(15)V9(03).
                   09  TBL-YT3Y2-MEISAISU         PIC 9(04).
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
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0430"      TO  JOB-PGID
               MOVE    "コメント編集（労災・自賠責）" 
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *    
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    "RECE44W"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
      *
           MOVE    "RECE450"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    "RECE44A"           TO  RECE441PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE441PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE441PARA-HOSPNUM
      *
           MOVE    "RECE45A"           TO  RECE451PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE451PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE451PARA-HOSPNUM
      *
           MOVE    "RECE44Y"           TO  RECE449PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE449PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE449PARA-HOSPNUM
      *
           MOVE    "RECE45Y"           TO  RECE459PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE459PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE459PARA-HOSPNUM
      *
           OPEN    INPUT               RECE41-FILE
      *
           OPEN    INPUT               RECE44-FILE
      *
           OPEN    INPUT               RECE45-FILE
      *
           OPEN    OUTPUT              RECE4441-FILE
           CLOSE                       RECE4441-FILE
      *
           OPEN    OUTPUT              RECE4551-FILE
           CLOSE                       RECE4551-FILE
      *
           OPEN    OUTPUT              RECE4449-FILE
           CLOSE                       RECE4449-FILE
      *
           OPEN    OUTPUT              RECE4559-FILE
           CLOSE                       RECE4559-FILE
      *
           OPEN    OUTPUT              RECE441-FILE
      *
           OPEN    OUTPUT              RECE451-FILE
      *
           OPEN    I-O                 RECE449-FILE
      *
           OPEN    I-O                 RECE459-FILE
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
           MOVE       ZERO             TO  CNT-SANTEIKA
                                           CNT-DOUJITU
                                           CNT-DENWA
      *
           INITIALIZE                      TBL-HASSYO-INF
           INITIALIZE                      TBL-ZAISIEN-INF
      *
           INITIALIZE                      TBL-YAKUTEI-INF
      *
           MOVE       ZERO             TO  FLG-OSIN-HENSYU
      *
           MOVE       ZERO             TO  IDX6
      *
           IF   RECE41-COMCNTFLG   =   "1"
      *
             PERFORM 800-RECE41-SET44-SEC
             MOVE    ZERO                TO  RECE44-ZAINUM
      *
             START   RECE44-FILE
                 KEY IS    >=   RECE44-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE44
                 NOT INVALID
                     PERFORM 990-RECE44-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               IF   RECE41-HKNKBN    NOT  =  "4"                        
                  EVALUATE      RECE44-SRYKBN
                      WHEN      "11"
      *               （初診料算定科：のカウント）
                         IF  RECE44-SYOSANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
                      WHEN      "12"
      *               （再診料算定科：のカウント）
                         IF  RECE44-SAISANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
      *               （電話再診のカウント）
                         IF  RECE44-DENWASAI-COMMENT     =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DENWA
                         END-IF
      *               （同日再診のカウント）
                         IF  RECE44-DOUJITUSAI-COMMENT   =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUJITU
                         END-IF
                      WHEN      "14"
      *               （在支援に関する診療のカウント）
                         IF  RECE44-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                            MOVE  RECE44-REC  TO  WRK-RECE44-REC
                            EVALUATE  RECE44-FLG-ZAISIEN-COMMENT
                              WHEN      "1"
                                MOVE  1  TO  TBL-ZAISIEN-COMMENT
                              WHEN      "2"
                                MOVE  2  TO  TBL-ZAISIEN-COMMENT
                            END-EVALUATE
                         END-IF
      *               （往診に関する編集）
                         IF  RECE44-FLG-OSIN-COMMENT     =  "2"
                            MOVE  1           TO  FLG-OSIN-HENSYU
                         END-IF
                      WHEN      "40"
      *               （介達牽引あり）
                         IF  RECE44-KAITATU-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KAITATU-COMMENT
                         END-IF
                      WHEN      "80"
      *               （リハビリあり）
                         IF  RECE44-SINDAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SINDAI-COMMENT
                         END-IF
                         IF  RECE44-KOKYUKI-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KOKYUKI-COMMENT
                         END-IF
                         IF  RECE44-NOUKE-COMMENT        =  "1"
                            MOVE   1   TO  TBL-HASSYO-NOUKE-COMMENT
                         END-IF
                         IF  RECE44-UNDOKI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-UNDOKI-COMMENT
                         END-IF
                         IF  RECE44-NANBYO-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-NANBYO-COMMENT
                         END-IF
                         IF  RECE44-SYOGAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SYOGAI-COMMENT
                         END-IF
                         IF  RECE44-SESSYOKU-COMMENT     =  "1"
                            MOVE   1   TO  TBL-HASSYO-SESSYOKU-COMMENT
                         END-IF
                  END-EVALUATE
               ELSE                                                     
                  EVALUATE      RECE44-SRYKBN  
                      WHEN      "11"
      *               （初診料算定科：のカウント）
                         IF  RECE44-SYOSANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
                      WHEN      "12"
      *               （再診料算定科：のカウント）
                         IF  RECE44-SAISANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
      *               （電話再診のカウント）
                         IF  RECE44-DENWASAI-COMMENT     =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DENWA
                         END-IF
      *               （同日再診のカウント）
                         IF  RECE44-DOUJITUSAI-COMMENT   =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUJITU
                         END-IF
                      WHEN      "14"
      *               （在支援に関する診療のカウント）
                         IF  RECE44-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                            MOVE  RECE44-REC  TO  WRK-RECE44-REC
                            EVALUATE  RECE44-FLG-ZAISIEN-COMMENT
                              WHEN      "1"
                                MOVE  1  TO  TBL-ZAISIEN-COMMENT
                              WHEN      "2"
                                MOVE  2  TO  TBL-ZAISIEN-COMMENT
                            END-EVALUATE
                         END-IF
      *               （往診に関する編集）
                         IF  RECE44-FLG-OSIN-COMMENT     =  "2"
                            MOVE  1           TO  FLG-OSIN-HENSYU
                         END-IF
                      WHEN      "40"
                      WHEN      "80"
      *               （介達牽引あり）
                         IF  RECE44-KAITATU-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KAITATU-COMMENT
                         END-IF
      *               （リハビリあり）
                         IF  RECE44-SINDAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SINDAI-COMMENT
                         END-IF
                         IF  RECE44-KOKYUKI-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KOKYUKI-COMMENT
                         END-IF
                         IF  RECE44-NOUKE-COMMENT        =  "1"
                            MOVE   1   TO  TBL-HASSYO-NOUKE-COMMENT
                         END-IF
                         IF  RECE44-UNDOKI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-UNDOKI-COMMENT
                         END-IF
                         IF  RECE44-NANBYO-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-NANBYO-COMMENT
                         END-IF
                         IF  RECE44-SYOGAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SYOGAI-COMMENT
                         END-IF
                         IF  RECE44-SESSYOKU-COMMENT     =  "1"
                            MOVE   1   TO  TBL-HASSYO-SESSYOKU-COMMENT
                         END-IF
                  END-EVALUATE                                          
               END-IF                                                   
      *
               PERFORM 990-RECE44-NEXT-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    発症日のコメント
           IF   RECE41-SRYYM      >=    "200604"
               PERFORM   2001-HASSYO-HANTEI-SEC
           ELSE
               MOVE      ZERO            TO   FLG-COMOKIKAE
               MOVE      CONS-KYUSEI     TO   WRK-SRYCD
               PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      *
      *    複数診療科受診
      *
           IF   CNT-SANTEIKA   >  ZERO
      *
             MOVE       ZERO             TO  CNT-JUSHIN
             INITIALIZE                      TBL-JUSHIN-INF
      *      診療科の取得
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE41-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key2"            TO  MCP-PATHNAME
               PERFORM   900-SYSKANRI-READ-SEC
               PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX             >      50)
                            OR    (FLG-SYSKANRI    =       1)
                   MOVE    SYS-1005-KBNCD  TO TBL-JUSHIN-SRYKA(IDX)
                   MOVE    SYS-1005-SRYKANAME   
                                           TO TBL-JUSHIN-SRYKANAME(IDX)
      *            受診履歴テーブルから診療科毎の受診日を検索セット
                   PERFORM   2001-JUSHIN-KENSAKU-SEC
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-READ-SEC
               END-PERFORM
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
      *      複数の診療科受診があるか判定
             MOVE       ZERO       TO   IDZ  
             INITIALIZE            WRK-FREE-AREA
      *
             PERFORM   VARYING   IDX    FROM   1   BY   1
                            UNTIL   (IDX             >      50)
               IF   TBL-JUSHIN-ARI (IDX)  =  "1"
                   ADD       1     TO   CNT-JUSHIN
                   PERFORM  2001-FREE-COMMENT-SEC
               END-IF
             END-PERFORM
      *      複数診療科受診のコメント編集
             IF    CNT-JUSHIN  >   1
                    PERFORM   2001-JUSHIN-COMMENT-SEC
             END-IF
      *
           END-IF
      *
      *    電話再診・同日複数再診
      *
           IF     CNT-DENWA       >   ZERO
           AND    CNT-DOUJITU     >   ZERO
                   PERFORM    2001-DENWA-DOUJITU-COMMENT-SEC
           ELSE
              IF   CNT-DENWA      >   ZERO
              AND  CNT-DOUJITU    =   ZERO
                   PERFORM    2001-DENWA-COMMENT-SEC
              ELSE
                IF   CNT-DENWA    =   ZERO
                AND  CNT-DOUJITU  >   ZERO
                   PERFORM    2001-DOUJITU-COMMENT-SEC
                END-IF
              END-IF
           END-IF
      *
      *    自賠責従来様式
           IF   RECE41-HKNKBN     =   "4"
           AND  WRK-PARA-RECEKBN  =   "5"
             EVALUATE  RECE41-JIBAIKBN
      *        加算率
               WHEN     "1"
                  PERFORM    2001-KASANRITU-COMMENT-SEC
               WHEN     "2"
      *           薬剤等点単価
                  IF    RECE41-SYUTENTANKA  =  RECE41-YKZTENTANKA
                     CONTINUE
                  ELSE
                     PERFORM    2001-YKZTENTANKA-COMMENT-SEC
                  END-IF
      *           乳幼児・幼児のコメント
                  IF    RECE41-NYUYOJIFLG   =  SPACE
                     CONTINUE
                  ELSE
                     PERFORM    2001-NYUYOJI-COMMENT-SEC
                  END-IF
             END-EVALUATE
           END-IF
      *
      *    在支援コメント
           IF     TBL-ZAISIEN-COMMENT   NOT =  ZERO
               PERFORM     2001-ZAISIEN-COMMENT-SEC
           END-IF
      *
      *    パラメータ０４ファイルより編集
           PERFORM 800-RECE41-SET44-SEC
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 990-RECE44-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               IF   RECE41-HKNKBN    NOT  =  "4"                        
                 EVALUATE      RECE44-SRYKBN                   
                   WHEN      "11"
                      IF   RECE41-HKNKBN     =   "5"
                      AND  RECE41-KOMUKBN    =   "2"
                          MOVE   ZERO   TO  FLG-COMMENT
      *                （初診（同日複数診療科）算定科のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-SYODOU-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2099-RECE44-TENSOU-SEC
                          END-IF
                      ELSE
                          PERFORM      2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "13"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （慢性疼痛疾患管理料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SHIDOU-HANTEI-SEC
                      END-IF
      *            （傷病手当金意見書交付料交付日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
      *            （療養費同意書交付料交付日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
      *            （ニコチン依存症管理料初回算定日のセット）
      *            （重度喘息患者治療管理加算第１回目治療管理日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NIKOCHINTOU-HANTEI-SEC
                      END-IF
      *            （小児特定疾患カウンセリング料初回算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOUNI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
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
      *            （在宅患者訪問診療料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "21"
                      IF   RECE44-COMCDFLG   =   "3"
                        PERFORM    2001-YAKUTEI-HENSYU-SEC
                      ELSE
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "33"
      *            （在宅患者訪問点滴注射管理指導料算定時の点滴注射の
      *              コメントのセット）
                      PERFORM    2001-HOUTEN-HANTEI-SEC
                   WHEN      "50"
      *            （手術日のセット）
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
      *            （（検選）日のセット）
                      PERFORM    2001-KENSEN-HANTEI-SEC
                   WHEN      "70"
      *            （ＣＴ・ＭＲＩ実施日のセット）
                      PERFORM    2001-CTMRI-HANTEI-SEC
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （リハビリのセット）
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE44-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE44-RIHABIRI-COMMENT      =  "1"
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
                            PERFORM   2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN   OTHER
                      PERFORM       2099-RECE44-TENSOU-SEC
                 END-EVALUATE
               ELSE                                                     
                 EVALUATE      RECE44-SRYKBN                            
                   WHEN      "11"
                      EVALUATE   RECE41-JIBAIKBN
                        WHEN       "1"
                          PERFORM      2099-RECE44-TENSOU-SEC
                        WHEN       "2"
                          MOVE   ZERO   TO  FLG-COMMENT
      *                （初診（同日複数診療科）算定科のセット）
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-SYODOU-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2099-RECE44-TENSOU-SEC
                          END-IF
                      END-EVALUATE
                   WHEN      "13"                                       
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （慢性疼痛疾患管理料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SHIDOU-HANTEI-SEC
                      END-IF
      *            （傷病手当金意見書交付料交付日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
      *            （療養費同意書交付料交付日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
      *            （ニコチン依存症管理料初回算定日のセット）
      *            （重度喘息患者治療管理加算第１回目治療管理日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NIKOCHINTOU-HANTEI-SEC
                      END-IF
      *            （小児特定疾患カウンセリング料初回算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOUNI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
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
      *            （在宅患者訪問診療料算定日のセット）
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "21"
                      IF   RECE44-COMCDFLG   =   "3"
                        PERFORM    2001-YAKUTEI-HENSYU-SEC
                      ELSE
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "33"
      *            （在宅患者訪問点滴注射管理指導料算定時の点滴注射の
      *              コメントのセット）
                      PERFORM    2001-HOUTEN-HANTEI-SEC
                   WHEN      "50"                                       
      *            （手術日のセット）                                   
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
      *            （（検選）日のセット）
                      PERFORM    2001-KENSEN-HANTEI-SEC
                   WHEN      "70"
      *            （ＣＴ・ＭＲＩ実施日のセット）
                      PERFORM    2001-CTMRI-HANTEI-SEC
                   WHEN      "40"          
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            （リハビリのセット）
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE44-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE44-RIHABIRI-COMMENT      =  "1"
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
                            PERFORM   2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN   OTHER                                         
                      PERFORM       2099-RECE44-TENSOU-SEC
                 END-EVALUATE                                           
               END-IF                                                   
      *
               PERFORM 990-RECE44-NEXT-SEC
      *
           END-PERFORM
      *
      *****内服薬剤料逓減に関する編集２****
           PERFORM     240-YAKUTEI-HENSYU2-SEC
      *
      *****差込編集２********************
           PERFORM     250-SASHIKOMI2-SEC
      *
           PERFORM 990-RECE41-READ-SEC
      *
           IF  CNT-IN-COMMIT   =    300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
           END-IF
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴テーブルから診療科毎の受診日を検索セット
      *****************************************************************
       2001-JUSHIN-KENSAKU-SEC             SECTION.
      *
      *    受診歴の検索
           INITIALIZE                      JYURRK-REC
           MOVE    RECE41-HOSPNUM      TO  JYURRK-HOSPNUM
           MOVE    RECE41-PTID         TO  JYURRK-PTID
           MOVE    RECE41-NYUGAIKBN    TO  JYURRK-NYUGAIKBN
           MOVE    TBL-JUSHIN-SRYKA(IDX)
                                       TO  JYURRK-SRYKA
           MOVE    RECE41-SRYYM        TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    RECE41-SRYYM        TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"    TO  MCP-TABLE
               MOVE    "key24"         TO  MCP-PATHNAME
               PERFORM   900-JYURRK-READ-SEC
               PERFORM   UNTIL   FLG-JYURRK   =    1
                  IF   RECE41-HKNKBN    NOT  =  "4" 
                   IF    JYURRK-HKNKBN    =       "1"  OR  "5"
                      MOVE   JYURRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUMX
                      IF  WRK-HKNCOMBINUM9  =  RECE41-HKNCOMBI
                          IF  RECE41-HKNKBN   NOT =  "3"
                               MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                               IF   RECE41-JNISSUTBL(WRK-SRYDD) = SPACE
                                  CONTINUE
                               ELSE
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                               END-IF
                          ELSE
                            IF  JYURRK-SRYYMD  =  RECE41-TEKSTYMD
                               MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                               IF   RECE41-JNISSUTBL(WRK-SRYDD) = SPACE
                                  CONTINUE
                               ELSE
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                               END-IF
                            END-IF
                          END-IF
                      END-IF      
                   END-IF
                  ELSE
                   IF    JYURRK-HKNKBN    =       "2"  OR  "4"
                      MOVE   JYURRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUMX
                      IF  WRK-HKNCOMBINUM9  =  RECE41-HKNCOMBI
                            MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                            IF   RECE41-JNISSUTBL(WRK-SRYDD) = SPACE
                               CONTINUE
                            ELSE
                               MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                               MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                            END-IF
                      END-IF      
                   END-IF
                  END-IF
      *
                  MOVE    "tbl_jyurrk"    TO  MCP-TABLE
                  MOVE    "key24"         TO  MCP-PATHNAME
                  PERFORM   900-JYURRK-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-JUSHIN-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数診療科受診の科・日付の編集
      *****************************************************************
       2001-FREE-COMMENT-SEC          SECTION.
      *
           ADD      1                      TO  IDZ
           MOVE     ZERO                   TO  WRK-KETA
                                               WRK-KETA-TAIHI
                                               WRK-KETA2
      *
           IF  TBL-JUSHIN-SRYKANAME(IDX)  NOT =  SPACE
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "2"                     TO  KANACHK-SYORI 
             MOVE  TBL-JUSHIN-SRYKANAME(IDX) TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                 COMPUTE WRK-KETA          =   KANACHK-MAX
                 COMPUTE WRK-KETA-TAIHI    =   KANACHK-MAX
                 COMPUTE WRK-KETA2         =   KANACHK-MAX  /  2
                 MOVE    WRK-KETA2   TO  WK-FREE-YUKOKETA(IDZ)
                 MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                   TO  WK-FREE-NAME(IDZ)(1:KANACHK-MAX)
             END-IF
           END-IF
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDY    FROM   1   BY   1
                          UNTIL   (IDY             >      31)
      *
             IF  TBL-JUSHIN-DAY(IDX IDY) =  "1"
                 IF  FLG-FIRST  =  ZERO
                    IF  IDY     <  10 
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "　"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "日"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                    ELSE
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "　"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)     
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "日"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  IDY     <  10 
                       IF    WK-FREE-YUKOKETA(IDZ)  =  25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA(IDZ)  =  24 OR 25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
                       IF    WK-FREE-YUKOKETA(IDZ)  =  25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA(IDZ)  =  23 OR 24 OR 25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
             END-IF
      *
           END-PERFORM
           .
       2001-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数診療科受診の科・日付の編集（改行）
      *****************************************************************
       20011-FREE-COMMENT-KAIGYO-SEC          SECTION.
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA(IDZ)
           MOVE    ALL "　"        TO  WK-FREE-NAME(IDZ)(1:WRK-KETA)
      *
           .
       20011-FREE-COMMENT-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数診療科受診のコメント編集
      *****************************************************************
       2001-JUSHIN-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           ADD     1            TO  IDX-RECE45
           MOVE    "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE    ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
           MOVE    7            TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
           MOVE    "複数診療科受診"
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
           MOVE    "1"          TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   49    )
                         OR      ( WK-FREE-NAME(IDX) =   SPACE )
                ADD    1        TO  IDX-RECE45
                MOVE   "810000001"
                                TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO     TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA(IDX)
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME(IDX)
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   "1"      TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM 
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                PERFORM 800-RECE41-SET459-SEC
      *         MOVE    ZERO                TO  RECE459-ZAINUM
                MOVE    99999999            TO  RECE459-ZAINUM
                MOVE    "99"                TO  RECE459-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE45   =   1)
                   ADD         1            TO  IDX-RECE45
                   IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                       MOVE    1            TO  FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE459-REC
                   ADD     1            TO   CNT-RECE459
                END-IF
           END-PERFORM     
      *
      *
           PERFORM 800-RECE41-SET449-SEC
      *    MOVE    ZERO                TO  RECE449-ZAINUM
           MOVE    99999999            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           .
       2001-JUSHIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電話再診・同日複数再診のコメント編集
      *****************************************************************
       2001-DENWA-DOUJITU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999991            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    2                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999991            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    電話再診のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    9                    TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "電話再診（　　回）" TO  WRK-RECE45XX-NAME(1 1)
      *
           MOVE    CNT-DENWA            TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE45XX-NAME(1 1)(11:4)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
      *
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(2 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(2 1)
           MOVE    11                   TO  WRK-RECE45XX-YUKOKETA(2 1)
           MOVE    "同日複数再診（　　回）"
                                        TO  WRK-RECE45XX-NAME(2 1)
      *
           MOVE    CNT-DOUJITU          TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE45XX-NAME(2 1)(15:4)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(2 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(2 1)
                                        TO  RECE459-SRYACT-TBL(2)
      *
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
      *
           .
       2001-DENWA-DOUJITU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電話再診のコメント編集
      *****************************************************************
       2001-DENWA-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999991            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999991            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    電話再診のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    9                    TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "電話再診（　　回）" TO  WRK-RECE45XX-NAME(1 1)
      *
           MOVE    CNT-DENWA            TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE45XX-NAME(1 1)(11:4)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
      *
           .
       2001-DENWA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日複数再診のコメント編集
      *****************************************************************
       2001-DOUJITU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999991            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999991            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    同日複数再診のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    11                   TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "同日複数再診（　　回）"
                                        TO  WRK-RECE45XX-NAME(1 1)
      ***  MOVE    CNT-DOUJITU          TO  DAY-IDX
      ***  MOVE    TBL-DAY(DAY-IDX)     TO  WRK-RECE45XX-NAME(1 1)(15:4)
      *
           MOVE    CNT-DOUJITU          TO  DAY-IDX2
           PERFORM    20011-KAISU-HENSYU-SEC
           MOVE    WRK-HENSYU-KAISU2    TO  WRK-RECE45XX-NAME(1 1)(15:4)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
      *
           .
       2001-DOUJITU-COMMENT-EXT.
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
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
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
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE41-SRYYM
                         AND  RECE41-SRYYM        <=  WRK-AFTER180(1:6)
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
               IF   RECE41-SRYYM      >=    "200604"
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
      *
           MOVE    ZERO                  TO  IDZ
           INITIALIZE                        WRK-FREE-AREA
      *++++
           PERFORM   VARYING   IDX15    FROM   2   BY  -1
                      UNTIL   (IDX15          <    1)
                       OR     (FLG-KYUSEI-OK  =    1)
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE41-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    IDX15                 TO  SRY-NYUGAIKBN
           MOVE    RECE41-PTID           TO  SRY-PTID
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
                        AND ( ACCT-HKNCOMBI  =  RECE41-HKNCOMBI )
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
              MOVE    RECE41-HOSPNUM         TO    SRY-HOSPNUM
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO    SRY-NYUGAIKBN
              MOVE    RECE41-PTID            TO    SRY-PTID
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
           MOVE    ZERO         TO  IDX-RECE45
           MOVE    SPACE        TO  WRK-RECE45XX-REC
           INITIALIZE               WRK-RECE45XX-REC
      *
           ADD     1            TO  IDX-RECE45
           MOVE    "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE    ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
           MOVE    9            TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
           MOVE    "発症　　　月　　日"
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(13:4)
           MOVE    "1"        TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           PERFORM   VARYING   IDX12   FROM    1   BY  1
                      UNTIL  ( IDX12               >   49    )
                        OR   ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1        TO  IDX-RECE45
                MOVE   "810000001"
                                TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO     TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME(IDX12)
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   "1"      TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                PERFORM 800-RECE41-SET459-SEC
                MOVE    99999996            TO  RECE459-ZAINUM
                MOVE    "99"                TO  RECE459-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE45   =    1)
                   ADD         1            TO  IDX-RECE45
                   IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                       MOVE    1            TO  FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE459-REC
                   ADD     1            TO   CNT-RECE459
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999996            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
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
           MOVE    ZERO         TO  IDX-RECE45
           MOVE    SPACE        TO  WRK-RECE45XX-REC
           INITIALIZE               WRK-RECE45XX-REC
      *
           ADD     1            TO  IDX-RECE45
           MOVE    "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE    ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
      *
           MOVE    SPACE        TO  WRK-OKIKAEFLG
      *
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    16   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "脳血管疾患急性発症　　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(21:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(27:4)
               COMPUTE WRK-ZAINUM    =    99998000
                                     +   (IDX16 + 31)
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    20   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "心大血管疾患リハ：治療開始　　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(29:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(35:4)
               IF      FLG-RIHA      =  1
                 MOVE    99998201      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998200
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE41-HKNKBN    NOT  =  "4"
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
                   MOVE    18  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "脳血管疾患等リハ：発症　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(25:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(31:4)
                 WHEN       1
                   MOVE    18  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "脳血管疾患等リハ：手術　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(25:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(31:4)
                 WHEN       2
                   MOVE    20  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "脳血管疾患等リハ：急性増悪　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(29:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(35:4)
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99998401      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998400
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE41-HKNKBN    NOT  =  "4"
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
                   MOVE    15  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "運動器リハ：発症　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(19:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(25:4)
                 WHEN       1
                   MOVE    15  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "運動器リハ：手術　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(19:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(25:4)
                 WHEN       2
                   MOVE    17  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "運動器リハ：急性増悪　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(23:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(29:4)
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99998601      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998600
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE41-HKNKBN    NOT  =  "4"
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
               MOVE    17   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "呼吸器リハ：治療開始　　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(23:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(29:4)
               IF      FLG-RIHA      =  1
                 MOVE    99998801      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99998800
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE41-HKNKBN    NOT  =  "4"
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
               MOVE    18   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "摂食機能療法：治療開始　　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(25:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(31:4)
               IF      FLG-RIHA      =  1
                 MOVE    99999001      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999000
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE41-HKNKBN    NOT  =  "4"
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
                   MOVE    16  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "難病患者リハ：発症　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(21:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(27:4)
                 WHEN       1
                   MOVE    16  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "難病患者リハ：手術　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(21:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(27:4)
                 WHEN       2
                   MOVE    18  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "難病患者リハ：急性増悪　　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(25:4)
                   MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(31:4)
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 MOVE    99999201      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999200
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE41-HKNKBN    NOT  =  "4"
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
               MOVE    18   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "障害児（者）リハ：発症　　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(25:4)
               MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(31:4)
               IF      FLG-RIHA      =  1
                 MOVE    99999401      TO   WRK-ZAINUM
               ELSE
                 COMPUTE WRK-ZAINUM    =    99999400
                                       +   (IDX16 + 31)
               END-IF
               IF   RECE41-HKNKBN    NOT  =  "4"
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
           MOVE    "1"        TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           PERFORM   VARYING   IDX12   FROM    1   BY  1
                      UNTIL  ( IDX12               >   49    )
                        OR   ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1        TO  IDX-RECE45
                MOVE   "810000001"
                                TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO     TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME(IDX12)
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   "1"      TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                PERFORM 800-RECE41-SET459-SEC
                MOVE    WRK-ZAINUM          TO  RECE459-ZAINUM
                MOVE    WRK-SRYKBN          TO  RECE459-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE45   =    1)
                   ADD         1            TO  IDX-RECE45
                   IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                       MOVE    1            TO  FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE459-REC
                   ADD     1            TO   CNT-RECE459
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    WRK-ZAINUM          TO  RECE449-ZAINUM
           MOVE    WRK-SRYKBN          TO  RECE449-SRYKBN
           MOVE    WRK-OKIKAEFLG       TO  RECE449-OKIKAEFLG
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           .
       20011-HASSYO-COMMENT-EXT.
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
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD2          TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
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
                 IF    SANTEI-SRYYM  <  RECE41-SRYYM
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
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
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
                 IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                  IF   SANTEI-SRYYM  =  RECE41-SRYYM
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
                     MOVE   RECE41-SRYYM
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
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999998            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999998            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    加算率のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    9                    TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "加算率（　．　　）"
                                        TO  WRK-RECE45XX-NAME(1 1)
      *
           MOVE    RECE41-KSNRATE(1:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(9:2)
           MOVE    RECE41-KSNRATE(2:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(13:2)
           MOVE    RECE41-KSNRATE(3:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(15:2)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
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
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999998            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999998            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    薬剤等点単価のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    13                   TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "薬剤等（１点単価　　　円）"
                                        TO  WRK-RECE45XX-NAME(1 1)
      *
           MOVE    RECE41-YKZTENTANKA(1:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           IF      WRK-HENSYU-KASANRITU2   =   "０"
              MOVE   "　"   TO   WRK-HENSYU-KASANRITU2
           END-IF
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(19:2)
           MOVE    RECE41-YKZTENTANKA(2:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(21:2)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
      *
           .
       2001-YKZTENTANKA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責従来様式：乳幼児・幼児のコメント編集
      *****************************************************************
       2001-NYUYOJI-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999990            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999990            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    乳幼児・幼児のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    11                   TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "　　月　　日まで乳幼児"
                                        TO  WRK-RECE45XX-NAME(1 1)
      *
           MOVE    RECE41-BIRTHDAY(5:2) TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE45XX-NAME(1 1)(1:4)
           MOVE    RECE41-BIRTHDAY(7:2) TO  DAY-IDX2
           COMPUTE    DAY-IDX2   =   DAY-IDX2  -   1
           MOVE    TBL-DAY(DAY-IDX2)    TO  WRK-RECE45XX-NAME(1 1)(7:4)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
      *
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
      *
           .
       2001-NYUYOJI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    在支援のコメント編集
      *****************************************************************
       2001-ZAISIEN-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999965            TO  RECE449-ZAINUM
           MOVE    WRK-RECE44-SRYKBN   TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           WRITE   RECE449-REC
           ADD     1                   TO  CNT-RECE449
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999965            TO  RECE459-ZAINUM
           MOVE    WRK-RECE44-SRYKBN   TO  RECE459-SRYKBN
      *    在支援コメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    3                    TO  WRK-RECE45XX-YUKOKETA(1 1)
           EVALUATE  TBL-ZAISIEN-COMMENT
             WHEN      1
               MOVE    "在支援"         TO  WRK-RECE45XX-NAME(1 1)
             WHEN      2
               MOVE    "在支病"         TO  WRK-RECE45XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           WRITE   RECE459-REC
           ADD     1                    TO  CNT-RECE459
      *
           .
       2001-ZAISIEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診（同日複数診療科）があるか判定
      *****************************************************************
       2001-SYODOU-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-SYODOU-COMMENT   =    "1"
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
                       OR     (RECE44-SYODOU-SRYKA(IDX)  =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "複初算定診療科："  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      診療科の取得
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE44-SYODOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE41-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
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
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           MOVE    RECE44-MEISAISU     TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SYODOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性疼痛疾患管理料があるか判定
      *****************************************************************
       2001-SHIDOU-HANTEI-SEC          SECTION.
      *
           IF   RECE44-FLG-SHIDOU-COMMENT   =    "1"
      *
             MOVE    ZERO                TO  FLG-SHIDOU-COMMENT
      *
      *      算定履歴検索
             INITIALIZE                  SANTEI-REC
             MOVE    RECE44-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE44-PTID         TO  SANTEI-PTID
             MOVE    RECE44-SRYYM        TO  SANTEI-SRYYM
             MOVE    CONS-MANSEI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
grpsys       PERFORM  900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                  MOVE    "tbl_santei"   TO  MCP-TABLE
                  MOVE    "key"          TO  MCP-PATHNAME
                  PERFORM  900-SANTEI-READ-SEC
                  PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                     IF     SANTEI-FSANTEIYMD(1:6)  =  RECE44-SRYYM   
                           MOVE    1    TO      FLG-SHIDOU-COMMENT
                     END-IF
      *
                     MOVE    "tbl_santei"   TO  MCP-TABLE
                     MOVE    "key"          TO  MCP-PATHNAME
                     PERFORM  900-SANTEI-READ-SEC
                  END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF      FLG-SHIDOU-COMMENT    =      1
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-SHIDOU-COMMENT-SEC
             END-IF         
      *
           END-IF
      *
           .
       2001-SHIDOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性疼痛疾患管理料のコメント編集
      *****************************************************************
       20011-SHIDOU-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       RECE44-REC    TO   RECE449-REC
           ADD        1             TO   RECE449-MEISAISU
           WRITE      RECE449-REC
           ADD        1             TO   CNT-RECE449
      *
           PERFORM 800-RECE44-SET45-SEC
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE45
                 NOT INVALID
                     PERFORM 990-RECE45-NEXT-SEC
           END-START
      *
           MOVE    RECE45-REC           TO  RECE459-REC
           MOVE    RECE45-SRYACT-TBL(1) TO  RECE459-SRYACT-TBL(1)
           MOVE    RECE45-SRYACT-TBL(2) TO  RECE459-SRYACT-TBL(3)
           MOVE    RECE45-SRYACT-TBL(3) TO  RECE459-SRYACT-TBL(4)
           MOVE    RECE45-SRYACT-TBL(4) TO  RECE459-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE459-SRYACT-TBL(2)
           INITIALIZE                       RECE459-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE459-SRYCD(2)
           MOVE    ZERO                 TO  RECE459-SRYSURYO(2)
           MOVE    15                   TO  RECE459-YUKOKETA(2)
           MOVE    "慢性疼痛疾患管理算定日　　　日"
                                        TO  RECE459-NAME(2)
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE459-NAME(2)(25:4)
      *
           MOVE     "1"                 TO  RECE459-SIKFLG(2)
      *
           WRITE    RECE459-REC
           ADD      1                   TO  CNT-RECE459
      *
           .
       20011-SHIDOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ニコチン依存症管理料・重度喘息患者治療管理加算があるか判定
      *****************************************************************
       2001-NIKOCHINTOU-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-NIKOCHIN-COMMENT   =    "1"
           OR      RECE44-FLG-ZENSOKU-COMMENT    =    "1"
      *
             IF    RECE44-FLG-NIKOCHIN-COMMENT   =    "1"
                 MOVE   CONS-NIKOCHIN  TO  WRK-SRYCD
             END-IF
             IF    RECE44-FLG-ZENSOKU-COMMENT    =    "1"
                 MOVE   CONS-ZENSOKU   TO  WRK-SRYCD
             END-IF
      *
             MOVE     ZERO     TO     FLG-NIKOCHINTOU-COMMENT
             MOVE     SPACE    TO     WRK-NIKOCHINTOU-YMD
      *
      *      算定履歴検索
             INITIALIZE                  SANTEI-REC
             MOVE    RECE44-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE44-PTID         TO  SANTEI-PTID
             MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
grpsys       PERFORM  900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI              =   1 )
                             OR   ( FLG-NIKOCHINTOU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <=  RECE44-SRYYM
                    MOVE  SANTEI-SRYYM     TO  WRK-NIKOCHINTOU-YMD(1:6)
                    MOVE  SANTEI-MSANTEID  TO  WRK-NIKOCHINTOU-YMD(7:2)
                    MOVE    1              TO  FLG-NIKOCHINTOU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF      FLG-NIKOCHINTOU-COMMENT    =      1
                 MOVE       99       TO     FLG-COMMENT
                 PERFORM    20011-NIKOCHINTOU-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-NIKOCHINTOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ニコチン依存症管理料・重度喘息患者治療管理加算のコメント編集
      *****************************************************************
       20011-NIKOCHINTOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           ADD      1            TO  IDZ
           IF    RECE44-FLG-NIKOCHIN-COMMENT   =    "1"
                 MOVE    "初回算定　　　月　　日"
                                 TO  WK-FREE-NAME2(IDZ)(1:22)
                 MOVE    WRK-NIKOCHINTOU-YMD(5:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(11:4)
                 MOVE    WRK-NIKOCHINTOU-YMD(7:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(17:4)
                 MOVE    11      TO  WK-FREE-YUKOKETA2(IDZ)
           END-IF
           IF    RECE44-FLG-ZENSOKU-COMMENT    =    "1"
                 MOVE    "第１回目治療管理　　　月　　日"
                                 TO  WK-FREE-NAME2(IDZ)(1:30)
                 MOVE    WRK-NIKOCHINTOU-YMD(5:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(19:4)
                 MOVE    WRK-NIKOCHINTOU-YMD(7:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(25:4)
                 MOVE    15      TO  WK-FREE-YUKOKETA2(IDZ)
           END-IF
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           MOVE    RECE44-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-NIKOCHINTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児特定疾患カウンセリング料（１回目）があるか判定
      *****************************************************************
       2001-SYOUNI-HANTEI-SEC          SECTION.
      *
           IF   RECE44-FLG-SYOUNI-COMMENT   =    "1"
      *
             MOVE        ZERO        TO      FLG-SYOUNI-COMMENT
      *
      *      算定履歴検索
             INITIALIZE                      SANTEI-REC
             MOVE    RECE44-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE44-PTID         TO  SANTEI-PTID
             MOVE    RECE44-SRYYM        TO  SANTEI-SRYYM
             MOVE    CONS-SYOUNI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                    MOVE    "tbl_santei"     TO  MCP-TABLE
                    MOVE    "key"            TO  MCP-PATHNAME
                    PERFORM        900-SANTEI-READ-SEC
                    PERFORM   UNTIL  ( FLG-SANTEI         =   1 )
                               OR    ( FLG-SYOUNI-COMMENT =   1 )
      *
                       MOVE  SANTEI-FSANTEIYMD TO WRK-SYOUNI-FSANTEIYMD
                       MOVE    1               TO FLG-SYOUNI-COMMENT
      *
                       MOVE    "tbl_santei"    TO MCP-TABLE
                       MOVE    "key"           TO MCP-PATHNAME
                       PERFORM        900-SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF      FLG-SYOUNI-COMMENT    =      1
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-SYOUNI-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-SYOUNI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児特定疾患カウンセリング料初回算定日編集
      *****************************************************************
       20011-SYOUNI-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO  IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           ADD      1            TO  IDZ
      *
           MOVE    "第１回目カウンセリング　　　年　　月　　日"
                                 TO  WK-FREE-NAME2(IDZ)(1:42)
           MOVE     21           TO  WK-FREE-YUKOKETA2(IDZ)
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-SYOUNI-FSANTEIYMD  TO  LNK-DAY2-YMD
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
                                   TO  WK-FREE-NAME2(IDZ)(25:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(31:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(37:4)
              END-IF
           END-IF
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           MOVE    RECE44-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SYOUNI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    傷病手当金意見書交付料があるか判定
      *****************************************************************
       2001-SYOBYO-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-SYOBYO-COMMENT   =    "1"
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
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
           IF      RECE44-FLG-RYOYO-COMMENT    =    "1"
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-RYOYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訪問看護指示料があるか判定
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-HOUSIJI-COMMENT   =    "1"
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
           IF   RECE44-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE44-KAISU(1 IDX3)  NOT =  ZERO
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
             MOVE    RECE44-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE44-PTID         TO  SANTEI-PTID
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
                      IF   SANTEI-SRYYM  <=  RECE44-SRYYM
                       MOVE   SANTEI-SRYYM     TO  WRK-FSANTEIYMD(1:6)
                       MOVE   SANTEI-MSANTEID  TO  WRK-FSANTEIYMD(7:2)
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
                       MOVE    RECE44-SRYYM    TO  WRK-HINKAIYMD(1:6)
                       MOVE    WRK-DAY         TO  WRK-HINKAIYMD(7:2)
      *
                       IF     WRK-DAY      NOT  =  ZERO
                       AND    WRK-FSANTEIYMD   <=  WRK-HINKAIYMD
                       AND    WRK-HINKAIYMD    <   WRK-AFTER2
                             MOVE    1    TO   FLG-HINKAI-COMMENT
                             MOVE  WRK-FSANTEIYMD
                                          TO   WRK-TOSEKIYMD
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
             PERFORM    200111-RECE45-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE45
             MOVE    SPACE                TO  WRK-RECE45XX-REC
             INITIALIZE                       WRK-RECE45XX-REC
      *      コメントの挿入
             COMPUTE  MAX-MEISAISU        =   RECE44-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16     TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "初回指導管理算定日　　　月　　日"
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(27:4)
             MOVE    "1"    TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
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
           IF     RECE44-FLG-OSIN-COMMENT   =    "1"
           AND    FLG-OSIN-HENSYU           =     1 
               MOVE       6       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-OSIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問診療料があるか判定
      *****************************************************************
       2001-ZAIHOU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-ZAIHOU-COMMENT   =    "1"
               MOVE       6       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-ZAIHOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問点滴注射管理指導料算定時の点滴注射があるか判定
      *****************************************************************
       2001-HOUTEN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-HOUTEN-COMMENT   =    "1"
               PERFORM    20011-HOUTEN-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE44-TENSOU-SEC
           END-IF
      *
           .
       2001-HOUTEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問点滴注射管理指導料算定時の点滴注射のコメント編集
      *****************************************************************
       20011-HOUTEN-COMMENT-SEC          SECTION.
      *
             PERFORM    200111-RECE45-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE45
             MOVE    SPACE                TO  WRK-RECE45XX-REC
             INITIALIZE                       WRK-RECE45XX-REC
      *      コメントの挿入
             ADD     1                    TO  IDX-RECE45
      *
             MOVE    "810000001"
                            TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
             MOVE    ZERO   TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
             MOVE    4      TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
             MOVE    "（訪点）"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
             MOVE    "1"    TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HOUTEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日のコメントがあるか判定
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-SHUJUTU-COMMENT   =    SPACE
               MOVE       1       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE44-TENSOU-SEC
           END-IF
      *
           .
       2001-SHUJUTSU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （検選）のコメントがあるか判定
      *****************************************************************
       2001-KENSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-SAFLG   =    "2"
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE44-TENSOU-SEC
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
           PERFORM  200111-RECE45-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           PERFORM
             UNTIL  ( FLG-RECE45           =   1                   )
             OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
             OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
             OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
             OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
             OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
             OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
             OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
             OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE45-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
      *++++++
                  IF    IDX-RECE45  =  RECE44-MEISAISU
      *
                    IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = "810000001"
                    OR  WRK-RECE45XX-SRYCD(IDX-RECE45 1)(1:3)  = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) * 2) + 1
      *
                        IF   WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) <=  96
                          MOVE    "　×"   TO
                           WRK-RECE45XX-NAME(IDX-RECE45 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
      *
                        MOVE    RECE44-ZAIKAISU   TO   DAY-IDX2
                        PERFORM 20011-KAISU-HENSYU-SEC
                        IF   WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) <=  96
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE45XX-NAME(IDX-RECE45 1)(IDV:4)
                          COMPUTE WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) =
                                  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) +
                                  4
                        END-IF
      *
                    END-IF
      *
                  END-IF
      *++++++
             END-PERFORM 
      *
             PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
      *    手術日コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE459-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE459-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE459-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE459-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE459-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE459-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE459-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE459-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE459-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE45XX-SIKFLG(IDX-RECE45 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE459-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE459-REC
                     ADD     1            TO   CNT-RECE459
                  END-IF
           END-PERFORM     
      *
           MOVE    RECE44-REC        TO      RECE449-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE449-MEISAISU
           WRITE   RECE449-REC
           ADD     1                 TO      CNT-RECE449
      *
           .
       20011-KENSENTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＴ・ＭＲＩがあるか判定
      *****************************************************************
       2001-CTMRI-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-CTMRI-COMMENT   =    "1"
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE44-TENSOU-SEC
           END-IF
      *
           .
       2001-CTMRI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等のコメント編集（差込：上）
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
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1                  TO  IDX-RECE45
                MOVE   "810000001"
                              TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO   TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE    "1"   TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM 
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等のコメント編集２（差込：下）
      *****************************************************************
       20011-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  200111-FREE-COMMENT2-SEC                              
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           MOVE    RECE44-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM 
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
                   IF    RECE44-KAISU(1 IDX)  NOT =  ZERO
                       ADD   1     TO   DAY-IDX
                       MOVE  IDX   TO  TB-SANTEI-DAY(DAY-IDX)
                   END-IF
           END-PERFORM
      *
           .
       200111-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等の日付の編集
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
                 MOVE    RECE41-SRYYM        TO  WRK-HENYMD(1:6)
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
                       IF   FLG-COMMENT   =   4
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
      *    手術日等の日付の編集（改行）
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
             PERFORM  200111-RECE45-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE45
             MOVE    SPACE                TO  WRK-RECE45XX-REC
             INITIALIZE                       WRK-RECE45XX-REC
      *      リハビリコメントの挿入
             COMPUTE  MAX-MEISAISU        =   RECE44-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "実施日数　　日"
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
             MOVE    "1"    TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             PERFORM  VARYING   IDX  FROM  1  BY  1
                       UNTIL    IDX   >   31
                 IF   RECE44-KAISU(1 IDX)  NOT = ZERO
                        ADD      1        TO  CNT-RIHA
                 END-IF
             END-PERFORM
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                           TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(9:4)
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
           EVALUATE      RECE44-RIHABIRI-COMMENT
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
           MOVE    RECE44-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE44-PTID          TO  SANTEI-PTID
           MOVE    RECE44-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD  TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    RECE44-HKNCOMBI      TO  SANTEI-HKNCOMBINUM
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
           MOVE    RECE44-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE44-PTID          TO  SANTEI-PTID
           MOVE    RECE44-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD2 TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    RECE44-HKNCOMBI      TO  SANTEI-HKNCOMBINUM
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
           PERFORM  200111-RECE45-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           COMPUTE  MAX-MEISAISU        =   RECE44-MEISAISU  +  1
      *
           MOVE    "810000001"
                         TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
           MOVE    9     TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
           MOVE    "開始　　　月　　日"
                         TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
           MOVE    "1"   TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           IF      WRK-RIHAIGAKU-FSANTEIYMD  NOT = SPACE
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(7:4)
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(13:4)
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
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
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
           IF     RECE44-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-RIHASEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関する編集
      *****************************************************************
       2001-YAKUTEI-HENSYU-SEC          SECTION.
      *
           ADD       1        TO         IDX6
      *
           IF    IDX6    <=    100
             MOVE    RECE44-SRYKA        TO   TBL-YT1-SRYKA(IDX6)
      *
             MOVE    RECE44-REC          TO   TBL-YT1-RECE44-REC(IDX6)
      *
             MOVE    RECE44-ZAITEN       TO   TBL-YT1-ZAITEN(IDX6)
             MOVE    RECE44-SRYCDTOTAL   TO   TBL-YT1-SRYCDTOTAL(IDX6)
             MOVE    RECE44-SURYOUTOTAL  TO   TBL-YT1-SURYOUTOTAL(IDX6)
             MOVE    RECE44-RUIKEITOTAL  TO   TBL-YT1-RUIKEITOTAL(IDX6)
             MOVE    RECE44-MEISAISU     TO   TBL-YT1-MEISAISU(IDX6)
      *
             PERFORM   VARYING   IDX7    FROM   2   BY   1
                        UNTIL   (IDX7      >    4)
      *
               COMPUTE   IDX9    =   IDX7  -   1
               PERFORM   VARYING   IDX8    FROM   1   BY   1
                          UNTIL   (IDX8      >    31)
      *
                  MOVE    RECE44-KAISU(IDX7 IDX8)
                            TO    TBL-YT1-KAISU(IDX6 IDX8 IDX9)
      *
               END-PERFORM
      *
             END-PERFORM
      *
             IF      RECE44-SRYCDTOTAL  =   630010002
                  MOVE      2      TO     TBL-YT1-FLG(IDX6)
             ELSE
                  MOVE      1      TO     TBL-YT1-FLG(IDX6)
             END-IF
           ELSE
      *
             DISPLAY "TABLE OVER1"
      *
           END-IF
      *
           .
       2001-YAKUTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０４転送処理
      *****************************************************************
       2099-RECE44-TENSOU-SEC     SECTION.
      *
           MOVE       RECE44-REC    TO   RECE449-REC
           WRITE      RECE449-REC
           ADD        1             TO   CNT-RECE449
           PERFORM    20991-RECE45-TENSOU-SEC
      *
           .
       2099-RECE44-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０５転送処理
      *****************************************************************
       20991-RECE45-TENSOU-SEC     SECTION.
      *
           PERFORM 800-RECE44-SET45-SEC
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE45           =   1                   )
               OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
               OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
               OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
               OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
               OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
               OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
               OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
               OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
      *
               MOVE          RECE45-REC       TO      RECE459-REC
               WRITE         RECE459-REC
               ADD     1                 TO  CNT-RECE459
      *
               PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　退避処理
      *****************************************************************
       200111-RECE45-TAIHI-SEC     SECTION.
      *
           PERFORM 800-RECE44-SET45-SEC
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE45
                 NOT INVALID
                     PERFORM 990-RECE45-NEXT-SEC
                     MOVE    RECE45-REC  TO  WRK-RECE45-REC
           END-START
      *
           .
       200111-RECE45-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE45           =   1                   )
             OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
             OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
             OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
             OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
             OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
             OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
             OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
             OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE45-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
             END-PERFORM 
      *
             PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
      *    手術日コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE459-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE459-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE459-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE459-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE459-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE459-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE459-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE459-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE459-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE45XX-SIKFLG(IDX-RECE45 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE459-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE459-REC
                     ADD     1            TO   CNT-RECE459
                  END-IF
           END-PERFORM     
      *
           MOVE    RECE44-REC        TO      RECE449-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE449-MEISAISU
           WRITE   RECE449-REC
           ADD     1                 TO      CNT-RECE449
      *
           .
       200111-SRYACTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関する編集２
      *****************************************************************
       240-YAKUTEI-HENSYU2-SEC          SECTION.
      *
      *    並び替え
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE44-REC(IDX6) =  SPACE)
      *
              COMPUTE    IDX7  =  IDX6  +  1
      *
              PERFORM    VARYING   IDX8   FROM   IDX7   BY   1
                          UNTIL   (IDX8                     >   100 )
                            OR    (TBL-YT1-RECE44-REC(IDX8) =  SPACE)
      *
                IF       TBL-YT1-KEY(IDX6)  >   TBL-YT1-KEY(IDX8)
                   MOVE  TBL-YT1-OCC(IDX6)  TO  WRK-YAKUTEI-DATA
                   MOVE  TBL-YT1-OCC(IDX8)  TO  TBL-YT1-OCC(IDX6)
                   MOVE  WRK-YAKUTEI-DATA   TO  TBL-YT1-OCC(IDX8)
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *    出力処理
           INITIALIZE                   TBL-YAKUTEI-INF2
           INITIALIZE                   TBL-YAKUTEI-INF3
           MOVE       SPACE     TO      WRK-TAIHI-YT1-KEY
      *
           MOVE       ZERO      TO      IDX11
      *
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE44-REC(IDX6) =  SPACE)
      *
              IF     TBL-YT1-KEY(IDX6)   NOT =  WRK-TAIHI-YT1-KEY
      *
                 IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
                      PERFORM   2401-YAKUTEI-MATOME-SEC
                 END-IF
      *
                 INITIALIZE                        TBL-YAKUTEI-INF2
                 MOVE   TBL-YT1-KEY(IDX6)      TO  WRK-TAIHI-YT1-KEY
      *
              END-IF
      *
              IF     TBL-YT1-FLG(IDX6)   =    1
      *
                   PERFORM   VARYING   IDX7    FROM   1   BY   1
                              UNTIL   (IDX7      >    31)
      *
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8      >    3)
      *
                        IF   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                                        NOT =  ZERO
                           PERFORM   VARYING   IDX9    FROM   1   BY   1
                            UNTIL (IDX9      >    25)
                              OR  (TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   25
                             DISPLAY "TABLE OVER2"
                           ELSE
                             MOVE   TBL-YT1-RECE44-REC(IDX6)
                                TO  TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X1-KAISU(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X1-DAY(IDX7 IDX8 IDX9 IDX7)
                             MOVE   TBL-YT1-HIKAKU(IDX6)
                                TO  TBL-YT2Y1-HIKAKU(IDX7 IDX8 IDX9)
      *
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-RUIKEITOTAL(IDX6)  *
                                     TBL-YT1-MEISAISU(IDX6)     *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL
                                                       (IDX7 IDX8 IDX9)
      *
                             ADD    TBL-YT1-ZAITEN(IDX6)
                                TO  TBL-YT2Y1-ZAITEN(IDX7 IDX8 26)
                             ADD    TBL-YT1-SRYCDTOTAL(IDX6)
                                TO  TBL-YT2Y1-SRYCDTOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-SURYOUTOTAL(IDX6)
                                TO  TBL-YT2Y1-SURYOUTOTAL(IDX7 IDX8 26)
      ***                    ADD    TBL-YT1-RUIKEITOTAL(IDX6)
      ***                       TO  TBL-YT2Y1-RUIKEITOTAL(IDX7 IDX8 26)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-RUIKEITOTAL(IDX6)  *
                                     TBL-YT1-MEISAISU(IDX6)     *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-MEISAISU(IDX6)
                                TO  TBL-YT2Y1-MEISAISU(IDX7 IDX8 26)
      *
                           END-IF
                        END-IF
      *
                     END-PERFORM
      *
                   END-PERFORM
      *
              ELSE
      *
                   PERFORM   VARYING   IDX7    FROM   1   BY   1
                              UNTIL   (IDX7      >    31)
      *
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8      >    3)
      *
                        IF   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                                        NOT =  ZERO
                           PERFORM   VARYING   IDX9    FROM   1   BY   1
                            UNTIL (IDX9      >    3)
                              OR  (TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   3
                             DISPLAY "TABLE OVER3"
                           ELSE
                             MOVE   TBL-YT1-RECE44-REC(IDX6)
                                TO  TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X2-KAISU(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X2-DAY(IDX7 IDX8 IDX9 IDX7)
                             MOVE   TBL-YT1-HIKAKU(IDX6)
                                TO  TBL-YT2Y2-HIKAKU(IDX7 IDX8 IDX9)
      *
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-RUIKEITOTAL(IDX6)  *
                                     TBL-YT1-MEISAISU(IDX6)     *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL
                                                       (IDX7 IDX8 IDX9)
      *
                             ADD    TBL-YT1-ZAITEN(IDX6)
                                TO  TBL-YT2Y2-ZAITEN(IDX7 IDX8 4)
                             ADD    TBL-YT1-SRYCDTOTAL(IDX6)
                                TO  TBL-YT2Y2-SRYCDTOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-SURYOUTOTAL(IDX6)
                                TO  TBL-YT2Y2-SURYOUTOTAL(IDX7 IDX8 4)
      ***                    ADD    TBL-YT1-RUIKEITOTAL(IDX6)
      ***                       TO  TBL-YT2Y2-RUIKEITOTAL(IDX7 IDX8 4)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-RUIKEITOTAL(IDX6)  *
                                     TBL-YT1-MEISAISU(IDX6)     *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-MEISAISU(IDX6)
                                TO  TBL-YT2Y2-MEISAISU(IDX7 IDX8 4)
      *
                           END-IF
                        END-IF
      *
                     END-PERFORM
      *
                   END-PERFORM
      *
              END-IF
      *
           END-PERFORM
      *
           IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
              PERFORM   2401-YAKUTEI-MATOME-SEC
              PERFORM   2402-YAKUTEI-WRITE-SEC
           END-IF
      *
           .
       240-YAKUTEI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイルまとめ処理
      *****************************************************************
       2401-YAKUTEI-MATOME-SEC          SECTION.
      *
           PERFORM   VARYING   IDX7    FROM   1   BY   1
                      UNTIL   (IDX7      >    31)
      *
              PERFORM   VARYING   IDX8    FROM   1   BY   1
                         UNTIL   (IDX8      >    3)
      *
               IF  (TBL-YT2Y1-HIKAKU(IDX7 IDX8 26)  NOT = ZERO)
               OR  (TBL-YT2Y2-HIKAKU(IDX7 IDX8  4)  NOT = ZERO)
                 MOVE     ZERO     TO    FLG-YAKUTEI
                 PERFORM    VARYING   IDX9    FROM   1   BY   1
                       UNTIL   (IDX9                          >   100 )
                         OR    (TBL-YT3X1-RECE44-REC(IDX9 1)  =  SPACE)
                         OR    (FLG-YAKUTEI                   =   1   )
      *
                    IF   (TBL-YT2Y1-HIKAKU(IDX7 IDX8 26) =
                                              TBL-YT3Y1-HIKAKU(IDX9 26))
                    AND  (TBL-YT2Y2-HIKAKU(IDX7 IDX8  4) =
                                              TBL-YT3Y2-HIKAKU(IDX9  4))
      *
                     MOVE    ZERO    TO   FLG-YAKUTEI2
                     PERFORM      VARYING   IDX17  FROM   1   BY   1
                         UNTIL (IDX17                         >    25 )
                           OR  (TBL-YT3X1-RECE44-REC(IDX9 IDX17)
                                                              =  SPACE)
                       MOVE    ZERO    TO   FLG-YAKUTEI2
                       PERFORM    VARYING   IDX10  FROM   1   BY   1
                         UNTIL (IDX10                         >    25 )
                           OR  (TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                           OR  (FLG-YAKUTEI2                  =   1   )
                        IF  TBL-YT3Y1-HIKAKU(IDX9 IDX17)      =
                            TBL-YT2Y1-HIKAKU(IDX7 IDX8 IDX10)
                         ADD    TBL-YT2X1-KAISU(IDX7 IDX8 IDX10)
                                    TO  TBL-YT3X1-KAISU(IDX9 IDX17)
                         ADD    TBL-YT2X1-DAY(IDX7 IDX8 IDX10 IDX7)
                                    TO  TBL-YT3X1-DAY(IDX9 IDX17 IDX7)
                         MOVE   1   TO  FLG-YAKUTEI2
                        END-IF
                       END-PERFORM
                     END-PERFORM
      *
                     MOVE    ZERO    TO   FLG-YAKUTEI2
                     PERFORM      VARYING   IDX17  FROM   1   BY   1
                         UNTIL (IDX17                         >     3 )
                           OR  (TBL-YT3X2-RECE44-REC(IDX9 IDX17)
                                                              =  SPACE)
                       MOVE    ZERO    TO   FLG-YAKUTEI2
                       PERFORM    VARYING   IDX10  FROM   1   BY   1
                         UNTIL (IDX10                         >     3 )
                           OR  (TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                           OR  (FLG-YAKUTEI2                  =   1   )
                        IF  TBL-YT3Y2-HIKAKU(IDX9 IDX17)      =
                            TBL-YT2Y2-HIKAKU(IDX7 IDX8 IDX10)
                         ADD    TBL-YT2X2-KAISU(IDX7 IDX8 IDX10)
                                    TO  TBL-YT3X2-KAISU(IDX9 IDX17)
                         ADD    TBL-YT2X2-DAY(IDX7 IDX8 IDX10 IDX7)
                                    TO  TBL-YT3X2-DAY(IDX9 IDX17 IDX7)
                         MOVE   1   TO  FLG-YAKUTEI2
                        END-IF
                       END-PERFORM
                     END-PERFORM
      *
                     MOVE    1    TO    FLG-YAKUTEI
                    END-IF
      *
                 END-PERFORM
                 IF  FLG-YAKUTEI  =  ZERO
                    IF   IDX9    >    100
                       DISPLAY   "TABLE OVER4"
                    ELSE
                       MOVE   TBL-YT2X-KAIT(IDX7 IDX8)
                                      TO  TBL-YT3X-KAIT(IDX9)
                       MOVE   TBL-YT2Y-KAIT(IDX7 IDX8)
                                      TO  TBL-YT3Y-KAIT(IDX9)
                    END-IF
                 END-IF
               END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
           .
       2401-YAKUTEI-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイル出力
      *****************************************************************
       2402-YAKUTEI-WRITE-SEC          SECTION.
      *
           PERFORM   VARYING   IDX7    FROM   1   BY   1
                      UNTIL   (IDX7                          >   100 )
                        OR    (TBL-YT3X1-RECE44-REC(IDX7 1)  =  SPACE)
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                             UNTIL   (IDX8                     >   25  )
                               OR    (TBL-YT3X1-RECE44-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE449-REC
                      MOVE   TBL-YT3X1-RECE44-REC(IDX7 IDX8)
                                  TO   RECE449-REC
                      MOVE   RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      MOVE   TBL-YT3X1-KAISU(IDX7 IDX8)
                                  TO   RECE449-ZAIKAISU
                      INITIALIZE       RECE449-DAY-AREA
                      PERFORM VARYING IDX17 FROM 1 BY 1
                               UNTIL  IDX17   > 31
                        MOVE   TBL-YT3X1-DAY(IDX7 IDX8 IDX17)
                                  TO   RECE449-KAISU(1 IDX17)
                      END-PERFORM
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE449-ZAINUM
                      MOVE      RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      WRITE     RECE449-REC
                      ADD       1            TO  CNT-RECE449
                      PERFORM   24021-YAKUTEI-RECE45-TENSOU-SEC
      *
                 END-PERFORM
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                              UNTIL  (IDX8                     >    3  )
                                OR   (TBL-YT3X2-RECE44-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE449-REC
                      MOVE   TBL-YT3X2-RECE44-REC(IDX7 IDX8)
                                  TO   RECE449-REC
                      MOVE   RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      MOVE   TBL-YT3X2-KAISU(IDX7 IDX8)
                                  TO   RECE449-ZAIKAISU
                      INITIALIZE       RECE449-DAY-AREA
                      PERFORM VARYING IDX17 FROM 1 BY 1
                               UNTIL  IDX17   > 31
                        MOVE   TBL-YT3X2-DAY(IDX7 IDX8 IDX17)
                                  TO   RECE449-KAISU(1 IDX17)
                      END-PERFORM
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE449-ZAINUM
                      MOVE      RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      WRITE     RECE449-REC
                      ADD       1            TO  CNT-RECE449
                      PERFORM   24021-YAKUTEI-RECE45-TENSOU-SEC
      *
                 END-PERFORM
      *
           END-PERFORM
      *
           .
       2402-YAKUTEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイル出力（４５ファイル）
      *****************************************************************
       24021-YAKUTEI-RECE45-TENSOU-SEC          SECTION.
      *
           PERFORM 800-RECE449-SET45-SEC
           MOVE    WRK-YAKUTEI-ZAINUM   TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE45             =   1                  )
               OR     ( RECE449-HOSPNUM    NOT =   RECE45-HOSPNUM     )
               OR     ( RECE449-SRYYM      NOT =   RECE45-SRYYM       )
               OR     ( RECE449-NYUGAIKBN  NOT =   RECE45-NYUGAIKBN   )
               OR     ( RECE449-PTID       NOT =   RECE45-PTID        )
               OR     ( RECE449-RECEKA     NOT =   RECE45-RECEKA      )
               OR     ( RECE449-HKNID      NOT =   RECE45-HKNID       )
               OR     ( RECE449-TEKSTYMD   NOT =   RECE45-TEKSTYMD    )
               OR     ( WRK-YAKUTEI-ZAINUM NOT =   RECE45-ZAINUM      )
      *
               MOVE     RECE45-REC            TO      RECE459-REC
               MOVE     WRK-YAKUTEI-ZAINUM2   TO      RECE459-ZAINUM
               WRITE    RECE459-REC
               ADD      1                     TO      CNT-RECE459
      *
               PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
           .
       24021-YAKUTEI-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    差込編集２
      *****************************************************************
       250-SASHIKOMI2-SEC     SECTION.
      *
      *    パラメータ４４９ファイルより編集
           PERFORM 800-RECE41-SET449-SEC
           MOVE    ZERO                TO  RECE449-ZAINUM
      *
           START   RECE449-FILE
               KEY IS    >=   RECE449-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE449
               NOT INVALID
                   PERFORM 990-RECE449-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE449          =   1                    )
               OR     ( RECE41-HOSPNUM   NOT =   RECE449-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE449-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE449-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE449-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE449-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE449-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE449-TEKSTYMD     )
      *
               MOVE   ZERO   TO  FLG-COMMENT
      *        （酸素計算式のコメントのセット）
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2501-SANSO-HANTEI-SEC
               END-IF
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2599-RECE449-TENSOU-SEC
               END-IF
      *
               PERFORM 990-RECE449-NEXT-SEC
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
           IF     RECE449-FLG-SANSO-COMMENT   =    "1"
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
           PERFORM 800-RECE449-SET459-SEC
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
               KEY IS    >=   RECE459-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE459
               NOT INVALID
                   PERFORM 990-RECE459-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE459           =   1                 )
               OR     ( RECE449-HOSPNUM   NOT =   RECE459-HOSPNUM   )
               OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM     )
               OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN )
               OR     ( RECE449-PTID      NOT =   RECE459-PTID      )
               OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA    )
               OR     ( RECE449-HKNID     NOT =   RECE459-HKNID     )
               OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD  )
               OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM    )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE459-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE459-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE459-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE459-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE459-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE459-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE459-SRYSURYO(IDX)  
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE459-NEXT-SEC
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
           PERFORM  250111-SANSO-FREE-COMMENT-SEC
      *
           PERFORM  250111-RECE459-TAIHI-SEC
      *
      *    ＲＥＣＥ４５９ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
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
      *    ４５９　退避処理
      *****************************************************************
       250111-RECE459-TAIHI-SEC     SECTION.
      *
           PERFORM 800-RECE449-SET459-SEC
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
                 KEY IS    >=   RECE459-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE459
                 NOT INVALID
                     PERFORM 990-RECE459-NEXT-SEC
                     MOVE  RECE459-REC  TO  WRK-RECE45-REC
           END-START
      *
           .
       250111-RECE459-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ４５９　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE459           =   1                    )
             OR     ( RECE449-HOSPNUM   NOT =   RECE459-HOSPNUM      )
             OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM        )
             OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN    )
             OR     ( RECE449-PTID      NOT =   RECE459-PTID         )
             OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA       )
             OR     ( RECE449-HKNID     NOT =   RECE459-HKNID        )
             OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD     )
             OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   5     )
                       OR      ( RECE459-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE459-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
      *
                  IF   RECE459-SRYCD(IDX) = "770020070" OR "770030070"
                       MOVE   IDX-RECE45   TO   WRK-JYUNBAN
                  END-IF
      *
             END-PERFORM 
      *
             PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      補正率又は高気圧酸素加算以降の明細をずらす
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE45   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE45XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE45XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE45XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      計算式の差し込み
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE45     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1     TO  MAX-MEISAISU
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    手術日コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE451-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE451-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE451-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE451-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE451-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE451-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE451-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE451-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE451-SRYACT-TBL(IDX)
      *
                         IF    WRK-RECE45XX-SIKFLG(IDX-RECE45 1)  =  "1"
                              ADD    1    TO   WRK-CNT-MEISAI
                         END-IF    
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE451-SRYCD(1)  NOT =  SPACE
                     WRITE   RECE451-REC
                     ADD     1            TO   CNT-RECE45
                  END-IF
           END-PERFORM     
      *
           MOVE    RECE449-REC       TO      RECE441-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE441-MEISAISU
           WRITE   RECE441-REC
           ADD     1                 TO      CNT-RECE44
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４９転送処理
      *****************************************************************
       2599-RECE449-TENSOU-SEC     SECTION.
      *
           MOVE       RECE449-REC    TO   RECE441-REC
           WRITE      RECE441-REC
           ADD        1              TO   CNT-RECE44
           PERFORM    25991-RECE459-TENSOU-SEC
      *
           .
       2599-RECE449-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４５９転送処理
      *****************************************************************
       25991-RECE459-TENSOU-SEC     SECTION.
      *
           PERFORM 800-RECE449-SET459-SEC
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
               KEY IS    >=   RECE459-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE459
               NOT INVALID
                   PERFORM 990-RECE459-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE459           =   1                    )
               OR     ( RECE449-HOSPNUM   NOT =   RECE459-HOSPNUM      )
               OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM        )
               OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN    )
               OR     ( RECE449-PTID      NOT =   RECE459-PTID         )
               OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA       )
               OR     ( RECE449-HKNID     NOT =   RECE459-HKNID        )
               OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD     )
               OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM       )
      *
               MOVE    RECE459-REC       TO   RECE451-REC
               WRITE   RECE451-REC
               ADD     1                 TO   CNT-RECE45
      *
               PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25991-RECE459-TENSOU-EXT.
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
      *            ジョブ管理開始処理
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
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE41  CNT-IN  :" CNT-RECE41  
           DISPLAY "RECE449 CNT-WK  :" CNT-RECE449 
           DISPLAY "RECE459 CNT-WK  :" CNT-RECE459 
           DISPLAY "RECE44  CNT-OUT :" CNT-RECE44  
           DISPLAY "RECE45  CNT-OUT :" CNT-RECE45  
           DISPLAY "*** ORCR0430 END ***"
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
           CLOSE                       RECE41-FILE
      *
           CLOSE                       RECE44-FILE
      *
           CLOSE                       RECE45-FILE
      *
           CLOSE                       RECE441-FILE     
           CLOSE                       RECE451-FILE     
      *
           CLOSE                       RECE449-FILE
           CLOSE                       RECE459-FILE
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４１データ編集処理（４４）
      *****************************************************************
       800-RECE41-SET44-SEC           SECTION.
      *
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPNUM      TO  RECE44-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
      *
           .
       800-RECE41-SET44-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４１データ編集処理（４４９）
      *****************************************************************
       800-RECE41-SET449-SEC           SECTION.
      *
           INITIALIZE                      RECE449-REC
           MOVE    RECE41-HOSPNUM      TO  RECE449-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE449-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE449-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE449-PTID
           MOVE    RECE41-RECEKA       TO  RECE449-RECEKA
           MOVE    RECE41-HKNID        TO  RECE449-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE449-TEKSTYMD
      *
           .
       800-RECE41-SET449-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４１データ編集処理（４５９）
      *****************************************************************
       800-RECE41-SET459-SEC           SECTION.
      *
           INITIALIZE                      RECE459-REC
           MOVE    RECE41-HOSPNUM      TO  RECE459-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE459-PTID
           MOVE    RECE41-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE41-HKNID        TO  RECE459-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE459-TEKSTYMD
      *
           .
       800-RECE41-SET459-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４データ編集処理（４５）
      *****************************************************************
       800-RECE44-SET45-SEC           SECTION.
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
      *
           .
       800-RECE44-SET45-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４９データ編集処理（４５）
      *****************************************************************
       800-RECE449-SET45-SEC           SECTION.
      *
           INITIALIZE                       RECE45-REC
           MOVE    RECE449-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE449-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE449-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE449-PTID         TO  RECE45-PTID
           MOVE    RECE449-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE449-HKNID        TO  RECE45-HKNID
           MOVE    RECE449-TEKSTYMD     TO  RECE45-TEKSTYMD
      *
           .
       800-RECE449-SET45-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４９データ編集処理（４５９）
      *****************************************************************
       800-RECE449-SET459-SEC           SECTION.
      *
           INITIALIZE                       RECE459-REC
           MOVE    RECE449-HOSPNUM      TO  RECE459-HOSPNUM
           MOVE    RECE449-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE449-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE449-PTID         TO  RECE459-PTID
           MOVE    RECE449-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE449-HKNID        TO  RECE459-HKNID
           MOVE    RECE449-TEKSTYMD     TO  RECE459-TEKSTYMD
      *
           .
       800-RECE449-SET459-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE41
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE44-NEXT-SEC           SECTION.
      *
           READ    RECE44-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE44
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE44
           END-READ
      *
           .
       990-RECE44-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト４４９データ読み
      ****************************************************************
       990-RECE449-NEXT-SEC           SECTION.
      *
           READ    RECE449-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE449
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE449
           END-READ
      *
           .
       990-RECE449-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE45-NEXT-SEC           SECTION.
      *
           READ    RECE45-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE45
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE45
           END-READ
      *
           .
       990-RECE45-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト４５９データ読み
      ****************************************************************
       990-RECE459-NEXT-SEC           SECTION.
      *
           READ    RECE459-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE459
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE459
           END-READ
      *
           .
       990-RECE459-NEXT-EXT.
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
      *    受診履歴読込
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
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
