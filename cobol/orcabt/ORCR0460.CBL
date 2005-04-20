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
       PROGRAM-ID.             ORCR0460.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      点数摘要欄編集（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02.08.23  検査にコメントがあるとき検査数
      *                                     にカウントしないようにする
      *  01.00.02    NACL-藤原    02.08.26  皮下筋肉注射以外は手技名を印刷
      *                                     するようにする
      *  01.00.03    NACL-門脇    02.09.19  自賠責対応
      *  01.00.04    NACL-藤原    02.09.20  検査のときの表示方法を変更
      *  01.00.05    NACL-門脇    02.10.02  摘要出力項目変更
      *  01.00.06    NACL-門脇    02.11.26  診療実日数算定用の
      *                                     コードを印刷しないよう追加 
      *                                     検診コードを印刷するよう追加
      *  01.00.07    NACL-藤原    02.12.02  酸素の時単価を表示するように追加
      *  01.00.08    NACL-藤原    03.01.10  検査の回数の編集
      *                                     画像診断のフィルムを手技に表示
      *  01.00.09    NACL-門脇    03.02.13  乳幼児加算の時手技に表示
      *  01.00.10    NACL-門脇    03.03.06  初再診のコメント編集対応
      *  01.00.11    NACL-門脇    03.03.12  先頭の診療行為コードの判定修正
      *  01.00.12    NACL-門脇    03.05.08  先頭の診療行為コードの判定修正
      *  01.00.13    NACL-門脇    03.05.13  腰部固定帯加算を器材点へ
      *  01.00.14    NACL-藤原    03.05.27  検査のまとめの修正
      *  01.00.15    NACL-門脇    03.06.05  摘要欄出力条件修正
      *  01.00.16    NACL-藤原    03.06.30  薬剤・器材の剤点数が０のとき
      *                                     摘要欄に印刷しないように修正
      *  01.00.17    NACL-門脇    03.09.29  点数マスタを読み込まないよう
      *                                     修正
      *  01.00.18    NACL-藤原    04.02.05  薬剤・器材の編集順を逆にする
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.01  初診（乳幼児）（夜間・休日・
      *                                     深夜）加算の摘要欄記載
      *  01.01.02    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指導料
      *                                     算定時の点滴注射のコメント対応
      *  01.01.03    NACL-門脇    04.04.26  ユーザーコメント追加対応
      *  01.01.04    NACL-門脇    04.06.11  皮下筋肉注射で、薬剤点・器材点
      *                                     が無いの場合の摘要欄記載対応
      *  01.01.05    NACL-藤原    04.07.16  検査のまとめを回数テーブルの
      *                                     合計で行うようにする 
      *  01.01.06    NACL-門脇    04.09.22  薬剤・器材の記載順修正
      *  中止処理追加
      *  01.02.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.02.02    NACL-門脇    04/12/17  胸部固定帯加算、創外固定器
      *                                     使用加算を薬剤等へ
      *  01.02.03    NACL-門脇    05/01/27  腰・胸部固定帯加算、創外固定器
      *                                     使用加算の技術・薬剤等の選択対応
      *  01.02.04    NACL-門脇    05/02/28  （サ）記載対応
      *  01.02.05    NACL-門脇    05/03/02  薬剤で単位のないデータの数量編集
      *                                     修正
      *  01.02.06    NACL-藤原    05/03/07  包括対象検査のまとめの修正
      *  01.02.07    NACL-藤原    05/04/18  包括対象検査がまとまらない
      *                                     場合の編集の修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE01PARA
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
           SELECT  RECE46-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE46-KEY
                                   FILE    STATUS  IS  STS-RECE46.
      *    
           SELECT  RECE46X-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE46X-KEY
                                   FILE    STATUS  IS  STS-RECE46.
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
       FD  RECE46-FILE.
       01  RECE46-REC. 
           COPY    "CPRCF046.INC".
      *
       FD  RECE46X-FILE.
       01  RECE46X-REC. 
           COPY    "CPRCF046.INC"  REPLACING  //RECE46//
                                   BY         //RECE46X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE44//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE45//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE46//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *     
      *    フラグ領域
       01  STS-AREA.
           03  SPA-STS             PIC 9(04).
           03  STS-RECE41          PIC X(02).
           03  STS-RECE44          PIC X(02).
           03  STS-RECE45          PIC X(02).
           03  STS-RECE46          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE41          PIC 9(01).
           03  FLG-RECE44          PIC 9(01).
           03  FLG-RECE45          PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-NOPRT           PIC 9(01).
      **** 03  FLG-TENSU           PIC 9(01).
           03  FLG-KENSA           PIC 9(01).
           03  FLG-TEIGEN          PIC 9(01).
           03  FLG-COMMENT         PIC 9(01).
           03  FLG-COMMENT2        PIC 9(01).
           03  FLG-HIKAKIN         PIC 9(01).
           03  FLG-SRYCD1          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE44          PIC 9(06).
           03  CNT-RECE45          PIC 9(06).
           03  CNT-RECE46          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
           03  CNT-SRYKBN60        PIC 9(03).     
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *    点数回数編集用     
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
      *    剤診療行為コード別編集用     
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *     
           03  WRK-SYU-INDEX       PIC 9(04).
           03  WRK-YKZ-INDEX       PIC 9(04).
           03  WRK-KIZ-INDEX       PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-LINENUM         PIC 9(05).
      *
           03  WRK-TENSUGOKEI      PIC 9(07).
      *    剤印字行数
           03  WRK-TBLMAX          PIC 9(03).
      *    剤明細総数（診療行為コード別）
           03  WRK-ZAIWMAX-G.
               05  WRK-ZAIWMAX     PIC 9(03)   OCCURS  6.
      * 
           03  WRK-YUKOKETA        PIC  9(02).
           03  WRK-NAME            PIC  X(200).
      *
           03  WRK-RECE45          PIC 9(01).     
      *
      *    数字半角全角変換用     
           03  WRK-NUM                         PIC 9(01).
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(10).
               05  WRK-CHAR-N                  PIC X(20).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-FLG                PIC 9(01). 
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *
           03  WRK-EDIT-AREA.
               05  WRK-CLM                     PIC 9(03).
               05  WRK-WCLM                    PIC 9(03).
      *
           03  WRK-KOUI                        PIC X(210).
      *
           03  WRK-ZAI.
               05  WRK-ZAI-MK                  PIC X(02).
               05  WRK-ZAI-NAME.
                   07  FILLER                  PIC X(28).
                   07  WRK-ZAI-TNSKAI          PIC X(22).
      *
           03  WRK-TNSKAI.
               05  WRK-ZAITEN                  PIC X(14).
               05  WRK-X                       PIC X(02).
               05  WRK-ZAIKAISU                PIC X(06).
      *
           03  WRK-ZAITENX                     PIC ZZZZZZ9.
           03  WRK-ZAIKAISUX                   PIC ZZ9.
           03  WRK-TNSKAI-ST                   PIC 9(02).
           03  WRK-TNSKAI-KETA                 PIC 9(02).
      *
           03  WRK-SRYCD                       PIC X(01).
      *     
           03  WRK-R05IDX                      PIC 9(02).
      *
      *    検査の回数
           03  WRK-KENSA-KAISU                 PIC 9(05). 
      *     
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-SRYSURYOW                   PIC X(20).
           03  WRK-ZAIIDX                      PIC 9(03).
           03  WRK-NAMEKETA                    PIC 9(03).
           03  WRK-YOHAKU                      PIC 9(03).
           03  WRK-WKETA                       PIC 9(03).
      *    分画数編集用     
           03  WRK-BUNKAKUKETA                 PIC 9(02).
           03  WRK-BUNKAKU-N                   PIC X(06).
           03  WRK-SRYKAISU-X                  PIC ZZ9.
      *    数量編集用     
           03  WRK-SURYOKETA                   PIC 9(02).
           03  WRK-SURYOKETA1                  PIC 9(02).
           03  WRK-SURYOKETA2                  PIC 9(02).
           03  WRK-SURYOST                     PIC 9(02).
           03  WRK-SURYO                       PIC 9(05)V9(03).
           03  WRK-SURYO-R  REDEFINES  WRK-SURYO.
               05  WRK-SURYO-S1                PIC 9(05).
               05  WRK-SURYO-S2                PIC 9(03).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1                PIC ZZZZZ.
               05  WRK-SURYO-Z2                PIC X(01).
               05  WRK-SURYO-Z3                PIC ZZZ.
           03  WRK-SURYO-N.
               05  WRK-SURYO-N1                PIC X(10).
               05  WRK-SURYO-N2                PIC X(02).
               05  WRK-SURYO-N3                PIC X(06).
      *    剤診療行為コード別編集用     
           03  WRK-WIDX1                       PIC 9(03).
           03  WRK-WIDX2                       PIC 9(03).
      *     
           03  WRK-Z9                          PIC ZZZ9.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *    剤・診療行為テーブル
       01  TBL-ZAI-AREA.
           03  TBL-ZAI      OCCURS 100  PIC X(52).
      *
      *    検査をまとめるテーブル
       01  WRK-KAISU-G.
           03  WRK-KAKSU-OCC               OCCURS  4.
               05  WRK-KAISU               PIC 9(03).  
           03  WRK-KAISU1                  PIC 9(02).
           03  WRK-KAISU2                  PIC 9(02).
       01  WRK-HOUKNSKBN                   PIC 9(01).
       01  WRK-KENSA-SU                    PIC 9(03).
      *
       01  WRK-RECE44-ZAITEN               PIC 9(08). 
      *         
       01  WRK-RECE44-MAX                  PIC 9(03).
       01  WRK-SUM-KAISU-TABLE.
           03  WRK-SUM-KAISU               PIC 9(04)
                                                  OCCURS  31.
       01  WRK-RECE44-FLG-TABLE.
           03  WRK-RECE44-FLG              PIC 9(01)
                                                  OCCURS  100.
       01  WRK-RECE44-MATOME-TABLE.
           03  WRK-RECE44-MATOME           PIC 9(01)
                                                  OCCURS  100.
       01  WRK-RECE44-TABLE.
           03  WRK-RECE44-TBL              OCCURS  100.
               05  WRK-RECE44-HOUKNSKBN           PIC 9(01).
               05  WRK-RECE44-REC                 PIC X(600).
               05  WRK-RECE44-DAY-AREA.
                   07  WRK-RECE44-DAY-TBL         OCCURS  4.
                       09  WRK-RECE44-KAISUINF    OCCURS  31.
                           11  WRK-RECE44-KAISU   PIC  9(03).
      *
      *    検査のとき適用の編集に使用
       01  WRK-KENSA-MAX                   PIC 9(04).
       01  TBL-NAME-AREA.
           03  TBL-NAME-REC      OCCURS 100.
               05  TBL-SRYCD               PIC  X(09).
               05  TBL-SRYSURYO            PIC  9(05)V9(03).
      *        単位コード/単位名称有効桁数/単位名称
               05  TBL-TANICD              PIC  9(03).
               05  TBL-TANIMOJISU          PIC  9(01).
               05  TBL-TANINAME            PIC  X(24).
               05  TBL-NAMEINF.
      *            漢字有効桁数/漢字名称
                   07  TBL-YUKOKETA        PIC  9(02).
                   07  TBL-NAME            PIC  X(200).
      *        包括対象検査情報
               05  TBL-HOUKNSKBN           PIC  9(02).
      *
      *     検査の項目数用
        01  WRK-KOUMOKUSU               PIC 9(02). 
        01  WRK-TNS-HOUKNSKBN           PIC 9(02).
        01  WRK-CONS-KENSACOMMENT-AREA.
            03  WRK-CONS-KENSACOMMENT-G.
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
            03  WRK-CONS-KENSACOMMENT-R    REDEFINES
                                           WRK-CONS-KENSACOMMENT-G.
                05  WRK-CONS-KENSACOMMENT  PIC X(50)   OCCURS  8.
      *          
       01  TBL-ZAIW-AREA.
      *    剤の先頭の診療コード
           03  TBL-ZAIW-SRYCD1             PIC X(09).
      *    剤の先頭の診療コードの各種番号等情報
           03  TBL-ZAIW-CDKBN-AREA.
               05  TBL-ZAIW-CDKBN-KBN          PIC X(01).
               05  TBL-ZAIW-CDKBN-SYO          PIC 9(01).
               05  TBL-ZAIW-CDKBN-BU           PIC 9(02).
               05  TBL-ZAIW-CDKBN-KBNNUM       PIC 9(03).
               05  TBL-ZAIW-CDKBN-KBNNUM-EDA   PIC 9(02).
               05  TBL-ZAIW-CDKBN-KOUBAN       PIC 9(02).
               05  TBL-ZAIW-KOUHYOJYUNNUM      PIC 9(09).
      *    剤の先頭の診療コードの検査等実施判断情報
           03  TBL-ZAIW-KNSJISKBN          PIC 9(01).
      *     
      *    診療行為別テーブル
      *    １：ユーザーコメント、２：コメント、
      *    ３：診療行為、４：薬剤、５：特定器材                     
           03  TBL-ZAIW-OCC                OCCURS 5.  
               05  TBL-ZAIW-REC            OCCURS 100.
                   07  TBL-ZAIW-SRYACTINF.
      *                診療コード/数量/回数
                       09  TBL-ZAIW-SRYCD           PIC  X(09).
                       09  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(03).
                       09  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *                単位コード/単位名称有効桁数/単位名称
                       09  TBL-ZAIW-TANICD          PIC  9(03).
                       09  TBL-ZAIW-TANIMOJISU      PIC  9(01).
                       09  TBL-ZAIW-TANINAME        PIC  X(24).
      *                点数識別区分/点数/明細請求フラグ
                       09  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
                       09  TBL-ZAIW-TEN             PIC  9(09)V9(02).
                       09  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *                自動算定区分/名称入力番号/点数欄集計先識別
                       09  TBL-ZAIW-AUTOKBN         PIC  X(01).
                       09  TBL-ZAIW-INPUTNUM        PIC  9(03).
                       09  TBL-ZAIW-TENTTLKBN       PIC  9(03).
                   07  TBL-ZAIW-NAMEINF.
      *                漢字有効桁数/漢字名称
                       09  TBL-ZAIW-YUKOKETA        PIC  9(02).
                       09  TBL-ZAIW-NAME            PIC  X(200).
      *                包括対象検査情報
                   07  TBL-ZAIW-HOUKNSKBNINF.
                       09  TBL-ZAIW-HOUKNSKBN       PIC  9(02).
      *
       01  TBL-ZAI-MAX         PIC  9(03)  VALUE   100.              
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *   印字しない診療行為テーブル
       01  TBL-NOSRYCD-AREA.
      *    時間外（再診）
           03  FILLER          PIC X(09)   VALUE   "112001110".
      ***  時間外特例（再診）
      ***  03  FILLER          PIC X(09)   VALUE   "112001410".
      *    時間外（再診）
           03  FILLER          PIC X(09)   VALUE   "112701010".
      ***  時間外特例（再診）
      ***  03  FILLER          PIC X(09)   VALUE   "112701310".
      *    休診（再診）
           03  FILLER          PIC X(09)   VALUE   "112001210".
      *    休日（再診）
           03  FILLER          PIC X(09)   VALUE   "112701110".
      *    深夜（再診）
           03  FILLER          PIC X(09)   VALUE   "112001310".
      *    深夜（再診）
           03  FILLER          PIC X(09)   VALUE   "112701210".
      *    処方料（７種類以上）
           03  FILLER          PIC X(09)   VALUE   "120002610".
      *    処方料（その他）
           03  FILLER          PIC X(09)   VALUE   "120001210".
      *    処方料（麻・向・覚・毒）
           03  FILLER          PIC X(09)   VALUE   "120001310".
      *    調基（その他）
           03  FILLER          PIC X(09)   VALUE   "120001810".
      *    調基（入院）
           03  FILLER          PIC X(09)   VALUE   "120001710".
      *    調剤・処方料（麻・向・覚・毒）
           03  FILLER          PIC X(09)   VALUE   "120002030".
      *    調剤料（外用薬）
           03  FILLER          PIC X(09)   VALUE   "120001010".
      *    調剤料（内用薬・浸煎薬・頓服薬）
           03  FILLER          PIC X(09)   VALUE   "120000710".
      *    調剤料（入院）
           03  FILLER          PIC X(09)   VALUE   "120001110".
      *    調剤料（麻・向・覚・毒）（入院）
           03  FILLER          PIC X(09)   VALUE   "120000410".
      *    調剤料（麻・向・覚・毒）（入院外）
           03  FILLER          PIC X(09)   VALUE   "120000110".
      *    外来管理加算（３月以内）
           03  FILLER          PIC X(09)   VALUE   "101120070".
      *    外来管理加算（３月超）（５回以内）
           03  FILLER          PIC X(09)   VALUE   "101120080".
      *    実日数算定
           03  FILLER          PIC X(09)   VALUE   "099409901".
      *    労災（加算なし）
           03  FILLER          PIC X(09)   VALUE   "099509901".
      *    初診料ダミー
           03  FILLER          PIC X(09)   VALUE   "099110001".
      *    再診料ダミー
           03  FILLER          PIC X(09)   VALUE   "099120001".
      *    初診料算定科：
           03  FILLER          PIC X(09)   VALUE   "830000020".
      *    再診料算定科：
           03  FILLER          PIC X(09)   VALUE   "830000021".
       01  TBL-NOSRYCD-R       REDEFINES   TBL-NOSRYCD-AREA.
            03  TBL-NOSRYCD     PIC X(09)   OCCURS   25.
      *
       01  TBL-NOSRYCD-MAX     PIC 9(02)   VALUE   25.
      *
      *    固定値
       01  CONST-AREA.
      *    画像診断の単位の変更（枚→回）
           03  WRK-CONS-TANICD PIC 9(03)   VALUE   006.
           03  WRK-CONS-TANINAME
                               PIC X(02)   VALUE   "回".
           03  WRK-CONS-BUNKAKU-MOJISU
                               PIC 9(02)   VALUE   4.                    
           03  WRK-CONS-BUNKAKU
                               PIC X(04)   VALUE   "分画".
      *    薬剤料逓減
           03  CONS-TEIGEN     PIC X(09)   VALUE   "630010002".
      *---- (01.00.05) LINE ADD START --------------------------------
      *フリーコメント
      *    電話再診（　回）
           03  CONS-COMMENT1   PIC X(09)   VALUE   "840000001".
      *    同日複数再診（　回） 
           03  CONS-COMMENT2   PIC X(09)   VALUE   "840000002".
      *    フリーコメント 
           03  CONS-COMMENT3   PIC X(09)   VALUE   "810000001".
      *    複数診療科受診
           03  CONS-COMMENT4   PIC X(09)   VALUE   "099999901".
      *    集団検診から
           03  CONS-COMMENT5   PIC X(09)   VALUE   "820000003".
      *    老人検診から
           03  CONS-COMMENT6   PIC X(09)   VALUE   "820000004".
      *    学校検診から
           03  CONS-COMMENT7   PIC X(09)   VALUE   "820000005".
      *    健康診断から
           03  CONS-COMMENT8   PIC X(09)   VALUE   "820000006".
      *---- (01.00.05) LINE ADD END   --------------------------------
      *    皮下筋肉注射
           03  CONS-HIKAKIN    PIC X(09)   VALUE   "130000510".
      *    （サ）
           03  CONS-RINYAK     PIC X(09)   VALUE   "820000046".
      *
      *    初診（診療所）紹介患者加算
           03  CONS-SYOSIN-KASAN1  PIC X(09) VALUE "111003770".
      *    老人初診（診療所）紹介患者加算
           03  CONS-SYOSIN-KASAN2  PIC X(09) VALUE "111700470".
      ***  初診（乳幼児）加算
      ***  03  CONS-SYOSIN-KASAN3  PIC X(09) VALUE "111000370".
      ***  初診（育児栄養指導）加算
      ***  03  CONS-SYOSIN-KASAN4  PIC X(09) VALUE "111000470".
      *    小児科外来診療料（初診時診療時の紹介患者）加算
           03  CONS-SYOSIN-KASAN5  PIC X(09) VALUE "111010370".
      *    小児科外来診療料（紹介患者加算３）
           03  CONS-SYOSIN-KASAN6  PIC X(09) VALUE "111010870".
      *    小児科外来診療料（紹介患者加算４）
           03  CONS-SYOSIN-KASAN7  PIC X(09) VALUE "111010970".
      *    小児科外来診療料（紹介患者加算５）
           03  CONS-SYOSIN-KASAN8  PIC X(09) VALUE "111011070".
      *    小児科外来診療料（紹介患者加算６）
           03  CONS-SYOSIN-KASAN9  PIC X(09) VALUE "111011170".
      *    紹介患者加算１（病院）
           03  CONS-SYOSIN-KASAN10 PIC X(09) VALUE "111011270".
      *    紹介患者加算２（病院）
           03  CONS-SYOSIN-KASAN11 PIC X(09) VALUE "111011370".
      *    紹介患者加算３（病院）
           03  CONS-SYOSIN-KASAN12 PIC X(09) VALUE "111009970".
      *    紹介患者加算４（病院）
           03  CONS-SYOSIN-KASAN13 PIC X(09) VALUE "111010070".
      *    紹介患者加算５（病院）
           03  CONS-SYOSIN-KASAN14 PIC X(09) VALUE "111010170".
      *    紹介患者加算６（病院）
           03  CONS-SYOSIN-KASAN15 PIC X(09) VALUE "111010270".
      *    ＜老＞紹介患者加算１（病院）
           03  CONS-SYOSIN-KASAN16 PIC X(09) VALUE "111702770".
      *    ＜老＞紹介患者加算２（病院）
           03  CONS-SYOSIN-KASAN17 PIC X(09) VALUE "111702870".
      *    ＜老＞紹介患者加算３（病院）
           03  CONS-SYOSIN-KASAN18 PIC X(09) VALUE "111702370".
      *    ＜老＞紹介患者加算４（病院）
           03  CONS-SYOSIN-KASAN19 PIC X(09) VALUE "111702470".
      *    ＜老＞紹介患者加算５（病院）
           03  CONS-SYOSIN-KASAN20 PIC X(09) VALUE "111702570".
      *    ＜老＞紹介患者加算６（病院）
           03  CONS-SYOSIN-KASAN21 PIC X(09) VALUE "111702670".
      *    初診（乳幼児）（夜間）加算
           03  CONS-SYOSIN-KASAN22 PIC X(09) VALUE "111011570".
      *    初診（乳幼児）（休日）加算
           03  CONS-SYOSIN-KASAN23 PIC X(09) VALUE "111011670".
      *    初診（乳幼児）（深夜）加算
           03  CONS-SYOSIN-KASAN24 PIC X(09) VALUE "111011770".
      *    初診（時間外特例）加算
           03  CONS-SYOSIN-KASAN25 PIC X(09) VALUE "111000870".
      *    ＜老＞初診（時間外特例）加算
           03  CONS-SYOSIN-KASAN26 PIC X(09) VALUE "111700870".
      *
      *    検査の回数に集計しない診療行為
      *    採血（Ｂ−Ａ）
           03  WRK-CONS-SAIKETSU1      PIC X(09)   VALUE "160101210".
      *    採血（Ｂ−Ｖ）
           03 WRK-CONS-SAIKETSU2       PIC X(09)   VALUE "160095710".
      *    採血（Ｂ−Ｃ）
           03 WRK-CONS-SAIKETSU3       PIC X(09)   VALUE "160095810".
      *
      *    腰部固定帯加算
           03  WRK-CONS-YOUBUKASAN     PIC X(09)   VALUE "140037490".
      *    胸部固定帯加算
           03  WRK-CONS-KYOBUKASAN     PIC X(09)   VALUE "140040110".
      *    創外固定器使用加算
           03  WRK-CONS-SOGAIKASAN     PIC X(09)   VALUE "150266970".
      *
      *    フィルム料（乳幼児）加算
           03  WRK-CONS-FILM-YOUJI-KASAN
                                       PIC X(09)   VALUE 
                                                   "799990070".
      *    酸素補正率
           03  WRK-CONS-SANSO          PIC X(09)   VALUE 
                                                   "770020070".
      *    高気圧酸素加算
           03  WRK-CONS-SANSO2         PIC X(09)   VALUE 
                                                   "770030070".
      *
      *    パス名領域
       01  WRK-PATH                        PIC X(64).
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
      *    点数
      **** COPY    "CPTENSU.INC".
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
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
                   UNTIL       SPA-STS     =   2
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SPA-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  TBL-ZAI-AREA
           INITIALIZE                  TBL-ZAIW-AREA
      *
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
           PERFORM 120-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0460"      TO  JOB-PGID
           MOVE    "点数摘要欄編集（労災・自賠責）"                      
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
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
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           MOVE    "RECE410"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
      *
           MOVE    "RECE44A"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
      *
           MOVE    "RECE45A"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
      *
           MOVE    "RECE460"           TO  RECE46PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE46PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    I-O                 RECE41-FILE
      *
           OPEN    INPUT               RECE44-FILE
      *
           OPEN    INPUT               RECE45-FILE
      *
           OPEN    OUTPUT              RECE46X-FILE
           CLOSE                       RECE46X-FILE
      *
           OPEN    I-O                 RECE46-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *        行カウンタ初期化
           MOVE    ZERO                TO  WRK-LINENUM
      *    検査回数初期化
           MOVE    ZERO                TO  WRK-KENSA-KAISU
      *
           MOVE    ZERO                TO  CNT-SRYKBN60   
           INITIALIZE                      WRK-RECE44-TABLE
                                           WRK-RECE44-MAX 
                                           WRK-SUM-KAISU-TABLE
      *
      *    パラメータ０４ファイルより編集
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPID       TO  RECE44-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
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
               UNTIL  ( SPA-STS              =   2                   )
               OR     ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPID    NOT =   RECE44-HOSPID       )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               MOVE    ZERO                  TO  FLG-SRYCD1
      *
               INITIALIZE                      TBL-ZAIW-AREA
               MOVE    ZERO                TO  WRK-ZAIWMAX-G
               MOVE    ZERO                TO  WRK-TBLMAX
               MOVE    ZERO                TO  FLG-TEIGEN
                                               FLG-COMMENT
                                               FLG-COMMENT2
                                               FLG-HIKAKIN 
               MOVE    ZERO                TO  WRK-RECE45
                                               IDY                           
      *
               PERFORM 2002-TBLZAIW-TAIHI-SEC
      *
      *        レセプト０４剤編集前処理
               PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
      *
               IF      SPA-STS          =   ZERO
      * 
      *            レセプト０４ファイル編集処理
                   EVALUATE    RECE44-SRYKBN
      *                初診
                       WHEN    "11"
                           PERFORM 2003-SRYKBN11-HENSYU-SEC
      *                再診
                       WHEN    "12"
                           PERFORM 2004-SRYKBN12-HENSYU-SEC
      *                検査
                       WHEN    "60"
                           PERFORM 2006-SRYKBN60-CHECK-SEC 
      *                それ以外
                       WHEN    OTHER
                           PERFORM 2005-SRYKBNELSE-HENSYU-SEC
                   END-EVALUATE
      *
                   ADD     1               TO  CNT-RECE44
      *
                   PERFORM 990-RECE44-NEXT-SEC
      *
      *            処理中止設定処理
                   IF      CNT-IN-CANCEL   =   50
                       MOVE    ZERO            TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
               END-IF     
           END-PERFORM
      *
           IF      SPA-STS         =   ZERO
      *        検査があるときまとめる
               IF      WRK-RECE44-MAX          >   ZERO
                   PERFORM 2007-SRYKBN60-HENSYU-SEC
               END-IF         
      *
      *        検査のとき回数を編集する
               IF      WRK-KENSA-KAISU        >   ZERO
                   IF      RECE41-HKNKBN          =   "3"
                       MOVE    WRK-KENSA-KAISU    TO  RECE41-KAISU  (24)
                   ELSE
                       MOVE    WRK-KENSA-KAISU    TO  RECE41-KAISU  (29)
                   END-IF
                   REWRITE RECE41-REC
               END-IF
      *
               PERFORM 990-RECE41-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF    
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０４より剤編集前処理
      *****************************************************************
       2001-RECE44-ZAIHEN-MAE-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAI
           MOVE    "＊"                TO  WRK-ZAI-MK
           MOVE    ZERO                TO  WRK-CLM
      *    点数・回数セット
           MOVE    RECE44-ZAITEN       TO  WRK-ZAITENX
           MOVE    RECE44-ZAIKAISU     TO  WRK-ZAIKAISUX
      *    必要有効桁数・開始位置・回数編集
           PERFORM 810-TENSUKAI-HEN-SEC
      *
           .
       2001-RECE44-ZAIHEN-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤　明細テーブル退避処理
      *****************************************************************
       2002-TBLZAIW-TAIHI-SEC    SECTION.
      *
      *    パラメータ０５ファイルより編集
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPID       TO  RECE45-HOSPID
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( SPA-STS              =   2                   )
               OR     ( FLG-RECE45           =   1                   )
               OR     ( RECE44-HOSPID    NOT =   RECE45-HOSPID       )
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
                         OR      ( SPA-STS           =   2     ) 
                 IF      RECE45-SIKFLG (IDX)         =   "1"        
      *
                   IF      RECE45-SRYCD (IDX) (1:1)  =   "1"
                       CONTINUE
                   ELSE
                       MOVE    1                     TO  FLG-SRYCD1
                   END-IF
      *              
                   IF      RECE44-SRYKBN     <   "31"
                       MOVE    1             TO  IDY
                   ELSE                          
                       EVALUATE    RECE45-SRYCD (IDX) (1:1)
                           WHEN    "0"
                             IF   RECE41-HKNKBN    NOT  =  "4" 
                               IF      RECE45-SRYCD (IDX) (1:3) = "059"
                                   IF   ( RECE44-SRYKBN     =   "70" )
                                   AND  ( RECE45-DATAKBN (IDX)
                                                            =   "3"  )
                                       MOVE    3             TO  IDY
                                   ELSE
                                       MOVE    4             TO  IDY
                                   END-IF
                               ELSE
                                   IF  RECE45-SRYCD (IDX) (1:3) = "008"
                                     IF   RECE45-SRYSYUKBN  =  "334"
                                         MOVE    5         TO  IDY
                                     ELSE
                                       IF      IDY          =   ZERO
                                         MOVE    2         TO  IDY
                                       END-IF
                                     END-IF
                                   ELSE
                                     IF      IDY           =   ZERO
                                         MOVE    1         TO  IDY
                                     END-IF
                                   END-IF
                               END-IF
                             ELSE
                               IF      RECE45-SRYCD (IDX) (1:3) = "059"
                                       MOVE    4             TO  IDY
                               ELSE
                                   IF  RECE45-SRYCD (IDX) (1:3) = "008"
                                      IF   RECE45-SRYSYUKBN  =  "334"
                                          MOVE    5         TO  IDY
                                      ELSE
                                        IF      IDY          =   ZERO
                                          MOVE    2         TO  IDY
                                        END-IF
                                      END-IF
                                   ELSE
                                      IF      IDY           =   ZERO
                                          MOVE    1         TO  IDY
                                      END-IF
                                   END-IF
                               END-IF
                             END-IF
                           WHEN    "8"
                               IF   RECE45-SRYSYUKBN  =  "334"
                                   MOVE    5         TO  IDY
                               ELSE
                                 IF (     IDY            =   ZERO      )
                                 OR ( RECE45-SRYCD (IDX) = CONS-RINYAK )
                                   MOVE    2         TO  IDY
                                 END-IF
                               END-IF
                           WHEN    "1"
                               IF   RECE41-HKNKBN    NOT  =  "4" 
                                   MOVE    3                 TO  IDY
                               ELSE
                                   IF  RECE45-SRYCD (IDX)    =
                                           WRK-CONS-YOUBUKASAN
                                        OR WRK-CONS-KYOBUKASAN
                                        OR WRK-CONS-SOGAIKASAN
                                     IF  RECE41-KOTEIKBN = SPACE OR "2"
                                       MOVE    4             TO  IDY
                                     ELSE
                                       MOVE    3             TO  IDY
                                     END-IF
                                   ELSE
                                       MOVE    3             TO  IDY
                                   END-IF
                               END-IF
                           WHEN    "6"
                               MOVE    5             TO  IDY
                           WHEN    "7"
                             IF   RECE41-HKNKBN    NOT  =  "4" 
                               IF    ( RECE44-SRYKBN     =   "70" )
                               AND   ( RECE45-DATAKBN (IDX)
                                                         =   "3"  )
                                   MOVE    3                 TO  IDY
                               ELSE
                                   IF  RECE45-SRYCD (IDX)    =
                                           WRK-CONS-FILM-YOUJI-KASAN
                                       MOVE    3             TO  IDY
                                   ELSE
                                       MOVE    4             TO  IDY
                                   END-IF
                               END-IF
                             ELSE
                                       MOVE    4             TO  IDY
                             END-IF
                           WHEN    OTHER
                               DISPLAY "ORCR0460 SRYCD="
                                                 RECE45-SRYCD (IDX) 
                               MOVE    "診療行為コードが範囲外です"
                                                 TO  WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               MOVE    2             TO  SPA-STS
                               GO  TO  2002-TBLZAIW-TAIHI-EXT
                       END-EVALUATE
                   END-IF            
                   ADD     1                 TO  WRK-ZAIWMAX (IDY)
                                                 WRK-ZAIWMAX (6)
                   IF      WRK-ZAIWMAX (IDY) >   TBL-ZAI-MAX 
                       MOVE    "テーブルがオーバーしました"
                                                 TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    2                 TO  SPA-STS
                       GO  TO  2002-TBLZAIW-TAIHI-EXT
                   ELSE
      *                先頭の診療行為コード編集              
                       IF      WRK-RECE45        =   ZERO
                         IF  ( RECE45-SRYCD(IDX)(1:1)  =  "8"       )
                         OR  ((RECE45-SRYCD(IDX)(1:3)  NOT =  "059") AND
                              (RECE45-SRYCD(IDX)(1:1)  =  "0"      ))
                           CONTINUE
                         ELSE
                           MOVE    RECE45-SRYCD (IDX)
                                                 TO  TBL-ZAIW-SRYCD1 
                           MOVE    RECE45-CDKBN-KBN(IDX)
                                                 TO  TBL-ZAIW-CDKBN-KBN
                           MOVE    RECE45-CDKBN-SYO(IDX)
                                                 TO  TBL-ZAIW-CDKBN-SYO
                           MOVE    RECE45-CDKBN-BU(IDX)
                                                 TO  TBL-ZAIW-CDKBN-BU
                           MOVE    RECE45-CDKBN-KBNNUM(IDX)
                                         TO  TBL-ZAIW-CDKBN-KBNNUM
                           MOVE    RECE45-CDKBN-KBNNUM-EDA(IDX)
                                         TO  TBL-ZAIW-CDKBN-KBNNUM-EDA
                           MOVE    RECE45-CDKBN-KOUBAN(IDX)
                                         TO  TBL-ZAIW-CDKBN-KOUBAN
                           MOVE    RECE45-KOUHYOJYUNNUM(IDX)
                                         TO  TBL-ZAIW-KOUHYOJYUNNUM
                           MOVE    RECE45-KNSJISKBN(IDX)
                                         TO  TBL-ZAIW-KNSJISKBN
                           MOVE    1             TO  WRK-RECE45
                         END-IF
                       END-IF
      *                                                         
                       MOVE    WRK-ZAIWMAX (IDY) TO  IDZ
                       MOVE    RECE45-SRYACTINF (IDX)
                                     TO  TBL-ZAIW-SRYACTINF (IDY IDZ)
                       MOVE    RECE45-NAMEINF   (IDX)
                                     TO  TBL-ZAIW-NAMEINF   (IDY IDZ)
                       MOVE    RECE45-HOUKNSKBNINF(IDX)
                                     TO  TBL-ZAIW-HOUKNSKBNINF(IDY IDZ)
                       IF      RECE45-SRYCD (IDX) (1:1)
                                                 =   "1"
                       AND     RECE44-SRYKBN     =   "70"
                       AND     RECE45-TANICD(IDX)
                                                 =   WRK-CONS-TANICD
                           MOVE    WRK-CONS-TANINAME TO 
                                         TBL-ZAIW-TANINAME  (IDY IDZ) 
                       END-IF                     
      *
                       IF      RECE45-SRYCD (IDX) (1:1)  =  "7"
                          IF    ( RECE44-SRYKBN     =   "70" )
                          AND   ( RECE45-DATAKBN (IDX)
                                                    =   "3"  )
                            IF  TBL-ZAIW-TANINAME  (IDY IDZ)  
                                                    =   SPACE
                                  MOVE    "＊"  TO 
                                         TBL-ZAIW-TANINAME  (IDY IDZ) 
                            END-IF
                          ELSE
                            IF      RECE45-SRYCD (IDX)    =
                                     WRK-CONS-FILM-YOUJI-KASAN  OR
                                     WRK-CONS-SANSO             OR
                                     WRK-CONS-SANSO2
                               CONTINUE
                            ELSE
                               IF  TBL-ZAIW-TANINAME  (IDY IDZ)  
                                                    =   SPACE
                                  MOVE    "＊"  TO 
                                         TBL-ZAIW-TANINAME  (IDY IDZ) 
                               END-IF
                            END-IF
                          END-IF
                       END-IF
      *
                       IF      RECE45-SRYCD (IDX) (1:1)  =  "6"
                          IF   RECE45-SRYCD (IDX) (1:2)  =  "63"
                               CONTINUE
                          ELSE
                            IF  TBL-ZAIW-TANINAME  (IDY IDZ)  
                                                    =   SPACE
                                  MOVE    "＊"  TO 
                                         TBL-ZAIW-TANINAME  (IDY IDZ) 
                            END-IF
                          END-IF
                       END-IF
      *
      *---- (01.00.05) LINE ADD START --------------------------------
                       IF      RECE45-SRYCD(IDX) =   CONS-COMMENT1   OR
                                                     CONS-COMMENT2   OR
                                                     CONS-COMMENT3   OR
                                                     CONS-COMMENT4 
                           MOVE    1             TO  FLG-COMMENT
                       END-IF 
                       IF      RECE45-SRYCD(IDX) =   CONS-COMMENT5   OR 
                                                     CONS-COMMENT6   OR 
                                                     CONS-COMMENT7   OR 
                                                     CONS-COMMENT8      
                           MOVE    1             TO  FLG-COMMENT2       
                       END-IF                                           
      *---- (01.00.05) LINE ADD END --------------------------------
      *
                       IF      RECE45-SRYCD(IDX) =   CONS-TEIGEN
                           MOVE    1             TO  FLG-TEIGEN
                       END-IF 
                       IF      RECE44-SRYKBN     =   "31"
                       AND     RECE45-SRYCD(IDX) =   CONS-HIKAKIN
                           IF  (RECE44-YKZTEN     =   ZERO)
                           AND (RECE44-KIZAITEN   =   ZERO)
                               CONTINUE
                           ELSE
                               MOVE    1         TO  FLG-HIKAKIN
                           END-IF
                       END-IF  
                   END-IF
                 END-IF  
               END-PERFORM
               ADD     1                 TO  CNT-RECE45
      *
               PERFORM 990-RECE45-NEXT-SEC
           END-PERFORM
      *
           .
       2002-TBLZAIW-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分（１１）編集処理
      *****************************************************************
       2003-SRYKBN11-HENSYU-SEC    SECTION.
      *
      *    剤　明細テーブルチェック
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           MOVE    ZERO              TO  WRK-TENSUGOKEI
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       1  
               PERFORM VARYING IDX2    FROM    1   BY  1
                         UNTIL IDX2    >   WRK-ZAIWMAX (IDX1)  
                   MOVE    IDX1          TO  WRK-WIDX1
                   MOVE    IDX2          TO  WRK-WIDX2
                   PERFORM 20031-SRYKBN11-CHECK-SEC
                   IF  FLG-NOPRT         =   ZERO
      *            レセプト０５剤編集処理
                       PERFORM 430-TBLZAI-HEN-SEC
                   END-IF
               END-PERFORM
           END-PERFORM    
      *
           IF  WRK-TBLMAX           >   ZERO
      *        剤明細数と印字対象明細数が異なる場合
               IF  WRK-TBLMAX       NOT =   RECE44-MEISAISU
                   PERFORM 400-TENSU-SAIKEISAN-SEC
               END-IF
      *        点数・回数セット処理
               MOVE    WRK-TBLMAX       TO  IDY
               PERFORM 440-TNSKAI-SET-SEC
               MOVE    IDY              TO  WRK-TBLMAX
      *        剤・診療行為テーブル出力処理
               PERFORM 450-RECE46-OUT-SEC
           END-IF
      *
           .
       2003-SRYKBN11-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細印字チェック 診療区分（１１） 処理
      *****************************************************************
       20031-SRYKBN11-CHECK-SEC        SECTION.
      * 
           MOVE    ZERO          TO  FLG-NOPRT
      *
      *    服用方法でないこと
           IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:3)   =   "001"
               MOVE    1             TO  FLG-NOPRT
           END-IF
      *    紹介患者加算服用方法でないこと
           IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)  
                                            =   CONS-SYOSIN-KASAN1  OR
                                                CONS-SYOSIN-KASAN2  OR
                                                CONS-SYOSIN-KASAN5  OR
                                                CONS-SYOSIN-KASAN6  OR
                                                CONS-SYOSIN-KASAN7  OR
                                                CONS-SYOSIN-KASAN8  OR
                                                CONS-SYOSIN-KASAN9  OR
                                                CONS-SYOSIN-KASAN10 OR
                                                CONS-SYOSIN-KASAN11 OR
                                                CONS-SYOSIN-KASAN12 OR
                                                CONS-SYOSIN-KASAN13 OR
                                                CONS-SYOSIN-KASAN14 OR
                                                CONS-SYOSIN-KASAN15 OR
                                                CONS-SYOSIN-KASAN16 OR
                                                CONS-SYOSIN-KASAN17 OR
                                                CONS-SYOSIN-KASAN18 OR
                                                CONS-SYOSIN-KASAN19 OR
                                                CONS-SYOSIN-KASAN20 OR
                                                CONS-SYOSIN-KASAN21 OR
                                                CONS-SYOSIN-KASAN22 OR
                                                CONS-SYOSIN-KASAN23 OR
                                                CONS-SYOSIN-KASAN24 OR
                                                CONS-SYOSIN-KASAN25 OR
                                                CONS-SYOSIN-KASAN26 OR
                                                CONS-COMMENT5       OR
                                                CONS-COMMENT6       OR
                                                CONS-COMMENT7       OR
                                                CONS-COMMENT8        
               CONTINUE
           ELSE
               MOVE    1             TO  FLG-NOPRT
           END-IF
      *
      *    識別が点数の場合
           IF  FLG-NOPRT                     =   ZERO
               IF      TBL-ZAIW-TENSIKIBETU (WRK-WIDX1 WRK-WIDX2)
                                             =   3
                   ADD     TBL-ZAIW-TEN (WRK-WIDX1 WRK-WIDX2)
                                             TO  WRK-TENSUGOKEI
               END-IF
           END-IF
      *
           .
       20031-SRYKBN11-CHECK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    レセプト０４ファイル  診療区分（１２）編集処理
      *****************************************************************
       2004-SRYKBN12-HENSYU-SEC    SECTION.
      *
      *    剤　明細テーブルチェック
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           MOVE    ZERO              TO  WRK-TENSUGOKEI
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       1  
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >   WRK-ZAIWMAX (IDX1)  
                   MOVE    IDX1          TO  WRK-WIDX1
                   MOVE    IDX2          TO  WRK-WIDX2
                   PERFORM 20041-SRYKBN12-CHECK-SEC
                   IF  FLG-NOPRT         =   ZERO
      *                レセプト０５剤編集処理
                       PERFORM 430-TBLZAI-HEN-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM
      *
           IF  WRK-TBLMAX           >   ZERO
      *        剤明細数と印字対象明細数が異なる場合
               IF  WRK-TBLMAX       NOT =   RECE44-MEISAISU
                   PERFORM 400-TENSU-SAIKEISAN-SEC
               END-IF
      *        点数・回数セット処理
               MOVE    WRK-TBLMAX       TO  IDY
               PERFORM 440-TNSKAI-SET-SEC
               MOVE    IDY              TO  WRK-TBLMAX
      *        剤・診療行為テーブル出力処理
               PERFORM 450-RECE46-OUT-SEC
           END-IF
      *
           .
       2004-SRYKBN12-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細印字チェック 診療区分（１２） 処理
      *****************************************************************
       20041-SRYKBN12-CHECK-SEC        SECTION.
      * 
           MOVE    ZERO              TO  FLG-NOPRT
      *
      *    服用方法でないこと
           IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:3)
                                     =   "001"
               MOVE    1                 TO  FLG-NOPRT
           END-IF
      *
      *    診療行為テーブルに存在した場合、印字しないこと
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL   ( IDY           >   TBL-NOSRYCD-MAX )
                     OR      ( FLG-NOPRT     =    1              )
               IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2) 
                                         =   TBL-NOSRYCD (IDY)
                   MOVE    1                 TO  FLG-NOPRT
      *            ＊外来管理加算の場合
                   IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                                         =   "101120070"  OR
                                             "101120080"
                     IF  RECE41-GAIRAIKSN-PRTKBN   =   "1"
                         MOVE    ZERO          TO  FLG-NOPRT
                     END-IF
                   END-IF
      *            一つの剤に他の明細も存在する場合
      ***          IF  RECE44-MEISAISU       >   1
      ***              MOVE    ZERO          TO  FLG-NOPRT
      ***          END-IF
      *            ＊再診ダミーの場合
                   IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)          
                                         =   "099120001"             
                       MOVE    1             TO  FLG-NOPRT           
                   END-IF                                            
               END-IF
           END-PERFORM
      *
           IF  FLG-NOPRT             =   ZERO
      *        識別が点数の場合
               IF  TBL-ZAIW-TENSIKIBETU (WRK-WIDX1 WRK-WIDX2)
                                     =   3
                   ADD     TBL-ZAIW-TEN (WRK-WIDX1 WRK-WIDX2)
                                         TO  WRK-TENSUGOKEI
               END-IF
           END-IF    
      *
           .
       20041-SRYKBN12-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０４ファイル  診療区分（その他）編集処理
      *****************************************************************
       2005-SRYKBNELSE-HENSYU-SEC    SECTION.
      *    処方のとき
           IF  RECE44-SRYKBN         =   "25"
               EVALUATE      RECE41-HKNKBN                              
                 WHEN      "1"                                          
                 WHEN      "2"                                          
                   IF    ( RECE41-KHNTEN  (19)     =   ZERO )  AND
                         ( WRK-ZAIWMAX    (6)      >   1    )
                       PERFORM 20042-SRYKBNELSE-REIGAI-SEC
                   ELSE
                       PERFORM 20052-SRYKBNELSE-CHECK-SEC
                   END-IF
                 WHEN      "3"                                          
                   IF    ( RECE41-KHNTEN  (12)     =   ZERO )  AND      
                         ( WRK-ZAIWMAX    (6)      >   1    )           
                       PERFORM 20042-SRYKBNELSE-REIGAI-SEC              
                   ELSE                                                 
                       PERFORM 20052-SRYKBNELSE-CHECK-SEC               
                   END-IF                                               
                 WHEN      "4"                                          
                   IF    ( RECE41-KHNTEN  (14)     =   ZERO )  AND      
                         ( WRK-ZAIWMAX    (6)      >   1    )           
                       PERFORM 20042-SRYKBNELSE-REIGAI-SEC              
                   ELSE                                                 
                       PERFORM 20052-SRYKBNELSE-CHECK-SEC               
                   END-IF                                               
               END-EVALUATE                                             
           ELSE            
      *
      *        検査のとき剤が一つ以上なら”、”で編集
               IF  RECE44-SRYKBN         =   "60"
                   PERFORM 20051-SRYKBNELSE-KENSA-SEC
               ELSE 
                   PERFORM 20052-SRYKBNELSE-CHECK-SEC
               END-IF
           END-IF    
      *
           IF  WRK-TBLMAX           >   ZERO
               IF      RECE44-SRYKBN      >=  "33"
                   CONTINUE
               ELSE        
                   MOVE    WRK-TBLMAX       TO  IDY
                   PERFORM 440-TNSKAI-SET-SEC
                   MOVE    IDY              TO  WRK-TBLMAX
               END-IF    
      *        剤・診療行為テーブル出力処理
               PERFORM 450-RECE46-OUT-SEC
           END-IF
      *
           .
       2005-SRYKBNELSE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤　明細テーブル編集　 診療区分（その他）検査のとき処理
      *****************************************************************
       20051-SRYKBNELSE-KENSA-SEC      SECTION.
      *
           MOVE    WRK-ZAIWMAX (3)     TO  WRK-KENSA-MAX
           MOVE    ZERO                TO  WRK-TNS-HOUKNSKBN
                                           WRK-KOUMOKUSU
      *     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-ZAIWMAX (3)
               MOVE    TBL-ZAIW-NAMEINF (3 IDX)  TO  TBL-NAMEINF (IDX)
               MOVE    TBL-ZAIW-SRYSURYO(3 IDX)  TO  TBL-SRYSURYO(IDX)
               MOVE    TBL-ZAIW-TANICD  (3 IDX)  TO  TBL-TANICD  (IDX)
               MOVE    TBL-ZAIW-TANIMOJISU(3 IDX)
                                             TO  TBL-TANIMOJISU(IDX)
               MOVE    TBL-ZAIW-TANINAME(3 IDX)  TO  TBL-TANINAME(IDX)
               MOVE    TBL-ZAIW-SRYCD   (3 IDX)  TO  TBL-SRYCD   (IDX)
               MOVE    TBL-ZAIW-HOUKNSKBN(3 IDX) TO  TBL-HOUKNSKBN(IDX)
               IF      TBL-YUKOKETA (IDX)      =   ZERO
                   PERFORM VARYING IDZ FROM   200 BY  -1
                           UNTIL   IDZ <      1
                       IF      TBL-NAME (IDX) (IDZ:1)    NOT =   SPACE
                           COMPUTE TBL-YUKOKETA (IDX)    =
                                   IDZ     /   2
                           MOVE    1          TO  IDZ
                       END-IF
                   END-PERFORM
               END-IF             
           END-PERFORM
      *
           COMPUTE WRK-ZAIWMAX (6)   =   WRK-ZAIWMAX  (6)     -   
                                         WRK-ZAIWMAX  (3) 
           INITIALIZE                    TBL-ZAIW-OCC (3)
           MOVE    ZERO              TO  WRK-ZAIWMAX  (3)
      *
      *    剤が一つ以上のとき’、’で名称を編集する
      *
           MOVE    1                 TO  IDX
               
           PERFORM UNTIL   IDX     >       WRK-KENSA-MAX
      ****     MOVE    TBL-SRYCD(IDX)       TO      TNS-SRYCD
      ****     MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
      ****     MOVE    RECE44-SRYYM         TO      ORC-DBYMD(1:6)
      ****     MOVE    "01"                 TO      ORC-DBYMD(7:2)
      ****     PERFORM 900-TENSU-INV-SEC
      ****     IF      TNS-HOUKNSKBN        =   ZERO
               IF      TBL-HOUKNSKBN(IDX)   =   ZERO
                   ADD     1                        TO  WRK-ZAIWMAX (3)
                   MOVE    WRK-ZAIWMAX (3)          TO  IDZ
                   MOVE    TBL-NAMEINF (IDX)   TO
                                    TBL-ZAIW-NAMEINF (3 IDZ)
                   MOVE    TBL-SRYSURYO(IDX)   TO
                                    TBL-ZAIW-SRYSURYO(3 IDZ)
                   MOVE    TBL-TANICD  (IDX)   TO
                                    TBL-ZAIW-TANICD  (3 IDZ)
                   MOVE    TBL-TANIMOJISU(IDX) TO
                                    TBL-ZAIW-TANIMOJISU(3 IDZ)
                   MOVE    TBL-TANINAME(IDX)   TO
                                    TBL-ZAIW-TANINAME(3 IDZ)
                   MOVE    TBL-SRYCD   (IDX)   TO
                                    TBL-ZAIW-SRYCD   (3 IDZ)
                   MOVE    TBL-HOUKNSKBN(IDX)  TO
                                    TBL-ZAIW-HOUKNSKBN(3 IDZ)
                   ADD     1                   TO  IDX                     
               ELSE
                   IF      IDX             =   1
      ****             MOVE    TNS-HOUKNSKBN    
                       MOVE    TBL-HOUKNSKBN(IDX)
                                           TO  WRK-TNS-HOUKNSKBN         
                   END-IF                                
                   MOVE    ZERO            TO  WRK-YUKOKETA
                   MOVE    SPACE           TO  WRK-NAME
                   MOVE    TBL-SRYCD (IDX) TO  WRK-SRYCD
                   PERFORM     UNTIL   IDX >       WRK-KENSA-MAX
                               OR      TBL-SRYCD (IDX) (1:1)
                                           NOT =   WRK-SRYCD
      *                次の名称があるとき”、”＋名称をつける
                       IF      WRK-YUKOKETA    >   ZERO
                           STRING  WRK-NAME    DELIMITED  BY  SPACE
                                   "，"        DELIMITED  BY  SIZE
                                   INTO        WRK-NAME
                           END-STRING        
                           ADD     2           TO  WRK-YUKOKETA
      *              
                           COMPUTE IDZ     =   WRK-YUKOKETA        +
                                               TBL-YUKOKETA (IDX)  *  2
                           IF      IDZ             >   38
                               ADD     1               TO
                                                   WRK-ZAIWMAX (3)
                               MOVE    WRK-ZAIWMAX (3) TO  IDZ
                               MOVE    WRK-NAME        TO
                                           TBL-ZAIW-NAME(3 IDZ)
                               COMPUTE TBL-ZAIW-YUKOKETA (3 IDZ) 
                                           =   WRK-YUKOKETA   /   2  
                               MOVE    SPACE           TO  WRK-NAME
                               MOVE    ZERO            TO  WRK-YUKOKETA
                           END-IF
                       END-IF
      *
                       IF      TBL-YUKOKETA (IDX)  >   19
                           ADD     1               TO  WRK-ZAIWMAX (3)
                           MOVE    WRK-ZAIWMAX (3) TO  IDZ
                           MOVE    TBL-NAME (IDX) (1:38)   TO
                                       TBL-ZAIW-NAME    (3 IDZ)
                           MOVE    19                      TO  
                                       TBL-ZAIW-YUKOKETA(3 IDZ)
                           MOVE    TBL-NAME (IDX) (39:)    TO  WRK-NAME
                           COMPUTE WRK-YUKOKETA    =  
                                   ( TBL-YUKOKETA (IDX) -   19 )  *   2                                           
                       ELSE
                           STRING  WRK-NAME        DELIMITED  BY  SPACE
                                   TBL-NAME    (IDX)
                                                   DELIMITED  BY  SPACE
                                   INTO            WRK-NAME
                           END-STRING
                           COMPUTE WRK-YUKOKETA =  WRK-YUKOKETA   +
                                       TBL-YUKOKETA (IDX)  *   2
                       END-IF
      *                 
                       ADD     1           TO  IDX
                       ADD     1           TO  WRK-KOUMOKUSU 
                   END-PERFORM                            
                   ADD     1               TO  WRK-ZAIWMAX (3)
                   MOVE    WRK-ZAIWMAX (3) TO  IDZ
                   MOVE    WRK-NAME    TO  TBL-ZAIW-NAME (3 IDZ)
                   COMPUTE TBL-ZAIW-YUKOKETA (3 IDZ) 
                                       =   WRK-YUKOKETA   /   2
               END-IF                                        
           END-PERFORM
      *
      *    検査の項目数をコメントにセット
           IF      WRK-TNS-HOUKNSKBN  >   ZERO
               INITIALIZE                 WRK-CHAR-HENKAN-AREA
               MOVE    1                  TO  WRK-CHAR-FLG
      *********MOVE    WRK-KENSA-MAX      TO  WRK-Z9
               MOVE    WRK-KOUMOKUSU      TO  WRK-Z9
               MOVE    WRK-Z9 (3:2)       TO  WRK-CHAR-X
               MOVE    2                  TO  WRK-CHAR-CNT
               PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
               ADD     1                  TO  WRK-ZAIWMAX (2)
                                              WRK-ZAIWMAX (6)
               MOVE    WRK-ZAIWMAX (2)    TO  IDZ   
               STRING  WRK-CONS-KENSACOMMENT (WRK-TNS-HOUKNSKBN)
                                          DELIMITED  BY  SPACE
                       "（"               DELIMITED  BY  SIZE
                       WRK-CHAR-N         DELIMITED  BY  SPACE
                       "項目）"           DELIMITED  BY  SIZE
                                          INTO    TBL-ZAIW-NAME (2 IDZ)
               PERFORM VARYING IDX FROM   200 BY  -1
                       UNTIL   IDX <      1
                   IF      TBL-ZAIW-NAME (2 IDZ) (IDX:1)
                                              NOT =   SPACE
                       COMPUTE TBL-ZAIW-YUKOKETA (2 IDZ)    =
                                                  IDX     /   2
                       MOVE    1          TO  IDX
                   END-IF
               END-PERFORM
               MOVE    "88888888"         TO  TBL-ZAIW-SRYCD(2 IDZ)
           END-IF                                    
      *
           COMPUTE WRK-ZAIWMAX (6)   =   WRK-ZAIWMAX  (6)     +   
                                         WRK-ZAIWMAX  (3) 
           MOVE    TBL-ZAIW-SRYCD1   TO  TBL-ZAIW-SRYCD (3 1) 
      *
           PERFORM 20052-SRYKBNELSE-CHECK-SEC
           .
       20051-SRYKBNELSE-KENSA-EXT.
           EXIT.
      *****************************************************************
      *    剤　明細テーブルチェック 診療区分（その他） 処理
      *****************************************************************
       20052-SRYKBNELSE-CHECK-SEC      SECTION.
      *
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           MOVE    SPACE             TO  WRK-ZAI
           MOVE    "＊"              TO  WRK-ZAI-MK
      *     
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       5
      *
      *        薬剤と器材の表示を逆にする
               EVALUATE    IDX3
                   WHEN    4
                       MOVE    5               TO  IDX1
                   WHEN    5
                       MOVE    4               TO  IDX1
                   WHEN   OTHER
                       MOVE    IDX3            TO  IDX1
               END-EVALUATE
      *
               IF      RECE44-SRYKBN   <=   "33"
                   MOVE    "＊"            TO  WRK-ZAI-MK
               END-IF    
      *************IF      RECE44-SRYKBN     =   "31"  OR  "32"
      *************AND     IDX1              =   3
               IF    (    (RECE44-YKZTEN     =   ZERO )
                      AND (IDX1              =   5    ) )
               OR    (    (RECE44-KIZAITEN   =   ZERO )
                      AND (IDX1              =   4    ) )
                   CONTINUE
               ELSE    
                   IF   (( RECE44-SRYKBN     =   "31"  AND
                           FLG-HIKAKIN       =   1          )  OR 
                         ( RECE44-SRYKBN     =   "32"       )     )
                   AND  (  IDX1              =   3                )
                       CONTINUE
                   ELSE    
                       IF      RECE44-SRYKBN     =   "31"  AND
                               FLG-HIKAKIN       =   ZERO  AND
                               WRK-ZAIWMAX (3)   >   ZERO  AND
                               IDX1              >   3
                           MOVE    SPACE             TO  WRK-ZAI-MK
                       END-IF            
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL   IDX2    >   WRK-ZAIWMAX (IDX1)  
                           MOVE    ZERO          TO  FLG-NOPRT
                           MOVE    IDX1          TO  WRK-WIDX1
                           MOVE    IDX2          TO  WRK-WIDX2
                           PERFORM 200521-SEYKBNELSE-MEISAI-SEC
                           IF  FLG-NOPRT         =   ZERO
      *                        レセプト０５剤編集処理
                               PERFORM 430-TBLZAI-HEN-SEC
                           END-IF    
                       END-PERFORM
                   END-IF
               END-IF     
               IF      WRK-ZAIWMAX (IDX1)    >   ZERO        
                   MOVE    SPACE             TO  WRK-ZAI
               END-IF    
      *             
      *        点数・回数セット処理（診療行為）
               IF      IDX1              =   3
               AND     WRK-ZAIWMAX(IDX1) >   ZERO
                   IF      RECE44-SRYKBN     >=  "33"       
                       MOVE    RECE44-SYUTEN1    TO  WRK-ZAITENX
                       MOVE    RECE44-ZAIKAISU   TO  WRK-ZAIKAISUX
                       PERFORM 810-TENSUKAI-HEN-SEC
                       MOVE    WRK-TBLMAX        TO  IDY
                       PERFORM 440-TNSKAI-SET-SEC
                       MOVE    IDY               TO  WRK-TBLMAX 
                   END-IF
               END-IF    
      *        点数・回数セット処理（特定器材）
               IF      IDX1              =   4
               AND     WRK-ZAIWMAX(IDX1) >   ZERO
               AND     RECE44-KIZAITEN   >   ZERO
                   IF      RECE44-SRYKBN     >=  "33"   
                       MOVE    RECE44-KIZAITEN   TO  WRK-ZAITENX
                       MOVE    RECE44-ZAIKAISU   TO  WRK-ZAIKAISUX
                       PERFORM 810-TENSUKAI-HEN-SEC
                       MOVE    WRK-TBLMAX        TO  IDY
                       PERFORM 440-TNSKAI-SET-SEC
                       MOVE    IDY               TO  WRK-TBLMAX 
                   END-IF
               END-IF     
      *        点数・回数セット処理（薬剤）
               IF      IDX1              =   5
               AND     WRK-ZAIWMAX(IDX1) >   ZERO
               AND     RECE44-YKZTEN     >   ZERO
                   IF      RECE44-SRYKBN     >=  "31"   
      *                点数・回数セット
                       IF      RECE44-SRYKBN     >=   "33"
                           MOVE    RECE44-YKZTEN     TO  WRK-ZAITENX
                           MOVE    RECE44-ZAIKAISU   TO  WRK-ZAIKAISUX
                           PERFORM 810-TENSUKAI-HEN-SEC
                           MOVE    WRK-TBLMAX        TO  IDY
                           PERFORM 440-TNSKAI-SET-SEC
                           MOVE    IDY               TO  WRK-TBLMAX 
                       END-IF          
                   END-IF
               END-IF        
           END-PERFORM
      *             
      *    検査のとき回数を集計
           IF      WRK-ZAIWMAX(3)     >   ZERO
           AND     RECE44-SRYKBN      =   "60"
               IF      TBL-ZAIW-SRYCD (3 1)   =   WRK-CONS-SAIKETSU1
                                              OR  WRK-CONS-SAIKETSU2
                                              OR  WRK-CONS-SAIKETSU3 
                   CONTINUE
               ELSE
      ****         MOVE    TBL-ZAIW-SRYCD (3 1)
      ****                                   TO  TNS-SRYCD
      ****         MOVE    TNS-TENSU-REC     TO  MCPDATA-REC
      ****         MOVE    RECE44-SRYYM      TO  ORC-DBYMD(1:6)
      ****         MOVE    "01"              TO  ORC-DBYMD(7:2)
      ****         PERFORM 900-TENSU-INV-SEC
      ****         IF      TNS-KNSJISKBN     =   "2"
                   IF      TBL-ZAIW-KNSJISKBN  =  "2"
                       CONTINUE
                   ELSE 
                       ADD     RECE44-ZAIKAISU   TO  WRK-KENSA-KAISU
                   END-IF
               END-IF
           END-IF     
      *
           .
       20052-SRYKBNELSE-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細印字チェック 診療区分（その他） 処理
      *****************************************************************
       200521-SEYKBNELSE-MEISAI-SEC        SECTION.
      *
           MOVE    ZERO          TO  FLG-NOPRT
      *
      *    服用方法でないこと
           IF  (TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:3) =   "001")
           OR  (TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)(1:1) =   "S"  )
               MOVE    1             TO  FLG-NOPRT
           END-IF
      *    診療行為テーブルに存在した場合、印字しないこと
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL   ( IDY           >   TBL-NOSRYCD-MAX )
                     OR      ( FLG-NOPRT     =    1 )
      *
               IF  TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2)
                                =   TBL-NOSRYCD (IDY)
                   MOVE    1                 TO  FLG-NOPRT
               END-IF
           END-PERFORM
      *
           IF   ( RECE44-SRYKBN               = "31"  )
           AND  ( TBL-ZAIW-NAME (3 1)         = SPACE )
           AND  ( TBL-ZAIW-NAME (3 WRK-WIDX2) = SPACE )
           AND  ( WRK-WIDX1                   = 3     )
           AND  ( WRK-WIDX2               NOT = 1     )
                   MOVE    1                 TO  FLG-NOPRT
           END-IF
      *
           .
       200521-SEYKBNELSE-MEISAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分（その他）　例外処理
      *****************************************************************
       20042-SRYKBNELSE-REIGAI-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAI
           MOVE    "＊"                TO  WRK-ZAI-MK
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       5  
      *
      *        薬剤と器材の表示を逆にする
               EVALUATE    IDX3
                   WHEN    4
                       MOVE    5               TO  IDX1
                   WHEN    5
                       MOVE    4               TO  IDX1
                   WHEN   OTHER
                       MOVE    IDX3            TO  IDX1
               END-EVALUATE
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >   WRK-ZAIWMAX (IDX1)  
                   MOVE    IDX1          TO  WRK-WIDX1
                   MOVE    IDX2          TO  WRK-WIDX2
      *            レセプト０５剤編集処理
                   PERFORM 430-TBLZAI-HEN-SEC
      ****         ADD     TBL-ZAIW-TEN (IDX IDX1) TO  WRK-TENSUGOKEI
               END-PERFORM    
           END-PERFORM
      *
           .
       20042-SRYKBNELSE-REIGAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査チェック処理
      *****************************************************************
       2006-SRYKBN60-CHECK-SEC         SECTION.
      *
      *    薬剤やコメントがあればまとめない
           IF      FLG-SRYCD1          =   1
               PERFORM 2005-SRYKBNELSE-HENSYU-SEC
           ELSE
      *        剤の明細数が２以上のときはまとめない
               IF      RECE44-MEISAISU     >   1
                   PERFORM 2005-SRYKBNELSE-HENSYU-SEC
               ELSE    
      *            包括対象でないときまとめない         
      ****         MOVE    TBL-ZAIW-SRYCD (3 1) TO      TNS-SRYCD
      ****         MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
      ****         MOVE    RECE44-SRYYM         TO      ORC-DBYMD(1:6)
      ****         MOVE    "01"                 TO      ORC-DBYMD(7:2)
      ****         PERFORM 900-TENSU-INV-SEC
      ****         IF      TNS-HOUKNSKBN        =   ZERO
      ****             PERFORM 2005-SRYKBNELSE-HENSYU-SEC
      ****         ELSE    
      ****             PERFORM 2006-SRYKBN60-TAIHI-SEC
      ****         END-IF
                   IF      TBL-ZAIW-HOUKNSKBN(3 1)  =   ZERO
                       PERFORM 2005-SRYKBNELSE-HENSYU-SEC
                   ELSE
                       PERFORM 2006-SRYKBN60-TAIHI-SEC
                   END-IF
               END-IF
           END-IF
           .
       2006-SRYKBN60-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査まとめ対象待避処理
      *****************************************************************
       2006-SRYKBN60-TAIHI-SEC         SECTION.
      *
           ADD     1               TO  WRK-RECE44-MAX
           MOVE    RECE44-REC      TO  WRK-RECE44-REC (WRK-RECE44-MAX)
           MOVE    RECE44-DAY-AREA TO  WRK-RECE44-DAY-AREA
                                                      (WRK-RECE44-MAX)
      **** MOVE    TNS-HOUKNSKBN   TO  WRK-RECE44-HOUKNSKBN
      ****                                            (WRK-RECE44-MAX)
           MOVE    TBL-ZAIW-HOUKNSKBN(3 1) TO  WRK-RECE44-HOUKNSKBN
                                                      (WRK-RECE44-MAX)
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               ADD      RECE44-KAISU (1 IDX)    TO  WRK-SUM-KAISU (IDX)
           END-PERFORM                                                       
           .
       2006-SRYKBN60-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括対象検査まとめ処理
      *****************************************************************
       2007-SRYKBN60-HENSYU-SEC         SECTION.
      * 
           INITIALIZE                   WRK-RECE44-MATOME-TABLE
                                        WRK-RECE44-FLG-TABLE
      *     
           MOVE    1                    TO  IDXX 
           PERFORM              UNTIL   IDXX >       31
      *********INITIALIZE                  WRK-RECE44-FLG-TABLE
               INITIALIZE                  WRK-KAISU-G
                                           WRK-HOUKNSKBN
                                           WRK-KENSA-SU
      *
               IF      WRK-SUM-KAISU (IDXX)   >   ZERO
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       WRK-RECE44-MAX
                       IF    ( WRK-RECE44-KAISU (IDY 1 IDXX)    
                                               >   ZERO )
                       AND   ( WRK-RECE44-FLG (IDY)
                                               =   ZERO )     
                           IF      WRK-KAISU (1)   =   ZERO
                               MOVE    WRK-RECE44-KAISU (IDY 1 IDXX)
                                                   TO  WRK-KAISU (1)
                               MOVE    WRK-RECE44-HOUKNSKBN (IDY)
                                                   TO  WRK-HOUKNSKBN                    
                           END-IF
                           IF      WRK-RECE44-KAISU (IDY 1 IDXX)
                                                   =   WRK-KAISU (1)
                           AND     WRK-RECE44-HOUKNSKBN (IDY)
                                                   =   WRK-HOUKNSKBN
                               MOVE    1           TO  WRK-RECE44-FLG 
                                                       (IDY)
                               ADD     1           TO  WRK-KENSA-SU
                           END-IF
                       END-IF
                   END-PERFORM
      *
      *            同じ回数の検査が同日に複数回あるか
                   IF      WRK-KENSA-SU       =   1
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       WRK-RECE44-MAX
                           IF    ( WRK-RECE44-FLG (IDY)    =   1     )
                           AND   ( WRK-RECE44-KAISU (IDY 1 IDXX)
                                                           =
                                                       WRK-KAISU (1) )
                               COMPUTE WRK-SUM-KAISU (IDXX) 
                                               =
                                               WRK-SUM-KAISU (IDXX) -
                                               WRK-KAISU   (1)
                               MOVE    1           TO  WRK-RECE44-MATOME 
                                                             (IDY)
                               MOVE    2           TO  WRK-RECE44-FLG 
                                                             (IDY)
                           END-IF                                       
                       END-PERFORM
                   ELSE
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       WRK-RECE44-MAX
                           IF    ( WRK-RECE44-FLG (IDY)    =   1     )
                           AND   ( WRK-RECE44-KAISU (IDY 1 IDXX)
                                                           =
                                                       WRK-KAISU (1) )
                               MOVE    ZERO        TO
                                           WRK-RECE44-KAISU (IDY 1 IDXX)
                                           WRK-RECE44-KAISU (IDY 2 IDXX)
                                           WRK-RECE44-KAISU (IDY 3 IDXX)
                                           WRK-RECE44-KAISU (IDY 4 IDXX)
                               COMPUTE WRK-SUM-KAISU (IDXX) 
                                               =
                                               WRK-SUM-KAISU (IDXX) -
                                               WRK-KAISU   (1)
                               MOVE    3           TO  WRK-RECE44-FLG 
                                                             (IDY)
                           END-IF                                       
                       END-PERFORM
                       ADD     1                  TO  WRK-KAISU1
      *
      *                同じ検査が月に何日あるか
                       COMPUTE IDX1    =   IDXX    +   1
                       PERFORM VARYING IDZ FROM    IDX1    BY  1
                               UNTIL   IDZ >       31
                         IF      WRK-SUM-KAISU (IDZ)   >   ZERO
                           MOVE    ZERO                TO  FLG-OK                            
                           PERFORM VARYING IDW FROM    1   BY  1
                                   UNTIL   IDW >       WRK-RECE44-MAX
                                   OR      FLG-OK      =   1
                               IF      WRK-RECE44-FLG   (IDW)  =   3
                                   IF      WRK-RECE44-KAISU (IDW 1 IDZ)
                                                   =   WRK-KAISU (1)
                                       MOVE    2           TO  FLG-OK                                    
                                   ELSE                    
                                       MOVE    1           TO  FLG-OK                                    
                                   END-IF
                               END-IF
                           END-PERFORM
      *                    同じ検査が同じ回数で全てあるとき                 
                           IF      FLG-OK          =   2
                               PERFORM VARYING IDW FROM    1   BY  1
                                       UNTIL   IDW >   WRK-RECE44-MAX
                                   IF      WRK-RECE44-FLG   (IDW)  =   3
                                       MOVE    ZERO    TO
                                                       WRK-RECE44-KAISU
                                                      (IDW 1 IDZ)
                                                       WRK-RECE44-KAISU
                                                      (IDW 2 IDZ)
                                                       WRK-RECE44-KAISU
                                                      (IDW 3 IDZ)
                                                       WRK-RECE44-KAISU
                                                      (IDW 4 IDZ)
                                       COMPUTE WRK-SUM-KAISU (IDZ) 
                                                   =
                                                   WRK-SUM-KAISU (IDZ) -
                                                   WRK-KAISU   (1)                                       
                                   END-IF
                               END-PERFORM
                               ADD     1              TO  WRK-KAISU1                                                               
                           END-IF  
                         END-IF  
                       END-PERFORM
      *
      *                同じ検査をまとめて出力する
                       MOVE    ZERO                  TO  FLG-SRYCD1
      *
                       INITIALIZE                      TBL-ZAIW-AREA
                       MOVE    ZERO                TO  WRK-ZAIWMAX-G
                       MOVE    ZERO                TO  WRK-TBLMAX
                       MOVE    ZERO                TO  FLG-TEIGEN
                                                       FLG-COMMENT
                                                       FLG-COMMENT2
                                                       FLG-HIKAKIN 
                       MOVE    ZERO                TO  WRK-RECE45
                                                       IDY
      *                                             
                       MOVE    SPACE               TO  RECE44-REC
                       INITIALIZE                      RECE44-REC
                       MOVE    ZERO                TO  WRK-RECE44-ZAITEN
                       PERFORM VARYING IDW FROM    1   BY  1
                               UNTIL   IDW >       WRK-RECE44-MAX
                           IF      WRK-RECE44-FLG   (IDW)  =   3
                               MOVE    ZERO   TO  WRK-RECE44-FLG
                                                             (IDW)
                               MOVE    WRK-RECE44-REC    (IDW) 
                                                   TO  RECE44-REC
                               ADD     RECE44-ZAITEN 
                                                   TO  WRK-RECE44-ZAITEN
                               PERFORM 2002-TBLZAIW-TAIHI-SEC
                           END-IF
                       END-PERFORM    
                       COMPUTE RECE44-ZAIKAISU     =
                                   WRK-KAISU (1)   *   WRK-KAISU1                
                       MOVE    WRK-RECE44-ZAITEN   TO  RECE44-SYUTEN1
                                                       RECE44-ZAITEN                                                        
      *
      *                レセプト０４剤編集前処理
                       PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
      *
                       IF      SPA-STS          =   ZERO
                           PERFORM 20051-SRYKBNELSE-KENSA-SEC
      *                    剤・診療行為テーブル出力処理
                           PERFORM 450-RECE46-OUT-SEC
                       END-IF    
                   END-IF
               END-IF
      *
               IF      WRK-SUM-KAISU (IDXX)   =   ZERO
                   ADD     1                  TO  IDXX
                   INITIALIZE                  WRK-RECE44-FLG-TABLE
               END-IF
           END-PERFORM
      *
      *    まとまらなかった検査は剤別に記載する
           PERFORM VARYING IDW     FROM    1   BY  1
                   UNTIL   IDW     >       WRK-RECE44-MAX
               MOVE    ZERO                TO  WRK-KAISU2                  
               IF      WRK-RECE44-MATOME (IDW)     =   1
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       31
                       COMPUTE WRK-KAISU2  =   WRK-KAISU2  +
                                           WRK-RECE44-KAISU (IDW 1 IDXX)
                   END-PERFORM
      *
                   MOVE    ZERO                  TO  FLG-SRYCD1
      *
                   INITIALIZE                      TBL-ZAIW-AREA
                   MOVE    ZERO                TO  WRK-ZAIWMAX-G
                   MOVE    ZERO                TO  WRK-TBLMAX
                   MOVE    ZERO                TO  FLG-TEIGEN
                                                   FLG-COMMENT
                                                   FLG-COMMENT2
                                                   FLG-HIKAKIN 
                   MOVE    ZERO                TO  WRK-RECE45
                                                   IDY
      *                                             
                   MOVE    SPACE               TO  RECE44-REC
                   INITIALIZE                      RECE44-REC
                   MOVE    ZERO                TO  WRK-RECE44-ZAITEN
                   MOVE    WRK-RECE44-REC    (IDW) 
                                               TO  RECE44-REC
                   ADD     RECE44-ZAITEN       TO  WRK-RECE44-ZAITEN
                   PERFORM 2002-TBLZAIW-TAIHI-SEC
                   MOVE    WRK-KAISU2          TO  RECE44-ZAIKAISU               
                   MOVE    WRK-RECE44-ZAITEN   TO  RECE44-SYUTEN1
                                                   RECE44-ZAITEN                                                        
      *
      *            レセプト０４剤編集前処理
                   PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
      *
                   IF      SPA-STS          =   ZERO
                       PERFORM 20051-SRYKBNELSE-KENSA-SEC
      *                剤・診療行為テーブル出力処理
                       PERFORM 450-RECE46-OUT-SEC
                   END-IF
               END-IF    
           END-PERFORM
           .
       2007-SRYKBN60-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数再計算処理
      *****************************************************************
       400-TENSU-SAIKEISAN-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAI
           MOVE    "＊"                TO  WRK-ZAI-MK
      *    点数・回数セット
           MOVE    WRK-TENSUGOKEI      TO  WRK-ZAITENX
           MOVE    RECE44-ZAIKAISU     TO  WRK-ZAIKAISUX
      *    必要有効桁数・開始位置・回数編集
           PERFORM 810-TENSUKAI-HEN-SEC
      *
           .
       400-TENSU-SAIKEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細空白処理
      *****************************************************************
       410-MEISAI-SPACE-SEC   SECTION.
      *
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL     IDY   >       WRK-TBLMAX
               IF  IDY                   =   1
                   MOVE    SPACE         TO  TBL-ZAI (IDY)(3:)
               ELSE
                   INITIALIZE                TBL-ZAI (IDY)
               END-IF
           END-PERFORM
           MOVE    1                 TO  WRK-TBLMAX
      *
           .
       410-MEISAI-SPACE-EXT.
           EXIT.
      *****************************************************************
      *    剤テーブル編集処理
      *****************************************************************
       430-TBLZAI-HEN-SEC     SECTION.
      *
      *    数量編集
           PERFORM 4301-SURYO-HEN1-SEC
      *
      *    分画数編集
           PERFORM 4301-BUNKAKU-HEN1-SEC
      *
      *    明細編集・明細桁数計算
           PERFORM 4302-MEISAI-KETA-HEN1-SEC
      *
      *    剤・診療行為テーブル編集処理
           PERFORM 4303-ZAITBL-TENSO-SEC
      *    
      *    明細が１行に収まりきらない場合
           IF  WRK-WCLM            >   50
               MOVE    SPACE             TO  WRK-ZAI-NAME
               MOVE    WRK-KOUI (51:)    TO  WRK-ZAI-NAME
               PERFORM 4303-ZAITBL-TENSO-SEC
           END-IF
      *
           .
       430-TBLZAI-HEN-EXT.
           EXIT.
      *     
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       4301-SURYO-HEN1-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    ZERO            TO  WRK-SURYOST
           MOVE    ZERO            TO  WRK-SURYOKETA
           MOVE    ZERO            TO  WRK-SURYOKETA1
           MOVE    ZERO            TO  WRK-SURYOKETA2
           MOVE    SPACE           TO  WRK-SURYO-N
           MOVE    SPACE           TO  WRK-SRYSURYOW
      *
           IF      TBL-ZAIW-SRYSURYO (WRK-WIDX1 WRK-WIDX2)
                                   =   ZERO
               GO  TO  4301-SURYO-HEN1-EXT  
           END-IF
      * 
      *    数字表示用編集（半角）
           MOVE    TBL-ZAIW-SRYSURYO (WRK-WIDX1 WRK-WIDX2)
                                   TO  WRK-SURYO
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY   FROM    3   BY  -1
                     UNTIL     IDY           =   ZERO
             IF  WRK-SURYO-S2(IDY:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-SURYO-S2(IDY:1)
                                         TO  WRK-SURYO-Z3(IDY:1)
             END-IF
           END-PERFORM
           IF  WRK-SURYO-Z3          NOT =   SPACE
               MOVE    "."               TO  WRK-SURYO-Z2
           END-IF
      *    表示文字数カウント
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL     IDY           >   9
             IF  WRK-SURYO-X (IDY:1)   NOT =   SPACE
                 IF  WRK-SURYOST           =   ZERO
                     MOVE    IDY           TO  WRK-SURYOST
                 END-IF
      *          数量桁数カウント(６桁目：ピリオド)
                 ADD     1                 TO  WRK-SURYOKETA
                 IF  IDY                   <   6
                     ADD     1             TO  WRK-SURYOKETA1
                 END-IF
                 IF  IDY                   >   6
                     ADD     1             TO  WRK-SURYOKETA2
                 END-IF
             END-IF
           END-PERFORM
           IF  WRK-SURYOST             =   6
               MOVE    "0"             TO  WRK-SURYO-Z1(5:1)
               MOVE    5               TO  WRK-SURYOST
               MOVE    1               TO  WRK-SURYOKETA1
               ADD     1               TO  WRK-SURYOKETA
           END-IF
      *    全角変換
           IF  WRK-SURYOKETA1          >   ZERO
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z1 (WRK-SURYOST:WRK-SURYOKETA1)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA1  TO  WRK-CHAR-CNT
               PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N1
           END-IF
           IF  WRK-SURYOKETA2          >   ZERO
               MOVE    "．"            TO  WRK-SURYO-N2
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z3 (1:WRK-SURYOKETA2)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA2  TO  WRK-CHAR-CNT
               PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N3
           END-IF
           STRING      WRK-SURYO-N1    DELIMITED   BY  " "
                       WRK-SURYO-N2    DELIMITED   BY  " "
                       WRK-SURYO-N3    DELIMITED   BY  " "
                       INTO            WRK-SRYSURYOW
           END-STRING
      *
           .
       4301-SURYO-HEN1-EXT.
           EXIT.
      *     
      *****************************************************************
      *    分画数編集処理
      *****************************************************************
       4301-BUNKAKU-HEN1-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-BUNKAKUKETA
           MOVE    SPACE           TO  WRK-BUNKAKU-N
      *
           IF      TBL-ZAIW-SRYCD (WRK-WIDX1 WRK-WIDX2) (1:1)
                                   =   "7"
           AND     TBL-ZAIW-TANICD(WRK-WIDX1 WRK-WIDX2)
                                   =   WRK-CONS-TANICD 
           AND     TBL-ZAIW-SRYKAISU(WRK-WIDX1 WRK-WIDX2)
                                   >   1
               CONTINUE
           ELSE                            
               GO  TO  4301-BUNKAKU-HEN1-EXT  
           END-IF
      * 
      *    表示用編集（半角）
           MOVE    TBL-ZAIW-SRYKAISU (WRK-WIDX1 WRK-WIDX2)
                                   TO  WRK-SRYKAISU-X
      *    全角変換
           INITIALIZE              WRK-CHAR-HENKAN-AREA
           MOVE    WRK-SRYKAISU-X  TO  WRK-CHAR-X
           MOVE    3               TO  WRK-CHAR-CNT
           MOVE    1               TO  WRK-CHAR-FLG
           PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
           STRING  WRK-CHAR-N      DELIMITED BY  SPACE
                   "分画"          DELIMITED  BY  SIZE
                                   INTO  WRK-BUNKAKU-N
           COMPUTE WRK-BUNKAKUKETA =   WRK-CHAR-N-CNT  +  4
      *
           .
       4301-BUNKAKU-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細・明細桁　編集処理
      *****************************************************************
       4302-MEISAI-KETA-HEN1-SEC    SECTION.
      *
           MOVE    SPACE           TO  WRK-KOUI
      *
           IF  TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
                                   =   SPACE
      *        < 名称 >
               MOVE    TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                   TO  WRK-KOUI
      *        < 名称*2 >
               COMPUTE WRK-WCLM  =
                       TBL-ZAIW-YUKOKETA (WRK-WIDX1 WRK-WIDX2) *  2
           ELSE
      *
               IF   TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)(1:2)
                                            =  "＊"
                   MOVE    SPACE   TO  
                              TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
               END-IF
      *        名称の長さ
               COMPUTE   WRK-NAMEKETA  =
                         TBL-ZAIW-YUKOKETA (WRK-WIDX1 WRK-WIDX2)
                         *   2
      *        -----------------------------------
      *        １明細に数量単位まで収まるかどうか
      *        -----------------------------------
               EVALUATE    TRUE
               WHEN    WRK-NAMEKETA    >   50
      *            明細行
                   MOVE    TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                       TO  WRK-KOUI
                   MOVE    WRK-KOUI    TO  WRK-ZAI-NAME
                   PERFORM 4303-ZAITBL-TENSO-SEC
      *
                   MOVE    SPACE       TO  WRK-KOUI
                   MOVE    SPACE       TO  WRK-ZAI-NAME
                   COMPUTE WRK-NAMEKETA    =   WRK-NAMEKETA  -  50
      *   　       < 名称後半 + 空白 + 数量 + 単位 + 分画数>
                   STRING  TBL-ZAIW-NAME     (WRK-WIDX1 WRK-WIDX2)
                                             (51:WRK-NAMEKETA)
                                              DELIMITED  BY  SIZE
                           "  "               DELIMITED  BY  SIZE
                           WRK-SRYSURYOW      DELIMITED  BY  " "
                           TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
                                              DELIMITED  BY  " "
                           "  "               DELIMITED  BY  SIZE
                           WRK-BUNKAKU-N      DELIMITED  BY  " "
                           INTO               WRK-KOUI
                   END-STRING
                   COMPUTE   WRK-WCLM
                         =   ( WRK-SURYOKETA                  *   2 )
                         +   ( TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                                                              *   2 )
                         +   WRK-NAMEKETA  +  2
                   IF      WRK-BUNKAKUKETA        >   ZERO
                       COMPUTE   WRK-WCLM    =   WRK-WCLM         +
                                                 WRK-BUNKAKUKETA  +
                                                 2
                   END-IF                                             
      *
               WHEN    WRK-NAMEKETA    =   50
      *            数量・単位　次行印字処理
                   PERFORM 43021-SURYO-NEXTPRT-SEC
      *
               WHEN    WRK-NAMEKETA    <   50
      *            間の空白分を足し込む
                   COMPUTE   WRK-NAMEKETA  =   WRK-NAMEKETA   +   2
                   IF  WRK-NAMEKETA    <   50
      *                余白
                       COMPUTE   WRK-YOHAKU  =   50  -   WRK-NAMEKETA
      *                数量・分画数桁数
                       COMPUTE   WRK-WKETA   =
                         ( WRK-SURYOKETA                  * 2 )
                             +
                         ( TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                                                          * 2 )
                       IF      WRK-BUNKAKUKETA   >   ZERO
                           COMPUTE   WRK-WKETA   =   WRK-WKETA        +
                                                     WRK-BUNKAKUKETA  +
                                                     2
                       END-IF                                             
      *
                       IF  WRK-YOHAKU        >=  WRK-WKETA
      *                    < 名称 + 空白 + 数量 + 単位 + 空白 + 分画数>
      ********************STRING  TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
      ********************                        DELIMITED  BY  " "
      ********************         "  "           DELIMITED  BY  SIZE
                          STRING  TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                                (1:WRK-NAMEKETA)
                                                  DELIMITED  BY  SIZE
                                   WRK-SRYSURYOW  DELIMITED  BY  " "
                                   TBL-ZAIW-TANINAME
                                                 (WRK-WIDX1 WRK-WIDX2)
                                                  DELIMITED  BY  " "
                                   "  "           DELIMITED  BY  SIZE
                                   WRK-BUNKAKU-N  DELIMITED  BY  " "
                                   INTO           WRK-KOUI
                           END-STRING
      *                    < 名称*2 + 空白*2 + 数量 + 単位*2 + 空白 + 分画数>
                           COMPUTE   WRK-WCLM     =
                               ( TBL-ZAIW-YUKOKETA 
                                         (WRK-WIDX1 WRK-WIDX2)  * 2 )
                             + ( WRK-SURYOKETA                  * 2 )
                             + ( TBL-ZAIW-TANIMOJISU 
                                         (WRK-WIDX1 WRK-WIDX2)  * 2 )
                             +  2
                           IF      WRK-BUNKAKUKETA   >   ZERO
                               COMPUTE   WRK-WCLM   =   WRK-WCLM      +
                                                     WRK-BUNKAKUKETA  +
                                                     2
                           END-IF                                             
                       ELSE
      *                    数量・単位　次行印字処理
                           PERFORM 43021-SURYO-NEXTPRT-SEC
                       END-IF
                   ELSE
      *                数量・単位　次行印字処理
                       PERFORM 43021-SURYO-NEXTPRT-SEC
                   END-IF
               END-EVALUATE
           END-IF
           MOVE    WRK-KOUI        TO  WRK-ZAI-NAME
      *
           .
       4302-MEISAI-KETA-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量・単位　次行編集処理
      *****************************************************************
       43021-SURYO-NEXTPRT-SEC    SECTION.
      *
      *    < 名称 >
           MOVE    TBL-ZAIW-NAME (WRK-WIDX1 WRK-WIDX2)
                                      TO  WRK-KOUI
           MOVE    WRK-KOUI           TO  WRK-ZAI-NAME
           PERFORM 4303-ZAITBL-TENSO-SEC
      *
           MOVE    SPACE              TO  WRK-KOUI
           MOVE    SPACE              TO  WRK-ZAI-NAME
      *    < 空白 + 数量 + 単位 + 空白 + 分画数>
           STRING  "    "             DELIMITED  BY  SIZE
                   WRK-SRYSURYOW      DELIMITED  BY  " "
                   TBL-ZAIW-TANINAME (WRK-WIDX1 WRK-WIDX2)
                                      DELIMITED  BY  " "
                   "  "               DELIMITED  BY  SIZE
                   WRK-BUNKAKU-N      DELIMITED  BY  " "
                   INTO               WRK-KOUI
           END-STRING
           COMPUTE   WRK-WCLM
                 =   ( WRK-SURYOKETA                  *   2 )
                 +   ( TBL-ZAIW-TANIMOJISU (WRK-WIDX1 WRK-WIDX2)
                                                      *   2 )
                 +   4
           IF      WRK-BUNKAKUKETA   >   ZERO
               COMPUTE   WRK-WCLM   =   WRK-WCLM         +
                                        WRK-BUNKAKUKETA  +
                                        2
           END-IF                                             
      *
           .
       43021-SURYO-NEXTPRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤・診療行為テーブル編集処理
      *****************************************************************
       4303-ZAITBL-TENSO-SEC    SECTION.
      *
           ADD     1                 TO  WRK-TBLMAX
           IF  WRK-TBLMAX           >   100
               DISPLAY "*** ORCR0460 TBL-ZAI INDEX[100] OVER  ***"
           END-IF
           MOVE    WRK-ZAI           TO  TBL-ZAI (WRK-TBLMAX)
           MOVE    SPACE             TO  WRK-ZAI
      *
           .
       4303-ZAITBL-TENSO-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数回数セット処理
      *****************************************************************
       440-TNSKAI-SET-SEC          SECTION.
      *
      *---- (01.00.05) LINE ADD START --------------------------------
           IF     (FLG-COMMENT        =   1)     AND
                  (RECE44-SRYKBN      =   "99")
               GO  TO  440-TNSKAI-SET-EXT
           END-IF
           IF     (FLG-COMMENT2       =   1)     AND                    
                  (RECE44-SRYKBN      =   "11" OR "12" OR "99")         
               GO  TO  440-TNSKAI-SET-EXT                               
           END-IF                                                       
           IF      RECE44-SAFLG       =   "1"
               GO  TO  440-TNSKAI-SET-EXT                              
           END-IF  
      *---- (01.00.05) LINE ADD END   --------------------------------
      *         
           MOVE    WRK-TNSKAI-ST     TO  IDA
           COMPUTE   IDA             =   IDA     -   2
           MOVE    WRK-TNSKAI-KETA   TO  IDB
      *****MOVE    WRK-TBLMAX       TO  IDY
      *
      *    最終明細に点数・回数を印字するスペース有り
           MOVE    TBL-ZAI (IDY)(3:)     TO  WRK-ZAI-NAME
           IF  WRK-ZAI-NAME (IDA:)       =   SPACE
               COMPUTE   WRK-TNSKAI-ST   =   22
                                         -   WRK-TNSKAI-KETA
               ADD     1                 TO  WRK-TNSKAI-ST
               MOVE    WRK-TNSKAI (WRK-TNSKAI-ST:IDB)
                                         TO  WRK-ZAI-TNSKAI
                                                    (WRK-TNSKAI-ST:)
               MOVE    WRK-ZAI-NAME      TO  TBL-ZAI (IDY)(3:)
           ELSE
      *    最終明細に点数・回数を印字するスペース無し
               ADD     1                 TO  IDY
               IF  IDY                   >   100
                   DISPLAY "*** ORCR0460 TBL-ZAI INDEX[100] OVER  ***"
               END-IF
               MOVE    SPACE             TO  WRK-ZAI
               MOVE    WRK-TNSKAI        TO  WRK-ZAI-TNSKAI
               MOVE    WRK-ZAI           TO  TBL-ZAI (IDY)
               MOVE    SPACE             TO  WRK-ZAI
           END-IF
      *
           .
       440-TNSKAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０６出力処理
      *****************************************************************
       450-RECE46-OUT-SEC    SECTION.
      *
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL     IDY   >   WRK-TBLMAX
      *
               ADD     1                 TO  WRK-LINENUM
               MOVE    SPACE             TO  RECE46-REC
               INITIALIZE                    RECE46-REC
               MOVE    RECE41-KEY        TO  RECE46-KEY
               MOVE    RECE44-ZAINUM     TO  RECE46-ZAINUM
               MOVE    WRK-LINENUM       TO  RECE46-LINENUM
               MOVE    WRK-TBLMAX        TO  RECE46-ZAIMEISAISU
               MOVE    RECE44-PTNUM      TO  RECE46-PTNUM
               MOVE    RECE44-SRYKA      TO  RECE46-SRYKA
               MOVE    RECE44-SRYKBN     TO  RECE46-SRYKBN
               MOVE    TBL-ZAI (IDY)     TO  RECE46-TEKIYO-INF
      *
               IF    ( FLG-COMMENT2     =   1           )               
               AND   ( RECE44-SRYKBN    =  "11" OR "12" )               
                   MOVE  SPACE           TO  RECE46-SRYKBN              
               END-IF                                     
      *---------適用欄並び替え用の編集（表用番号等）-----------------
               IF      IDY               =   1
                 IF       (TBL-ZAIW-SRYCD1(1:1)  =   "6"  OR  "7")
                 OR       (TBL-ZAIW-SRYCD1(1:3)  =   "059"       )
                     MOVE    ALL "9"            TO
                                            TBL-ZAIW-CDKBN-AREA
                     IF    TBL-ZAIW-SRYCD1(1:1)  =   "6"
                       MOVE    999999998   TO  TBL-ZAIW-KOUHYOJYUNNUM
                     ELSE
                       MOVE    999999999   TO  TBL-ZAIW-KOUHYOJYUNNUM
                     END-IF
                 END-IF
               END-IF
               MOVE    TBL-ZAIW-CDKBN-SYO  TO  RECE46-CDKBN-SYO
               MOVE    TBL-ZAIW-CDKBN-BU   TO  RECE46-CDKBN-BU
               MOVE    TBL-ZAIW-CDKBN-KBN  TO  RECE46-CDKBN-KBN
               MOVE    TBL-ZAIW-CDKBN-KBNNUM
                                           TO  RECE46-CDKBN-KBNNUM
               MOVE    TBL-ZAIW-CDKBN-KBNNUM-EDA
                                           TO  RECE46-CDKBN-KBNNUM-EDA
               MOVE    TBL-ZAIW-CDKBN-KOUBAN
                                           TO  RECE46-CDKBN-KOUBAN
               IF      RECE44-SRYKBN           =   "21"
               AND     TBL-ZAIW-SRYCD1     NOT =   CONS-TEIGEN
                   CONTINUE
               ELSE
                   MOVE    TBL-ZAIW-KOUHYOJYUNNUM
                                               TO  RECE46-KOUHYOJUNNUM
               END-IF
      *
      *--------------------------------------------------------------
               MOVE    RECE44-ZAITEN     TO  RECE46-ZAITEN
      *
               IF      RECE44-SRYKBN     =   "60"
                       MOVE   TBL-ZAIW-KNSJISKBN
                                             TO  RECE46-KNSJISKBN
               ELSE
                       MOVE   ZERO           TO  RECE46-KNSJISKBN
               END-IF
      *
               WRITE   RECE46-REC
               IF  STS-RECE46          NOT =   ZERO
                   DISPLAY "*** ORCR0460 RECE46 WRITE ERR  ***"
                   MOVE   WRK-TBLMAX     TO  IDY
                   MOVE    2             TO  SPA-STS                       
               ELSE
                   ADD     1             TO  CNT-RECE46
               END-IF    
           END-PERFORM
      *     
           INITIALIZE                    TBL-ZAI-AREA
      *
           .
       450-RECE46-OUT-EXT.
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
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           MOVE    2                   TO  SPA-STS
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
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
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
      *            ジョブ管理終了処理
                   MOVE    "CAN"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                   MOVE    WRK-PARA-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "8888"          TO  JOB-ERRCD
                   MOVE    2               TO  JOB-STOPFLG
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
               END-IF
      *                             
               MOVE    1                   TO  FLG-END
               MOVE    2                   TO  SPA-STS
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
      *
           CLOSE                       RECE41-FILE
           CLOSE                       RECE44-FILE
           CLOSE                       RECE45-FILE
           CLOSE                       RECE46-FILE     
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
           DISPLAY "RECE41  IN  " CNT-RECE41   
           DISPLAY "RECE46  CNT " CNT-RECE46 
           DISPLAY "*** ORCR0460 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値半角→全角変換処理
      *****************************************************************
       800-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
      *    FLG=1のとき空白は編集しない
      *
           MOVE    1                   TO  WRK-CHAR-IDX
           MOVE    ZERO                TO  WRK-CHAR-N-CNT
      *     
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL       IDZ     >   WRK-CHAR-CNT
               IF      WRK-CHAR-FLG        =   1
               AND     WRK-CHAR-X (IDZ:1)  =   SPACE
                   CONTINUE
               ELSE        
                   IF    WRK-CHAR-X (IDZ:1)  IS  NUMERIC
                       IF      WRK-CHAR-X (IDZ:1)  =   "0"
                           MOVE    TBL-SUJI (10)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE
                           MOVE    WRK-CHAR-X (IDZ:1)  TO  WRK-NUM
                           MOVE    TBL-SUJI (WRK-NUM)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF                    
                   ELSE
                       MOVE    "  "    TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
                                           WRK-CHAR-N-CNT
               END-IF    
           END-PERFORM
      *
           .
       800-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数・回数編集処理
      *****************************************************************
       810-TENSUKAI-HEN-SEC    SECTION.
      *    点数編集
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-ZAITENX         TO  WRK-CHAR-X
           MOVE    7                   TO  WRK-CHAR-CNT
           PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N          TO  WRK-ZAITEN
      *    回数編集
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-ZAIKAISUX       TO  WRK-CHAR-X
           MOVE    3                   TO  WRK-CHAR-CNT
           PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N          TO  WRK-ZAIKAISU
           MOVE    "×"                TO  WRK-X
      *
      *    薬剤料逓減のとき点数のまえに▲を表示
           IF      FLG-TEIGEN          =   1
               PERFORM VARYING IDZ FROM    13  BY  -2
                       UNTIL   IDZ <       3
                   IF      WRK-ZAITEN (IDZ:2)  =   SPACE
                       MOVE    "▲"        TO  WRK-ZAITEN (IDZ:2)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
           END-IF
      *
      *
      *    必要有効桁数（点数ZZZZZZZ X 数量ZZZ の全角の桁数）
           MOVE    ZERO                TO  WRK-TNSKAI-KETA
           PERFORM VARYING   IDZ     FROM    1   BY  1
                   UNTIL     IDZ     >       22
               IF      WRK-TNSKAI (IDZ:1)      NOT =   SPACE
                   COMPUTE WRK-TNSKAI-KETA   =   22  -  IDZ     +   1
                   MOVE    22                  TO  IDZ
               END-IF 
           END-PERFORM
      *    開始位置を取得
           COMPUTE   WRK-TNSKAI-ST     =   ( 50  -  WRK-TNSKAI-KETA )
                                       +   1
      *
           .
       810-TENSUKAI-HEN-EXT.
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
                   MOVE    2           TO  SPA-STS
               NOT AT  END
                   ADD     1           TO  CNT-RECE41
                   ADD     1           TO  CNT-IN-CANCEL
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
                   ADD     1         TO  CNT-IN-CANCEL
                   IF      RECE44-MEISAISU    =   ZERO
                       GO  TO  990-RECE44-NEXT-SEC
                   END-IF       
           END-READ
      *
           .
       990-RECE44-NEXT-EXT.
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
      *****************************************************************
      *    点数マスター読込（キー）
      *****************************************************************
      * 900-TENSU-INV-SEC         SECTION.
      *
      *     MOVE    "DBSELECT"          TO  MCP-FUNC
      *     MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
      *     IF      MCP-RC              =   ZERO
      *         MOVE    "DBFETCH"           TO  MCP-FUNC
      *         MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
      *         IF      MCP-RC              =   ZERO
      *             MOVE    ZERO                TO  FLG-TENSU
      *             MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
      *         ELSE
      *             INITIALIZE                  TNS-TENSU-REC
      *             MOVE    1                   TO  FLG-TENSU
      *         END-IF
      *     ELSE
      *         INITIALIZE                  TNS-TENSU-REC
      *         MOVE    1                   TO  FLG-TENSU
      *     END-IF
      *
      *     MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *     MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
      *
      *     .
      * 900-TENSU-INV-EXT.
      *     EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
      * TENSU-READ-SEC           SECTION.
      *
      *     MOVE    "DBFETCH"           TO  MCP-FUNC
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
      *
      *     IF      MCP-RC              =   ZERO
      *         MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *         MOVE    ZERO            TO  FLG-TENSU
      *     ELSE
      *         INITIALIZE                  TNS-TENSU-REC
      *         MOVE    1               TO  FLG-TENSU
      *     END-IF
      *
      *     .
      * TENSU-READ-EXT.
      *     EXIT.
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       CSR-CLOSE-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
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
       120-DBOPEN-EXT.
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
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
