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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGR02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト作成−印刷指示（Ｒ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.07.13  県内・県外別に印刷するように修正 
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     01/12/05  レセプト種別が不明のレセプトも
      *                                     印刷できるようにする      
      *  01.02.01    MCC-藤原     02.01.23  帳票出力用パラメタファイル
      *                                     を未使用とする
      *  01.02.02    MCC-藤原     02.01.31  ジョブ管理ＤＢの追加
      *                                     Ｆ８の追加
      *  01.02.03    MCC-藤原     02.02.12  件数の追加
      *  01.02.04    MCC-藤原     02.02.25  不明分の出力方法の変更
      *  01.02.05    MCC-藤原     02/02/26  レセプト管理ファイルを
      *                                     索引ファイルからＤＢへ変更
      *  01.02.06    MCC-藤原     02/02/26  レセ電処理の追加
      *  01.02.07    MCC-藤原     02.03.18  端末ＩＤ
      *                                     X(16)からX(32)に変更
      *  01.02.08    MCC-藤原     02/04/30  印刷順の追加
      *  01.02.09    MCC-藤原     02/06/06  処理結果画面追加
      *  01.02.10    NACL-藤原    02/07/30  情報削除ボタン追加
      *  01.02.11    NACL-藤原    02/08/23  自費を不明分と分けて表示する
      *  01.02.12    NACL-藤原    02/08/29  レセプトが選択されていないとき
      *                                     エラーとする
      *
      *  01.03.01    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.03.02    NACL-藤原    02/09/24  ２００２．１０改正対応
      *  01.03.03    NACL-藤原    02/10/28  印刷順が０３、０４のときの印刷修正
      *                                     修正
      *　入院機能追加　
      *  01.04.01    NACL-藤原    02/12/11  入外区分追加 
      *  01.04.02    NACL-藤原    03/01/08  通し番号のためのパラメタ追加 
      *  01.04.03    NACL-藤原    03/01/15  確定のとき自動的に情報確認画面を表示
      *  01.04.04    NACL-藤原    03/02/25  プリンタ名をオンラインより渡す
      *  01.04.05    NACL-藤原    03/03/03  再印刷機能追加
      *  01.04.06    NACL-藤原    03/05/23  レセ電のボタンを削除
      *  01.04.07    NACL-藤原    03/08/05  ヴで始まるカナ氏名のレセプト
      *                                     を対象とするように修正
      *  01.04.08    NACL-藤原    03/08/05  レセ電のボタンを削除
      *  01.04.09    NACL-藤原    03/08/19  件数印刷の処理を追加
      *  01.04.10    NACL-藤原    03/08/29  出力帳票の指定を追加
      *  01.04.11    NACL-小豆沢  04/04/15  入院レセ出力先プリンタの指定追加
      *　プレビュー機能追加
      *  01.05.01    NACL-藤原    04/04/07  プレビュー機能追加
      *  01.05.02    NACL-藤原    04/06/30  印刷順の追加（個別作成時のみ）
      *  01.05.03    NACL-藤原    04/07/12  指定印刷時の選択項目の色指定
      *  中止処理追加
      *  01.06.01    NACL-藤原    04/09/28  中止ボタンを追加
      *                                     印刷済のチェック表示対応 
      *  01.06.02    NACL-藤原    05/01/13  受診日指定
      *  01.06.03    NACL-藤原    05/01/30  訂正日指定
      *  01.06.03    NACL-藤原    05/01/30  「印刷する」場合だけ印刷済の
      *                                     チェック表示を行う
      *
      *  02.06.01    NACL-藤原    05/07/13  国保の県内県外の印刷順の指定
      *                                     「４」の追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.03    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *  02.07.04    NACL-藤原    06/03/10  患者番号順の範囲指定を変更
      *  02.07.05    NACL-藤原    06/03/14  編綴順情報の表示の修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    帳票出力用パラメタ  
       01  WRK-RECE09-REC. 
           03  WRK-RECE09-KEY.
               05  WRK-RECE09-PRTID        PIC X(04).
               05  WRK-RECE09-SRYYM        PIC X(06).
               05  WRK-RECE09-CREYMD       PIC X(08).
               05  WRK-RECE09-CREHMS       PIC X(06).
               05  WRK-RECE09-PREFKBN      PIC X(01).
               05  WRK-RECE09-TEISYUTUSAKI PIC X(01).
               05  WRK-RECE09-RECESYUBETU  PIC X(04).
               05  WRK-RECE09-NYUGAIKBN    PIC X(01).
           03  WRK-RECE09-PRTJYUN          PIC X(02).
           03  WRK-RECE09-CHOUKI           PIC X(01).
           03  WRK-RECE09-ZAITAKU          PIC X(01).
           03  WRK-RECE09-HKNJANUM         PIC X(08).
       01  WRK-RECE09-STDATA               PIC X(100).
       01  WRK-RECE09-EDDATA               PIC X(100).
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-R02.
           03  SPA-R02-AREA.
               05  SPA-GMN-MAX                     PIC 9(04).
               05  SPA-GMN-PAGE                    PIC 9(02).
               05  SPA-GMN-PAGE-MAX                PIC 9(02).
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SYOKBN              PIC X(01).
                   07  SPA-GMN-FILEKBN             PIC X(42).
                   07  SPA-GMN-CREYMD              PIC X(22).
                   07  SPA-GMN-PAGEH               PIC 9(04).
                   07  SPA-GMN-KENSUH              PIC 9(04).
                   07  SPA-GMN-PRTJYUN-G.
                       09  SPA-GMN-PRTJYUN         PIC X(02).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-PRTJYUNMEI      PIC X(22).
                   07  SPA-GMN-PRTJYUNLST          OCCURS  5.
                       09  SPA-GMN-PRTJYUNL        PIC X(02).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-PRTJYUNMEIL     PIC X(22).
                   07  SPA-GMN-PRTJYUN1-G.
                       09  SPA-GMN-PRTJYUN1        PIC X(02).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-PRTJYUN1MEI     PIC X(22).
                   07  SPA-GMN-PRTJYUN1LST         OCCURS  5.
                       09  SPA-GMN-PRTJYUN1L       PIC X(02).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-PRTJYUN1MEIL    PIC X(22).
      *----(01.00.01) LINE ADD START --------------------------------
                   07  SPA-GMN-PREFKBN-G.
                       09  SPA-GMN-PREFKBN         PIC X(01).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-PREFKBNMEI      PIC X(18).
                   07  SPA-GMN-PREFKBNLST          OCCURS  4.
                       09  SPA-GMN-PREFKBNL        PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-PREFKBNMEIL     PIC X(18).
      *----(01.00.01) LINE ADD END   --------------------------------
                   07  SPA-GMN-PRTKBN-G.
                       09  SPA-GMN-PRTKBN          PIC X(01).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-PRTKBNMEI       PIC X(12).
                   07  SPA-GMN-PRTKBNLST           OCCURS  3.
                       09  SPA-GMN-PRTKBNL         PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-PRTKBNMEIL      PIC X(12).
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBL-OCC         OCCURS   30.
                           11  SPA-GMN-TBL-G       OCCURS   36.
                               13  SPA-GMN-TSTDATA PIC X(100).
                               13  SPA-GMN-TEDDATA PIC X(100).
                               13  SPA-GMN-TCHK.
                                   15  SPA-GMN-TCHK-VALUE
                                                   PIC X(01).
      *******                      15  SPA-GMN-TCHK-STYLE
      *******                                      PIC X(20).
                               13  SPA-GMN-TPRTFLG PIC X(01).
                   07  SPA-GMN-TBL1.
                       09  SPA-GMN-TBL-OCC1        OCCURS   30.
                           11  SPA-GMN-TBL-G1      OCCURS   36.
                               13  SPA-GMN-TPAGE   PIC 9(04).
                               13  SPA-GMN-TKENSU  PIC 9(04).
                               13  SPA-GMN-TSYURUINAME
                                                   PIC X(40).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-WIDGET              PIC X(08). 
                   07  SPA-NAI-2005-SYAPRTNM       PIC X(16).
                   07  SPA-NAI-2005-KKHPRTNM       PIC X(16).
                   07  SPA-NAI-2005-SYAPRTNM-NEXT  PIC X(16).
                   07  SPA-NAI-2005-KKHPRTNM-NEXT  PIC X(16).
                   07  SPA-NAI-2005-SYAPRTNM-N     PIC X(16).
                   07  SPA-NAI-2005-KKHPRTNM-N     PIC X(16).
                   07  SPA-NAI-2005-SYAPRTNM-NEXT-N  PIC X(16).
                   07  SPA-NAI-2005-KKHPRTNM-NEXT-N  PIC X(16).
      *
                   07  SPA-NAI-2008-ZAITAKU        PIC  X(01).
                   07  SPA-NAI-2008-CHOUKI         PIC  X(01).
                   07  SPA-NAI-2008-PREFKBN-PRTJYUN
                                                   PIC  X(01).
                   07  SPA-NAI-2008-HKNJANUM-PRTJYUN
                                                   PIC  X(01).
                   07  SPA-NAI-2008-RECESYUBETU-PRTJYUN
                                                   PIC  X(01).
                   07  SPA-NAI-2008-HKNJANUM       PIC  X(08).
                   07  SPA-NAI-2008-HKNJANUM-PRTKBN
                                                   PIC  X(01).
                   07  SPA-NAI-2008-HKNJANAME      PIC  X(50).
      *
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBL-OCC         OCCURS  30.
                           11  SPA-NAI-TBL-G       OCCURS  36.
                               13  SPA-NAI-PRTJYUNI
                                                   PIC X(02).
                               13  SPA-NAI-TEISYUTUSAKI
                                                   PIC X(01).
                               13  SPA-NAI-THKNJANUM
                                                   PIC X(08).
                               13  SPA-NAI-THKNJANUM-ERR
                                                   PIC X(01).
                               13  SPA-NAI-TPREFKBN
                                                   PIC X(01).
                               13  SPA-NAI-TPAGE-CHOUKI
                                                   PIC 9(04).
                               13  SPA-NAI-TPAGE-ZAITAKU
                                                   PIC 9(04).
                               13  SPA-NAI-SYUBETU-G.
                                   15  SPA-NAI-SYUBETU
                                                   PIC X(04)
                                                   OCCURS 10.
      *    プレビュー用情報
           03  SPA-LNK-XC01-KEY.
               05  SPA-LNK-XC01-TBL-KEY    PIC X(8).
               05  SPA-LNK-XC01-RENNUM     PIC 9(4).
               05  SPA-LNK-XC01-TBL-GROUP  PIC X(14).
               05  SPA-LNK-XC01-SHORI-RENNUM
                                          PIC 9(4).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *
      *
           03  STS-RECEERR         PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(03).
               05  WRK-WIDGET2     PIC 9(02).
               05  FILLER          PIC X(01).
               05  WRK-WIDGET3     PIC 9(02). 
           03  WRK-RIDMSG          PIC X(70).
           03  WRK-Z9              PIC ZZZ9.
           03  WRK-YM              PIC X(07). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-RECEKBN         PIC X(06).
           03  WRK-NYUGAIKBN       PIC X(06).
           03  WRK-PREFKBN         PIC X(01).
           03  WRK-TEISYUTUSAKI    PIC X(01).
           03  WRK-CHOUKI          PIC X(01).
           03  WRK-ZAITAKU         PIC X(01).
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-PAGE            PIC ZZZZ9.
           03  WRK-STYMD           PIC X(22).
           03  WRK-EDYMD           PIC X(22).
           03  WRK-PRTJYUN         PIC X(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *     
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC  X(06).
             05  WRK-SHELL-TERMID  PIC  X(16).
             05  WRK-SHELL-SYSYMD  PIC  X(08).  
      *
           03  WRK-RECE08-PRTJYUN  PIC 9(02).
      *
           03  WRK-KBNCD.
               05  WRK-KBNCD-9     PIC 9(08).
      *      
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG1        PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *                             
       01  WRK-SHELL-NAME.
           05  FILLER             PIC X(16) VALUE
                                             "scripts/monthly/".
           05  WRK-SHELL-SHELL    PIC X(20).         
      *
       01  WRK-SHELL-NAME1.
           05  FILLER             PIC X(16) VALUE
                                             "scripts/allways/".
           05  WRK-SHELL-SHELL1   PIC X(20).         
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".             
      *
       01  WRK-CONS-AREA.
      *    １頁の最大行数
           03  WRK-CONS-LINE-MAX   PIC 9(02)   VALUE   36.   
      *    １頁の最大行数
           03  WRK-CONS-PAGE-MAX   PIC 9(02)   VALUE   30.   
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT2".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPT2".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK2001.INC".
      *
           COPY    "CPSK2002.INC".
      *
      *    レセプト作成指示（印刷順）情報
           COPY    "CPSK2003.INC".
      *    レセプト作成指示（レセプト種別）情報
           COPY    "CPSK2004.INC".
      *
           COPY    "CPSK2005.INC".
      *
           COPY    "CPSK2008.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
           COPY    "CPRCF008.INC".
      *
      *    保険者番号
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "R01.INC".
           COPY    "R02.INC".
           COPY    "R03.INC".
           COPY    "R04.INC".
           COPY    "R05.INC".
           COPY    "R98.INC".
           COPY    "R99.INC".
           COPY    "RERR.INC".
           COPY    "RID1.INC".
           COPY    "RID2.INC".
           COPY    "RID3.INC".
      *
       PROCEDURE                   DIVISION    USING
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R02
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
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
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "RERR"
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
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "R02    "           TO  MCP-WINDOW.
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
           MOVE    1               TO  SPA-GMN-CUR
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XB01"  OR  "XC01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    SPA-WORK        TO  SPA-R01KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-R02
                   GO  TO  300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "RID1"
                   PERFORM 330-RID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "RID3"
                   PERFORM 330-RID3-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R99"
                   PERFORM 340-R99-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R05"
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
      *****MOVE    1               TO  SPA-GMN-PAGE
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
           INITIALIZE              SPA-R02-AREA
      *
      *    処理区分
           MOVE    "T"             TO  SPA-GMN-SYOKBN
      *
      *　　ファイル作成区分
           EVALUATE    SPA-R01-FILEKBN
               WHEN    "1"
                   MOVE    SPA-R01-SRYYM       TO  WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2) 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   EVALUATE    SPA-R01-RECE07-RECEKBN
                       WHEN    "0"
                           MOVE    SPACE           TO  WRK-RECEKBN
                       WHEN    "1"
                           MOVE    "・社保"        TO  WRK-RECEKBN
                       WHEN    "2"
                           MOVE    "・国保"        TO  WRK-RECEKBN
                   END-EVALUATE  
                   EVALUATE    SPA-R01-RECE07-NYUGAIKBN
                       WHEN    "1"
                           MOVE    "入院"        TO  WRK-NYUGAIKBN
                       WHEN    "2"
                           MOVE    "入院外"      TO  WRK-NYUGAIKBN
                   END-EVALUATE  
                   STRING  WRK-HENYMDG3(1:16) DELIMITED  BY  SIZE
                           WRK-NYUGAIKBN      DELIMITED  BY  SPACE 
                           WRK-RECEKBN        DELIMITED  BY  SPACE 
                           "一括作成分"       DELIMITED  BY  SIZE
                           INTO    SPA-GMN-FILEKBN
                   END-STRING
               WHEN    "2"
                   EVALUATE    SPA-R01-RECE07-NYUGAIKBN
                       WHEN    "1"
                           MOVE    "入院"        TO  WRK-NYUGAIKBN
                       WHEN    "2"
                           MOVE    "入院外"      TO  WRK-NYUGAIKBN
                   END-EVALUATE  
                   EVALUATE    SPA-R01-RECE07-SYORISBT
                       WHEN    ZERO
                       WHEN    SPACE
                           STRING  WRK-NYUGAIKBN  DELIMITED  BY  SPACE 
                                   "個別作成分"   DELIMITED  BY  SIZE
                                   INTO    SPA-GMN-FILEKBN
                           END-STRING
                       WHEN    "1"
                           MOVE    SPA-R01-RECE07-STYMD
                                                      TO  WRK-SYMD
                           MOVE    "01"               TO  WRK-SYMD (7:)
                           PERFORM 5002-HIZUKE-HEN-SEC
                           STRING  "訂正日指定　" DELIMITED  BY SIZE
                                   WRK-HENYMDG3 (1:16)
                                                  DELIMITED  BY SIZE
                                   "分"           DELIMITED  BY SIZE
                                   INTO    SPA-GMN-FILEKBN
                           END-STRING
                       WHEN    "2"
                           MOVE    SPA-R01-RECE07-STYMD
                                                      TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    WRK-HENYMDG3       TO  WRK-STYMD
                           MOVE    SPA-R01-RECE07-EDYMD
                                                      TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    WRK-HENYMDG3       TO  WRK-EDYMD
                           STRING  "受診日指定　" DELIMITED  BY SIZE
                                   WRK-STYMD      DELIMITED  BY SIZE
                                   "〜"           DELIMITED  BY SIZE
                                   WRK-EDYMD (17:6)
                                                  DELIMITED  BY SIZE
                                   INTO    SPA-GMN-FILEKBN
                           END-STRING
                   END-EVALUATE
           END-EVALUATE
      *　　ファイル作成日
           IF      SPA-R01-RECE07-CREYMD   NOT =   SPACE
               MOVE    SPA-R01-RECE07-CREYMD
                                           TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG1        TO  SPA-GMN-CREYMD
               MOVE    SPA-R01-RECE07-CREHMS(1:2)
                                           TO  SPA-GMN-CREYMD (13:2)
               MOVE    ":"                 TO  SPA-GMN-CREYMD (15:1)
               MOVE    SPA-R01-RECE07-CREHMS(3:2)
                                           TO  SPA-GMN-CREYMD (16:2)
               MOVE    ":"                 TO  SPA-GMN-CREYMD (18:1)
               MOVE    SPA-R01-RECE07-CREHMS(5:2)
                                           TO  SPA-GMN-CREYMD (19:2)
           END-IF
      *    印刷順（社保）
           MOVE    "01"                  TO  SPA-GMN-PRTJYUNL    (1)
           MOVE    "保険者番号・カナ氏名順"
                                         TO  SPA-GMN-PRTJYUNMEIL (1)
           MOVE    "02"                  TO  SPA-GMN-PRTJYUNL    (2)
           MOVE    "保険者番号・患者番号順"
                                         TO  SPA-GMN-PRTJYUNMEIL (2)
           MOVE    "03"                  TO  SPA-GMN-PRTJYUNL    (3)
           MOVE    "カナ氏名順"          TO  SPA-GMN-PRTJYUNMEIL (3)
           MOVE    "04"                  TO  SPA-GMN-PRTJYUNL    (4)
           MOVE    "患者番号順"          TO  SPA-GMN-PRTJYUNMEIL (4)
           IF    ( SPA-R01-FILEKBN       =   "2"  )
           AND   ( SPA-R01-RECE07-SYORISBT
                                         =   ZERO )
               MOVE    "05"                  TO  SPA-GMN-PRTJYUNL    (5)
               MOVE    "入力順"              TO  SPA-GMN-PRTJYUNMEIL (5)
           END-IF
      *
           MOVE    SPA-GMN-PRTJYUNLST(01)
                                         TO  SPA-GMN-PRTJYUN-G
      *
      *        印刷順（国保）一括作成時のみ
           IF      SPA-R01-FILEKBN       =   "1"
               MOVE    "01"              TO  SPA-GMN-PRTJYUN1L    (1)
               MOVE    "保険者番号・カナ氏名順"
                                         TO  SPA-GMN-PRTJYUN1MEIL (1)
               MOVE    "02"              TO  SPA-GMN-PRTJYUN1L    (2)
               MOVE    "保険者番号・患者番号順"
                                         TO  SPA-GMN-PRTJYUN1MEIL (2)
               MOVE    "03"              TO  SPA-GMN-PRTJYUN1L    (3)
               MOVE    "カナ氏名順"      TO  SPA-GMN-PRTJYUN1MEIL (3)
               MOVE    "04"              TO  SPA-GMN-PRTJYUN1L    (4)
               MOVE    "患者番号順"      TO  SPA-GMN-PRTJYUN1MEIL (4)
               IF    ( SPA-R01-RECEKBN   =   ZERO    OR  "2" )
               AND   ( SPA-R01-RECE07-HSEQNUM
                                     NOT =   ZERO            )
               AND   ( SPA-R01-RECE07-HKNJANUM
                                     NOT =   "99999999"      )
                   MOVE    "06"              TO  SPA-GMN-PRTJYUN1L   (5)
                   MOVE    "編綴順"          TO  SPA-GMN-PRTJYUN1MEIL(5)
               END-IF
      *
               MOVE    SPA-GMN-PRTJYUN1LST(01)
                                         TO  SPA-GMN-PRTJYUN1-G
           ELSE
               MOVE    SPA-GMN-PRTJYUNLST(01)
                                         TO  SPA-GMN-PRTJYUN1-G
           END-IF
      *    県内・県外
           MOVE    "1"                   TO  SPA-GMN-PREFKBNL    (1)
           MOVE    "全部（県内分先頭）"  TO  SPA-GMN-PREFKBNMEIL (1)
           MOVE    "2"                   TO  SPA-GMN-PREFKBNL    (2)
           MOVE    "県内"                TO  SPA-GMN-PREFKBNMEIL (2)
           MOVE    "3"                   TO  SPA-GMN-PREFKBNL    (3)
           MOVE    "県外"                TO  SPA-GMN-PREFKBNMEIL (3)
           MOVE    "4"                   TO  SPA-GMN-PREFKBNL    (4)
           MOVE    "全部（順不同）"      TO  SPA-GMN-PREFKBNMEIL (4)
      *
           MOVE    SPA-GMN-PREFKBNLST(01)
                                         TO  SPA-GMN-PREFKBN-G
      *    出力内容
           MOVE    "1"                   TO  SPA-GMN-PRTKBNL    (1)
           MOVE    "標準"                TO  SPA-GMN-PRTKBNMEIL (1)
           MOVE    "2"                   TO  SPA-GMN-PRTKBNL    (2)
           MOVE    "一号用紙のみ"        TO  SPA-GMN-PRTKBNMEIL (2)
           MOVE    "3"                   TO  SPA-GMN-PRTKBNL    (3)
           MOVE    "続紙のみ"            TO  SPA-GMN-PRTKBNMEIL (3)
      *
           MOVE    SPA-GMN-PRTKBNLST(01) TO  SPA-GMN-PRTKBN-G
      *         
      *    レセプト編綴順情報
           IF    ( SPA-R01-FILEKBN       =   "1"             )
           AND   ( SPA-R01-RECEKBN       =   ZERO    OR  "2" )
           AND   ( SPA-R01-RECE07-HKNJANUM
                                     NOT =   "99999999"      )
           AND   ( SPA-R01-RECE07-HSEQNUM
                                         >   ZERO            )
               MOVE    SPACE               TO  SYS-2008-REC
               INITIALIZE                      SYS-2008-REC
               MOVE    "2008"              TO  SYS-2008-KANRICD
               MOVE    SPA-R01-RECE07-HSEQNUM
                                           TO  WRK-KBNCD-9
               MOVE    WRK-KBNCD           TO  SYS-2008-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-2008-STYUKYMD
                                               SYS-2008-EDYUKYMD
               MOVE    SYS-2008-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI     =   ZERO
                   MOVE    MCPDATA-REC          TO  SYS-2008-REC
                   MOVE    SYS-2008-ZAITAKU     TO  SPA-NAI-2008-ZAITAKU
                   MOVE    SYS-2008-CHOUKI      TO  SPA-NAI-2008-CHOUKI
                   MOVE    SYS-2008-PREFKBN-PRTJYUN
                                                TO
                                        SPA-NAI-2008-PREFKBN-PRTJYUN
                   MOVE    SYS-2008-HKNJANUM-PRTJYUN
                                                TO
                                       SPA-NAI-2008-HKNJANUM-PRTJYUN
                   MOVE    SYS-2008-RECESYUBETU-PRTJYUN
                                                TO
                                       SPA-NAI-2008-RECESYUBETU-PRTJYUN
                   MOVE    SYS-2008-HKNJANUM-PRTKBN
                                                TO
                                       SPA-NAI-2008-HKNJANUM-PRTKBN
                   MOVE    SYS-2008-HKNJANUM    TO
                                       SPA-NAI-2008-HKNJANUM
                   MOVE    SPACE                TO  HKNJAINF-REC
                   INITIALIZE                       HKNJAINF-REC
                   MOVE    SPA-HOSPID           TO  HKNJA-HOSPID
                   MOVE    SPA-NAI-2008-HKNJANUM
                                                TO  HKNJA-HKNJANUM
                   MOVE    HKNJAINF-REC         TO  MCPDATA-REC
                   PERFORM 900-HKNJAINF-READ-SEC
                   MOVE    HKNJA-HKNJANAME      TO
                                                SPA-NAI-2008-HKNJANAME
               ELSE    
                   IF      SPA-GMN-PRTJYUN1L   (5)  =   "06"
                       MOVE    SPACE        TO  SPA-GMN-PRTJYUN1LST(5)
                   END-IF
               END-IF  
           END-IF
      *     
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYS-2005-REC
           INITIALIZE                   SYS-2005-REC
           MOVE    "2005"           TO  SYS-2005-KANRICD
           MOVE    "*"              TO  SYS-2005-KBNCD
           MOVE    SPA-SYSYMD       TO  SYS-2005-STYUKYMD
                                        SYS-2005-EDYUKYMD
           MOVE    SYS-2005-REC     TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
               INITIALIZE                   SYS-2005-REC
           END-IF    
           MOVE    SYS-2005-SYAPRTNM    TO  SPA-NAI-2005-SYAPRTNM
           MOVE    SYS-2005-KKHPRTNM    TO  SPA-NAI-2005-KKHPRTNM
           MOVE    SYS-2005-SYAPRTNM-NEXT
                                        TO  SPA-NAI-2005-SYAPRTNM-NEXT
           MOVE    SYS-2005-KKHPRTNM-NEXT
                                        TO  SPA-NAI-2005-KKHPRTNM-NEXT
           IF      SYS-2005-SYAPRTNM-N  =   SPACE
                   MOVE   SYS-2005-SYAPRTNM
                                        TO  SYS-2005-SYAPRTNM-N
           END-IF
           IF      SYS-2005-KKHPRTNM-N  =   SPACE
                   MOVE   SYS-2005-KKHPRTNM
                                        TO  SYS-2005-KKHPRTNM-N
           END-IF
           IF      SYS-2005-SYAPRTNM-NEXT-N  =   SPACE
                   MOVE   SYS-2005-SYAPRTNM-NEXT
                                        TO  SYS-2005-SYAPRTNM-NEXT-N
           END-IF
           IF      SYS-2005-KKHPRTNM-NEXT-N  =   SPACE
                   MOVE   SYS-2005-KKHPRTNM-NEXT
                                        TO  SYS-2005-KKHPRTNM-NEXT-N
           END-IF
           IF      SYS-2005-PRTJYUNKBN1 =   SPACE
                   MOVE   "01"          TO  SYS-2005-PRTJYUNKBN1
           END-IF
           IF      SYS-2005-PRTJYUNKBN2 =   SPACE
                   MOVE   "01"          TO  SYS-2005-PRTJYUNKBN2
           END-IF
           IF      SYS-2005-PRTJYUNKBN3 =   SPACE
                   MOVE    SYS-2005-PRTJYUNKBN1
                                        TO  SYS-2005-PRTJYUNKBN3
           END-IF
           MOVE    SYS-2005-SYAPRTNM-N  TO  SPA-NAI-2005-SYAPRTNM-N
           MOVE    SYS-2005-KKHPRTNM-N  TO  SPA-NAI-2005-KKHPRTNM-N
           MOVE    SYS-2005-SYAPRTNM-NEXT-N
                                       TO SPA-NAI-2005-SYAPRTNM-NEXT-N
           MOVE    SYS-2005-KKHPRTNM-NEXT-N
                                       TO SPA-NAI-2005-KKHPRTNM-NEXT-N
      *    
      *    出力順（一括作成）
           IF      SPA-R01-FILEKBN       =   "1" 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      SPA-GMN-PRTJYUNL (IDX)
                                   =   SPACE
                   IF      SYS-2005-PRTJYUNKBN1
                                           =   SPA-GMN-PRTJYUNL (IDX)
                       MOVE    SPA-GMN-PRTJYUNLST(IDX)
                                               TO  SPA-GMN-PRTJYUN-G
                       MOVE    5               TO  IDX
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      SPA-GMN-PRTJYUN1L (IDX)
                                   =   SPACE
                   IF      SYS-2005-PRTJYUNKBN3
                                           =   SPA-GMN-PRTJYUN1L (IDX)
                       MOVE    SPA-GMN-PRTJYUN1LST(IDX)
                                               TO  SPA-GMN-PRTJYUN1-G
                       MOVE    5               TO  IDX
                   END-IF
               END-PERFORM
           ELSE
               IF    ( SPA-R01-RECE07-SYORISBT =   "1" OR  "2" )
               AND   ( SYS-2005-PRTJYUNKBN2    =   "05"        )
                   MOVE    SYS-2005-PRTJYUNKBN1
                                           TO  SYS-2005-PRTJYUNKBN2
               END-IF
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      SPA-GMN-PRTJYUNL (IDX)
                                   =   SPACE
                   IF      SYS-2005-PRTJYUNKBN2
                                           =   SPA-GMN-PRTJYUNL (IDX)
                       MOVE    SPA-GMN-PRTJYUNLST(IDX)
                                               TO  SPA-GMN-PRTJYUN-G
                                                   SPA-GMN-PRTJYUN1-G
                       MOVE    5               TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                  SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    "*"                 TO  SYS-1031-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1031-STYUKYMD
                                           SYS-1031-EDYUKYMD
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
           ELSE    
               INITIALIZE                  SYS-1031-REC
           END-IF                                     
      *    社保印刷先
           IF      SYS-2005-SYAPRTNM   =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-SYAPRTNM
           END-IF                                              
           IF      SYS-2005-SYAPRTNM-N =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-SYAPRTNM-N
           END-IF                                              
      *    国保印刷先
           IF      SYS-2005-KKHPRTNM   =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-KKHPRTNM
           END-IF                                              
           IF      SYS-2005-KKHPRTNM-N =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-KKHPRTNM-N
           END-IF                                              
      *    社保印刷先（続紙）
           IF      SYS-2005-SYAPRTNM-NEXT  =   SPACE
               IF      SYS-2005-SYAPRTNM       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-SYAPRTNM-NEXT
               ELSE                                              
                   MOVE    SYS-2005-SYAPRTNM   TO
                                           SPA-NAI-2005-SYAPRTNM-NEXT
               END-IF
           END-IF                                              
           IF      SYS-2005-SYAPRTNM-NEXT-N  =   SPACE
               IF      SYS-2005-SYAPRTNM-N       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-SYAPRTNM-NEXT-N
               ELSE                                              
                   MOVE    SYS-2005-SYAPRTNM-N   TO
                                           SPA-NAI-2005-SYAPRTNM-NEXT-N
               END-IF
           END-IF                                              
      *    国保印刷先（続紙）
           IF      SYS-2005-KKHPRTNM-NEXT  =   SPACE
               IF      SYS-2005-KKHPRTNM       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-KKHPRTNM-NEXT
               ELSE                                              
                   MOVE    SYS-2005-KKHPRTNM   TO
                                           SPA-NAI-2005-KKHPRTNM-NEXT
               END-IF
           END-IF                                              
           IF      SYS-2005-KKHPRTNM-NEXT-N  =   SPACE
               IF      SYS-2005-KKHPRTNM-N       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-KKHPRTNM-NEXT-N
               ELSE                                              
                   MOVE    SYS-2005-KKHPRTNM-N  TO
                                           SPA-NAI-2005-KKHPRTNM-NEXT-N
               END-IF
           END-IF                                              
      *
      *    頁・件数の編集処理
           PERFORM 3101-HENSYU-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理
      *****************************************************************
       3101-HENSYU-SEC                 SECTION.
      *
           MOVE    SPA-GMN-PREFKBNLST (1)
                                       TO  SPA-GMN-PREFKBN-G
           MOVE    SPA-GMN-PRTKBNLST  (1)
                                       TO  SPA-GMN-PRTKBN-G
           MOVE    "T"                 TO  SPA-GMN-SYOKBN
      *
           INITIALIZE                  SPA-GMN-TBL
                                       SPA-GMN-TBL1
                                       SPA-NAI-TBL   
           MOVE    ZERO                TO  SPA-GMN-PAGEH
                                           SPA-GMN-KENSUH     
                                           SPA-GMN-PAGE-MAX                          
      *
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       WRK-CONS-PAGE-MAX
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-LINE-MAX
                   MOVE    "F"         TO  SPA-GMN-TCHK-VALUE (IDW IDX)
               END-PERFORM
           END-PERFORM
      *    
      *    社保分
           MOVE    "1"                 TO  WRK-TEISYUTUSAKI
           EVALUATE    SPA-GMN-PRTJYUN
               WHEN    "01"
               WHEN    "02"
               WHEN    "05"
                   PERFORM 3101-PAGE-HENSYU-01-SEC     
               WHEN    OTHER
                   PERFORM 3101-PAGE-HENSYU-02-SEC     
           END-EVALUATE
      *    
      *    国保分
           MOVE    "2"                 TO  WRK-TEISYUTUSAKI
           EVALUATE    SPA-GMN-PRTJYUN1
               WHEN    "01"
               WHEN    "02"
               WHEN    "05"
                   PERFORM 3101-PAGE-HENSYU-01-SEC     
               WHEN    "06"
                   PERFORM 3101-PAGE-HENSYU-03-SEC     
               WHEN    OTHER
                   PERFORM 3101-PAGE-HENSYU-02-SEC     
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-PRTJYUN ALSO    SPA-GMN-PRTJYUN1
               WHEN    ANY             ALSO    "06"
                   CONTINUE
               WHEN    "01"            ALSO    ANY
               WHEN    "02"            ALSO    ANY
               WHEN    "05"            ALSO    ANY
                   MOVE    2                   TO  SPA-GMN-PAGE-MAX
               WHEN    "03"            ALSO    "01"
               WHEN    "03"            ALSO    "02"
               WHEN    "04"            ALSO    "01"
               WHEN    "04"            ALSO    "02"
                   MOVE    2                   TO  SPA-GMN-PAGE-MAX
               WHEN    "03"            ALSO    ANY
               WHEN    "04"            ALSO    ANY
                   MOVE    1                   TO  SPA-GMN-PAGE-MAX
           END-EVALUATE
      *    
      *    特別療養費・自費・不明分の編集     
           PERFORM 31011-PAGE-HENSYU-SEC
      *
           MOVE    199             TO  SPA-GMN-CUR
      *
           MOVE    1               TO  SPA-GMN-PAGE     
           .
       3101-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（レセプト種別）
      *****************************************************************
       3101-PAGE-HENSYU-01-SEC            SECTION.
      *
      *    ページ数・件数設定
           EVALUATE    WRK-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    1               TO  IDW
               WHEN    "2"
                   MOVE    2               TO  IDW
           END-EVALUATE
      *
      *    レセプト作成指示（印刷順）読み込み
           MOVE    SPACE               TO  SYS-2003-KEY
           MOVE    "2003"              TO  SYS-2003-KANRICD
           MOVE    SPA-R01-RECE07-SRYYM
                                       TO  SYS-2003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2003-STYUKYMD (7:2)
           MOVE    SYS-2003-STYUKYMD   TO  SYS-2003-EDYUKYMD
      * 
           MOVE    SYS-2003-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-2003-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *                                           
           PERFORM    UNTIL   FLG-SYSKANRI     =   1
               MOVE    MCPDATA-REC   TO  SYS-2003-REC
               IF    ( SYS-2003-TEISYUTUSAKI   =   WRK-TEISYUTUSAKI ) 
               AND   ( SYS-2003-NYUGAIKBN      =
                                           SPA-R01-RECE07-NYUGAIKBN )
                   MOVE    SYS-2003-DSPSEQ         TO  IDX                            
                   MOVE    SYS-2003-DSPSEQ (2:2)   TO                         
                                           SPA-NAI-PRTJYUNI    (IDW IDX)
                   MOVE    SYS-2003-TEISYUTUSAKI   TO
                                           SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    SYS-2003-SYUBETU-G      TO
                                           SPA-NAI-SYUBETU-G   (IDW IDX)
                   MOVE    SYS-2003-SYURUINAME     TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
                   MOVE    "T"         TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
               END-IF    
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME     
           PERFORM 900-CLOSE-SEC
      * 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-LINE-MAX
                   OR      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
               MOVE    ZERO                TO  SPA-GMN-TPAGE  (IDW IDX)
               MOVE    ZERO                TO  SPA-GMN-TKENSU (IDW IDX)
      *
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      SPA-NAI-SYUBETU (IDW IDX IDY)   =   SPACE 
                   INITIALIZE                  RECE07-REC
                   MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
                   MOVE    SPA-R01-RECE07-NYUGAIKBN
                                               TO  RECE07-NYUGAIKBN
                   MOVE    SPA-NAI-TEISYUTUSAKI (IDW IDX)
                                               TO  RECE07-TEISYUTUSAKI
                   MOVE    SPA-NAI-SYUBETU      (IDW IDX IDY)
                                               TO  RECE07-RECESYUBETU
                   MOVE    RECE07-REC          TO  MCPDATA-REC                             
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key9"              TO  WRK-MCP-PATHNAME
                   PERFORM 900-RECE07-READ-SEC
                   IF      FLG-RECE07          =   ZERO
                       ADD     RECE07-PAGE     TO
                                               SPA-GMN-TPAGE (IDW IDX)
                                               SPA-GMN-PAGEH
                       ADD     RECE07-KENSU    TO
                                               SPA-GMN-TKENSU(IDW IDX)
                                               SPA-GMN-KENSUH
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       3101-PAGE-HENSYU-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（社保・国保別）
      *****************************************************************
       3101-PAGE-HENSYU-02-SEC            SECTION.
      *
      *    表示ページ・行の設定
           EVALUATE    WRK-TEISYUTUSAKI    ALSO    SPA-GMN-PRTJYUN
               WHEN    "1"                 ALSO    ANY
                   MOVE    1               TO  IDW
                   MOVE    1               TO  IDX 
               WHEN    "2"                 ALSO    "01"
               WHEN    "2"                 ALSO    "02"
               WHEN    "2"                 ALSO    "05"
                   MOVE    2               TO  IDW
                   MOVE    1               TO  IDX 
               WHEN    "2"                 ALSO    "03"
               WHEN    "2"                 ALSO    "04"
                   MOVE    1               TO  IDW
                   MOVE    2               TO  IDX 
           END-EVALUATE
      *
           EVALUATE    WRK-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "01"        TO  SPA-NAI-PRTJYUNI    (IDW IDX)
                   MOVE    "1"         TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    "9999"      TO  SPA-NAI-SYUBETU
                                                           (IDW IDX 1)
                   MOVE    "社保"      TO  SPA-GMN-TSYURUINAME (IDW IDX)
                   MOVE    "T"         TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
               WHEN    "2"
                   MOVE    "02"        TO  SPA-NAI-PRTJYUNI    (IDW IDX)
                   MOVE    "2"         TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    "9999"      TO  SPA-NAI-SYUBETU
                                                           (IDW IDX 1)
                   MOVE    "国保"      TO  SPA-GMN-TSYURUINAME (IDW IDX)
                   MOVE    "T"         TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
           END-EVALUATE
      *
      *    ページ数・件数設定
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    SPA-NAI-TEISYUTUSAKI(IDW IDX)
                                       TO  RECE07-TEISYUTUSAKI
           MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               IF      RECE07-RECESYUBETU  =   "6666"
                   CONTINUE
               ELSE
                   ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (IDW IDX)
                                               SPA-GMN-PAGEH
                   ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(IDW IDX)
                                               SPA-GMN-KENSUH
               END-IF
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       3101-PAGE-HENSYU-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（編綴順）
      *****************************************************************
       3101-PAGE-HENSYU-03-SEC            SECTION.
      *
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "2"                 TO  RECE07-TEISYUTUSAKI
           MOVE    SPA-R01-RECE07-NYUGAIKBN   
                                       TO  RECE07-NYUGAIKBN
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  IDW
           PERFORM         UNTIL   FLG-RECE07  =   1
                           OR      IDW         >=  WRK-CONS-PAGE-MAX
               ADD     1                   TO  IDW
               MOVE    ZERO                TO  IDX
               PERFORM         UNTIL   FLG-RECE07  =   1
                               OR      IDX     >=  WRK-CONS-LINE-MAX
                   ADD     1               TO  IDX
                   MOVE    RECE07-HKNJANUM TO
                                           SPA-NAI-THKNJANUM(IDW IDX)
                   MOVE    RECE07-PREFKBN-SRT (1:1)
                                           TO
                                           SPA-NAI-TPREFKBN (IDW IDX)
                   IF      SPA-NAI-THKNJANUM(IDW IDX)
                                           =   SPACE
                       MOVE    "保険者設定なし"    TO  
                                               SPA-GMN-TSYURUINAME
                                                        (IDW IDX)
                       MOVE    "1"             TO   
                                               SPA-NAI-THKNJANUM-ERR
                                                       (IDW IDX)
                   ELSE    
                       MOVE    SPACE           TO  HKNJAINF-REC
                       INITIALIZE                  HKNJAINF-REC
                       MOVE    SPA-HOSPID      TO  HKNJA-HOSPID
                       MOVE    SPA-NAI-THKNJANUM(IDW IDX)
                                               TO  HKNJA-HKNJANUM
                       MOVE    HKNJAINF-REC    TO  MCPDATA-REC
                       PERFORM 900-HKNJAINF-READ-SEC
                       IF      FLG-HKNJAINF     =   ZERO
                           MOVE    HKNJA-HKNJANAME
                                           TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
                       ELSE
                           MOVE    SPA-NAI-THKNJANUM(IDW IDX)
                                           TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
                           MOVE    "1"     TO   
                                           SPA-NAI-THKNJANUM-ERR
                                                               (IDW IDX)
                       END-IF
                   END-IF
                   ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE   (IDW IDX)
                                               SPA-GMN-PAGEH
                   ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU  (IDW IDX)
                                               SPA-GMN-KENSUH
                   MOVE    IDX (3:2)       TO  SPA-NAI-PRTJYUNI(IDW IDX)
                   MOVE    "2"             TO
                                       SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    "9999"          TO
                                       SPA-NAI-SYUBETU     (IDW IDX 1)
                   MOVE    "T"             TO
                                       SPA-GMN-TCHK-VALUE  (IDW IDX)
                   ADD     RECE07-PAGE-CHOUKI
                                           TO
                                       SPA-NAI-TPAGE-CHOUKI(IDW IDX)
                   ADD     RECE07-PAGE-ZAITAKU
                                           TO
                                       SPA-NAI-TPAGE-ZAITAKU(IDW IDX)
      * 
                   MOVE    "tbl_recekanri"     TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-RECE07-NEXT-SEC
               END-PERFORM
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDW                 TO  SPA-GMN-PAGE-MAX
           .
       3101-PAGE-HENSYU-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    特別療養費・自費・不明分の編集処理
      *****************************************************************
       31011-PAGE-HENSYU-SEC            SECTION.
      *
      *
      *    特別療養費の編集     
           MOVE    "34"              TO  SPA-NAI-PRTJYUNI    (1 34)
           MOVE    "2"               TO  SPA-NAI-TEISYUTUSAKI(1 34)
           MOVE    "6666"            TO  SPA-NAI-SYUBETU     (1 34 1)
           MOVE    "特別療養費"      TO  SPA-GMN-TSYURUINAME (1 34)
           MOVE    "T"               TO  SPA-GMN-TCHK-VALUE  (1 34)
      *
      *    自費の編集     
           MOVE    "35"              TO  SPA-NAI-PRTJYUNI    (1 35)
           MOVE    "1"               TO  SPA-NAI-TEISYUTUSAKI(1 35)
           MOVE    "8888"            TO  SPA-NAI-SYUBETU     (1 35 1)
           MOVE    "自費"            TO  SPA-GMN-TSYURUINAME (1 35)
           MOVE    "T"               TO  SPA-GMN-TCHK-VALUE  (1 35)
      *
      *    レセプト種別不明の編集     
           MOVE    "36"              TO  SPA-NAI-PRTJYUNI    (1 36)
           MOVE    "1"               TO  SPA-NAI-TEISYUTUSAKI(1 36)
           MOVE    "0000"            TO  SPA-NAI-SYUBETU     (1 36 1)
           MOVE    "レセプト種別不明"
                                     TO  SPA-GMN-TSYURUINAME (1 36)
           MOVE    "T"               TO  SPA-GMN-TCHK-VALUE  (1 36)
      *                                     
      *    特別療養費レセプトのページ数     
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "2"                 TO  RECE07-TEISYUTUSAKI 
           MOVE    "6666"              TO  RECE07-RECESYUBETU
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   ZERO
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 34)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 34)
                                           SPA-GMN-KENSUH
           END-IF
      *                                     
      *    自費レセプトのページ数     
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
           MOVE    "8888"              TO  RECE07-RECESYUBETU
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   ZERO
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 35)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 35)
                                           SPA-GMN-KENSUH
           END-IF
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "2"                 TO  RECE07-TEISYUTUSAKI 
           MOVE    "8888"              TO  RECE07-RECESYUBETU
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   ZERO
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 35)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 35)
                                           SPA-GMN-KENSUH
           END-IF
      *                                     
      *    レセプト種別不明のレセプトのページ数     
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
           MOVE    "0000"              TO  RECE07-RECESYUBETU
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   ZERO
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 36)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 36)
                                           SPA-GMN-KENSUH
           END-IF
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "2"                 TO  RECE07-TEISYUTUSAKI 
           MOVE    "0000"              TO  RECE07-RECESYUBETU
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   ZERO
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 36)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 36)
                                           SPA-GMN-KENSUH
           END-IF
           .
       31011-PAGE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-RID1-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    =   "OK"
               EVALUATE    SPA-RIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"           TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1003"    
                       PERFORM 440-PRINT-KENSU-SEC
                   WHEN    "8888"
                       CONTINUE    
               END-EVALUATE        
           END-IF
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
           MOVE    SPACE           TO  SPA-RIDCD      
           .
       330-RID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＲＩＤ３）ＯＫ処理
      *****************************************************************
       330-RID3-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    NOT =   SPACE
               EVALUATE    SPA-RIDCD3
                   WHEN    "1001"
                   WHEN    "1002"    
                       PERFORM 450-PRINT-SEC
               END-EVALUATE
           ELSE        
               MOVE    SPACE           TO  SPA-RID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-RIDCD3      
           .
       330-RID3-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-R99-SET-SEC            SECTION.
      *
           IF      SPA-R99-FLG     =   "OK"
               PERFORM 4501-NEXT-GMN-SEC
           END-IF    
      *
      *    印刷済フラグの設定
           IF      SPA-R99-CANCEL  =   SPACE
               IF    ( SPA-GMN-SYOKBN  =   "F"         )
               AND   ( SPA-GMN-PRTKBN  =   "1"         )
               AND   ( SPA-GMN-PREFKBN =   "1" OR  "4" )
               AND   ( SPA-RID1-FLG    =   "OK"        )
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       SPA-GMN-PAGE-MAX
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       WRK-CONS-LINE-MAX
                           IF      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                               CONTINUE
                           ELSE
                               IF    ( SPA-GMN-TCHK-VALUE  (IDW IDX)
                                                           =   "T"   )
                               AND   ( SPA-GMN-TSTDATA (IDW IDX)
                                                           =   SPACE )
                               AND   ( SPA-GMN-TEDDATA (IDW IDX)
                                                           =   SPACE )
                                   MOVE    "1"     TO
                                               SPA-GMN-TPRTFLG (IDW IDX)
                               END-IF
                           END-IF 
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-IF     
      *
           MOVE    SPACE           TO  SPA-R99-FLG
           MOVE    SPACE           TO  SPA-RIDCD
           MOVE    SPACE           TO  SPA-RIDCD2      
           MOVE    SPACE           TO  SPA-RIDCD3
      *****MOVE    ZERO            TO  SPA-R01-JOBCNT       
      *****                            SPA-R01-STOPFLG    
      *****MOVE    SPACE           TO  SPA-RID1-FLG
      *****                            SPA-R99-CANCEL
           .
       340-R99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.       
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R02"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    SPA-LNK-XC01-KEY    TO  LNK-XC01-KEY
           MOVE    1                   TO  LNK-XC01-SHORI-RENNUM
           MOVE    1                   TO  LNK-XC01-ST-PAGE
           MOVE    99999               TO  LNK-XC01-ED-PAGE             
           MOVE    "1"                 TO  LNK-XC01-MODE            
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       4501-NEXT-GMN-EXT.
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
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    件数印刷
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-PRINT-KENSU-GIDMSG-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *     処理区分
               WHEN    "CLICKED"       ALSO    "B08"
               WHEN    "CLICKED"       ALSO    "SYOKBN"
                   PERFORM 410-SYOKBN-CHK-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *    印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-PRINT-GIDMSG-SEC
      *    編綴情報表示
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-HENTETU-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B24"
                   PERFORM 490-KAKUNIN-SEC
      *     個別出力
               WHEN    "CLICKED"       ALSO    "CHK01"
               WHEN    "CLICKED"       ALSO    "CHK02"
               WHEN    "CLICKED"       ALSO    "CHK03"
               WHEN    "CLICKED"       ALSO    "CHK04"
               WHEN    "CLICKED"       ALSO    "CHK05"
               WHEN    "CLICKED"       ALSO    "CHK06"
               WHEN    "CLICKED"       ALSO    "CHK07"
               WHEN    "CLICKED"       ALSO    "CHK08"
               WHEN    "CLICKED"       ALSO    "CHK09"
               WHEN    "CLICKED"       ALSO    "CHK10"
               WHEN    "CLICKED"       ALSO    "CHK11"
               WHEN    "CLICKED"       ALSO    "CHK12"
               WHEN    "CLICKED"       ALSO    "CHK13"
               WHEN    "CLICKED"       ALSO    "CHK14"
               WHEN    "CLICKED"       ALSO    "CHK15"
               WHEN    "CLICKED"       ALSO    "CHK16"
               WHEN    "CLICKED"       ALSO    "CHK17"
               WHEN    "CLICKED"       ALSO    "CHK18"
               WHEN    "CLICKED"       ALSO    "CHK19"
               WHEN    "CLICKED"       ALSO    "CHK20"
               WHEN    "CLICKED"       ALSO    "CHK21"
               WHEN    "CLICKED"       ALSO    "CHK22"
               WHEN    "CLICKED"       ALSO    "CHK23"
               WHEN    "CLICKED"       ALSO    "CHK24"
               WHEN    "CLICKED"       ALSO    "CHK25"
               WHEN    "CLICKED"       ALSO    "CHK26"
               WHEN    "CLICKED"       ALSO    "CHK27"
               WHEN    "CLICKED"       ALSO    "CHK28"
               WHEN    "CLICKED"       ALSO    "CHK29"
               WHEN    "CLICKED"       ALSO    "CHK30"
               WHEN    "CLICKED"       ALSO    "CHK31"
               WHEN    "CLICKED"       ALSO    "CHK32"
               WHEN    "CLICKED"       ALSO    "CHK33"
               WHEN    "CLICKED"       ALSO    "CHK34"
               WHEN    "CLICKED"       ALSO    "CHK35"
               WHEN    "CLICKED"       ALSO    "CHK36"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET2         TO  IDX 
                   PERFORM 420-CHK-CHK-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "STDATA01"
               WHEN    "ACTIVATE"      ALSO    "STDATA02"
               WHEN    "ACTIVATE"      ALSO    "STDATA03"
               WHEN    "ACTIVATE"      ALSO    "STDATA04"
               WHEN    "ACTIVATE"      ALSO    "STDATA05"
               WHEN    "ACTIVATE"      ALSO    "STDATA06"
               WHEN    "ACTIVATE"      ALSO    "STDATA07"
               WHEN    "ACTIVATE"      ALSO    "STDATA08"
               WHEN    "ACTIVATE"      ALSO    "STDATA09"
               WHEN    "ACTIVATE"      ALSO    "STDATA10"
               WHEN    "ACTIVATE"      ALSO    "STDATA11"
               WHEN    "ACTIVATE"      ALSO    "STDATA12"
               WHEN    "ACTIVATE"      ALSO    "STDATA13"
               WHEN    "ACTIVATE"      ALSO    "STDATA14"
               WHEN    "ACTIVATE"      ALSO    "STDATA15"
               WHEN    "ACTIVATE"      ALSO    "STDATA16"
               WHEN    "ACTIVATE"      ALSO    "STDATA17"
               WHEN    "ACTIVATE"      ALSO    "STDATA18"
               WHEN    "ACTIVATE"      ALSO    "STDATA19"
               WHEN    "ACTIVATE"      ALSO    "STDATA20"
               WHEN    "ACTIVATE"      ALSO    "STDATA21"
               WHEN    "ACTIVATE"      ALSO    "STDATA22"
               WHEN    "ACTIVATE"      ALSO    "STDATA23"
               WHEN    "ACTIVATE"      ALSO    "STDATA24"
               WHEN    "ACTIVATE"      ALSO    "STDATA25"
               WHEN    "ACTIVATE"      ALSO    "STDATA26"
               WHEN    "ACTIVATE"      ALSO    "STDATA27"
               WHEN    "ACTIVATE"      ALSO    "STDATA28"
               WHEN    "ACTIVATE"      ALSO    "STDATA29"
               WHEN    "ACTIVATE"      ALSO    "STDATA30"
               WHEN    "ACTIVATE"      ALSO    "STDATA31"
               WHEN    "ACTIVATE"      ALSO    "STDATA32"
               WHEN    "ACTIVATE"      ALSO    "STDATA33"
               WHEN    "ACTIVATE"      ALSO    "STDATA34"
               WHEN    "ACTIVATE"      ALSO    "STDATA35"
               WHEN    "ACTIVATE"      ALSO    "STDATA36"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET3         TO  IDX 
                   PERFORM 4103-TSTDATA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "EDDATA01"
               WHEN    "ACTIVATE"      ALSO    "EDDATA02"
               WHEN    "ACTIVATE"      ALSO    "EDDATA03"
               WHEN    "ACTIVATE"      ALSO    "EDDATA04"
               WHEN    "ACTIVATE"      ALSO    "EDDATA05"
               WHEN    "ACTIVATE"      ALSO    "EDDATA06"
               WHEN    "ACTIVATE"      ALSO    "EDDATA07"
               WHEN    "ACTIVATE"      ALSO    "EDDATA08"
               WHEN    "ACTIVATE"      ALSO    "EDDATA09"
               WHEN    "ACTIVATE"      ALSO    "EDDATA10"
               WHEN    "ACTIVATE"      ALSO    "EDDATA11"
               WHEN    "ACTIVATE"      ALSO    "EDDATA12"
               WHEN    "ACTIVATE"      ALSO    "EDDATA13"
               WHEN    "ACTIVATE"      ALSO    "EDDATA14"
               WHEN    "ACTIVATE"      ALSO    "EDDATA15"
               WHEN    "ACTIVATE"      ALSO    "EDDATA16"
               WHEN    "ACTIVATE"      ALSO    "EDDATA17"
               WHEN    "ACTIVATE"      ALSO    "EDDATA18"
               WHEN    "ACTIVATE"      ALSO    "EDDATA19"
               WHEN    "ACTIVATE"      ALSO    "EDDATA20"
               WHEN    "ACTIVATE"      ALSO    "EDDATA21"
               WHEN    "ACTIVATE"      ALSO    "EDDATA22"
               WHEN    "ACTIVATE"      ALSO    "EDDATA23"
               WHEN    "ACTIVATE"      ALSO    "EDDATA24"
               WHEN    "ACTIVATE"      ALSO    "EDDATA25"
               WHEN    "ACTIVATE"      ALSO    "EDDATA26"
               WHEN    "ACTIVATE"      ALSO    "EDDATA27"
               WHEN    "ACTIVATE"      ALSO    "EDDATA28"
               WHEN    "ACTIVATE"      ALSO    "EDDATA29"
               WHEN    "ACTIVATE"      ALSO    "EDDATA30"
               WHEN    "ACTIVATE"      ALSO    "EDDATA31"
               WHEN    "ACTIVATE"      ALSO    "EDDATA32"
               WHEN    "ACTIVATE"      ALSO    "EDDATA33"
               WHEN    "ACTIVATE"      ALSO    "EDDATA34"
               WHEN    "ACTIVATE"      ALSO    "EDDATA35"
               WHEN    "ACTIVATE"      ALSO    "EDDATA36"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET3         TO  IDX 
                   PERFORM 4104-TEDDATA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTJYUN"
                   PERFORM 4105-PRTJYUN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTJYUN1"
                   PERFORM 4108-PRTJYUN1-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PREFKBN"
                   PERFORM 4106-PREFKBN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTKBN"
                   PERFORM 4107-PRTKBN-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力開始（個別）チェック処理
      *****************************************************************
       4103-TSTDATA-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-SYOKBN     =   "T"
               MOVE    "0006"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  4103-TSTDATA-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-PRTJYUN    =   "05"
               MOVE    "0011"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  4103-TSTDATA-CHK-EXT
           END-IF
      *
           MOVE    2                  TO  SPA-GMN-CUR
      *
           COMPUTE SPA-GMN-CUR        =   SPA-GMN-CUR     +
                                          IDX *  3
           MOVE    SPA-GMN-PAGE       TO  IDW
      *                                    
           MOVE    R02-TSTDATA (IDX)  TO  SPA-GMN-TSTDATA (IDW IDX)
      *
           IF      SPA-NAI-TEISYUTUSAKI (IDW IDX)
                                      =   "1"
               MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
           ELSE
               IF      SPA-NAI-SYUBETU  (IDW IDX 1) =   "6666"
                   MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
               ELSE
                   MOVE    SPA-GMN-PRTJYUN1   TO  WRK-PRTJYUN
               END-IF
           END-IF    
      *             
           IF      SPA-GMN-TSTDATA (IDW IDX)  NOT =   SPACE   
               EVALUATE    WRK-PRTJYUN
                   WHEN    "01"
                   WHEN    "03"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TSTDATA (IDW IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "2"
                           MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TSTDATA (IDW IDX)
                           IF      SPA-GMN-TSTDATA (IDW IDX)(1:2)
                                                  >=  "ア"    AND
                                                  <=  "ヵ"
                               CONTINUE
                           ELSE
                               MOVE    "0001"         TO  SPA-ERRCD
                           END-IF 
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
                   WHEN    "02"
                   WHEN    "04"
                   WHEN    "06"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TSTDATA (IDW IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "1"
                           IF      SPA-GMN-TSTDATA (IDW IDX)(1:1)
                                              <  "0"    OR
                                              >  "z"
                               MOVE    "0001"             TO  SPA-ERRCD
                           END-IF                        
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
               END-EVALUATE
           END-IF   
      *
           IF      SPA-ERRCD           =   SPACE
               ADD     1                   TO  SPA-GMN-CUR
           END-IF 
           .
       4103-TSTDATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力終了（個別）チェック処理
      *****************************************************************
       4104-TEDDATA-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-SYOKBN     =   "T"
               MOVE    "0006"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  4104-TEDDATA-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-PRTJYUN    =   "05"
               MOVE    "0011"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  4104-TEDDATA-CHK-EXT
           END-IF
      *
           MOVE    3                  TO  SPA-GMN-CUR
      *
           COMPUTE SPA-GMN-CUR        =   SPA-GMN-CUR     +
                                          IDX *  3
           MOVE    SPA-GMN-PAGE       TO  IDW
      *                                     
           MOVE    R02-TEDDATA (IDX)  TO  SPA-GMN-TEDDATA (IDW IDX)
      *
           IF      SPA-NAI-TEISYUTUSAKI (IDW IDX)
                                      =   "1"
               MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
           ELSE
               IF      SPA-NAI-SYUBETU  (IDW IDX 1)    =   "6666"
                   MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
               ELSE
                   MOVE    SPA-GMN-PRTJYUN1   TO  WRK-PRTJYUN
               END-IF
           END-IF    
      *
           IF      SPA-GMN-TEDDATA (IDW IDX)  NOT =   SPACE   
               EVALUATE    WRK-PRTJYUN
                   WHEN    "01"
                   WHEN    "03"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TEDDATA (IDW IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "2"
                           MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TEDDATA (IDW IDX)
                           IF      SPA-GMN-TEDDATA (IDW IDX)(1:2)
                                                  >=  "ア"    AND
                                                  <=  "ヵ"
                               CONTINUE
                           ELSE
                               MOVE    "0001"         TO  SPA-ERRCD
                           END-IF 
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
                   WHEN    "02"
                   WHEN    "04"
                   WHEN    "06"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TEDDATA (IDW IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "1"
                           IF      SPA-GMN-TEDDATA (IDW IDX)(1:1)
                                              <  "0"    OR
                                              >  "z"
                               MOVE    "0001"         TO  SPA-ERRCD
                           END-IF                        
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
               END-EVALUATE
           END-IF   
      *
           IF      SPA-ERRCD           =   SPACE
               IF      IDX                 =   WRK-CONS-LINE-MAX
                   MOVE    199                 TO  SPA-GMN-CUR
               ELSE 
                   ADD     1                   TO  SPA-GMN-CUR
               END-IF
           END-IF 
           .
       4104-TEDDATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順（社保）入力処理
      *****************************************************************
       4105-PRTJYUN-CHK-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    R02-PRTJYUN         TO  SPA-GMN-PRTJYUN-G
      *
           IF      SPA-GMN-PRTJYUNMEI  =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      SPA-GMN-PRTJYUNLST  (IDX)   =   SPACE
                   IF      SPA-GMN-PRTJYUN =   SPA-GMN-PRTJYUNL (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    5                   TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF    
           END-IF
      *
           IF      SPA-R01-FILEKBN     =   "1"
               CONTINUE
           ELSE
               MOVE    SPA-GMN-PRTJYUN-G   TO  SPA-GMN-PRTJYUN1-G
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        頁・件数の編集処理
               PERFORM 3101-HENSYU-SEC
           END-IF         
           .
       4105-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    県内・県外入力処理
      *****************************************************************
       4106-PREFKBN-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-PRTJYUN    =   "05"
               MOVE    "0011"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  4106-PREFKBN-CHK-EXT
           END-IF
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    R02-PREFKBN         TO  SPA-GMN-PREFKBN-G
      *
           IF      SPA-GMN-PREFKBNMEI   =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                       OR      SPA-GMN-PREFKBNLST  (IDX)   =   SPACE
                   IF      SPA-GMN-PREFKBN =   SPA-GMN-PREFKBNL (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    4                   TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4106-PREFKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力内容入力処理
      *****************************************************************
       4107-PRTKBN-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-PRTJYUN    =   "05"
               MOVE    "0011"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  4107-PRTKBN-CHK-EXT
           END-IF
      *
           MOVE    200                TO  SPA-GMN-CUR
      *
           MOVE    R02-PRTKBN         TO  SPA-GMN-PRTKBN-G
      *
           IF      SPA-GMN-PRTKBNMEI   =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       3
                       OR      SPA-GMN-PRTKBNLST  (IDX)   =   SPACE
                   IF      SPA-GMN-PRTKBN =   SPA-GMN-PRTKBNL (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    3                   TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4107-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順（国保）入力処理
      *****************************************************************
       4108-PRTJYUN1-CHK-SEC             SECTION.
      *
           IF      SPA-R01-FILEKBN     =   "2"
               GO  TO  4108-PRTJYUN1-CHK-EXT
           END-IF
      *
           MOVE    180                  TO  SPA-GMN-CUR
      *
           MOVE    R02-PRTJYUN1         TO  SPA-GMN-PRTJYUN1-G
      *
           IF      SPA-GMN-PRTJYUN1MEI  =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      SPA-GMN-PRTJYUN1LST  (IDX)   =   SPACE
                   IF      SPA-GMN-PRTJYUN1 =   SPA-GMN-PRTJYUN1L (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    5                   TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF    
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        頁・件数の編集処理
               PERFORM 3101-HENSYU-SEC
           END-IF         
           .
       4108-PRTJYUN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る前処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC 
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
      *********AND     JOB-TERMID      =   SPA-TERMID
                   MOVE    "6666"          TO  SPA-RIDCD
                   GO  TO  210-BACK-EXT
               END-IF
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
           MOVE    SPACE           TO  SPA-R99-FLG
           MOVE    SPACE           TO  SPA-RIDCD
           MOVE    SPACE           TO  SPA-RIDCD2      
           MOVE    SPACE           TO  SPA-RIDCD3
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           MOVE    SPACE           TO  SPA-RID1-FLG
                                       SPA-R99-CANCEL
      *
           MOVE    "R03"               TO  SPA-SAKIPG
           MOVE    "R02"               TO  SPA-MOTOPG
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
      *    処理区分  入力処理
      *****************************************************************
       410-SYOKBN-CHK-SEC                SECTION.
      *
           IF    ( SPA-GMN-PRTJYUN    =   "05" )
           AND   ( SPA-GMN-SYOKBN     =   "T"  )
               MOVE    "0011"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  410-SYOKBN-CHK-EXT
           END-IF
      *   
           MOVE    1                   TO  SPA-GMN-CUR
      *
           EVALUATE    SPA-GMN-SYOKBN
      *            *指定印刷
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-SYOKBN
                   MOVE    4               TO  SPA-GMN-CUR
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       WRK-CONS-PAGE-MAX
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >       WRK-CONS-LINE-MAX
                             OR      SPA-NAI-PRTJYUNI (IDW IDX)
                                           =   SPACE
                       MOVE    "F"     TO  SPA-GMN-TCHK-VALUE (IDW IDX)
                     END-PERFORM 
                   END-PERFORM
                   MOVE    "F"      TO  SPA-GMN-TCHK-VALUE (1 34)
                                        SPA-GMN-TCHK-VALUE (1 35)
                                        SPA-GMN-TCHK-VALUE (1 36)
      *            *全件印刷
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-SYOKBN
                   MOVE    199             TO  SPA-GMN-CUR
                   INITIALIZE                  SPA-GMN-TBL
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       WRK-CONS-PAGE-MAX
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       WRK-CONS-LINE-MAX
                           MOVE    "F" TO  SPA-GMN-TCHK-VALUE (IDW IDX)
                       END-PERFORM
                   END-PERFORM    
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       WRK-CONS-PAGE-MAX
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >       WRK-CONS-LINE-MAX
                             OR      SPA-NAI-PRTJYUNI (IDW IDX)
                                                 =   SPACE
                       MOVE    "T"     TO  SPA-GMN-TCHK-VALUE (IDW IDX)
                     END-PERFORM 
                   END-PERFORM
                   MOVE    "T"      TO  SPA-GMN-TCHK-VALUE (1 34)
                                        SPA-GMN-TCHK-VALUE (1 35)
                                        SPA-GMN-TCHK-VALUE (1 36)
           END-EVALUATE 
      *
           .
       410-SYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別出力  入力処理
      *****************************************************************
       420-CHK-CHK-SEC                SECTION.
      *
           IF      SPA-GMN-SYOKBN     =   "T"
               MOVE    "0006"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  420-CHK-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-PRTJYUN    =   "05"
               MOVE    "0011"             TO  SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
               GO  TO  420-CHK-CHK-EXT
           END-IF
      *
           MOVE    1                  TO  SPA-GMN-CUR
      *
           COMPUTE SPA-GMN-CUR        =   SPA-GMN-CUR     +
                                          IDX *  3
           MOVE    SPA-GMN-PAGE       TO  IDW                               
      *
           IF    ( R02-TCHK-VALUE (IDX)   =   "T"  )
           AND   ( SPA-GMN-TPAGE  (IDW IDX)
                                          =   ZERO )
               MOVE    "0002"                 TO  SPA-ERRCD
           ELSE     
               MOVE    R02-TCHK-VALUE (IDX)   TO
                                          SPA-GMN-TCHK-VALUE (IDW IDX)
           END-IF                               
      *
           ADD     1                  TO  SPA-GMN-CUR
           .
       420-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    件数出力前処理
      *****************************************************************
       440-PRINT-KENSU-GIDMSG-SEC      SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC 
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   GO  TO  450-PRINT-GIDMSG-EXT
               END-IF
           END-IF    
      *
      *    ガイドメッセージセット
           MOVE    "1003"              TO  SPA-RIDCD
      *
           .
       440-PRINT-KENSU-GIDMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       440-PRINT-KENSU-SEC                SECTION.
      *
           EVALUATE   SPA-R01-RECE07-NYUGAIKBN
               WHEN   "1"
                   CALL    "ORCHCM35"  USING   SPA-AREA
                                               SPA-NAI-2005-SYAPRTNM-N
                                               SPA-GMN-FILEKBN
                                               SPA-GMN-TBL1
               WHEN   "2"
                   CALL    "ORCHCM35"  USING   SPA-AREA
                                               SPA-NAI-2005-SYAPRTNM
                                               SPA-GMN-FILEKBN
                                               SPA-GMN-TBL1
           END-EVALUATE
      *
           .
       440-PRINT-KENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力前処理
      *****************************************************************
       450-PRINT-GIDMSG-SEC                SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC 
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   GO  TO  450-PRINT-GIDMSG-EXT
               END-IF
           END-IF    
      *
      *    レセプトが選択されているか
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       SPA-GMN-PAGE-MAX
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       WRK-CONS-LINE-MAX
               IF      SPA-GMN-TCHK-VALUE (IDW IDX)    =   "T"
                   MOVE    1                   TO  IDZ
                   MOVE    WRK-CONS-LINE-MAX   TO  IDX
               END-IF
             END-PERFORM  
           END-PERFORM
           IF      IDZ                 =   ZERO
               MOVE    "0008"          TO  SPA-ERRCD
               GO  TO  450-PRINT-GIDMSG-EXT
           END-IF
      *
      *    ガイドメッセージセット
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "T"
                   MOVE    "1001"              TO  SPA-RIDCD3
               WHEN    "F"
                   MOVE    "1002"              TO  SPA-RIDCD3
           END-EVALUATE
      *
           .
       450-PRINT-GIDMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       450-PRINT-SEC                SECTION.
      *
      *    フラグ初期化
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           MOVE    SPACE           TO  SPA-R99-CANCEL
      *
      *    レセ印刷ＤＢの初期化
           INITIALIZE                  RECE08-REC
           MOVE    SPA-R01-RECE07-PRTID  TO  RECE08-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  RECE08-RECEYM
           ELSE
               MOVE    ZERO                TO  RECE08-RECEYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD  TO  RECE08-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS
                                       TO  RECE08-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  RECE08-NYUGAIKBN
           MOVE    RECE08-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
      *    印刷ＤＢのＫＥＹ用     
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           MOVE    SMCNDATE-HMS        TO  SYS-TIME
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP                                
           MOVE    WRK-SYSYMD          TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SPA-R01-PRTKANRI-TBL-GROUP
      *
      *    ジョブ開始セット
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
      *    帳票ファイル作成処理
           MOVE    ZERO             TO  WRK-RECE08-PRTJYUN  
      *
      *    入力順指定
           IF      SPA-GMN-PRTJYUN =   "05"
               PERFORM 4502-RECEPRT-INPUT-HENSYU-SEC
           ELSE
      *        社保出力
               MOVE    1                TO  IDW 
               MOVE    "1"              TO  WRK-TEISYUTUSAKI
               MOVE    "0"              TO  WRK-PREFKBN
               PERFORM 4501-PRTJYUN-PRINT-SEC
      *
      *        国保出力
               IF      SPA-ERRCD        =   SPACE
      *            入力順指定
                   IF      SPA-GMN-PRTJYUN1    =   "06"
                       MOVE    "2"              TO  WRK-TEISYUTUSAKI
                       PERFORM 4503-RECEPRT-INPUT-HENSYU-SEC
                   ELSE
      *                国保対象頁設定
                       IF    ( SPA-GMN-PRTJYUN1    =   "01"   OR  "02" )
                       OR    ( SPA-GMN-PRTJYUN     =   "01"   OR  "02" )
                           MOVE    2               TO  IDW
                       ELSE
                           MOVE    1               TO  IDW
                       END-IF 
      *       
                       MOVE    "2"             TO  WRK-TEISYUTUSAKI
      *                国保（県内・県外混在）出力
                       IF      SPA-GMN-PREFKBN =   "4"
                           MOVE    "%"             TO  WRK-PREFKBN
                           PERFORM 4501-PRTJYUN-PRINT-SEC
                       ELSE
      *
      *                    国保（県内分）出力
                           IF    ( SPA-ERRCD       =   SPACE )
                               IF      SPA-GMN-PREFKBN =   "1" OR  "2"
                                   MOVE    "1"     TO  WRK-PREFKBN
                                   PERFORM 4501-PRTJYUN-PRINT-SEC
                               END-IF
                           END-IF
      *
      *                    国保（県外分）出力
                           IF    ( SPA-ERRCD       =   SPACE )
                               IF      SPA-GMN-PREFKBN =   "1" OR  "3"
                                   MOVE    "2"      TO  WRK-PREFKBN
                                   PERFORM 4501-PRTJYUN-PRINT-SEC
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
      *        特別療養費レセプト出力
               IF    ( SPA-ERRCD        =   SPACE )
                   IF    ( SPA-GMN-TCHK-VALUE  (1 34)   =   "T"  )
                   AND   ( SPA-GMN-TPAGE       (1 34)   >   ZERO )
                       MOVE    1           TO  IDW
                       MOVE    ZERO        TO  WRK-TEISYUTUSAKI
                       MOVE    ZERO        TO  WRK-PREFKBN
                       MOVE    34          TO  IDX
                       MOVE    1           TO  IDY
                       PERFORM 45011-RECEPRT-HENSYU-SEC
                   END-IF
               END-IF
      *
      *        自費レセプト出力
               IF    ( SPA-ERRCD        =   SPACE )
                   IF      SPA-GMN-TCHK-VALUE  (1 35)   =   "T"
                   AND     SPA-GMN-TPAGE       (1 35)   >   ZERO
                       MOVE    1               TO  IDW
                       MOVE    ZERO            TO  WRK-TEISYUTUSAKI
                       MOVE    ZERO            TO  WRK-PREFKBN
                       MOVE    35              TO  IDX
                       MOVE    1               TO  IDY
                       PERFORM 45011-RECEPRT-HENSYU-SEC
                   END-IF
               END-IF
      *
      *        レセプト不明分出力
               IF    ( SPA-ERRCD        =   SPACE )
                   IF      SPA-GMN-TCHK-VALUE  (1 36)   =   "T"
                   AND     SPA-GMN-TPAGE       (1 36)   >   ZERO
                       MOVE    1               TO  IDW
                       MOVE    ZERO            TO  WRK-TEISYUTUSAKI
                       MOVE    ZERO            TO  WRK-PREFKBN
                       MOVE    36              TO  IDX
                       MOVE    1               TO  IDY
                       PERFORM 45011-RECEPRT-HENSYU-SEC
                   END-IF
               END-IF    
           END-IF
      *
      *    印刷処理開始（ＯＲＣＲ０１０２）
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    SPA-TERMID          TO  RECEERR-TERMID
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept2.sh"    TO  WRK-SHELL-SHELL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    WRK-SHELL-NAME  TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *     
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG2
           MOVE    ZERO            TO  WRK-SHELL-SRYYM
           MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
           MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG3 
           MOVE    "1"             TO  SHELLTBL-ARG4
           IF      SPA-R01-FILEKBN =   "1"
               MOVE    "1"             TO  SHELLTBL-ARG5
           ELSE
               MOVE    "2"             TO  SHELLTBL-ARG5
           END-IF    
           MOVE    RECEERR         TO  SHELLTBL-ARG6
           MOVE    WRK-RECE09-KEY  TO  SHELLTBL-ARG7 
           MOVE    "00000"         TO  SHELLTBL-ARG8
           MOVE    "99999"         TO  SHELLTBL-ARG9
           EVALUATE   SPA-R01-RECE07-NYUGAIKBN
               WHEN   "1"
                   MOVE    SPA-NAI-2005-SYAPRTNM-N
                                   TO  SHELLTBL-ARG11
                   MOVE    SPA-NAI-2005-KKHPRTNM-N
                                   TO  SHELLTBL-ARG12
               WHEN   "2"
                   MOVE    SPA-NAI-2005-SYAPRTNM
                                   TO  SHELLTBL-ARG11
                   MOVE    SPA-NAI-2005-KKHPRTNM
                                   TO  SHELLTBL-ARG12
           END-EVALUATE
      *                                     
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           MOVE    1               TO  LNK-PRTKANRI-RENNUM
                                       SPA-LNK-XC01-RENNUM
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY
                                       SPA-LNK-XC01-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
                                       SPA-LNK-XC01-TBL-GROUP
           MOVE    SPA-R01-RECE07-SRYYM
                                   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "recept2.sh"    TO  LNK-PRTKANRI-SHELLID
           IF      SPA-R01-RECE07-NYUGAIKBN    =   "1"
               MOVE    1               TO  LNK-PRTKANRI-PRIORITY
           ELSE
               MOVE    2               TO  LNK-PRTKANRI-PRIORITY
           END-IF
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *****MOVE    SYS-1031-PRTNM (5)
      *****                            TO  LNK-PRTKANRI-PRTNM
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG13       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG14     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG15       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG16       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG17       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG18       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG19       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG20      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG21       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG22      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG23
           MOVE    SPA-GMN-PRTKBN  TO  SHELLTBL-ARG24
           EVALUATE   SPA-R01-RECE07-NYUGAIKBN
               WHEN   "1"
                   MOVE    SPA-NAI-2005-SYAPRTNM-NEXT-N
                                   TO  SHELLTBL-ARG25
                   MOVE    SPA-NAI-2005-KKHPRTNM-NEXT-N
                                   TO  SHELLTBL-ARG26
               WHEN   "2"
                   MOVE    SPA-NAI-2005-SYAPRTNM-NEXT
                                   TO  SHELLTBL-ARG25
                   MOVE    SPA-NAI-2005-KKHPRTNM-NEXT
                                   TO  SHELLTBL-ARG26
           END-EVALUATE
           EVALUATE    SPA-RID1-FLG
               WHEN    "PV"
                   MOVE    "4"             TO  SHELLTBL-ARG27
               WHEN    "NO"
                   MOVE    "3"             TO  SHELLTBL-ARG27
               WHEN    "OK"
                   MOVE    "2"             TO  SHELLTBL-ARG27
           END-EVALUATE
      *     
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           COMPUTE SPA-R01-JOBCNT  =   WRK-RECE08-PRTJYUN  +   1
      *                         
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       450-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順出力処理
      *****************************************************************
       4501-PRTJYUN-PRINT-SEC         SECTION.
      *
      *    印刷順にファイル読み込み
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-LINE-MAX
                   OR      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                   OR      SPA-ERRCD               NOT =   SPACE    
               IF      SPA-GMN-TCHK-VALUE  (IDW IDX)   =   "T"
               AND     SPA-GMN-TPAGE       (IDW IDX)   >   ZERO
               AND     SPA-NAI-TEISYUTUSAKI(IDW IDX)
                                               =   WRK-TEISYUTUSAKI
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       10
                           OR      SPA-NAI-SYUBETU (IDW IDX IDY)
                                                       =   SPACE
                           OR      SPA-ERRCD       NOT =   SPACE
                       PERFORM 45011-RECEPRT-HENSYU-SEC  
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       4501-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       45011-RECEPRT-HENSYU-SEC         SECTION.
      *
      *    印刷用パラメタ
           INITIALIZE                      WRK-RECE09-REC
                                           WRK-RECE09-STDATA
                                           WRK-RECE09-EDDATA
      *
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  WRK-RECE09-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  WRK-RECE09-SRYYM
           ELSE
               MOVE    ZERO                TO  WRK-RECE09-SRYYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD   TO  WRK-RECE09-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS   TO  WRK-RECE09-CREHMS
      *
           MOVE    SPA-NAI-SYUBETU (IDW IDX IDY)
                                       TO  WRK-RECE09-RECESYUBETU
           MOVE    WRK-TEISYUTUSAKI    TO  WRK-RECE09-TEISYUTUSAKI
           MOVE    WRK-PREFKBN         TO  WRK-RECE09-PREFKBN
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  WRK-RECE09-NYUGAIKBN
           IF      WRK-TEISYUTUSAKI    =   "1"
               MOVE    SPA-GMN-PRTJYUN     TO  WRK-RECE09-PRTJYUN
           ELSE
               IF      SPA-NAI-SYUBETU (IDW IDX IDY)   =   "6666"
                   MOVE    SPA-GMN-PRTJYUN     TO  WRK-RECE09-PRTJYUN
               ELSE
                   MOVE    SPA-GMN-PRTJYUN1    TO  WRK-RECE09-PRTJYUN
               END-IF
           END-IF
      * 
           MOVE    SPA-GMN-TSTDATA (IDW IDX)
                                       TO  WRK-RECE09-STDATA
           MOVE    SPA-GMN-TEDDATA (IDW IDX)
                                       TO  WRK-RECE09-EDDATA 
           IF      WRK-RECE09-STDATA   =   SPACE
               IF      WRK-RECE09-PRTJYUN  =   "01"  OR  "03"
                   MOVE    SPACE       TO  WRK-RECE09-STDATA
               ELSE
                   MOVE    "0"         TO
                                           WRK-RECE09-STDATA (1:1)
               END-IF
           ELSE     
               IF      WRK-RECE09-PRTJYUN  =   "02"  OR  "04"
                   PERFORM VARYING IDZ FROM    20  BY  -1
                           UNTIL   IDZ <       1
                       IF      WRK-RECE09-STDATA(IDZ:1)
                                               NOT =   SPACE
                           MOVE    1       TO  IDZ 
                       ELSE  
                           MOVE    "Z"     TO  WRK-RECE09-STDATA(IDZ:1)
                       END-IF
                   END-PERFORM
               END-IF     
           END-IF
           IF      WRK-RECE09-EDDATA       =   SPACE
               IF      WRK-RECE09-PRTJYUN      =   "01"  OR  "03"
                   MOVE    "ヵ"    TO  WRK-RECE09-EDDATA (1:2)
               ELSE
                   MOVE    "zzzzzzzzzzzzzzzzzzzz"
                                   TO  WRK-RECE09-EDDATA (1:20)
               END-IF
           ELSE     
               IF      WRK-RECE09-PRTJYUN  =   "02" OR  "04"
                   PERFORM VARYING IDZ FROM    20  BY  -1
                           UNTIL   IDZ <       1
                       IF      WRK-RECE09-EDDATA(IDZ:1)
                                                   NOT =   SPACE
                           MOVE    1       TO  IDZ 
                       ELSE  
                           MOVE    "Z"     TO  WRK-RECE09-EDDATA(IDZ:1)
                       END-IF
                   END-PERFORM
               END-IF     
           END-IF
      *
           IF      WRK-RECE09-PRTJYUN  =   "06"
               MOVE    SPA-NAI-THKNJANUM (IDW IDX)
                                       TO  WRK-RECE09-HKNJANUM
               MOVE    WRK-CHOUKI      TO  WRK-RECE09-CHOUKI
               MOVE    WRK-ZAITAKU     TO  WRK-RECE09-ZAITAKU
           END-IF
      *
      *　　（ＯＲＣＲ０１０１）　
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    SPA-TERMID          TO  RECEERR-TERMID
      *
           ADD     1               TO  WRK-RECE08-PRTJYUN                 
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept2.sh"    TO  WRK-SHELL-SHELL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    WRK-SHELL-NAME  TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *     
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG2
           MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
           MOVE    SPA-R01-RECE07-SRYYM
                                   TO  WRK-SHELL-SRYYM    
           MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG3 
           MOVE    "0"             TO  SHELLTBL-ARG4
           MOVE    "0"             TO  SHELLTBL-ARG5
           MOVE    RECEERR         TO  SHELLTBL-ARG6
           MOVE    WRK-RECE09-REC  TO  SHELLTBL-ARG7 
           MOVE    WRK-RECE09-STDATA
                                   TO  SHELLTBL-ARG8
           MOVE    WRK-RECE09-EDDATA
                                   TO  SHELLTBL-ARG9
           MOVE    WRK-RECE08-PRTJYUN
                                   TO  SHELLTBL-ARG10
           MOVE    SPA-GMN-PRTKBN  TO  SHELLTBL-ARG24
           MOVE    SPA-GMN-PREFKBN TO  SHELLTBL-ARG28
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       45011-RECEPRT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力順出力処理（個別作成時）
      *****************************************************************
       4502-RECEPRT-INPUT-HENSYU-SEC       SECTION.
      *
      *    印刷用パラメタ
           INITIALIZE                  WRK-RECE09-REC
                                       WRK-RECE09-STDATA
                                       WRK-RECE09-EDDATA
      *
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  WRK-RECE09-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  WRK-RECE09-SRYYM
           ELSE
               MOVE    ZERO                TO  WRK-RECE09-SRYYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD   TO  WRK-RECE09-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS   TO  WRK-RECE09-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  WRK-RECE09-NYUGAIKBN
      *    初期値をセット
           MOVE    "0000"              TO  WRK-RECE09-RECESYUBETU
           MOVE    "0"                 TO  WRK-RECE09-TEISYUTUSAKI
           MOVE    "0"                 TO  WRK-RECE09-PREFKBN
           MOVE    SPA-GMN-PRTJYUN     TO  WRK-RECE09-PRTJYUN
           MOVE    "0"                 TO  WRK-RECE09-STDATA (1:1)
           MOVE    "Z"                 TO  WRK-RECE09-EDDATA (1:1)
      *
      *　　（ＯＲＣＲ０１０１）　
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    SPA-TERMID          TO  RECEERR-TERMID
      *
           ADD     1               TO  WRK-RECE08-PRTJYUN                 
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept2.sh"    TO  WRK-SHELL-SHELL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    WRK-SHELL-NAME  TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *     
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG2
           MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
           MOVE    SPA-R01-RECE07-SRYYM
                                   TO  WRK-SHELL-SRYYM    
           MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG3 
           MOVE    "0"             TO  SHELLTBL-ARG4
           MOVE    "0"             TO  SHELLTBL-ARG5
           MOVE    RECEERR         TO  SHELLTBL-ARG6
           MOVE    WRK-RECE09-REC  TO  SHELLTBL-ARG7 
           MOVE    WRK-RECE09-STDATA
                                   TO  SHELLTBL-ARG8
           MOVE    WRK-RECE09-EDDATA
                                   TO  SHELLTBL-ARG9
           MOVE    WRK-RECE08-PRTJYUN  TO  SHELLTBL-ARG10
           MOVE    SPA-GMN-PRTKBN  TO  SHELLTBL-ARG24
           MOVE    SPA-GMN-PREFKBN TO  SHELLTBL-ARG28
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       4502-RECEPRT-INPUT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴出力処理（国保のみ）
      *****************************************************************
       4503-RECEPRT-INPUT-HENSYU-SEC       SECTION.
      *
      *    県内・県外の括りで出力
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       2
               EVALUATE    IDX1
                   WHEN    1   
                       MOVE    "1"                 TO  WRK-PREFKBN
                   WHEN    2   
                       MOVE    "2"                 TO  WRK-PREFKBN
               END-EVALUATE
      *    
      *        長期の設定が県内・県外の先頭のとき
               IF      SPA-NAI-2008-CHOUKI     =   "1"
                   MOVE    "1"                 TO  WRK-CHOUKI
                   MOVE    "%"                 TO  WRK-ZAITAKU
      *
                   PERFORM 45031-RECEPRT-HENSYU-SEC
               END-IF
      *
      *        在総診の設定が県内・県外の先頭のとき（入院外のみ）
               IF    ( SPA-NAI-2008-ZAITAKU    =   "1" )
               AND   ( SPA-R01-RECE07-NYUGAIKBN
                                               =   "2" )
                   MOVE    "1"                 TO  WRK-ZAITAKU
                   EVALUATE    SPA-NAI-2008-CHOUKI
                       WHEN    "1"
                       WHEN    "2" 
                           MOVE    "9"             TO  WRK-CHOUKI
                       WHEN    SPACE 
                           MOVE    "%"             TO  WRK-CHOUKI
                   END-EVALUATE
      *
                   PERFORM 45031-RECEPRT-HENSYU-SEC
               END-IF
      *
               EVALUATE    SPA-NAI-2008-CHOUKI
                   WHEN    "1"
                       MOVE    "9"                 TO  WRK-CHOUKI
                   WHEN    "2"
                   WHEN    SPACE
                       MOVE    "%"                 TO  WRK-CHOUKI
               END-EVALUATE
               EVALUATE    SPA-NAI-2008-ZAITAKU
                   WHEN    "1"
                       MOVE    "9"                 TO  WRK-ZAITAKU
                   WHEN    "2"
                   WHEN    SPACE
                       MOVE    "%"                 TO  WRK-ZAITAKU
               END-EVALUATE
               PERFORM 45031-RECEPRT-HENSYU-SEC
           END-PERFORM
           .
       4503-RECEPRT-INPUT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       45031-RECEPRT-HENSYU-SEC             SECTION.
      *
           PERFORM VARYING IDW FROM    2   BY  1
                   UNTIL   IDW >       SPA-GMN-PAGE-MAX
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-LINE-MAX
                       OR      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                       OR      SPA-ERRCD               NOT =   SPACE    
                   IF    ( SPA-GMN-TCHK-VALUE  (IDW IDX)   =   "T"   )
                   AND   ( SPA-GMN-TPAGE       (IDW IDX)   >   ZERO  )
                   AND   ( WRK-PREFKBN                     =
                                           SPA-NAI-TPREFKBN (IDW IDX))
                       MOVE    1               TO  IDY
                       IF      WRK-CHOUKI      =   "1"
                           IF      SPA-NAI-TPAGE-CHOUKI (IDW IDX)
                                                      >   ZERO
                               PERFORM 45011-RECEPRT-HENSYU-SEC
                           END-IF
                       ELSE
                           IF      WRK-ZAITAKU     =   "1"
                               IF      SPA-NAI-TPAGE-ZAITAKU (IDW IDX)
                                                          >   ZERO
                                   PERFORM 45011-RECEPRT-HENSYU-SEC
                               END-IF
                           ELSE
                               PERFORM 45011-RECEPRT-HENSYU-SEC
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       45031-RECEPRT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-PAGE       =   1
               MOVE    "0009"             TO  SPA-ERRCD
               GO  TO  460-MAEGMN-EXT
           END-IF
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    -   1
      *
           MOVE    199                  TO  SPA-GMN-CUR
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC                    SECTION.
      *
          IF      SPA-GMN-PAGE       =   SPA-GMN-PAGE-MAX
               MOVE    "0010"             TO  SPA-ERRCD
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    +   1       
      *
           MOVE    199                  TO  SPA-GMN-CUR
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴情報表示
      *****************************************************************
       490-HENTETU-SEC                    SECTION.
      *
           MOVE    SPACE               TO  R05
      *
           EVALUATE    SPA-NAI-2008-PREFKBN-PRTJYUN
               WHEN    "1"
                   MOVE    "県内・県外"    TO  R05-PR-PRTJYUN
               WHEN    "2"
                   MOVE    "県外・県内"    TO  R05-PR-PRTJYUN
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-HKNJANUM-PRTJYUN
               WHEN    "1"
                   MOVE
                       "市町村国保・国保組合・県内扱いの県外国保組合"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "2"
                   MOVE
                       "市町村国保・県内扱いの県外国保組合・国保組合"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "3"
                   MOVE
                       "国保組合・市町村国保・県内扱いの県外国保組合"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "4"
                   MOVE
                       "国保組合・県内扱いの県外国保組合・市町村国保"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "5"
                   MOVE
                       "県内扱いの県外国保組合・市町村国保・国保組合"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "6"
                   MOVE
                       "県内扱いの県外国保組合・国保組合・市町村国保"
                                           TO  R05-HKN-PRTJYUN
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-RECESYUBETU-PRTJYUN
               WHEN    "1"
                   MOVE    "国保一般・老人保健・退職国保"
                                           TO  R05-REC-PRTJYUN
               WHEN    "2"
                   MOVE    "国保一般・退職国保・老人保健"
                                           TO  R05-REC-PRTJYUN
               WHEN    "3"
                   MOVE    "老人保健・国保一般・退職国保"
                                           TO  R05-REC-PRTJYUN
               WHEN    "4"
                   MOVE    "老人保健・退職国保・国保一般"
                                           TO  R05-REC-PRTJYUN
               WHEN    "5"
                   MOVE    "退職国保・国保一般・老人保健"
                                           TO  R05-REC-PRTJYUN
               WHEN    "6"
                   MOVE    "退職国保・老人保健・国保一般" 
                                           TO  R05-REC-PRTJYUN
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-ZAITAKU
               WHEN    "1"
                    MOVE    "県内・県外の先頭に印刷" 
                                           TO  R05-ZAITAKU
               WHEN    "2"
                   MOVE    "保険者の先頭に印刷"
                                           TO  R05-ZAITAKU
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-CHOUKI
               WHEN    "1"
                    MOVE    "県内・県外の先頭に印刷" 
                                           TO  R05-CHOUKI
               WHEN    "2"
                   MOVE    "保険者の先頭に印刷"
                                           TO  R05-CHOUKI
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-HKNJANUM-PRTKBN
               WHEN    "1"
                   MOVE    "市町村国保の先頭に印刷" 
                                           TO  R05-HKNJ-PRTKBN
               WHEN    "2"
                   MOVE    "市町村国保の最後に印刷"
                                           TO  R05-HKNJ-PRTKBN
           END-EVALUATE
      *
           MOVE    SPA-NAI-2008-HKNJANUM   TO  R05-HKNJANUM
           MOVE    SPA-NAI-2008-HKNJANAME  TO  R05-HKNJANAME
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "R05"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "R05"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       490-HENTETU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *                                      
      *    画面移行用のパラメタ
           PERFORM 4901-PARA-HENSYU-SEC
      *
           MOVE    SPACE               TO  R99
           MOVE    ZERO                TO  R99-COUNT
                                           R99-PROGRESSBAR-VALUE
      *
           IF      SPA-RID1-FLG    =   "PV"
               MOVE    "F12:プレビュー"    TO  R99-B12-LABEL
      *****ELSE
      *****    MOVE    "F12:印    刷"      TO  R99-B12-LABEL
           END-IF
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC 
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX                     
               MOVE    JOB-JOBID       TO  R99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  R99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  R99-ERRMSG   (IDX)
               COMPUTE R99-PROGRESSBAR-VALUE  =
                       JOB-STEPCNT  /   SPA-R01-JOBCNT *   100
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  R99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       CONTINUE
                   ELSE
                       STRING  JOB-YOBI    DELIMITED  BY  SPACE
                               "【"        DELIMITED  BY  SIZE
                              JOB-SHELLMSG DELIMITED  BY  SPACE
                              "】"         DELIMITED  BY  SIZE      
                                           INTO  R99-MSG
                       END-STRING
                   END-IF                          
               END-IF    
               MOVE    IDX                     TO  R99-COUNT    
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE   TO  R99-B12-STATE
           MOVE    WIDGET-INSENSITIVE   TO  R99-B21-STATE
           IF      R99-MSG              =   SPACE
               MOVE    "B01"                TO  MCP-WIDGET
               IF      R99-COUNT            >   ZERO    
                   IF      JOB-UPDCNT           =   ZERO
                       MOVE    "該当のレセプトはありませんでした"
                                                TO  R99-MSG
                   ELSE
                       IF      JOB-STOPFLG      NOT =   ZERO
                           MOVE    "印刷処理が中止されました"
                                                        TO  R99-MSG
                       ELSE
                           MOVE    JOB-UPDCNT           TO  WRK-PAGE
                           STRING
                               "処理は正常に終了しました　処理枚数は"
                                                DELIMITED  BY  SIZE
                               WRK-PAGE         DELIMITED  BY  SIZE
                               "枚です"         DELIMITED  BY  SIZE
                                                INTO  R99-MSG
                           END-STRING
                       END-IF
                   END-IF
                   MOVE   100               TO  R99-PROGRESSBAR-VALUE
                   IF    ( SPA-RID1-FLG    =   "PV" )
                   AND   ( JOB-UPDCNT      >   ZERO )
                   AND   ( JOB-STOPFLG     =   ZERO )
                       MOVE    WIDGET-NORMAL    TO  R99-B12-STATE
                       MOVE    "B12"            TO  MCP-WIDGET
                   END-IF
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  R99-MSG
               END-IF                                 
           ELSE                                        
               IF      JOB-STOPFLG          =   ZERO
                   MOVE    WIDGET-NORMAL        TO  R99-B21-STATE
               END-IF
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "R99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "R99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面移行用のパラメタ変更処理
      *****************************************************************
       4901-PARA-HENSYU-SEC            SECTION.       
      *    
      *****MOVE    SPACE               TO  SPA-R01KYOUTU
      *****INITIALIZE    SPA-R01KYOUTU
      *
           MOVE    "R02"               TO  SPA-R01-GMNKBN
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           .
       4901-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R02"               TO  SPA-MOTOPG
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
           IF      SPA-RIDCD       NOT =   SPACE
               PERFORM 520-RIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-RIDCD3      NOT =   SPACE
               PERFORM 530-RID3SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "R02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE     R02
      *
           MOVE    SPA-GMN-SYOKBN         TO  R02-SYOKBN-VALUE
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "T"
                   MOVE    "全件印刷"         TO  R02-SYOKBN-LABEL
               WHEN    "F"
                   MOVE    "指定印刷"         TO  R02-SYOKBN-LABEL
           END-EVALUATE      
      *
           MOVE    SPA-GMN-FILEKBN     TO  R02-FILEKBN
      *
           MOVE    SPA-GMN-CREYMD      TO  R02-CREYMD  
      *
           MOVE    SPA-GMN-PAGEH       TO  R02-PAGE  
      *
           MOVE    SPA-GMN-KENSUH      TO  R02-KENSU  
      *
           MOVE    SPA-GMN-PRTJYUN-G   TO  R02-PRTJYUN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-PRTJYUNL    (IDX)   =   SPACE 
               MOVE    SPA-GMN-PRTJYUNLST(IDX) TO  R02-PRTJYUNLST (IDX)
               MOVE    IDX                     TO  R02-PRTJYUN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-PRTJYUN1-G   TO  R02-PRTJYUN1
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-PRTJYUN1L (IDX) =   SPACE 
               MOVE    SPA-GMN-PRTJYUN1LST(IDX)
                                           TO  R02-PRTJYUN1LST (IDX)
               MOVE    IDX                 TO  R02-PRTJYUN1-COUNT
           END-PERFORM
           IF      SPA-R01-FILEKBN     =   "1"
               MOVE    WIDGET-NORMAL       TO  R02-PRTJYUN1-STATE
               MOVE    "  社保"            TO  R02-LABEL1
               MOVE    "  国保"            TO  R02-LABEL2
               MOVE    "出力順"            TO  R02-LABEL3
           ELSE 
               MOVE    WIDGET-INSENSITIVE  TO  R02-PRTJYUN1-STATE
               MOVE    "出力順"            TO  R02-LABEL1
               MOVE    SPACE               TO  R02-LABEL2
                                               R02-LABEL3
           END-IF
      *
           MOVE    SPA-GMN-PREFKBN-G   TO  R02-PREFKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   4
                   OR          SPA-GMN-PREFKBNL    (IDX)   =   SPACE 
               MOVE    SPA-GMN-PREFKBNLST(IDX) TO  R02-PREFKBNLST (IDX)
               MOVE    IDX                     TO  R02-PREFKBN-COUNT
           END-PERFORM
           IF      SPA-GMN-PRTJYUN1    =   "06"
               MOVE    WIDGET-INSENSITIVE  TO  R02-PREFKBN-STATE
           ELSE 
               MOVE    WIDGET-NORMAL       TO  R02-PREFKBN-STATE
           END-IF
      *
           MOVE    SPA-GMN-PRTKBN-G   TO  R02-PRTKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   3
                   OR          SPA-GMN-PRTKBNL    (IDX)   =   SPACE 
               MOVE    SPA-GMN-PRTKBNLST(IDX) TO  R02-PRTKBNLST (IDX)
               MOVE    IDX                     TO  R02-PRTKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-PAGE        TO  IDW
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       WRK-CONS-LINE-MAX
               IF      SPA-NAI-PRTJYUNI (IDW IDX)  =   SPACE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  R02-TCHK-STATE    (IDX)
                                               R02-TSTDATA-STATE (IDX)
                                               R02-TEDDATA-STATE (IDX)
               ELSE 
                   MOVE    WIDGET-NORMAL   TO  R02-TCHK-STATE    (IDX)
                                               R02-TSTDATA-STATE (IDX)
                                               R02-TEDDATA-STATE (IDX)
               END-IF    
               MOVE    SPA-GMN-TSTDATA     (IDW IDX)
                                           TO  R02-TSTDATA    (IDX)
               MOVE    SPA-GMN-TEDDATA     (IDW IDX) 
                                           TO  R02-TEDDATA    (IDX)
               MOVE    SPA-GMN-TKENSU      (IDW IDX)
                                           TO  R02-TKENSU     (IDX)
               MOVE    SPA-GMN-TPAGE       (IDW IDX)
                                           TO  R02-TPAGE      (IDX)
               MOVE    SPA-GMN-TCHK-VALUE  (IDW IDX)
                                           TO  R02-TCHK-VALUE (IDX) 
               MOVE    SPA-GMN-TSYURUINAME (IDW IDX)
                                           TO  R02-TCHK-LABEL (IDX)
               IF      SPA-GMN-TPRTFLG     (IDW IDX)
                                           =   "1"
                   MOVE    "済"            TO  R02-TPRTFLG        (IDX)
                   MOVE    "blue"          TO  R02-TPRTFLG-STYLE  (IDX)
               ELSE
                   MOVE    "black"         TO  R02-TPRTFLG-STYLE  (IDX)
               END-IF
               IF    ( SPA-GMN-SYOKBN      =   "F"   )
               AND   ( SPA-GMN-TCHK-VALUE  (IDW IDX)
                                           =   "T"   )
                   MOVE    "blue-bar"      TO  R02-TCHK-STYLE   (IDX)
                   MOVE    "blue-rev"      TO  R02-TPAGE-STYLE  (IDX)
                                               R02-TKENSU-STYLE (IDX)
               ELSE
                   IF    ( SPA-NAI-THKNJANUM-ERR  (IDW IDX)
                                               =   "1"   )
                       MOVE    "red-bar"   TO  R02-TCHK-STYLE   (IDX)
                       MOVE    "black"     TO  R02-TPAGE-STYLE  (IDX)
                                               R02-TKENSU-STYLE (IDX)
                   ELSE
                       MOVE    "black"     TO  R02-TCHK-STYLE   (IDX)
                                               R02-TPAGE-STYLE  (IDX)
                                               R02-TKENSU-STYLE (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-PRTJYUN1        =   "06" )
           AND   ( SPA-GMN-PAGE-MAX        >   1    )
               MOVE    WIDGET-NORMAL       TO  R02-B13-STATE
           ELSE 
               MOVE    WIDGET-INSENSITIVE  TO  R02-B13-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
         EVALUATE    SPA-GMN-CUR
             WHEN    1
                    MOVE  "SYOKBN"     TO  MCP-WIDGET
             WHEN    2
                    MOVE  "PREFKBN"    TO  MCP-WIDGET
             WHEN    3
                    MOVE  "PRTJYUN"    TO  MCP-WIDGET
             WHEN   200
                    MOVE  "PRTKBN"     TO  MCP-WIDGET
             WHEN    5
                    MOVE  "STDATA01"   TO  MCP-WIDGET
             WHEN    6
                    MOVE  "EDDATA01"   TO  MCP-WIDGET
             WHEN    4
                    MOVE  "CHK01"      TO  MCP-WIDGET
             WHEN    8
                    MOVE  "STDATA02"   TO  MCP-WIDGET
             WHEN    9
                    MOVE  "EDDATA02"   TO  MCP-WIDGET
             WHEN    7
                    MOVE  "CHK02"      TO  MCP-WIDGET
             WHEN    11
                    MOVE  "STDATA03"   TO  MCP-WIDGET
             WHEN    12
                    MOVE  "EDDATA03"   TO  MCP-WIDGET
             WHEN    10
                    MOVE  "CHK03"      TO  MCP-WIDGET
             WHEN    14
                    MOVE  "STDATA04"     TO  MCP-WIDGET
             WHEN    15
                    MOVE  "EDDATA04"     TO  MCP-WIDGET
             WHEN    13
                    MOVE  "CHK04"       TO  MCP-WIDGET
             WHEN    17
                    MOVE  "STDATA05"     TO  MCP-WIDGET
             WHEN    18
                    MOVE  "EDDATA05"     TO  MCP-WIDGET
             WHEN    16
                    MOVE  "CHK05"       TO  MCP-WIDGET
             WHEN    20
                    MOVE  "STDATA06"     TO  MCP-WIDGET
             WHEN    21
                    MOVE  "EDDATA06"     TO  MCP-WIDGET
             WHEN    19
                    MOVE  "CHK06"       TO  MCP-WIDGET
             WHEN    23
                    MOVE  "STDATA07"     TO  MCP-WIDGET
             WHEN    24
                    MOVE  "EDDATA07"     TO  MCP-WIDGET
             WHEN    22
                    MOVE  "CHK07"       TO  MCP-WIDGET
             WHEN    26
                    MOVE  "STDATA08"     TO  MCP-WIDGET
             WHEN    27
                    MOVE  "EDDATA08"     TO  MCP-WIDGET
             WHEN   25
                    MOVE  "CHK08"       TO  MCP-WIDGET
             WHEN    29
                    MOVE  "STDATA09"     TO  MCP-WIDGET
             WHEN    30
                    MOVE  "EDDATA09"     TO  MCP-WIDGET
             WHEN    28
                    MOVE  "CHK09"       TO  MCP-WIDGET
             WHEN    32
                    MOVE  "STDATA10"    TO  MCP-WIDGET
             WHEN    33
                    MOVE  "EDDATA10"    TO  MCP-WIDGET
             WHEN    31
                    MOVE  "CHK10"      TO  MCP-WIDGET
             WHEN    35
                    MOVE  "STDATA11"    TO  MCP-WIDGET
             WHEN    36
                    MOVE  "EDDATA11"    TO  MCP-WIDGET
             WHEN    34
                    MOVE  "CHK11"      TO  MCP-WIDGET
             WHEN    38
                    MOVE  "STDATA12"    TO  MCP-WIDGET
             WHEN    39
                    MOVE  "EDDATA12"    TO  MCP-WIDGET
             WHEN    37
                    MOVE  "CHK12"      TO  MCP-WIDGET
             WHEN    41
                    MOVE  "STDATA13"    TO  MCP-WIDGET
             WHEN    42
                    MOVE  "EDDATA13"    TO  MCP-WIDGET
             WHEN    40
                    MOVE  "CHK13"      TO  MCP-WIDGET
             WHEN    44
                    MOVE  "STDATA14"    TO  MCP-WIDGET
             WHEN    45
                    MOVE  "EDDATA14"    TO  MCP-WIDGET
             WHEN    43
                    MOVE  "CHK14"      TO  MCP-WIDGET
             WHEN    47
                    MOVE  "STDATA15"    TO  MCP-WIDGET
             WHEN    48
                    MOVE  "EDDATA15"    TO  MCP-WIDGET
             WHEN    46
                    MOVE  "CHK15"      TO  MCP-WIDGET
             WHEN    50
                    MOVE  "STDATA16"    TO  MCP-WIDGET
             WHEN    51
                    MOVE  "EDDATA16"    TO  MCP-WIDGET
             WHEN    49
                    MOVE  "CHK16"      TO  MCP-WIDGET
             WHEN    53
                    MOVE  "STDATA17"     TO  MCP-WIDGET
             WHEN    54
                    MOVE  "EDDATA17"     TO  MCP-WIDGET
             WHEN    52
                    MOVE  "CHK17"      TO  MCP-WIDGET
             WHEN    56
                    MOVE  "STDATA18"     TO  MCP-WIDGET
             WHEN    57
                    MOVE  "EDDATA18"     TO  MCP-WIDGET
             WHEN    55
                    MOVE  "CHK18"      TO  MCP-WIDGET
             WHEN    59
                    MOVE  "STDATA19"     TO  MCP-WIDGET
             WHEN    60
                    MOVE  "EDDATA19"     TO  MCP-WIDGET
             WHEN    58
                    MOVE  "CHK19"      TO  MCP-WIDGET
             WHEN    62
                    MOVE  "STDATA20"    TO  MCP-WIDGET
             WHEN    63
                    MOVE  "EDDATA20"    TO  MCP-WIDGET
             WHEN    61
                    MOVE  "CHK20"       TO  MCP-WIDGET
             WHEN    65
                    MOVE  "STDATA21"    TO  MCP-WIDGET
             WHEN    66
                    MOVE  "EDDATA21"    TO  MCP-WIDGET
             WHEN    64
                    MOVE  "CHK21"       TO  MCP-WIDGET
             WHEN    68
                    MOVE  "STDATA22"    TO  MCP-WIDGET
             WHEN    69
                    MOVE  "EDDATA22"    TO  MCP-WIDGET
             WHEN    67
                    MOVE  "CHK22"       TO  MCP-WIDGET
             WHEN    71
                    MOVE  "STDATA23"    TO  MCP-WIDGET
             WHEN    72
                    MOVE  "EDDATA23"    TO  MCP-WIDGET
             WHEN    70
                    MOVE  "CHK23"       TO  MCP-WIDGET
             WHEN    74
                    MOVE  "STDATA24"    TO  MCP-WIDGET
             WHEN    75
                    MOVE  "EDDATA24"    TO  MCP-WIDGET
             WHEN    73
                    MOVE  "CHK24"       TO  MCP-WIDGET
             WHEN    77
                    MOVE  "STDATA25"    TO  MCP-WIDGET
             WHEN    78
                    MOVE  "EDDATA25"    TO  MCP-WIDGET
             WHEN    76
                    MOVE  "CHK25"       TO  MCP-WIDGET
             WHEN    80
                    MOVE  "STDATA26"    TO  MCP-WIDGET
             WHEN    81
                    MOVE  "EDDATA26"    TO  MCP-WIDGET
             WHEN    79
                    MOVE  "CHK26"       TO  MCP-WIDGET
             WHEN    83
                    MOVE  "STDATA27"     TO  MCP-WIDGET
             WHEN    84
                    MOVE  "EDDATA27"     TO  MCP-WIDGET
             WHEN    82
                    MOVE  "CHK27"      TO  MCP-WIDGET
             WHEN    86
                    MOVE  "STDATA28"     TO  MCP-WIDGET
             WHEN    87
                    MOVE  "EDDATA28"     TO  MCP-WIDGET
             WHEN    85
                    MOVE  "CHK28"      TO  MCP-WIDGET
             WHEN    89
                    MOVE  "STDATA29"     TO  MCP-WIDGET
             WHEN    90
                    MOVE  "EDDATA29"     TO  MCP-WIDGET
             WHEN    88
                    MOVE  "CHK29"      TO  MCP-WIDGET
             WHEN    92
                    MOVE  "STDATA30"    TO  MCP-WIDGET
             WHEN    93
                    MOVE  "EDDATA30"    TO  MCP-WIDGET
             WHEN    91
                    MOVE  "CHK30"       TO  MCP-WIDGET
             WHEN    95
                    MOVE  "STDATA31"    TO  MCP-WIDGET
             WHEN    96
                    MOVE  "EDDATA31"    TO  MCP-WIDGET
             WHEN    94
                    MOVE  "CHK31"       TO  MCP-WIDGET
             WHEN    98
                    MOVE  "STDATA32"    TO  MCP-WIDGET
             WHEN    99
                    MOVE  "EDDATA32"    TO  MCP-WIDGET
             WHEN    97
                    MOVE  "CHK32"       TO  MCP-WIDGET
             WHEN    101
                    MOVE  "STDATA33"    TO  MCP-WIDGET
             WHEN    102
                    MOVE  "EDDATA33"    TO  MCP-WIDGET
             WHEN    100
                    MOVE  "CHK33"       TO  MCP-WIDGET
             WHEN    104
                    MOVE  "STDATA34"    TO  MCP-WIDGET
             WHEN    105
                    MOVE  "EDDATA34"    TO  MCP-WIDGET
             WHEN    103
                    MOVE  "CHK34"       TO  MCP-WIDGET
             WHEN    107
                    MOVE  "STDATA35"    TO  MCP-WIDGET
             WHEN    108
                    MOVE  "EDDATA35"    TO  MCP-WIDGET
             WHEN    106
                    MOVE  "CHK35"       TO  MCP-WIDGET
             WHEN    110
                    MOVE  "STDATA36"    TO  MCP-WIDGET
             WHEN    111
                    MOVE  "EDDATA36"    TO  MCP-WIDGET
             WHEN    109
                    MOVE  "CHK36"       TO  MCP-WIDGET
             WHEN    180
                    MOVE  "PRTJYUN1"   TO  MCP-WIDGET
             WHEN    199
                    MOVE  "B12"        TO  MCP-WIDGET   
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
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"            TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "印刷するレセプトがありません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE  "印刷用パラメタファイルが作成できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "一括出力のとき入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "個別出力のとき入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "印刷するレセプトが選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "前頁はありません"         TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "次頁はありません"         TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "入力順印刷を指定のとき入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    "レセプト印刷またはレセ電処理中です"
                                                       TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RERR
           MOVE    SPA-ERRCD            TO  RERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  RERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "RERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1003"
                   MOVE    "件数印刷をします"        TO  WRK-RIDMSG
               WHEN    "6666"
                   MOVE    "レセプト印刷中です"      TO  WRK-RIDMSG
               WHEN    "8888"
                   MOVE    "処理が正常に終了しました"
                                                     TO  WRK-RIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RID1
           MOVE    SPA-RIDCD            TO  RID1-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID1-ID1MSG
           MOVE    "戻る"               TO  RID1-B01-LABEL
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "RID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-RIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ３表示処理
      *****************************************************************
       530-RID3SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD3
               WHEN    "1001"
                   MOVE    "全件印刷をします"        TO  WRK-RIDMSG
               WHEN    "1002"
                   MOVE    "指定印刷をします"        TO  WRK-RIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RID3
           MOVE    SPA-RIDCD3           TO  RID3-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID3-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "RID3"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID3"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-RID3SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YM   (1:5)  NOT NUMERIC
               INSPECT     WRK-YM       REPLACING   ALL " "
                                                    BY  "0"
               MOVE    WRK-YM(1:1)      TO  WRK-WGO
               MOVE    WRK-YM(2:2)      TO  WRK-WYY
               MOVE    WRK-YM(5:2)      TO  WRK-WMM
           ELSE
               MOVE    WRK-YM(1:5)      TO  WRK-WYMD (1:5)
           END-IF
           MOVE    1                    TO  WRK-WDD
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG1
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票管理ファイル読込
      *****************************************************************
       900-RECE07-READ-SEC             SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票管理ファイル読込（ＫＥＹ２）
      *****************************************************************
       900-RECE07-NEXT-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE07
               MOVE    MCPDATA-REC         TO  RECE07-REC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           .
       900-RECE07-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      * 
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
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
      *    保険者マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-PUT-WINDOW-EXT.
           EXIT.
