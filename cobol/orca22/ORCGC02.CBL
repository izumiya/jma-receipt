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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGC02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 病名登録（Ｃ０２）
      *  管理者            : 
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/04/10  保険組合せ削除区分＝１
      *                                     読み飛ばし
      *  01.00.02    MCC-多々納   01/04/18  終了時の更新確認等
      *  01.00.03    MCC-藤原     01/06/06  保険病名フラグの追加
      *  01.00.04    MCC-多々納   01/07/02  日付画面用チェックサブ追加
      *  01.02.01    MCC-藤原     01/08/27  画面修正による項目の追加  
      *  01.02.02    MCC-藤原     01/09/11  転記のチェック修正    
      *  01.02.03    MCC-藤原     01/09/11  選択番号の編集    
      *  01.02.04    MCC-藤原     01/09/12  項目編集時に開始日順に
      *                                     並び替えを行う    
      *  01.02.05    MCC-藤原     01/09/13  基本病名コードの編集追加    
      *  01.03.01    MCC-藤原     01/09/20  JOINの修正      
      *  01.03.02    MCC-藤原     01/10/11  接頭語のみのとき文字数を
      *                                     ゼロにする      
      *  01.03.03    MCC-藤原     01/10/17  疑いフラグ変更のとき
      *                                     病名編集フラグを空白にする
      *                                     転記区分の修正（４→８）       
      *  01.04.01    MCC-藤原     01/11/05  患者番号、診療科を入力項目
      *                                     に変更
      *                                     C01の削除
      *  01.04.02    MCC-藤原     01/11/13  病名コードを７桁に変更     
      *  01.05.01    MCC-藤原     01/11/21  ORCMCPSUBへ変更
      *  01.05.02    MCC-藤原     01/11/27  ボタン・日付の変更     
      *  01.06.01    MCC-藤原     01/12/06  自院病名マスタの追加     
      *  01.06.02    MCC-藤原     01/12/13  疾患区分短縮名の追加     
      *                                     修飾語コードの入力方法の変更    
      *  01.06.03    MCC-藤原     01/12/18  病名のみ入力のとき検索処理を行う     
      *                                     病名コード未入力のとき病名へ移動    
      *  01.06.04    MCC-藤原     02/01/08  病名入力コードの追加     
      *  01.06.05    MCC-藤原     02/01/31  保険組合せの表示の修正     
      *  01.06.06    MCC-藤原     02/02/05  最終受診科の表示を追加     
      *                                     単科のとき初期表示をして入力はしない
      *                                     ようにする     
      *  01.06.07    MCC-藤原     02/02/18  診療開始日と保険組合せの適用日の
      *                                     チェックの追加     
      *  01.06.08    MCC-藤原     02/02/20  慢性疾患の短縮名（０５）の
      *                                     短縮名の修正     
      *  01.06.09    MCC-藤原     02/03/05  転帰日付のチェック追加
      *  01.06.10    MCC-藤原     02/04/04  自院病名の読み込みのＫＥＹ変更
      *  01.06.11    MCC-藤原     02/04/05  自院病名検索画面の追加
      *  01.06.12    MCC-藤原     02/04/11  主病名フラグの重複チェック削除
      *                                     8002"の疑い"の病名コードのとき
      *                                     疑いフラグに１をセット
      *  01.06.13    MCC-藤原     02/05/07  病名コードが削除されるように修正 
      *  01.06.14    MCC-藤原     02/05/08  半角チェック追加
      *                                     転帰日と転帰区分のチェック追加 
      *  01.06.15    MCC-多々納   02/05/08  半角チェック時、半角空白を
      *                                     全角空白へ置換えの追加
      *  01.06.16    MCC-藤原     02/05/09  自院コードのとき病名・カルテ病名に
      *                                     入力できるように修正 
      *                                     同一病名があるときエラーにする
      *  01.06.17    MCC-多々納   02/06/05  患者氏名での検索を追加
      *  01.06.18    MCC-多々納   02/06/21  労災保険追加
      *  01.06.19    MCC-小豆沢   02/06/26  皮膚科特定疾患（３）をコンボから削除
      *  01.06.20    MCC-藤原     02/07/05  病名コード空白の時開始日に移動する
      *  01.06.21    NACL-多々納  02/07/19  患者検索エラーメッセージ修正
      *
      *  01.07.01    NACL-藤原    02/08/20  新傷病名マスタ対応
      *  01.07.02    NACL-藤原    02/08/28  労災のとき保険組合せの名称の修正
      *  01.07.03    NACL-藤原    02/09/17  自賠責とき保険組合せの名称の修正
      *  01.07.04    NACL-藤原    02/10/24  戻り先にｋ０２Ｎを追加
      *  01.07.05    NACL-藤原    02/11/19  編集病名のときの接頭語のチェック
      *                                     の修正
      *                                     病名の長さチェックの修正
      *  01.07.06    NACL-藤原    02/11/25  ＳＰＡパラメタファイルの
      *                                     端末ＩＤ変更
      *  01.07.07    NACL-藤原    02/12/13  病名文字数のチェック修正
      *  01.07.08    NACL-藤原    03/02/14  入力ＣＤ検索から戻ったときの
      *                                     カルテ病名の編集を追加
      *  01.07.09    NACL-藤原    03/02/28  診療科４０に変更
      *  01.07.10    NACL-藤原    03/03/05  排他制御処理追加
      *  01.07.11    NACL-多々納  03/03/14  ファイル削除追加
      *  01.07.12    NACL-藤原    03/03/14  同一開始日の病名の表示順の修正
      *  01.07.13    NACL-藤原    03/03/26  選択番号がエラーのときクリアする
      *  01.07.14    NACL-藤原    03/04/15  診療科から病名を編集しておく
      *  01.07.15    NACL-藤原    03/05/28  変更・削除処理後の選択の解除
      *  01.07.16    NACL-藤原    03/06/03  未コード化傷病名コードのとき
      *                                     病名よりコード検索をする
      *                                     病名からのコード検索方法の修正
      *  01.07.17    NACL-藤原    03/07/03  主病名編集フラグにより主病名
      *                                     のチェックを行う
      *  01.07.18    NACL-藤原    03/07/07  病名コード入力時、病名が変更
      *                                     なければ疑い・慢性区分の変更
      *                                     をしないように修正
      *  01.07.19    NACL-藤原    03/07/15  更新時の入力コードのチェック
      *                                     チェックを追加
      *  01.07.20    NACL-藤原    03/07/20  接頭語＋入力コードの入力時の
      *                                     表示の修正
      *  01.07.21    NACL-藤原    03/08/27  病名から検索したコードがエラー
      *                                     のときの確認画面を表示
      *  01.07.22    NACL-藤原    03/08/28  病名から検索したコードが廃止病名
      *                                     のときの処理を修正
      *  01.07.23    NACL-藤原    03/09/24  転帰日が入力されている病名の
      *                                     同一病名チェックの修正
      *
      *  新患者病名マスタ対応
      *  01.08.01    NACL-藤原    03/08/28  登録数の制限をなくす
      *  01.08.02    NACL-藤原    04/02/24  入外区分の表示切替の機能追加
      *                                     病名文字数のエラー時の表示
      *  01.08.03    NACL-藤原    04/03/08  コラムリストの行制御
      *  01.08.04    NACL-藤原    04/03/09  手術歴の追加
      *  01.08.05    NACL-藤原    04/03/29  主病名の確認画面後の更新の修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ＳＰＡデータ
           SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ＳＰＡパラメタデータ
       FD  SPASCR-FILE.
       01  SPA-DATA-REC            PIC X(30000).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡパラメタデータ
       01  FILE-NAME1.
           03  FILLE               PIC X(18)   VALUE
                                               "/var/tmp/C02SPASCR".
      *****03  FILE-TERMID         PIC X(16)   VALUE   SPACE.
           03  FILE-TERMID         PIC X(64)   VALUE   SPACE.
           03  FILLER              PIC X(04)   VALUE   ".TXT".
      *
       01  WRK-DATA-REC.
           03  WRK-DATA            PIC X(30000)
                                   OCCURS  10.
      * 
       01  SPA-SCR-REC.
           02  SPA-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //SPA-PTBYO-//.
           02  WRK-PTBYOMEI-REC    OCCURS  6.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-//.
           COPY    "CPC02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-END2            PIC 9(01).
           03  FLG-UPDKBN          PIC 9(01).
           03  FLG-ROW             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *     
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
           03  IDXX                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNO           PIC 9(03). 
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-GMN-TBL         PIC X(1000).
           03  WRK-NAI-TBL         PIC X(1000). 
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(09).
               05  WRK-WIDGET2     PIC 9(01).  
      *****03  WRK-YMD             PIC X(09).
           03  WRK-YMD             PIC X(10).
           03  WRK-YMDMAX          PIC 9(01). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *         
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEI      PIC X(100).
               05  WRK-CHARTBYOMEI PIC X(100).
               05  WRK-RECECD-TBL.
                   07  WRK-RECECD  OCCURS  3
                                   PIC X(01).
               05  WRK-MOJISU      PIC 9(03).
      *
           03  WRK-HKNCOMBIMEI     PIC X(100).
           03  WRK-HKNKBN-MEI       PIC X(02).
           03  WRK-ZZ              PIC ZZZZZ.
           03  WRK-YMD-9.
               05  WRK-YY          PIC 9(04).
               05  WRK-MD          PIC 9(04).
               05  WRK-MD-R        REDEFINES   WRK-MD. 
                   07  WRK-MM      PIC 9(02).
                   07  WRK-DD      PIC 9(02). 
           03  WRK-YMD2            PIC 9(08).
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-NUM             PIC ZZZZ9.
           03  WRK-CHKFLG          PIC X(01).
           03  WRK-HAISIYMDH       PIC X(22).
           03  WRK-BANGO           PIC S9(05).
           03  WRK-YM              PIC 9(06).
           03  WRK-SYSYMD          PIC 9(08).
           03  WRK-BUNRUICD        PIC 9(03).
           03  WRK-GMN-BANGO       PIC ZZZZZ.
      *
           03  WRK-KANTAN-HENSYU-AREA.
               05  WRK-HENSYU-NUM  PIC X(40).
               05  WRK-HENSYU-NUM-G    OCCURS  5.
                   07  WRK-HENSYU-NUM-O
                                   PIC X(10).
                   07  WRK-STNUM-O PIC 9(05).
                   07  WRK-EDNUM-O PIC 9(05).
               05  WRK-STNUM       PIC X(05).
               05  WRK-EDNUM       PIC X(05).
               05  WRK-HENSYU-DATA-G.
                   07  WRK-HENSYU-DATA-O
                                   PIC X(10)   OCCURS  7.
      *
           03  WRK-STR             PIC 9(05).
      *
           03  WRK-TEKSTYMD        PIC 9(08).
           03  WRK-TEKEDYMD        PIC 9(08).
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
           03  WRK-ERR-SRYYMD      PIC X(22).
      *
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *
      *
           03  WRK-KBN-HENSYU-AREA.
               05  WRK-KBN         PIC X(04).
               05  WRK-KBNMEI      PIC X(42).
               05  WRK-KBNMEITAN   PIC X(10).                            
      *     
           03  WRK-TANDOKU-AREA.
               05  WRK-TANDOKUKBN-BYOMEICD
                                   PIC X(07).
               05  WRK-TANDOKUKBN-BYOMEI
                                   PIC X(80).                    
               05  WRK-TANDOKUKBN  PIC 9(01).
      *     
           03  WRK-BYOMEICD-KENSAKU-AREA.
               05  WRK-BYOMEICD    PIC X(07).
               05  WRK-IKOSAKICD   PIC X(07).
               05  WRK-HAISIYMD    PIC X(08).
               05  WRK-BYO-MAX     PIC 9(01).
      *                             
           03  WRK-DBPATH          PIC X(64).
      *
           03  WRK-BYOMEI-OLD      PIC X(480).
      * 
           03  WRK-SEIGYO          PIC X(01).
      *
           03  WRK-C02-BYO-ROW     PIC S9(09). 
      *
      *    未コード化傷病名コード
       01  WRK-CONS-BYOMEICD       PIC X(07)   VALUE   "0000999". 
      *                                    
      *    自院病名検索画面ＭＡＸ行
       01  WRK-CONS-C04-MAX        PIC 9(03)   VALUE   100.
      *    病名登録画面ＭＡＸ行
       01  WRK-CONS-C02-MAX        PIC 9(03)   VALUE   200.
       01  WRK-CONS-C02-HKNCOMBI-MAX
                                   PIC 9(03)   VALUE   20.
      *
       01  WRK-CONS-BYOMEICD-MAX   PIC 9(02)   VALUE   21.
      *
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    の疑い
           03  WRK-CONS-UTAGAI     PIC X(07)   VALUE   "ZZZ8002".                                     
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4. 
      *
       01  WRK-HEN-AREA.
           03  WRK-LENGTH          PIC 9(02).
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    スパ領域（ワーク）
           COPY    "COMMON-SPA"    REPLACING   //SPA-//
                                   BY          //WRK-SPA-//.
      *
      *
      *----(01.00.02) LINE ADD START ----------------------------------
           03  WRK-CIDMSG          PIC X(80).
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
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
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK2005.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名マスタ−
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診履歴マスタ−
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    病名コード検索サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *----(01.00.03) LINE ADD START ----------------------------------
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
           COPY    "CPORCMCP.INC".  
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *****COPY    "MCPAREA.INC".
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "C02.INC".
           COPY    "C03.INC".
           COPY    "C04.INC".
           COPY    "C05.INC".
           COPY    "C06.INC".
           COPY    "C07.INC".
           COPY    "C08.INC".
           COPY    "C09.INC".
           COPY    "C10.INC".
           COPY    "C11.INC".
           COPY    "CERR.INC".
           COPY    "CID1.INC".
           COPY    "CID2.INC".
           COPY    "CID3.INC".
           COPY    "CID4.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-C02FREE
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-C02SPA-SET-SEC
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
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 1003-C02SPA-OUT-SEC
           END-IF
      *
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ＳＰＡデータセット処理
      *****************************************************************
      *
       1002-C02SPA-SET-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
      *
           OPEN    INPUT               SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               GO      TO         1002-C02SPA-SET-EXT
           END-IF
      *
           INITIALIZE          SPA-SCR-REC
           INITIALIZE          WRK-DATA-REC
           MOVE    ZERO                TO  FLG-SPASCR
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL       FLG-SPASCR  =   1
               READ    SPASCR-FILE
                   AT  END
                       MOVE   1            TO  FLG-SPASCR
                   NOT AT  END
                       ADD    1            TO  IDX
                       MOVE   SPA-DATA-REC     TO  WRK-DATA (IDX)
               END-READ
           END-PERFORM
      *
           MOVE    WRK-DATA-REC        TO  SPA-SCR-REC 
      *
           CLOSE                       SPASCR-FILE
           .
       1002-C02SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-C02SPA-OUT-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
      *
           OPEN    OUTPUT              SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               DISPLAY "C02SPASCR OUT OPEN ERR:"   STS-SPASCR
      ***      STOP    " "
               GO  TO  1003-C02SPA-SET-EXT
           END-IF
           MOVE    SPACE              TO         WRK-DATA-REC
      *
           MOVE    SPA-SCR-REC        TO  WRK-DATA-REC 
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (WRK-DATA (IDX)     =   SPACE)
      *
               MOVE    WRK-DATA (IDX)      TO  SPA-DATA-REC
               WRITE                       SPA-DATA-REC
               IF      STS-SPASCR     NOT =   ZERO
                   DISPLAY "C02SPASCR OUT WRITE ERR:"   STS-SPASCR
      ***          STOP    " "
                   GO  TO  1003-C02SPA-SET-EXT
                END-IF
           END-PERFORM
      *
           CLOSE                       SPASCR-FILE
           .
       1003-C02SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "CERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *----(01.00.02) LINE ADD START ----------------------------------
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "C02    "           TO  MCP-WINDOW.
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
      **** MOVE    1               TO  SPA-GMN-C02-CUR
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"  
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-C02
                   MOVE    SPA-WORK        TO  SPA-C01KYOUTU
               END-IF
      *        予約とエラー表示の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01" 
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-U01SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      WRK-MOTOPG          =   "M01"  OR  "U02"  OR
                                               "K02"  OR  "K02N"
                   MOVE    SPA-MOTOPG          TO  SPA-C02-MOTOPG
               END-IF
      *                      
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *
               INITIALIZE              SPA-C01KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG(1:3)
               WHEN    "C03"
               WHEN    "C07"
               WHEN    "C09"
               WHEN    "C11"
                   GO      TO      300-SCREEN-EXT
               WHEN    "C04"
      *            自院病名セット処理
                   PERFORM 320-C04SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "C05"
      *            病名セット処理
                   PERFORM 320-C05SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "C10"
      *            簡単修正処理
                   PERFORM 320-C10SPASET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *----(01.00.02) LINE ADD START ----------------------------------
      *    確認画面より
           IF      SPA-MOTOPG(1:4)     =   "CID1"
               IF      SPA-CIDCD       =   "0101"
                   MOVE    SPACE           TO  SPA-CIDCD
                   IF      SPA-CID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-C02-UPDFLG
                       MOVE    "JOIN"              TO  MCP-PUTTYPE
                       PERFORM 210-BACK
                   ELSE
                       MOVE    "1"                 TO  SPA-C02-UPDFLG
                       MOVE    1                   TO  SPA-GMN-C02-CUR
                   END-IF
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0102"
      ********     MOVE    SPACE           TO  SPA-CIDCD
                   IF      SPA-CID1-FLG    =   "OK"
                       PERFORM 4502-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD
                   GO      TO      300-SCREEN-EXT
               END-IF
           END-IF              
      *
      *    病名移行確認画面より
           IF      SPA-MOTOPG(1:4)     =   "CID2"
               IF      SPA-CIDCD2          =   "0103"  OR  "0101"
      *            病名コードの移行
                   IF      SPA-CID2-FLG    =   "OK"
                       MOVE    SPA-C02-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                           TO  SPA-NAI-C02-BYOMEICD(IDZ)
                       IF      SPA-CIDCD2      =   "0103"
                           MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  SPA-GMN-C02-BYOMEICD(IDZ)
                       END-IF
                       MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
                   IF      SPA-CIDCD2      =   "0101"
                       MOVE    28              TO  SPA-GMN-C02-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD2
                   GO      TO      300-SCREEN-EXT
               END-IF    
               IF      SPA-CIDCD2          =   "0104"
      *            自院病名コードの移行
                   IF      SPA-CID2-FLG    =   "OK"
                       MOVE    SPA-C02-IKOSAKI-INDEX
                                               TO  IDZ
                       MOVE    SPA-C02-IKOSAKI-INDEX1
                                               TO  IDX2
                       MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                               TO
                                       SPA-NAI-C02-BYOMEICD-O (IDZ IDX2)
                                       BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      FLG-BYOMEI      =   ZERO
                           MOVE    BYO-BYOMEI      TO
                                       SPA-NAI-C02-BYOMEI-O (IDZ IDX2)
                       END-IF
                       MOVE    SPA-NAI-C02-BYOMEI-O (IDZ 1)
                                               TO
                                           SPA-NAI-C02-BYOMEI (IDZ)
                       PERFORM VARYING IDX2    FROM    2   BY  1
                               UNTIL   IDX2    >       21
                               OR      SPA-NAI-C02-BYOMEICD-O (IDZ IDX2)
                                                   =   SPACE
                           STRING   SPA-NAI-C02-BYOMEI (IDZ)
                                               DELIMITED  BY  SPACE
                                    SPA-NAI-C02-BYOMEI-O (IDZ IDX2)
                                               DELIMITED  BY  SPACE
                                        INTO    SPA-NAI-C02-BYOMEI (IDZ)
                           END-STRING
                       END-PERFORM        
                   END-IF
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
                   MOVE    SPACE           TO  SPA-CIDCD2
                   GO      TO      300-SCREEN-EXT
               END-IF    
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *    確認画面４より
           IF      SPA-MOTOPG(1:4)     =   "CID4"
               IF      SPA-CIDCD4      =   "0101"
                   MOVE    SPACE           TO  SPA-CIDCD4
                   IF      SPA-CID4-FLG        =   "OK"
                       PERFORM 440-SAKUJYO-SEC
      *                病名一覧
                       MOVE    "PTBYOMEI-KEY16" TO  WRK-DBPATH
                       INITIALIZE                  PTBYOMEI-REC
                       MOVE    SPA-HOSPID      TO  PTBYO-HOSPID
                       MOVE    SPA-NAI-C02-PTID
                                               TO  PTBYO-PTID
                       MOVE    SPA-GMN-C02-SRYKA
                                               TO  PTBYO-SRYKA
                       PERFORM 310-SPASET-PTBYO-HENSYU-SEC
                   END-IF
                   MOVE    1                   TO  SPA-GMN-C02-CUR  
                   MOVE    ZERO                TO  FLG-ROW
                   GO      TO      300-SCREEN-EXT
               END-IF
           END-IF              
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-C02-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
                   MOVE    WRK-SPA-PTNUM       TO  C02-PTNUM
                   PERFORM 41015-PTNUM-CHK-SEC
                   MOVE    27              TO  SPA-GMN-C02-CUR
                   IF      WRK-SPA-SRYKA       NOT =   SPACE
                        MOVE    WRK-SPA-SRYKA      TO  C02-SRYKA
                        PERFORM 41016-SRYKA-CHK-SEC
                       MOVE    1                  TO  SPA-GMN-C02-CUR
                   ELSE
                       PERFORM 330-SPASET-JYURRK-HENSYU-SEC
                       IF      SPA-GMN-C02-SRYKA-MAX =   1
                           MOVE    1               TO  SPA-GMN-C02-CUR
                       ELSE      
                           MOVE    27              TO  SPA-GMN-C02-CUR
                       END-IF
                       IF      SPA-GMN-C02-SRYKA NOT =   SPACE 
      *                    病名一覧
                           MOVE    1               TO  SPA-GMN-C02-PAGE
      *
                           MOVE    "PTBYOMEI-KEY16" TO  WRK-DBPATH
                           INITIALIZE                  PTBYOMEI-REC
                           MOVE    SPA-HOSPID      TO  PTBYO-HOSPID
                           MOVE    SPA-NAI-C02-PTID
                                                   TO  PTBYO-PTID
                           MOVE    SPA-GMN-C02-SRYKA
                                                   TO  PTBYO-SRYKA
                           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
                       ELSE
                           MOVE    27              TO  SPA-GMN-C02-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF    
           .
       3101-U01SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-C02-AREA
                                   SPA-GMN-C02-TBL
                                   SPA-NAI-C02-TBL
                                   SPA-PTBYOMEI-REC
      *
           MOVE    "1"             TO  SPA-GMN-C02-TENKIKBNL        (1)
           MOVE    "治ゆ"          TO  SPA-GMN-C02-TENKIKBNMEIL     (1)
           MOVE    "2"             TO  SPA-GMN-C02-TENKIKBNL        (2)
           MOVE    "死亡"          TO  SPA-GMN-C02-TENKIKBNMEIL     (2)
           MOVE    "3"             TO  SPA-GMN-C02-TENKIKBNL        (3)
           MOVE    "中止"          TO  SPA-GMN-C02-TENKIKBNMEIL     (3)
           MOVE    "8"             TO  SPA-GMN-C02-TENKIKBNL        (4)
           MOVE    "移行"          TO  SPA-GMN-C02-TENKIKBNMEIL     (4)
           MOVE    4               TO  SPA-GMN-C02-TENKIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-UTAGAIFLGL       (1)
           MOVE    "疑い"          TO  SPA-GMN-C02-UTAGAIFLGMEIL    (1)
           MOVE    "2"             TO  SPA-GMN-C02-UTAGAIFLGL       (2)
           MOVE    "急性"          TO  SPA-GMN-C02-UTAGAIFLGMEIL    (2)
           MOVE    "3"             TO  SPA-GMN-C02-UTAGAIFLGL       (3)
           MOVE    "急性かつ疑い"  TO  SPA-GMN-C02-UTAGAIFLGMEIL    (3)
           MOVE    3               TO  SPA-GMN-C02-UTAGAIFLG-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-SYUBYOFLGL       (1)
           MOVE    "主病名である"  TO  SPA-GMN-C02-SYUBYOFLGMEIL    (1)
           MOVE    1               TO  SPA-GMN-C02-SYUBYOFLG-MAX
      *
           MOVE    "03"            TO  SPA-GMN-C02-MANSEIKBNL       (1)
           MOVE    "皮膚科特定疾患指導管理料(１)"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (1)
           MOVE    "皮膚（１）"    TO  SPA-GMN-C02-MANSEIKBNTANL    (1)
           MOVE    "04"            TO  SPA-GMN-C02-MANSEIKBNL       (2)
           MOVE    "皮膚科特定疾患指導管理料(２)"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (2)
           MOVE    "皮膚（２）"    TO  SPA-GMN-C02-MANSEIKBNTANL    (2)
           MOVE    "05"            TO  SPA-GMN-C02-MANSEIKBNL       (3)
           MOVE    "特定疾患療養指導料／老人慢性疾患生活指導料"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (3)
           MOVE    "特定疾患"      TO  SPA-GMN-C02-MANSEIKBNTANL    (3)
      *     MOVE    "07"            TO  SPA-GMN-C02-MANSEIKBNL       (4)
      *     MOVE    "皮膚科特定疾患指導管理料(３)"
      *                             TO  SPA-GMN-C02-MANSEIKBNMEIL    (4)
      *     MOVE    "皮膚（３）"    TO  SPA-GMN-C02-MANSEIKBNTANL    (4)
           MOVE    3               TO  SPA-GMN-C02-MANSEIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-NYUGAIKBNL       (1)
           MOVE    "入院"          TO  SPA-GMN-C02-NYUGAIKBNMEIL    (1)
           MOVE    "2"             TO  SPA-GMN-C02-NYUGAIKBNL       (2)
           MOVE    "入院外"        TO  SPA-GMN-C02-NYUGAIKBNMEIL    (2)
           MOVE    2               TO  SPA-GMN-C02-NYUGAIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-HKNBYOMEIFLGL    (1)
           MOVE    "保険病名"      TO  SPA-GMN-C02-HKNBYOMEIFLGMEIL (1)
           MOVE    1               TO  SPA-GMN-C02-HKNBYOMEIFLG-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-REZEFLGL         (1)
           MOVE    "表示しない"    TO  SPA-GMN-C02-REZEFLGMEIL      (1)
           MOVE    1               TO  SPA-GMN-C02-REZEFLG-MAX
      *
           MOVE    "0"             TO  SPA-GMN-C02-SYOKBNL        (1)
           MOVE    "当月"          TO  SPA-GMN-C02-SYOKBNMEIL     (1)
           MOVE    "1"             TO  SPA-GMN-C02-SYOKBNL        (2)
           MOVE    "前月"          TO  SPA-GMN-C02-SYOKBNMEIL     (2)
           MOVE    "2"             TO  SPA-GMN-C02-SYOKBNL        (3)
           MOVE    "全体"          TO  SPA-GMN-C02-SYOKBNMEIL     (3)
           MOVE    3               TO  SPA-GMN-C02-SYOKBN-MAX
      *
           MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                       TO  SPA-GMN-C02-SYOKBN-G
      *
           MOVE    "0"             TO  SPA-GMN-C02-HNYUGAIKBNL     (1)
           MOVE    "入院・入院外"  TO  SPA-GMN-C02-HNYUGAIKBNMEIL  (1)
           MOVE    "1"             TO  SPA-GMN-C02-HNYUGAIKBNL     (2)
           MOVE    "入院"          TO  SPA-GMN-C02-HNYUGAIKBNMEIL  (2)
           MOVE    "2"             TO  SPA-GMN-C02-HNYUGAIKBNL     (3)
           MOVE    "入院外"        TO  SPA-GMN-C02-HNYUGAIKBNMEIL  (3)
           MOVE    3               TO  SPA-GMN-C02-HNYUGAIKBN-MAX
      *
           MOVE    SPA-GMN-C02-HNYUGAIKBNLST (1)
                                   TO  SPA-GMN-C02-HNYUGAIKBN-G
      *      
      *    システム日付
           IF    ( SPA-C02-MOTOPG      =   "K02"   OR  "U02"
                                                   OR  "K02N")
               MOVE    SPA-SRYYMDW     TO  SPA-GMN-C02-SYSYMD
               MOVE    SPA-SRYYMD      TO  SPA-NAI-C02-SYSYMD
           ELSE    
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-C02-SYSYMD
               MOVE    SPA-SYSYMD      TO  SPA-NAI-C02-SYSYMD
           END-IF   
      *
      *    システム日付の前月
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-C02-SYSYMD  TO  LNK-DAY6-KIJUN 
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-C02-SYSYMD-BACK
      *         
      *    主病名編集フラグ情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
           MOVE    SYS-2005-SYUBYOHENFLG
                                       TO  SPA-C02-2005-SYUBYOHENFLG
      *
      *    
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE    
               MOVE    SPACE           TO  SYS-1001-REC
           END-IF
           MOVE    SYS-1001-HOSPSBT1   TO  SPA-C02-1001-HOSPSBT1
      *
      *    診療科編集
           PERFORM 3104-SRYKA-SPA-SEC
      *
      *    患者情報（診療行為等で患者番号を入力したとき） 
           IF    ( SPA-C02-MOTOPG      =   "K02"   OR  "U02"
                                                   OR  "K02N")
           AND   ( SPA-PTNUM       NOT =   SPACE             )
               MOVE    SPA-PTNUM       TO  C02-PTNUM
               PERFORM 41015-PTNUM-CHK-SEC
               MOVE    SPA-HKNCOMBIMEI TO  SPA-GMN-C02-HKNCOMBIMEIH
               MOVE    SPA-FTNRATEMEI  TO  SPA-GMN-C02-FTNRATE
               IF      SPA-ERRCD   NOT =   SPACE
                   MOVE    27              TO  SPA-GMN-C02-CUR
                   MOVE    SPACE           TO  SPA-ERRCD
               ELSE
                   IF      SPA-SRYKA   NOT =   SPACE
                       IF      SPA-SRYKA   NOT =   SPA-GMN-C02-SRYKA    
                           MOVE    SPA-SRYKA   TO  SPA-GMN-C02-SRYKA
                           MOVE    SPA-GMN-C02-SRYKA
                                               TO  WRK-KBN
                           PERFORM 5001-SRYKA-HENSYU-SEC
                           MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-SRYKAMEI
                           MOVE    SPA-GMN-C02-SRYKA-G
                                               TO  SPA-GMN-C02-BSRYKA-G
      *                    病名一覧
                           MOVE    1           TO  SPA-GMN-C02-PAGE
      *
                           MOVE    "PTBYOMEI-KEY16"
                                               TO  WRK-DBPATH
                           INITIALIZE              PTBYOMEI-REC
                           MOVE    SPA-HOSPID  TO  PTBYO-HOSPID
                           MOVE    SPA-NAI-C02-PTID
                                               TO  PTBYO-PTID
                           MOVE    SPA-GMN-C02-SRYKA
                                               TO  PTBYO-SRYKA
                           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
                           PERFORM 310-SPASET-HKNCOMBI-HENSYU-SEC
                       END-IF
                   END-IF    
                   MOVE    1               TO  SPA-GMN-C02-CUR
               END-IF
           ELSE    
               MOVE    26              TO  SPA-GMN-C02-CUR
           END-IF   
      *
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-BSRYKA      =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    1               TO  SPA-GMN-C02-PAGE
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険組合せ情報 編集処理
      *****************************************************************
       310-SPASET-HKNCOMBI-HENSYU-SEC  SECTION.
      * 
           MOVE    ZERO                TO  IDY
                                           IDX
      *     
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *     
           PERFORM UNTIL       (IDX            =   20   )  OR
                               (FLG-HKNCOMBI   =   1    )
      *---- (01.00.01) LINE MOD START ---------------------------------
      **       PERFORM 3102-HKNCOMBI-SPA-SEC
               IF      COMB-DELKBN           =   SPACE
                   PERFORM 3102-HKNCOMBI-SPA-SEC
               END-IF
      *---- (01.00.01) LINE MOD END   ---------------------------------
      *
               MOVE    "HKNCOMBI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-COMB-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *                                 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-NAI-C02-HKN-MAX
                   OR      IDY =       40
               PERFORM 3103-HKNCOMBI-GMN-SEC
           END-PERFORM                                
           .
       310-SPASET-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター情報 編集処理
      *****************************************************************
       310-SPASET-PTBYO-HENSYU-SEC     SECTION.
      *                              
            INITIALIZE                 SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
      *
      *    指定頁に表示する開始件数
           COMPUTE WRK-STR         = ( SPA-GMN-C02-PAGE  -  1 )
                                   *   WRK-CONS-C02-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-C02-MAX
      *
      *    全科目指定のとき
           IF      SPA-GMN-C02-SRYKA   =   ZERO
               EVALUATE    WRK-DBPATH
                   WHEN    "PTBYOMEI-KEY16"
                       MOVE    "PTBYOMEI-KEY17"    TO  WRK-DBPATH
               END-EVALUATE
           END-IF
      *
      *    月による表示切替えとき
           EVALUATE    SPA-GMN-C02-SYOKBN
               WHEN    ZERO
                   MOVE    SPA-NAI-C02-SYSYMD  TO  PTBYO-SRYYMD
                   MOVE    "31"                TO  PTBYO-SRYYMD (7:2)
               WHEN    "1"
                   MOVE    SPA-NAI-C02-SYSYMD-BACK
                                               TO  PTBYO-SRYYMD
                   MOVE    "31"                TO  PTBYO-SRYYMD (7:2)
               WHEN    "2"
                   MOVE    ALL "9"             TO  PTBYO-SRYYMD
           END-EVALUATE
      *
      *    入外区分による表示切替えとき
           IF      SPA-GMN-C02-HNYUGAIKBN  =   ZERO
               MOVE    SPACE                   TO  PTBYO-NYUGAIKBN
           ELSE
               EVALUATE    SPA-GMN-C02-HNYUGAIKBN
                   WHEN    "1"
                       MOVE    "1"             TO  PTBYO-NYUGAIKBN
                   WHEN    "2"
                       MOVE    "2"             TO  PTBYO-NYUGAIKBN
               END-EVALUATE
      *
               EVALUATE    WRK-DBPATH
                   WHEN    "PTBYOMEI-KEY16"
                       MOVE    "PTBYOMEI-KEY22"   TO  WRK-DBPATH
                   WHEN    "PTBYOMEI-KEY17"
                       MOVE    "PTBYOMEI-KEY23"   TO  WRK-DBPATH
               END-EVALUATE
           END-IF
      *
           MOVE    PTBYO-SRYYMD        TO  WRK-SYSYMD
      *
           MOVE    PTBYOMEI-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL ( IDX             >=  WRK-CONS-C02-MAX ) OR
                         ( FLG-PTBYOMEI    =   1                )
               PERFORM 3101-PTBYO-SPA-SEC
      *
               MOVE    WRK-DBPATH      TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF    ( IDX                 >=  WRK-CONS-C02-MAX )  AND
                 ( FLG-PTBYOMEI        =   ZERO             )
               MOVE    1                   TO  SPA-GMN-C02-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C02-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROW
           .
       310-SPASET-PTBYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名画面編集処理
      *****************************************************************
       3101-PTBYO-SPA-SEC         SECTION.
      *
      *    レセプト対象の病名かチェック（全体以外のとき）
           IF      SPA-GMN-C02-SYOKBN   NOT =   "2"
               IF      PTBYO-SRYYMD(1:6)    =   WRK-SYSYMD (1:6)
                   CONTINUE
               ELSE
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE 
                       IF      PTBYO-TENKIYMD(1:6)  >=  WRK-SYSYMD (1:6)
                           CONTINUE
                       ELSE
                           GO  TO  3101-PTBYO-SPA-EXT
                       END-IF
                   END-IF
               END-IF
      *
               IF      PTBYO-REZEMM        >   ZERO
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    PTBYO-SRYYMD        TO  LNK-DAY6-KIJUN 
                   MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN  =   PTBYO-REZEMM    -   1    
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO  WRK-YM
                   IF      WRK-YM              >=  WRK-SYSYMD (1:6)
                       CONTINUE
                   ELSE
                       GO  TO  3101-PTBYO-SPA-EXT
                   END-IF
               END-IF
           END-IF                    
      *
      *    該当頁に表示するデータかどうか
           ADD     1               TO  IDX2
           IF      IDX2            <   WRK-STR
               GO  TO  3101-PTBYO-SPA-EXT
           END-IF
      *
           ADD     1                   TO  IDX
           IF      IDX             >   WRK-CONS-C02-MAX
               GO  TO  3101-PTBYO-SPA-EXT
           END-IF
      *
           MOVE    IDX2            TO  SPA-GMN-C02-TBANGO  (IDX)
           MOVE    PTBYO-SRYKA     TO  SPA-NAI-C02-TSRYKA  (IDX)
                                       WRK-KBN
           PERFORM 5001-SRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI      TO  SPA-GMN-C02-TSRYKA  (IDX)
           MOVE    PTBYO-RENNUM    TO  SPA-NAI-C02-TRENNUM (IDX)
           MOVE    PTBYO-BYOMEI    TO  SPA-GMN-C02-TBYOMEI (IDX)
      *
           MOVE    PTBYO-SRYYMD    TO  SPA-NAI-C02-TSRYYMD (IDX)
                                       WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-C02-TSRYYMD (IDX)
      *
           MOVE    PTBYO-REZEMM    TO  SPA-GMN-C02-TREZEMM (IDX)
      *
           MOVE    PTBYO-TENKIYMD  TO  SPA-NAI-C02-TTENKIYMD (IDX)
           IF      PTBYO-TENKIYMD  NOT =   SPACE
               MOVE    PTBYO-TENKIYMD  TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-C02-TTENKIYMD(IDX)
           ELSE
               MOVE    SPACE           TO  SPA-GMN-C02-TTENKIYMD(IDX)
           END-IF
      * 
           MOVE    PTBYO-TENKIKBN      TO  WRK-KBN
           PERFORM 5001-TENKIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TTENKIKBN (IDX)
      * 
           MOVE    PTBYO-UTAGAIFLG     TO  WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TUTAGAIFLG(IDX)
      * 
           MOVE    PTBYO-SYUBYOFLG     TO  WRK-KBN
           PERFORM 5001-SYUBYOFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TSYUBYOFLG(IDX)
      *
           MOVE    PTBYO-MANSEIKBN     TO  WRK-KBN 
           PERFORM 5001-MANSEIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEITAN       TO  SPA-GMN-C02-TMANSEIKBN (IDX)
      *
           MOVE    PTBYO-NYUGAIKBN     TO  SPA-NAI-C02-TNYUGAIKBN (IDX)
                                           WRK-KBN 
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TNYUGAIKBN (IDX)
      * 
           MOVE    PTBYO-HKNCOMBI      TO  WRK-KBN 
           PERFORM 5001-HKNCOMBI-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-THKNCOMBI (IDX)
      * 
           MOVE    PTBYO-HKNBYOMEIFLG  TO  WRK-KBN 
           PERFORM 5001-HKNBYOMEIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO
                                       SPA-GMN-C02-THKNBYOMEIFLG (IDX)
      * 
           MOVE    PTBYO-REZEFLG       TO  WRK-KBN 
           PERFORM 5001-REZEFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TREZEFLG (IDX)
      *
           MOVE    IDX             TO  SPA-GMN-C02-MAX
      *
      *
      *    初期表示のとき警告エラー表示できないのでスキップ
      *
           GO  TO  3101-PTBYO-SPA-EXT
      * 
           IF    ( PTBYO-TENKIKBN      =   SPACE   OR  "8" )
           AND   ( PTBYO-UTAGAIFLG     =   "1"     OR  "2" )
           AND   ( PTBYO-REZEFLG       =   SPACE           )
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    PTBYO-SRYYMD    TO  LNK-DAY6-KIJUN 
               MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
               EVALUATE    PTBYO-UTAGAIFLG
                   WHEN    "1"
                       MOVE    2               TO  LNK-DAY6-ZOGEN 
                   WHEN    "2"
                       MOVE    1               TO  LNK-DAY6-ZOGEN 
               END-EVALUATE         
               CALL    "ORCSDAY"       USING    STS-AREA-DAY
                                                LNK-DAY6-AREA
               IF      STS-DAY-RC1         NOT =   ZERO
                   MOVE    ZERO            TO  LNK-DAY6-KEISAN
               END-IF
               IF      LNK-DAY6-KEISAN     <=  SPA-NAI-C02-SYSYMD
                   MOVE    "0003"              TO  SPA-ERRCD
               END-IF
           END-IF           
           .
       3101-PTBYO-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せＳＰＡ画面編集処理
      *****************************************************************
       3102-HKNCOMBI-SPA-SEC          SECTION.
      *
           ADD     1                   TO  IDX
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           IF       COMB-SYU-TANSEIDONAME  NOT =   SPACE                    
               STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                             INTO              WRK-HKNCOMBIMEI
               END-STRING
           ELSE
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                             INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF                      
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    SPA-NAI-C02-PTID    TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBIMEI
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
               END-IF
           END-IF    
      *     
           MOVE    COMB-HKNCOMBINUM    TO  SPA-GMN-C02-HKNCOMBIL(IDX)
           MOVE    WRK-HKNCOMBIMEI     TO
                                       SPA-GMN-C02-HKNCOMBIMEIL (IDX)
      *                                 
           MOVE    COMB-HKNCOMBINUM    TO
                                       SPA-NAI-C02-THKNCOMBINUM (IDX)
           MOVE    WRK-HKNCOMBIMEI     TO
                                       SPA-NAI-C02-THKNCOMBIMEI (IDX)
           MOVE    COMB-TEKSTYMD       TO  SPA-NAI-C02-TEKSTYMD(IDX)
           MOVE    COMB-TEKEDYMD       TO  SPA-NAI-C02-TEKEDYMD(IDX)
      *
           MOVE    IDX                 TO  SPA-NAI-C02-HKN-MAX
                                           SPA-GMN-C02-HKNCOMBI-MAX
           .
       3102-HKNCOMBI-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ画面編集処理
      *****************************************************************
       3103-HKNCOMBI-GMN-SEC          SECTION.
      *
           ADD     1                   TO  IDY
      *      
           MOVE    SPA-NAI-C02-THKNCOMBINUM (IDX)
                                   TO  SPA-GMN-C02-THKNCOMBINUM (IDY)
           MOVE    SPA-NAI-C02-THKNCOMBIMEI (IDX)
                                   TO  SPA-GMN-C02-THKNCOMBIMEI (IDY)
           MOVE    SPA-NAI-C02-THKNCOMBINUM (IDX)
                                   TO  SPA-GMN-C02-TNUM (IDY)
      *
           ADD     1               TO  IDY 
           MOVE    SPA-NAI-C02-TEKSTYMD     (IDX)
                                   TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-C02-THKNCOMBIMEI (IDY)
      *
           MOVE    SPA-NAI-C02-TEKEDYMD     (IDX)
                                   TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO
                                   SPA-GMN-C02-THKNCOMBIMEI (IDY)(15:)
           MOVE    SPA-NAI-C02-THKNCOMBINUM (IDX)
                                   TO  SPA-GMN-C02-TNUM (IDY)
      *
           MOVE    IDY             TO  SPA-GMN-C02-HKN-MAX
      *
           .
       3103-HKNCOMBI-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科ＳＰＡ画面編集処理
      *****************************************************************
       3104-SRYKA-SPA-SEC              SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-C02-SRYKA-MAX  
      *
      *    旧総合病院以外のとき”００全科”の表示を追加
           IF      SPA-C02-1001-HOSPSBT1   =   "1"
               CONTINUE
           ELSE
               MOVE    "00"                TO  SPA-GMN-C02-SRYKAL   (1)
               MOVE    "全科"              TO  SPA-GMN-C02-SRYKAMEIL(1)
               MOVE    1                   TO  SPA-GMN-C02-SRYKA-MAX
           END-IF  
      *
      *    診療科の表示    
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-NAI-C02-SYSYMD  TO  ORC-DBYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF 
      *
           MOVE    SPA-GMN-C02-SRYKA-MAX   TO  IDX
           MOVE    ZERO                    TO  IDY
           PERFORM         UNTIL       (IDX            >=  40   )
                           OR          (FLG-SYSKANRI   =   1    )
               ADD     1                     TO  IDX
                                                 IDY
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO
                                             SPA-GMN-C02-SRYKAL   (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO
                                             SPA-GMN-C02-SRYKAMEIL(IDX)
               MOVE    SPA-GMN-C02-SRYKALST (IDX)
                                             TO  
                                             SPA-GMN-C02-BSRYKALST (IDY)
      *
               MOVE    IDX                   TO  SPA-GMN-C02-SRYKA-MAX
               MOVE    IDY                   TO  SPA-GMN-C02-BSRYKA-MAX  
      *
               MOVE    "SYSKANRI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
      *    １診療科のとき全科の表示はなし
           IF      SPA-GMN-C02-SRYKA-MAX
                                   =   2
               MOVE    SPA-GMN-C02-SRYKALST (2)
                                       TO  SPA-GMN-C02-SRYKALST (1)
               MOVE    SPACE           TO   SPA-GMN-C02-SRYKALST (2)
               MOVE    1               TO  SPA-GMN-C02-SRYKA-MAX
           END-IF
      * 
           IF      SPA-GMN-C02-SRYKA-MAX    =   1
               MOVE    SPA-GMN-C02-SRYKALST (1)
                                       TO  SPA-GMN-C02-SRYKA-G
           END-IF                                                             
      *
           .
       3104-SRYKA-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名セット処理（病名検索画面より戻り）
      *****************************************************************
       320-C05SPASET-SEC          SECTION.
      *
           IF      SPA-C01KYOUTU       =   SPACE
               GO  TO  320-C05SPASET-EXT
           END-IF
      *
      *    コードの画面表示はしない
           INITIALIZE                  SPA-GMN-C02-BYOMEICD-G
      *   
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       21
               MOVE    SPA-C01-TBYOMEICD (IDZ)
                                   TO  SPA-NAI-C02-BYOMEICD (IDZ)
                                       SPA-NAI-C02-BYOMEICD-O (IDZ 1)
               MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                   TO  BYO-BYOMEICD
               PERFORM 410111-BYOMEI-HENSYU-SEC
               MOVE    SPACE       TO  SPA-NAI-C02-BYOMEITOP (IDZ) 
           END-PERFORM
           MOVE    "1"             TO  SPA-NAI-C02-BYOMEITOP (1)
      *
           PERFORM 41021-BYOMEI-HENSYU-SEC
      *
      *   入力病名コード数カウント
           PERFORM 410111-BYOMEICDSU-HENSYU-SEC
      *
           MOVE    28                  TO  SPA-GMN-C02-CUR
           .
       320-C05SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名セット処理（自院病名検索画面より戻り）
      *****************************************************************
       320-C04SPASET-SEC          SECTION.
      *
           IF      SPA-C01KYOUTU       =   SPACE
               GO  TO  320-C04SPASET-EXT
           END-IF
      *   
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       6      
               MOVE    SPA-C01-TBYOMEICD (IDZ)
                                       TO  SPA-NAI-C02-BYOMEICD (IDZ)
                                           SPA-GMN-C02-BYOMEICD (IDZ)
               MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
               PERFORM 410112-USERBYOMEI-HENSYU-SEC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                   IF      SPA-C01-TBYOMEICD (IDZ)
                                       =   SPA-GMN-C04-INPUTCD (IDX)
                       PERFORM VARYING IDX3    FROM    1   BY  1
                               UNTIL   IDX3    >       21
                               OR      SPA-NAI-C04-BYOMEICD-O (IDX IDX3)
                                                       =   SPACE
                           MOVE    SPA-NAI-C04-BYOMEICD-O (IDX IDX3)
                                       TO  SPA-NAI-C02-BYOMEICD-O
                                                          (IDZ IDX3)
                           MOVE    SPA-NAI-C04-BYOMEI-O (IDX IDX3)
                                       TO  SPA-NAI-C02-BYOMEI-O
                                                          (IDZ IDX3)
                       END-PERFORM 
                       MOVE    SPA-NAI-C04-BYOMEI  (IDX)
                                       TO  SPA-NAI-C02-BYOMEI (IDZ)
                       MOVE    6       TO  IDX
                   END-IF   
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       6
               PERFORM 4101111-BYOMEITOP-HENSYU-SEC
           END-PERFORM
           PERFORM 41021-BYOMEI-HENSYU-SEC
           PERFORM 41022-CHARTBYOMEI-HENSYU-SEC
      *
      *   入力病名コード数カウント
           PERFORM 410111-BYOMEICDSU-HENSYU-SEC
      *
           MOVE    9                   TO  SPA-GMN-C02-CUR
           .
       320-C04SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正更新処理（簡単修正画面より戻り）
      *****************************************************************
       320-C10SPASET-SEC          SECTION.
      *
           IF      SPA-CIDCD           =   SPACE
               MOVE    1                   TO  SPA-GMN-C02-CUR
               GO  TO  320-C10SPASET-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-C02-BANGO-X
                                           SPA-CIDCD
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       7
               MOVE    SPA-C01-HENSYU-DATA-O (IDX)
                                       TO  WRK-HENSYU-DATA-O (IDX)
           END-PERFORM
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               MOVE    SPA-C01-STNUM-O (IDX)
                                       TO  WRK-STNUM-O (IDX)
               MOVE    SPA-C01-EDNUM-O (IDX)
                                       TO  WRK-EDNUM-O (IDX)
           END-PERFORM
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    更新処理
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      WRK-STNUM-O (IDXX)  >   ZERO
                   MOVE    WRK-STNUM-O (IDXX)  TO  WRK-STNUM
                   MOVE    WRK-EDNUM-O (IDXX)  TO  WRK-EDNUM
                   PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                           UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                       PERFORM 4101131-KANTAN-SYUSEI-KOUSIN-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           GO  TO  320-C10SPASET-EXT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           INITIALIZE                      SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-PTBYOMEI-REC
      *
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "PTBYOMEI-KEY16"    TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
           .
       320-C10SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終受診診療科情報 編集処理
      *****************************************************************
       330-SPASET-JYURRK-HENSYU-SEC    SECTION.
      *     
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  JYURRK-PTID
           MOVE    SPA-NAI-C02-SYSYMD  TO  JYURRK-SRYYMD
           PERFORM 900-JYURRK-INV-SEC
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-SRYKA        TO  SPA-GMN-C02-SRYKA
                                               WRK-KBN
               PERFORM 5001-SRYKA-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SRYKAMEI
           END-IF
           .
       330-SPASET-JYURRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"    TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *    前回の患者番号
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *    病名検索へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 600-C05-SYORI-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 440-SAKUJYO-GID-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-KENSAKU-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
      *    自院病名検索
               WHEN    "CLICKED"       ALSO    "B04"
                   MOVE    ZERO                TO  WRK-BUNRUICD 
                   PERFORM 490-USERBYOMEI-KENSAKU-SEC
      *    入力ＣＤへ
               WHEN    "CLICKED"       ALSO    "B16"
                   PERFORM 600-C03-SYORI-SEC
      *    コメント処理
               WHEN    "CLICKED"       ALSO    "B17"
                   PERFORM 430-FREEMSG-SEC
      *    手術歴登録処理
               WHEN    "CLICKED"       ALSO    "B18"
                   PERFORM 430-OPECOM-SEC
      *    入外区分表示切替え処理
               WHEN    "CLICKED"       ALSO    "B23"
                   PERFORM 430-HNYUGAIKBN-CHANGE-SEC
      *    表示切替え処理
               WHEN    "CLICKED"       ALSO    "B21"
                   PERFORM 430-SYOKBN-CHANGE-SEC
      *    コード表示
               WHEN    "CLICKED"       ALSO    "B22"
                   PERFORM 490-C09-SYORI-SEC
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
      *     
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41015-PTNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41016-SRYKA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "BANGO"
                   PERFORM 41011-BANGO-HEN-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYYMD"
                   PERFORM 41012-SRYYMD-CHK-SEC
                   IF      FLG-UPDKBN          =   1
                       PERFORM 450-TOUROKU-SEC
                   END-IF
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_1"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_2"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_3"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_4"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_5"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_6"
                   PERFORM 4102-BYOMEICD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "UTAGAIFLG"
                   PERFORM 4104-UTAGAI-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SYUBYOFLG"
                   PERFORM 4105-SYUBYOFLG-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "MANSEIFLG"
                   PERFORM 4106-MANSEI-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 4107-NYUGAIKBN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "HKNCOMBI"
                   PERFORM 4108-COMB-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "REZEFLG"
                   PERFORM 4109-REZEFLG-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "REZEMM"
                   PERFORM 41010-REZEMM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "TENKIKBN"
                   PERFORM 41011-TENKIKBN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "TENKIYMD"
                   PERFORM 41012-TENKIYMD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "BYOMEI"
                   PERFORM 41013-BYOMEI-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "CHARTBYOMEI"
                   PERFORM 41018-CHARTBYOMEI-CHK-SEC
      *----(01.00.03) LINE ADD START ----------------------------------
               WHEN    "ACTIVATE"      ALSO    "HKNBYOMEIFLG"
                   PERFORM 41014-HKNBYOMEIFLG-CHK-SEC
      *----(01.00.03) LINE ADD END   ----------------------------------
               WHEN    "ACTIVATE"      ALSO    "BSRYKA"
                   PERFORM 41019-BSRYKA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SYOKBN"
                   PERFORM 41022-SYOKBN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "HNYUGAIKBN"
                   PERFORM 41023-HNYUGAIKBN-CHK-SEC
      *        病名行選択
               WHEN    ANY             ALSO    "BYOMEILST"
                   PERFORM 41020-BYOMEISEL-SEC
      *        保険組合せ行選択
               WHEN    ANY             ALSO    "HKNCOMBILST"
                   PERFORM 41021-HKNCOMBISEL-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       41015-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    26                  TO  SPA-GMN-C02-CUR
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-GMN-C02-HEAD
                                       SPA-NAI-C02-HEAD
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
      *                                 
                                       SPA-GMN-C02-HKN-TBL
                                       SPA-NAI-C02-HKN-TBL
                                       SPA-GMN-C02-HKNCOMBILST-G
                                       SPA-GMN-C02-HKN-MAX
                                       SPA-GMN-C02-HKNCOMBI-MAX
                                       SPA-PTBYOMEI-REC
      *
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                       TO  SPA-GMN-C02-SYOKBN-G
           MOVE    SPA-GMN-C02-HNYUGAIKBNLST (1)
                                       TO  SPA-GMN-C02-HNYUGAIKBN-G
      *
           IF      SPA-GMN-C02-SRYKA-MAX   >   1
               INITIALIZE              SPA-GMN-C02-SRYKA-G
           ELSE
               MOVE    SPA-GMN-C02-SRYKALST (1)
                                       TO  SPA-GMN-C02-SRYKA-G
           END-IF                                      
      *
           MOVE    C02-PTNUM           TO  SPA-GMN-C02-PTNUM
      *
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-GMN-C02-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
               WHEN   10
                   MOVE    "0031"              TO  SPA-ERRCD
               WHEN    20
      *            氏名検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-C02             TO  LNK-SCRSPA
      *
                   MOVE    "C02"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
      *            氏名セット
                   MOVE    SPA-GMN-C02-PTNUM   TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
                   GO      TO     41015-PTNUM-CHK-EXT
               WHEN    OTHER
                   MOVE    "0031"          TO  SPA-ERRCD
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPTID-PTNUM         TO  SPA-GMN-C02-PTNUM
               MOVE    SPTID-PTID          TO  SPA-NAI-C02-PTID
      *
      *        患者マスター存在チェック
               MOVE    SPA-HOSPID          TO  PTINF-HOSPID
               MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           NOT =   ZERO
                   MOVE    "0031"              TO  SPA-ERRCD
               ELSE
      *
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                           GO      TO      41015-PTNUM-CHK-EXT
                   END-IF
      *
                   MOVE    PTINF-NAME        TO  SPA-GMN-C02-NAME
                   MOVE    PTINF-KANANAME    TO  SPA-GMN-C02-KANANAME
                   EVALUATE    PTINF-SEX
                       WHEN    "1" 
                           MOVE    "男"          TO  SPA-GMN-C02-SEX
                       WHEN    "2"
                           MOVE    "女"          TO  SPA-GMN-C02-SEX
                   END-EVALUATE
                   MOVE    PTINF-BIRTHDAY    TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG       TO  SPA-GMN-C02-BIRTHDAY
                   MOVE    SPA-NAI-C02-PTID  TO  SPA-LAST-PTID
                   MOVE    SPA-GMN-C02-PTNUM TO  SPA-LAST-PTNUM 
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-HKNCOMBI-HENSYU-SEC
               IF      SPA-GMN-C02-SRYKA-MAX   =   1
                   MOVE    1                   TO  SPA-GMN-C02-CUR
               ELSE      
                   PERFORM 330-SPASET-JYURRK-HENSYU-SEC
                   MOVE    27                  TO  SPA-GMN-C02-CUR
               END-IF 
      *
               IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
                   MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
               END-IF
      *
               IF      SPA-GMN-C02-SRYKA
                                       NOT =   SPACE 
      *            病名一覧
                   MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
                   MOVE    "PTBYOMEI-KEY16"    TO  WRK-DBPATH
                   INITIALIZE                      PTBYOMEI-REC
                   MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
                   PERFORM 310-SPASET-PTBYO-HENSYU-SEC
               END-IF
           END-IF    
           . 
       41015-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41016-SRYKA-CHK-SEC             SECTION.
      *
           MOVE    27                  TO  SPA-GMN-C02-CUR
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           MOVE    C02-SRYKA           TO  SPA-GMN-C02-SRYKA-G
      *
           MOVE    SPA-GMN-C02-SRYKA   TO  WRK-KBN
           PERFORM 5001-SRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SRYKAMEI
           IF      WRK-KBNMEI          =   SPACE
               MOVE    "0030"              TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
                   MOVE    SPA-GMN-C02-SRYKA-G
                                           TO  SPA-GMN-C02-BSRYKA-G
               END-IF
      *        病名一覧
               MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
               MOVE    "PTBYOMEI-KEY16"    TO  WRK-DBPATH
               INITIALIZE                  PTBYOMEI-REC
               MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
               MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
               MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
               PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
               MOVE    1                   TO  SPA-GMN-C02-CUR
           END-IF    
           .
       41016-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41011-BANGO-HEN-SEC         SECTION.
      *
           MOVE    1                   TO  SPA-GMN-C02-CUR
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-IKOFLG
                                       SPA-PTBYOMEI-REC
      *                                
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           IF      C02-BANGO       =   SPACE
               MOVE    ZERO            TO  SPA-GMN-C02-BANGO
               MOVE    2               TO  SPA-GMN-C02-CUR 
           ELSE
               IF      C02-BANGO (1:1) =   "."
                   PERFORM 410113-KANTAN-SYUSEI-SEC
                   MOVE    1                   TO  SPA-GMN-C02-CUR
                   INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                               SPA-NAI-C02-KOUMOKU
                                               SPA-NAI-C02-SELNO
                                               SPA-C02-IKOFLG
                   MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
                   IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
                       MOVE    SPA-GMN-C02-SRYKA-G
                                               TO  SPA-GMN-C02-BSRYKA-G
                   END-IF
      *
      *            番号のみ指定してあるとき   
                   IF    ( WRK-HENSYU-DATA-G   =   SPACE )
                   AND   ( SPA-ERRCD           =   SPACE )
                       MOVE    C02-BANGO       TO  SPA-GMN-C02-BANGO-X
                       INITIALIZE                  SPA-C01KYOUTU
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       5
                           MOVE    WRK-STNUM-O (IDX)   TO
                                                   SPA-C01-STNUM-O (IDX)
                           MOVE    WRK-EDNUM-O (IDX)   TO
                                                   SPA-C01-EDNUM-O (IDX)
                       END-PERFORM
      *
                       MOVE    SPACE               TO  SPA-MOTOPG
                       MOVE    SPA-C01KYOUTU       TO  SPA-WORK
                       MOVE    SPA-AREA            TO  SPA-COMMON
                       MOVE    SPA-C02FREE         TO  SPA-FREE
      *
                       MOVE    "LINK"              TO  MCP-STATUS
                       MOVE    SPACE               TO  MCP-EVENT
      *
                       CALL    "ORCGC10"           USING
                                                   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *  
                       MOVE    SPA-COMMON          TO  SPA-AREA
                       MOVE    SPA-WORK            TO  SPA-C01KYOUTU
                       MOVE    SPA-FREE            TO  SPA-C02FREE
      *
                       MOVE    "TENKIKBN"          TO  MCP-WIDGET
      *
                       MOVE    "C02"               TO  SPA-MOTOPG
                       MOVE    "C10"               TO  SPA-SAKIPG
      *
                       MOVE    "NEW"               TO  MCP-PUTTYPE
                       MOVE    "C10"               TO  MCP-WINDOW
      *
                       PERFORM 900-PUT-WINDOW
      *
                       MOVE    1                    TO  FLG-END
                       GO  TO  41011-BANGO-HEN-EXT              
                   END-IF
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    C02-BANGO       TO  SPA-GMN-C02-BANGO-X
                   END-IF
               ELSE
                   INITIALIZE                  ORCSNUMAREA
                   MOVE    C02-BANGO       TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF      SNUM-RC             =   ZERO
                       MOVE    SNUM-OUTNUM         TO  SPA-GMN-C02-BANGO
      *
                       COMPUTE WRK-BANGO
                                   =   SPA-GMN-C02-BANGO
                                   - ( SPA-GMN-C02-PAGE    -   1 )
                                   *   WRK-CONS-C02-MAX
      *
                       IF    ( WRK-BANGO       >   SPA-GMN-C02-MAX )
                       OR    ( WRK-BANGO       <   1               )
                           MOVE    ZERO            TO  SPA-GMN-C02-BANGO
                           MOVE    "0012"          TO  SPA-ERRCD
                       ELSE
                           MOVE    WRK-BANGO       TO  SPA-NAI-C02-SELNO     
                           PERFORM 410111-BYOMEI-SELECT-SEC     
      *
                           MOVE    SPA-NAI-C02-SELNO
                                                   TO  C02-BYO-ROW
                       END-IF
                   ELSE
                       MOVE    "0012"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       41011-BANGO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正処理
      *****************************************************************
       410113-KANTAN-SYUSEI-SEC         SECTION.
      *
           INITIALIZE                   WRK-KANTAN-HENSYU-AREA
      *
           UNSTRING    C02-BANGO (2:)   DELIMITED  BY  "/"
                                        INTO    WRK-HENSYU-NUM
                                                WRK-HENSYU-DATA-O (1)
                                                WRK-HENSYU-DATA-O (2)
                                                WRK-HENSYU-DATA-O (3)
                                                WRK-HENSYU-DATA-O (4)
                                                WRK-HENSYU-DATA-O (5)
                                                WRK-HENSYU-DATA-O (6)
                                                WRK-HENSYU-DATA-O (7)
               ON  OVERFLOW
                   MOVE    "0082"      TO  SPA-ERRCD  
                   GO  TO  410113-KANTAN-SYUSEI-EXT
           END-UNSTRING
      *
      *    入力がないとき
           IF    ( WRK-HENSYU-NUM      =   SPACE )
               MOVE    "0080"          TO  SPA-ERRCD
               GO  TO  410113-KANTAN-SYUSEI-EXT
           END-IF
      *
      *    病名番号の入力数
           UNSTRING    WRK-HENSYU-NUM  DELIMITED   BY  ","
                                       INTO    WRK-HENSYU-NUM-O (1)
                                               WRK-HENSYU-NUM-O (2)
                                               WRK-HENSYU-NUM-O (3)
                                               WRK-HENSYU-NUM-O (4)
                                               WRK-HENSYU-NUM-O (5)
               ON  OVERFLOW
                   MOVE    "0081"      TO  SPA-ERRCD  
                   GO  TO  410113-KANTAN-SYUSEI-EXT
           END-UNSTRING
      *
      *    入力番号チェック
           IF      WRK-HENSYU-NUM-O (1)    =   "*"
               IF    ( WRK-HENSYU-NUM-O (2)    NOT =   SPACE )
               OR    ( WRK-HENSYU-NUM-O (3)    NOT =   SPACE )
               OR    ( WRK-HENSYU-NUM-O (4)    NOT =   SPACE )
               OR    ( WRK-HENSYU-NUM-O (5)    NOT =   SPACE )
                   MOVE    "0081"      TO  SPA-ERRCD  
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               END-IF
               IF      SPA-GMN-C02-SYOKBN  =   "0" OR  "1"
                   MOVE    1               TO  WRK-STNUM-O (1)
                   MOVE    SPA-GMN-C02-MAX TO  WRK-EDNUM-O (1)
               ELSE
                   MOVE    "0083"          TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               END-IF
           ELSE
             PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
                   OR      SPA-ERRCD   NOT =   SPACE
               MOVE    SPACE               TO  WRK-STNUM
                                               WRK-EDNUM
               IF      WRK-HENSYU-NUM-O (IDXX) =   SPACE
                   CONTINUE
               ELSE
                   UNSTRING    WRK-HENSYU-NUM-O (IDXX)
                                           DELIMITED   BY  "-"
                                           INTO    WRK-STNUM
                                                   WRK-EDNUM
                   END-UNSTRING
      *
                   IF    ( WRK-STNUM           =   SPACE )
                   AND   ( WRK-EDNUM           =   SPACE )
                       MOVE    "0012"          TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
                   IF      WRK-STNUM       NOT =   SPACE
                       INITIALIZE              ORCSNUMAREA
                       MOVE    WRK-STNUM       TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF      SNUM-RC         =   ZERO
                           MOVE    SNUM-OUTNUM     TO  WRK-BANGO
                           COMPUTE WRK-BANGO
                                   =   WRK-BANGO
                                   - ( SPA-GMN-C02-PAGE    -   1 )
                                   *   WRK-CONS-C02-MAX
                           IF    ( WRK-BANGO       >   SPA-GMN-C02-MAX )
                           OR    ( WRK-BANGO       <   1               )
                               MOVE    "0012"          TO  SPA-ERRCD
                           ELSE
                               MOVE    WRK-BANGO   TO  WRK-STNUM-O(IDXX)
                               IF      WRK-EDNUM       =   SPACE
                                   MOVE    WRK-STNUM-O(IDXX)
                                                   TO  WRK-EDNUM-O(IDXX)
                               END-IF
                           END-IF
                       ELSE
                           MOVE    "0012"          TO  SPA-ERRCD
                           GO  TO  410113-KANTAN-SYUSEI-EXT
                       END-IF
                   END-IF
                   IF    ( WRK-EDNUM       NOT =   SPACE )
                   AND   ( SPA-ERRCD           =   SPACE )
                       INITIALIZE              ORCSNUMAREA
                       MOVE    WRK-EDNUM       TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF      SNUM-RC         =   ZERO
                           MOVE    SNUM-OUTNUM     TO  WRK-BANGO
                           COMPUTE WRK-BANGO
                                   =   WRK-BANGO
                                   - ( SPA-GMN-C02-PAGE    -   1 )
                                   *   WRK-CONS-C02-MAX
                           IF    ( WRK-BANGO       >   SPA-GMN-C02-MAX )
                           OR    ( WRK-BANGO       <   1               )
                               MOVE    "0012"      TO  SPA-ERRCD
                           ELSE
                               MOVE    WRK-BANGO   TO  WRK-EDNUM-O(IDXX)
                           END-IF
                       ELSE
                           MOVE    "0012"          TO  SPA-ERRCD
                           GO  TO  410113-KANTAN-SYUSEI-EXT
                       END-IF
                   END-IF
               END-IF
             END-PERFORM
           END-IF
      *   
           IF    ( WRK-HENSYU-DATA-G   =   SPACE )
               GO  TO  410113-KANTAN-SYUSEI-EXT
           END-IF
      *
      *    転帰区分・転帰日チェック
           IF    ( WRK-HENSYU-DATA-O (1)   =   "-"   )
           AND   ( WRK-HENSYU-DATA-O (2)   =   SPACE )
               MOVE    "-"                     TO  WRK-HENSYU-DATA-O (2)
           END-IF
      *
           IF      WRK-HENSYU-DATA-O (1)   =   SPACE OR  "-"
               IF      WRK-HENSYU-DATA-O (2)   NOT =   SPACE   AND "-"
                   IF      WRK-HENSYU-DATA-O (1)   =   SPACE
                       MOVE    "0037"              TO  SPA-ERRCD
                   ELSE
                       MOVE    "0025"              TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               IF      WRK-HENSYU-DATA-O (1) (2:)
                                           NOT =   SPACE
                   MOVE    "0024"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (1)
                                               TO  WRK-KBN        
                   PERFORM 5001-TENKIKBN-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0024"              TO  SPA-ERRCD
                   ELSE
                       IF      WRK-HENSYU-DATA-O (2)   =   SPACE
                           MOVE    SPA-NAI-C02-SYSYMD  TO
                                               WRK-HENSYU-DATA-O (2)
                       END-IF
                   END-IF
               END-IF 
           END-IF
           IF      SPA-ERRCD           NOT =   SPACE
               GO  TO  410113-KANTAN-SYUSEI-EXT
           END-IF
      *
           IF      WRK-HENSYU-DATA-O (2)   =   SPACE OR  "-"
               IF      WRK-HENSYU-DATA-O (1)   NOT =   SPACE   AND "-"
                   MOVE    "0036"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               END-IF
               IF      WRK-HENSYU-DATA-O (2)   =   "-"
                   MOVE    "00000000"          TO  WRK-HENSYU-DATA-O (2)
               END-IF
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-HENSYU-DATA-O (2)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC             >   ZERO
                   MOVE    WRK-HENSYU-DATA-O (2)    TO  SGDAY-INDAY
               ELSE
                   EVALUATE    SNUM-SEISUU
                       WHEN    1
                       WHEN    2
                           MOVE    SPA-NAI-C02-SYSYMD  TO  WRK-YMD-9
                           MOVE    SNUM-OUTNUM         TO  WRK-DD
                       WHEN    3
                           MOVE    SPA-NAI-C02-SYSYMD  TO  WRK-YMD-9
                           MOVE    SNUM-OUTNUM         TO  WRK-MD
                       WHEN    4
                           MOVE    SNUM-OUTNUM         TO  WRK-MD
                           IF    ( WRK-MM              =   12   )
                           AND   ( SPA-NAI-C02-SYSYMD (5:2)
                                                       =   "01" )
                               MOVE    SPA-NAI-C02-SYSYMD-BACK (1:4)
                                                       TO  WRK-YY
                           ELSE
                               MOVE    SPA-NAI-C02-SYSYMD (1:4)
                                                       TO  WRK-YY
                           END-IF
                       WHEN    OTHER
                           MOVE    WRK-HENSYU-DATA-O (2)
                                                       TO  WRK-YMD-9
                   END-EVALUATE
                   MOVE    WRK-YMD-9       TO  SGDAY-INDAY
               END-IF
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   IF      WRK-HENSYU-DATA-O (1)   =   SPACE
                       MOVE    "0037"          TO  SPA-ERRCD
                   END-IF    
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE       
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               END-IF
               MOVE    SGDAY-SDAY          TO  WRK-HENSYU-DATA-O (2)
           END-IF
      *
      *    疑いフラグチェック
           IF      WRK-HENSYU-DATA-O (3)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (3) (2:)
                                           NOT =   SPACE
                   MOVE    "0018"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (3)   TO  WRK-KBN
                   PERFORM 5001-UTAGAIFLG-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0018"              TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    主病名フラグチェック
           IF      WRK-HENSYU-DATA-O (4)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (4) (2:)
                                           NOT =   SPACE
                   MOVE    "0019"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (4)   TO  WRK-KBN
                   PERFORM 5001-UTAGAIFLG-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0019"              TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    疾患区分チェック
           IF      WRK-HENSYU-DATA-O (5)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (5) (3:)
                                           NOT =   SPACE
                   MOVE    "0021"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (5)   TO  WRK-KBN
                   PERFORM 5001-MANSEIKBN-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0021"              TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    入外区分チェック
           IF      WRK-HENSYU-DATA-O (6)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (6) (2:)
                                           NOT =   SPACE
                   MOVE    "0022"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (6)   TO  WRK-KBN
                   PERFORM 5001-NYUGAIKBN-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0022"              TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    保険適用チェック
           IF      WRK-HENSYU-DATA-O (7)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (7) (5:)
                                           NOT =   SPACE
                   MOVE    "0009"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (7)   TO  WRK-KBN
                   PERFORM 5001-HKNCOMBI-HENSYU-SEC
                   IF      WRK-KBNMEI      =   SPACE
                       MOVE    "0009"               TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    転帰・保険適用と開始日のチェック
           IF    ( WRK-HENSYU-DATA-O (1)   NOT =   SPACE         )
           OR    ( WRK-HENSYU-DATA-O (7)   NOT =   SPACE AND "-" )
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX     FROM
                                               WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                           PERFORM 4101132-KANTAN-SYUSEI-DABURI-CHK-SEC
                           IF      SPA-ERRCD   NOT =   SPACE
                               GO  TO  410113-KANTAN-SYUSEI-EXT
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    更新処理
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      WRK-STNUM-O (IDXX)  >   ZERO
                   MOVE    WRK-STNUM-O (IDXX)  TO  WRK-STNUM
                   MOVE    WRK-EDNUM-O (IDXX)  TO  WRK-EDNUM
                   PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                           UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                       PERFORM 4101131-KANTAN-SYUSEI-KOUSIN-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           GO  TO  410113-KANTAN-SYUSEI-EXT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           INITIALIZE                      SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-PTBYOMEI-REC
      *
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "PTBYOMEI-KEY16"    TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
           .
       410113-KANTAN-SYUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正転帰・保険適用チェック処理
      *****************************************************************
       4101132-KANTAN-SYUSEI-DABURI-CHK-SEC         SECTION.
      *
      *    転帰と開始日のチェック
           IF      WRK-HENSYU-DATA-O (1)
                               NOT =   "-" AND SPACE  
               IF      SPA-NAI-C02-TSRYYMD (IDX)
                                       >    WRK-HENSYU-DATA-O (2)
                   MOVE    "0084"      TO  SPA-ERRCD
                   GO  TO  4101132-KANTAN-SYUSEI-DABURI-CHK-EXT
               END-IF
           END-IF
      *    保険適用と開始日のチェック
           IF     WRK-HENSYU-DATA-O (7)  
                               NOT =   "-" AND SPACE  
               IF    ( SPA-NAI-C02-TSRYYMD (IDX)
                                   <=   WRK-TEKEDYMD )
               AND   ( SPA-NAI-C02-TSRYYMD (IDX) 
                                   >=   WRK-TEKSTYMD )
                   CONTINUE
               ELSE    
                   MOVE    "0085"     TO  SPA-ERRCD
                   MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                               TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-ERR-SRYYMD
                   MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
                   GO  TO  4101132-KANTAN-SYUSEI-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    マスタとのチェック
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                       TO  PTBYO-BYOMEI
           MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                       TO  PTBYO-NYUGAIKBN
           IF      WRK-HENSYU-DATA-O (6)   NOT =   SPACE 
               IF      WRK-HENSYU-DATA-O (6)   =   "-"
                   MOVE    SPACE                   TO  PTBYO-NYUGAIKBN
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (6)   TO  PTBYO-NYUGAIKBN
               END-IF
           END-IF     
           IF      PTBYO-NYUGAIKBN     =   SPACE 
               MOVE    "PTBYOMEI-KEY13"    TO  WRK-DBPATH
           ELSE
               MOVE    "PTBYOMEI-KEY21"    TO  WRK-DBPATH
           END-IF    
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C02-TSRYYMD (IDX)   =   PTBYO-SRYYMD
               AND     SPA-NAI-C02-TSRYKA  (IDX)   =   PTBYO-SRYKA
               AND     SPA-NAI-C02-TRENNUM (IDX)   =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   IF      WRK-HENSYU-DATA-O (2)   NOT =   SPACE
                       IF      SPA-NAI-C02-TSRYYMD (IDX)
                                               <   PTBYO-SRYYMD
                           IF      WRK-HENSYU-DATA-O (2)   =   "-"
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     WRK-HENSYU-DATA-O (2)
                                                   >   PTBYO-SRYYMD
                                   MOVE    "0038"          TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
                   END-IF    
               END-IF
      *
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      SPA-ERRCD           =   "0038"
               MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-ERR-SRYYMD
               MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                           TO  WRK-ERR-BYOMEI
           END-IF
           .
       4101132-KANTAN-SYUSEI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正更新処理
      *****************************************************************
       4101131-KANTAN-SYUSEI-KOUSIN-SEC         SECTION.
      *
           INITIALIZE                      PTBYO-KEY
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "PTBYOMEI-KEY"      TO  WRK-DBPATH
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  410113-KANTAN-SYUSEI-EXT
           END-IF
      *
      *    転帰区分・転帰日
           EVALUATE    WRK-HENSYU-DATA-O (1)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-TENKIKBN
                                       PTBYO-TENKIYMD
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (1)
                                   TO  PTBYO-TENKIKBN
                   MOVE    WRK-HENSYU-DATA-O (2)
                                   TO  PTBYO-TENKIYMD
           END-EVALUATE      
      *
      *    疑いフラグ
           EVALUATE    WRK-HENSYU-DATA-O (3)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-UTAGAIFLG
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (3)
                                   TO  PTBYO-UTAGAIFLG
           END-EVALUATE      
      *
      *    主病名フラグ
           EVALUATE    WRK-HENSYU-DATA-O (4)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-SYUBYOFLG
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (4)
                                   TO  PTBYO-SYUBYOFLG
           END-EVALUATE      
      *
      *    疾患区分
           EVALUATE    WRK-HENSYU-DATA-O (5)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    ZERO    TO  PTBYO-MANSEIKBN
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (5)
                                   TO  PTBYO-MANSEIKBN
           END-EVALUATE      
      *
      *    入外区分
           EVALUATE    WRK-HENSYU-DATA-O (6)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-NYUGAIKBN
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (6)
                                   TO  PTBYO-NYUGAIKBN
           END-EVALUATE      
      *
      *    保険適用
           EVALUATE    WRK-HENSYU-DATA-O (7)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-HKNCOMBI
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (7)
                                   TO  PTBYO-HKNCOMBI
           END-EVALUATE      
      *
      *    レセ電エラーフラグ
      *    （主病名・疑いフラグに変更があったとき）
           IF    ( WRK-HENSYU-DATA-O (3) =   SPACE )
           AND   ( WRK-HENSYU-DATA-O (4) =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPACE       TO  PTBYO-RECEDENFLG
               IF      WRK-HENSYU-DATA-O (4) =   SPACE
                   CONTINUE
               ELSE
                   IF      PTBYO-BYOMEIHENFLG          =   "1"
                       EVALUATE    PTBYO-SYUBYOFLG       
                           WHEN    "1"
                               IF      PTBYO-BYOMEIMOJI    >   17
                                   MOVE    "1" TO  PTBYO-RECEDENFLG       
                               END-IF
                           WHEN    SPACE
                               IF      PTBYO-BYOMEIMOJI    >   20
                                   MOVE    "1" TO  PTBYO-RECEDENFLG       
                               END-IF
                       END-EVALUATE
                   END-IF
               END-IF
      *
               IF      WRK-HENSYU-DATA-O (3) =   SPACE
                   CONTINUE
               ELSE
                   IF    ( PTBYO-RECEDENFLG    =   SPACE ) 
                   AND   ( PTBYO-UTAGAIFLG         
                                           NOT =   SPACE )
                       MOVE    SPACE       TO  WRK-HENFLG1
                                               WRK-HENFLG2
      *                病名文字数を計算
                       COMPUTE WRK-MOJISU      =   PTBYO-BYOMEIMOJI       
                                                   *   2
                       IF      PTBYO-UTAGAIFLG     =   "1" OR  "3"
                           IF      WRK-MOJISU      <   8
                               MOVE    1               TO  IDZ
                           ELSE    
                               COMPUTE IDZ     =   WRK-MOJISU    -   7
                           END-IF    
                           PERFORM VARYING IDY FROM    IDZ BY  2
                                   UNTIL   IDY >       WRK-MOJISU
                               IF      PTBYO-BYOMEI (IDY:2) =   "疑"
                                   MOVE    "1"          TO  WRK-HENFLG1
                                   MOVE    WRK-MOJISU   TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF                
                       IF      PTBYO-UTAGAIFLG     =   "2" OR  "3"
                           PERFORM VARYING IDY FROM    1   BY  2
                                   UNTIL   IDY >       WRK-MOJISU
                               IF      PTBYO-BYOMEI (IDY:4) =  "急性"
                                   MOVE    "1"         TO  WRK-HENFLG2
                                   MOVE    WRK-MOJISU  TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF
      *
      *                疑いフラグがセットされているとき
                       IF    ( PTBYO-UTAGAIFLG     =   "1" OR  "3" )
                       AND   ( WRK-HENFLG1         =   SPACE       )                         
                           COMPUTE WRK-MOJISU  =
                                              WRK-MOJISU   +
                                              WRK-CONS-UTAGAI-MOJISU
                           IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                               EVALUATE    PTBYO-SYUBYOFLG       
                                   WHEN    "1"
                                       IF      WRK-MOJISU  >   17
                                           MOVE    "1"     TO
                                                   PTBYO-RECEDENFLG       
                                       END-IF
                                   WHEN    SPACE
                                       IF      WRK-MOJISU  >   20
                                           MOVE    "1"     TO
                                                   PTBYO-RECEDENFLG       
                                       END-IF
                               END-EVALUATE
                           END-IF
                       END-IF    
                       IF    ( PTBYO-UTAGAIFLG       
                                                   =   "2" OR  "3" )
                       AND   ( WRK-HENFLG2         =   SPACE       )                         
                           COMPUTE WRK-MOJISU  =
                                              WRK-MOJISU   +
                                              WRK-CONS-KYUSEI-MOJISU
                           IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                               EVALUATE    PTBYO-SYUBYOFLG       
                                   WHEN    "1"
                                       IF      WRK-MOJISU  >   17
                                           MOVE    "1"     TO
                                                   PTBYO-RECEDENFLG       
                                       END-IF
                                   WHEN    SPACE
                                       IF      WRK-MOJISU  >   20
                                           MOVE    "1"     TO
                                                   PTBYO-RECEDENFLG       
                                       END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF            
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    "DBUPDATE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "PTBYOMEI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>"  PTBYO-KEY 
           END-IF
           .
       410113-KANTAN-SYUSEI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    選択病名処理
      *****************************************************************
       410111-BYOMEI-SELECT-SEC        SECTION.
      *      
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           MOVE    SPACE               TO  WRK-HAISIYMDH
      *      
           MOVE    SPA-GMN-C02-TSRYYMD (SPA-NAI-C02-SELNO)
                                       TO  SPA-GMN-C02-SRYYMD
           MOVE    SPA-NAI-C02-TSRYYMD (SPA-NAI-C02-SELNO)
                                       TO  SPA-NAI-C02-SRYYMD
                                           SPA-NAI-C02-SRYYMD-OLD
           MOVE    SPA-NAI-C02-TRENNUM (SPA-NAI-C02-SELNO)
                                       TO  SPA-NAI-C02-RENNUM
           MOVE    SPA-NAI-C02-TSRYKA (SPA-NAI-C02-SELNO)
                                       TO  SPA-GMN-C02-BSRYKA
                                           SPA-NAI-C02-SRYKA-OLD
      *
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-RENNUM  TO  PTBYO-RENNUM
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "PTBYOMEI-KEY"      TO  WRK-DBPATH
           PERFORM 900-PTBYO-INV-SEC
      *
           MOVE    PTBYOMEI-REC        TO  SPA-PTBYOMEI-REC
      *
      *    入力コード
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       6
               MOVE    PTBYO-BYOMEIINPUTCD (IDZ)
                                   TO  SPA-GMN-C02-BYOMEICD (IDZ)
           END-PERFORM
      *
      *    病名コード
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       21
               MOVE    PTBYO-BYOMEICD(IDZ)
                                   TO  SPA-NAI-C02-BYOMEICD   (IDZ)
                                       SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                                       BYO-BYOMEICD
               PERFORM 410111-BYOMEI-HENSYU-SEC
               MOVE    SPACE       TO  SPA-NAI-C02-BYOMEITOP (IDZ) 
               IF      SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                                   =   WRK-CONS-BYOMEICD
                   MOVE    PTBYO-BYOMEI
                                   TO  SPA-NAI-C02-BYOMEI-O (IDZ 1)
                                       SPA-NAI-C02-BYOMEI   (IDZ)
               END-IF
           END-PERFORM
           MOVE    "1"             TO  SPA-NAI-C02-BYOMEITOP (1)
      *
           PERFORM 4101111-BYOMEI-SELECT-HENSYU-SEC              
           .
       410111-BYOMEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理
      *****************************************************************
       4101111-BYOMEI-SELECT-HENSYU-SEC      SECTION.
      *      
           MOVE    SPA-PTBYO-SRYKA    TO  SPA-GMN-C02-BSRYKA
                                          WRK-KBN
           PERFORM 5001-BSRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI         TO  SPA-GMN-C02-BSRYKAMEI
      *      
           MOVE    SPA-PTBYO-BYOMEI    TO  SPA-GMN-C02-BYOMEI
      *                                 
           MOVE    SPA-PTBYO-BYOMEIMOJI
                                       TO  SPA-NAI-C02-BYOMEIMOJI
      *                                 
           MOVE    SPA-PTBYO-BYOMEIHENFLG
                                       TO  SPA-NAI-C02-BYOMEIHENFLG
           IF      SPA-NAI-C02-BYOMEIHENFLG  =   "1" 
               MOVE    "病名編集"      TO  SPA-GMN-C02-BYOMEIHENFLG
           END-IF    
      *      
           MOVE    SPA-PTBYO-CHARTBYOMEI
                                       TO  SPA-GMN-C02-CHARTBYOMEI
      *                                 
           MOVE    SPA-PTBYO-CHARTBYOMEIMOJI
                                       TO  SPA-NAI-C02-CHARTBYOMEIMOJI
      *
           MOVE    SPA-PTBYO-UTAGAIFLG TO  SPA-GMN-C02-UTAGAIFLG
                                           WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-UTAGAIFLGMEI
      *                                                              
           MOVE    SPA-PTBYO-SYUBYOFLG TO  SPA-GMN-C02-SYUBYOFLG
                                           WRK-KBN
           PERFORM 5001-SYUBYOFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SYUBYOFLGMEI
      *                                 
           IF      SPA-PTBYO-MANSEIKBN NOT =   ZERO
               MOVE    SPA-PTBYO-MANSEIKBN TO  SPA-GMN-C02-MANSEIKBN
                                               WRK-KBN
               PERFORM 5001-MANSEIKBN-HENSYU-SEC                                
               MOVE    WRK-KBNMEI         TO  SPA-GMN-C02-MANSEIKBNMEI
           END-IF
      *                                     
           MOVE    SPA-PTBYO-NYUGAIKBN TO  SPA-GMN-C02-NYUGAIKBN
                                           WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-NYUGAIKBNMEI
      *                                 
           IF      SPA-PTBYO-HKNCOMBI  NOT =   ZERO
               MOVE    SPA-PTBYO-HKNCOMBI  TO  SPA-GMN-C02-HKNCOMBI
                                               WRK-KBN
               PERFORM 5001-HKNCOMBI-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HKNCOMBIMEI
           END-IF
      *                                     
           MOVE    SPA-PTBYO-REZEFLG   TO  SPA-GMN-C02-REZEFLG
                                           WRK-KBN
           PERFORM 5001-REZEFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-REZEFLGMEI
      *                                 
           MOVE    SPA-PTBYO-REZEMM    TO  SPA-GMN-C02-REZEMM
      *                                 
           MOVE    SPA-PTBYO-TENKIKBN  TO  SPA-GMN-C02-TENKIKBN
                                           WRK-KBN
           PERFORM 5001-TENKIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TENKIKBNMEI
      *
           MOVE    SPA-PTBYO-TENKIYMD  TO  SPA-NAI-C02-TENKIYMD
           IF      SPA-PTBYO-TENKIYMD  NOT =   SPACE
               MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-C02-TENKIYMD
           END-IF
      *                                 
           MOVE    SPA-PTBYO-HKNBYOMEIFLG
                                        TO  SPA-GMN-C02-HKNBYOMEIFLG 
                                           WRK-KBN
           PERFORM 5001-HKNBYOMEIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HKNBYOMEIFLGMEI
      *                                 
           MOVE    SPA-PTBYO-RECEDENFLG
                                       TO  SPA-NAI-C02-RECEDENFLG
      *
      *   入力病名コード数カウント
           PERFORM 410111-BYOMEICDSU-HENSYU-SEC
      *
      *    開始日へ遷移
           MOVE    9                   TO  SPA-GMN-C02-CUR
      *
           .
       4101111-BYOMEI-SELECT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力病名コード数カウント処理
      *****************************************************************
       410111-BYOMEICDSU-HENSYU-SEC      SECTION.
      *
      *   入力病名コード数カウント
           MOVE    ZERO                TO  SPA-NAI-C02-BYOMEICD-MAX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-NAI-C02-BYOMEITOP (IDX)  =   "1"                   
                   ADD     1               TO  SPA-NAI-C02-BYOMEICD-MAX
               END-IF
           END-PERFORM
      *
           .
       410111-BYOMEICDSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスタ編集処理
      *****************************************************************
       410111-BYOMEI-HENSYU-SEC          SECTION.
      *
           INITIALIZE                    SPA-NAI-C02-BYOMEI-G(IDZ)
      *
           MOVE    ZERO                  TO  FLG-BYOMEI
      *
           IF     BYO-BYOMEICD       NOT =   SPACE
               IF  (   BYO-BYOMEICD (1:3)    =   "ZZZ"    )
               AND ( ( BYO-BYOMEICD (4:1)    <   "1" ) OR 
                     ( BYO-BYOMEICD (4:1)    =   "C" )    )
      *            予約病名コード検索
                   SET     TBL-BYOMEICD-IDX  TO  1
                   SEARCH  TBL-BYOMEICD-OCC  VARYING TBL-BYOMEICD-IDX
                       AT  END
                           MOVE    1           TO  FLG-BYOMEI
                       WHEN    BYO-BYOMEICD    =
                                   TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                           MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                               TO
                                       SPA-NAI-C02-BYOMEI(IDZ)
                           MOVE    "1"         TO
                                       SPA-NAI-C02-US-BYOMEIHENFLG (IDZ)
                           MOVE    BYO-BYOMEICD
                                               TO
                                       SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                           MOVE    SPA-NAI-C02-BYOMEI (IDZ)
                                               TO
                                       SPA-NAI-C02-BYOMEI-O   (IDZ 1)
                   END-SEARCH
               ELSE
      *            病名マスタ検索
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI        =   ZERO
                       IF      BYO-BYOMEICD  =   WRK-CONS-BYOMEICD
                           MOVE    SPACE TO  SPA-NAI-C02-BYOMEI (IDZ)
                       ELSE                           
                           MOVE    BYO-BYOMEI
                                         TO  SPA-NAI-C02-BYOMEI  (IDZ)
                       END-IF    
                       MOVE    BYO-UTAGAIFLG
                                         TO  SPA-NAI-C02-UTAGAIFLG(IDZ)
                       IF      BYO-BYOMEICD  =   WRK-CONS-UTAGAI
                           MOVE    "1"   TO  SPA-NAI-C02-UTAGAIFLG(IDZ)
                       END-IF    
                       MOVE    BYO-TOKSKNCD
                                         TO  SPA-NAI-C02-TOKSKNCD  (IDZ)
                       MOVE    BYO-BYOMEICD
                                         TO
                                         SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                       MOVE    SPA-NAI-C02-BYOMEI (IDZ)
                                         TO
                                         SPA-NAI-C02-BYOMEI-O   (IDZ 1)
                       MOVE    BYO-TANDOKUKBN
                                         TO
                                         SPA-NAI-C02-TANDOKUKBN-O(IDZ 1)
                   END-IF
               END-IF
      *
               IF      FLG-BYOMEI        =   ZERO
                   MOVE    IDZ               TO  IDX3
                   PERFORM 4101111-BYOMEITOP-HENSYU-SEC
                   MOVE    SPACE         TO
                                         SPA-NAI-C02-USERBYOMEIFLG(IDZ)
               ELSE
                   MOVE    "■■■"      TO  SPA-NAI-C02-BYOMEI   (IDZ)
               END-IF
           END-IF
           .
       410111-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数病名の先頭フラグ編集処理
      *****************************************************************
       4101111-BYOMEITOP-HENSYU-SEC      SECTION.
      *
           IF      SPA-NAI-C02-BYOMEICD (IDX3) 
                                   =   SPACE
               MOVE    SPACE           TO  SPA-NAI-C02-BYOMEITOP(IDX3)
               GO  TO  4101111-BYOMEITOP-HENSYU-EXT
           END-IF
      * 
           IF      IDX3            =   1
               MOVE    "1"             TO  SPA-NAI-C02-BYOMEITOP (IDX3)
               GO  TO  4101111-BYOMEITOP-HENSYU-EXT
           END-IF
      *
      *   自院病名の部位が入力されている場合は複数病名にはしない
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       6
               IF    ( SPA-NAI-C02-USERBYOMEIFLG (IDW)
                                           =   "1" )
               AND   ( SPA-NAI-C02-BYOMEISBT(IDW)
                                           =   "1" )
                   MOVE    SPACE       TO  SPA-NAI-C02-BYOMEITOP(IDX3) 
                   GO  TO  4101111-BYOMEITOP-HENSYU-EXT
               END-IF
           END-PERFORM
      *
               COMPUTE IDW     =   IDX3    -   1
               IF    ( SPA-NAI-C02-BYOMEICD (IDW)
                                           =   SPACE )
               AND   ( IDW                 >   1     )
                   COMPUTE IDW     =   IDW    -   1
               END-IF
      *
               IF      SPA-NAI-C02-USERBYOMEIFLG (IDX3)
                                       =   "1"
                   EVALUATE    SPA-NAI-C02-BYOMEISBT(IDX3)
                       WHEN    "8"
                           MOVE    SPACE   TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                       WHEN    "7"
                       WHEN    "0"
                           IF      SPA-NAI-C02-USERBYOMEIFLG (IDW)
                                                       =   "1"
                             EVALUATE    SPA-NAI-C02-BYOMEISBT (IDW)
                               WHEN    "7"
                               WHEN    "1"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "8"
                               WHEN    "0"
                                   MOVE    "1"     TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                             END-EVALUATE
                           ELSE
                             EVALUATE    SPA-NAI-C02-BYOMEICD (IDW)(1:1)
                                   ALSO   SPA-NAI-C02-BYOMEICD(IDW)(4:1)
                               WHEN    "Z" ALSO    "1"    THRU    "7"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "Z" ALSO    "8"
                                   MOVE    "1"         TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "Z" ALSO    ANY
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    OTHER
                                   MOVE    "1"     TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                             END-EVALUATE
                           END-IF
                       WHEN    "1"
                           MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                   END-EVALUATE    
               ELSE
                   EVALUATE    SPA-NAI-C02-BYOMEICD(IDX3)(1:1)
                               ALSO   SPA-NAI-C02-BYOMEICD(IDX3)(4:1) 
                       WHEN    "Z"                 ALSO    "8"
                           MOVE    SPACE   TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                       WHEN    "Z"                 ALSO
                                                   "1"    THRU    "7"
                       WHEN    "0" THRU    "9"     ALSO    ANY
                           IF      SPA-NAI-C02-USERBYOMEIFLG (IDW)
                                                       =   "1"
                             EVALUATE    SPA-NAI-C02-BYOMEISBT (IDW)
                               WHEN    "7"
                               WHEN    "1"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "8"
                               WHEN    "0"
                                   MOVE    "1"     TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                             END-EVALUATE
                           ELSE
                             EVALUATE    SPA-NAI-C02-BYOMEICD (IDW)(1:1)
                                   ALSO   SPA-NAI-C02-BYOMEICD(IDW)(4:1)
                               WHEN    "Z" ALSO    "1"    THRU    "7"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "Z" ALSO    "8"
                                   MOVE    "1"         TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "Z" ALSO    ANY
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    OTHER
                                   MOVE    "1"     TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                             END-EVALUATE
                           END-IF
                       WHEN    "Z"                 ALSO    ANY
                           MOVE    SPACE   TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                   END-EVALUATE    
               END-IF    
           .
       4101111-BYOMEITOP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ編集処理
      *****************************************************************
       410112-USERBYOMEI-HENSYU-SEC      SECTION.
      *
           INITIALIZE                    SPA-NAI-C02-BYOMEI-G (IDZ)
      *
           MOVE    ZERO                  TO  FLG-USERBYOMEI
           IF      USBYO-BYOMEIINPUTCD   =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-HOSPID        TO  USBYO-HOSPID    
               PERFORM 900-USERBYOMEI-INV-SEC
               IF      FLG-USERBYOMEI    =   ZERO
                   MOVE    "1"           TO
                                         SPA-NAI-C02-USERBYOMEIFLG(IDZ)
                   MOVE    USBYO-BYOMEI  TO  SPA-NAI-C02-BYOMEI   (IDZ)
                   MOVE    USBYO-CHARTBYOMEI
                                         TO
                                         SPA-NAI-C02-CHARTBYOMEI  (IDZ)
                   MOVE    USBYO-UTAGAIFLG
                                         TO  SPA-NAI-C02-UTAGAIFLG(IDZ)
                   MOVE    USBYO-MANSEIKBN
                                         TO  SPA-NAI-C02-TOKSKNCD (IDZ)
                   MOVE    USBYO-BYOMEIHENFLG
                                         TO
                                       SPA-NAI-C02-US-BYOMEIHENFLG(IDZ)
                   MOVE    USBYO-HKNBYOMEIFLG
                                         TO
                                       SPA-NAI-C02-US-HKNBYOMEIFLG(IDZ)
                   MOVE    USBYO-BYOMEISBT
                                         TO  SPA-NAI-C02-BYOMEISBT (IDZ)
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   21
                           OR      USBYO-BYOMEICD (IDXX)   =   SPACE
                           OR      USBYO-BYOMEICD (IDXX)   =   ALL "9"
                       MOVE    USBYO-BYOMEICD (IDXX)
                                       TO
                                       SPA-NAI-C02-BYOMEICD-O (IDZ IDXX)
                       IF      SPA-NAI-C02-BYOMEICD-O (IDZ IDXX)
                                       =   WRK-CONS-BYOMEICD
                           MOVE    SPA-NAI-C02-BYOMEI   (IDZ)
                                           TO
                                       SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                       ELSE                           
                           MOVE    SPA-NAI-C02-BYOMEICD-O (IDZ IDXX)
                                           TO   BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI        =   ZERO
                               MOVE    BYO-BYOMEI
                                           TO
                                   SPA-NAI-C02-BYOMEI-O    (IDZ IDXX)
                               MOVE    BYO-TANDOKUKBN
                                           TO
                                   SPA-NAI-C02-TANDOKUKBN-O(IDZ IDXX)
                           END-IF 
                       END-IF    
                   END-PERFORM
      *************MOVE    IDZ               TO  IDX3
      *************PERFORM 4101111-BYOMEITOP-HENSYU-SEC
               ELSE    
                   MOVE    "■■■"      TO  SPA-NAI-C02-BYOMEI   (IDZ)
               END-IF
           END-IF
           .
       410112-USERBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始日チェック処理
      *****************************************************************
       41012-SRYYMD-CHK-SEC           SECTION.
      *
           MOVE    9                    TO  SPA-GMN-C02-CUR
           MOVE    ZERO                 TO  FLG-UPDKBN
      * 
           MOVE   C02-SRYYMD            TO  SPA-GMN-C02-SRYYMD
      *
           IF     SPA-GMN-C02-SRYYMD    =   SPACE
      *********MOVE    SPA-SYSYMD          TO  SPA-NAI-C02-SRYYMD
               MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
               MOVE    SPACE               TO  SPA-GMN-C02-SRYYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-C02-SRYYMD      TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC             >   ZERO
                   MOVE    SPA-GMN-C02-SRYYMD      TO  SGDAY-INDAY
               ELSE
                   EVALUATE    SNUM-SEISUU
                       WHEN    1
                       WHEN    2
                           MOVE    SPA-NAI-C02-SYSYMD  TO  WRK-YMD-9
                           MOVE    SNUM-OUTNUM         TO  WRK-DD
                       WHEN    3
                           MOVE    SPA-NAI-C02-SYSYMD  TO  WRK-YMD-9
                           MOVE    SNUM-OUTNUM         TO  WRK-MD
                       WHEN    4
                           MOVE    SNUM-OUTNUM         TO  WRK-MD
                           IF    ( WRK-MM              =   12   )
                           AND   ( SPA-NAI-C02-SYSYMD (5:2)
                                                       =   "01" )
                               MOVE    SPA-NAI-C02-SYSYMD-BACK (1:4)
                                                       TO  WRK-YY
                           ELSE
                               MOVE    SPA-NAI-C02-SYSYMD (1:4)
                                                       TO  WRK-YY
                           END-IF
                       WHEN    OTHER
                           MOVE    SPA-GMN-C02-SRYYMD  TO  WRK-YMD-9
                   END-EVALUATE
                   MOVE    WRK-YMD-9       TO  SGDAY-INDAY
               END-IF
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-C02-SRYYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-C02-SRYYMD
               ELSE
                   MOVE    "0015"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *         
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-C02-HKNCOMBI    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-C02-HKNCOMBI
                                               TO  WRK-KBN
                   PERFORM 5001-HKNCOMBI-HENSYU-SEC
                   IF      WRK-KBNMEI      NOT =   SPACE
                       IF    ( SPA-NAI-C02-SRYYMD
                                           <=   WRK-TEKEDYMD )
                       AND   ( SPA-NAI-C02-SRYYMD
                                           >=   WRK-TEKSTYMD )
                           CONTINUE
                       ELSE    
                           MOVE    "0011"     TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *         
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-C02-TENKIYMD    =   SPACE
                   CONTINUE
               ELSE    
                   IF      SPA-NAI-C02-SRYYMD  >   SPA-NAI-C02-TENKIYMD
                       MOVE    "0027"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF        
      *
      *    新規のときエラーがなかったら登録処理へ     
           IF      SPA-ERRCD           =   SPACE
      *********IF      SPA-GMN-C02-BANGO   =   ZERO
      *********    MOVE    1                   TO  FLG-UPDKBN
      *********ELSE
                   MOVE    17                  TO  SPA-GMN-C02-CUR
      *********END-IF 
           END-IF     
           .
       41012-SRYYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4102-BYOMEICD-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-HAISIYMDH
                                       WRK-SEIGYO
      *
           MOVE    MCP-WIDGET      TO  WRK-WIDGET
           MOVE    WRK-WIDGET2     TO  IDZ
      *
           COMPUTE SPA-GMN-C02-CUR     =   1   +   IDZ
      *
      *    自院病名検索へ（自院病名のみ入力のとき）
           IF      C02-BYOMEICD (IDZ)(1:2) =   "//"
               MOVE    ZERO                    TO  WRK-BUNRUICD
               IF      C02-BYOMEICD (IDZ) (3:) NOT =   SPACE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    C02-BYOMEICD (IDZ) (3:)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF      SNUM-RC             =   ZERO
                   AND     SNUM-SEISUU         <   4
                       MOVE    SNUM-OUTNUM         TO  WRK-BUNRUICD
                   END-IF
               END-IF
               PERFORM 490-USERBYOMEI-KENSAKU-SEC
               GO  TO  4102-BYOMEICD-CHK-EXT
           END-IF     
      *
      *    病名欄入力後コード入力した時の処理
           IF    ( SPA-NAI-C02-BYOMEICD (1)
                               NOT =   SPA-GMN-C02-BYOMEICD (1) )
           OR    ( SPA-NAI-C02-BYOMEICD (2)
                               NOT =   SPA-GMN-C02-BYOMEICD (2) )
           OR    ( SPA-NAI-C02-BYOMEICD (3)
                               NOT =   SPA-GMN-C02-BYOMEICD (3) )
           OR    ( SPA-NAI-C02-BYOMEICD (4)
                               NOT =   SPA-GMN-C02-BYOMEICD (4) )
           OR    ( SPA-NAI-C02-BYOMEICD (5)
                               NOT =   SPA-GMN-C02-BYOMEICD (5) )
           OR    ( SPA-NAI-C02-BYOMEICD (6)
                               NOT =   SPA-GMN-C02-BYOMEICD (6) )
           OR    ( SPA-NAI-C02-BYOMEICD (7)
                               NOT =   SPACE                    )
               INITIALIZE                    SPA-NAI-C02-BYOMEI-TBL
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       6
      *************IF      IDZ             =   WRK-WIDGET2
      *************    CONTINUE
      *************ELSE
                       MOVE    SPA-GMN-C02-BYOMEICD (IDZ)
                                       TO  SPA-NAI-C02-BYOMEICD (IDZ)
                       MOVE    ZERO                TO  WRK-LENGTH                    
                       PERFORM VARYING IDX FROM    10  BY  -1
                               UNTIL   IDX <       1
                           IF      SPA-NAI-C02-BYOMEICD(IDZ)(IDX:1)
                                               NOT =   SPACE
                               MOVE    IDX             TO  WRK-LENGTH
                               MOVE    1               TO  IDX
                           END-IF
                       END-PERFORM
                       EVALUATE    WRK-LENGTH
                           WHEN    7   
                               MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                               TO  BYO-BYOMEICD
                               PERFORM 410111-BYOMEI-HENSYU-SEC
                           WHEN    ZERO
                               CONTINUE
                           WHEN    OTHER       
                               INITIALIZE          USERBYOMEI-REC
                               MOVE    SPA-HOSPID
                                               TO  USBYO-HOSPID
                               MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                               PERFORM 410112-USERBYOMEI-HENSYU-SEC
                       END-EVALUATE 
      *************END-IF
               END-PERFORM
               MOVE    WRK-WIDGET2     TO  IDZ
           END-IF 
      *
      *    コードの挿入・削除の制御文字取得
           IF      C02-BYOMEICD (IDZ)  NOT =   SPACE
               PERFORM VARYING IDX FROM    12  BY  -1
                       UNTIL   IDX <       2
                   IF      C02-BYOMEICD (IDZ)(IDX:1)
                                       NOT =   SPACE
                       EVALUATE    C02-BYOMEICD (IDZ)(IDX:1)
                           WHEN    "-"
                               MOVE    "-"         TO  WRK-SEIGYO
                           WHEN    "+"
                               MOVE    "+"         TO  WRK-SEIGYO
                           WHEN    OTHER
                               IF      IDX             >   2
                                   COMPUTE IDW     =   IDX     -   1
                                   IF      C02-BYOMEICD (IDZ)(IDW:2)
                                                   =   "−"
                                       MOVE    "-"     TO  WRK-SEIGYO
                                   END-IF
                                   IF      C02-BYOMEICD (IDZ)(IDW:2)
                                                   =   "＋"
                                       MOVE    "+"     TO  WRK-SEIGYO
                                   END-IF
                               END-IF
                       END-EVALUATE
      *                追加のとき
                       IF      WRK-SEIGYO      =   "+"
                           IF      SPA-GMN-C02-BYOMEICD (6)
                                                   NOT =   SPACE
                               MOVE    SPACE           TO  WRK-SEIGYO
                           END-IF 
                           IF      IDZ         >   1
                               COMPUTE IDW     =   IDZ     -   1
                               IF      SPA-NAI-C02-BYOMEICD (IDW)
                                                           =   SPACE 
                                   MOVE    SPACE       TO  WRK-SEIGYO
                               END-IF
                           END-IF
                           IF      WRK-SEIGYO      =   SPACE
                               COMPUTE SPA-GMN-C02-CUR
                                                   =   1   +   IDZ
                               GO  TO  4102-BYOMEICD-CHK-EXT
                           END-IF
                       END-IF
                       MOVE    2               TO  IDX
                   END-IF 
               END-PERFORM
      *****ELSE
      *****    MOVE    "-"                     TO  WRK-SEIGYO
           END-IF
      *       
           EVALUATE    WRK-SEIGYO
               WHEN    "-"
                   PERFORM VARYING IDX FROM    IDZ BY  1
                           UNTIL   IDX >       6
                       IF      IDX             =   6
                           INITIALIZE  SPA-NAI-C02-BYOMEI-OCC(IDX)
                                       SPA-GMN-C02-BYOMEICD  (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     +   1
                           MOVE    SPA-NAI-C02-BYOMEI-OCC (IDW)
                                   TO  SPA-NAI-C02-BYOMEI-OCC (IDX)
                           MOVE    SPA-GMN-C02-BYOMEICD   (IDW)
                                   TO  SPA-GMN-C02-BYOMEICD  (IDX)
                       END-IF                     
                   END-PERFORM                           
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  4102-BYOMEICD-CHK-EXT
               WHEN    "+"
                   PERFORM VARYING IDX FROM    6   BY  -1
                           UNTIL   IDX <       IDZ
                       IF      IDX             =   IDZ
                           INITIALIZE  SPA-NAI-C02-BYOMEI-OCC(IDX)
                                       SPA-GMN-C02-BYOMEICD  (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     -   1
                           MOVE    SPA-NAI-C02-BYOMEI-OCC (IDW)
                                   TO  SPA-NAI-C02-BYOMEI-OCC(IDX)
                           MOVE    SPA-GMN-C02-BYOMEICD   (IDW)
                                   TO  SPA-GMN-C02-BYOMEICD  (IDX)
                       END-IF                     
                   END-PERFORM
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  4102-BYOMEICD-CHK-EXT
           END-EVALUATE                               
      *
      *    半角チェック
           IF      C02-BYOMEICD (IDZ)  NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    C02-BYOMEICD (IDZ)  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-RC2         =   ZERO
                   MOVE    KANACHK-OUT-INPUT   TO  C02-BYOMEICD (IDZ)
               ELSE
                   MOVE    "0072"              TO  SPA-ERRCD
                   GO  TO  4102-BYOMEICD-CHK-EXT
               END-IF
      *             
      *    未コード化傷病名入力のチェック
               IF      C02-BYOMEICD (IDZ)
                                       =   WRK-CONS-BYOMEICD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       6
                       IF      SPA-NAI-C02-BYOMEICD (IDX)
                                              NOT =  SPACE
                       AND     IDX            NOT =   IDZ                       
                           MOVE    "0062"      TO  SPA-ERRCD
                           MOVE    6           TO  IDX
                       END-IF
                   END-PERFORM                                    
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       6
                       IF      SPA-NAI-C02-BYOMEICD(IDX)
                                               =   WRK-CONS-BYOMEICD
                       AND     IDX         NOT =   IDZ                       
                           MOVE    "0063"      TO  SPA-ERRCD
                       END-IF
                   END-PERFORM                                    
               END-IF
           END-IF
      *    
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-BYOMEICD-CHK-EXT
           END-IF
      *
           MOVE    C02-BYOMEICD (IDZ)  TO  SPA-GMN-C02-BYOMEICD (IDZ)
                                           SPA-NAI-C02-BYOMEICD (IDZ)
      *
           INITIALIZE                  SPA-NAI-C02-BYOMEI-G(IDZ)
                                       WRK-HAISIYMDH
      *         
           IF      C02-BYOMEICD (IDZ) (1:1)
                                       =   "."
               MOVE    "ZZZ"       TO  SPA-GMN-C02-BYOMEICD(IDZ)(1:3)
               MOVE    C02-BYOMEICD(IDZ)(2:)
                                   TO  SPA-GMN-C02-BYOMEICD(IDZ)(4:)
               MOVE    SPA-GMN-C02-BYOMEICD (IDZ)
                                   TO  SPA-NAI-C02-BYOMEICD(IDZ)
           END-IF                                                       
      *
           MOVE    ZERO                TO  WRK-LENGTH                    
           PERFORM VARYING IDX FROM    10  BY  -1
                   UNTIL   IDX <       1
               IF      SPA-NAI-C02-BYOMEICD(IDZ)(IDX:1)
                                   NOT =   SPACE
                   MOVE    IDX             TO  WRK-LENGTH
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
      *
      *    ７桁入力のときは病名マスタ、以外は自院病名マスタを読み込む
           EVALUATE    WRK-LENGTH
             WHEN    ZERO
               PERFORM VARYING IDX FROM    IDZ BY  1
                       UNTIL   IDX >       6
                   IF      IDX             =   6
                       INITIALIZE  SPA-NAI-C02-BYOMEI-OCC(IDX)
                                   SPA-GMN-C02-BYOMEICD  (IDX)
                   ELSE
                       COMPUTE IDW     =   IDX     +   1
                       MOVE    SPA-NAI-C02-BYOMEI-OCC (IDW)
                                       TO  SPA-NAI-C02-BYOMEI-OCC (IDX)
                       MOVE    SPA-GMN-C02-BYOMEICD   (IDW)
                                       TO  SPA-GMN-C02-BYOMEICD  (IDX)
                   END-IF                     
               END-PERFORM                           
             WHEN    7   
      *        病名マスタ読み込み
               MOVE    SPA-NAI-C02-BYOMEICD (IDZ) TO  BYO-BYOMEICD
               PERFORM 410111-BYOMEI-HENSYU-SEC
               IF      FLG-BYOMEI      NOT =   ZERO
                   MOVE    "0054"              TO  SPA-ERRCD
               ELSE
                   IF      BYO-IKOSAKICD       =   SPACE
                       IF      BYO-HAISIYMD        =   ALL "9" OR  SPACE
                           CONTINUE
                       ELSE
                           MOVE    BYO-HAISIYMD    TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-HAISIYMDH
                       END-IF        
                   ELSE
                       MOVE    BYO-IKOSAKICD   TO  BYO-BYOMEICD
                       MOVE    ZERO            TO  FLG-BYOMEI
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      FLG-BYOMEI          =   ZERO
                           MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                                   TO
                                               SPA-C02-IKOMOTO-BYOMEICD
                           MOVE    SPA-NAI-C02-BYOMEI   (IDZ)
                                                   TO
                                               SPA-C02-IKOMOTO-BYOMEI
                           MOVE    BYO-BYOMEICD    TO
                                               SPA-C02-IKOSAKI-BYOMEICD
                           MOVE    BYO-BYOMEI      TO
                                               SPA-C02-IKOSAKI-BYOMEI
                           MOVE    IDZ         TO  SPA-C02-IKOSAKI-INDEX                    
                           MOVE    "0103"      TO  SPA-CIDCD2
                           GO  TO  4102-BYOMEICD-CHK-EXT
                       END-IF    
                   END-IF        
               END-IF
             WHEN    OTHER       
      *        自院病名マスタ読み込み
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-HOSPID          TO  USBYO-HOSPID
               MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
               PERFORM 410112-USERBYOMEI-HENSYU-SEC
               IF      FLG-USERBYOMEI   NOT =   ZERO
                   MOVE    "0053"          TO  SPA-ERRCD
               ELSE
                 IF      USBYO-BYOMEIHENFLG    =   SPACE        
                   PERFORM VARYING IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >       21
                           OR    ( USBYO-BYOMEICD (IDX3)   =
                                           SPACE   OR  ALL "9" )
                       IF      USBYO-BYOMEICD (IDX3) (1:1) =   "Z"
                           CONTINUE
                       ELSE
                           MOVE    USBYO-BYOMEICD (IDX3)
                                                       TO  BYO-BYOMEICD
                           MOVE    ZERO                TO  FLG-BYOMEI
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI          =   ZERO
                           AND     BYO-IKOSAKICD   NOT =   SPACE
                               MOVE    USBYO-BYOMEICD (IDX3) 
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                               MOVE    BYO-BYOMEI 
                                           TO  SPA-C02-IKOMOTO-BYOMEI
                               MOVE    BYO-IKOSAKICD   TO  BYO-BYOMEICD
                               PERFORM 900-BYOMEI-READ-SEC
                               IF      FLG-BYOMEI          =   ZERO
                                   MOVE    BYO-BYOMEICD    TO
                                               SPA-C02-IKOSAKI-BYOMEICD
                                   MOVE    BYO-BYOMEI      TO
                                               SPA-C02-IKOSAKI-BYOMEI
                                   MOVE    IDZ             TO
                                               SPA-C02-IKOSAKI-INDEX                    
                                   MOVE    IDX3            TO
                                               SPA-C02-IKOSAKI-INDEX1                    
                                   MOVE    "0104"      TO  SPA-CIDCD2
                                   GO  TO  4102-BYOMEICD-CHK-EXT
                               END-IF
                           END-IF
                           MOVE    21                  TO  IDX3
                       END-IF    
                   END-PERFORM        
                 END-IF                      
               END-IF
           END-EVALUATE    
      *
           PERFORM 41021-BYOMEICD-HENSYU-SEC
           MOVE    SPACE           TO  WRK-SEIGYO
           .
       4102-BYOMEICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード編集処理（病名コード入力時）
      *****************************************************************
       41021-BYOMEICD-HENSYU-SEC      SECTION.
      *
      *    病名編集処理
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       6
               PERFORM 4101111-BYOMEITOP-HENSYU-SEC
           END-PERFORM
           PERFORM 41021-BYOMEI-HENSYU-SEC
           PERFORM 41022-CHARTBYOMEI-HENSYU-SEC
      *
      *   入力病名コード数カウント
           PERFORM 410111-BYOMEICDSU-HENSYU-SEC
      *                                       
      *    新規以外のときは複数病名の入力不可
           IF      SPA-ERRCD               =   SPACE  
               IF      SPA-GMN-C02-BANGO           >   ZERO
               AND     SPA-NAI-C02-BYOMEICD-MAX    >   1
                   MOVE    "0056"             TO  SPA-ERRCD
               END-IF
           END-IF        
      *
      *    括弧で始まる部位が末尾の警告以外のとき
           IF    ( SPA-ERRCD            NOT =   SPACE  )
           AND   ( SPA-ERRCD            NOT =   "0079" )     
               GO  TO  41021-BYOMEICD-HENSYU-EXT
           END-IF
      *
      *    全ての病名コード未入力のとき病名へ遷移
           IF    ( IDZ                 =   1      )
           AND   ( SPA-GMN-C02-BYOMEICD-G  
                                       =   SPACE  )
               MOVE    8               TO  SPA-GMN-C02-CUR
               GO  TO  41021-BYOMEICD-HENSYU-EXT
           END-IF
      *    病名コード未入力のとき開始日へ遷移     
           IF      SPA-NAI-C02-BYOMEICD (IDZ)  =   SPACE
               IF      WRK-SEIGYO      =   "-"
                   GO  TO  41021-BYOMEICD-HENSYU-EXT
               END-IF
               PERFORM VARYING IDX3    FROM    IDZ   BY  1
                       UNTIL   IDX3    >       6
                   IF       SPA-NAI-C02-BYOMEICD (IDX3)
                                       NOT =   SPACE
                       GO  TO  41021-BYOMEICD-HENSYU-EXT
                   END-IF
               END-PERFORM 
               MOVE    9                   TO  SPA-GMN-C02-CUR
               GO  TO  41021-BYOMEICD-HENSYU-EXT
           END-IF    
      *    複数病名コード入力のとき開始日へ遷移
           IF      WRK-SEIGYO      =   "-"
               CONTINUE
           ELSE
               ADD     1               TO  SPA-GMN-C02-CUR
           END-IF
           IF      SPA-GMN-C02-CUR         =   8
               MOVE    9           TO  SPA-GMN-C02-CUR
           END-IF
      *
      *    廃止病名のとき
           IF      WRK-HAISIYMDH       =   SPACE
               CONTINUE
           ELSE
               MOVE    "0075"              TO  SPA-ERRCD
           END-IF                     
           .
       41021-BYOMEICD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード編集チェック処理
      *****************************************************************
       41021-BYOMEI-HENSYU-SEC        SECTION.
      *         
           MOVE    ZERO                TO  SPA-NAI-C02-BYOMEIMOJI
           MOVE    SPACE               TO  SPA-GMN-C02-BYOMEIHENFLG
           MOVE    SPACE               TO  SPA-NAI-C02-BYOMEIHENFLG
      *
      *    病名編集
           MOVE    SPA-NAI-C02-BYOMEI(1)
                                       TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       21
               IF      SPA-NAI-C02-BYOMEITOP   (IDX)   =   "1"
               AND     WRK-BYOMEI                  NOT =   SPACE    
                   STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                           "／"                    DELIMITED  BY  SIZE
                           SPA-NAI-C02-BYOMEI(IDX) DELIMITED  BY  SPACE
                                  INTO             WRK-BYOMEI
                   END-STRING
               ELSE    
                   STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                           SPA-NAI-C02-BYOMEI(IDX) DELIMITED  BY  SPACE
                                  INTO          WRK-BYOMEI
                   END-STRING
               END-IF    
           END-PERFORM
      **** MOVE    WRK-BYOMEI          TO  SPA-GMN-C02-BYOMEI
      *
           IF      WRK-BYOMEI          NOT =   SPA-GMN-C02-BYOMEI
      *      疑いフラグ編集
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       21
                     OR      SPA-GMN-C02-UTAGAIFLG   =   "3"
               EVALUATE    SPA-NAI-C02-UTAGAIFLG   (IDX)
                   WHEN    "1"
                       IF      SPA-GMN-C02-UTAGAIFLG   =   "2"
                           MOVE    "3"     TO  SPA-GMN-C02-UTAGAIFLG
                        ELSE
                           MOVE    SPA-NAI-C02-UTAGAIFLG(IDX)
                                           TO  SPA-GMN-C02-UTAGAIFLG
                        END-IF    
                   WHEN    "2"
                       IF      SPA-GMN-C02-UTAGAIFLG   =   "1"
                           MOVE    "3"     TO  SPA-GMN-C02-UTAGAIFLG
                        ELSE
                           MOVE    SPA-NAI-C02-UTAGAIFLG(IDX)
                                           TO  SPA-GMN-C02-UTAGAIFLG
                       END-IF
                   WHEN    "3"
                       MOVE    "3"     TO  SPA-GMN-C02-UTAGAIFLG
                END-EVALUATE
             END-PERFORM
             MOVE    SPA-GMN-C02-UTAGAIFLG
                                           TO  WRK-KBN
             PERFORM 5001-UTAGAIFLG-HENSYU-SEC
             MOVE    WRK-KBNMEI            TO  SPA-GMN-C02-UTAGAIFLGMEI
      *            
      *      慢性区分編集
             MOVE    SPACE               TO  SPA-GMN-C02-MANSEIKBN-G  
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       21
                 IF      SPA-NAI-C02-TOKSKNCD (IDX)   NOT =   ZERO
                     MOVE    SPA-NAI-C02-TOKSKNCD (IDX)
                                             TO  SPA-GMN-C02-MANSEIKBN
                     MOVE    21              TO  IDX
                 END-IF   
             END-PERFORM
      *     
             MOVE    SPA-GMN-C02-MANSEIKBN
                                       TO  WRK-KBN
             PERFORM 5001-MANSEIKBN-HENSYU-SEC
             MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-MANSEIKBNMEI
           END-IF
      *
      *    保険病名フラグセット
           MOVE    SPACE               TO  SPA-GMN-C02-HKNBYOMEIFLG-G
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6 
               IF      SPA-NAI-C02-US-HKNBYOMEIFLG (IDX)
                                       =   "1"
                   MOVE    "1"         TO  SPA-GMN-C02-HKNBYOMEIFLG
                   MOVE    "保険病名"  TO  SPA-GMN-C02-HKNBYOMEIFLGMEI
                   MOVE    6           TO  IDX
               END-IF
           END-PERFORM                                
      *
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C02-BYOMEI
      *
      *    病名コードエラーの時以下の処理はしない
           IF      SPA-ERRCD            NOT =   SPACE     
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF
      *
      *    文字チェック     
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >       100
                   OR  WRK-BYOMEI(IDX:2)       =   SPACE
               IF      WRK-BYOMEI(IDX:2)   =   "■"
                   MOVE    "0035"          TO  SPA-ERRCD
                   GO  TO  41021-BYOMEI-HENSYU-EXT
              END-IF
           END-PERFORM
      * 
      *    接尾語・接頭語チェック
           MOVE    SPACE                   TO  WRK-RECECD-TBL
      * 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
      *
               IF      SPA-NAI-C02-BYOMEITOP(IDX)    =   "1"
      *            接頭語のみのとき     
                   IF      WRK-RECECD (1)    NOT =   SPACE   AND
                           WRK-RECECD (2)        =   SPACE   AND
                           WRK-RECECD (3)        =   SPACE 
                       MOVE    "0006"        TO  SPA-ERRCD
                       MOVE    21             TO  IDX
                   ELSE
                     MOVE    SPACE               TO  WRK-RECECD-TBL
                   END-IF
               END-IF  
      *
               IF      SPA-ERRCD             =   SPACE   OR  "0079"
                   IF      SPA-NAI-C02-BYOMEICD(IDX)     NOT =   SPACE
                       IF      SPA-NAI-C02-USERBYOMEIFLG(IDX)
                                                     =   SPACE
                         MOVE    SPACE                 TO  SPA-ERRCD                      
                         EVALUATE    SPA-NAI-C02-BYOMEICD(IDX)(1:3)
                               ALSO    SPA-NAI-C02-BYOMEICD(IDX)(4:1)
                           WHEN    "ZZZ"         ALSO    "1"  THRU   "7"
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (1)
                               END-IF            
                           WHEN    "ZZZ"       ALSO    "8"  
                               IF      WRK-RECECD (2)    =   SPACE 
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (3)
                               END-IF
                           WHEN    OTHER                 
                               IF      WRK-RECECD (3)    NOT =   SPACE 
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (2)
                               END-IF    
                         END-EVALUATE
                       ELSE
                         EVALUATE    SPA-NAI-C02-BYOMEISBT (IDX)
                           WHEN    "1"
                               MOVE    "1"       TO
                                               SPA-NAI-C02-BYOMEIHENFLG
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   IF      SPA-NAI-C02-BYOMEI (IDX)(1:2)
                                                     =   "（"
                                       CONTINUE
                                   ELSE
                                       MOVE    "0079"    TO  SPA-ERRCD
                                   END-IF
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (1)
                               END-IF            
                           WHEN    "7"
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (1)
                               END-IF            
                           WHEN    "8"  
                               IF      WRK-RECECD (2)    =   SPACE 
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (3)
                               END-IF
                           WHEN    ZERO
                               IF      WRK-RECECD (3)    NOT =   SPACE 
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (2)
                               END-IF    
                         END-EVALUATE
                       END-IF
                   END-IF   
               END-IF
           END-PERFORM
      *
      *    最後の部位の警告以外は終り
           IF      SPA-ERRCD             =   "0006"
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF
      *     
      *    文字数チェック
           MOVE    SPA-NAI-C02-BYOMEI(1)
                                       TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX  >      21  
               IF      SPA-NAI-C02-BYOMEITOP   (IDX)   =   "1"   
               AND     WRK-BYOMEI                  NOT =   SPACE
                   INITIALIZE                      ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI 
                   MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
                   IF      KANACHK-RC          =   ZERO
                   AND     KANACHK-SYUBETU     =   "2"
                       IF      KANACHK-MAX     >   80
                           MOVE    "0007"          TO  SPA-ERRCD
                           GO  TO  41021-BYOMEI-HENSYU-EXT
                       END-IF
                   END-IF 
                   MOVE    SPACE                   TO  WRK-BYOMEI
               END-IF    
               STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                       SPA-NAI-C02-BYOMEI(IDX) DELIMITED  BY  SPACE
                              INTO          WRK-BYOMEI
               END-STRING
           END-PERFORM    
           IF     WRK-BYOMEI                  NOT =   SPACE     
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   IF      KANACHK-MAX     >   80
                       MOVE    "0007"          TO  SPA-ERRCD
                       GO  TO  41021-BYOMEI-HENSYU-EXT
                   END-IF
      *
      *            病名文字数編集
                   COMPUTE SPA-NAI-C02-BYOMEIMOJI
                                        =   KANACHK-MAX  /   2
               END-IF 
           END-IF    
      *
      *    病名編集フラグセット
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6 
               IF      SPA-NAI-C02-US-BYOMEIHENFLG (IDX)
                                       =   "1"
                   MOVE    "1"             TO  SPA-NAI-C02-BYOMEIHENFLG
                   MOVE    6               TO  IDX
               END-IF
           END-PERFORM                                
           IF      SPA-NAI-C02-BYOMEIHENFLG    =   "1"
               MOVE    "病名編集"      TO  SPA-GMN-C02-BYOMEIHENFLG
           END-IF
           .
       41021-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ病名編集チェック処理
      *****************************************************************
       41022-CHARTBYOMEI-HENSYU-SEC        SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6 
               IF      SPA-NAI-C02-BYOMEICD (IDX)
                                       =   WRK-CONS-BYOMEICD
                   GO  TO  41022-CHARTBYOMEI-HENSYU-EXT
               END-IF
           END-PERFORM                                
      *
           MOVE    ZERO                TO  SPA-NAI-C02-CHARTBYOMEIMOJI
      *　　病名編集
           MOVE    SPA-NAI-C02-CHARTBYOMEI (1)
                                       TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
               IF      SPA-NAI-C02-BYOMEITOP   (IDX)   =   "1"
               AND     WRK-BYOMEI                  NOT =   SPACE 
                   STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                           "／"                    DELIMITED  BY  SIZE
                           SPA-NAI-C02-CHARTBYOMEI (IDX)
                                                   DELIMITED  BY  SPACE
                                  INTO             WRK-BYOMEI
                   END-STRING
               ELSE    
                   STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                           SPA-NAI-C02-CHARTBYOMEI (IDX)
                                                   DELIMITED  BY  SPACE
                                  INTO          WRK-BYOMEI
                   END-STRING
               END-IF    
           END-PERFORM    
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C02-CHARTBYOMEI
      *
      *    病名文字数編集
           COMPUTE WRK-MOJISU   =   WRK-MOJISU  /   2        
           MOVE    WRK-MOJISU          TO  SPA-NAI-C02-CHARTBYOMEIMOJI
           .
       41022-CHARTBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑い　入力処理
      *****************************************************************
       4104-UTAGAI-CHK-SEC                SECTION.
      *        
           MOVE    18                     TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  4104-UTAGAI-CHK-EXT
           END-IF
      *
           MOVE    C02-UTAGAIFLG          TO  SPA-GMN-C02-UTAGAIFLG-G
      *
           MOVE    SPA-GMN-C02-UTAGAIFLG   TO  WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI              TO  SPA-GMN-C02-UTAGAIFLGMEI
           IF      SPA-GMN-C02-UTAGAIFLG =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-UTAGAIFLGMEI   =   SPACE    
                  MOVE    "0018"               TO  SPA-ERRCD
                  GO  TO  4104-UTAGAI-CHK-EXT
               END-IF
           END-IF    
      *        
           MOVE    19                   TO  SPA-GMN-C02-CUR
           .
       4104-UTAGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主病　入力処理
      *****************************************************************
       4105-SYUBYOFLG-CHK-SEC                SECTION.
      *   
           MOVE    19                   TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  4105-SYUBYOFLG-CHK-EXT
           END-IF
      *
           MOVE    C02-SYUBYOFLG        TO  SPA-GMN-C02-SYUBYOFLG-G
      *
      *
           MOVE    SPA-GMN-C02-SYUBYOFLG
                                        TO  WRK-KBN
           PERFORM 5001-SYUBYOFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI              TO  SPA-GMN-C02-SYUBYOFLGMEI
           IF      SPA-GMN-C02-SYUBYOFLG  =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-SYUBYOFLGMEI     =   SPACE
                   MOVE    "0019"               TO  SPA-ERRCD
                   GO  TO  4105-SYUBYOFLG-CHK-EXT
               END-IF
           END-IF        
      *     
           MOVE    20                   TO  SPA-GMN-C02-CUR
           .
       4105-SYUBYOFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性　入力処理
      *****************************************************************
       4106-MANSEI-CHK-SEC                SECTION.
      *   
           MOVE    20                   TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  4106-MANSEI-CHK-EXT
           END-IF
      *
           MOVE    C02-MANSEIFLG        TO  SPA-GMN-C02-MANSEIKBN-G
      *
           MOVE    SPA-GMN-C02-MANSEIKBN
                                       TO  WRK-KBN
           PERFORM 5001-MANSEIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-MANSEIKBNMEI
      *                                 
           IF      SPA-GMN-C02-MANSEIKBN =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-MANSEIKBNMEI   =   SPACE    
                  MOVE    "0021"               TO  SPA-ERRCD
                  GO  TO  4106-MANSEI-CHK-EXT
               END-IF
           END-IF    
      *
      *
           IF      SPA-ERRCD              =   SPACE
               MOVE    21                   TO  SPA-GMN-C02-CUR
           END-IF
           .
       4106-MANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4107-NYUGAIKBN-CHK-SEC                SECTION.
      *   
           MOVE    21                   TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  4107-NYUGAIKBN-CHK-EXT
           END-IF
      *
           MOVE    C02-NYUGAIKBN        TO  SPA-GMN-C02-NYUGAIKBN-G
      *
           MOVE    SPA-GMN-C02-NYUGAIKBN
                                       TO  WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-NYUGAIKBNMEI
      *                                 
           IF      SPA-GMN-C02-NYUGAIKBN =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-NYUGAIKBNMEI   =   SPACE    
                   MOVE    "0022"               TO  SPA-ERRCD
                   GO  TO  4107-NYUGAIKBN-CHK-EXT
               END-IF
           END-IF
      *             
           MOVE    22                   TO  SPA-GMN-C02-CUR
           .
       4107-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ入力処理
      *****************************************************************
       4108-COMB-CHK-SEC                SECTION.
      *
           MOVE    22                  TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  4108-COMB-CHK-EXT
           END-IF
      *
           MOVE    C02-HKNCOMBI        TO  SPA-GMN-C02-HKNCOMBI-G
      *
           IF      SPA-GMN-C02-HKNCOMBI
                                       =   SPACE
               MOVE    SPACE               TO  SPA-GMN-C02-HKNCOMBIMEI
           ELSE
               MOVE    SPA-GMN-C02-HKNCOMBI
                                           TO  WRK-KBN
               PERFORM 5001-HKNCOMBI-HENSYU-SEC
               IF      WRK-KBNMEI      =   SPACE
                   MOVE    "0009"               TO  SPA-ERRCD
               ELSE
                   IF    ( SPA-NAI-C02-SRYYMD
                                       <=   WRK-TEKEDYMD )
                   AND   ( SPA-NAI-C02-SRYYMD
                                       >=   WRK-TEKSTYMD )
                       MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-HKNCOMBIMEI
                   ELSE    
                       MOVE    "0011"     TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD              =   SPACE
               MOVE    23                  TO  SPA-GMN-C02-CUR
           END-IF
      *
           .
       4108-COMB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト表示入力処理
      *****************************************************************
       4109-REZEFLG-CHK-SEC             SECTION.
      *
           MOVE    23                 TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  4109-REZEFLG-CHK-EXT
           END-IF
      *
           MOVE    C02-REZEFLG        TO  SPA-GMN-C02-REZEFLG-G
      *
           MOVE    SPA-GMN-C02-REZEFLG TO  WRK-KBN
           PERFORM 5001-REZEFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-REZEFLGMEI
      *                                 
           IF      SPA-GMN-C02-REZEFLG =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-REZEFLGMEI   =   SPACE    
                   MOVE    "0023"               TO  SPA-ERRCD
                   GO  TO  4109-REZEFLG-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    24                 TO  SPA-GMN-C02-CUR
           .
       4109-REZEFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト月  入力処理
      *****************************************************************
       41010-REZEMM-CHK-SEC            SECTION.
      *
           MOVE    24                  TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  41010-REZEMM-CHK-EXT
           END-IF
      *
           MOVE    C02-REZEMM          TO  SPA-GMN-C02-REZEMM
      *
           MOVE    25                  TO  SPA-GMN-C02-CUR
           .
       41010-REZEMM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰区分  入力処理
      *****************************************************************
       41011-TENKIKBN-CHK-SEC            SECTION.
      *
           MOVE    17                  TO  SPA-GMN-C02-CUR
      *
           MOVE    C02-TENKIKBN        TO  SPA-GMN-C02-TENKIKBN-G
      *
           MOVE    SPA-GMN-C02-TENKIKBN
                                       TO  WRK-KBN
           PERFORM 5001-TENKIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TENKIKBNMEI
      *                                 
           IF      SPA-GMN-C02-TENKIKBN
                                       =   SPACE
               MOVE    SPACE               TO  SPA-NAI-C02-TENKIYMD
               MOVE    SPACE               TO  SPA-GMN-C02-TENKIYMD
           ELSE
               IF      SPA-GMN-C02-TENKIKBNMEI
                                           =   SPACE
                   MOVE    "0024"              TO  SPA-ERRCD
                   GO  TO  41011-TENKIKBN-CHK-EXT
               END-IF
           END-IF
      *     
      *----(01.02.02) LINE INS START ----------------------------------
           IF    ( SPA-GMN-C02-TENKIKBN NOT =   SPACE )
           AND   ( SPA-GMN-C02-TENKIYMD     =   SPACE )
               MOVE    SPA-NAI-C02-SYSYMD   TO  SPA-NAI-C02-TENKIYMD
               MOVE    SPA-GMN-C02-SYSYMD   TO  SPA-GMN-C02-TENKIYMD
      *********MOVE    "0026"          TO  SPA-ERRCD
      *********GO      TO      41011-TENKIKBN-CHK-EXT
           END-IF    
      *----(01.02.02) LINE INS END   ----------------------------------
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX >   1
               MOVE    2                    TO  SPA-GMN-C02-CUR
           ELSE
               IF      SPA-GMN-C02-TENKIKBN =   SPACE
                   MOVE    18                   TO  SPA-GMN-C02-CUR
               ELSE                 
                   MOVE    13                   TO  SPA-GMN-C02-CUR
               END-IF    
           END-IF    
           .
       41011-TENKIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰年月日  入力処理
      *****************************************************************
       41012-TENKIYMD-CHK-SEC            SECTION.
      *
           MOVE    13                 TO  SPA-GMN-C02-CUR
      *
           MOVE    C02-TENKIYMD       TO  SPA-GMN-C02-TENKIYMD
      *     
           MOVE    SPACE              TO  SPA-NAI-C02-TENKIYMD
           IF      SPA-GMN-C02-TENKIYMD
                                      =   SPACE
               IF      SPA-GMN-C02-TENKIKBN   NOT =   SPACE
                   MOVE    "0036"          TO  SPA-ERRCD
               END-IF    
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-C02-TENKIYMD    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC             >   ZERO
                   MOVE    SPA-GMN-C02-TENKIYMD    TO  SGDAY-INDAY
               ELSE
                   EVALUATE    SNUM-SEISUU
                       WHEN    1
                       WHEN    2
                           MOVE    SPA-NAI-C02-SYSYMD  TO  WRK-YMD-9
                           MOVE    SNUM-OUTNUM         TO  WRK-DD
                       WHEN    3
                           MOVE    SPA-NAI-C02-SYSYMD  TO  WRK-YMD-9
                           MOVE    SNUM-OUTNUM         TO  WRK-MD
                       WHEN    4
                           MOVE    SNUM-OUTNUM         TO  WRK-MD
                           IF    ( WRK-MM              =   12   )
                           AND   ( SPA-NAI-C02-SYSYMD (5:2)
                                                       =   "01" )
                               MOVE    SPA-NAI-C02-SYSYMD-BACK (1:4)
                                                       TO  WRK-YY
                           ELSE
                               MOVE    SPA-NAI-C02-SYSYMD (1:4)
                                                       TO  WRK-YY
                           END-IF
                       WHEN    OTHER
                           MOVE    SPA-GMN-C02-TENKIYMD
                                                       TO  WRK-YMD-9
                   END-EVALUATE
                   MOVE    WRK-YMD-9       TO  SGDAY-INDAY
               END-IF
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-C02-TENKIYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-C02-TENKIYMD
      *             
                   IF      SPA-NAI-C02-SRYYMD  >   SPA-NAI-C02-TENKIYMD
                       MOVE    "0027"              TO  SPA-ERRCD
                   END-IF    
                   IF      SPA-GMN-C02-TENKIKBN    =   SPACE
                       MOVE    "0037"          TO  SPA-ERRCD
                   END-IF    
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *         
           IF      SPA-ERRCD           =   SPACE
               MOVE    18                  TO  SPA-GMN-C02-CUR 
           END-IF     
           .
       41012-TENKIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名  入力処理
      *****************************************************************
       41013-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    8                    TO  SPA-GMN-C02-CUR
      * 
      *    複数病名入力のとき
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR 
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    未コード化傷病名コードのとき病名がおなじとき    
           IF      C02-BYOMEI          =   SPA-GMN-C02-BYOMEI
           AND     SPA-NAI-C02-BYOMEICD (1)
                                       =   WRK-CONS-BYOMEICD
               MOVE    9                   TO  SPA-GMN-C02-CUR 
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    病名コード入力済でエラーのときコードクリア
           IF    ( SPA-GMN-C02-BYOMEICD-G
                                   NOT =   SPACE )
           AND   ( SPA-NAI-C02-BYOMEIMOJI
                                       =   ZERO  )
                INITIALIZE                   SPA-GMN-C02-BYOMEICD-G
                                             SPA-NAI-C02-BYOMEI-TBL
                MOVE    ZERO             TO  SPA-NAI-C02-BYOMEICD-MAX
           END-IF
      *
           MOVE    SPA-GMN-C02-BYOMEI   TO  WRK-BYOMEI-OLD
      *
           MOVE    C02-BYOMEI           TO  SPA-GMN-C02-BYOMEI
      *     
           MOVE    ZERO                 TO  SPA-NAI-C02-BYOMEIMOJI
      *
           IF      SPA-GMN-C02-BYOMEI   =   SPACE
                INITIALIZE                   SPA-GMN-C02-BYOMEICD-G
                                             SPA-NAI-C02-BYOMEI-TBL
                MOVE    SPACE            TO  SPA-NAI-C02-BYOMEIHENFLG
                                             SPA-GMN-C02-BYOMEIHENFLG
                MOVE    ZERO             TO  SPA-NAI-C02-BYOMEIMOJI
                MOVE    ZERO             TO  SPA-NAI-C02-BYOMEICD-MAX
                INITIALIZE                   SPA-GMN-C02-UTAGAIFLG-G
                                             SPA-GMN-C02-MANSEIKBN-G
                MOVE    28               TO  SPA-GMN-C02-CUR
                GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    半角・文字数チェック
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    SPA-GMN-C02-BYOMEI  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-SYUBETU     =   "2"
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-C02-BYOMEI
      *        病名文字数
               IF      KANACHK-MAX     >   80
                   MOVE    "0007"              TO  SPA-ERRCD
                   GO  TO  41013-BYOMEI-CHK-EXT
               END-IF
           ELSE
               MOVE    "0071"              TO  SPA-ERRCD
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    文字チェック     
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >       80
                   OR  SPA-GMN-C02-BYOMEI(IDX:2)   =   SPACE
               IF      SPA-GMN-C02-BYOMEI(IDX:2)   =   "■"
                   MOVE    "0035"          TO  SPA-ERRCD
                   GO  TO  41013-BYOMEI-CHK-EXT
              END-IF
           END-PERFORM
      *
      *    病名からコード検索処理
           INITIALIZE           ORCSBYOMEIAREA
           MOVE    "1"             TO  LNK-BYO-SYORI
           MOVE    SPA-GMN-C02-BYOMEI
                                   TO  LNK-BYO-BYOMEI
           IF      SPA-GMN-C02-SYUBYOFLG
                                   =   "1"
               MOVE    SPA-GMN-C02-SYUBYOFLG
                                       TO  LNK-BYO-SYUBYOFLG 
           END-IF
           CALL    "ORCSBYOMEI"     USING    ORCSBYOMEIAREA
      * 
           INITIALIZE               SPA-NAI-C02-BYOMEI-TBL
           MOVE    ZERO         TO  IDZ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
               ADD     1           TO  IDZ
               MOVE    LNK-BYO-TBYOMEICD (IDX)
                                   TO  SPA-NAI-C02-BYOMEICD  (IDZ)
                                       SPA-NAI-C02-BYOMEICD-O(IDZ 1)
               IF      IDZ             =   1
                   MOVE    "1"         TO  SPA-NAI-C02-BYOMEITOP(IDZ)
               END-IF
               MOVE    LNK-BYO-TBYOMEI (IDX)
                                       TO  SPA-NAI-C02-BYOMEI   (IDZ)
                                           SPA-NAI-C02-BYOMEI-O (IDZ 1)
               MOVE   LNK-BYO-TUTAGAIFLG (IDX)
                                       TO  SPA-NAI-C02-UTAGAIFLG(IDZ)
               MOVE    LNK-BYO-TTOKSKNCD (IDX)
                                       TO  SPA-NAI-C02-TOKSKNCD (IDZ)
               MOVE    LNK-BYO-TTANDOKUKBN (IDX)
                                       TO
                                       SPA-NAI-C02-TANDOKUKBN-O (IDZ 1)
               IF      SPA-NAI-C02-TANDOKUKBN-O (IDZ 1)
                                   NOT =   ZERO
                   MOVE    LNK-BYO-TBYOMEI (IDX)
                                               TO  WRK-TANDOKUKBN-BYOMEI
               END-IF  
           END-PERFORM
      *
           MOVE    LNK-BYO-BYOMEIMOJI  TO  SPA-NAI-C02-BYOMEIMOJI
           MOVE    LNK-BYO-UTAGAIFLG   TO  SPA-GMN-C02-UTAGAIFLG
                                           WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-UTAGAIFLGMEI
           IF      LNK-BYO-MANSEIKBN
                                   NOT =   ZERO
               MOVE    LNK-BYO-MANSEIKBN   TO  SPA-GMN-C02-MANSEIKBN
                                               WRK-KBN
               PERFORM 5001-MANSEIKBN-HENSYU-SEC                                
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-MANSEIKBNMEI
           ELSE
               MOVE    SPACE               TO  SPA-GMN-C02-MANSEIKBN-G 
           END-IF
           MOVE    LNK-BYO-RECEDENFLG  TO  SPA-NAI-C02-RECEDENFLG
           MOVE    LNK-BYO-BYOMEIHENFLG
                                       TO  SPA-NAI-C02-BYOMEIHENFLG
           IF      SPA-NAI-C02-BYOMEIHENFLG  =   "1" 
               MOVE    "病名編集"  TO  SPA-GMN-C02-BYOMEIHENFLG
           ELSE
               MOVE    SPACE       TO  SPA-GMN-C02-BYOMEIHENFLG
           END-IF    
           MOVE    1           TO  SPA-NAI-C02-BYOMEICD-MAX
           IF      LNK-BYO-BYOMEIHENFLG    =   SPACE                          
               IF      LNK-BYO-HAISIYMD-ERR    NOT =   ZERO
                   MOVE    LNK-BYO-HAISIYMD-ERR
                                               TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HAISIYMDH
               END-IF        
               IF      LNK-BYO-IKOSAKICD   NOT =   SPACE
                   MOVE    LNK-BYO-IKOSAKICD-INDEX
                                               TO  IDX
                   MOVE    SPA-NAI-C02-BYOMEICD (IDX) 
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                   MOVE    SPA-NAI-C02-BYOMEI   (IDX) 
                                           TO  SPA-C02-IKOMOTO-BYOMEI
                   MOVE    LNK-BYO-IKOSAKICD
                                           TO  SPA-C02-IKOSAKI-BYOMEICD
                   MOVE    LNK-BYO-IKOSAKICD-BYOMEI
                                           TO  SPA-C02-IKOSAKI-BYOMEI
                   MOVE    IDX     TO  SPA-C02-IKOSAKI-INDEX                    
                   MOVE    "0101"  TO  SPA-CIDCD2
                   MOVE    SPACE   TO  WRK-HAISIYMDH
               END-IF
           END-IF
      *
      *    修飾語コードのみのとき
           IF      LNK-BYO-BYOMEICD-ERR    =   1
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-CIDCD2
                                           WRK-HAISIYMDH
               MOVE    ZERO            TO  SPA-NAI-C02-BYOMEIMOJI 
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    病名コード最大個数オーバー
           IF      LNK-BYO-BYOMEICD-MAX-ERR =   1
               MOVE    "0077"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-CIDCD2
                                           WRK-HAISIYMDH
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    病名コード順エラー
           IF      LNK-BYO-BYOMEICD-ERR    =   2   OR  9
               MOVE    "0103"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-CIDCD2
                                           WRK-HAISIYMDH
               MOVE    28          TO  SPA-GMN-C02-CUR
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    単独使用禁止エラー
           IF      LNK-BYO-TANDOKUKBN-ERR
                                       =   1
               MOVE    "0076"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-CIDCD2
                                           WRK-HAISIYMDH
               GO  TO  41013-BYOMEI-CHK-EXT
           ELSE
               MOVE    SPACE           TO  WRK-TANDOKUKBN-BYOMEI
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    28              TO  SPA-GMN-C02-CUR
           END-IF
      *
      *    廃止病名警告
           IF    ( SPA-CIDCD2          =   SPACE )
           AND   ( WRK-HAISIYMDH   NOT =   SPACE )
               MOVE    "0075"          TO  SPA-ERRCD
           END-IF    
           .
       41013-BYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険病名フラグ入力処理
      *****************************************************************
       41014-HKNBYOMEIFLG-CHK-SEC                SECTION.
      *   
           MOVE    25                   TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR 
               GO  TO  41014-HKNBYOMEIFLG-CHK-EXT
           END-IF    
      *
           MOVE    C02-HKNBYOMEIFLG     TO  SPA-GMN-C02-HKNBYOMEIFLG-G
      *
           MOVE    SPA-GMN-C02-HKNBYOMEIFLG
                                       TO  WRK-KBN
           PERFORM 5001-HKNBYOMEIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HKNBYOMEIFLGMEI
      *
           IF      SPA-GMN-C02-HKNBYOMEIFLG   =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-HKNBYOMEIFLGMEI
                                              =   SPACE
                   MOVE    "0028"               TO  SPA-ERRCD
                   GO  TO  41014-HKNBYOMEIFLG-CHK-EXT
                   
               END-IF
           END-IF
      *     
           MOVE    2                   TO  SPA-GMN-C02-CUR
           .
       41014-HKNBYOMEIFLG-CHK-EXT.
           EXIT.
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    カルテ病名  入力処理
      *****************************************************************
       41018-CHARTBYOMEI-CHK-SEC        SECTION.
      *
           MOVE    28                   TO  SPA-GMN-C02-CUR
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR 
               GO  TO  41018-CHARTBYOMEI-CHK-EXT
      *****ELSE
      *        IF      SPA-NAI-C02-USERBYOMEIFLG (1)
      *                                    =   "1"
      *            IF      C02-CHARTBYOMEI
      *                                NOT =   SPA-GMN-C02-CHARTBYOMEI
      *                MOVE    "0061"              TO  SPA-ERRCD
      *                MOVE    9                   TO  SPA-GMN-C02-CUR 
      *                GO  TO  41018-CHARTBYOMEI-CHK-EXT
      *            END-IF
      *********END-IF      
           END-IF    
      *
           MOVE    C02-CHARTBYOMEI      TO  SPA-GMN-C02-CHARTBYOMEI
      *
           MOVE    ZERO                 TO  SPA-NAI-C02-CHARTBYOMEIMOJI
      *
           IF      SPA-GMN-C02-CHARTBYOMEI  =   SPACE
               MOVE    9                    TO  SPA-GMN-C02-CUR
               GO  TO  41018-CHARTBYOMEI-CHK-EXT
           END-IF                                       
      *
      *    半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    SPA-GMN-C02-CHARTBYOMEI
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-SYUBETU     =   "2"
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-C02-CHARTBYOMEI
      *        病名文字数
               IF      KANACHK-MAX     >   80
                   MOVE    "0033"          TO  SPA-ERRCD
               END-IF 
           ELSE
               MOVE    "0071"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        病名文字数編集
               COMPUTE SPA-NAI-C02-CHARTBYOMEIMOJI
                                       =   KANACHK-MAX  /   2        
      *
               MOVE    9                    TO  SPA-GMN-C02-CUR
           END-IF
           .
       41018-CHARTBYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41019-BSRYKA-CHK-SEC             SECTION.
      *
           MOVE    31                  TO  SPA-GMN-C02-CUR
      *
           MOVE    C02-BSRYKA          TO  SPA-GMN-C02-BSRYKA-G
      *
           IF      SPA-GMN-C02-BSRYKA  =   SPACE
               IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
                   MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
               ELSE
                   MOVE    "0030"              TO  SPA-ERRCD
               END-IF
           ELSE    
               MOVE    SPA-GMN-C02-BSRYKA  TO  WRK-KBN
               PERFORM 5001-BSRYKA-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-BSRYKAMEI
               IF      WRK-KBNMEI          =   SPACE  
                   MOVE    "0030"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-GMN-C02-CUR
           END-IF    
           .
       41019-BSRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分入力処理
      *****************************************************************
       41022-SYOKBN-CHK-SEC            SECTION.
      *
           MOVE    32                  TO  SPA-GMN-C02-CUR
      *
           MOVE    C02-SYOKBN          TO  SPA-GMN-C02-SYOKBN-G
      *
      *****IF      SPA-GMN-C02-SRYKA  =   SPACE
      *****    MOVE    SPA-GMN-C02-SYOKBNLST (1)
      *****                                TO  SPA-GMN-C02-SYOKBN-G
      *****ELSE    
               MOVE    SPA-GMN-C02-SYOKBN  TO  WRK-KBN
               PERFORM 5001-SYOKBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SYOKBNMEI
               IF      WRK-KBNMEI          =   SPACE  
                   MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                               TO  SPA-GMN-C02-SYOKBN-G
               END-IF
      *****END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           MOVE    "PTBYOMEI-KEY16"     TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
           MOVE    1                   TO  SPA-GMN-C02-CUR
           .
       41022-SYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分表示切替入力処理
      *****************************************************************
       41023-HNYUGAIKBN-CHK-SEC         SECTION.
      *   
           MOVE    33                   TO  SPA-GMN-C02-CUR
      *
           MOVE    C02-HNYUGAIKBN       TO  SPA-GMN-C02-HNYUGAIKBN-G
      *
           MOVE    SPA-GMN-C02-HNYUGAIKBN
                                       TO  WRK-KBN
           PERFORM 5001-HNYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HNYUGAIKBNMEI
           IF      SPA-GMN-C02-HNYUGAIKBNMEI  =   SPACE    
               MOVE    "0022"                 TO  SPA-ERRCD
               GO  TO  41023-HNYUGAIKBN-CHK-EXT
           END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           MOVE    "PTBYOMEI-KEY16"     TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
           MOVE    1                   TO  SPA-GMN-C02-CUR
           .
       41023-HNYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41020-BYOMEISEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  SPA-C02-IKOFLG           
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-MAX
               IF      C02-BYO-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-C02-SELNO
                       CONTINUE
                   ELSE        
                       MOVE    IDX                 TO  WRK-SELNO
                       MOVE    WRK-CONS-C02-MAX    TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-C02-SELNO
      *****     ELSE
      *****         GO  TO  41020-BYOMEISEL-EXT    
           END-IF
      *
           INITIALIZE                 SPA-GMN-C02-KOUMOKU
                                      SPA-NAI-C02-KOUMOKU
      *
           MOVE    SPA-GMN-C02-TBANGO (SPA-NAI-C02-SELNO)
                                       TO  SPA-GMN-C02-BANGO
           MOVE    SPA-NAI-C02-SELNO   TO  WRK-BANGO
      *                                 
           PERFORM 410111-BYOMEI-SELECT-SEC     
           .
       41020-BYOMEISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41021-HKNCOMBISEL-SEC       SECTION.
      *          
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR 
               GO  TO  41021-HKNCOMBISEL-EXT
           END-IF
      *     
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-HKN-MAX
               IF      C02-HKN-SELECT (IDX)    =   "T"
                   MOVE    IDX                  TO  WRK-SELNO
                   MOVE    SPA-GMN-C02-HKN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      ***  MOVE    SPA-GMN-C02-TNUM (WRK-SELNO)
      ***                              TO  SPA-GMN-C02-HKNCOMBI
      *
           MOVE    ZERO               TO  WRK-HENFLG
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-C02-HKNCOMBI-MAX
               IF      SPA-GMN-C02-TNUM (WRK-SELNO) 
                                      =   SPA-GMN-C02-HKNCOMBIL (IDY)
                   IF    ( SPA-NAI-C02-SRYYMD
                                  <= SPA-NAI-C02-TEKEDYMD (IDY) )
                   AND   ( SPA-NAI-C02-SRYYMD
                                  >= SPA-NAI-C02-TEKSTYMD (IDY) )
                       MOVE    SPA-GMN-C02-HKNCOMBILST (IDY)
                                          TO  SPA-GMN-C02-HKNCOMBI-G
                   ELSE    
                       MOVE    "0011"     TO  SPA-ERRCD
                   END-IF        
                   MOVE    1              TO  WRK-HENFLG
                   MOVE    SPA-GMN-C02-HKNCOMBI-MAX
                                          TO  IDY
               END-IF
           END-PERFORM
           IF      WRK-HENFLG         =   ZERO
           AND     SPA-ERRCD          =   SPACE
               MOVE    "0009"               TO  SPA-ERRCD
           END-IF
           .
       41021-HKNCOMBISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *----(01.00.02) LINE ADD START ----------------------------------
           IF      SPA-C02-UPDFLG          =   "1"
               MOVE    "0101"          TO  SPA-CIDCD
               GO      TO      210-BACK-EXT
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    ファイルクリア
           MOVE    SPA-TERMID          TO  FILE-TERMID
           MOVE    ZERO                TO  IF-RESULT
           MOVE    FILE-NAME1          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           MOVE    1                   TO  FLG-END2
      *
           MOVE    SPA-C02-MOTOPG     TO  SPA-SAKIPG
           IF      SPA-C02-MOTOPG NOT =   "K02"  AND  "U02"   AND
                                          "K02N"
      ********* 患者番号入力へ
      *********MOVE   "C01"            TO  SPA-SAKIPG
               MOVE   "M01"            TO  SPA-SAKIPG
           END-IF
           MOVE    "C02"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           IF      SPA-C02-MOTOPG  NOT =   "K02"  AND  "U02"  AND 
                                           "K02N"
               INITIALIZE                  SPA-KYOTUKEY
                                           SPA-KYOTU-SET
           END-IF 
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
      ***  MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *    
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
           MOVE    ZERO                TO  SPA-C02-IKOFLG
      *      
           IF    ( SPA-GMN-C02-BYOMEI  NOT =   SPACE )
           OR    ( SPA-GMN-C02-BANGO-X NOT =   SPACE )        
               INITIALIZE               SPA-GMN-C02-KOUMOKU
                                        SPA-NAI-C02-KOUMOKU
                                        SPA-NAI-C02-SELNO
               MOVE    1                TO  SPA-GMN-C02-CUR
           ELSE                             
      *
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *
               INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
                                           SPA-C02-UPDFLG
                                           SPA-C02-IKOFLG
      *                              
                                           SPA-GMN-C02-TBL
                                           SPA-NAI-C02-TBL
                                           SPA-GMN-C02-MAX
                                           SPA-GMN-C02-SRYKA-G
                                           SPA-PTBYOMEI-REC
               MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                       TO  SPA-GMN-C02-SYOKBN-G   
               MOVE    SPA-GMN-C02-HNYUGAIKBNLST (1)
                                       TO  SPA-GMN-C02-HNYUGAIKBN-G   
      *
      ******** IF      SPA-C02-MOTOPG      =   "K02"   OR  "U02"   OR
      *                                         "K02N"
      *             MOVE    27              TO  SPA-GMN-C02-CUR
      ******** ELSE                                     
                   INITIALIZE              SPA-GMN-C02-HEAD
                                           SPA-NAI-C02-HEAD
                                           SPA-GMN-C02-HKN-TBL
                                           SPA-NAI-C02-HKN-TBL
                                           SPA-GMN-C02-HKNCOMBILST-G
                                           SPA-GMN-C02-HKN-MAX
                                           SPA-GMN-C02-HKNCOMBI-MAX
                   MOVE    26              TO  SPA-GMN-C02-CUR
      ******** END-IF
      *
               IF      SPA-GMN-C02-SRYKA-MAX   =   1 
                   MOVE    SPA-GMN-C02-SRYKALST (1)
                                           TO  SPA-GMN-C02-SRYKA-G
               END-IF                            
           END-IF                                    
      *
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C02-PAGE    =   1
               GO      TO      460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-C02-PAGE    =   SPA-GMN-C02-PAGE    -   1
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "PTBYOMEI-KEY16"     TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C02-NEXT        =   ZERO
               GO      TO      470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C02-PAGE
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "PTBYOMEI-KEY16"     TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *    
      *****************************************************************
      *    前回患者番号編集処理
      *****************************************************************
       230-MAEKPTNUM-SEC             SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      230-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  C02-PTNUM
           PERFORM 41015-PTNUM-CHK-SEC
      *
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除ガイドメッセージ処理
      *****************************************************************
       440-SAKUJYO-GID-SEC             SECTION.
      *
           IF      SPA-NAI-C02-PTID    =   ZERO
           OR      SPA-GMN-C02-SRYKA   =   SPACE
               MOVE    "0050"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-GID-EXT
           END-IF
      *
           IF      SPA-GMN-C02-BANGO  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-GID-EXT
           END-IF
      *
           MOVE    "0101"             TO  SPA-CIDCD4
          .
       440-SAKUJYO-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       440-SAKUJYO-SEC             SECTION.
      *
           INITIALIZE                      PTBYO-KEY
           MOVE    SPA-HOSPID              TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID        TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-SRYKA-OLD   TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD-OLD  TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-RENNUM      TO  PTBYO-RENNUM 
           MOVE    PTBYOMEI-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "PTBYOMEI-KEY"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  440-SAKUJYO-EXT
           END-IF
      *
           COMPUTE C02-BYO-ROW =   SPA-NAI-C02-SELNO   -   1    
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
          .
       440-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
           IF     (SPA-NAI-C02-PTID         =   ZERO  )
             OR   (SPA-GMN-C02-SRYKA        =   SPACE )
               MOVE    "0050"             TO  SPA-ERRCD
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
           IF     (SPA-GMN-C02-BANGO        =   ZERO  )
           AND    (SPA-GMN-C02-BYOMEI       =   SPACE )
               MOVE    "0051"             TO  SPA-ERRCD
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    入力項目チェック
           MOVE    SPA-GMN-C02-BSRYKA-G   TO  C02-BSRYKA
           PERFORM 41019-BSRYKA-CHK-SEC
           IF      SPA-ERRCD           NOT =   SPACE         
               GO  TO  450-TOUROKU-EXT
           END-IF
           MOVE    SPA-GMN-C02-SRYYMD      TO  C02-SRYYMD
           PERFORM 41012-SRYYMD-CHK-SEC
           IF      SPA-ERRCD           NOT =   SPACE         
               GO  TO  450-TOUROKU-EXT
           END-IF
           MOVE    SPA-GMN-C02-TENKIYMD    TO  C02-TENKIYMD
           PERFORM 41012-TENKIYMD-CHK-SEC
           IF      SPA-ERRCD           NOT =   SPACE         
               GO  TO  450-TOUROKU-EXT
           END-IF
           MOVE    SPA-GMN-C02-TENKIKBN-G  TO  C02-TENKIKBN
           PERFORM 41011-TENKIKBN-CHK-SEC
           IF      SPA-ERRCD           NOT =   SPACE         
               GO  TO  450-TOUROKU-EXT
           END-IF
           IF      SPA-NAI-C02-BYOMEICD-MAX    =   1
               MOVE    SPA-GMN-C02-UTAGAIFLG-G TO  C02-UTAGAIFLG
                   PERFORM 4104-UTAGAI-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-SYUBYOFLG-G TO  C02-SYUBYOFLG
               PERFORM 4105-SYUBYOFLG-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-MANSEIKBN-G TO  C02-MANSEIFLG
               PERFORM 4106-MANSEI-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-NYUGAIKBN-G TO  C02-NYUGAIKBN
               PERFORM 4107-NYUGAIKBN-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-HKNCOMBI-G TO  C02-HKNCOMBI
               PERFORM 4108-COMB-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-REZEFLG-G  TO  C02-REZEFLG
               PERFORM 4109-REZEFLG-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-REZEMM TO  C02-REZEMM
               PERFORM 41010-REZEMM-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-CHARTBYOMEI TO  C02-CHARTBYOMEI
               PERFORM 41018-CHARTBYOMEI-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-HKNBYOMEIFLG-G TO  C02-HKNBYOMEIFLG
               PERFORM 41014-HKNBYOMEIFLG-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
           END-IF    
      *
      *    病名が未入力のとき
           IF      SPA-NAI-C02-BYOMEIMOJI   =   ZERO
               MOVE    "0010"               TO  SPA-ERRCD
               GO      TO      450-TOUROKU-EXT
           END-IF
      *
      *    単独使用禁止の病名のとき
           INITIALIZE                  WRK-TANDOKU-AREA
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       21
               IF    ( SPA-NAI-C02-BYOMEITOP (IDZ)
                                           =   "1" )
               AND   ( IDZ                 >   1   )
                   IF      WRK-TANDOKUKBN      =   ZERO
                       MOVE    "0076"              TO  SPA-ERRCD  
                       GO  TO  450-TOUROKU-EXT
                   END-IF
                   INITIALIZE                  WRK-TANDOKU-AREA
               END-IF
               PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       21
                           OR      SPA-NAI-C02-BYOMEICD-O  (IDZ IDX1)
                                           =   SPACE
                   IF      SPA-NAI-C02-TANDOKUKBN-O  (IDZ IDX1) 
                                           =   ZERO
                       MOVE    1       TO  WRK-TANDOKUKBN
                   ELSE
                       MOVE    SPA-NAI-C02-BYOMEICD-O (IDZ IDX1)
                                       TO  WRK-TANDOKUKBN-BYOMEICD  
                       MOVE    SPA-NAI-C02-BYOMEI-O   (IDZ IDX1)
                                       TO  WRK-TANDOKUKBN-BYOMEI  
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF      WRK-TANDOKUKBN      =   ZERO
               MOVE    "0076"              TO  SPA-ERRCD  
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    病名編集
           IF      SPA-NAI-C02-SRYYMD   =   SPACE
               MOVE    SPA-NAI-C02-SYSYMD   TO  SPA-NAI-C02-SRYYMD
           END-IF
      *
           MOVE    SPACE           TO  WRK-PTBYOMEI-REC (1)
                                       WRK-PTBYOMEI-REC (2)
                                       WRK-PTBYOMEI-REC (3)
                                       WRK-PTBYOMEI-REC (4)
                                       WRK-PTBYOMEI-REC (5)
                                       WRK-PTBYOMEI-REC (6)
           INITIALIZE                  WRK-PTBYOMEI-REC (1)
                                       WRK-PTBYOMEI-REC (2)
                                       WRK-PTBYOMEI-REC (3)
                                       WRK-PTBYOMEI-REC (4)
                                       WRK-PTBYOMEI-REC (5)
                                       WRK-PTBYOMEI-REC (6)
           MOVE    ZERO            TO  IDXX
           MOVE    ZERO            TO  IDY
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
               IF      SPA-NAI-C02-BYOMEITOP (IDX)
                                           =   "1"
                   IF      IDXX            >   ZERO
                       PERFORM 4503-BYOMEICD-HENSYU-02-SEC
                   END-IF
                   ADD     1               TO  IDXX
                   MOVE    ZERO            TO  IDY
               END-IF
               PERFORM 4501-BYOMEICD-HENSYU-SEC
           END-PERFORM
           IF      IDXX            >   ZERO
               PERFORM 4503-BYOMEICD-HENSYU-02-SEC
           END-IF
      *
      *    病名重複チェック（画面・ＤＢ）
           MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                       WRK-ERR-SRYYMD
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6
                   OR      SPA-ERRCD           NOT =   SPACE
                   OR      WRK-PTBYO-BYOMEI (IDXX) =   SPACE
               PERFORM 4502-DABURI-CHK-SEC
               IF      SPA-ERRCD          NOT =   SPACE
                   MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                              TO  WRK-ERR-BYOMEI
                   MOVE    6                  TO  IDXX                  
               END-IF
           END-PERFORM
           IF      SPA-ERRCD        NOT =   SPACE
              GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    主病名の存在チェック
           IF      SPA-GMN-C02-SYUBYOFLG       =   SPACE
               IF      SPA-C02-2005-SYUBYOHENFLG   =   ZERO
                   CONTINUE
               ELSE
                   INITIALIZE                  PTBYOMEI-REC
                   MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   IF      SPA-C02-1001-HOSPSBT1   =   "1"
                       MOVE    "PTBYOMEI-KEY14"    TO  WRK-DBPATH
                   ELSE
                       MOVE    "PTBYOMEI-KEY15"    TO  WRK-DBPATH
                   END-IF
                   PERFORM 900-PTBYO-INV-SEC
                   IF      FLG-PTBYOMEI        =   1
                       MOVE    "0102"            TO  SPA-CIDCD
                       GO  TO  450-TOUROKU-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    登録処理
           PERFORM 4502-TOUROKU-SYORI-SEC
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       4501-BYOMEICD-HENSYU-SEC                SECTION.
      * 
      *    入力コード
           IF      IDX             <   7
               IF      SPA-GMN-C02-BYOMEICD (IDX)
                                   NOT =   SPACE    
                   ADD     1               TO  IDY
                   MOVE    SPA-GMN-C02-BYOMEICD (IDX)
                                   TO
                                   WRK-PTBYO-BYOMEIINPUTCD (IDXX IDY)
               END-IF
           END-IF
      *
           IF      SPA-NAI-C02-BYOMEICD (IDX)
                                       =   SPACE
               GO  TO  4501-BYOMEICD-HENSYU-EXT
           END-IF
      *
      *    病名コード・病名コード数
           IF      WRK-PTBYO-BYOMEICDSU (IDXX)     <=  21
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   IDZ     >       21
                       OR      SPA-NAI-C02-BYOMEICD-O  (IDX IDZ)
                                       =   SPACE
                   ADD     1           TO  WRK-PTBYO-BYOMEICDSU (IDXX)
                   IF      WRK-PTBYO-BYOMEICDSU (IDXX)
                                           <=   21
                       IF      SPA-NAI-C02-BYOMEICD-O (IDX IDZ)
                                                =   WRK-CONS-BYOMEICD
                           MOVE    22  TO  WRK-PTBYO-BYOMEICDSU (IDXX)
                       ELSE
                           MOVE    WRK-PTBYO-BYOMEICDSU (IDXX)
                                       TO  IDX1 
                           MOVE    SPA-NAI-C02-BYOMEICD-O (IDX IDZ)
                                        TO  WRK-PTBYO-BYOMEICD 
                                                        (IDXX IDX1)
                       END-IF
                   END-IF
      *
                   IF      WRK-PTBYO-BYOMEICDSU (IDXX)
                                       >   21
                       MOVE    21                  TO  IDZ
                   END-IF  
               END-PERFORM
           END-IF
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1    
               PERFORM 45011-BYOMEI-HENSYU-SEC
           END-IF
           .
       4501-BYOMEICD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数病名のときマスタより項目編集処理
      *****************************************************************
       45011-BYOMEI-HENSYU-SEC      SECTION.
      *
      *    病名    
           STRING  WRK-PTBYO-BYOMEI  (IDXX)
                                           DELIMITED  BY  SPACE
                   SPA-NAI-C02-BYOMEI(IDX) DELIMITED  BY  SPACE
                                   INTO    WRK-PTBYO-BYOMEI (IDXX)
           END-STRING
      *
      *    カルテ病名
           STRING  WRK-PTBYO-CHARTBYOMEI   (IDXX)
                                           DELIMITED  BY  SPACE
                   SPA-NAI-C02-CHARTBYOMEI (IDX)
                                          DELIMITED  BY  SPACE
                                   INTO   WRK-PTBYO-CHARTBYOMEI (IDXX)
           END-STRING
      *
      *    疑いフラグ
           IF      WRK-PTBYO-UTAGAIFLG (IDXX)  =   "3"
               CONTINUE
           ELSE
               EVALUATE    SPA-NAI-C02-UTAGAIFLG   (IDX)
                   WHEN    "1"
                       IF      WRK-PTBYO-UTAGAIFLG (IDXX)  =   "2"
                           MOVE    "3" TO  WRK-PTBYO-UTAGAIFLG (IDXX)
                       ELSE
                           MOVE    SPA-NAI-C02-UTAGAIFLG(IDX)
                                       TO  WRK-PTBYO-UTAGAIFLG (IDXX) 
                       END-IF    
                   WHEN    "2"
                       IF      WRK-PTBYO-UTAGAIFLG (IDXX)  =   "1"
                           MOVE    "3" TO  WRK-PTBYO-UTAGAIFLG (IDXX)
                        ELSE
                           MOVE    SPA-NAI-C02-UTAGAIFLG(IDX)
                                       TO  WRK-PTBYO-UTAGAIFLG (IDXX)
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    慢性区分
           IF    ( SPA-NAI-C02-TOKSKNCD (IDX)  NOT =   ZERO )
           AND   ( WRK-PTBYO-MANSEIKBN  (IDXX)     =   ZERO )  
               MOVE    SPA-NAI-C02-TOKSKNCD (IDX)
                                       TO  WRK-PTBYO-MANSEIKBN (IDXX)
           END-IF
      *    
      *    病名編集フラグ
           IF    ( SPA-NAI-C02-US-BYOMEIHENFLG (IDX) 
                                                   =   "1"   )
           AND   ( WRK-PTBYO-BYOMEIHENFLG (IDXX)   =   SPACE )  
               MOVE    "1"             TO  WRK-PTBYO-BYOMEIHENFLG(IDXX)
           END-IF
      *
      *    保険病名フラグ  
           IF    ( SPA-NAI-C02-US-HKNBYOMEIFLG (IDX) 
                                                   =   "1"   )
           AND   ( WRK-PTBYO-BYOMEIHENFLG (IDXX)   =   SPACE )  
               MOVE    "1"             TO  WRK-PTBYO-HKNBYOMEIFLG(IDXX)
           END-IF
           .                   
       45011-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理（単独病名・共通項目）
      *****************************************************************
       4503-BYOMEICD-HENSYU-02-SEC     SECTION.
      *
      *    単独病名のとき画面より編集
           IF      SPA-NAI-C02-BYOMEICD-MAX    =   1
               MOVE    SPA-GMN-C02-UTAGAIFLG
                                       TO  WRK-PTBYO-UTAGAIFLG (IDXX)
               MOVE    SPA-GMN-C02-SYUBYOFLG
                                       TO  WRK-PTBYO-SYUBYOFLG (IDXX)
               IF      SPA-GMN-C02-MANSEIKBN   =   SPACE
                   MOVE    ZERO        TO  WRK-PTBYO-MANSEIKBN (IDXX)
               ELSE                            
                   MOVE    SPA-GMN-C02-MANSEIKBN
                                       TO  WRK-PTBYO-MANSEIKBN (IDXX)
               END-IF                     
               MOVE    SPA-GMN-C02-NYUGAIKBN
                                       TO  WRK-PTBYO-NYUGAIKBN (IDXX)
               IF      SPA-GMN-C02-HKNCOMBI    =   ZERO
                   MOVE    ZERO        TO  WRK-PTBYO-HKNCOMBI  (IDXX)
               ELSE                          
                   MOVE    SPA-GMN-C02-HKNCOMBI
                                       TO  WRK-PTBYO-HKNCOMBI  (IDXX)
               END-IF                     
               MOVE    SPA-GMN-C02-REZEFLG
                                       TO  WRK-PTBYO-REZEFLG   (IDXX)
               MOVE    SPA-GMN-C02-REZEMM
                                       TO  WRK-PTBYO-REZEMM    (IDXX)
               MOVE    SPA-GMN-C02-BYOMEI
                                       TO  WRK-PTBYO-BYOMEI    (IDXX)
               MOVE    SPA-NAI-C02-BYOMEIMOJI
                                       TO  WRK-PTBYO-BYOMEIMOJI (IDXX)
               MOVE    SPA-GMN-C02-CHARTBYOMEI
                                       TO  WRK-PTBYO-CHARTBYOMEI (IDXX)
               MOVE    SPA-NAI-C02-CHARTBYOMEIMOJI
                                       TO
                                       WRK-PTBYO-CHARTBYOMEIMOJI (IDXX)
               MOVE    SPA-NAI-C02-BYOMEIHENFLG
                                       TO  WRK-PTBYO-BYOMEIHENFLG(IDXX)
               MOVE    SPA-GMN-C02-HKNBYOMEIFLG
                                       TO  WRK-PTBYO-HKNBYOMEIFLG(IDXX)
      *********MOVE    SPA-GMN-C02-HKNNUM
      *********                        TO  WRK-PTBYO-HKNNUM (IDXX)
           ELSE
      *        病名文字数
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   COMPUTE WRK-PTBYO-BYOMEIMOJI (IDXX)
                                       =   KANACHK-MAX  /   2        
               END-IF
      *        カルテ病名文字数
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-PTBYO-CHARTBYOMEI (IDXX)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   COMPUTE WRK-PTBYO-CHARTBYOMEIMOJI (IDXX)
                                       =   KANACHK-MAX  /   2        
               END-IF
           END-IF
      * 
      *    病名コードが２２以上のとき未コード化傷病名コードとする
           IF      WRK-PTBYO-BYOMEICDSU (IDXX) >   21
               INITIALIZE              WRK-PTBYO-BYOMEICD-G     (IDXX)
               MOVE    WRK-CONS-BYOMEICD
                                       TO  WRK-PTBYO-BYOMEICD  (IDXX 1)
               MOVE    1               TO  WRK-PTBYO-BYOMEICDSU(IDXX)
           END-IF
      *      
      *    基本病名コード・基本部位コード
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       21
                   OR      WRK-PTBYO-BYOMEICD (IDXX IDY)
                                       =   SPACE
               IF      WRK-PTBYO-BYOMEICD (IDXX IDY)(1:1)
                                               NOT =   "Z"
                   IF      WRK-PTBYO-KHNBYOMEICD (IDXX)    =   SPACE
                       MOVE    WRK-PTBYO-BYOMEICD (IDXX IDY)
                                       TO  WRK-PTBYO-KHNBYOMEICD (IDXX)
                   END-IF
               ELSE
                   IF      WRK-PTBYO-BYOMEICD (IDXX IDY) (1:4)
                                                   =   "ZZZ1"
                       ADD     1       TO  IDZ
                       IF      IDZ         <   4
                           MOVE    WRK-PTBYO-BYOMEICD (IDXX IDY)
                                       TO  WRK-PTBYO-KHNBUICD(IDXX IDZ)
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( WRK-PTBYO-KHNBYOMEICD(IDXX) NOT =   SPACE )
               AND   ( WRK-PTBYO-KHNBUICD (IDXX 3) NOT =   SPACE )
                   MOVE    21                  TO  IDY
               END-IF
           END-PERFORM
      *
      *    転帰区分・転帰日
           MOVE    SPA-GMN-C02-TENKIKBN
                                       TO  WRK-PTBYO-TENKIKBN (IDXX)
           MOVE    SPA-NAI-C02-TENKIYMD
                                       TO  WRK-PTBYO-TENKIYMD (IDXX)
      *
      *    レセ電エラーフラグ（編集病名のとき）
           IF      WRK-PTBYO-BYOMEIHENFLG (IDXX)   =   "1"
               EVALUATE    WRK-PTBYO-SYUBYOFLG (IDXX)
                   WHEN    "1"
                       IF      WRK-PTBYO-BYOMEIMOJI (IDXX) >   17
                           MOVE    "1" TO  WRK-PTBYO-RECEDENFLG (IDXX)
                       END-IF
                   WHEN    SPACE
                       IF      WRK-PTBYO-BYOMEIMOJI (IDXX) >   20
                           MOVE    "1" TO  WRK-PTBYO-RECEDENFLG (IDXX)
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    レセ電エラーフラグ（疑いフラグが設定されているとき）
           IF    ( WRK-PTBYO-RECEDENFLG (IDXX) =   SPACE ) 
           AND   ( WRK-PTBYO-UTAGAIFLG  (IDXX) 
                                           NOT =   SPACE )
               MOVE    SPACE               TO  WRK-HENFLG1
                                               WRK-HENFLG2
      *     
      *        病名文字数を計算
               COMPUTE WRK-MOJISU      =   WRK-PTBYO-BYOMEIMOJI (IDXX)
                                           *   2
               IF      WRK-PTBYO-UTAGAIFLG (IDXX)  =   "1" OR  "3"
                   IF      WRK-MOJISU      <   8
                       MOVE    1               TO  IDZ
                   ELSE    
                       COMPUTE IDZ     =   WRK-MOJISU    -   7
                   END-IF    
                   PERFORM VARYING IDY FROM    IDZ BY  2
                           UNTIL   IDY >       WRK-MOJISU
                       IF      WRK-PTBYO-BYOMEI (IDXX) (IDY:2) =   "疑"
                           MOVE    "1"              TO  WRK-HENFLG1
                           MOVE    WRK-MOJISU       TO  IDY
                       END-IF
                   END-PERFORM
               END-IF                
               IF      WRK-PTBYO-UTAGAIFLG (IDXX)  =   "2" OR  "3"
                   PERFORM VARYING IDY FROM    1   BY  2
                           UNTIL   IDY >       WRK-MOJISU
                       IF      WRK-PTBYO-BYOMEI (IDXX) (IDY:4) =  "急性"
                           MOVE    "1"              TO  WRK-HENFLG2
                           MOVE    WRK-MOJISU       TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        疑いフラグがセットされているとき
               IF    ( WRK-PTBYO-UTAGAIFLG (IDXX)
                                           =   "1" OR  "3" )
               AND   ( WRK-HENFLG1         =   SPACE       )                         
                   COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-UTAGAI-MOJISU
                   IF      WRK-PTBYO-BYOMEICD (IDXX 21) NOT =   SPACE             
                       EVALUATE    WRK-PTBYO-SYUBYOFLG (IDXX)
                           WHEN    "1"
                               IF      WRK-MOJISU      >   17
                                   MOVE    "1"     TO
                                           WRK-PTBYO-RECEDENFLG (IDXX)
                               END-IF
                           WHEN    SPACE
                               IF      WRK-MOJISU      >   20
                                   MOVE    "1"     TO
                                           WRK-PTBYO-RECEDENFLG (IDXX)
                               END-IF
                       END-EVALUATE
                   END-IF
               END-IF    
               IF    ( WRK-PTBYO-UTAGAIFLG (IDXX)
                                           =   "2" OR  "3" )
               AND   ( WRK-HENFLG2         =   SPACE       )                         
                   COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-KYUSEI-MOJISU
                   IF      WRK-PTBYO-BYOMEICD (IDXX 21) NOT =   SPACE             
                       EVALUATE    WRK-PTBYO-SYUBYOFLG (IDXX)
                           WHEN    "1"
                               IF      WRK-MOJISU      >   17
                                   MOVE    "1"     TO
                                           WRK-PTBYO-RECEDENFLG (IDXX)
                               END-IF
                           WHEN    SPACE
                               IF      WRK-MOJISU      >   20
                                   MOVE    "1"     TO
                                           WRK-PTBYO-RECEDENFLG (IDXX)
                               END-IF
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF    
      *****MOVE    SPA-GMN-C02-HKNNUM  TO  WRK-PTBYO-HKNNUM (IDXX)
           .
       4503-BYOMEICD-HENSYU-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力病名重複チェック処理
      *****************************************************************
       4502-DABURI-CHK-SEC        SECTION.
      *
      *    修飾語コードのみの病名のとき
           IF      WRK-PTBYO-BYOMEIHENFLG (IDXX)
                                       =   SPACE
               INITIALIZE                  WRK-BYOMEICD
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       21
                       OR      WRK-PTBYO-BYOMEICD  (IDXX IDZ)
                                           =   SPACE
                   IF      WRK-PTBYO-BYOMEICD  (IDXX IDZ)(1:1)
                                           NOT =   "Z"
                       MOVE    WRK-PTBYO-BYOMEICD  (IDXX IDZ)
                                               TO  WRK-BYOMEICD
                   END-IF
               END-PERFORM
      *
               IF      WRK-BYOMEICD        =   SPACE
                   MOVE    "0067"              TO  SPA-ERRCD  
                   GO  TO  4502-DABURI-CHK-EXT
               END-IF
           END-IF    
      *
      *    入力病名との重複チェック（複数病名のとき）
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               PERFORM VARYING IDY    FROM    IDXX    BY  1
                       UNTIL   IDY     >       6
                       OR      WRK-PTBYO-BYOMEI (IDY)  =   SPACE
                   IF      IDY             >   IDXX
                       IF      WRK-PTBYO-BYOMEI (IDXX)
                                           =   WRK-PTBYO-BYOMEI (IDY)
                           MOVE   "0055"           TO  SPA-ERRCD
                           MOVE    6               TO  IDY
                       END-IF
                   END-IF
               END-PERFORM
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4502-DABURI-CHK-EXT
               END-IF
           END-IF                   
      *
      *    マスタとの重複チェック
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                       TO  PTBYO-BYOMEI
           MOVE    WRK-PTBYO-NYUGAIKBN (IDXX)
                                       TO  PTBYO-NYUGAIKBN
           IF      PTBYO-NYUGAIKBN     =   SPACE
               MOVE    "PTBYOMEI-KEY13"    TO  WRK-DBPATH
           ELSE
               MOVE    "PTBYOMEI-KEY21"    TO  WRK-DBPATH
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C02-SRYYMD-OLD  =   PTBYO-SRYYMD
               AND     SPA-NAI-C02-SRYKA-OLD   =   PTBYO-SRYKA
               AND     SPA-NAI-C02-RENNUM      =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-SRYYMD  =   PTBYO-SRYYMD
                           MOVE    "0002"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-SRYYMD  <   PTBYO-SRYYMD
                           IF      SPA-NAI-C02-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     SPA-NAI-C02-TENKIYMD
                                                   >   PTBYO-TENKIYMD
                                   MOVE    "0038"          TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-SRYYMD  >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               IF    ( SPA-NAI-C02-TENKIYMD
                                               NOT =   SPACE          )
                               AND   ( SPA-NAI-C02-TENKIYMD
                                                   <=  PTBYO-TENKIYMD )
                                   CONTINUE
                               ELSE
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
                   IF      SPA-ERRCD               =   "0038"  OR 
                                                       "0002"
                       MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3    TO  WRK-ERR-SRYYMD
                   END-IF
               END-IF
      *
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .                   
       4502-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4502-TOUROKU-SYORI-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6
                   OR      WRK-PTBYO-BYOMEI (IDXX) =   SPACE
                   OR      SPA-ERRCD           NOT =   SPACE
               PERFORM 45022-PTBYOMEI-UPDATE-SEC
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
              GO  TO  4502-TOUROKU-SYORI-EXT
           END-IF
      *
      *    患者マスタ更新（転帰区分＝２）のとき
           IF      SPA-GMN-C02-TENKIKBN    =   "2"
               MOVE    SPA-HOSPID          TO  PTINF-HOSPID
               MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF               =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    "1"                 TO  PTINF-DEATHKBN 
      *
                   MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
                   MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "PTINF-KEY"         TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1004"              TO  SPA-ERRCD
                       DISPLAY "UPD PTINF-KEY =>" PTINF-KEY 
                       GO  TO  4502-TOUROKU-SYORI-EXT
                   END-IF
               END-IF
           END-IF     
      *
           IF      SPA-NAI-C02-SELNO   >   ZERO
               MOVE    SPA-NAI-C02-SELNO   TO  C02-BYO-ROW
           ELSE
               MOVE    ZERO                TO  C02-BYO-ROW
           END-IF
      *                   
           MOVE    SPACE               TO  WRK-HAISIYMDH
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           INITIALIZE                      SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-PTBYOMEI-REC
      *
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "PTBYOMEI-KEY16"    TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
      *
           IF      C02-BYO-ROW         =   ZERO
               MOVE    SPA-GMN-C02-MAX     TO  C02-BYO-ROW
           END-IF
           MOVE    ZERO                TO  FLG-ROW
           .
       4502-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名別入力項目編集処理
      *****************************************************************
       45022-PTBYOMEI-UPDATE-SEC                SECTION.
      *
      *    更新処理
           EVALUATE   SPA-NAI-C02-RENNUM
               WHEN    ZERO
      *            患者病名マスター追加
                   MOVE    ZERO                TO  WRK-RENNUM
                   INITIALIZE                  PTBYO-KEY
                   MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   MOVE    "PTBYOMEI-KEY7"     TO  WRK-DBPATH
                   PERFORM 900-PTBYO-INV-SEC
                   IF      FLG-PTBYOMEI        =   ZERO
                       MOVE    PTBYO-RENNUM    TO  WRK-RENNUM
                   END-IF
                   ADD     1                   TO  WRK-RENNUM
      *
                   MOVE    WRK-PTBYOMEI-REC (IDXX)
                                               TO  PTBYOMEI-REC 
                   MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                   MOVE    WRK-RENNUM          TO  PTBYO-RENNUM
      *
                   MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
                   MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
                   MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
      *
                   MOVE    "DBINSERT"          TO  MCP-FUNC
               WHEN    OTHER
                   IF    ( SPA-GMN-C02-BSRYKA  NOT =
                                       SPA-NAI-C02-SRYKA-OLD  )
                   OR    ( SPA-NAI-C02-SRYYMD  NOT =
                                       SPA-NAI-C02-SRYYMD-OLD )
      *                患者病名マスター更新（診療科変更のとき）
                       INITIALIZE                      PTBYO-KEY
                       MOVE    SPA-HOSPID              TO  PTBYO-HOSPID
                       MOVE    SPA-NAI-C02-PTID        TO  PTBYO-PTID
                       MOVE    SPA-NAI-C02-SRYKA-OLD   TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD-OLD  TO  PTBYO-SRYYMD
                       MOVE    SPA-NAI-C02-RENNUM      TO  PTBYO-RENNUM 
                       MOVE    PTBYOMEI-REC            TO  MCPDATA-REC
                       MOVE    "DBDELETE"              TO  MCP-FUNC
                       MOVE    "PTBYOMEI-KEY"          TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"          USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                       IF      MCP-RC              NOT =   ZERO 
                           MOVE    "1002"                  TO  SPA-ERRCD
                           DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
                           GO  TO  45022-PTBYOMEI-UPDATE-EXT
                       END-IF
      *
                       MOVE    ZERO                TO  WRK-RENNUM
                       INITIALIZE                  PTBYO-KEY
                       MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
                       MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                       MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                       MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                       MOVE    "PTBYOMEI-KEY7"     TO  WRK-DBPATH
                       PERFORM 900-PTBYO-INV-SEC
                       IF      FLG-PTBYOMEI        =   ZERO
                           MOVE    PTBYO-RENNUM    TO  WRK-RENNUM
                       END-IF
                       ADD     1                   TO  WRK-RENNUM
                       MOVE    WRK-PTBYOMEI-REC (IDXX)
                                                   TO  PTBYOMEI-REC
                       MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
                       MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                       MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                       MOVE    WRK-RENNUM          TO  PTBYO-RENNUM
      *
                       MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
                       MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
                       MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
      *
                       MOVE    "DBINSERT"          TO  MCP-FUNC
                   ELSE
      *                患者病名マスター更新（診療科変更なし）
                       INITIALIZE                  PTBYO-KEY
                       MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
                       MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                       MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                       MOVE    SPA-NAI-C02-RENNUM  TO  PTBYO-RENNUM 
                       MOVE    "PTBYOMEI-KEY"      TO  WRK-DBPATH
                       PERFORM 900-PTBYO-INV-SEC
                       IF      FLG-PTBYOMEI        =   1
                           MOVE    "1003"          TO  SPA-ERRCD
                           DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
                           GO  TO  45022-PTBYOMEI-UPDATE-EXT
                       ELSE
                           MOVE    PTBYO-KEY       TO
                                                   WRK-PTBYO-KEY (IDXX)
                           MOVE    WRK-PTBYOMEI-REC (IDXX)
                                                   TO  PTBYOMEI-REC
      *
                           MOVE    SMCNDATE-YMD    TO  PTBYO-UPYMD
                           MOVE    SMCNDATE-HMS    TO  PTBYO-UPHMS
      *
                           MOVE    "DBUPDATE"      TO  MCP-FUNC
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "PTBYOMEI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" PTBYO-KEY 
           END-IF
      *
          .
       45022-PTBYOMEI-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名検索へ
      *****************************************************************
       600-C05-SYORI-SEC           SECTION.
      *
           IF      SPA-NAI-C02-PTID    =   ZERO
           OR      SPA-GMN-C02-SRYKA   =   SPACE
      *****OR      SPA-GMN-C02-CUR     =   27  OR  26
               MOVE    "0050"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *     
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0058"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *     
           IF      SPA-NAI-C02-USERBYOMEIFLG (1)   =  "1"
               MOVE    "0065"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *     
           IF      SPA-NAI-C02-BYOMEIHENFLG        =  "1"
               MOVE    "0078"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *
           IF      SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE
               MOVE    "0090"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *
      *    画面移行用のパラメタ変更
      *    受付と患者検索以外の元をセット
           MOVE    "C05"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    "C02"               TO  SPA-C01-MOTOPG
                                           SPA-MOTOPG
           MOVE    ZERO                TO  IDY
      * 
           IF      SPA-NAI-C02-BYOMEIMOJI          >   ZERO
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                   IF      SPA-NAI-C02-BYOMEICD (IDX)  NOT =   SPACE
                       ADD     1               TO  IDY              
                       MOVE    SPA-NAI-C02-BYOMEICD (IDX)
                                           TO  SPA-C01-TBYOMEICD (IDY)
                       MOVE    SPA-NAI-C02-BYOMEI   (IDX)
                                           TO  SPA-C01-TBYOMEI   (IDY)
                   END-IF                              
               END-PERFORM
           END-IF 
      *
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "C05    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       600-C05-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名登録へ
      *****************************************************************
       600-C03-SYORI-SEC           SECTION.
      *
      *    画面移行用のパラメタ変更
      *    受付と患者検索以外の元をセット
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "C03"               TO  SPA-SAKIPG
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "C03    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       600-C03-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント画面へ
      *****************************************************************
       430-FREEMSG-SEC              SECTION.
      *
      *    修正のチェック
           IF      SPA-GMN-C02-BANGO   NOT =   ZERO
               MOVE    "0073"          TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF
      *
           IF      SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE
               MOVE    "0090"              TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC07"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
      *    カーソル移動
           MOVE    "COMMENT"           TO  MCP-WIDGET
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "C07"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "C07"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-FREEMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴登録画面へ
      *****************************************************************
       430-OPECOM-SEC              SECTION.
      *
      *    修正のチェック
           IF      SPA-GMN-C02-BANGO   NOT =   ZERO
               MOVE    "0073"          TO  SPA-ERRCD
               GO      TO      430-OPECOM-EXT
           END-IF
      *
           IF      SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE
               MOVE    "0090"              TO  SPA-ERRCD
               GO      TO      430-OPECOM-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC11"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
      *    カーソル移動
           IF      SPA-NAI-C11-PTID    =   ZERO
               MOVE    "PTNUM"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "C11"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "C11"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-OPECOM-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示切替え処理
      *****************************************************************
       430-SYOKBN-CHANGE-SEC          SECTION.
      *
           EVALUATE    SPA-GMN-C02-SYOKBN
               WHEN    ZERO
                   MOVE    "1"         TO  SPA-GMN-C02-SYOKBN
               WHEN    "1"
                   MOVE    "2"         TO  SPA-GMN-C02-SYOKBN
               WHEN    "2"
                   MOVE    ZERO        TO  SPA-GMN-C02-SYOKBN
           END-EVALUATE
           MOVE    SPA-GMN-C02-SYOKBN  TO  WRK-KBN
           PERFORM 5001-SYOKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SYOKBNMEI
           IF      WRK-KBNMEI          =   SPACE  
               MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                           TO  SPA-GMN-C02-SYOKBN-G
           END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           MOVE    "PTBYOMEI-KEY16"     TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
           MOVE    1                   TO  SPA-GMN-C02-CUR
      *
           .
       430-SYOKBN-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分表示切替処理
      *****************************************************************
       430-HNYUGAIKBN-CHANGE-SEC          SECTION.
      *
           EVALUATE    SPA-GMN-C02-HNYUGAIKBN
               WHEN    ZERO
                   MOVE    "1"         TO  SPA-GMN-C02-HNYUGAIKBN
               WHEN    "1"
                   MOVE    "2"         TO  SPA-GMN-C02-HNYUGAIKBN
               WHEN    "2"
                   MOVE    ZERO        TO  SPA-GMN-C02-HNYUGAIKBN
           END-EVALUATE
           MOVE    SPA-GMN-C02-HNYUGAIKBN  TO  WRK-KBN
           PERFORM 5001-HNYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HNYUGAIKBNMEI
           IF      WRK-KBNMEI          =   SPACE  
               MOVE    SPA-GMN-C02-HNYUGAIKBNLST (1)
                                           TO  SPA-GMN-C02-HNYUGAIKBN-G
           END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           MOVE    "PTBYOMEI-KEY16"    TO  WRK-DBPATH
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
           MOVE    1                   TO  SPA-GMN-C02-CUR
      *
           .
       430-HNYUGAIKBN-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード表示へ
      *****************************************************************
       490-C09-SYORI-SEC           SECTION.
      *
           IF    ( SPA-GMN-C02-BYOMEI  =   SPACE )
           OR    ( SPA-NAI-C02-BYOMEIMOJI
                                       =   SPACE )
      *****OR    ( SPA-NAI-C02-BYOMEICD-MAX
      *****                            >   1     )
               GO  TO  490-C09-SYORI-EXT
           END-IF
      *  
           MOVE    SPACE               TO  C09
      *
           MOVE    ZERO                TO  IDZ
                                           IDXX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
               IF      SPA-NAI-C02-BYOMEICD (IDX)  NOT =   SPACE
                   IF    ( SPA-NAI-C02-BYOMEITOP (IDX) =   "1"  )
                   AND   ( IDXX                        >   ZERO )
                       MOVE    ZERO                    TO  IDXX
                       ADD     1                       TO  IDZ
                   END-IF  
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       21
                           OR      SPA-NAI-C02-BYOMEICD-O (IDX IDY)
                                               =   SPACE
                        ADD     1                   TO  IDZ
                                                       IDXX
                       IF      IDZ                 <=   100
                           MOVE    IDXX        TO  WRK-GMN-BANGO
                           MOVE    WRK-GMN-BANGO (4:2)
                                               TO  C09-TBANGO    (IDZ)
                           MOVE    SPA-NAI-C02-BYOMEICD-O (IDX IDY)
                                               TO  C09-TBYOMEICD (IDZ)
                           MOVE    SPA-NAI-C02-BYOMEI-O   (IDX IDY)
                                               TO  C09-TBYOMEI   (IDZ)
                           MOVE    IDZ             TO  C09-COUNT
                       ELSE
                           MOVE    21               TO  IDX
                                                        IDY
                       END-IF 
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C02"                TO  SPA-MOTOPG
           MOVE    "C09"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-C09-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       480-KENSAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "C02"               TO  SPA-MOTOPG3
      *     
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "U01    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名検索画面編集処理
      *****************************************************************
       490-USERBYOMEI-KENSAKU-SEC      SECTION.
      *
           IF      SPA-NAI-C02-PTID    =   ZERO
           OR      SPA-GMN-C02-SRYKA   =   SPACE
      *****OR      SPA-GMN-C02-CUR     =   27  OR  26
               MOVE    "0050"              TO  SPA-ERRCD
               GO  TO  490-USERBYOMEI-KENSAKU-EXT
           END-IF
      *
           IF      SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE
               MOVE    "0090"              TO  SPA-ERRCD
               GO  TO  490-USERBYOMEI-KENSAKU-EXT
           END-IF
      *
           IF    ( SPA-GMN-C02-BANGO       NOT =   SPACE )
           AND   ( SPA-NAI-C02-BYOMEICD-MAX    >   1     )
               MOVE    "0056"             TO  SPA-ERRCD
               GO  TO  490-USERBYOMEI-KENSAKU-EXT 
           END-IF
      *
      *    変更処理のとき自院コードの入力がない場合
           IF    ( SPA-GMN-C02-BANGO   
                                   NOT =   SPACE ) 
           AND   ( SPA-GMN-C02-BYOMEICD-G
                                       =   SPACE )
           AND   (SPA-NAI-C02-BYOMEICD (1)
                                   NOT =   SPACE )       
               MOVE    "0069"              TO  SPA-ERRCD
               GO  TO  490-USERBYOMEI-KENSAKU-EXT 
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE             
                   MOVE    ZERO                TO  WRK-LENGTH                    
                   PERFORM VARYING IDZ FROM    10  BY  -1
                           UNTIL   IDZ <       1
                       IF      SPA-GMN-C02-BYOMEICD(IDX)(IDZ:1)
                                           NOT =   SPACE
                           MOVE    IDZ             TO  WRK-LENGTH
                           MOVE    1               TO  IDZ
                       END-IF
                       IF      WRK-LENGTH           =   7    
                           MOVE    "0069"              TO  SPA-ERRCD
                           GO  TO  490-USERBYOMEI-KENSAKU-EXT 
                       END-IF
                   END-PERFORM
           END-PERFORM
      *
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    "C02"               TO  SPA-C01-MOTOPG
      *    
           MOVE    ZERO                TO  IDY
      * 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE
                   ADD     1               TO  IDY              
                   MOVE    SPA-GMN-C02-BYOMEICD (IDX)
                                           TO  SPA-C01-TBYOMEICD (IDY)
      *************MOVE    SPA-NAI-C02-BYOMEI   (IDX)
      *************                        TO  SPA-C01-TBYOMEI   (IDY)
               END-IF                              
           END-PERFORM
      *
           MOVE    WRK-BUNRUICD        TO  SPA-C01-BUNRUICD 
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC04"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
      *****MOVE    "BUNRUICD"           TO  MCP-WIDGET
      *
           MOVE    "C02"                TO  SPA-MOTOPG
           MOVE    "C04"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "C04"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-USERBYOMEI-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑いフラグ編集処理
      *****************************************************************
       5001-UTAGAIFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-UTAGAIFLG-MAX
                   IF      WRK-KBN (1:1) 
                                  =   SPA-GMN-C02-UTAGAIFLGL (IDY)
                       MOVE    SPA-GMN-C02-UTAGAIFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-UTAGAIFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-UTAGAIFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性フラグ編集処理
      *****************************************************************
       5001-MANSEIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
                                          WRK-KBNMEITAN
      *     
           IF      WRK-KBN (1:2)      =   SPACE   OR  ZERO
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-MANSEIKBN-MAX
                   IF      WRK-KBN (1:2)
                                  =   SPA-GMN-C02-MANSEIKBNL (IDY)
                       MOVE    SPA-GMN-C02-MANSEIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-MANSEIKBNTANL(IDY)
                                          TO  WRK-KBNMEITAN                   
                       MOVE    SPA-GMN-C02-MANSEIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-MANSEIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰区分編集処理
      *****************************************************************
       5001-TENKIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN  =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-TENKIKBN-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C02-TENKIKBNL (IDY)
                       MOVE    SPA-GMN-C02-TENKIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-TENKIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主病フラグ編集処理
      *****************************************************************
       5001-SYUBYOFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-SYUBYOFLG-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C02-SYUBYOFLGL (IDY)
      **************** MOVE    SPA-GMN-C02-SYUBYOFLGMEIL (IDY) (1:6)
                       MOVE    SPA-GMN-C02-SYUBYOFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-SYUBYOFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       5001-SRYKA-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-SRYKA-MAX
                   IF      WRK-KBN    =   SPA-GMN-C02-SRYKAL (IDY)
                       MOVE    SPA-GMN-C02-SRYKAMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-SRYKA-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科（明細欄）編集処理
      *****************************************************************
       5001-BSRYKA-HENSYU-SEC      SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-BSRYKA-MAX
                   IF      WRK-KBN    =   SPA-GMN-C02-BSRYKAL (IDY)
                       MOVE    SPA-GMN-C02-BSRYKAMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-BSRYKA-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-BSRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       5001-NYUGAIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-NYUGAIKBN-MAX
                       OR      SPA-GMN-C02-NYUGAIKBNL (IDY) =   SPACE
                   IF      WRK-KBN    =   SPA-GMN-C02-NYUGAIKBNL (IDY)
                       MOVE    SPA-GMN-C02-NYUGAIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-NYUGAIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-NYUGAIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分表示切替編集処理
      *****************************************************************
       5001-HNYUGAIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-HNYUGAIKBN-MAX
                       OR      SPA-GMN-C02-HNYUGAIKBNL (IDY) =   SPACE
                   IF      WRK-KBN    =   SPA-GMN-C02-HNYUGAIKBNL (IDY)
                       MOVE    SPA-GMN-C02-HNYUGAIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-HNYUGAIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-HNYUGAIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       5001-HKNCOMBI-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
           MOVE    ZERO               TO  WRK-TEKEDYMD
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-HKNCOMBI-MAX
                   IF      WRK-KBN        =
                                          SPA-GMN-C02-HKNCOMBIL (IDY)
                       MOVE    SPA-GMN-C02-HKNCOMBIMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-NAI-C02-TEKSTYMD     (IDY)
                                          TO  WRK-TEKSTYMD
                       MOVE    SPA-NAI-C02-TEKEDYMD     (IDY)
                                          TO  WRK-TEKEDYMD
                       MOVE    SPA-GMN-C02-HKNCOMBI-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険病名フラグ編集処理
      *****************************************************************
       5001-HKNBYOMEIFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-HKNBYOMEIFLG-MAX
                   IF      WRK-KBN     =
                                       SPA-GMN-C02-HKNBYOMEIFLGL (IDY)
                       MOVE    SPA-GMN-C02-HKNBYOMEIFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-HKNBYOMEIFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-HKNBYOMEIFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト表示フラグ編集処理
      *****************************************************************
       5001-REZEFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-REZEFLG-MAX
                   IF      WRK-KBN     =   SPA-GMN-C02-REZEFLGL (IDY)
                       MOVE    SPA-GMN-C02-REZEFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-REZEFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-REZEFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分編集処理
      *****************************************************************
       5001-SYOKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-SYOKBN-MAX
                   IF      WRK-KBN (1:1)  =   SPA-GMN-C02-SYOKBNL (IDY)
                       MOVE    SPA-GMN-C02-SYOKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-SYOKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-SYOKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *----(01.00.03) LINE ADD START ----------------------------------
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0017"          TO  SPA-ERRCD
           END-IF
      *----(01.00.03) LINE ADD END   ----------------------------------
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
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD2-G  TO  WRK-NENGO
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
           IF      SPA-CIDCD       NOT =   SPACE
               PERFORM 520-CIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD2      NOT =   SPACE
               PERFORM 530-CIDSET2-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD4      NOT =   SPACE
               PERFORM 540-CIDSET4-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "C02    "           TO  MCP-WINDOW.
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
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-C02-BYO-ROW
           ELSE
               MOVE    C02-BYO-ROW         TO  WRK-C02-BYO-ROW
           END-IF
      *
           MOVE    SPACE               TO  C02
      *
           MOVE    SPA-GMN-C02-PTNUM       TO  C02-PTNUM
           MOVE    SPA-GMN-C02-SYSYMD      TO  C02-SYSYMD
           MOVE    SPA-GMN-C02-KANANAME    TO  C02-KANANAME
           MOVE    SPA-GMN-C02-NAME        TO  C02-NAME
           MOVE    SPA-GMN-C02-SEX         TO  C02-SEX
           MOVE    SPA-GMN-C02-BIRTHDAY    TO  C02-BIRTHDAY
           MOVE    SPA-GMN-C02-HKNCOMBIMEIH
                                           TO  C02-HKNNAME
           MOVE    SPA-GMN-C02-FTNRATE     TO  C02-PTFTNRATE
           MOVE    SPA-GMN-C02-SRYKA-G     TO  C02-SRYKA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-SRYKA-MAX
               MOVE    SPA-GMN-C02-SRYKALST (IDX)
                                               TO  C02-SRYKALST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-SRYKA-MAX   TO  C02-SRYKA-COUNT
           MOVE    SPA-GMN-C02-SYOKBN-G    TO  C02-SYOKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-SYOKBN-MAX
               MOVE    SPA-GMN-C02-SYOKBNLST (IDX)
                                               TO  C02-SYOKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-SYOKBN-MAX  TO  C02-SYOKBN-COUNT
           MOVE    SPA-GMN-C02-BSRYKA-G    TO  C02-BSRYKA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-BSRYKA-MAX
               MOVE    SPA-GMN-C02-BSRYKALST (IDX)
                                               TO  C02-BSRYKALST(IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-BSRYKA-MAX  TO  C02-BSRYKA-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-MAX
               MOVE    SPA-GMN-C02-TBANGO    (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C02-TBANGO    (IDX)
               IF      SPA-GMN-C02-SRYKA   =   ZERO
                   MOVE    SPA-GMN-C02-TSRYKA     (IDX)
                                           TO  C02-TSRYKA    (IDX)
               END-IF
               MOVE    SPA-GMN-C02-TSRYYMD    (IDX)
                                           TO  C02-TSRYYMD   (IDX)
               MOVE    SPA-GMN-C02-TBYOMEI    (IDX)
                                           TO  C02-TBYOMEI   (IDX)
               MOVE    SPA-GMN-C02-TSYUBYOFLG (IDX)
                                           TO  C02-TSYUBYOFLG(IDX)
               MOVE    SPA-GMN-C02-TMANSEIKBN (IDX)
                                           TO  C02-TMANSEIFLG(IDX)
               MOVE    SPA-GMN-C02-TNYUGAIKBN (IDX)
                                           TO  C02-TNYUGAIKBN(IDX)
               MOVE    SPA-GMN-C02-TUTAGAIFLG (IDX)
                                           TO  C02-TUTAGAIFLG(IDX)
               MOVE    SPA-GMN-C02-THKNCOMBI  (IDX)
                                           TO  C02-THKNCOMBI (IDX)
               MOVE    SPA-GMN-C02-TREZEFLG (IDX)
                                           TO  C02-TREZEFLG (IDX)
               IF      SPA-GMN-C02-TREZEMM  (IDX)    NOT =   ZERO
                   MOVE    SPA-GMN-C02-TREZEMM (IDX)
                                           TO  C02-TREZEMM (IDX)(2:)
               END-IF
               MOVE    SPA-GMN-C02-TTENKIKBN  (IDX)
                                           TO  C02-TTENKIKBN  (IDX)
               MOVE    SPA-GMN-C02-TTENKIYMD (IDX)
                                           TO  C02-TTENKIYMD (IDX)
      *----(01.00.03) LINE ADD START ----------------------------------
               MOVE    SPA-GMN-C02-THKNBYOMEIFLG (IDX)
                                           TO  C02-THKNBYOMEIFLG (IDX)
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      ***      MOVE    IDX                 TO  C02-BYO-COUNT
      *         
               IF      IDX             =   SPA-NAI-C02-SELNO
                   MOVE    "T"             TO  C02-BYO-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  C02-BYO-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-C02-MAX         TO  C02-BYO-COUNT
           MOVE    ZERO                    TO  C02-BYO-ROW
           IF      WRK-C02-BYO-ROW         >   ZERO
               MOVE    WRK-C02-BYO-ROW         TO  C02-BYO-ROW
           END-IF
      *
           IF      SPA-GMN-C02-BANGO-X NOT =   SPACE
               MOVE    SPA-GMN-C02-BANGO-X     TO  C02-BANGO
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-C02-BANGO       TO  SNUM-INX
               CALL    "ORCSNUM"               USING
                                               ORCSNUMAREA
               IF      SNUM-RC                 =   ZERO
                   PERFORM VARYING IDX FROM    11  BY  1
                           UNTIL   IDX >       15
                       IF      SNUM-OUTEDT (IDX:1) NOT =   ZERO
                           MOVE    SNUM-OUTEDT (IDX:)  TO  C02-BANGO
                           MOVE    15                  TO  IDX
                       END-IF
                   END-PERFORM
               END-IF             
           END-IF
           MOVE    SPA-GMN-C02-SRYYMD      TO  C02-SRYYMD
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      6
               MOVE    SPA-GMN-C02-BYOMEICD(IDX)
                                              TO  C02-BYOMEICD (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-BYOMEI         TO  C02-BYOMEI
           MOVE    SPA-GMN-C02-BYOMEIHENFLG   TO  C02-BYOMEIHENFLG-VALUE
           MOVE    "black"                    TO  C02-BYOMEIHENFLG-STYLE
           IF    ( SPA-NAI-C02-BYOMEIHENFLG   =   "1" )
           AND   ( SPA-NAI-C02-RECEDENFLG     =   "1" )
               MOVE    "red"                  TO  C02-BYOMEIHENFLG-STYLE
           END-IF
           MOVE    SPA-GMN-C02-CHARTBYOMEI    TO  C02-CHARTBYOMEI
      *
           MOVE    SPA-GMN-C02-REZEMM         TO  C02-REZEMM
           MOVE    SPA-GMN-C02-TENKIYMD       TO  C02-TENKIYMD
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-HKN-MAX
               IF      SPA-GMN-C02-THKNCOMBINUM(IDX)    NOT =   ZERO
                   MOVE    SPA-GMN-C02-THKNCOMBINUM(IDX)
                                               TO  C02-HKNCOMBINUM(IDX)
               END-IF
               MOVE    SPA-GMN-C02-THKNCOMBIMEI(IDX)
                                               TO  C02-HKNCOMBIMEI(IDX)
      *
      ****     MOVE    IDX                     TO  C02-HKN-COUNT
           END-PERFORM
           MOVE    SPA-GMN-C02-HKN-MAX    TO  C02-HKN-COUNT
           MOVE    ZERO                   TO  C02-HKN-ROW 
      *
           MOVE    SPA-GMN-C02-TENKIKBN-G TO  C02-TENKIKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-C02-TENKIKBNL  (IDX)   =   SPACE
               MOVE    SPA-GMN-C02-TENKIKBNLST (IDX)
                                           TO  C02-TENKIKBN-ITEM (IDX)
               MOVE    IDX                 TO  C02-TENKIKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-UTAGAIFLG-G TO  C02-UTAGAIFLG
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-C02-UTAGAIFLGL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-UTAGAIFLGLST (IDX)
                                           TO  C02-UTAGAIFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-UTAGAIFLG-COUNT    
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-SYUBYOFLG-G TO  C02-SYUBYOFLG
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-C02-SYUBYOFLGL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-SYUBYOFLGLST (IDX)
                                           TO  C02-SYUBYOFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-SYUBYOFLG-COUNT  
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-MANSEIKBN-G TO  C02-MANSEIFLG
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-C02-MANSEIKBNL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-MANSEIKBNLST (IDX)
                                           TO  C02-MANSEIFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-MANSEIFLG-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-NYUGAIKBN-G TO  C02-NYUGAIKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-C02-NYUGAIKBNL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-NYUGAIKBNLST (IDX)
                                           TO  C02-NYUGAIKBN-ITEM (IDX)
               MOVE    IDX                 TO  C02-NYUGAIKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-HNYUGAIKBN-G
                                       TO  C02-HNYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-HNYUGAIKBN-MAX
               MOVE    SPA-GMN-C02-HNYUGAIKBNLST (IDX)
                                           TO  C02-HNYUGAIKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-HNYUGAIKBN-MAX
                                       TO  C02-HNYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-C02-HKNCOMBI-G TO  C02-HKNCOMBI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-HKNCOMBI-MAX
               MOVE    SPA-GMN-C02-HKNCOMBILST (IDX)
                                           TO  C02-HKNCOMBI-ITEM (IDX)
      *****    MOVE    IDX                 TO  C02-HKNCOMBI-COUNT
           END-PERFORM
           MOVE    SPA-GMN-C02-HKNCOMBI-MAX    TO  C02-HKNCOMBI-COUNT
      *
           MOVE    SPA-GMN-C02-HKNBYOMEIFLG-G TO  C02-HKNBYOMEIFLG
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-C02-HKNBYOMEIFLGL(IDX) =   SPACE
               MOVE    SPA-GMN-C02-HKNBYOMEIFLGLST (IDX)
                                       TO  C02-HKNBYOMEIFLG-ITEM (IDX)
               MOVE    IDX             TO  C02-HKNBYOMEIFLG-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-REZEFLG-G TO  C02-REZEFLG
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-C02-REZEFLGL  (IDX)   =   SPACE
               MOVE    SPA-GMN-C02-REZEFLGLST (IDX)
                                           TO  C02-REZEFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-REZEFLG-COUNT
           END-PERFORM
      *
      *    
      **** IF    ( SPA-C02-MOTOPG      =   "K02"   OR  "U02" 
      *                                             OR  "K02N")
      *     AND   ( SPA-PTNUM       NOT =   SPACE             )
      *         MOVE    WIDGET-INSENSITIVE  TO  C02-PTNUM-STATE
      *         MOVE    WIDGET-INSENSITIVE  TO  C02-B03-STATE
      *                                         C02-B09-STATE
      ***     ELSE
              MOVE    WIDGET-NORMAL        TO  C02-PTNUM-STATE
                                               C02-B03-STATE
                                               C02-B09-STATE
      ***     END-IF
      *
           EVALUATE    SPA-GMN-C02-SYOKBN
               WHEN    ZERO
                   MOVE    "当月"          TO  C02-B21-LABEL
               WHEN    "1"
                   MOVE    "前月"          TO  C02-B21-LABEL
               WHEN    "2"
                   MOVE    "全体"          TO  C02-B21-LABEL
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-C02-HNYUGAIKBN
               WHEN    ZERO
                   MOVE    "入・外"        TO  C02-B23-LABEL
               WHEN    "1"
                   MOVE    "入  院"        TO  C02-B23-LABEL
               WHEN    "2"
                   MOVE    "入院外"        TO  C02-B23-LABEL
           END-EVALUATE
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
         EVALUATE    SPA-GMN-C02-CUR
              WHEN    01
                     MOVE  "BANGO"      TO  MCP-WIDGET
             WHEN    9
                     MOVE  "SRYYMD"     TO  MCP-WIDGET
             WHEN    2
                     MOVE  "BYOMEICD_1" TO  MCP-WIDGET
             WHEN    3
                     MOVE  "BYOMEICD_2" TO  MCP-WIDGET
             WHEN    4
                     MOVE  "BYOMEICD_3" TO  MCP-WIDGET
             WHEN    5
                     MOVE  "BYOMEICD_4" TO  MCP-WIDGET
             WHEN    6
                     MOVE  "BYOMEICD_5" TO  MCP-WIDGET
             WHEN    7
                     MOVE  "BYOMEICD_6" TO  MCP-WIDGET
             WHEN    18
                     MOVE  "UTAGAIFLG"  TO  MCP-WIDGET
             WHEN    19
                     MOVE  "SYUBYOFLG"  TO  MCP-WIDGET
             WHEN    20
                     MOVE  "MANSEIFLG"  TO  MCP-WIDGET
             WHEN    21
                     MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
             WHEN    22
                     MOVE  "HKNCOMBI"   TO  MCP-WIDGET
             WHEN    23
                     MOVE  "REZEFLG"    TO  MCP-WIDGET
             WHEN    24
                     MOVE  "REZEMM"     TO  MCP-WIDGET
             WHEN    17
                     MOVE  "TENKIKBN"   TO  MCP-WIDGET
             WHEN    13
                     MOVE  "TENKIYMD"   TO  MCP-WIDGET
             WHEN    8
                     MOVE  "BYOMEI"   TO  MCP-WIDGET
             WHEN    28
                     MOVE  "CHARTBYOMEI"
                                      TO  MCP-WIDGET
      *----(01.00.03) LINE MOD START ----------------------------------
             WHEN    25
                     MOVE  "HKNBYOMEIFLG"
                                      TO  MCP-WIDGET
      *----(01.00.03) LINE MOD END   ----------------------------------
             WHEN   26
                    MOVE  "PTNUM"    TO  MCP-WIDGET
             WHEN   27
                    MOVE  "SRYKA"    TO  MCP-WIDGET
             WHEN   30
                    MOVE  "PTNUM"    TO  MCP-WIDGET
             WHEN   31
                    MOVE  "BSRYKA"   TO  MCP-WIDGET
             WHEN   32
                    MOVE  "SYOKBN"   TO  MCP-WIDGET
             WHEN   33
                    MOVE  "HNYUGAIKBN"
                                     TO  MCP-WIDGET
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
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。"  DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   MOVE    "警告メッセージ"            TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが選択してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "病名コード順がエラーです"  TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "病名が長すぎます"          TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "該当の保険組合せがありません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "病名が未入力か入力の途中です"
                                                       TO  SPA-ERRMSG
               WHEN    "0011"
               MOVE "開始日が該当の保険組合せの適用日の範囲外です"
                                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "病名番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "年号入力エラー"            TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "数字で入力してください"    TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "開始日付入力エラー"        TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "転帰日付入力エラー"        TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "日付入力エラー"            TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "疑い入力エラー"            TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "主病名入力エラー"          TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "既に主病名が入力されています"
                                                       TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "慢性区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "入外区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "レセプト表示入力エラー"    TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "転帰区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "転帰年月日が入力してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "転帰年月日が入力してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "開始日付以前の転帰日付が入力してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "保険病名入力エラー"        TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "基本病名が入力してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "診療科目入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "該当の患者が存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "カルテ病名が長すぎます"    TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "これ以上追加できません"    TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "不明の病名コードが入力されています"
                                               TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "転帰区分が入力してあります"
                                               TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "転帰区分が入力してありません"
                                               TO  SPA-ERRMSG
               WHEN    "0038"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。転帰日等を確認して下さい"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0050"
                   MOVE    "患者番号・診療科の入力がありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0051"
                   MOVE    "入力がありません"  TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE    "自院病名マスタが変更されています"
                                                       TO  SPA-ERRMSG
               WHEN    "0053"
                   MOVE    "自院病名コードがありません" TO  SPA-ERRMSG
               WHEN    "0054"
                   MOVE    "病名コードがありません"    TO  SPA-ERRMSG
               WHEN    "0055"
                   STRING   WRK-ERR-BYOMEI     DELIMITED   BY  SPACE
                            "が複数入力されています"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0056"
                   MOVE    "変更処理のときは複数病名の入力はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0057"
                   MOVE    "すべて病名が登録できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0058"
                   MOVE    "複数病名の入力中なので病名検索はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0059"
                   MOVE    "接頭語しか入力できません"  TO  SPA-ERRMSG
               WHEN    "0060"
                   MOVE
                 "複数の病名を入力中ですので、この項目の編集は無効です"
                                                       TO  SPA-ERRMSG
               WHEN    "0061"
                   MOVE    "自院病名の病名・カルテ病名は変更できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0062"
                   MOVE
               "未コード化傷病名は他のコードといっしょに入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0063"
                   MOVE
                 "未コード化傷病名が入力されているので入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0064"
                   MOVE    "自院病名が入力されているので入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0065"
                 MOVE    "自院病名の入力があるので病名検索はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0066"
                   MOVE    "複数の病名が入力してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0067"
                   STRING   WRK-ERR-BYOMEI     DELIMITED   BY  SPACE
                            "は接頭語で病名が終わっています"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0068"
                   MOVE    "既にこのコードは登録されています"
                                                       TO  SPA-ERRMSG
               WHEN    "0069"
                 MOVE
                 "自院病名以外の入力があるので自院病名検索はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0070"
                   MOVE    "病名入力コードの長さが違います"
                                                       TO  SPA-ERRMSG
               WHEN    "0071"
                   MOVE    "全角入力して下さい"        TO  SPA-ERRMSG
               WHEN    "0072"
                   MOVE    "全角または半角のみで入力して下さい" 
                                                       TO  SPA-ERRMSG
               WHEN    "0073"
                   MOVE
                   "病名の入力中です。クリアして下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0074"
                   MOVE
                   "病名を移行すると全ての病名コードを表示できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE    "入力された病名はマスター上で"
                                       TO  SPA-ERRMSG
                   MOVE    WRK-HAISIYMDH
                                       TO  SPA-ERRMSG (29:22)
                   MOVE    "で廃止の扱いとなっています。"
                                       TO  SPA-ERRMSG (51:)
               WHEN    "0076"
                   STRING   WRK-TANDOKUKBN-BYOMEI  DELIMITED  BY  SPACE
                           "は単独使用禁止の病名です"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG 
               WHEN    "0077"
                   MOVE
              "病名コードが２２個以上になりますので、編集病名とします。"
                                       TO  SPA-ERRMSG
               WHEN    "0078"
                 MOVE    "編集病名なので病名検索はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0079"
                 MOVE    "最後の部位が（）で始まらない病名です"
                                                       TO  SPA-ERRMSG
               WHEN    "0080"
                 MOVE    "修正項目が未入力です"       TO  SPA-ERRMSG
               WHEN    "0081"
                 MOVE    "病名番号の指定数が多すぎます"  TO  SPA-ERRMSG
               WHEN    "0082"
                 MOVE    "修正項目が多すぎます"        TO  SPA-ERRMSG
               WHEN    "0083"
                 MOVE    "全体表示のときは、全部指定はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0084"
                 MOVE
                     "入力した転帰日以降の診療開始日の病名が指定"
                                                   TO  SPA-ERRMSG
                 MOVE
                     "してあります"                TO  SPA-ERRMSG (43:)
               WHEN    "0085"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は開始日が"            DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "ですので該当の保険適用は範囲外です"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0090"
                   MOVE    "簡単修正処理中です"    TO  SPA-ERRMSG   
               WHEN    "0103"
                   MOVE    "病名コードでの組み立てができませんでした。"
                                       TO  SPA-ERRMSG
                   MOVE    "編集病名とします"
                                       TO  SPA-ERRMSG (43:)
               WHEN    "1001"
                   MOVE    "更新処理でエラーになりました"
                                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理でエラーになりました"
                                                       TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "患者病名マスタが存在しません"
                                                       TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "患者マスタの更新処理でエラーになりました"
                                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "自院病名の更新処理でエラーになりました"
                                                       TO  SPA-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE                TO  WRK-HAISIYMDH
      *
           MOVE    SPACE                TO  CERR
           MOVE    SPA-ERRCD            TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  CERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C02"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *****************************************************************
      *    確認画面表示処理
      *****************************************************************
       520-CIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-CIDCD
               WHEN    "0101"
                   MOVE    "内容が修正されています。"
                                       TO  WRK-CIDMSG
                   MOVE    "ＯＫで修正分は反映されずに終了します"
                                       TO  WRK-CIDMSG (25:)
               WHEN    "0102"
                   MOVE    "主病名がありません。更新しますか？"
                                       TO  WRK-CIDMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-CID1-FLG
      *
           MOVE    SPACE               TO  CID1
           MOVE    SPA-CIDCD           TO  CID1-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "CID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-CIDSET-EXT.
           EXIT.
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       530-CIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID2-FLG
      *
           EVALUATE    SPA-CIDCD2
               WHEN    "0101"
               WHEN    "0103"
                   MOVE    "あなたの入力された病名はマスター上で移行"
                                       TO  WRK-CIDMSG
                   MOVE    "病名の扱いとなっています。"
                                       TO  WRK-CIDMSG (41:)
               WHEN    "0104"
                   MOVE    "あなたの入力された自院病名の病名はマスタ"
                                       TO  WRK-CIDMSG
                   MOVE    "ー上で移行病名の扱いとなっています。"
                                       TO  WRK-CIDMSG (41:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  CID2
           MOVE    WRK-CIDMSG          TO  CID2-MSG
           MOVE    SPA-C02-IKOMOTO-BYOMEICD
                                       TO  CID2-BYOMEICD1
           MOVE    SPA-C02-IKOMOTO-BYOMEI  
                                       TO  CID2-BYOMEI1
           MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                       TO  CID2-BYOMEICD2
           MOVE    SPA-C02-IKOSAKI-BYOMEI
                                       TO  CID2-BYOMEI2
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "CID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID2"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-CIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       540-CIDSET4-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID4-FLG
      *
           EVALUATE    SPA-CIDCD4
               WHEN    "0101"
                   MOVE    "選択された病名を削除しますか？"
                                       TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  CID4
           MOVE    SPA-CIDCD4          TO  CID4-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID4-ID1MSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "CID4"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID4"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-CIDSET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込（主キー）
      *****************************************************************
       900-PTBYO-INV-SEC         SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE                  PTBYOMEI-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込
      *****************************************************************
       900-PTBYO-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-READ-SEC         SECTION.
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
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "BYOMEI-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "BYOMEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "BYOMEI-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-N-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-BYOMEI
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
              MOVE    1               TO  FLG-BYOMEI
              INITIALIZE                  BYOMEI-REC
           END-IF
           .
       900-BYOMEI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込
      *****************************************************************
       900-USERBYOMEI-INV-SEC         SECTION.
      *
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "USERBYOMEI-KEY6"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "USERBYOMEI-KEY6"   TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  USERBYOMEI-REC
                   MOVE    ZERO                TO  FLG-USERBYOMEI
               ELSE
                   MOVE    1                   TO  FLG-USERBYOMEI
                   INITIALIZE                  USERBYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "USERBYOMEI-KEY6"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-USERBYOMEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込
      *****************************************************************
       900-USBYO-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
               MOVE    ZERO            TO  FLG-USERBYOMEI
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
       900-USBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       900-JYURRK-INV-SEC         SECTION.
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY10"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "JYURRK-KEY10"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  JYURRK-REC
                   MOVE    ZERO                TO  FLG-JYURRK
               ELSE
                   MOVE    1                   TO  FLG-JYURRK
               END-IF
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "JYURRK-KEY10"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-JYURRK-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           MOVE    SPA-NAI-C02-SYSYMD
                                   TO  ORC-DBYMD
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               MOVE    "DBFETCH"       TO  MCP-FUNC
               CALL    "ORCMCPSUB"     USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *     
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
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
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-NAI-C02-PTID    TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
