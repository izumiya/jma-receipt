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
      *  01.08.05    NACL-藤原    04/03/25  レセ電エラーフラグの判定に
      *                                     廃止年月日を追加
      *  01.08.06    NACL-藤原    04/03/29  主病名の確認画面後の更新の修正
      *  01.08.07    NACL-藤原    04/03/30  開始日と保険組合せのチェックの修正
      *  01.08.08    NACL-藤原    04/04/05  ＣＤ検索からの病名コードの
      *                                     編集の修正
      *  01.08.09    NACL-藤原    04/04/13  修飾語コードのみ検索時の病名
      *                                     編集フラグのクリア
      *                                     種別が部位の自院コード入力のとき
      *                                     の病名編集フラグの設定を修正
      *  01.08.10    NACL-藤原    04/04/20  ２１の修飾語コード＋病名コード
      *                                     入力時の病名編集フラグの設定
      *  01.08.11    NACL-藤原    04/05/14  簡単修正時の開始日と
      *                                     保険組合せのチェックの修正
      *  01.08.12    NACL-藤原    04/06/21  アフターケア日の表示修正
      *  01.08.13    NACL-藤原    04/08/19  開始日の未来日を入力したとき
      *                                     警告を表示
      *                                     まとめ入力時の番号の整数以外
      *                                     のチェックを追加
      *  01.08.14    NACL-藤原    04/10/20  自院コード入力時の廃止年月日
      *                                     の警告を追加
      *  01.08.15    NACL-藤原    04/11/15  主病名の確認メッセージの表示
      *                                     をシス管の設定より行う
      *  00.08.16    NACL-藤原    04/11/01  病名入力時修飾語コードのみで
      *                                     構成されていても病名とする
      *  01.08.17    NACL-藤原    05/02/10  訂正日指定対応
      *  ver.2.6.0
      *  02.06.01    NACL-藤原    05/04/20  移行病名警告メッセージの表示、
      *                                     廃止年月日警告メッセージ表示、
      *                                     診療科、表示対象年月の初期表示
      *                                     をシス管の設定より行う
      *  02.06.02    NACL-藤原    05/05/14  同一病名のチェックを修正
      *                                     病名複写機能対応
      *  02.06.03    NACL-藤原    05/07/29  簡単修正の番号指定を「.」も
      *                                     可能とする
      *  02.06.04    NACL-藤原    05/08/25  労災・自賠責の保険限定の場合の
      *                                     同一診療科の同一病名を登録可能
      *                                     とする
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    06/01/16  年齢表示を追加
      *  02.07.03    NACL-藤原    06/01/25  疑いフラグからの病名の編集方
      *                                     法の変更
      *  02.07.04    NACL-藤原    06/01/26  保険組合せを増やす
      *
      *  02.08.01    NACL-藤原    06/03/02  「０入院・入院外」指定で診療
      *                                      科指定で削除処理した後の一覧
      *                                      表示が消えるのを修正
      *  02.08.02    NACL-藤原    06/02/01   自院病名コード検索追加
      *  02.08.03    NACL-藤原    06/03/22   改正対応
      *                                      疾患区分の表示を変更
      *  02.08.04    NACL-藤原    06/03/30   旧総合病院のときの診療科の表示
      *  02.08.05    NACL-藤原    06/04/01   自院病名検索時のチェックの修正
      *                                      登録時の変更時の複数病名のチ
      *                                      ェックの追加
      *  02.08.05    NACL-藤原    06/04/04   自院病名検索時の病名種別のセット
      *
      *  03.00.01    NACL-多々納  06/05/16   コメント入力画面変更
      *  03.00.02    NACL-藤原    06/06/19   C06,C07,C08 削除
      *
      *  03.01.01    NACL-藤原    06/06/01  病名入力コードのアルファベッ
      *                                     ト７文字対応
      *  03.01.02    NACL-藤原    06/07/06  自院コード入力時の移行病名の
      *                                     特定疾患の表示をする
      *                                     自院コード検索より戻ったときの
      *                                     移行病名の特定疾患の表示をする
      *  03.01.03    NACL-藤原    06/07/21  保険組合せの名称の編集方法の
      *                                     修正
      *
      *  03.02.01    NACL-藤原    06/07/25  自費による保険限定の病名の重複
      *                                     登録を可能とする
      *  03.02.02    NACL-藤原    06/08/04  自院検索で移行病名がないときの
      *                                     疾患区分の表示方法の修正
      *  03.02.03    NACL-藤原    06/08/16  自院コードのみのときの自院検索
      *                                     遷移がエラーにならないようにする
      *
      *  03.04.01    NACL-藤原    06/11/01  病名一括削除機能対応
      *  03.04.02    NACL-藤原    06/11/27  ＳＰＡデータの削除
      *                                     入外区分、処理区分変更時の入力
      *                                     項目のクリア
      *  03.04.03    NACL-藤原    06/12/01  月途中保険変更時の病名対応
      *  03.04.04    NACL-藤原    07/03/02  月途中保険変更時の病名対応中止
      *  03.04.05    NACL-藤原    07/04/03  カルテ病名の表示の修正
      *  03.04.06    NACL-藤原    07/05/14  カナチェック後の編集の修正
      *  03.04.07    NACL-藤原    07/06/21  処理区分、入外区分変更時の
      *                                     診療科の表示
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/25  複数病名登録時のレセプト表示期間対応
      *  03.05.03    NACL-藤原    07/10/12  ファンクションキー操作による
      *                                     処理区分、入外区分変更時の
      *                                     診療科の表示
      *
      *  04.01.01    NACL-藤原    07/10/11  診療科コピー（移動）機能対応
      *  04.01.02    NACL-藤原    07/10/26  公務災害対応
      *  04.01.03    NACL-藤原    08/02/06  登録年月日の更新時の編集追加
      *  04.01.04    NACL-藤原    08/02/13  診療科チェックの修正
      *
      *  04.02.01    NACL-藤原    08/03/03  特定疾患区分07,08の追加
      *  04.02.02    NACL-藤原    08/05/07  更新時の診療科未入力エラー
      *
      *  04.03.01    NACL-藤原    08/06/11  前回来院日の表示
      *                                     慢性と急性のチェック
      *
      *  04.04.01    NACL-藤原    09/02/16  データチェック後の病名修正対応
      *  04.04.02    NACL-藤原    09/02/16  未コード化傷病名コード入力で
      *                                     病名が変わらないときもコード
      *                                     変換処理を行なうようにする
      *                                     入力のコードクリアも行なう
      *  04.04.03    NACL-藤原    09/03/23  外来まとめからの遷移対応
      *  04.04.04    NACL-藤原    09/10/16  保険組合せの名称の編集方法の変更
      *
      *  04.05.01    NACL-藤原    09/05/27  複数病名登録時の保険組み合わせ対応
      *  04.05.02    NACL-藤原    09/05/28  照会画面からの遷移対応
      *  04.05.03    NACL-藤原    09/06/01  主病名の存在チェックの改善対応
      *                                     システム日付より前に転帰となった
      *                                     病名は主病名としないようにする
      *  04.05.04    NACL-藤原    09/06/01  排他制御時の表示の変更
      *  04.05.05    NACL-藤原    09/06/02  病名の入力順対応
      *  04.05.06    NACL-藤原    09/07/22  照会画面からの遷移対応
      *                                     渡された病名を表示
      *  04.05.07    NACL-藤原    09/11/11  病名の入力順対応を中止
      *  04.05.08    NACL-藤原    11/02/15  誕生日と診療開始日のチェック
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
           02  SPA-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //SPA-PTBYO-//.
           02  WRK-HAISIYMD-OCC.
               03  WRK-HAISIYMD-O  PIC 9(08)   OCCURS  6.
           02  WRK-PTBYOMEI-REC    OCCURS  6.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-//.
      *
           COPY    "ENUM-VALUE".
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
           03  FLG-ROW             PIC 9(01).
           03  FLG-MANSEI          PIC 9(01).
           03  FLG-KYUSEI          PIC 9(01).
           03  FLG-SYUBYO          PIC 9(01).
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
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
      *
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDZZ                PIC 9(04).
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
           03  WRK-BYOMEIHENFLG    PIC X(01).
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
                                   PIC X(10)   OCCURS  9.
      *
           03  WRK-STR             PIC 9(05).
      *
           03  WRK-TEKSTYMD        PIC 9(08).
           03  WRK-TEKEDYMD        PIC 9(08).
      *
           03  WRK-TENKIYMD        PIC X(08).
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
           03  WRK-ERR-SRYYMD      PIC X(22).
           03  WRK-ERR-SRYKA       PIC X(20).
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
           03  WRK-BYOMEICD        PIC X(07).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-BYOMEI-OLD      PIC X(480).
      * 
           03  WRK-SEIGYO          PIC X(01).
      *
           03  WRK-C02-BYO-ROW     PIC S9(09).
      *
           03  WRK-PTID            PIC X(10).
      *
      *    年齢表示
           03  WRK-NENREI-G.
               05  WRK-NENREI-Z    PIC ZZ9.
               05  WRK-NENREI-MEI  PIC X(04).
           03  WRK-NENREI-GR       REDEFINES   WRK-NENREI-G.
               05  WRK-NENREI-ZN   PIC Z9.
               05  WRK-NENREI-MEIN PIC X(04).
               05  FILLER          PIC X(01).
      *
      *    保険組合せ重複チェック用エリア
       01  WRK-CHK-HKNCOMBI-AREA.
           02  WRK-CHK-HKNCOMBI1   PIC 9(04).
           02  WRK-CHK-HKNCOMBI2   PIC 9(04).
      *
           02  FLG-HKNCOMBI-DABURI PIC 9(01).
           02  WRK-CHK-HKNCOMBI-REC    OCCURS  2.
               COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                           BY         //WRK-CHK-COMB-//.
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
      *    複写用患者病名マスタ−
       01  WRK-PTBYOMEI-COPY-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-COPY-//.
      *
      *    診療科移動用患者病名マスタ−
       01  WRK-PTBYOMEI-DELETE-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-DELETE-//.
      *
      *    患者病名マスタ−（レセ電エラーフラグチェック用）
       01  WRK-PTBYOMEI-CHK-AREA.
           02  WRK-HAISIYMD-CHK    PIC 9(08).
           02  WRK-PTBYOMEI-CHK-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-CHK-//.
      *
      *    患者病名マスタ−（行指定追加処理用）
       01  WRK-PTBYOMEI-INSERT-AREA.
           02  WRK-PTBYOMEI-INSERT-REC         OCCURS  99.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-INSERT-//.
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
      *    D04画面からの遷移情報
           COPY    "CPLNKD04.INC".
      *
      *    感染症ファイル名
       01  INFECTION-FILE          PIC X(17)   VALUE
                                   "infectiondata2.sh".
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
      *    病名機能情報
           COPY    "CPSK1042.INC".
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
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
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
      *    レセプト作成管理マスタ更新サブ
           COPY    "CPORCSRECEUPD.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    for Infection
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
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
           COPY    "C09.INC".
           COPY    "C10.INC".
           COPY    "C11.INC".
           COPY    "C12.INC".
           COPY    "C99.INC".
           COPY    "CERR.INC".
           COPY    "CID1.INC".
           COPY    "CID2.INC".
           COPY    "CID3.INC".
           COPY    "CID4.INC".
           COPY    "CID5.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-C02FREE
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
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
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "CERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-CIDCD       NOT =   SPACE
                   EVALUATE    SPA-CIDCD
                       WHEN    "0102"
                       WHEN    "0104"
                           PERFORM 520-CIDSET-SEC
                           GO  TO  100-INIT-EXT
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"  
                                           OR  "C50"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-C02
                   MOVE    SPA-WORK        TO  SPA-C01KYOUTU
               END-IF
      *        予約とエラー表示の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01" 
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        コメントから
               IF      WRK-MOTOPG(1:3)     =   "C50"
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
                                               "K02"  OR  "K02N" OR
                                               "D04"  OR  "KT01" OR
                                               "Q02"
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
               WHEN    "C09"
               WHEN    "C11"
               WHEN    "C12"
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
                   MOVE    SPACE           TO  SPA-CIDCD
                                               SPA-CID1-FLG
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0102"
                   IF      SPA-CID1-FLG    =   "OK"
                       PERFORM 4502-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD
                                               SPA-CID1-FLG
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-BYOMEI
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0103"
                   IF      SPA-CID1-FLG    =   "OK"
                       IF      SPA-C02-KEIKOKU-FLG3
                                               =   1
                           MOVE    SPACE               TO  SPA-CID1-FLG
                           MOVE    "0104"              TO  SPA-CIDCD
                           GO      TO      300-SCREEN-EXT
                       ELSE 
                           IF      SPA-C02-KEIKOKU-FLG2
                                                   =   1
                               MOVE    SPACE           TO  SPA-CID1-FLG
                               MOVE    "0102"          TO  SPA-CIDCD
                               GO      TO      300-SCREEN-EXT
                           ELSE 
                               PERFORM 4502-TOUROKU-SYORI-SEC
                           END-IF
                       END-IF
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD
                                               SPA-CID1-FLG
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-BYOMEI
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0104"
                   IF      SPA-CID1-FLG    =   "OK"
                       IF      SPA-C02-KEIKOKU-FLG2
                                               =   1
                           MOVE    SPACE               TO  SPA-CID1-FLG
                           MOVE    "0102"              TO  SPA-CIDCD
                           GO      TO      300-SCREEN-EXT
                       ELSE 
                           PERFORM 4502-TOUROKU-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD
                                               SPA-CID1-FLG
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-BYOMEI
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
                           MOVE    BYO-HAISIYMD    TO
                                       SPA-NAI-C02-HAISIYMD (IDZ)
                           MOVE    BYO-TOKSKNCD    TO
                                       SPA-NAI-C02-TOKSKNCD (IDZ)
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
                       MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                       MOVE    "key16"         TO  WRK-MCP-PATHNAME
                       INITIALIZE                  PTBYOMEI-REC
grpsys                 MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
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
           IF      WRK-MOTOPG(1:3)     =   "P97"
               MOVE    SPACE               TO  WRK-SPA-SRYKA
           END-IF
      *
      *    受付、氏名検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-C02-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
                   MOVE    WRK-SPA-PTNUM       TO  C02-PTNUM
                   PERFORM 41015-PTNUM-CHK-SEC
                   IF    ( WRK-SPA-SRYKA   NOT =   SPACE )
                   AND   ( SPA-C02-1042-SRYKA-DSPKBN
                                           NOT =   "1"   )
                       IF    ( WRK-SPA-SRYKA
                                           NOT =   SPA-GMN-C02-SRYKA )
      *****************AND   ( SPA-C02-1042-SRYKA-DSPKBN
      *****************                    NOT =   "1"               )
                           MOVE    WRK-SPA-SRYKA
                                               TO  SPA-GMN-C02-SRYKA
                           MOVE    SPA-GMN-C02-SRYKA
                                               TO  WRK-KBN
                           PERFORM 5001-SRYKA-HENSYU-SEC
                           MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-SRYKAMEI
                           MOVE    SPA-GMN-C02-SRYKA-G
                                               TO  SPA-GMN-C02-BSRYKA-G
      *                    病名一覧
                           MOVE    1           TO  SPA-GMN-C02-PAGE
      *
                           MOVE    "tbl_ptbyomei"
                                               TO  WRK-MCP-TABLE
                           MOVE    "key16"     TO  WRK-MCP-PATHNAME
                           INITIALIZE              PTBYOMEI-REC
grpsys                     MOVE    SPA-HOSPNUM TO  PTBYO-HOSPNUM
                           MOVE    SPA-NAI-C02-PTID
                                               TO  PTBYO-PTID
                           MOVE    SPA-GMN-C02-SRYKA
                                               TO  PTBYO-SRYKA
                           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
                           PERFORM 310-SPASET-HKNCOMBI-HENSYU-SEC
                       END-IF
                       MOVE    1           TO  SPA-GMN-C02-CUR
                   END-IF    
               END-IF
      *********MOVE    1               TO  SPA-GMN-C02-CUR
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
                                   WRK-PTBYOMEI-REC (1)
                                   WRK-PTBYOMEI-REC (2)
                                   WRK-PTBYOMEI-REC (3)
                                   WRK-PTBYOMEI-REC (4)
                                   WRK-PTBYOMEI-REC (5)
                                   WRK-PTBYOMEI-REC (6)
                                   WRK-HAISIYMD-OCC
      *      
      *    システム日付
           IF    ( SPA-C02-MOTOPG      =   "K02"   OR  "U02"
                                                   OR  "K02N"
                                                   OR  "D04"
                                                   OR  "KT01"
                                                   OR  "Q02" )
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
           MOVE    "特定疾患療養管理料"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (3)
           MOVE    "特定疾患"      TO  SPA-GMN-C02-MANSEIKBNTANL    (3)
           MOVE    "07"            TO  SPA-GMN-C02-MANSEIKBNL       (4)
           MOVE    "てんかん指導料"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (4)
           MOVE    "てんかん"      TO  SPA-GMN-C02-MANSEIKBNTANL    (4)
           MOVE    "08"            TO  SPA-GMN-C02-MANSEIKBNL       (5)
           MOVE    "特定疾患療養管理料又はてんかん指導料"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (5)
           MOVE    "特定・てん"    TO  SPA-GMN-C02-MANSEIKBNTANL    (5)
           MOVE    5               TO  SPA-GMN-C02-MANSEIKBN-MAX
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
      *    主病名編集フラグ情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
grpsys     MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    SPA-NAI-C02-SYSYMD
                                    TO  SYS-STYUKYMD
                                        SYS-EDYUKYMD
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
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-NAI-C02-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE    
               MOVE    SPACE           TO  SYS-1001-REC
           END-IF
           MOVE    SYS-1001-HOSPSBT1   TO  SPA-C02-1001-HOSPSBT1
      *         
      *    病名機能情報 
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
grpsys     MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    "1042"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    SPA-NAI-C02-SYSYMD
                                    TO  SYS-STYUKYMD
                                        SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-1042-REC
           ELSE    
               INITIALIZE                   SYS-1042-REC
           END-IF
           IF      SYS-1042-MSG1-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG1-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG1-DSPKBN
                                       TO  SPA-C02-1042-MSG1-DSPKBN
           IF      SYS-1042-MSG2-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG2-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG2-DSPKBN
                                       TO  SPA-C02-1042-MSG2-DSPKBN
           IF      SYS-1042-MSG3-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG3-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG3-DSPKBN
                                       TO  SPA-C02-1042-MSG3-DSPKBN
           IF      SYS-1042-MSG4-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG4-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG4-DSPKBN
                                       TO  SPA-C02-1042-MSG4-DSPKBN
      *
      *    旧総合病院の場合は全科表示はなしとする
           IF      SYS-1042-SRYKA-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-SRYKA-DSPKBN
           END-IF
           MOVE    SYS-1042-SRYKA-DSPKBN
                                       TO  SPA-C02-1042-SRYKA-DSPKBN
           IF    ( SPA-C02-1001-HOSPSBT1
                                       =   "1" )
           AND   ( SPA-C02-1042-SRYKA-DSPKBN
                                       =   "1" )
               MOVE    ZERO             TO  SPA-C02-1042-SRYKA-DSPKBN
           END-IF
      *
           IF      SYS-1042-SYOKBN-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-SYOKBN-DSPKBN
           END-IF
           MOVE    SYS-1042-SYOKBN-DSPKBN
                                       TO  SPA-C02-1042-SYOKBN-DSPKBN
           EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
               WHEN    ZERO
                   MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                           TO  SPA-GMN-C02-SYOKBN-G
               WHEN    "1"
                   MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                           TO  SPA-GMN-C02-SYOKBN-G
               WHEN    "2"
                   MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                           TO  SPA-GMN-C02-SYOKBN-G
           END-EVALUATE
      *
      *    診療科編集
           PERFORM 3104-SRYKA-SPA-SEC
      *
      *    患者情報（診療行為等で患者番号を入力したとき） 
           IF    ( SPA-C02-MOTOPG      =   "K02"   OR  "U02"
                                                   OR  "K02N"
                                                   OR  "D04"
                                                   OR  "KT01"
                                                   OR  "Q02"  )
           AND   ( SPA-PTNUM       NOT =   SPACE              )
               IF      SPA-C02-MOTOPG      =   "D04"
                   MOVE    SPA-WORK        TO  LNK-D04-BYOMEI
               END-IF
               MOVE    SPA-PTNUM       TO  C02-PTNUM
               PERFORM 41015-PTNUM-CHK-SEC
               MOVE    SPA-HKNCOMBIMEI TO  SPA-GMN-C02-HKNCOMBIMEIH
               MOVE    SPA-FTNRATEMEI  TO  SPA-GMN-C02-FTNRATE
               IF      SPA-ERRCD   NOT =   SPACE
                   MOVE    27              TO  SPA-GMN-C02-CUR
                   MOVE    SPACE           TO  SPA-ERRCD
               ELSE
                   IF    ( SPA-SRYKA   NOT =   SPACE )
                   AND   ( SPA-C02-1042-SRYKA-DSPKBN
                                       NOT =   "1"   )
                       IF    ( SPA-SRYKA   NOT =   SPA-GMN-C02-SRYKA )
      *****************AND   ( SPA-C02-1042-SRYKA-DSPKBN
      *****************                    NOT =   "1"               )
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
                           MOVE    "tbl_ptbyomei"
                                               TO  WRK-MCP-TABLE
                           MOVE    "key16"     TO  WRK-MCP-PATHNAME
                           INITIALIZE              PTBYOMEI-REC
grpsys                     MOVE    SPA-HOSPNUM TO  PTBYO-HOSPNUM
                           MOVE    SPA-NAI-C02-PTID
                                               TO  PTBYO-PTID
                           MOVE    SPA-GMN-C02-SRYKA
                                               TO  PTBYO-SRYKA
                           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
                           PERFORM 310-SPASET-HKNCOMBI-HENSYU-SEC
                       END-IF
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF    
      *************MOVE    1               TO  SPA-GMN-C02-CUR
               END-IF
               IF      SPA-C02-MOTOPG      =   "D04"
                   IF      LNK-D04-BYOMEI  NOT =   SPACE
                       MOVE    LNK-D04-BYOMEI      TO  C02-BYOMEI
                       PERFORM 41013-BYOMEI-CHK-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
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
           MOVE    SPA-NAI-C02-PTID    TO  SPA-PTID
           MOVE    SPA-NAI-C02-BIRTHDAY
                                       TO  SPA-BIRTHDAY
           MOVE    "2"                 TO  SPA-NYUGAIKBN
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "2"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN3
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *     
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      ORCSHKN-HKN-MAX
                   OR          IDX     >      100
               PERFORM 3102-HKNCOMBI-SPA-SEC
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
           END-IF
      *
      *    全科目指定・入外区分指定のときのキー設定
           IF      SPA-GMN-C02-HNYUGAIKBN  =   ZERO
               IF      SPA-GMN-C02-SRYKA       =   ZERO
                   MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                   MOVE    "key17"             TO  WRK-MCP-PATHNAME
               ELSE
                   MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                   MOVE    "key16"             TO  WRK-MCP-PATHNAME
               END-IF
           ELSE
               IF      SPA-GMN-C02-SRYKA       =   ZERO
                   MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                   MOVE    "key23"             TO  WRK-MCP-PATHNAME
               ELSE
                   MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                   MOVE    "key22"             TO  WRK-MCP-PATHNAME
               END-IF
           END-IF
      *
           MOVE    PTBYO-SRYYMD        TO  WRK-SYSYMD
      *
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
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
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
           MOVE    PTBYO-HKNCOMBI      TO  SPA-NAI-C02-THKNCOMBI  (IDX)
                                           WRK-KBN 
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
      *    自費の時
           IF      ORCSHKN-NAI-HKNNUM (IDX)(1:2)
                                       =   "98"
               MOVE    1                   TO
                                           SPA-NAI-C02-HKNCOMBI-JIHI
           END-IF
      *
      *    労災の時、コメントの追加
           IF      ORCSHKN-NAI-HKNNUM (IDX)
                                       =   "971"  OR  "973"
               MOVE    1                   TO
                                           SPA-NAI-C02-HKNCOMBI-ROUSAI
           END-IF
      *     
           MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-C02-HKNCOMBIL   (IDX)
                                           SPA-NAI-C02-THKNCOMBINUM(IDX)
           MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-NAI-C02-THKNCOMBIMEI(IDX)
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX) (1:34)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-C02-HKNCOMBIMEIL(IDX)
      *                                 
           MOVE    ORCSHKN-NAI-TEKSTYMD (IDX)
                                       TO  SPA-NAI-C02-TEKSTYMD(IDX)
           MOVE    ORCSHKN-NAI-TEKEDYMD (IDX)
                                       TO  SPA-NAI-C02-TEKEDYMD(IDX)
      *
           MOVE    IDX                 TO  SPA-NAI-C02-HKN-MAX
                                           SPA-GMN-C02-HKNCOMBI-MAX
      *
           ADD     1                   TO  IDY
           MOVE    SPA-NAI-C02-THKNCOMBINUM (IDX)
                                   TO  SPA-GMN-C02-THKNCOMBINUM (IDY)
           MOVE    SPA-NAI-C02-THKNCOMBIMEI (IDX)
                                   TO  SPA-GMN-C02-THKNCOMBIMEI (IDY)
           MOVE    SPA-NAI-C02-THKNCOMBINUM (IDX)
                                   TO  SPA-GMN-C02-TNUM (IDY)
      *
           ADD     1               TO  IDY 
           MOVE    ORCSHKN-GMN-TEKSTYMD (IDX)
                                   TO  SPA-GMN-C02-THKNCOMBIMEI (IDY)
           MOVE    ORCSHKN-GMN-TEKEDYMD (IDX)
                                   TO
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
           MOVE    SPA-NAI-C02-SYSYMD  TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
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
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    １診療科のとき全科の表示はなし
           IF      SPA-C02-1001-HOSPSBT1   =   "1"
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-SRYKA-MAX
                                           =   2
                   MOVE    SPA-GMN-C02-SRYKALST (2)
                                           TO  SPA-GMN-C02-SRYKALST (1)
                   MOVE    SPACE           TO  SPA-GMN-C02-SRYKALST (2)
                   MOVE    1               TO  SPA-GMN-C02-SRYKA-MAX
               END-IF
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
      *    レセ電エラーフラグ
           PERFORM 41041-RECEDENFLG-CHK-SEC
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
           IF      SPA-C01-INDEX   =   ZERO
      *      自院病名検索時
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
                   IF      SPA-C01-TBYOMEICD-INDEX (IDZ)
                                               =   IDX
                       PERFORM VARYING IDX3    FROM    1   BY  1
                               UNTIL   IDX3    >       21
                               OR      SPA-NAI-C04-BYOMEICD-O (IDX IDX3)
                                                       =   SPACE
                               OR      SPA-NAI-C04-BYOMEICD-O (IDX IDX3)
                                                       =   ALL "9"
                           MOVE    SPA-NAI-C04-BYOMEICD-O (IDX IDX3)
                                       TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-HAISIYMD    TO
                                           SPA-NAI-C02-HAISIYMD (IDZ)
                               IF      SPA-NAI-C04-BYOMEICD-O (IDX IDX3)
                                           NOT =
                                       SPA-NAI-C02-BYOMEICD-O (IDZ IDX3)
                                   MOVE    BYO-TOKSKNCD    TO
                                           SPA-NAI-C02-TOKSKNCD (IDZ)
                               END-IF 
                           END-IF
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
           ELSE
      *        自院病名コード検索時
               MOVE    SPA-C01-INDEX   TO  IDZ
               MOVE    SPA-C01-BYOMEIINPUTCD
                                       TO  SPA-NAI-C02-BYOMEICD (IDZ)
                                           SPA-GMN-C02-BYOMEICD (IDZ)
               MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                       TO  USBYO-BYOMEIINPUTCD
               PERFORM 410112-USERBYOMEI-HENSYU-SEC
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       21
                       OR      SPA-NAI-C04-BYOMEICD-O (IDZ IDX3)
                                               =   SPACE
                       OR      SPA-NAI-C04-BYOMEICD-O (IDZ IDX3)
                                               =   ALL "9"
                   MOVE    SPA-NAI-C04-BYOMEICD-O (IDZ IDX3)
                                           TO  BYO-BYOMEICD
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI      =   ZERO
                       MOVE    BYO-HAISIYMD    TO
                                           SPA-NAI-C02-HAISIYMD (IDZ)
                       IF      SPA-NAI-C04-BYOMEICD-O (IDZ IDX3)
                                       NOT =   SPA-NAI-C02-BYOMEICD-O
                                                          (IDZ IDX3)
                           MOVE    BYO-TOKSKNCD    TO
                                           SPA-NAI-C02-TOKSKNCD (IDZ)
                       END-IF
                   END-IF
                   MOVE    SPA-NAI-C04-BYOMEICD-O (IDZ IDX3)
                                       TO  SPA-NAI-C02-BYOMEICD-O
                                                          (IDZ IDX3)
                   MOVE    SPA-NAI-C04-BYOMEI-O   (IDZ IDX3)
                                       TO  SPA-NAI-C02-BYOMEI-O
                                                          (IDZ IDX3)
               END-PERFORM 
               MOVE    SPA-NAI-C04-BYOMEI  (IDZ)
                                   TO  SPA-NAI-C02-BYOMEI (IDZ)
           END-IF
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
      *    レセ電エラーフラグ（複数病名以外）
           IF      SPA-NAI-C02-BYOMEICD-MAX    =   1
               PERFORM 41041-RECEDENFLG-CHK-SEC
           ELSE
               MOVE    SPACE               TO  SPA-NAI-C02-RECEDENFLG
           END-IF
      *
           IF      SPA-C01-INDEX   =   ZERO
               MOVE    9                   TO  SPA-GMN-C02-CUR
           ELSE
               IF      SPA-C01-INDEX   =   6
                   MOVE    9                   TO  SPA-GMN-C02-CUR
               ELSE
                   COMPUTE SPA-GMN-C02-CUR =   SPA-C01-INDEX   +   2
               END-IF
           END-IF
           .
       320-C04SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正更新処理（簡単修正画面より戻り）
      *****************************************************************
       320-C10SPASET-SEC          SECTION.
      *
           IF      SPA-CID1-FLG        =   SPACE
               MOVE    1                   TO  SPA-GMN-C02-CUR
               GO  TO  320-C10SPASET-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-C02-BANGO-X
                                           SPA-CIDCD
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       9
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
                       IF      SPA-CID1-FLG        =   "DL"
                           PERFORM 3202-BYOMEI-DELETE-KOUSIN-SEC
                       ELSE
                           IF    ( WRK-HENSYU-DATA-O (8)   =   SPACE )
                           AND   ( WRK-HENSYU-DATA-O (9)   =   SPACE )
                               PERFORM 4101131-KANTAN-SYUSEI-KOUSIN-SEC
                           ELSE
                               IF      WRK-HENSYU-DATA-O (9)   =   SPACE
                                   PERFORM 3201-BYOMEI-COPY-KOUSIN-SEC
                               ELSE
                                   PERFORM
                                       3203-BYOMEI-SRYKA-COPY-KOUSIN-SEC
                               END-IF
                           END-IF
                       END-IF
                       IF      SPA-ERRCD   NOT =   SPACE
                           GO  TO  320-C10SPASET-EXT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
                                           SPA-CID1-FLG
                                           SPA-CID3-FLG
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           INITIALIZE                      SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
                                           SPA-NAI-C02-SELNO-INSERT
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-NAI-C02-SELNO-INSERT
                                       SPA-PTBYOMEI-REC
      *
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
           .
       320-C10SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名複写更新処理
      *****************************************************************
       3201-BYOMEI-COPY-KOUSIN-SEC         SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  3201-BYOMEI-COPY-KOUSIN-EXT
           ELSE
               MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-COPY-REC
           END-IF
      *
      *    転帰区分・転帰日
           INITIALIZE                  WRK-PTBYO-COPY-TENKIYMD
                                       WRK-PTBYO-COPY-TENKIKBN
      *
      *    開始日
           MOVE    WRK-HENSYU-DATA-O (8)
                                   TO  WRK-PTBYO-COPY-SRYYMD
      *
      *    疑いフラグ
           EVALUATE    WRK-HENSYU-DATA-O (3)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  WRK-PTBYO-COPY-UTAGAIFLG
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (3)
                                   TO  WRK-PTBYO-COPY-UTAGAIFLG
           END-EVALUATE      
      *
      *    主病名フラグ
           EVALUATE    WRK-HENSYU-DATA-O (4)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  WRK-PTBYO-COPY-SYUBYOFLG
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (4)
                                   TO  WRK-PTBYO-COPY-SYUBYOFLG
           END-EVALUATE      
      *
      *    疾患区分
           EVALUATE    WRK-HENSYU-DATA-O (5)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    ZERO    TO  WRK-PTBYO-COPY-MANSEIKBN
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (5)
                                   TO  WRK-PTBYO-COPY-MANSEIKBN
           END-EVALUATE      
      *
      *    入外区分
           EVALUATE    WRK-HENSYU-DATA-O (6)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  WRK-PTBYO-COPY-NYUGAIKBN
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (6)
                                   TO  WRK-PTBYO-COPY-NYUGAIKBN
           END-EVALUATE      
      *
      *    保険適用
           EVALUATE    WRK-HENSYU-DATA-O (7)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    ZERO    TO  WRK-PTBYO-COPY-HKNCOMBI
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (7)
                                   TO  WRK-PTBYO-COPY-HKNCOMBI
           END-EVALUATE      
      *
      *    レセ電エラーフラグ
           INITIALIZE                  WRK-PTBYOMEI-CHK-AREA
           IF      WRK-PTBYO-COPY-BYOMEIHENFLG
                                       =   SPACE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-BYOMEICD-MAX
                       OR      WRK-PTBYO-COPY-BYOMEICD (IDY)
                                           =   SPACE
                   MOVE    WRK-PTBYO-COPY-BYOMEICD (IDY)
                                               TO  BYO-BYOMEICD
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI          =   ZERO
                       IF    ( WRK-HAISIYMD-CHK
                                               =   ZERO                )
                       AND   ( BYO-HAISIYMD
                                         NOT   =   ZERO    AND ALL "9" )
                           MOVE    BYO-HAISIYMD    TO  WRK-HAISIYMD-CHK
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
           MOVE    WRK-PTBYOMEI-COPY-REC
                                       TO  WRK-PTBYOMEI-CHK-REC
           PERFORM 5001-RECEDENFLG-HENSYU-SEC
           MOVE    WRK-PTBYO-CHK-RECEDENFLG
                                       TO  WRK-PTBYO-COPY-RECEDENFLG
      *
      *    連番取得
           MOVE    ZERO                TO  WRK-RENNUM
      *
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    WRK-PTBYO-COPY-SRYKA
                                       TO  PTBYO-SRYKA
           MOVE    WRK-PTBYO-COPY-SRYYMD
                                       TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               IF      PTBYO-RENNUM        =   99
                   MOVE    "0089"              TO  SPA-ERRCD
                   GO  TO  3201-BYOMEI-COPY-KOUSIN-EXT
               END-IF
               MOVE    PTBYO-RENNUM        TO  WRK-RENNUM
           END-IF
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  WRK-PTBYO-COPY-RENNUM
      *
           MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-COPY-CREYMD
           MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-COPY-UPYMD
           MOVE    SMCNDATE-HMS        TO  WRK-PTBYO-COPY-UPHMS
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
      *
      *???
           DISPLAY "PTBYO=" WRK-PTBYO-COPY-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    WRK-PTBYOMEI-COPY-REC
                                       TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" WRK-PTBYO-COPY-KEY
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
           .
       3201-BYOMEI-COPY-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名削除処理
      *****************************************************************
       3202-BYOMEI-DELETE-KOUSIN-SEC   SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  3202-BYOMEI-DELETE-KOUSIN-EXT
           END-IF
      *    削除フラグ
           MOVE    "1"                 TO  PTBYO-DLTFLG
           MOVE    SPA-OPID            TO  PTBYO-DLT-OPID
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    "DBUPDATE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>"  PTBYO-KEY 
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
           .
       3202-BYOMEI-DELETE-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名診療科複写更新処理
      *****************************************************************
       3203-BYOMEI-SRYKA-COPY-KOUSIN-SEC         SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  3203-BYOMEI-SRYKA-COPY-KOUSIN-EXT
           ELSE
               MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-COPY-REC
                                               WRK-PTBYOMEI-DELETE-REC
           END-IF
      *
      *    診療科
           MOVE    WRK-HENSYU-DATA-O (9)
                                   TO  WRK-PTBYO-COPY-SRYKA
      *
      *    連番取得
           MOVE    ZERO                TO  WRK-RENNUM
      *
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    WRK-PTBYO-COPY-SRYKA
                                       TO  PTBYO-SRYKA
           MOVE    WRK-PTBYO-COPY-SRYYMD
                                       TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               IF      PTBYO-RENNUM        =   99
                   MOVE    "0089"              TO  SPA-ERRCD
                   GO  TO  3203-BYOMEI-SRYKA-COPY-KOUSIN-EXT
               END-IF
               MOVE    PTBYO-RENNUM        TO  WRK-RENNUM
           END-IF
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  WRK-PTBYO-COPY-RENNUM
      *
           MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-COPY-CREYMD
           MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-COPY-UPYMD
           MOVE    SMCNDATE-HMS        TO  WRK-PTBYO-COPY-UPHMS
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
      *
      *???
           DISPLAY "PTBYO=" WRK-PTBYO-COPY-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    WRK-PTBYOMEI-COPY-REC
                                       TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" WRK-PTBYO-COPY-KEY
               GO  TO  3203-BYOMEI-SRYKA-COPY-KOUSIN-EXT
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
      *
      *    診療科に移動のとき元の診療科分を削除する
           IF      SPA-CID1-FLG        =   "MV"
               MOVE    WRK-PTBYOMEI-DELETE-REC
                                           TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" WRK-PTBYO-COPY-KEY "#" MCP-FUNC "#"
      *???                
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" WRK-PTBYO-COPY-KEY
                   GO  TO  3203-BYOMEI-SRYKA-COPY-KOUSIN-EXT
               END-IF
           END-IF
           .
       3203-BYOMEI-SRYKA-COPY-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終受診診療科情報 編集処理
      *****************************************************************
       330-SPASET-JYURRK-HENSYU-SEC    SECTION.
      *     
grpsys     MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
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
      *    削除病名一覧表示
               WHEN    "CLICKED"       ALSO    "B20"
                   PERFORM 430-DELETE-LIST-SEC
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
                                       SPA-NAI-C02-SELNO-INSERT
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
           EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
               WHEN    ZERO
                   MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                           TO  SPA-GMN-C02-SYOKBN-G
               WHEN    "1"
                   MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                           TO  SPA-GMN-C02-SYOKBN-G
               WHEN    "2"
                   MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                           TO  SPA-GMN-C02-SYOKBN-G
           END-EVALUATE
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
grpsys         MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           NOT =   ZERO
                   MOVE    "0031"            TO  SPA-ERRCD
               ELSE
                   MOVE    PTINF-NAME        TO  SPA-GMN-C02-NAME
                   MOVE    PTINF-KANANAME    TO  SPA-GMN-C02-KANANAME
                   EVALUATE    PTINF-SEX
                       WHEN    "1" 
                           MOVE    "男"          TO  SPA-GMN-C02-SEX
                       WHEN    "2"
                           MOVE    "女"          TO  SPA-GMN-C02-SEX
                   END-EVALUATE
                   MOVE    PTINF-BIRTHDAY    TO  SPA-NAI-C02-BIRTHDAY
                                                 WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG       TO  SPA-GMN-C02-BIRTHDAY
                   MOVE    SPA-NAI-C02-PTID  TO  SPA-LAST-PTID
                   MOVE    SPA-GMN-C02-PTNUM TO  SPA-LAST-PTNUM 
      *            年齢計算
                   INITIALIZE                ORCSAGECHKAREA
grpsys             MOVE    SPA-HOSPNUM       TO  AGECHK-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID  TO  AGECHK-PTID
                   MOVE    SPA-NAI-C02-SYSYMD
                                             TO  AGECHK-SRYYMD
                   MOVE    SPA-NAI-C02-BIRTHDAY
                                             TO  AGECHK-BIRTHDAY
                   CALL    "ORCSAGECHK"        USING
                                               ORCSAGECHKAREA
grpsys                                         SPA-AREA
                   MOVE    SPACE             TO  WRK-NENREI-G
                   IF      AGECHK-NENREI2-YY   =   ZERO
                       MOVE    AGECHK-NENREI2-MM   TO  WRK-NENREI-ZN
                       MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
                   ELSE
                       MOVE    AGECHK-NENREI2-YY   TO  WRK-NENREI-Z
                       MOVE    "才"                TO  WRK-NENREI-MEI
                   END-IF
                   MOVE    WRK-NENREI-G        TO  SPA-GMN-C02-AGE
      *
      *            前回来院日
                   INITIALIZE                  ORCSLASTYMDAREA
                   MOVE    SPA-NAI-C02-PTID    TO  ORCSLASTYMD-PTID
                   CALL    "ORCSLASTYMD"       USING
                                               ORCSLASTYMDAREA
                                               SPA-AREA
      *            入院中は病棟を表示
                   IF      ORCSLASTYMD-BRMNUM  =   SPACE
                       IF      ORCSLASTYMD-LASTYMD NOT =   SPACE
                           STRING  "前回："    DELIMITED  BY  SIZE
                                   ORCSLASTYMD-LASTYMDW
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-C02-LASTYMD
                           END-STRING
                       END-IF
                   ELSE
                       STRING  "病棟："    DELIMITED  BY  SIZE
                               ORCSLASTYMD-BRMNUM
                                           DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-C02-LASTYMD
                       END-STRING
                   END-IF
      *
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    2           TO  SPA-NAI-C02-LOCK
                       GO  TO  41015-PTNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-HKNCOMBI-HENSYU-SEC
               IF      SPA-GMN-C02-SRYKA-MAX   =   1
                   MOVE    1                   TO  SPA-GMN-C02-CUR
               ELSE      
                   IF      SPA-C02-1042-SRYKA-DSPKBN
                                               =   ZERO
                       PERFORM 330-SPASET-JYURRK-HENSYU-SEC
                       MOVE    27              TO  SPA-GMN-C02-CUR
                   ELSE
                       MOVE    SPA-GMN-C02-SRYKALST (1)
                                               TO  SPA-GMN-C02-SRYKA-G
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
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
                   MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                   MOVE    "key16"             TO  WRK-MCP-PATHNAME
                   INITIALIZE                      PTBYOMEI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
                                       SPA-NAI-C02-SELNO-INSERT
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
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key16"             TO  WRK-MCP-PATHNAME
               INITIALIZE                  PTBYOMEI-REC
grpsys         MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
                                       SPA-NAI-C02-SELNO-INSERT
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
                                               SPA-NAI-C02-SELNO-INSERT
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
      *************病名指定行追加処理
      *****        IF      C02-BANGO (1:1) =   "+"
      *****          INITIALIZE                  ORCSNUMAREA
      *****          MOVE    C02-BANGO (2:)      TO  SNUM-INX
      *****          CALL    "ORCSNUM"           USING
      *****                                      ORCSNUMAREA
      *****          IF      SNUM-RC             =   ZERO
      *****            MOVE    C02-BANGO         TO  SPA-GMN-C02-BANGO-X
      *
      *****            COMPUTE WRK-BANGO
      *****                        =   SNUM-OUTNUM
      *****                        - ( SPA-GMN-C02-PAGE    -   1 )
      *****                        *   WRK-CONS-C02-MAX
      *
      *****            IF    ( WRK-BANGO       >   SPA-GMN-C02-MAX )
      *****            OR    ( WRK-BANGO       <   1               )
      *****                MOVE    "0012"          TO  SPA-ERRCD
      *****            ELSE
      *
      *                    病名数のチェック
      *****                INITIALIZE                  PTBYOMEI-REC
grpsys*****                MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
      *****                MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
      *****                MOVE    SPA-NAI-C02-TSRYKA (WRK-BANGO)
      *****                                            TO  PTBYO-SRYKA
      *****                MOVE    SPA-NAI-C02-TSRYYMD (WRK-BANGO)
      *****                                            TO  PTBYO-SRYYMD
      *****                MOVE    ZERO                TO  PTBYO-RENNUM
      *****                MOVE    "%"                 TO  PTBYO-DLTFLG
      *****                MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
      *****                MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
      *****                MOVE    "key34"             TO
      *****                                            WRK-MCP-PATHNAME
      *****                PERFORM 900-PTBYO-INV-SEC
      *****                同一診療科、開始日で９９件のとき（削除分含む）
      *****                IF      PTBYO-RENNUM        =   99
      *****                    MOVE    "0034"          TO  SPA-ERRCD
      *****                    INITIALIZE                  PTBYOMEI-REC
grpsys*****                    MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
      *****                    MOVE    SPA-NAI-C02-PTID
      *****                                            TO  PTBYO-PTID
      *****                    MOVE    SPA-NAI-C02-TSRYKA (WRK-BANGO)
      *****                                            TO  PTBYO-SRYKA
      *****                    MOVE    SPA-NAI-C02-TSRYYMD (WRK-BANGO)
      *****                                            TO  PTBYO-SRYYMD
      *****                    MOVE    ZERO            TO  PTBYO-RENNUM
      *****                    MOVE    "1"             TO  PTBYO-DLTFLG
      *****                    MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
      *****                    MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
      *****                    MOVE    "key34"         TO
      *****                                            WRK-MCP-PATHNAME
      *****                    PERFORM 900-PTBYO-INV-SEC
      *                        同一診療科、開始日で削除分がないとき
      *****                    IF      PTBYO-RENNUM    =   ZERO
      *****                       MOVE    "0088"           TO  SPA-ERRCD
      *****                    END-IF
      *****                    GO  TO  41011-BANGO-HEN-EXT
      *****                END-IF
      * 
      *****                MOVE    WRK-BANGO       TO
      *****                                    SPA-NAI-C02-SELNO-INSERT     
      *
      *****                MOVE    SPA-GMN-C02-TSRYYMD (WRK-BANGO)
      *****                                    TO  SPA-GMN-C02-SRYYMD
      *****                MOVE    SPA-NAI-C02-TSRYYMD (WRK-BANGO)
      *****                                    TO  SPA-NAI-C02-SRYYMD
      *****                MOVE    SPA-NAI-C02-TSRYKA (WRK-BANGO)
      *****                                    TO  SPA-GMN-C02-BSRYKA
      *****                IF      SPA-NAI-C02-SELNO-INSERT
      *****                                    <   10
      *****                    MOVE    SPA-NAI-C02-SELNO-INSERT
      *****                                            TO  C02-BYO-ROW
      *****                ELSE
      *****                    COMPUTE C02-BYO-ROW     =   
      *****                            SPA-NAI-C02-SELNO-INSERT    -   5
      *****                END-IF
      *****                MOVE    2               TO  SPA-GMN-C02-CUR 
      *****            END-IF
      *****          ELSE
      *****            MOVE    "0012"          TO  SPA-ERRCD
      *****          END-IF
      *****        ELSE
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
      *****        END-IF
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
           MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                       WRK-ERR-SRYYMD
                                       WRK-ERR-SRYKA
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
           UNSTRING    WRK-HENSYU-NUM  DELIMITED   BY  "," OR  "."
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
                       IF    ( SNUM-RC         =   ZERO  )
                       AND   ( SNUM-MINKBN     =   SPACE )
                       AND   ( SNUM-SYOKBN     =   SPACE ) 
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
                       IF    ( SNUM-RC         =   ZERO  )
                       AND   ( SNUM-MINKBN     =   SPACE )
                       AND   ( SNUM-SYOKBN     =   SPACE ) 
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
                               IF      WRK-STNUM       =   SPACE
                                   MOVE    WRK-EDNUM-O(IDXX)
                                                   TO  WRK-STNUM-O(IDXX)
                               END-IF
                           END-IF
                       ELSE
                           MOVE    "0012"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
             END-PERFORM
             IF      SPA-ERRCD     NOT =   SPACE    
                 GO  TO  410113-KANTAN-SYUSEI-EXT
             END-IF
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
           IF    ( WRK-HENSYU-DATA-O (1)   NOT =   SPACE )
           OR    ( WRK-HENSYU-DATA-O (6)   NOT =   SPACE )
           OR    ( WRK-HENSYU-DATA-O (7)   NOT =   SPACE )
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
                                           SPA-NAI-C02-SELNO-INSERT
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-NAI-C02-SELNO-INSERT
                                       SPA-PTBYOMEI-REC
      *
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
      *    転記日のセット
           MOVE    SPA-NAI-C02-TTENKIYMD (IDX)
                                   TO  WRK-TENKIYMD
           IF      WRK-HENSYU-DATA-O (1)
                                   =   SPACE
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (1)
                                       =   "-"
                   MOVE    SPACE                   TO  WRK-TENKIYMD
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (2)   TO  WRK-TENKIYMD
               END-IF
           END-IF
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
      *    保険適用指定時の指定病名の重複チェック
           IF     WRK-HENSYU-DATA-O (7)  
                               NOT =   SPACE
               PERFORM VARYING IDXY    FROM    IDXX    BY  1
                       UNTIL   IDXY    >       5
                   IF      WRK-STNUM-O (IDXY)  >   ZERO
                       PERFORM VARYING IDY FROM    WRK-STNUM-O (IDXY)
                                           BY 1
                               UNTIL   IDY >       WRK-EDNUM-O (IDXY)
                           PERFORM
                               41011321-KANTAN-SYUSEI-HKNCOMBI-CHK-SEC
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  4101132-KANTAN-SYUSEI-DABURI-CHK-EXT
           END-IF
      *
      *    マスタとのチェック
           INITIALIZE                      PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                       TO  PTBYO-BYOMEI
           MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                       TO  PTBYO-NYUGAIKBN
           IF      WRK-HENSYU-DATA-O (6)   =   SPACE
               CONTINUE
           ELSE 
               IF      WRK-HENSYU-DATA-O (6)   =   "-"
                   MOVE    SPACE               TO  PTBYO-NYUGAIKBN
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (6)
                                               TO  PTBYO-NYUGAIKBN
               END-IF
           END-IF     
           IF      PTBYO-NYUGAIKBN     =   SPACE 
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key13"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key21"             TO  WRK-MCP-PATHNAME
           END-IF    
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
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
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           =   PTBYO-SRYYMD
                           MOVE    "0002"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           <   PTBYO-SRYYMD
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     WRK-TENKIYMD >   PTBYO-SRYYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-TSRYYMD (IDX)
                                               <   PTBYO-TENKIYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD           =   "0038"  OR
                                                   "0002"
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                                   TO  WRK-CHK-HKNCOMBI1
                       ELSE 
                           IF      WRK-HENSYU-DATA-O (7)   =   "-"
                               MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                           ELSE
                               MOVE    WRK-HENSYU-DATA-O (7)
                                                   TO  WRK-CHK-HKNCOMBI1
                           END-IF
                       END-IF
                       MOVE    PTBYO-HKNCOMBI      TO  WRK-CHK-HKNCOMBI2
                       PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE   SPACE            TO  SPA-ERRCD
                       ELSE 
                           MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       4101132-KANTAN-SYUSEI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ変更時同一病名重複チェック処理
      *****************************************************************
       41011321-KANTAN-SYUSEI-HKNCOMBI-CHK-SEC SECTION.
      *
      *    同一診療科・同一病名の場合だけチェック
           IF  ( IDXX              =    IDXY )
           AND ( IDX               =    IDY  )
                CONTINUE
           ELSE
               IF    ( SPA-GMN-C02-TBYOMEI (IDX)   =
                                   SPA-GMN-C02-TBYOMEI (IDY) )
               AND   ( SPA-NAI-C02-TSRYKA  (IDX)   =
                                   SPA-NAI-C02-TSRYKA  (IDY) )
      *
      *            入外区分が違う場合はチェックしない
                   IF      WRK-HENSYU-DATA-O (6)   =   SPACE
                       IF (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "1" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDY)
                                                       =   "2" ) )
                       OR (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "2" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDY)
                                                       =   "1" ) )
                           GO  TO
                               41011321-KANTAN-SYUSEI-HKNCOMBI-CHK-EXT
                       END-IF
                   END-IF     
      *
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           =   SPA-NAI-C02-TSRYYMD (IDY)
                           MOVE    "0002"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           <   SPA-NAI-C02-TSRYYMD (IDY)
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     WRK-TENKIYMD >
                                               SPA-NAI-C02-TSRYYMD (IDY)
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           >   SPA-NAI-C02-TSRYYMD (IDY)
                           IF      SPA-NAI-C02-TTENKIYMD (IDY)
                                               =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-TSRYYMD (IDX)   <
                                               SPA-NAI-C02-TSRYYMD (IDY)
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD           =   "0038"  OR
                                                   "0002"
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                                   TO  WRK-CHK-HKNCOMBI1
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDY)
                                                   TO  WRK-CHK-HKNCOMBI2
                       ELSE 
                           IF      WRK-HENSYU-DATA-O (7)   =   "-"
                               MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                                                       WRK-CHK-HKNCOMBI2
                           ELSE
                               MOVE    WRK-HENSYU-DATA-O (7)
                                                   TO  WRK-CHK-HKNCOMBI1
                                                       WRK-CHK-HKNCOMBI2
                           END-IF
                       END-IF
                       PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE   SPACE            TO  SPA-ERRCD
                       ELSE 
                           MOVE    SPA-NAI-C02-TSRYYMD (IDY)
                                                   TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       41011321-KANTAN-SYUSEI-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正更新処理
      *****************************************************************
       4101131-KANTAN-SYUSEI-KOUSIN-SEC         SECTION.
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  4101131-KANTAN-SYUSEI-KOUSIN-EXT
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
                   MOVE    ZERO    TO  PTBYO-HKNCOMBI
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (7)
                                   TO  PTBYO-HKNCOMBI
           END-EVALUATE      
      *
      *    レセ電エラーフラグ
           INITIALIZE                      WRK-PTBYOMEI-CHK-AREA
           IF      PTBYO-BYOMEIHENFLG  =   SPACE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-BYOMEICD-MAX
                       OR      PTBYO-BYOMEICD (IDY)    =   SPACE
                   MOVE    PTBYO-BYOMEICD (IDY)    TO   BYO-BYOMEICD
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI          =   ZERO
                       IF    ( WRK-HAISIYMD-CHK
                                               =   ZERO                )
                       AND   ( BYO-HAISIYMD
                                         NOT   =   ZERO    AND ALL "9" )
                           MOVE    BYO-HAISIYMD    TO  WRK-HAISIYMD-CHK
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
           MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-CHK-REC
           PERFORM 5001-RECEDENFLG-HENSYU-SEC
           MOVE    WRK-PTBYO-CHK-RECEDENFLG
                                       TO  PTBYO-RECEDENFLG
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    "DBUPDATE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>"  PTBYO-KEY 
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
           .
       4101131-KANTAN-SYUSEI-KOUSIN-EXT.
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
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-RENNUM  TO  PTBYO-RENNUM
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
      *
           MOVE    PTBYOMEI-REC        TO  SPA-PTBYOMEI-REC
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
      *
      *    入力コード
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       6
               MOVE    PTBYO-BYOMEIINPUTCD (IDZ)
                                   TO  SPA-GMN-C02-BYOMEICD (IDZ)
      *        
               IF      SPA-GMN-C02-BYOMEICD (IDZ)
                                   NOT =   SPACE
                   MOVE    ZERO                TO  WRK-LENGTH                 
                   PERFORM VARYING IDX FROM    10  BY  -1
                           UNTIL   IDX <       1
                       IF      SPA-GMN-C02-BYOMEICD (IDZ)(IDX:1)
                                               NOT =   SPACE
                           MOVE    IDX             TO  WRK-LENGTH
                           MOVE    1               TO  IDX
                       END-IF
                   END-PERFORM
      *
      *            ７桁のコードのとき病名コードか自院コードか確認
                   MOVE    ZERO                TO  FLG-BYOMEI
                   IF      WRK-LENGTH          =   7
                       MOVE    PTBYO-BYOMEIINPUTCD (IDZ)
                                               TO  BYO-BYOMEICD
                       IF  (   BYO-BYOMEICD (1:3)    =   "ZZZ"    )
                       AND ( ( BYO-BYOMEICD (4:1)    <   "1" ) OR 
                             ( BYO-BYOMEICD (4:1)    =   "C" )    )
      *                    予約病名コード検索
                           SET     TBL-BYOMEICD-IDX  TO  1
                           SEARCH  TBL-BYOMEICD-OCC
                                               VARYING TBL-BYOMEICD-IDX
                               AT  END
                                   MOVE    1           TO  FLG-BYOMEI
                               WHEN    BYO-BYOMEICD    =
                                                   TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                                   MOVE    ZERO        TO  FLG-BYOMEI
                           END-SEARCH
                       ELSE
      *                    病名マスタ検索
                           PERFORM 900-BYOMEI-READ-SEC
                       END-IF
                   END-IF
      *
                   IF      WRK-LENGTH          =   7
                       IF      FLG-BYOMEI          =   ZERO
                           MOVE    SPACE           TO
                                       SPA-NAI-C02-USERBYOMEIFLG (IDZ)
                       ELSE
                           MOVE    "1"             TO
                                       SPA-NAI-C02-USERBYOMEIFLG (IDZ)
                       END-IF    
                   ELSE
                       MOVE    "1"             TO
                                       SPA-NAI-C02-USERBYOMEIFLG (IDZ)
                   END-IF
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
                                         SPA-NAI-C02-USERBYOMEIFLG(IDZ)
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
                       MOVE    BYO-HAISIYMD
                                         TO  SPA-NAI-C02-HAISIYMD  (IDZ)
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
      *************MOVE    SPACE         TO
      *************                      SPA-NAI-C02-USERBYOMEIFLG(IDZ)
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
                                         SPA-NAI-C02-USERBYOMEIFLG(IDZ)
      *
           MOVE    ZERO                  TO  FLG-USERBYOMEI
           IF      USBYO-BYOMEIINPUTCD   =   SPACE
               CONTINUE
           ELSE
grpsys         MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
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
                               IF    ( SPA-NAI-C02-HAISIYMD (IDZ)
                                               =   ZERO                )
                               AND   ( BYO-HAISIYMD
                                           NOT =   ZERO    AND ALL "9" )
                                   MOVE    BYO-HAISIYMD    TO
                                           SPA-NAI-C02-HAISIYMD (IDZ)
                               END-IF
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
      *
           IF      SPA-NAI-C02-SELNO-INSERT
                                       >   ZERO
               IF      C02-SRYYMD      NOT =   SPA-GMN-C02-SRYYMD
                   MOVE    "0087"              TO  SPA-ERRCD
               END-IF
               MOVE    17                  TO  SPA-GMN-C02-CUR
               GO  TO  41012-SRYYMD-CHK-EXT
           END-IF 
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
               IF      SPA-NAI-C02-SRYYMD  <   SPA-NAI-C02-BIRTHDAY
                   MOVE    "0096"      TO  SPA-ERRCD
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
                       IF      SPA-NAI-C02-SRYYMD
                                           <=   WRK-TEKEDYMD
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
           IF      SPA-ERRCD           =   SPACE
      *
      *        未来日入力チェック（警告）         
               IF      SPA-NAI-C02-SRYYMD  >   SPA-NAI-C02-SYSYMD
                   MOVE    "0039"          TO  SPA-ERRCD
               END-IF
      *
      *        レセ電エラーフラグ
               PERFORM 41041-RECEDENFLG-CHK-SEC
               MOVE    17                  TO  SPA-GMN-C02-CUR
           END-IF
      *
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
                   MOVE    SPA-GMN-C02-BYOMEICD (IDZ)
                                       TO  SPA-NAI-C02-BYOMEICD (IDZ)
                   IF      SPA-NAI-C02-BYOMEICD(IDZ)   =   SPACE
                       CONTINUE
                   ELSE 
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
                               IF      FLG-BYOMEI  =   ZERO
                                   CONTINUE
                               ELSE
                                   INITIALIZE          USERBYOMEI-REC
grpsys                             MOVE    SPA-HOSPNUM
                                               TO  USBYO-HOSPNUM
                                   MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                                   PERFORM 410112-USERBYOMEI-HENSYU-SEC
                               END-IF 
                           WHEN    OTHER
                               INITIALIZE          USERBYOMEI-REC
grpsys                         MOVE    SPA-HOSPNUM
                                               TO  USBYO-HOSPNUM
                               MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                               PERFORM 410112-USERBYOMEI-HENSYU-SEC
                       END-EVALUATE
                   END-IF 
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
                           INITIALIZE  SPA-NAI-C02-BYOMEI-OCC    (IDX)
                                       SPA-GMN-C02-BYOMEICD      (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     +   1
                           MOVE    SPA-NAI-C02-BYOMEI-OCC    (IDW)
                                   TO  SPA-NAI-C02-BYOMEI-OCC (IDX)
                           MOVE    SPA-GMN-C02-BYOMEICD      (IDW)
                                   TO  SPA-GMN-C02-BYOMEICD   (IDX)
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
                                       SPA-NAI-C02-USERBYOMEIFLG (IDZ)
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
      *    ７桁入力のときは病名マスタ、以外は自院病名マスタを先に読み込む
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
                                       TO  SPA-GMN-C02-BYOMEICD   (IDX)
                   END-IF                     
               END-PERFORM                           
             WHEN    7   
      *        病名マスタ読み込み
               MOVE    SPA-NAI-C02-BYOMEICD (IDZ) TO  BYO-BYOMEICD
               PERFORM 410111-BYOMEI-HENSYU-SEC
               IF      FLG-BYOMEI      NOT =   ZERO
      *            自院病名マスタ読み込み
                   INITIALIZE                  USERBYOMEI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 410112-USERBYOMEI-HENSYU-SEC
                   IF      FLG-USERBYOMEI   NOT =   ZERO
      *                自院病名検索
                       PERFORM 41029-USERBYOMEICD-KENSAKU-SEC
                       IF      SPA-ERRCD       =   "0053"
                           MOVE    "0054"              TO  SPA-ERRCD
                       ELSE
                           IF      SPA-ERRCD       =   SPACE
                               PERFORM 41021-BYOMEICD-HENSYU-SEC
                               MOVE    SPACE           TO  SPA-ERRCD
                               GO  TO  4102-BYOMEICD-CHK-EXT
                           END-IF
                       END-IF
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
                               MOVE    ZERO            TO  FLG-BYOMEI
                               PERFORM 900-BYOMEI-READ-SEC
                               IF    ( FLG-BYOMEI          =   ZERO  )
                               AND   ( BYO-IKOSAKICD   NOT =   SPACE )
                               AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                           =   ZERO  )
                                   MOVE    USBYO-BYOMEICD (IDX3) 
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                                   MOVE    BYO-BYOMEI 
                                           TO  SPA-C02-IKOMOTO-BYOMEI
                                   MOVE    BYO-IKOSAKICD
                                           TO  BYO-BYOMEICD
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
                                       MOVE    "0104"  TO  SPA-CIDCD2
                                       GO  TO  4102-BYOMEICD-CHK-EXT
                                   END-IF
                               END-IF
                               MOVE    21                  TO  IDX3
                           END-IF    
                       END-PERFORM
                       IF      SPA-NAI-C02-HAISIYMD (IDZ)  NOT =   ZERO
                           MOVE    SPA-NAI-C02-HAISIYMD (IDZ)
                                                   TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-HAISIYMDH
                       END-IF
                     END-IF
                   END-IF                        
               ELSE
                   IF    ( BYO-IKOSAKICD   NOT =   SPACE )
                   AND   ( SPA-C02-1042-MSG2-DSPKBN
                                               =   ZERO  )
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
                   ELSE    
                       IF      BYO-HAISIYMD        =   ALL "9" OR  SPACE
                           CONTINUE
                       ELSE
                           MOVE    BYO-HAISIYMD    TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-HAISIYMDH
                       END-IF        
                   END-IF        
               END-IF
             WHEN    OTHER       
      *        自院病名マスタ読み込み
               INITIALIZE                  USERBYOMEI-REC
grpsys         MOVE    SPA-HOSPNUM          TO  USBYO-HOSPNUM
               MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
               PERFORM 410112-USERBYOMEI-HENSYU-SEC
               IF      FLG-USERBYOMEI   NOT =   ZERO
                   PERFORM 41029-USERBYOMEICD-KENSAKU-SEC
                   IF      SPA-ERRCD       =   SPACE
                       PERFORM 41021-BYOMEICD-HENSYU-SEC
                       MOVE    SPACE           TO  SPA-ERRCD
                       GO  TO  4102-BYOMEICD-CHK-EXT
                   END-IF
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
                           IF    ( FLG-BYOMEI          =   ZERO  )
                           AND   ( BYO-IKOSAKICD   NOT =   SPACE )
                           AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                       =   ZERO  )
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
                   IF      SPA-NAI-C02-HAISIYMD (IDZ)  NOT =   ZERO
                       MOVE    SPA-NAI-C02-HAISIYMD (IDZ)
                                               TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3
                                               TO  WRK-HAISIYMDH
                   END-IF
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
      *    新規（行指定がない場合）以外のときは複数病名の入力不可
           IF      SPA-ERRCD               =   SPACE  
               IF    ( SPA-NAI-C02-BYOMEICD-MAX    >   1    )
                   IF    ( SPA-GMN-C02-BANGO           >   ZERO )
                   OR    ( SPA-NAI-C02-SELNO-INSERT    >   ZERO )
                       MOVE    "0056"             TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF        
      *
      *    括弧で始まる部位が末尾の警告以外のとき
           IF    ( SPA-ERRCD            NOT =   SPACE  )
           AND   ( SPA-ERRCD            NOT =   "0079" )     
               GO  TO  41021-BYOMEICD-HENSYU-EXT
           END-IF
      *
      *    レセ電エラーフラグ（複数病名以外）
           IF      SPA-NAI-C02-BYOMEICD-MAX    =   1
               PERFORM 41041-RECEDENFLG-CHK-SEC
           ELSE
               MOVE    SPACE               TO  SPA-NAI-C02-RECEDENFLG
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
           IF    ( WRK-HAISIYMDH       =   SPACE )
           OR    ( SPA-C02-1042-MSG3-DSPKBN
                                       =   "1"   )
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
               IF      SPA-NAI-C02-BYOMEICD(IDX)     NOT =   SPACE
                   IF      SPA-ERRCD             =   "0079"
                       MOVE    SPACE                 TO  SPA-ERRCD
                   END-IF
                   IF    ( SPA-ERRCD             =   SPACE   OR  "0079")
                   AND   ( SPA-NAI-C02-BYOMEIHENFLG
                                             =   SPACE                 )
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
                                   MOVE    "0006"     TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"        TO  WRK-RECECD (3)
                               END-IF
                           WHEN    OTHER                 
                               IF      WRK-RECECD (3)  NOT =   SPACE 
                                   MOVE    "0006"      TO  SPA-ERRCD
                                   MOVE    21          TO  IDX
                               ELSE
                                   IF      WRK-RECECD (2)
                                                   NOT =   SPACE 
                                       MOVE    "1"       TO
                                               SPA-NAI-C02-BYOMEIHENFLG
                                   ELSE
                                       MOVE    "1"   TO  WRK-RECECD (2)
                                   END-IF
                               END-IF    
                         END-EVALUATE
                       ELSE
                         EVALUATE    SPA-NAI-C02-BYOMEISBT (IDX)
                           WHEN    "1"
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    "1"       TO
                                               SPA-NAI-C02-BYOMEIHENFLG
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
                                   IF      WRK-RECECD (2)
                                                   NOT =   SPACE 
                                       MOVE    "1"       TO
                                               SPA-NAI-C02-BYOMEIHENFLG
                                   ELSE
                                       MOVE    "1"   TO  WRK-RECECD (2)
                                   END-IF
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
      *    自院病名検索画面編集処理（コードより検索）
      *****************************************************************
       41029-USERBYOMEICD-KENSAKU-SEC      SECTION.
      *
      *    検索対象の病名があるか
           INITIALIZE                  USERBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           STRING  SPA-NAI-C02-BYOMEICD(IDZ)
                                       DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
                                       INTO    USBYO-BYOMEIINPUTCD
           END-STRING
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "key11"             TO  MCP-PATHNAME
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key11"             TO  MCP-PATHNAME
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               PERFORM 900-USBYO-READ-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           MOVE    "key11"             TO  MCP-PATHNAME
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-USERBYOMEI      =   1
               MOVE    "0053"              TO  SPA-ERRCD
               GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
           END-IF
      *
      *    簡単修正処理中のとき
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
           AND   ( SPA-NAI-C02-SELNO-INSERT
                                       =   ZERO  )
               MOVE    "0090"              TO  SPA-ERRCD
               GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
           END-IF
      *
      *****IF    ( SPA-GMN-C02-BANGO           >   ZERO )
      *    AND   ( SPA-NAI-C02-BYOMEICD-MAX    >   1    )
      *        MOVE    "0056"             TO  SPA-ERRCD
      *        GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
      *****END-IF
      *
      *    変更処理のとき自院コードの入力がない場合
           IF    ( SPA-GMN-C02-BANGO   
                                   NOT =   SPACE ) 
           AND   ( SPA-GMN-C02-BYOMEICD-G
                                       =   SPACE )
           AND   (SPA-NAI-C02-BYOMEICD (1)
                                   NOT =   SPACE )       
               MOVE    "0069"              TO  SPA-ERRCD
               GO  TO  41029-USERBYOMEICD-KENSAKU-EXT 
           END-IF
      *
      *    病名コードが正常に入力されているとき
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE )             
               AND   ( SPA-NAI-C02-USERBYOMEIFLG (IDX) =   SPACE )
               AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)
                                                   NOT =   "■"  )
                   MOVE    "0069"              TO  SPA-ERRCD
                   GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
               END-IF 
           END-PERFORM
      *
      *    文字チェック     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE             
                   IF    ( SPA-NAI-C02-BYOMEI(IDX)(1:2)   =   "■" )
                   AND   ( IDX                        NOT =   IDZ  )
                       MOVE    "0035"          TO  SPA-ERRCD
                       GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
                  END-IF
               END-IF
           END-PERFORM
      *
      *
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    "C02"               TO  SPA-C01-MOTOPG
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE
                   ADD     1               TO  IDY              
                   IF      IDX         NOT =   IDZ
                       MOVE    SPA-GMN-C02-BYOMEICD  (IDX)
                                           TO  SPA-C01-TBYOMEICD  (IDY)
                       MOVE    SPA-NAI-C02-BYOMEITOP (IDX)
                                           TO  SPA-C01-TBYOMEITOP (IDY)
                       MOVE    SPA-NAI-C02-BYOMEISBT (IDX)
                                           TO  SPA-C01-TBYOMEISBT (IDY)
                       MOVE    SPA-NAI-C02-USERBYOMEIFLG (IDX)
                                           TO  SPA-C01-TUSERBYOMEIFLG
                                                                  (IDY)
                       MOVE    SPA-NAI-C02-BYOMEI    (IDX)
                                           TO  SPA-C01-TBYOMEI    (IDY)
                       MOVE    IDX         TO  SPA-C01-TBYOMEICD-INDEX
                                                                  (IDY)
                   END-IF
               END-IF                              
           END-PERFORM
           MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                       TO  SPA-C01-BYOMEIINPUTCD
           MOVE    IDZ                 TO  SPA-C01-INDEX
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
       41029-USERBYOMEICD-KENSAKU-EXT.
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
      *    レセ電エラーフラグ
           PERFORM 41041-RECEDENFLG-CHK-SEC
      *        
           MOVE    19                   TO  SPA-GMN-C02-CUR
           .
       4104-UTAGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電エラーフラグチェック処理
      *****************************************************************
       41041-RECEDENFLG-CHK-SEC        SECTION.
      *   
           INITIALIZE                      WRK-PTBYOMEI-CHK-AREA
           IF      SPA-NAI-C02-BYOMEIHENFLG    =   SPACE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       WRK-CONS-BYOMEICD-MAX
                       OR      SPA-NAI-C02-BYOMEICD (IDXX) =   SPACE
                   IF    ( WRK-HAISIYMD-CHK    =   ZERO                )
                   AND   ( SPA-NAI-C02-HAISIYMD (IDXX)
                                           NOT =   ZERO    AND ALL "9" )
                       MOVE    SPA-NAI-C02-HAISIYMD (IDXX)
                                               TO  WRK-HAISIYMD-CHK
                   END-IF
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >       WRK-CONS-BYOMEICD-MAX
                           OR      SPA-NAI-C02-BYOMEICD-O (IDXX IDXY)
                                               =   SPACE
                       ADD     1           TO  WRK-PTBYO-CHK-BYOMEICDSU
                       IF      WRK-PTBYO-CHK-BYOMEICDSU <   22
                           MOVE    SPA-NAI-C02-BYOMEICD-O (IDXX IDXY)
                                           TO  WRK-PTBYO-CHK-BYOMEICD 
                                              (WRK-PTBYO-CHK-BYOMEICDSU)
                       ELSE
                           MOVE    "1"     TO  SPA-NAI-C02-BYOMEIHENFLG
                           MOVE    "病名編集"
                                           TO  SPA-GMN-C02-BYOMEIHENFLG
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
           MOVE    SPA-NAI-C02-BYOMEIHENFLG
                                       TO  WRK-PTBYO-CHK-BYOMEIHENFLG
           MOVE    SPA-NAI-C02-BYOMEIMOJI
                                       TO  WRK-PTBYO-CHK-BYOMEIMOJI
           MOVE    SPA-GMN-C02-UTAGAIFLG
                                       TO  WRK-PTBYO-CHK-UTAGAIFLG 
           MOVE    SPA-GMN-C02-SYUBYOFLG
                                       TO  WRK-PTBYO-CHK-SYUBYOFLG 
           MOVE    SPA-GMN-C02-BYOMEI  TO  WRK-PTBYO-CHK-BYOMEI
           MOVE    SPA-NAI-C02-SRYYMD  TO  WRK-PTBYO-CHK-SRYYMD 
           PERFORM 5001-RECEDENFLG-HENSYU-SEC
           MOVE    WRK-PTBYO-CHK-RECEDENFLG
                                       TO  SPA-NAI-C02-RECEDENFLG
           .
       41041-RECEDENFLG-CHK-EXT.
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
      *    レセ電エラーフラグ
           PERFORM 41041-RECEDENFLG-CHK-SEC
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
      *****IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
      *****    MOVE    "0060"              TO  SPA-ERRCD
      *****    MOVE    2                   TO  SPA-GMN-C02-CUR
      *****    GO  TO  4108-COMB-CHK-EXT
      *****END-IF
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
                   IF      SPA-NAI-C02-SRYYMD
                                       <=   WRK-TEKEDYMD
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
      *****IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
      *****    MOVE    "0060"              TO  SPA-ERRCD
      *****    MOVE    2                   TO  SPA-GMN-C02-CUR
      *****    GO  TO  41010-REZEMM-CHK-EXT
      ***** END-IF
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
      *****未コード化傷病名コードのとき病名がおなじとき    
      *****IF      C02-BYOMEI          =   SPA-GMN-C02-BYOMEI
      *****AND     SPA-NAI-C02-BYOMEICD (1)
      *****                            =   WRK-CONS-BYOMEICD
      *****    MOVE    9                   TO  SPA-GMN-C02-CUR 
      *****    GO  TO  41013-BYOMEI-CHK-EXT
      *****END-IF
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
      *    未コード化傷病名コード入力のときコードクリア
           IF      SPA-GMN-C02-BYOMEICD (1)
                                       =   WRK-CONS-BYOMEICD
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
           MOVE    80                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-SYUBETU     =   2
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
grpsys                                       SPA-AREA 
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
               MOVE    LNK-BYO-THAISIYMD (IDX)
                                       TO  SPA-NAI-C02-HAISIYMD (IDZ)
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
               IF    ( LNK-BYO-IKOSAKICD   NOT =   SPACE )
               AND   ( SPA-C02-1042-MSG2-DSPKBN
                                               =   ZERO  )
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
      *    病名コード最大個数オーバー
           IF      LNK-BYO-BYOMEICD-MAX-ERR =   1
               MOVE    "0077"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-CIDCD2
                                           WRK-HAISIYMDH
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *****修飾語コードのみのとき
      *****IF      LNK-BYO-BYOMEICD-ERR    =   1
      *****    MOVE    "0006"          TO  SPA-ERRCD
      *****    MOVE    SPACE           TO  SPA-CIDCD2
      *****                                WRK-HAISIYMDH
      *****    MOVE    ZERO            TO  SPA-NAI-C02-BYOMEIMOJI
      *****    MOVE    SPACE           TO  SPA-NAI-C02-BYOMEIHENFLG
      *****                                SPA-GMN-C02-BYOMEIHENFLG
      *****    GO  TO  41013-BYOMEI-CHK-EXT
      *****END-IF
      *
      *    病名コード順エラー
           IF      LNK-BYO-BYOMEICD-ERR    =   1   OR  2   OR  9
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
           AND   ( SPA-C02-1042-MSG3-DSPKBN
                                       =   ZERO  )
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
           MOVE    80                  TO  KANACHK-MAX-CNT
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
           IF      SPA-NAI-C02-SELNO-INSERT
                                       >   ZERO
               IF      C02-BSRYKA      NOT =   SPA-GMN-C02-BSRYKA-G
                   MOVE    "0087"              TO  SPA-ERRCD
               END-IF
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  41019-BSRYKA-CHK-EXT
           END-IF 
      *
           MOVE    C02-BSRYKA          TO  SPA-GMN-C02-BSRYKA-G
      *
           IF      SPA-GMN-C02-BSRYKA  =   SPACE
               IF      SPA-GMN-C02-SRYKA   NOT =   ZERO    AND SPACE
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
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-NAI-C02-SELNO-INSERT
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
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
                   EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
                       WHEN    ZERO
                           MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                               TO  SPA-GMN-C02-SYOKBN-G
                       WHEN    "1"
                           MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                               TO  SPA-GMN-C02-SYOKBN-G
                       WHEN    "2"
                           MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                               TO  SPA-GMN-C02-SYOKBN-G
                   END-EVALUATE
               END-IF
      *****END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-NAI-C02-SELNO-INSERT
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    C02-HNYUGAIKBN      TO  SPA-GMN-C02-HNYUGAIKBN-G
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
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
      *****IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
      *****    MOVE    "0060"              TO  SPA-ERRCD
      *****    MOVE    2                   TO  SPA-GMN-C02-CUR 
      *****    GO  TO  41021-HKNCOMBISEL-EXT
      *****END-IF
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
                   IF      SPA-NAI-C02-SRYYMD
                                  <= SPA-NAI-C02-TEKEDYMD (IDY)
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
      *    感染症送信
           IF     (SPA-DATATEISYUTUKBN     =      1)  AND
                  (SPA-NAI-C02-PTID    NOT =   ZERO)
               PERFORM 690-INFECTION-SEND-SEC
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPA-C02-MOTOPG     TO  SPA-SAKIPG
           IF      SPA-C02-MOTOPG NOT =   "K02"  AND  "U02"   AND
                                          "K02N" AND  "D04"   AND
                                          "KT01" AND  "Q02" 
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
                                           "K02N" AND  "D04"  AND
                                           "KT01" AND  "Q02"
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
                                        SPA-NAI-C02-SELNO-INSERT
               MOVE    1                TO  SPA-GMN-C02-CUR
           ELSE                             
      *        感染症送信
               IF     (SPA-DATATEISYUTUKBN     =      1)  AND
                      (SPA-NAI-C02-PTID    NOT =   ZERO)
                   PERFORM 690-INFECTION-SEND-SEC
               END-IF
      *
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *
               INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
                                           SPA-NAI-C02-SELNO-INSERT
                                           SPA-C02-UPDFLG
                                           SPA-C02-IKOFLG
      *                              
                                           SPA-GMN-C02-TBL
                                           SPA-NAI-C02-TBL
                                           SPA-GMN-C02-MAX
                                           SPA-GMN-C02-SRYKA-G
                                           SPA-PTBYOMEI-REC
               EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
                   WHEN    ZERO
                       MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                               TO  SPA-GMN-C02-SYOKBN-G
                   WHEN    "1"
                       MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                               TO  SPA-GMN-C02-SYOKBN-G
                   WHEN    "2"
                       MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                               TO  SPA-GMN-C02-SYOKBN-G
               END-EVALUATE
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
                                       SPA-NAI-C02-SELNO-INSERT
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
                                       SPA-NAI-C02-SELNO-INSERT
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
      *****OR      SPA-GMN-C02-BSRYKA  =   SPACE
               MOVE    "0050"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-GID-EXT
           END-IF
      *
           IF      SPA-GMN-C02-BANGO  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-GID-EXT
           END-IF
      *
      *    入力項目チェック
           MOVE    SPA-GMN-C02-BSRYKA-G   TO  C02-BSRYKA
           PERFORM 41019-BSRYKA-CHK-SEC
           IF      SPA-ERRCD           NOT =   SPACE         
               GO  TO  450-TOUROKU-EXT
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
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM             TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID        TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-SRYKA-OLD   TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD-OLD  TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-RENNUM      TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  440-SAKUJYO-EXT
           END-IF
      *    削除フラグ
           MOVE    "1"                 TO  PTBYO-DLTFLG
           MOVE    SPA-OPID            TO  PTBYO-DLT-OPID
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    "DBUPDATE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  440-SAKUJYO-EXT
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
      *
           COMPUTE C02-BYO-ROW =   SPA-NAI-C02-SELNO   -   1    
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-NAI-C02-SELNO-INSERT
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
      *****OR     (SPA-GMN-C02-BSRYKA       =   SPACE )
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
           IF      SPA-NAI-C02-SELNO-INSERT
                                       =   ZERO
               MOVE    SPA-GMN-C02-BSRYKA-G   TO  C02-BSRYKA
               PERFORM 41019-BSRYKA-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE         
                   GO  TO  450-TOUROKU-EXT
               END-IF
               MOVE    SPA-GMN-C02-SRYYMD      TO  C02-SRYYMD
               PERFORM 41012-SRYYMD-CHK-SEC
               IF      SPA-ERRCD               =   "0039"
                   MOVE    SPACE                   TO  SPA-ERRCD
               END-IF
           END-IF     
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
           MOVE    SPA-GMN-C02-HKNCOMBI-G TO  C02-HKNCOMBI
           PERFORM 4108-COMB-CHK-SEC
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
      *    新規以外のときは複数病名の入力不可
           IF    ( SPA-NAI-C02-BYOMEICD-MAX    >   1    )
               IF    ( SPA-GMN-C02-BANGO           >   ZERO )
               OR    ( SPA-NAI-C02-SELNO-INSERT    >   ZERO )
                   MOVE    "0056"             TO  SPA-ERRCD
                   GO      TO      450-TOUROKU-EXT
               END-IF
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
               MOVE    8                   TO  SPA-GMN-C02-CUR
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    新規又は科変更、開始日変更時の行番号追加が可能か
           IF   (  SPA-NAI-C02-RENNUM  =   ZERO                 )
           OR   (    ( SPA-GMN-C02-BSRYKA  NOT =
                                       SPA-NAI-C02-SRYKA-OLD  )
                  OR ( SPA-NAI-C02-SRYYMD  NOT =
                                       SPA-NAI-C02-SRYYMD-OLD ) )
      *        病名数のチェック
               INITIALIZE                      PTBYOMEI-REC
               MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
               MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
               MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
               MOVE    ZERO                TO  PTBYO-RENNUM
               MOVE    "%"                 TO  PTBYO-DLTFLG
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key34"             TO  WRK-MCP-PATHNAME
               PERFORM 900-PTBYO-INV-SEC
      *        同一診療科、開始日で９９件のとき（削除分含む）
               COMPUTE IDXX    =   PTBYO-RENNUM
                               +   SPA-NAI-C02-BYOMEICD-MAX
               IF      IDXX                >   99
                   MOVE    "0034"              TO  SPA-ERRCD
                   INITIALIZE                      PTBYOMEI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                   MOVE    ZERO                TO  PTBYO-RENNUM
                   MOVE    "1"                 TO  PTBYO-DLTFLG
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                   MOVE    "key34"             TO  WRK-MCP-PATHNAME
                   PERFORM 900-PTBYO-INV-SEC
      *            同一診療科、開始日で削除分以外が９９件のとき
                   COMPUTE IDXX    =   IDXX    -   PTBYO-RENNUM
                   IF      IDXX                >   99
                       MOVE    "0088"          TO  SPA-ERRCD
                   END-IF
                   GO  TO  450-TOUROKU-EXT
               END-IF
           END-IF
      *
      *    病名編集
           IF      SPA-NAI-C02-SRYYMD   =   SPACE
               MOVE    SPA-NAI-C02-SYSYMD   TO  SPA-NAI-C02-SRYYMD
           END-IF
      *
           MOVE    SPACE           TO  WRK-HAISIYMD-OCC
                                       WRK-PTBYOMEI-REC (1)
                                       WRK-PTBYOMEI-REC (2)
                                       WRK-PTBYOMEI-REC (3)
                                       WRK-PTBYOMEI-REC (4)
                                       WRK-PTBYOMEI-REC (5)
                                       WRK-PTBYOMEI-REC (6)
           INITIALIZE                  WRK-HAISIYMD-OCC
                                       WRK-PTBYOMEI-REC (1)
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
                                       WRK-ERR-SRYKA
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
      *
      *    警告チェック
           IF      SPA-ERRCD            =   SPACE
               PERFORM 4504-KEIKOKU-CHK-SEC
           END-IF
      *
      *    登録処理
           IF    ( SPA-ERRCD            =   SPACE )
           AND   ( SPA-CIDCD            =   SPACE )
               PERFORM 4502-TOUROKU-SYORI-SEC
           END-IF
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告チェック処理
      *****************************************************************
       4504-KEIKOKU-CHK-SEC                SECTION.
      * 
           MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                       WRK-ERR-SRYYMD
                                       WRK-ERR-SRYKA
      * 
           INITIALIZE                  SPA-C02-KEIKOKU-FLG-AREA
                                       SPA-C02-KEIKOKU-BYOMEI
      *
      *    他診療科の同一病名の警告チェック
           IF      SPA-C02-1042-MSG4-DSPKBN
                                           =   1
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       6
                       OR      SPA-CIDCD           NOT =   SPACE
                       OR      WRK-PTBYO-BYOMEI (IDXX) =   SPACE
                   INITIALIZE                      PTBYOMEI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                               TO  PTBYO-BYOMEI
                   MOVE    WRK-PTBYO-NYUGAIKBN (IDXX)
                                               TO  PTBYO-NYUGAIKBN
                   IF      PTBYO-NYUGAIKBN     =   SPACE
                       MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                       MOVE    "key27"             TO  WRK-MCP-PATHNAME
                   ELSE
                       MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                       MOVE    "key28"             TO  WRK-MCP-PATHNAME
                   END-IF
      *
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-PTBYO-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-PTBYOMEI
                   END-IF
      *
                   PERFORM             UNTIL   FLG-PTBYOMEI    =   1
                                       OR      SPA-CIDCD   NOT =   SPACE
                     IF    ( SPA-NAI-C02-RENNUM   >   ZERO         )
                     AND   ( SPA-NAI-C02-SRYYMD-OLD
                                                  =   PTBYO-SRYYMD )
                     AND   ( SPA-NAI-C02-SRYKA-OLD
                                                  =   PTBYO-SRYKA  )
                     AND   ( SPA-NAI-C02-RENNUM   =   PTBYO-RENNUM )
                         CONTINUE
                     ELSE
                       EVALUATE    TRUE
                           WHEN    SPA-NAI-C02-SRYYMD  =   PTBYO-SRYYMD
                               MOVE    1       TO  SPA-C02-KEIKOKU-FLG1
                           WHEN    SPA-NAI-C02-SRYYMD  <   PTBYO-SRYYMD
                               IF      SPA-NAI-C02-TENKIYMD    =   SPACE
                                   MOVE    1          TO
                                                  SPA-C02-KEIKOKU-FLG1
                               ELSE
                                   IF     SPA-NAI-C02-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                       MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                   END-IF
                               END-IF
                           WHEN    SPA-NAI-C02-SRYYMD  >   PTBYO-SRYYMD
                               IF      PTBYO-TENKIYMD  =   SPACE
                                   MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                               ELSE
                                   IF      SPA-NAI-C02-SRYYMD
                                                   <   PTBYO-TENKIYMD
                                       MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                   END-IF
                               END-IF
                       END-EVALUATE
                       IF      SPA-C02-KEIKOKU-FLG1    =   1
                           INITIALIZE              WRK-CHK-HKNCOMBI-AREA
      *
                           MOVE    SPA-GMN-C02-HKNCOMBI
                                                   TO  WRK-CHK-HKNCOMBI1
                           MOVE    PTBYO-HKNCOMBI  TO  WRK-CHK-HKNCOMBI2
                           PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                           IF      FLG-HKNCOMBI-DABURI =   1
                               CONTINUE
                           ELSE 
                               MOVE    "0103"              TO  SPA-CIDCD
                               MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3    TO
                                                       WRK-ERR-SRYYMD
                               MOVE    PTBYO-SRYKA         TO  WRK-KBN
                               PERFORM 5001-SRYKA-HENSYU-SEC
                               MOVE    WRK-KBNMEI          TO
                                                       WRK-ERR-SRYKA
                           END-IF
                       END-IF
                     END-IF
      *
                     MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                     MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                     PERFORM 900-PTBYO-READ-SEC
                   END-PERFORM
      *
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      SPA-C02-KEIKOKU-FLG1    =   1
                       MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                              TO  WRK-ERR-BYOMEI
                       MOVE    6              TO  IDXX                  
                   END-IF
               END-PERFORM
           END-IF
      *
      *    慢性と急性の警告チェック
           IF    ( SPA-NAI-C02-BYOMEICD-MAX    =   1           )
           AND   ( SPA-GMN-C02-UTAGAIFLG       =   "2" OR  "3" ) 
               MOVE    ZERO                TO  FLG-MANSEI
                                               FLG-KYUSEI
               PERFORM VARYING IDZZ    FROM    1   BY  2
                       UNTIL   IDZZ    >       79
                   IF      WRK-PTBYO-BYOMEI (1) (IDZZ:4)
                                                   =   "慢性"
                       MOVE    1               TO  FLG-MANSEI
                   END-IF
                   IF      WRK-PTBYO-BYOMEI (IDXX) (IDZZ:4)
                                                   =   "急性"
                       MOVE    1               TO  FLG-KYUSEI
                   END-IF
                   IF    ( FLG-MANSEI              =   1 )
                   AND   ( FLG-KYUSEI              =   1 )
                       MOVE    80                      TO  IDZZ
                   END-IF
               END-PERFORM    
      *
               IF      FLG-MANSEI              =   1
                   IF      FLG-KYUSEI              =   1
                       CONTINUE
                   ELSE
                       MOVE    1               TO  SPA-C02-KEIKOKU-FLG3
                       MOVE    WRK-PTBYO-BYOMEI (1)
                                               TO  WRK-ERR-BYOMEI
                       IF      SPA-CIDCD       =   SPACE
                           MOVE    "0104"          TO  SPA-CIDCD
                       END-IF 
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-ERR-BYOMEI      NOT =   SPACE
               MOVE    WRK-ERR-BYOMEI      TO  SPA-C02-KEIKOKU-BYOMEI
               MOVE    SPACE               TO  WRK-ERR-BYOMEI
           END-IF
      *
      *         
      *    主病名の存在チェック（警告）
           MOVE    ZERO                TO  FLG-SYUBYO
      *
           IF    ( SPA-C02-2005-SYUBYOHENFLG
                                       NOT =   ZERO )
           AND   ( SPA-C02-1042-MSG1-DSPKBN
                                           =   ZERO )
               IF    ( SPA-GMN-C02-SYUBYOFLG   =   "1"                )
               AND   ( SPA-NAI-C02-TENKIYMD    >=  SPA-NAI-C02-SYSYMD
                           OR                                    
                       SPA-NAI-C02-TENKIYMD    =   SPACE              )
                   CONTINUE
               ELSE
                   INITIALIZE                  PTBYOMEI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   IF      SPA-C02-1001-HOSPSBT1   =   "1"
                       MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                       MOVE    "key14"         TO  WRK-MCP-PATHNAME
                   ELSE
                       MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                       MOVE    "key15"         TO  WRK-MCP-PATHNAME
                   END-IF
      *
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-PTBYO-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-PTBYOMEI
                   END-IF
                   PERFORM UNTIL   FLG-PTBYOMEI    =   1
                           OR      FLG-SYUBYO      =   1
      *                選択中の病名は対象外
                       IF    ( SPA-NAI-C02-RENNUM      >  ZERO         )
                       AND   ( SPA-NAI-C02-RENNUM      =  PTBYO-RENNUM )
                       AND   ( SPA-NAI-C02-SRYKA-OLD   =  PTBYO-SRYKA  )
                       AND   ( SPA-NAI-C02-SRYYMD-OLD  =  PTBYO-SRYYMD )
                           CONTINUE
                       ELSE
                           IF      PTBYO-TENKIYMD      =   SPACE
                               MOVE    1               TO  FLG-SYUBYO
                           ELSE    
                               IF      PTBYO-TENKIYMD
                                               <   SPA-NAI-C02-SYSYMD
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  FLG-SYUBYO
                               END-IF
                           END-IF
                       END-IF
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-PTBYO-READ-SEC
                   END-PERFORM
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      FLG-SYUBYO          =   ZERO
                       MOVE    1               TO  SPA-C02-KEIKOKU-FLG2
                       IF      SPA-CIDCD           =   SPACE
                           MOVE    "0102"            TO  SPA-CIDCD
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       4504-KEIKOKU-CHK-EXT.
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
      *    廃止年月日（レセ電フラグチェック用）
           IF      WRK-PTBYO-BYOMEICDSU (IDXX)
                                       <=  21
               IF      SPA-NAI-C02-HAISIYMD (IDX)
                                           =   ZERO   OR  ALL "9"
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-C02-HAISIYMD (IDX)
                                           TO  WRK-HAISIYMD-O (IDXX)
               END-IF         
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
      *
      *    保険組み合わせ
           IF      SPA-GMN-C02-HKNCOMBI    =   SPACE
               MOVE    ZERO            TO  WRK-PTBYO-HKNCOMBI  (IDXX)
           ELSE                          
               MOVE    SPA-GMN-C02-HKNCOMBI
                                       TO  WRK-PTBYO-HKNCOMBI  (IDXX)
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
               IF      SPA-GMN-C02-HKNCOMBI    =   SPACE
                   MOVE    ZERO        TO  WRK-PTBYO-HKNCOMBI  (IDXX)
               ELSE
                   MOVE    SPA-GMN-C02-HKNCOMBI
                                       TO  WRK-PTBYO-HKNCOMBI  (IDXX)
               END-IF
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
               MOVE    SPA-GMN-C02-REZEFLG
                                       TO  WRK-PTBYO-REZEFLG   (IDXX)
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
               MOVE    "1"             TO  WRK-PTBYO-BYOMEIHENFLG(IDXX)
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
      *    レセ電エラーフラグ
           INITIALIZE                      WRK-PTBYOMEI-CHK-AREA
           MOVE    WRK-HAISIYMD-O   (IDXX)
                                       TO  WRK-HAISIYMD-CHK
           MOVE    WRK-PTBYOMEI-REC (IDXX)
                                       TO  WRK-PTBYOMEI-CHK-REC
           MOVE    SPA-NAI-C02-SRYYMD  TO  WRK-PTBYO-CHK-SRYYMD 
           PERFORM 5001-RECEDENFLG-HENSYU-SEC
           MOVE    WRK-PTBYO-CHK-RECEDENFLG
                                       TO  WRK-PTBYO-RECEDENFLG (IDXX)
      *    レセプト表示期間
           MOVE    SPA-GMN-C02-REZEMM
                                       TO  WRK-PTBYO-REZEMM    (IDXX)
      *
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
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                       TO  PTBYO-BYOMEI
           MOVE    WRK-PTBYO-NYUGAIKBN (IDXX)
                                       TO  PTBYO-NYUGAIKBN
           IF      PTBYO-NYUGAIKBN     =   SPACE
               MOVE    "tbl_ptbyomei"          TO  WRK-MCP-TABLE
               MOVE    "key13"                 TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_ptbyomei"          TO  WRK-MCP-TABLE
               MOVE    "key21"                 TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
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
                                                   >   PTBYO-SRYYMD
                                   MOVE    "0038"          TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-SRYYMD  >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-SRYYMD
                                                   <   PTBYO-TENKIYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD               =   "0038"  OR
                                                       "0002"
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       MOVE    SPA-GMN-C02-HKNCOMBI
                                               TO  WRK-CHK-HKNCOMBI1
                       MOVE    PTBYO-HKNCOMBI  TO  WRK-CHK-HKNCOMBI2
                       PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE   SPACE                TO  SPA-ERRCD
                       ELSE 
                           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3    TO
                                                       WRK-ERR-SRYYMD
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .                   
       4502-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一病名における保険限定の重複チェック処理
      *****************************************************************
       45021-HKNCOMBI-DABURI-CHK-SEC   SECTION.
      *
      *    労災または自賠責または自費の保険がない場合はチェックしない
           IF    ( SPA-NAI-C02-HKNCOMBI-ROUSAI )
                                       =   ZERO
           AND   ( SPA-NAI-C02-HKNCOMBI-JIHI   )
                                       =   ZERO
               GO  TO   45021-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険組合せの設定がない場合はチェックしない
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
               GO  TO   45021-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
           IF      WRK-CHK-HKNCOMBI1
                                   NOT =   ZERO
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI1   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-INV-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (1)
           END-IF    
           IF      WRK-CHK-HKNCOMBI2
                                   NOT =   ZERO
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI2   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-INV-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (2)
           END-IF
      *
      *    片方のみ労災または自賠責の場合は登録可能とする
           EVALUATE    WRK-CHK-COMB-HKNNUM (1)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2)
               WHEN    "971"       ALSO    "971"
               WHEN    "973"       ALSO    "973"
                   IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                       MOVE    1                TO  FLG-HKNCOMBI-DABURI
                   END-IF
               WHEN    "971"       ALSO    ANY
               WHEN    "973"       ALSO    ANY
               WHEN    ANY         ALSO    "971"
               WHEN    ANY         ALSO    "973"
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
           END-EVALUATE
      *
      *    自費と保険限定なしまたは同じ自費の場合は登録不可とする
           IF    ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   =   "98" ) 
           OR    ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                   =   "98" ) 
               EVALUATE    WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2) (1:2)
                   WHEN    "98"       ALSO    "98"
                       IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                           MOVE    1            TO  FLG-HKNCOMBI-DABURI
                       END-IF
                   WHEN    "98"       ALSO    SPACE
                   WHEN    SPACE      ALSO    "98"
                       MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
                   WHEN    OTHER
                       MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-EVALUATE
           END-IF
      *
           .                   
       45021-HKNCOMBI-DABURI-CHK-EXT.
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
grpsys         MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
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
                   MOVE    "tbl_ptinf"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
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
                                           SPA-NAI-C02-SELNO-INSERT
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-NAI-C02-SELNO-INSERT
                                       SPA-PTBYOMEI-REC
      *
           INITIALIZE                  WRK-PTBYOMEI-REC (1)
                                       WRK-PTBYOMEI-REC (2)
                                       WRK-PTBYOMEI-REC (3)
                                       WRK-PTBYOMEI-REC (4)
                                       WRK-PTBYOMEI-REC (5)
                                       WRK-PTBYOMEI-REC (6)
                                       WRK-HAISIYMD-OCC
      *
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-C02-MAX
      *    検索処理
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
                   IF      SPA-NAI-C02-SELNO-INSERT    =   ZERO
      *                患者病名マスター追加
                       PERFORM 450222-PTBYOMEI-INSERT-SEC
                       GO  TO  45022-PTBYOMEI-UPDATE-EXT
                   ELSE
      *                患者病名マスター行指定追加
                       MOVE    ZERO                TO  WRK-RENNUM
                       INITIALIZE                      PTBYOMEI-REC
                       MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                       MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                       MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                       MOVE    SPA-NAI-C02-TRENNUM
                                       (SPA-NAI-C02-SELNO-INSERT)
                                                   TO  PTBYO-RENNUM
                       MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                       MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                       MOVE    "key35"             TO  WRK-MCP-PATHNAME
                       PERFORM 900-PTBYO-INV-SEC
                       IF      FLG-PTBYOMEI        =   ZERO
                           MOVE    PTBYO-RENNUM    TO  WRK-RENNUM
                       END-IF
                       ADD     1                   TO  WRK-RENNUM
                       IF      WRK-RENNUM          <
                                           SPA-NAI-C02-TRENNUM
                                              (SPA-NAI-C02-SELNO-INSERT)
      *                    連番に空きがあれば追加
                           MOVE    WRK-PTBYOMEI-REC (IDXX)
                                                   TO  PTBYOMEI-REC 
                           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                           MOVE    WRK-RENNUM          TO  PTBYO-RENNUM
                           MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
                           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
                           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
                           MOVE    "DBINSERT"          TO  MCP-FUNC
                       ELSE
      *                    連番をつけなおして更新
                           PERFORM 450220-PTBYOMEI-SELNO-INSERT-SEC
                           GO  TO  45022-PTBYOMEI-UPDATE-EXT
                       END-IF
                   END-IF
               WHEN    OTHER
                   IF    ( SPA-GMN-C02-BSRYKA  NOT =
                                       SPA-NAI-C02-SRYKA-OLD  )
                   OR    ( SPA-NAI-C02-SRYYMD  NOT =
                                       SPA-NAI-C02-SRYYMD-OLD )
      *                患者病名マスター更新（診療科変更のとき）
                       INITIALIZE                      PTBYO-KEY
grpsys                 MOVE    SPA-HOSPNUM             TO  PTBYO-HOSPNUM
                       MOVE    SPA-NAI-C02-PTID        TO  PTBYO-PTID
                       MOVE    SPA-NAI-C02-SRYKA-OLD   TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD-OLD  TO  PTBYO-SRYYMD
                       MOVE    SPA-NAI-C02-RENNUM      TO  PTBYO-RENNUM 
                       MOVE    PTBYOMEI-REC            TO  MCPDATA-REC
                       MOVE    "DBDELETE"              TO  MCP-FUNC
                       MOVE    "tbl_ptbyomei"          TO  MCP-TABLE
                       MOVE    "key"                   TO  MCP-PATHNAME
grpsys                 PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC              NOT =   ZERO 
                           MOVE    "1002"                  TO  SPA-ERRCD
                           DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
                           GO  TO  45022-PTBYOMEI-UPDATE-EXT
                       END-IF
      *
      *                患者病名マスター追加
                       PERFORM 450222-PTBYOMEI-INSERT-SEC
                       GO  TO  45022-PTBYOMEI-UPDATE-EXT
                   ELSE
      *                患者病名マスター更新（診療科変更なし）
                       INITIALIZE                  PTBYO-KEY
grpsys                 MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                       MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                       MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                       MOVE    SPA-NAI-C02-RENNUM  TO  PTBYO-RENNUM 
                       MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                       MOVE    "key"               TO  WRK-MCP-PATHNAME
                       PERFORM 900-PTBYO-INV-SEC
                       IF      FLG-PTBYOMEI        =   1
                           MOVE    "1003"          TO  SPA-ERRCD
                           DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
                           GO  TO  45022-PTBYOMEI-UPDATE-EXT
                       ELSE
                           MOVE    PTBYO-KEY       TO
                                                   WRK-PTBYO-KEY (IDXX)
                           MOVE    PTBYO-CREYMD    TO
                                                   WRK-PTBYO-CREYMD
                                                                 (IDXX)
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
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" PTBYO-KEY
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
      *
          .
       45022-PTBYOMEI-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    行指定追加処理（連番をつけなおして更新）
      *****************************************************************
       450220-PTBYOMEI-SELNO-INSERT-SEC           SECTION.
      *
      *    指定行以降に連番の空きがあるか
           COMPUTE IDY1    =   SPA-NAI-C02-SELNO-INSERT
                           -   1
           COMPUTE IDY2    =   100
                           -   SPA-NAI-C02-TRENNUM(IDY1)
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDY1)
                                       TO  PTBYO-RENNUM
           MOVE    "%"                 TO  PTBYO-DLTFLG 
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key34"             TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      PTBYO-RENNUM    <   IDY2
      *        指定行以降の連番をつけなおして更新
               MOVE    SPA-NAI-C02-TRENNUM (SPA-NAI-C02-SELNO-INSERT)
                                           TO  WRK-RENNUM
           ELSE
      *        全ての連番をつけなおして更新
               MOVE    ZERO               TO  WRK-RENNUM
           END-IF
      *
           INITIALIZE                      WRK-PTBYOMEI-INSERT-AREA
           MOVE    ZERO                TO  IDY1
      *
           IF      WRK-RENNUM          =   ZERO
      *        指定行までの病名をを退避して削除
               INITIALIZE                      PTBYOMEI-REC
               MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
               MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
               MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
               MOVE    ZERO                TO  PTBYO-RENNUM
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key36"             TO  WRK-MCP-PATHNAME
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-PTBYOMEI
               END-IF
      *
               PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                               OR      IDY1            =   99
                               OR      PTBYO-RENNUM    >=
                                           SPA-NAI-C02-TRENNUM
                                           (SPA-NAI-C02-SELNO-INSERT)
                   ADD     1               TO  IDY1
                   ADD     1               TO  WRK-RENNUM
      *
                   MOVE    PTBYOMEI-REC    TO  WRK-PTBYOMEI-INSERT-REC
                                                               (IDY1)
                   MOVE    WRK-RENNUM      TO  WRK-PTBYO-INSERT-RENNUM
                                                               (IDY1)
                   MOVE    SMCNDATE-YMD    TO  WRK-PTBYO-INSERT-UPYMD
                                                               (IDY1)
                   MOVE    SMCNDATE-HMS    TO  WRK-PTBYO-INSERT-UPHMS
                                                               (IDY1)
      *            既に登録済の病名を削除する
                   MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
      *???
                   DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO 
                       MOVE    "1002"          TO  SPA-ERRCD
                       DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
                       GO  TO  450220-PTBYOMEI-SELNO-INSERT-EXT
                   END-IF
      *
                   MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME
                                           TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               END-PERFORM
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               ADD     1               TO  WRK-RENNUM
           END-IF     
      *
      *    行指定分を追加
           ADD     1                   TO  IDY1
      *
           MOVE    WRK-PTBYOMEI-REC (IDXX)
                                       TO  PTBYOMEI-REC 
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    WRK-RENNUM          TO  PTBYO-RENNUM
           MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-INSERT-REC
                                                           (IDY1)
      *
      *    指定行以降の病名をを退避して削除
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (SPA-NAI-C02-SELNO-INSERT)
                                       TO  PTBYO-RENNUM
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key36"             TO  WRK-MCP-PATHNAME
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                           OR      IDY1            =   99
               ADD     1                   TO  IDY1
               ADD     1                   TO  WRK-RENNUM
      *
               MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-INSERT-REC
                                                               (IDY1)
               MOVE    WRK-RENNUM          TO  WRK-PTBYO-INSERT-RENNUM
                                                               (IDY1)
               MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-INSERT-UPYMD
                                                               (IDY1)
               MOVE    SMCNDATE-HMS        TO  WRK-PTBYO-INSERT-UPHMS
                                                               (IDY1)
      *        既に登録済の病名を削除する
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
      *???
               DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1002"              TO  SPA-ERRCD
                   DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
                   GO  TO  450220-PTBYOMEI-SELNO-INSERT-EXT
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       99
                   OR      WRK-PTBYO-INSERT-RENNUM (IDY1)
                                           =   ZERO
               MOVE    WRK-PTBYOMEI-INSERT-REC (IDY1) 
                                           TO  PTBYOMEI-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
      *???
               DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" PTBYO-KEY
                   GO  TO  450220-PTBYOMEI-SELNO-INSERT-EXT
               END-IF
          END-PERFORM
          PERFORM 450221-RECEUPD-UPDATE-SEC
          .
       450220-PTBYOMEI-SELNO-INSERT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       450222-PTBYOMEI-INSERT-SEC           SECTION.
      *
           INITIALIZE                      WRK-PTBYOMEI-INSERT-AREA
           MOVE    ZERO                TO  IDY1
           MOVE    ZERO                TO  WRK-RENNUM
      *
      *    患者病名マスター追加
           INITIALIZE                  PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               MOVE    PTBYO-RENNUM    TO  WRK-RENNUM
           END-IF
           IF      WRK-RENNUM          =   99
      *        連番をつけなおして更新
                MOVE    ZERO               TO  WRK-RENNUM
      *
      *        登録済の病名を退避して削除する
               INITIALIZE                      PTBYOMEI-REC
               MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
               MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
               MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
               MOVE    ZERO                TO  PTBYO-RENNUM
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key36"             TO  WRK-MCP-PATHNAME
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-PTBYOMEI
               END-IF
      *
               PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                               OR      IDY1            =   99
                   ADD     1               TO  IDY1
                                               WRK-RENNUM
      *
                   MOVE    PTBYOMEI-REC    TO  WRK-PTBYOMEI-INSERT-REC
                                                               (IDY1)
                   MOVE    WRK-RENNUM      TO  WRK-PTBYO-INSERT-RENNUM
                                                               (IDY1)
                   MOVE    SMCNDATE-YMD    TO  WRK-PTBYO-INSERT-UPYMD
                                                               (IDY1)
                   MOVE    SMCNDATE-HMS    TO  WRK-PTBYO-INSERT-UPHMS
                                                               (IDY1)
      *            既に登録済の病名を削除する
                   MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
      *???
                   DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1002"              TO  SPA-ERRCD
                       DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
                       GO  TO  450222-PTBYOMEI-INSERT-EXT
                   END-IF
      *
                   MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME
                                           TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               END-PERFORM
               MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
      *    追加分を追加
           ADD     1                   TO  IDY1
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-PTBYOMEI-REC (IDXX)
                                       TO  PTBYOMEI-REC 
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    WRK-RENNUM          TO  PTBYO-RENNUM
           MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-INSERT-REC
                                                               (IDY1)
      *
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       99
                   OR      WRK-PTBYO-INSERT-RENNUM (IDY1)
                                           =   ZERO
               MOVE    WRK-PTBYOMEI-INSERT-REC (IDY1) 
                                           TO  PTBYOMEI-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
      *???
               DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" PTBYO-KEY
                   GO  TO  450222-PTBYOMEI-INSERT-EXT
               END-IF
          END-PERFORM
      *
          PERFORM 450221-RECEUPD-UPDATE-SEC
          .
       450222-PTBYOMEI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理マスタ更新処理
      *****************************************************************
       450221-RECEUPD-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      ORCSRECEUPDAREA
           MOVE    "1"                 TO  LNK-UPD-SYORI
grpsys     MOVE    SPA-HOSPNUM         TO  LNK-UPD-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  LNK-UPD-PTID
           MOVE    SPA-TERMID          TO  LNK-UPD-TERMID
           MOVE    SPA-OPID            TO  LNK-UPD-OPID
           CALL    "ORCSRECEUPD"       USING    ORCSRECEUPDAREA
grpsys                                          SPA-AREA
           IF      LNK-UPD-RC      NOT =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               DISPLAY "LNK-UPD-RC =>" LNK-UPD-RC
           END-IF
      *
           .
       450221-RECEUPD-UPDATE-EXT.
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
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE )             
               AND   ( SPA-NAI-C02-USERBYOMEIFLG (IDX) =   "1"   )
               AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)
                                                   NOT =   "■"  )
                   MOVE    "0065"              TO  SPA-ERRCD
                   GO  TO  600-C05-SYORI-EXT
               END-IF
           END-PERFORM
      *     
           IF      SPA-NAI-C02-BYOMEIHENFLG        =  "1"
               MOVE    "0078"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
           AND   ( SPA-NAI-C02-SELNO-INSERT
                                       =   ZERO  )
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
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
           AND   ( SPA-NAI-C02-SELNO-INSERT
                                       =   ZERO  )
               MOVE    "0090"              TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "C50"               TO  SPA-SAKIPG
      *
      *    遷移後の初期表示
           MOVE    SPA-GMN-C02-SYSYMD  TO  SPA-SRYYMDW
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-SRYYMD
           MOVE    SPACE               TO  SPA-SRYKA
           MOVE    SPACE               TO  SPA-NYUGAIKBN
           MOVE    SPA-NAI-C02-PTID    TO  SPA-PTID
           MOVE    SPA-GMN-C02-PTNUM   TO  SPA-PTNUM
           MOVE    SPA-GMN-C02-NAME    TO  SPA-NAME
           MOVE    SPA-GMN-C02-SEX     TO  SPA-SEX
           MOVE    SPA-GMN-C02-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-C02-BIRTHDAY    TO  SPA-BIRTHDAY
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "C50"               TO  MCP-WINDOW
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
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
           AND   ( SPA-NAI-C02-SELNO-INSERT
                                       =   ZERO  )
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
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
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
      *    削除病名一覧表示画面
      *****************************************************************
       430-DELETE-LIST-SEC              SECTION.
      *
      *    修正のチェック
           IF      SPA-NAI-C02-PTID    =   ZERO
               MOVE    "0086"          TO  SPA-ERRCD
               GO  TO  430-DELETE-LIST-EXT
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
           CALL    "ORCGC12"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
           MOVE    1               TO  FLG-END
      *
           .
       430-DELETE-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示切替え処理
      *****************************************************************
       430-SYOKBN-CHANGE-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-NAI-C02-SELNO-INSERT
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
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
               EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
                   WHEN    ZERO
                       MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                           TO  SPA-GMN-C02-SYOKBN-G
                   WHEN    "1"
                       MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                           TO  SPA-GMN-C02-SYOKBN-G
                   WHEN    "2"
                       MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                           TO  SPA-GMN-C02-SYOKBN-G
               END-EVALUATE
           END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-NAI-C02-SELNO-INSERT
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
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
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
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
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
           AND   ( SPA-NAI-C02-SELNO-INSERT
                                       =   ZERO  )
               MOVE    "0090"              TO  SPA-ERRCD
                GO  TO  490-USERBYOMEI-KENSAKU-EXT
           END-IF
      *
      *****IF    ( SPA-GMN-C02-BANGO           >   ZERO )
      *    AND   ( SPA-NAI-C02-BYOMEICD-MAX    >   1    )
      *        MOVE    "0056"             TO  SPA-ERRCD
      *        GO  TO  490-USERBYOMEI-KENSAKU-EXT 
      *****END-IF
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
               IF    ( SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE )             
               AND   ( SPA-NAI-C02-USERBYOMEIFLG (IDX) =   SPACE )
               AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)
                                                   NOT =   "■"  )
                   MOVE    "0069"              TO  SPA-ERRCD
                   GO  TO  490-USERBYOMEI-KENSAKU-EXT
               END-IF
           END-PERFORM
      *
      *    文字チェック     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE )             
               AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)    =   "■"  )
                   MOVE    "0035"          TO  SPA-ERRCD
                   GO  TO  490-USERBYOMEI-KENSAKU-EXT 
               END-IF
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
                   MOVE    SPA-NAI-C02-BYOMEI   (IDX)
                                           TO  SPA-C01-TBYOMEI   (IDY)
                   MOVE    SPA-NAI-C02-USERBYOMEIFLG (IDX)
                                           TO  SPA-C01-TUSERBYOMEIFLG
                                                                 (IDY)
                   MOVE    SPA-NAI-C02-BYOMEITOP (IDX)
                                           TO  SPA-C01-TBYOMEITOP (IDY)
                   MOVE    SPA-NAI-C02-BYOMEISBT (IDX)
                                           TO  SPA-C01-TBYOMEISBT (IDY)
                   MOVE    IDX             TO  SPA-C01-TBYOMEICD-INDEX
                                                                  (IDY)
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
      *    レセ電エラーフラグ編集処理
      *****************************************************************
       5001-RECEDENFLG-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-PTBYO-CHK-RECEDENFLG
      *
           COMPUTE WRK-MOJISU      =   WRK-PTBYO-CHK-BYOMEIMOJI
                                       *   2
           MOVE    WRK-PTBYO-CHK-BYOMEIHENFLG
                                   TO  WRK-BYOMEIHENFLG
      *   
      *    疑いフラグが設定されているとき
           IF      WRK-PTBYO-CHK-UTAGAIFLG
                                       NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    WRK-PTBYO-CHK-UTAGAIFLG
                                           TO  SMKBYO-UTAGAIFLG
               MOVE    WRK-PTBYO-CHK-BYOMEI
                                           TO  SMKBYO-BYOMEI
               MOVE    WRK-BYOMEIHENFLG    TO  SMKBYO-BYOMEIHENFLG
               MOVE    WRK-PTBYO-CHK-BYOMEICD-G
                                           TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
               MOVE    SMKBYO-LENGTH       TO  WRK-MOJISU
               MOVE    SMKBYO-BYOMEIHENFLG TO  WRK-BYOMEIHENFLG
           END-IF    
      *
           COMPUTE WRK-MOJISU      =   WRK-MOJISU  /   2
      *
           IF  (   WRK-BYOMEIHENFLG    =   "1"                       )
           OR  ( ( WRK-HAISIYMD-CHK    >   ZERO                 ) AND
                 ( WRK-HAISIYMD-CHK    <   WRK-PTBYO-CHK-SRYYMD )    )
               EVALUATE    WRK-PTBYO-CHK-SYUBYOFLG
                   WHEN    "1"
                       IF      WRK-MOJISU  >   17
                           MOVE    "1"     TO  WRK-PTBYO-CHK-RECEDENFLG
                       END-IF
                   WHEN    SPACE
                       IF      WRK-MOJISU  >   20
                           MOVE    "1"     TO  WRK-PTBYO-CHK-RECEDENFLG
                       END-IF
               END-EVALUATE
           END-IF
           .
       5001-RECEDENFLG-HENSYU-EXT.
           EXIT
           .
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
           INITIALIZE                      C02
      *
           MOVE    SPA-GMN-C02-PTNUM       TO  C02-PTNUM
           MOVE    SPA-GMN-C02-SYSYMD      TO  C02-SYSYMD
           MOVE    SPA-GMN-C02-KANANAME    TO  C02-KANANAME
           MOVE    SPA-GMN-C02-NAME        TO  C02-NAME
           IF      SPA-NAI-C02-LOCK        =   ZERO
               MOVE    "black"                 TO  C02-NAME-STYLE
                                                   C02-KANANAME-STYLE
           ELSE
               MOVE    "red"                     TO  C02-NAME-STYLE
                                                   C02-KANANAME-STYLE
           END-IF
           MOVE    SPA-GMN-C02-SEX         TO  C02-SEX
           MOVE    SPA-GMN-C02-BIRTHDAY    TO  C02-BIRTHDAY
           MOVE    SPA-GMN-C02-AGE         TO  C02-AGE
           MOVE    SPA-GMN-C02-HKNCOMBIMEIH
                                           TO  C02-HKNNAME
           MOVE    SPA-GMN-C02-FTNRATE     TO  C02-PTFTNRATE
           MOVE    SPA-GMN-C02-LASTYMD     TO  C02-LASTYMD
           IF      SPA-NAI-C02-LOCK        =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G     TO  C02-SRYKA
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GMN-C02-SRYKA-MAX
                   MOVE    SPA-GMN-C02-SRYKALST (IDX)
                                               TO  C02-SRYKALST (IDX)
               END-PERFORM
               MOVE    SPA-GMN-C02-SRYKA-MAX   TO  C02-SRYKA-COUNT
               MOVE    SPA-GMN-C02-BSRYKA-G    TO  C02-BSRYKA
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GMN-C02-BSRYKA-MAX
                   MOVE    SPA-GMN-C02-BSRYKALST (IDX)
                                               TO  C02-BSRYKALST(IDX)
               END-PERFORM
               MOVE    SPA-GMN-C02-BSRYKA-MAX  TO  C02-BSRYKA-COUNT
           END-IF
           MOVE    SPA-GMN-C02-SYOKBN-G    TO  C02-SYOKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-SYOKBN-MAX
               MOVE    SPA-GMN-C02-SYOKBNLST (IDX)
                                               TO  C02-SYOKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-SYOKBN-MAX  TO  C02-SYOKBN-COUNT
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
           IF      SPA-NAI-C02-SELNO-INSERT
                                           >   ZERO
               MOVE    2                       TO  C02-BYO-ROWATTR
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
           IF      SPA-NAI-C02-RECEDENFLG     =   "1"
               MOVE    "red"                  TO  C02-BYOMEIHENFLG-STYLE
               IF       SPA-GMN-C02-BYOMEIHENFLG   =   SPACE
                   MOVE    "文字超過"         TO  C02-BYOMEIHENFLG-VALUE
               END-IF
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
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-TENKIKBN-MAX 
                   OR          SPA-GMN-C02-TENKIKBNL  (IDX)   =   SPACE
               MOVE    SPA-GMN-C02-TENKIKBNLST (IDX)
                                           TO  C02-TENKIKBN-ITEM (IDX)
               MOVE    IDX                 TO  C02-TENKIKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-UTAGAIFLG-G TO  C02-UTAGAIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-UTAGAIFLG-MAX
                   OR          SPA-GMN-C02-UTAGAIFLGL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-UTAGAIFLGLST (IDX)
                                           TO  C02-UTAGAIFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-UTAGAIFLG-COUNT    
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-SYUBYOFLG-G TO  C02-SYUBYOFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-SYUBYOFLG-MAX
                   OR          SPA-GMN-C02-SYUBYOFLGL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-SYUBYOFLGLST (IDX)
                                           TO  C02-SYUBYOFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-SYUBYOFLG-COUNT  
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-MANSEIKBN-G TO  C02-MANSEIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-MANSEIKBN-MAX
                   OR          SPA-GMN-C02-MANSEIKBNL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-MANSEIKBNLST (IDX)
                                           TO  C02-MANSEIFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-MANSEIFLG-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-NYUGAIKBN-G TO  C02-NYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-NYUGAIKBN-MAX
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
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >
                                           SPA-GMN-C02-HKNBYOMEIFLG-MAX
                   OR          SPA-GMN-C02-HKNBYOMEIFLGL(IDX) =   SPACE
               MOVE    SPA-GMN-C02-HKNBYOMEIFLGLST (IDX)
                                       TO  C02-HKNBYOMEIFLG-ITEM (IDX)
               MOVE    IDX             TO  C02-HKNBYOMEIFLG-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-REZEFLG-G   TO  C02-REZEFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-REZEFLG-MAX
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
                   MOVE    "診療科入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "該当の患者が存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "カルテ病名が長すぎます"    TO  SPA-ERRMSG
               WHEN    "0034"
                   STRING  "これ以上追加できません、"
                                               DELIMITED  BY  SIZE
                           "削除一覧を確認し削除処理を行なって下さい"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
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
               WHEN    "0039"
                   MOVE
                   "（警告）開始日が未来日で入力されています" 
                                               TO  SPA-ERRMSG
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
                   STRING
                       "変更処理または行指定での追加処理"
                                               DELIMITED   BY  SIZE 
                       "のときは複数病名の入力はできません"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
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
               WHEN    "0086"
                   MOVE    "患者番号の入力がありません" 
                                                   TO  SPA-ERRMSG
               WHEN    "0087"
                   MOVE
               "行指定での追加処理時は診療科、開始日の変更はできません"
                                               TO  SPA-ERRMSG
               WHEN    "0088"
                   MOVE
                   "指定された診療科の開始日にはこれ以上追加できません"
                                               TO  SPA-ERRMSG
               WHEN    "0089"
                   MOVE
                   "指定された診療科の開始日にはこれ以上追加できません"
                                               TO  SPA-ERRMSG
               WHEN    "0090"
                   MOVE    "簡単修正処理中です"    TO  SPA-ERRMSG   
               WHEN    "0096"
                   MOVE    "誕生日前の開始日は入力できません"
                                               TO  SPA-ERRMSG
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
               WHEN    "1006"
                   MOVE
                  "レセプト作成管理マスタの更新処理でエラーになりました"
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
           MOVE    SPACE           TO  WRK-CIDMSG
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
               WHEN    "0103"
                   STRING
                       "この病名は"            DELIMITED   BY  SIZE 
                       WRK-ERR-SRYKA           DELIMITED   BY  SPACE
                       "に"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。更新しますか？"
                                               DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING
                       "この病名は「急性」と「慢性」が混在して"
                                                DELIMITED   BY  SIZE 
                       "レセプトに記載されます。更新しますか？"
                                                DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-CID1-FLG
      *
           MOVE    SPACE               TO  CID1
           MOVE    SPA-CIDCD           TO  CID1-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID1-ID1MSG
           EVALUATE    SPA-CIDCD
               WHEN    "0103"
               WHEN    "0104"
                   MOVE    SPA-C02-KEIKOKU-BYOMEI  TO  CID1-BYOMEI
           END-EVALUATE 
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
      *    感染症送信処理
      *****************************************************************
       690-INFECTION-SEND-SEC          SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    INFECTION-FILE      TO  SHELLTBL-NAME
           MOVE    "C02"               TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG2
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG3
           MOVE    SPACE               TO  SHELLTBL-ARG4
           MOVE    SPA-NYUGAIKBN       TO  SHELLTBL-ARG5
           MOVE    SPA-PTID            TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG6
           MOVE    SPA-SRYYMD          TO  SHELLTBL-ARG9
           MOVE    SPA-SRYKA           TO  SHELLTBL-ARG8
           MOVE    SPA-BEDFLG          TO  SHELLTBL-ARG9
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       690-INFECTION-SEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込（主キー）
      *****************************************************************
       900-PTBYO-INV-SEC         SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE                  PTBYOMEI-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
grpsys     PERFORM 900-DBFETCH-SEC
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
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-INV-SEC                SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-COMB-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-BYOMEI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
grpsys     PERFORM 900-DBFETCH-SEC
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
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
grpsys     PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC   
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *     
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
