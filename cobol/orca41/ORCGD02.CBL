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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGD02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェックサブ
      *  コンポーネント名  : チェック条件によりデータ抽出
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01.07.13    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    ひさなが     01/11/08  実日数の確認追加
      *  01.00.02    ひさなが     02/01/25  疑い病名のチェックを修正
      *  01.00.03    ひさなが     02/03/04  初診日と病名関係の確認追加
      *  01.00.03    ひさなが     02/05/02  初診日と病名関係の確認修正
      *  01.00.04    ひさなが     02/05/07  半角チェックの追加
      *  01.00.05    ひさなが     02/06/03  労災チェックの追加
      *  01.00.06    MCC-森脇     02/06/14  併用禁忌チェックの修正
      *  01.00.07    ひさなが     02/07/03  老人保険チェックの修正
      *  01.00.08    ひさなが     02/08/22  保険番号チェックの修正
      *  01.00.08    ひさなが     02/08/27  適応病名チェックの修正
      *  01.00.09    ひさなが     02/09/06  ５２０ー０３メッセージ修正
      *  01.00.10    ひさなが     02/11/28  保険番号記号チェックの修正
      *  01.00.11    ひさなが     02/12/06  ５００ー０３メッセージ修正
      *  01.00.12    ひさなが     02/12/18  院外処方のチェック追加
      *  01.00.13    ひさなが     03/01/31  テスト患者を処理対象外に
      *                                     老人保健の条件修正
      *  01.00.14    ひさなが     03/02/05  老人保健の条件を診療年月の
      *                                     １日現在に修正
      *  01.00.15    ひさなが     03/02/07  初診日と病名関係の確認修正
      *  01.00.16    ひさなが     03/04/01  入外区分が入院の診療会計の
      *                                     実日数チェックの修正
      *  01.00.16    ひさなが     03/04/17  入外区分が入院の入院会計の
      *                                     実日数チェック
      *                                     外泊チェックの追加
      *  01.00.17    ひさなが     03/05/09  病名に対応する病名コードの
      *                                     未入力チェックの追加
      *  01.00.18    ひさなが     03/06/03  診療行為と病名チェック
      *                                     薬剤と病名チェックの修正
      *
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    チェク条件用ファイル
           SELECT  DID1-FILE       ASSIGN  DID1PARA
                                   FILE    STATUS  IS  STS-DID1.
      *    確認抽出用ファイル１
           SELECT  DID2-FILE       ASSIGN  DID2PARA
                                   FILE    STATUS  IS  STS-DID2.
      *    確認抽出用ファイル２
           SELECT  DID3-FILE       ASSIGN  DID3PARA
                                   FILE    STATUS  IS  STS-DID3.
      *    エラー用ファイル
           SELECT  DID4-FILE       ASSIGN  DID4PARA
                                   FILE    STATUS  IS  STS-DID4.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    チェク条件用ファイル
       FD  DID1-FILE.
       01  DID1-REC                PIC X(200). 
      *    確認抽出用ファイル１
       FD  DID2-FILE.
       01  DID2-REC                PIC X(200). 
      *    確認抽出用ファイル２
       FD  DID3-FILE.
       01  DID3-REC                PIC X(200). 
      *    エラー用ファイル
       FD  DID4-FILE.
       01  DID4-REC                PIC X(200).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    チェク条件用ファイル 名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID1//.
      *
      *    確認抽出用ファイル１ 名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID2//.
      *
      *    確認抽出用ファイル２ 名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID3//.
      *
      *    エラー用ファイル 名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID4//.
      *
      *
       01  SPA-D02.
           03  SPA-D02-AREA.
               05  SPA-GMN-AREA.
                   07  SPA-GMN-HOSPID         PIC X(24).
                   07  SPA-GMN-SRYYMD         PIC X(08).
                   07  SPA-GMN-NYUGAIKBN      PIC X(01).
                   07  SPA-GMN-NYUGAIKBN-MSG  PIC X(10).
                   07  SPA-GMN-TBLG2.
                       09  SPA-GNM-TBL3       OCCURS  20.
                           11  SPA-GMN-CNV    PIC X(01).
      *
      *    診療年月領域
       01  T-SINRYOU-AREA.
           03  T-SINRYOU-Y         PIC 9(04).
           03  T-SINRYOU-M         PIC 9(02).
      *
      *    入外区分領域
       01  T-NYUGAIKBN-AREA.
           03  T-NYUGAIKBN         PIC X(01).
      *
      *    判定、カウント領域
       01  T-HANTEI-CNT-AREA.
           03  T-HANNTEI1          PIC X(01).
           03  T-HANNTEI2          PIC X(01).
           03  T-HANNTEI3          PIC X(01).
           03  T-HANNTEI4          PIC X(01).
           03  T-HANNTEI5          PIC X(01).
           03  T-HANNTEI6          PIC X(02).
           03  T-HANNTEI7          PIC X(02).
           03  T-HANNTEI8          PIC X(02).
           03  T-ERR-01            PIC X(01).
           03  T-ERR-02            PIC X(01).
           03  T-ERR-03            PIC X(01).
           03  T-ERR-04            PIC X(01).
           03  T-ERR-05            PIC X(01).
           03  T-ERR-06            PIC X(01).
           03  T-ERR-07            PIC X(01).
           03  T-ERR-08            PIC X(01).
           03  T-ERR-09            PIC X(01).
           03  T-ERR-10            PIC X(01).
           03  T-ERR-11            PIC X(01).
           03  T-ERR-11-2          PIC X(01).
           03  T-ERR-12            PIC X(01).
           03  T-ERR-13            PIC X(01).
           03  T-ERR-13-2          PIC X(01).
           03  T-ERR-13-3          PIC X(01).
           03  T-ERR-14            PIC X(01).
           03  T-CNT-IN            PIC 9(09).
           03  T-CNT-OUT           PIC 9(09).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-TBL             PIC 9(01).
           03  FLG-TENKIYMD-ER     PIC 9(01).
           03  FLG-RED             PIC 9(01).
      *
      *    エンドステータス領域
       01  STS-AREA.
           03  STS-DID1            PIC X(02).
           03  STS-DID2            PIC X(02).
           03  STS-DID3            PIC X(02).
           03  STS-DID4            PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW1                PIC 9(03).
           03  IDW2                PIC 9(03).
           03  IDX1                PIC 9(03).
           03  IDX2                PIC 9(03).
           03  IDX3                PIC 9(03).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(03).
           03  IDX6                PIC 9(03).
           03  IDX7                PIC 9(02).
           03  IDX8                PIC 9(02).
           03  IDX9                PIC 9(03).
           03  IDY1                PIC 9(05).
           03  IDY2                PIC 9(03).
           03  IDY3                PIC 9(03).
           03  IDY4                PIC 9(03).
           03  IDY5                PIC 9(05).
      *
      *    生年月日、適用開始年月日、診療開始日領域
       01  W-YM1.
           03  W-YY1               PIC 9(04).
           03  W-MM1               PIC 9(02).
       01  W-YM2.
           03  W-YY2               PIC 9(04).
           03  W-MM2               PIC 9(02).
       01  W-YM3.
           03  W-YY3               PIC 9(04).
           03  W-MM3               PIC 9(02).
       01  W-YMD4.
           03  W-YY4               PIC 9(04).
           03  W-MM4               PIC 9(02).
           03  W-DD4               PIC 9(02).
       01  W-YMD5.
           03  W-YY5               PIC 9(04).
           03  W-MM5               PIC 9(02).
           03  W-DD5               PIC 9(02).
      *
      *    実日数計算作業
       01  W-JITUDAY.
           03  WK-JITUDAY1.
               05  W-JITU-DAY1         PIC X(01)  OCCURS   31.
           03  WK-JITUDAY2.
               04  W-JITU-SRYCD        PIC X(09)  OCCURS   20.
           03  W-JITU-DAY3             PIC 9(07).
           03  W-JITU-DAY4             PIC 9(07).
           03  W-JITU-DAY5             PIC 9(07).
      *
      *    診療会計作業
       01  W-SRYACCT.
           03  WK-SRYACCT.
               05  W-ACCT-DAY          PIC 9(03)  OCCURS   31.
      *
      *    編集作業
       01  W-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                  PIC X(06).
               05  WRK-PARA-TERMID                 PIC X(64).
               05  WRK-PARA-SYSYMD                 PIC X(08).
               05  WRK-PARA-OPID                   PIC X(16).
               05  WRK-PARA-SYOKBN                 PIC X(01).
               05  WRK-PARA-RECEKBN                PIC X(01).
      *
      *    保険組合せ作業
       01  W-HKNCOMBI.
           03  W-COMB-HANNTEI1     PIC X(03).
           03  W-COMB-HANNTEI2     PIC X(01).
           03  W-COMB-HKNNUM       PIC X(03).
      *
      *   診療行為作業
       01  W-SRYACT.
           03  W1-SRY-HOSPID       PIC X(24).
           03  W1-SRY-PTID         PIC 9(10).
           03  W1-SRY-SRYKA        PIC X(02).
           03  W-SRYACT-TABLE      OCCURS  500.
               05  W-SRY-HOSPID    PIC X(24).
               05  W-SRY-PTID      PIC 9(10).
               05  W-SRY-SRYKA     PIC X(02).
               05  W-SRY-SRYCD     PIC X(09).
      *
      *    診療コード退避作業
       01  W-SRYCD.
           03  WK1-HKNNUM-S        PIC X(03).
      *    （診療行為の併用算定）
           03  W-TABLE-S1.
               05  W-SRYCD-S1-TABLE    OCCURS  1000.
                   07  W-SRYCD-11      PIC X(09).
                   07  W-SRYCD-12      PIC X(09).
                   07  W-ZAINUM-11     PIC 9(08).
                   07  W-SRYCD-13      PIC X(09).
                   07  W-SRYCD-14      PIC X(09).
                   07  W-ZAINUM-12     PIC 9(08).
      *    （薬剤の併用禁忌）
           03  W-TABLE-S2.
               05  W-SRYCD-S2-TABLE    OCCURS  1000.
                   07  W-SRYCD-21      PIC X(09).
                   07  W-SRYCD-22      PIC X(09).
                   07  W-ZAINUM-21     PIC 9(08).
      *    （設定コード判定用）
           03  W-TABLE-S3.
               05  W-SRYCD-S3-KBN      PIC X(01).
               05  W-SRYCD-S3-TABLE    OCCURS  3000.
                   07  W-SRYCD-31      PIC X(09).
                   07  W-SRYCD-32      PIC X(01).
                   07  W-SRYCD-41      PIC X(09).
                   07  W-SRYCD-42      PIC X(01).
                   07  W-SRYCD-51      PIC X(09).
                   07  W-SRYCD-52      PIC X(01).
                   07  W-SRYCD-61      PIC X(09).
                   07  W-SRYCD-62      PIC X(01).
                   07  W-SRYCD-63      PIC X(09).
                   07  W-SRYCD-71      PIC X(09).
                   07  W-SRYCD-72      PIC X(01).
                   07  W-SRYCD-73      PIC X(09).
                   07  W-SRYCD-81      PIC X(09).
                   07  W-SRYCD-82      PIC X(01).
           03  W-TABLE-S4.
               05  W-SRYCD-S4-TABLE    OCCURS  1000.
                   07  W-SRYCD-91      PIC X(09).
                   07  W-SRYCD-92      PIC X(09).
           03  W-CHK-KUBUN.
               05  W-CHK-ST1-KBN       PIC X(01).
               05  W-CHK-ST2-KBN       PIC X(01).
               05  W-CHK-ST3-KBN       PIC X(01).
      *    入院会計の区分作業
           03  WK-ZAISKBKBN        PIC X(01).
           03  WK-NYUGAIKBN        PIC X(01).
           03  WK-SRYKA            PIC X(02).
           03  WK-ZAINUM           PIC 9(08).
           03  WK-SRYSYUKBN        PIC X(01).
      *    入院会計の日作業
       01  W-NYUINACCT.
           03  WK-NYUINACCT.
               05  W-NACCT-DAY     PIC 9(03)  OCCURS   31.
           03  WK-NYUINACCT-K.
               05  W-NACCT-K-DAY   PIC 9(03)  OCCURS   31.
           03  W-NACCT-DAY-B       PIC X(08).
           03  W-NACCT-DAY-D       PIC 9(03).
           03  W-NACCT-DAY-H       PIC 9(01).
           03  W-NACCT-DAY-K-ERR   PIC 9(01).
      *
      *    メッセージ領域
       01  T-MESSAGE-AREA.
           03  T-MESSAGE-000-01    PIC X(90)   VALUE
               "000-01診療年月の誤り                                    
      -        "                                  ".
           03  T-MESSAGE-000-02    PIC X(90)   VALUE
               "000-02入外区分の誤り                                    
      -        "                                  ".
           03  T-MESSAGE-100-01    PIC X(90)   VALUE
               "100-01診療会計テーブル異常                              
      -        "                                  ".
           03  T-MESSAGE-100-02    PIC X(90)   VALUE
               "100-02対象データ無し                                    
      -        "                                  ".
           03  T-MESSAGE-100-03    PIC X(90)   VALUE
               "100-03警告データ無し                                    
      -        "                                  ".
           03  T-MESSAGE-100-04    PIC X(90)   VALUE
               "100-04警告データ件数                                    
      -        "                                  ".
           03  T-MESSAGE-100-05    PIC X(90)   VALUE
               "100-05件                                                
      -        "                                  ".
           03  T-MESSAGE-200-01    PIC X(90)   VALUE
               "200-01保険組合せテーブル異常                            
      -        "                                  ".
           03  T-MESSAGE-200-02    PIC X(90)   VALUE
               "200-02診療行為で使用した保険組合せ番号が保険組合せ情報に
      -        "存在しない                        ".
           03  T-MESSAGE-300-01    PIC X(90)   VALUE
               "300-01患者保険情報テーブル異常                          
      -        "                                  ".
           03  T-MESSAGE-300-02    PIC X(90)   VALUE
               "300-02該当患者の保険者番号が患者保険情報に未登録です    
      -        "                                  ".
           03  T-MESSAGE-300-03    PIC X(90)   VALUE
               "300-03該当患者の保険者番号が未入力です                  
      -        "                                  ".
           03  T-MESSAGE-300-04    PIC X(90)   VALUE
               "300-04該当患者の保険情報に記号又は番号が未入力です      
      -        "                                  ".
           03  T-MESSAGE-300-05    PIC X(90)   VALUE
               "300-05該当患者の保険情報の記号又は番号に全角文字以外が入
      -        "力されています                    ".
           03  T-MESSAGE-400-01    PIC X(90)   VALUE
               "400-01患者公費情報テーブル異常１                        
      -        "                                  ".
           03  T-MESSAGE-400-02    PIC X(90)   VALUE
               "400-02患者公費情報テーブル異常２                        
      -        "                                  ".
           03  T-MESSAGE-400-03    PIC X(90)   VALUE
               "400-03患者公費情報テーブル異常３                        
      -        "                                  ".
           03  T-MESSAGE-400-04    PIC X(90)   VALUE
               "400-04患者公費情報テーブル異常４                        
      -        "                                  ".
           03  T-MESSAGE-400-05    PIC X(90)   VALUE
               "400-05診療行為で使用した保険組合せで指定されている公費が
      -        "患者公費情報に未登録です          ".
           03  T-MESSAGE-400-06    PIC X(90)   VALUE
               "400-06該当患者の公費情報に受給者番号又は負担者番号が未入
      -        "力です                            ".
           03  T-MESSAGE-500-01    PIC X(90)   VALUE
               "500-01患者情報テーブル異常                              
      -        "                                  ".
           03  T-MESSAGE-500-02    PIC X(90)   VALUE
               "500-02該当患者は患者情報に登録されていません            
      -        "                                  ".
           03  T-MESSAGE-500-03    PIC X(90)   VALUE
               "500-03該当患者は１５歳未満ですが本人で登録されています  
      -        "                                  ".
           03  T-MESSAGE-500-04    PIC X(90)   VALUE
               "500-04該当患者は老人保険の適応年齢誤りです              
      -        "                                  ".
           03  T-MESSAGE-500-05    PIC X(90)   VALUE
               "500-05該当患者は老人保険が未設定です                    
      -        "                                  ".
           03  T-MESSAGE-500-06    PIC X(90)   VALUE
               "500-06保険の適用が切れている期間があります              
      -        "                                  ".
           03  T-MESSAGE-510-01    PIC X(90)   VALUE
               "510-01患者病名情報テーブル異常                          
      -        "                                  ".
           03  T-MESSAGE-510-02    PIC X(90)   VALUE
               "510-02該当患者は病名が未入力です                        
      -        "                                  ".
           03  T-MESSAGE-510-03    PIC X(90)   VALUE
               "510-03該当患者は患者病名情報に病名が未入力です          
      -        "                                  ".
           03  T-MESSAGE-510-04    PIC X(90)   VALUE
               "510-04該当患者は患者病名情報に古い診療開始日の疑い病名が
      -        "入力されています                  ".
           03  T-MESSAGE-510-05    PIC X(90)   VALUE
               "510-05初診入力日に病名が登録されていません              
      -        "                                  ".
           03  T-MESSAGE-510-06    PIC X(90)   VALUE
               "510-06該当患者は病名に対応する病名コードが入力されていま
      -        "せん                              ".
           03  T-MESSAGE-520-01    PIC X(90)   VALUE
               "520-01診療科履歴情報テーブル異常                        
      -        "                                  ".
           03  T-MESSAGE-520-02    PIC X(90)   VALUE
               "520-02該当患者は診療科コードが診療科履歴情報に未登録です
      -        "                                  ".
           03  T-MESSAGE-520-03    PIC X(90)   VALUE
               "520-03病名の開始日に初診日より古い日付がセットされていま
      -        "す                                ".
           03  T-MESSAGE-530-01    PIC X(90)   VALUE
               "530-01チェック情報テーブル異常                          
      -        "                                  ".
           03  T-MESSAGE-530-02    PIC X(90)   VALUE
               "530-02診療行為情報テーブル異常                          
      -        "                                  ".
           03  T-MESSAGE-530-03    PIC X(90)   VALUE
               "530-03薬剤の適応病名未入力です                          
      -        "                                  ".
           03  T-MESSAGE-530-04    PIC X(90)   VALUE
               "530-04診療行為の適応病名未入力です                      
      -        "                                  ".
           03  T-MESSAGE-530-05    PIC X(90)   VALUE
               "530-05薬剤と禁忌の誤りです                              
      -        "                                  ".
           03  T-MESSAGE-530-06    PIC X(90)   VALUE
               "530-06禁忌の薬剤が入力されています                      
      -        "                                  ".
           03  T-MESSAGE-530-07    PIC X(90)   VALUE
               "530-07診療行為が併用算定の誤りです                      
      -        "                                  ".
           03  T-MESSAGE-530-08    PIC X(90)   VALUE
               "530-08相互作用情報テーブル異常                          
      -        "                                  ".
           03  T-MESSAGE-530-A     PIC X(06)   VALUE
               "転帰日".
           03  T-MESSAGE-540-01    PIC X(90)   VALUE
               "540-01点数情報テーブル異常                              
      -        "                                  ".
           03  T-MESSAGE-540-02    PIC X(90)   VALUE
               "540-02入力された診療行為コードが点数マスタに存在しません
      -        "                                  ".
           03  T-MESSAGE-540-03    PIC X(90)   VALUE
               "540-03該当患者は実日数を越えた回数を算定しています      
      -        "                                  ".
           03  T-MESSAGE-540-04    PIC X(90)   VALUE
               "540-04患者入院履歴情報テーブル異常                      
      -        "                                  ".
           03  T-MESSAGE-540-05    PIC X(90)   VALUE
               "540-05入院会計テーブル異常                              
      -        "                                  ".
           03  T-MESSAGE-540-06    PIC X(90)   VALUE
               "540-06入院行為テーブル異常                              
      -        "                                  ".
           03  T-MESSAGE-540-07    PIC X(90)   VALUE
               "540-07入院していない日に入院診療があります              
      -        "                                  ".
           03  T-MESSAGE-540-08    PIC X(90)   VALUE
               "540-08外泊のときに入院調剤料が発生しています            
      -        "                                  ".
           03  T-MESSAGE-600-01    PIC X(90)   VALUE
               "600-01該当患者は氏名に全角文字以外が入力されています    
      -        "                                  ".
           03  T-MESSAGE-600-02    PIC X(90)   VALUE
               "600-02該当患者は記号に全角文字以外が入力されています    
      -        "                                  ".
           03  T-MESSAGE-600-03    PIC X(90)   VALUE
               "600-03該当患者は番号に全角文字以外が入力されています    
      -        "                                  ".
           03  T-MESSAGE-600-04    PIC X(90)   VALUE
               "600-04該当患者は病名に全角文字以外が入力されています    
      -        "                                  ".
      *
      *    予約コード領域
       01  WRK-YOYAKU.
      *    初診（病院）
           03  W-YOYAKU1           PIC X(09)   VALUE   "111700110".
      *    初診（診療所）
           03  W-YOYAKU2           PIC X(09)   VALUE   "111700210".
      *    初診（病院）
           03  W-YOYAKU3           PIC X(09)   VALUE   "111000110".
      *    初診（診療所）
           03  W-YOYAKU4           PIC X(09)   VALUE   "111003610".
      *    小児科外来診療料（院外処方）初診時
           03  W-YOYAKU5           PIC X(09)   VALUE   "113003510".
      *    小児科外来診療料（院内処方）初診時
           03  W-YOYAKU6           PIC X(09)   VALUE   "113003710".
       01  WRK-HKNNUM.
      *    労災
           03  W-HKNNUM1           PIC X(03)   VALUE   "971".
      *    自費
           03  W-HKNNUM2           PIC X(03)   VALUE   "980".
       01  WRK-BIRTHDAY.
      *    老人
           03  W-BIRTHDAY1         PIC X(08)   VALUE   "19320929".
       01  WRK-PTKOHI.
      *    保険番号（公費）
           03  W-KOHNUM1           PIC X(03)   VALUE   "027".
       01  WRK-SRYSYUKBN.
      *    入院調剤料
           03  W-SRYSYUKBN1        PIC X(03)   VALUE   "240".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者病名情報
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    診療科履歴情報
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    チェク情報
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    診療行為情報
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    相互作用情報
       01  INTERACT-REC.
           COPY    "CPINTERACT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院行為マスタ−
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    連絡
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    テーブルアクセス
           COPY    "MCPAREA".
      *
      *    半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
        01  COMMAND-PARAM.
            02  FILLSE              PIC X(2000).
      *
      *
       PROCEDURE                   DIVISION    USING
                                               COMMAND-PARAM.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *    初期処理
           PERFORM T-000-CHEKINIT-SEC.
      *    診療年月 確認　データチェク処理
           IF  SPA-GMN-HOSPID    NOT =   SPACE
               PERFORM T-000-RECECHEK-SEC
           END-IF.
           IF  (SPA-GMN-HOSPID    NOT =   SPACE)  AND
               (T-NYUGAIKBN-AREA      =   "1")    AND
               ((SPA-GMN-CNV (12)      =   "1")    OR
                (SPA-GMN-CNV (14)      =   "1"))
               INITIALIZE                W-SRYACT
               INITIALIZE                WK-NYUINACCT
               MOVE    ZERO              TO  W-JITU-DAY4
               MOVE    ZERO              TO  W-JITU-DAY5
               MOVE    ALL SPACE         TO  WK-JITUDAY2
               PERFORM T-540-RECECHEK-CHK4-SEC
           END-IF.
      *    終了処理
           PERFORM T-000-CHEKTERM-SEC.
      *
       000-PROC-SEC-EXT.
           STOP    RUN.
      *
      *****************************************************************
      *    初　期　処　理
      *****************************************************************
       T-000-CHEKINIT-SEC                SECTION.
      *    ＤＢ　 オープン処理
           PERFORM 000-DBOPEN-SEC.
      *    外部ファイル名 設定処理
           PERFORM T-000-CHEKSETT-SEC.
      *
           OPEN    INPUT                   DID1-FILE.
           OPEN    OUTPUT                  DID2-FILE.
           OPEN    OUTPUT                  DID3-FILE.
           INITIALIZE                      SPA-D02.
           INITIALIZE                      W-HKNCOMBI.
           INITIALIZE                      W-SRYACT.
           INITIALIZE                      PTHKNINF-REC.
           INITIALIZE                      PTKOHINF-REC.
           INITIALIZE                      W-TABLE-S3.
           MOVE    ZERO                    TO  W-JITU-DAY3.
           MOVE    ZERO                    TO  W-JITU-DAY4.
           MOVE    ALL "*"                 TO  W1-SRY-HOSPID.
      *    確認条件 入力処理
           PERFORM T-000-CHEKCHEK-SEC VARYING IDX1  FROM  1  BY 1
                                      UNTIL   IDX1  >     4.
      *
           MOVE    SPACE                   TO  W-CHK-ST1-KBN.
           MOVE    SPACE                   TO  W-CHK-ST2-KBN.
           MOVE    SPACE                   TO  W-CHK-ST3-KBN.
      *
           INITIALIZE                CHK-REC.
           MOVE    "1"               TO  CHK-CHKKBN.
           MOVE    "1"               TO  CHK-CDKBN.
           MOVE    CHK-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"        TO  MCP-FUNC.
           MOVE    "CHK-KEY5"        TO  ORC-DBPATH.
           MOVE    T-SINRYOU-AREA    TO  ORC-DBYMD(1:6).
           MOVE    "01"              TO  ORC-DBYMD(7:2).
      *
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-000-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   MOVE    "*"                 TO  W-CHK-ST1-KBN
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "CHK-KEY5"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           INITIALIZE                CHK-REC.
           MOVE    "2"               TO  CHK-CHKKBN.
           MOVE    "1"               TO  CHK-CDKBN.
           MOVE    CHK-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"        TO  MCP-FUNC.
           MOVE    "CHK-KEY5"        TO  ORC-DBPATH.
           MOVE    T-SINRYOU-AREA    TO  ORC-DBYMD(1:6).
           MOVE    "01"              TO  ORC-DBYMD(7:2).
      *
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-000-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   MOVE    "*"                 TO  W-CHK-ST2-KBN
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "CHK-KEY5"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           INITIALIZE                CHK-REC.
           MOVE    "5"               TO  CHK-CHKKBN.
           MOVE    "2"               TO  CHK-CDKBN.
           MOVE    CHK-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"        TO  MCP-FUNC.
           MOVE    "CHK-KEY5"        TO  ORC-DBPATH.
           MOVE    T-SINRYOU-AREA    TO  ORC-DBYMD(1:6).
           MOVE    "01"              TO  ORC-DBYMD(7:2).
      *
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-000-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   MOVE    "*"                 TO  W-CHK-ST3-KBN
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "CHK-KEY5"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-000-CHEKINITK-EXT.
           EXIT.
      *
      *******************
      *
       T-000-RECECHEK-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "CHK-KEY5"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-000-RECECHEK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終　了　処　理
      *****************************************************************
       T-000-CHEKTERM-SEC                SECTION.
      *
           CLOSE                           DID1-FILE.
           CLOSE                           DID2-FILE.
           CLOSE                           DID3-FILE.
      *    エラー出力処理
           PERFORM T-900-ERR-HENSYU-SEC.
      *    ＤＢ　 クローズ処理
           PERFORM 000-DBCLOSE-SEC.
      *
       T-000-CHEKTERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　 オープン処理
      *****************************************************************
       000-DBOPEN-SEC                 SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
       000-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　 クローズ処理
      *****************************************************************
       000-DBCLOSE-SEC                SECTION.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
       000-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    外部ファイル名 設定処理
      *****************************************************************
       T-000-CHEKSETT-SEC                SECTION.
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA.
      *
           MOVE    "ORCGD1"            TO  DID1PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID1PARA-TERMID.
      *
           MOVE    "ORCGD2"            TO  DID2PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID2PARA-TERMID.
      *
           MOVE    "ORCGD3"            TO  DID3PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID3PARA-TERMID.
      *
           MOVE    "ORCGD4"            TO  DID4PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID4PARA-TERMID.
      *
       T-000-CHEKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認条件 入力処理
      *****************************************************************
       T-000-CHEKCHEK-SEC                SECTION.
      *
           READ    DID1-FILE
                   AT   END
                        MOVE    5                      TO  IDX1
                   NOT  AT  END
                        EVALUATE    IDX1
                          WHEN    01
                              MOVE    DID1-REC(1:24)
                                      TO  SPA-GMN-HOSPID
                          WHEN    02
                              MOVE    DID1-REC(7:6)
                                      TO  SPA-GMN-SRYYMD(1:6)
                                          T-SINRYOU-AREA
                          WHEN    03
                              MOVE    DID1-REC(1:1)
                                      TO  SPA-GMN-NYUGAIKBN
                                          T-NYUGAIKBN-AREA
                              MOVE    DID1-REC(2:10)
                                      TO  SPA-GMN-NYUGAIKBN-MSG
                          WHEN    04
                              MOVE    DID1-REC(1:20)
                                      TO  SPA-GMN-TBLG2
                        END-EVALUATE
           END-READ.
      *
       T-000-CHEKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月 確認処理
      *****************************************************************
       T-000-RECECHEK-SEC                SECTION.
      *
           MOVE    SPACE                   TO    T-HANNTEI1.
           MOVE    SPACE                   TO    T-HANNTEI2.
           MOVE    ZERO                    TO    T-CNT-IN.
           MOVE    ZERO                    TO    T-CNT-OUT.
      *
           IF  T-SINRYOU-Y    NOT  NUMERIC
               MOVE    "*"                   TO    T-HANNTEI1
           ELSE
               IF  T-SINRYOU-Y    <  2000
                   MOVE    "*"                   TO    T-HANNTEI1
               END-IF
           END-IF.
      *
           IF  T-SINRYOU-M    NOT  NUMERIC
               MOVE    "*"                   TO    T-HANNTEI1
           ELSE
               IF  T-SINRYOU-M    <  1    OR   >  12
                   MOVE    "*"                   TO    T-HANNTEI1
               END-IF
           END-IF.
      *
           IF  T-NYUGAIKBN    NOT  =  "1"  AND  "2"
               MOVE    "*"                   TO    T-HANNTEI2
           END-IF.
      *
           MOVE    SPACE              TO    DID2-REC.
           MOVE    SPA-GMN-TBLG2      TO    DID2-REC(1:20).
           WRITE   DID2-REC.
      *
           IF  (T-HANNTEI1   NOT  =  "*")  AND
               (T-HANNTEI2   NOT  =  "*")
               PERFORM T-100-RECECHEK-SEC
           ELSE
               IF  T-HANNTEI1   =  "*"
                   MOVE    SPACE                   TO    DID2-REC
                   MOVE    T-SINRYOU-AREA(1:4)     TO    DID2-REC(1:4)
                   MOVE    "/"                     TO    DID2-REC(5:1)
                   MOVE    T-SINRYOU-AREA(5:2)     TO    DID2-REC(6:2)
                   MOVE    T-MESSAGE-000-01(7:83)  TO    DID2-REC(8:83)
                   WRITE   DID2-REC
               END-IF
               IF  T-HANNTEI2   =  "*"
                   MOVE    SPACE                   TO    DID2-REC
                   MOVE    T-SINRYOU-AREA(1:4)     TO    DID2-REC(1:4)
                   MOVE    "/"                     TO    DID2-REC(5:1)
                   MOVE    T-SINRYOU-AREA(5:2)     TO    DID2-REC(6:2)
                   MOVE    "-"                     TO    DID2-REC(8:1)
                   MOVE    T-NYUGAIKBN(1:1)        TO    DID2-REC(9:1)
                   MOVE    T-MESSAGE-000-02(7:83)  TO    DID2-REC(10:83)
                   WRITE   DID2-REC
               END-IF
           END-IF.
      *
       T-000-RECECHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブル 参照処理
      *****************************************************************
       T-100-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  SRYACCT-REC.
           MOVE    SPA-GMN-HOSPID      TO  ACCT-HOSPID.
           MOVE    T-NYUGAIKBN-AREA    TO  ACCT-NYUGAIKBN.
           MOVE    T-SINRYOU-AREA      TO  ACCT-SRYYM.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SRYACCT-KEY6"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                     TO    DID2-REC
               MOVE    T-MESSAGE-100-01          TO    DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                     TO    DID3-REC
               MOVE    T-MESSAGE-100-01(1:60)    TO    DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-100-RECECHEK-READ-SEC
               PERFORM UNTIL      MCP-RC   NOT  =  ZERO
                   MOVE    MCPDATA-REC               TO    SRYACCT-REC
                   COMPUTE T-CNT-IN    =   T-CNT-IN    +   1
                   IF  ACCT-ZAIKAISU   NOT =  ZERO
                       PERFORM T-100-RECECHEK-HANTI-SEC
                   END-IF
                   PERFORM T-100-RECECHEK-READ-SEC
               END-PERFORM
               IF  (SPA-GMN-CNV (12)   =  "1")          AND
                   (T-ERR-12           =  SPACE)        AND
                   (W-JITU-DAY4    NOT =  ZERO)         AND
                   (W-JITU-DAY4        <  W-JITU-DAY5)
                   PERFORM T-540-RECECHEK-CHK2-SEC
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "SRYACCT-KEY6"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  T-CNT-IN      =  ZERO
               MOVE    SPACE                  TO  DID2-REC
               MOVE    T-SINRYOU-AREA(1:4)    TO  DID2-REC(1:4)
               MOVE    "/"                    TO  DID2-REC(5:1)
               MOVE    T-SINRYOU-AREA(5:2)    TO  DID2-REC(6:2)
               MOVE    "-"                    TO  DID2-REC(8:1)
               MOVE    T-NYUGAIKBN(1:1)       TO  DID2-REC(9:1)
               MOVE    T-MESSAGE-100-02(7:83) TO  DID2-REC(10:83)
               WRITE   DID2-REC
           ELSE
               IF  T-CNT-OUT     =  ZERO
                   MOVE    SPACE                  TO  DID2-REC
                   MOVE    T-SINRYOU-AREA(1:4)    TO  DID2-REC(1:4)
                   MOVE    "/"                    TO  DID2-REC(5:1)
                   MOVE    T-SINRYOU-AREA(5:2)    TO  DID2-REC(6:2)
                   MOVE    "-"                    TO  DID2-REC(8:1)
                   MOVE    T-NYUGAIKBN(1:1)       TO  DID2-REC(9:1)
                   MOVE    T-MESSAGE-100-03(7:83) TO  DID2-REC(10:83)
                   WRITE   DID2-REC
               ELSE
                   MOVE    SPACE                  TO  DID2-REC
                   MOVE    T-SINRYOU-AREA(1:4)    TO  DID2-REC(1:4)
                   MOVE    "/"                    TO  DID2-REC(5:1)
                   MOVE    T-SINRYOU-AREA(5:2)    TO  DID2-REC(6:2)
                   MOVE    "-"                    TO  DID2-REC(8:1)
                   MOVE    T-NYUGAIKBN(1:1)       TO  DID2-REC(9:1)
                   MOVE    T-MESSAGE-100-04(7:14) TO  DID2-REC(10:14)
                   MOVE    T-CNT-OUT              TO  DID2-REC(24:9)
                   MOVE    T-MESSAGE-100-05(7:2)  TO  DID2-REC(33:2)
                   WRITE   DID2-REC
               END-IF
           END-IF.
      *
       T-100-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-100-RECECHEK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "SRYACCT-KEY6"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-100-RECECHEK-READ-EXT.
           EXIT.
      *
      *******************
      *
       T-100-RECECHEK-HANTI-SEC          SECTION.
      *
           IF  (ACCT-HOSPID   NOT =   W1-SRY-HOSPID)  OR
               (ACCT-PTID     NOT =   W1-SRY-PTID)
               IF  (W1-SRY-HOSPID   NOT =  ALL "*")     AND
                   ((T-ERR-04            =  SPACE)     OR
                    (T-ERR-13            =  SPACE))
                   PERFORM T-500-RECECHEK2-SEC
               END-IF
               PERFORM T-500-RECECHEK-SEC
               IF  (SPA-GMN-CNV (12)   =  "1")          AND
                   (T-ERR-12           =  SPACE)        AND
                   (W-JITU-DAY4    NOT =  ZERO)         AND
                   (W-JITU-DAY4        <  W-JITU-DAY5)
                   PERFORM T-540-RECECHEK-CHK2-SEC
               END-IF
               INITIALIZE                WK-JITUDAY1
               INITIALIZE                WK-JITUDAY2
               INITIALIZE                W-TABLE-S1
               INITIALIZE                W-TABLE-S2
               INITIALIZE                W-TABLE-S4
               INITIALIZE                WK-NYUINACCT-K
               MOVE    ALL ZERO          TO  W-NACCT-DAY-K-ERR
               MOVE    ZERO              TO  W-JITU-DAY4
               MOVE    ZERO              TO  W-JITU-DAY5
               MOVE    SPACE             TO  T-HANNTEI6
               MOVE    SPACE             TO  T-HANNTEI7
               MOVE    SPACE             TO  T-HANNTEI8
               MOVE    "---"             TO  W-COMB-HANNTEI1
               MOVE    SPACE             TO  W-COMB-HANNTEI2
               MOVE    SPACE             TO  T-ERR-01
               MOVE    SPACE             TO  T-ERR-02
               MOVE    SPACE             TO  T-ERR-03
               MOVE    SPACE             TO  T-ERR-04
               MOVE    SPACE             TO  T-ERR-05
               MOVE    SPACE             TO  T-ERR-06
               MOVE    SPACE             TO  T-ERR-07
               MOVE    SPACE             TO  T-ERR-08
               MOVE    SPACE             TO  T-ERR-09
               MOVE    SPACE             TO  T-ERR-10
               MOVE    SPACE             TO  T-ERR-11
               MOVE    SPACE             TO  T-ERR-11-2
               MOVE    SPACE             TO  T-ERR-12
               MOVE    SPACE             TO  T-ERR-13
               MOVE    SPACE             TO  T-ERR-13-2
               MOVE    SPACE             TO  T-ERR-14
               IF  (T-NYUGAIKBN-AREA       =  "1")   AND
                   (SPA-GMN-CNV (12)       =  "1")   AND
                   (PTINF-TSTPTNUMKBN  NOT =  "1")
                   PERFORM T-540-RECECHEK-CHK3-SEC
                   PERFORM T-540-RECECHEK-CHK7-SEC
               END-IF
           END-IF.
           IF  PTINF-TSTPTNUMKBN  NOT =  "1"
               IF  (ACCT-HOSPID     NOT =   COMB-HOSPID)  OR
                   (ACCT-PTID       NOT =   COMB-PTID)  OR
                   (ACCT-HKNCOMBI   NOT =   COMB-HKNCOMBINUM)
                   PERFORM T-200-RECECHEK-SEC
               END-IF
               IF  T-HANNTEI5   =   "*"
                   PERFORM T-200-RECECHEK-CHK1-SEC
               END-IF
               IF  COMB-HKNNUM   =   W-HKNNUM1
                   MOVE    W-HKNNUM1         TO  WK1-HKNNUM-S
               ELSE
                   MOVE    SPACE             TO  WK1-HKNNUM-S
               END-IF
               MOVE    SPACE             TO  WK-SRYSYUKBN
      *
               PERFORM T-530-RECECHEK-CHK1-SEC
               IF  (ACCT-HOSPID   NOT =   W1-SRY-HOSPID)  OR
                   (ACCT-PTID     NOT =   W1-SRY-PTID)    OR
                   (ACCT-SRYKA    NOT =   W1-SRY-SRYKA)
                   MOVE    SPACE             TO  T-ERR-05
                   MOVE    SPACE             TO  T-ERR-06
                   MOVE    SPACE             TO  T-ERR-07
                   MOVE    SPACE             TO  T-ERR-13-3
                   PERFORM T-510-RECECHEK-SEC
               END-IF
      *
               IF  (SPA-GMN-CNV (5)   =   "1")    AND
                   (T-ERR-05          =  SPACE)   AND
                   (T-HANNTEI6        =   "CH")   AND
                   ((T-HANNTEI7        =  "ER")    OR
                    (T-HANNTEI8        =  "ER"))
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE             TO  DID2-REC
                   MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                   MOVE    "-"               TO  DID2-REC(25:1)
                   MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                   MOVE    T-MESSAGE-510-05  TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"               TO  T-ERR-05
               END-IF
      *
               IF  (SPA-GMN-CNV (12)    =  "1")    AND
                   (W-NACCT-DAY-K-ERR   =   ZERO)  AND
                   (T-NYUGAIKBN-AREA    =   "1")
                   PERFORM VARYING  IDY3  FROM  1  BY  1
                           UNTIL    IDY3  >    31
                       IF  (ACCT-DAY (1,IDY3)    NOT =  ZERO)  AND
                           (W-NACCT-K-DAY (IDY3)     =  ZERO)
                           COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                           MOVE    SPACE             TO  DID2-REC
                           MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                           MOVE    "-"               TO  DID2-REC(25:1)
                           MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                           MOVE    T-MESSAGE-540-07  TO  DID2-REC(71:90)
                           WRITE   DID2-REC
                           MOVE    1               TO  W-NACCT-DAY-K-ERR
                           MOVE    40              TO  IDY3
                       END-IF
      *
                       IF  (WK-SRYSYUKBN             =  "1")   AND
                           (ACCT-DAY (1,IDY3)    NOT =  ZERO)  AND
                           (W-NACCT-K-DAY (IDY3)     =  2)
                           COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                           MOVE    SPACE             TO  DID2-REC
                           MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                           MOVE    "-"               TO  DID2-REC(25:1)
                           MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                           MOVE    T-MESSAGE-540-08  TO  DID2-REC(71:90)
                           WRITE   DID2-REC
                           MOVE    1               TO  W-NACCT-DAY-K-ERR
                           MOVE    40              TO  IDY3
                       END-IF
                   END-PERFORM
               END-IF
           END-IF.
      *
           MOVE    ACCT-HOSPID  TO  W1-SRY-HOSPID.
           MOVE    ACCT-PTID    TO  W1-SRY-PTID.
           MOVE    ACCT-SRYKA   TO  W1-SRY-SRYKA.
           MOVE    COMB-HKNNUM  TO  W-COMB-HKNNUM.
           MOVE    "ED"         TO  T-HANNTEI6.
      *
       T-100-RECECHEK-HANTI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せテーブル 参照処理
      *****************************************************************
       T-200-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC.
           MOVE    SPACE               TO  T-HANNTEI5.
           MOVE    ACCT-HOSPID         TO  COMB-HOSPID.
           MOVE    ACCT-PTID           TO  COMB-PTID.
           MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                  TO  DID2-REC
               MOVE    T-MESSAGE-200-01       TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                  TO  DID3-REC
               MOVE    T-MESSAGE-200-01(1:60) TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-200-RECECHEK-READ-SEC
               IF  MCP-RC    NOT  =   ZERO
                   COMPUTE T-CNT-OUT   =   T-CNT-OUT   +   1 
                   MOVE    SPACE                  TO  DID2-REC
                   MOVE    ACCT-HOSPID            TO  DID2-REC(1:24)
                   MOVE    "-"                    TO  DID2-REC(25:1)
                   MOVE    ACCT-PTID              TO  DID2-REC(26:10)
                   MOVE    "-"                    TO  DID2-REC(36:1)
                   MOVE    ACCT-HKNCOMBI          TO  DID2-REC(37:4)
                   MOVE    T-MESSAGE-200-02       TO  DID2-REC(71:90)
                   WRITE   DID2-REC
               ELSE
                   MOVE    MCPDATA-REC            TO  HKNCOMBI-REC
                   MOVE    "*"                    TO  T-HANNTEI5
                   IF  (SPA-GMN-CNV (14)   =  "1")    AND
                       (T-ERR-14           =  SPACE)
                       PERFORM T-200-RECECHEK-CHK2-SEC
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-200-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-200-RECECHEK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-200-RECECHEK-READ-EXT.
           EXIT.
      *
      *******************
      *
       T-200-RECECHEK-CHK1-SEC           SECTION.
      *
           IF  ((T-ERR-01          =   SPACE)         OR
                (T-ERR-02          =   SPACE)         OR
                (T-ERR-13-2        =   SPACE))       AND
               (((ACCT-PTID     NOT =   PTHKN-PTID)  AND
                 (COMB-HKNID    NOT =   ZERO))        OR
                ((ACCT-PTID         =   PTHKN-PTID)  AND
                 (COMB-HKNID    NOT =   PTHKN-HKNID) AND
                 (COMB-HKNID    NOT =   ZERO)))      AND
               (COMB-HKNNUM   NOT =   W-HKNNUM1  AND  W-HKNNUM2)
               PERFORM T-300-RECECHEK-SEC
           END-IF.
           IF  ((ACCT-PTID      NOT =   PTKOH-PTID)  AND
                (COMB-KOH1ID    NOT =   ZERO))        OR
               ((ACCT-PTID          =   PTKOH-PTID)  AND
                (COMB-KOH1ID    NOT =   PTKOH-KOHID) AND
                (COMB-KOH1ID    NOT =   ZERO))
               PERFORM T-401-RECECHEK-SEC
           END-IF.
           IF  ((ACCT-PTID      NOT =   PTKOH-PTID)  AND
                (COMB-KOH2ID    NOT =   ZERO))        OR
               ((ACCT-PTID          =   PTKOH-PTID)  AND
                (COMB-KOH2ID    NOT =   PTKOH-KOHID) AND
                (COMB-KOH2ID    NOT =   ZERO))
               PERFORM T-402-RECECHEK-SEC
           END-IF.
           IF  ((ACCT-PTID      NOT =   PTKOH-PTID)  AND
                (COMB-KOH3ID    NOT =   ZERO))        OR
               ((ACCT-PTID          =   PTKOH-PTID)  AND
                (COMB-KOH3ID    NOT =   PTKOH-KOHID) AND
                (COMB-KOH3ID    NOT =   ZERO))
               PERFORM T-403-RECECHEK-SEC
           END-IF.
           IF  ((ACCT-PTID      NOT =   PTKOH-PTID)  AND
                (COMB-KOH4ID    NOT =   ZERO))        OR
               ((ACCT-PTID          =   PTKOH-PTID)  AND
                (COMB-KOH4ID    NOT =   PTKOH-KOHID) AND
                (COMB-KOH4ID    NOT =   ZERO))
               PERFORM T-404-RECECHEK-SEC
           END-IF.
      *
       T-200-RECECHEK-CHK1-EXT.
           EXIT.
      *
      *******************
      *
       T-200-RECECHEK-CHK2-SEC           SECTION.
      *
           PERFORM VARYING    IDX6    FROM    1   BY  1
                   UNTIL      IDX6    >      31
               IF  ACCT-DAY (1,IDX6)   NOT =    ZERO
                   MOVE    ACCT-SRYYM         TO  W-NACCT-DAY-B(1:6)
                   MOVE    IDX6(2:2)          TO  W-NACCT-DAY-B(7:2)
                   IF  (COMB-TEKSTYMD  >  W-NACCT-DAY-B)  OR
                       (COMB-TEKEDYMD  <  W-NACCT-DAY-B)
                       COMPUTE T-CNT-OUT   =   T-CNT-OUT   +   1 
                       MOVE    SPACE              TO  DID2-REC
                       MOVE    ACCT-HOSPID        TO  DID2-REC(1:24)
                       MOVE    "-"                TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID          TO  DID2-REC(26:10)
                       MOVE    "-"                TO  DID2-REC(36:1)
                       MOVE    ACCT-HKNCOMBI      TO  DID2-REC(37:4)
                       MOVE    T-MESSAGE-500-06   TO  DID2-REC(71:90)
                       MOVE    "診療"             TO  DID2-REC(115:4)
                       WRITE   DID2-REC
                       MOVE    "*"                TO  T-ERR-14
                       MOVE    40                 TO  IDX6
                   END-IF
               END-IF
           END-PERFORM.
      *
       T-200-RECECHEK-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報テーブル 参照処理
      *****************************************************************
       T-300-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  PTHKNINF-REC.
           MOVE    ACCT-HOSPID         TO  PTHKN-HOSPID.
           MOVE    ACCT-PTID           TO  PTHKN-PTID.
           MOVE    COMB-HKNID          TO  PTHKN-HKNID.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    SPACE                  TO  DID2-REC
               MOVE    T-MESSAGE-300-01       TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                  TO  DID3-REC
               MOVE    T-MESSAGE-300-01(1:60) TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-300-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   IF  (T-ERR-01     =   SPACE)  AND
                       (T-ERR-02     =   SPACE)  AND
                       (T-ERR-13-2   =   SPACE)
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE              TO  DID2-REC
                       MOVE    ACCT-HOSPID        TO  DID2-REC(1:24)
                       MOVE    "-"                TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID          TO  DID2-REC(26:10)
                       MOVE    "-"                TO  DID2-REC(36:1)
                       MOVE    COMB-HKNID         TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-300-02   TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"                TO  T-ERR-01
                       MOVE    "*"                TO  T-ERR-02
                       MOVE    "*"                TO  T-ERR-13-2
                   END-IF
               ELSE
                   MOVE    MCPDATA-REC            TO  PTHKNINF-REC
                   MOVE    PTHKN-HONKZKKBN        TO  W-COMB-HANNTEI2
                   IF  (T-ERR-01          =   SPACE)  AND
                       (PTHKN-HKNJANUM    =   SPACE)  AND
                       (SPA-GMN-CNV (1)   =   "1")
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    COMB-HKNID        TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-300-03  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-01
                   END-IF
                   IF  (T-ERR-02           =   SPACE)  AND
                       ((PTHKN-KIGO        =   SPACE)   OR
                        (PTHKN-NUM         =   SPACE))  AND
                       (SPA-GMN-CNV (2)   =   "1")
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    COMB-HKNID        TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-300-04  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-02
                   END-IF
                   IF  (T-ERR-02           =   SPACE)  AND
                       (PTHKN-KIGO     NOT =   SPACE)  AND
                       (SPA-GMN-CNV (2)    =   "1")
                       PERFORM VARYING     IDY4     FROM    1   BY  1
                               UNTIL       IDY4             >   80
                           IF  ((PTHKN-KIGO(IDY4:1) NOT = X"0A") AND
                                (PTHKN-KIGO(IDY4:1) NOT = X"00") AND
                                (PTHKN-KIGO(IDY4:1)     < X"A1"))  OR
                               ((PTHKN-NUM(IDY4:1) NOT = X"0A") AND
                                (PTHKN-NUM(IDY4:1) NOT = X"00") AND
                                (PTHKN-NUM(IDY4:1)     < X"A1"))
                               COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                               MOVE    SPACE         TO  DID2-REC
                               MOVE    ACCT-HOSPID   TO  DID2-REC(1:24)
                               MOVE    "-"           TO  DID2-REC(25:1)
                               MOVE    ACCT-PTID     TO  DID2-REC(26:10)
                               MOVE    "-"           TO  DID2-REC(36:1)
                               MOVE    COMB-HKNID    TO  DID2-REC(37:10)
                               MOVE    T-MESSAGE-300-05
                                                     TO  DID2-REC(71:90)
                               WRITE   DID2-REC
                               MOVE    "*"           TO  T-ERR-02
                               MOVE    99            TO  IDY4
                           END-IF
                       END-PERFORM
                   END-IF
                   IF  (T-ERR-13-2         =   SPACE)  AND
                       (SPA-GMN-CNV (13)   =   "1")
                       INITIALIZE                ORCSKANACHKAREA
                       MOVE    "1"               TO  KANACHK-SYORI
                       MOVE    PTHKN-KIGO        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"     USING
                                                 ORCSKANACHKAREA
                       IF  KANACHK-RC   NOT =   ZERO
                           COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                           MOVE    SPACE             TO  DID2-REC
                           MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                           MOVE    "-"               TO  DID2-REC(25:1)
                           MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                           MOVE    "-"               TO  DID2-REC(36:1)
                           MOVE    COMB-HKNID        TO  DID2-REC(37:10)
                           MOVE    T-MESSAGE-600-02  TO  DID2-REC(71:90)
                           WRITE   DID2-REC
                           MOVE    "*"               TO  T-ERR-13-2
                       END-IF
                       INITIALIZE                ORCSKANACHKAREA
                       MOVE    "1"               TO  KANACHK-SYORI
                       MOVE    PTHKN-NUM         TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"     USING
                                                 ORCSKANACHKAREA
                       IF  KANACHK-RC   NOT =   ZERO
                           COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                           MOVE    SPACE             TO  DID2-REC
                           MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                           MOVE    "-"               TO  DID2-REC(25:1)
                           MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                           MOVE    "-"               TO  DID2-REC(36:1)
                           MOVE    COMB-HKNID        TO  DID2-REC(37:10)
                           MOVE    T-MESSAGE-600-03  TO  DID2-REC(71:90)
                           WRITE   DID2-REC
                           MOVE    "*"               TO  T-ERR-13-2
                       END-IF
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-300-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-300-RECECHEK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-300-RECECHEK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報テーブル 参照処理
      *****************************************************************
       T-401-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  PTKOHINF-REC.
           MOVE    ACCT-HOSPID         TO  PTKOH-HOSPID.
           MOVE    ACCT-PTID           TO  PTKOH-PTID.
           MOVE    COMB-KOH1ID         TO  PTKOH-KOHID.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-400-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-400-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-400-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   IF  T-ERR-03   =   SPACE
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    COMB-KOH1ID       TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-400-05  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-03
                   END-IF
               ELSE
                   MOVE    MCPDATA-REC           TO  PTKOHINF-REC
                   IF  (T-ERR-03          =   SPACE)   AND
                       ((PTKOH-JKYSNUM    =   SPACE)   OR
                        (PTKOH-FTNJANUM   =   SPACE))  AND
                       (SPA-GMN-CNV (3)   =   "1")
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    PTKOH-KOHNUM      TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-400-06  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-03
                   END-IF
                   IF  PTKOH-KOHNUM   =   W-KOHNUM1
                       MOVE    PTKOH-KOHNUM      TO  W-COMB-HANNTEI1
                   ELSE
                       MOVE    SPACE             TO  W-COMB-HANNTEI1
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-401-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-402-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  PTKOHINF-REC.
           MOVE    ACCT-HOSPID         TO  PTKOH-HOSPID.
           MOVE    ACCT-PTID           TO  PTKOH-PTID.
           MOVE    COMB-KOH2ID         TO  PTKOH-KOHID.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-400-02        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-400-02(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-400-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   IF  T-ERR-03   =   SPACE
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    COMB-KOH2ID       TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-400-05  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-03
                   END-IF
               ELSE
                   MOVE    MCPDATA-REC           TO  PTKOHINF-REC
                   IF  (T-ERR-03          =   SPACE)   AND
                       ((PTKOH-JKYSNUM    =   SPACE)   OR
                        (PTKOH-FTNJANUM   =   SPACE))  AND
                       (SPA-GMN-CNV (3)   =   "1")
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    PTKOH-KOHNUM      TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-400-06  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-03
                   END-IF
                   IF  PTKOH-KOHNUM   =   W-KOHNUM1
                       MOVE    PTKOH-KOHNUM      TO  W-COMB-HANNTEI1
                   ELSE
                       MOVE    SPACE             TO  W-COMB-HANNTEI1
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-402-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-403-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  PTKOHINF-REC.
           MOVE    ACCT-HOSPID         TO  PTKOH-HOSPID.
           MOVE    ACCT-PTID           TO  PTKOH-PTID.
           MOVE    COMB-KOH3ID         TO  PTKOH-KOHID.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-400-03        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-400-03(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-400-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   IF  T-ERR-03   =   SPACE
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    COMB-KOH3ID       TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-400-05  TO  DID2-REC(71:90) 
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-03
                   END-IF
               ELSE
                   MOVE    MCPDATA-REC           TO  PTKOHINF-REC
                   IF  (T-ERR-03          =   SPACE)   AND
                       ((PTKOH-JKYSNUM    =   SPACE)   OR
                        (PTKOH-FTNJANUM   =   SPACE))  AND
                       (SPA-GMN-CNV (3)   =   "1")
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    PTKOH-KOHNUM      TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-400-06  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-03
                   END-IF
                   IF  PTKOH-KOHNUM   =   W-KOHNUM1
                       MOVE    PTKOH-KOHNUM      TO  W-COMB-HANNTEI1
                   ELSE
                       MOVE    SPACE             TO  W-COMB-HANNTEI1
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-403-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-404-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  PTKOHINF-REC.
           MOVE    ACCT-HOSPID         TO  PTKOH-HOSPID.
           MOVE    ACCT-PTID           TO  PTKOH-PTID.
           MOVE    COMB-KOH4ID         TO  PTKOH-KOHID.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-400-04        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-400-04(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-400-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   IF  T-ERR-03   =   SPACE
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    COMB-KOH4ID       TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-400-05  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-03
                   END-IF
               ELSE
                   MOVE    MCPDATA-REC           TO  PTKOHINF-REC
                   IF  (T-ERR-03          =   SPACE)   AND
                       ((PTKOH-JKYSNUM    =   SPACE)   OR
                        (PTKOH-FTNJANUM   =   SPACE))  AND
                       (SPA-GMN-CNV (3)   =   "1")
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                       MOVE    SPACE             TO  DID2-REC
                       MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                       MOVE    "-"               TO  DID2-REC(36:1)
                       MOVE    PTKOH-KOHNUM      TO  DID2-REC(37:10)
                       MOVE    T-MESSAGE-400-06  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-03
                   END-IF
                   IF  PTKOH-KOHNUM   =   W-KOHNUM1
                       MOVE    PTKOH-KOHNUM      TO  W-COMB-HANNTEI1
                   ELSE
                       MOVE    SPACE             TO  W-COMB-HANNTEI1
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-404-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-400-RECECHEK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.    
      *
       T-400-RECECHEK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報テーブル 参照処理
      *****************************************************************
      *
       T-500-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  PTINF-REC.
           MOVE    ACCT-HOSPID         TO  PTINF-HOSPID.
           MOVE    ACCT-PTID           TO  PTINF-PTID.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-500-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-500-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-500-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE                   TO  DID2-REC
                   MOVE    ACCT-HOSPID             TO  DID2-REC(1:24)
                   MOVE    "-"                     TO  DID2-REC(25:1)
                   MOVE    ACCT-PTID               TO  DID2-REC(26:10)
                   MOVE    T-MESSAGE-500-02        TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"                     TO  T-ERR-04
               ELSE
                   MOVE    MCPDATA-REC             TO  PTINF-REC
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-500-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-500-RECECHEK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.    
      *
       T-500-RECECHEK-READ-EXT.
           EXIT.
      *
      *******************
      *
       T-500-RECECHEK2-SEC               SECTION.
      *
           IF  (T-ERR-04              =   SPACE)  AND
               (PTINF-TSTPTNUMKBN NOT =   "1")    AND
               (SPA-GMN-CNV (4)       =   "1")
               MOVE    PTINF-BIRTHDAY(1:6)     TO  W-YM1
               MOVE    PTINF-BIRTHDAY(1:6)     TO  W-YM2
               MOVE    PTINF-BIRTHDAY(1:6)     TO  W-YM3
               COMPUTE W-YY1 = W-YY1 + 15
               COMPUTE W-YY2 = W-YY2 + 65
               IF   PTINF-BIRTHDAY   <=   W-BIRTHDAY1
                    COMPUTE W-YY3 = W-YY3 + 70
               ELSE
                    COMPUTE W-YY3 = W-YY3 + 75
               END-IF
               IF  (W-COMB-HANNTEI2   =   "1")    AND
                   (T-SINRYOU-AREA    <   W-YM1)
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE             TO  DID2-REC
                   MOVE    W1-SRY-HOSPID     TO  DID2-REC(1:24)
                   MOVE    "-"               TO  DID2-REC(25:1)
                   MOVE    W1-SRY-PTID       TO  DID2-REC(26:10)
                   MOVE    T-MESSAGE-500-03  TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"               TO  T-ERR-04
               END-IF
               IF  (W-COMB-HANNTEI1   =   W-KOHNUM1)  AND
                   (T-SINRYOU-AREA    <   W-YM2)
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE             TO  DID2-REC
                   MOVE    W1-SRY-HOSPID     TO  DID2-REC(1:24)
                   MOVE    "-"               TO  DID2-REC(25:1)
                   MOVE    W1-SRY-PTID       TO  DID2-REC(26:10)
                   MOVE    T-MESSAGE-500-04  TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"               TO  T-ERR-04
               END-IF
               IF  (W-COMB-HANNTEI1      =   "---")  AND
                   (T-SINRYOU-AREA       >   W-YM3)
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE             TO  DID2-REC
                   MOVE    W1-SRY-HOSPID     TO  DID2-REC(1:24)
                   MOVE    "-"               TO  DID2-REC(25:1)
                   MOVE    W1-SRY-PTID       TO  DID2-REC(26:10)
                   MOVE    T-MESSAGE-500-05  TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"               TO  T-ERR-04
               END-IF
               IF  (W-COMB-HANNTEI1      =   "---")  AND
                   (T-SINRYOU-AREA       =   W-YM3)  AND
                   (PTINF-BIRTHDAY(7:2)  =   "01")
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE             TO  DID2-REC
                   MOVE    W1-SRY-HOSPID     TO  DID2-REC(1:24)
                   MOVE    "-"               TO  DID2-REC(25:1)
                   MOVE    W1-SRY-PTID       TO  DID2-REC(26:10)
                   MOVE    T-MESSAGE-500-05  TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"               TO  T-ERR-04
               END-IF
           END-IF.
           IF  (T-ERR-13               =   SPACE)  AND
               (PTINF-TSTPTNUMKBN  NOT =   "1")    AND
               (SPA-GMN-CNV (13)       =   "1")
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    PTINF-KANANAME  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING
                                       ORCSKANACHKAREA
               IF  KANACHK-RC           NOT =   ZERO
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE             TO  DID1-REC
                   MOVE    W1-SRY-HOSPID     TO  DID2-REC(1:24)
                   MOVE    "-"               TO  DID2-REC(25:1)
                   MOVE    W1-SRY-PTID       TO  DID2-REC(26:10)
                   MOVE    T-MESSAGE-600-01  TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"               TO  T-ERR-13
               ELSE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI
                   MOVE    PTINF-NAME      TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING
                                           ORCSKANACHKAREA
                   IF  KANACHK-RC   NOT =   ZERO
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE             TO  DID1-REC
                       MOVE    W1-SRY-HOSPID     TO  DID2-REC(1:24)
                       MOVE    "-"               TO  DID2-REC(25:1)
                       MOVE    W1-SRY-PTID       TO  DID2-REC(26:10)
                       MOVE    T-MESSAGE-600-01  TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"               TO  T-ERR-13
                   END-IF
               END-IF
           END-IF.
      *
       T-500-RECECHEK2-EXT.
           EXIT.
      *
      *******************
      *
       T-500-RECECHEK3-SEC               SECTION.
      *
           INITIALIZE                  PTINF-REC.
           MOVE    NACCT-HOSPID        TO  PTINF-HOSPID.
           MOVE    NACCT-PTID          TO  PTINF-PTID.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-500-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-500-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-500-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE                   TO  DID2-REC
                   MOVE    NACCT-HOSPID            TO  DID2-REC(1:24)
                   MOVE    "-"                     TO  DID2-REC(25:1)
                   MOVE    NACCT-PTID              TO  DID2-REC(26:10)
                   MOVE    T-MESSAGE-500-02        TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"                     TO  T-ERR-04
               ELSE
                   MOVE    MCPDATA-REC             TO  PTINF-REC
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-500-RECECHEK3-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名情報テーブル 参照処理
      *****************************************************************
       T-510-RECECHEK-SEC                SECTION.
      *
           MOVE    ACCT-SRYYM          TO  W-YMD4(1:6).
           MOVE    00                  TO  W-YMD4(7:2).
           IF  (W-MM4   =   1) OR
               (W-MM4   =   2)
               COMPUTE  W-YY4 = W-YY4 - 1
               IF  W-MM4   =   1  
                   MOVE    11              TO  W-MM4
               ELSE
                   MOVE    12              TO  W-MM4
               END-IF
           ELSE
               COMPUTE  W-MM4 = W-MM4 - 2
           END-IF.
      *
           INITIALIZE                  PTBYOMEI-REC.
           MOVE    ACCT-HOSPID         TO  PTBYO-HOSPID.
           MOVE    ACCT-PTID           TO  PTBYO-PTID.
           MOVE    ACCT-SRYKA          TO  PTBYO-SRYKA.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-510-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-510-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-510-RECECHEK-READ-SEC
               IF  MCP-RC   NOT =   ZERO
                   IF  ((SPA-GMN-CNV (5)   =  "1")    OR
                        (SPA-GMN-CNV (7)   =  "1"))  AND
                       (T-ERR-05          = SPACE)   AND
                       (T-ERR-07          = SPACE)
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE                 TO  DID2-REC
                       MOVE    ACCT-HOSPID           TO  DID2-REC(1:24)
                       MOVE    "-"                   TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID             TO  DID2-REC(26:10)
                       MOVE    T-MESSAGE-510-02      TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"                   TO  T-ERR-05
                       MOVE    "*"                   TO  T-ERR-07
                       MOVE    "ED"                  TO  T-HANNTEI6
                   END-IF
               ELSE
                   PERFORM UNTIL      MCP-RC   NOT  =  ZERO
                       MOVE    MCPDATA-REC           TO  PTBYOMEI-REC
                       MOVE    "*"                   TO  T-HANNTEI3
                       IF  PTBYO-SRYYMD(1:6)      > ACCT-SRYYM
                           MOVE    SPACE             TO  T-HANNTEI3
                       END-IF
                       IF  (PTBYO-SRYYMD(1:6) NOT > ACCT-SRYYM)  AND
                           (PTBYO-TENKIKBN        = SPACE)
                           MOVE    SPACE             TO  T-HANNTEI3
                       END-IF
                       IF  (PTBYO-TENKIYMD    NOT = SPACE)  AND
                           (PTBYO-TENKIYMD    NOT = ZERO)
                           MOVE    SPACE             TO  T-HANNTEI3
                       END-IF
                       IF  (SPA-GMN-CNV (5)   = "1")    AND
                           (T-ERR-05          = SPACE)  AND
                           ((T-HANNTEI3      = "*")     OR
                            (PTBYO-BYOMEI    = SPACE))
                           COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                           MOVE SPACE                TO  DID2-REC
                           MOVE ACCT-HOSPID          TO  DID2-REC(1:24)
                           MOVE "-"                  TO  DID2-REC(25:1)
                           MOVE ACCT-PTID            TO  DID2-REC(26:10)
                           MOVE T-MESSAGE-510-03     TO  DID2-REC(71:90)
                           WRITE   DID2-REC
                           MOVE    "*"               TO  T-ERR-05
                           MOVE    "ED"              TO  T-HANNTEI6
                       END-IF
      *修正/*開始*03.05.09*********************************************
                       IF  (SPA-GMN-CNV (5)    = "1")    AND
                           (T-ERR-05           = SPACE)  AND
                           (PTBYO-BYOMEI  NOT  = SPACE)  AND
                           (PTBYO-BYOMEICD (1) = SPACE)  AND
                           (PTBYO-BYOMEICD (2) = SPACE)  AND
                           (PTBYO-BYOMEICD (3) = SPACE)  AND
                           (PTBYO-BYOMEICD (4) = SPACE)  AND
                           (PTBYO-BYOMEICD (5) = SPACE)  AND
                           (PTBYO-BYOMEICD (6) = SPACE)
                           COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                           MOVE SPACE                TO  DID2-REC
                           MOVE ACCT-HOSPID          TO  DID2-REC(1:24)
                           MOVE "-"                  TO  DID2-REC(25:1)
                           MOVE ACCT-PTID            TO  DID2-REC(26:10)
                           MOVE T-MESSAGE-510-06     TO  DID2-REC(71:90)
                           WRITE   DID2-REC
                           MOVE    "*"               TO  T-ERR-05
                           MOVE    "ED"              TO  T-HANNTEI6
                       END-IF
      *修正/*終了*03.05.09*********************************************
                       IF  (SPA-GMN-CNV (7)   = "1")          AND
                           (PTBYO-TENKIKBN    = SPACE)        AND
                           (PTBYO-UTAGAIFLG   = "1")          AND
                           (PTBYO-SRYYMD(1:6) < W-YMD4(1:6))  AND
                           (T-ERR-07          = SPACE)
                           COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                           MOVE    SPACE             TO  DID2-REC
                           MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                           MOVE    "-"               TO  DID2-REC(25:1)
                           MOVE    PTBYO-PTID        TO  DID2-REC(26:10)
                           MOVE    T-MESSAGE-510-04  TO  DID2-REC(71:90)
                           WRITE   DID2-REC
                           MOVE    "*"               TO  T-ERR-07
                       END-IF
                       IF  (SPA-GMN-CNV (13)   =   "1")  AND
                           (T-ERR-13-3         =   SPACE)
                           INITIALIZE              ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI
                           MOVE    PTBYO-BYOMEI    TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                    ORCSKANACHKAREA
                           IF  KANACHK-RC   NOT =   ZERO
                               COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                               MOVE    SPACE       TO  DID1-REC
                               MOVE    ACCT-HOSPID TO  DID2-REC(1:24)
                               MOVE    "-"         TO  DID2-REC(25:1)
                               MOVE    ACCT-PTID   TO  DID2-REC(26:10)
                               MOVE    T-MESSAGE-600-04
                                                   TO  DID2-REC(71:90)
                               WRITE   DID2-REC
                               MOVE    "*"         TO  T-ERR-13-3
                           END-IF
                       END-IF
                       IF  (SPA-GMN-CNV (13)   =   "1")  AND
                           (T-ERR-13-3         =   SPACE)
                           INITIALIZE          ORCSKANACHKAREA
                           MOVE    "1"         TO  KANACHK-SYORI
                           MOVE    PTBYO-CHARTBYOMEI
                                               TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF  KANACHK-RC   NOT =   ZERO
                               COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                               MOVE    SPACE         TO  DID1-REC
                               MOVE    ACCT-HOSPID   TO  DID2-REC(1:24)
                               MOVE    "-"           TO  DID2-REC(25:1)
                               MOVE    ACCT-PTID     TO  DID2-REC(26:10)
                               MOVE    T-MESSAGE-600-04
                                                     TO  DID2-REC(71:90)
                               WRITE   DID2-REC
                               MOVE    "*"       TO  T-ERR-13-3
                           END-IF
                       END-IF
                       IF  (T-HANNTEI6           =  "CH")       AND
                           (PTBYO-BYOMEI     NOT = SPACE)       AND
                           (PTBYO-SRYYMD(1:6)    = ACCT-SRYYM)  AND
                           (T-ERR-05             = SPACE)
                           MOVE    SPACE              TO  T-HANNTEI8
                           MOVE    PTBYO-SRYYMD(7:2)  TO  IDX7
                           IF  W-ACCT-DAY (IDX7) =  ZERO
                               MOVE    "ER"           TO  T-HANNTEI7
                           ELSE
                               MOVE    "ED"           TO  T-HANNTEI6
                           END-IF
                       END-IF
                       IF  (ACCT-SRYKBN            = "11")      AND
                           (ACCT-DAY-TBL (1)   NOT = ALL ZERO)  AND
                           (SPA-GMN-CNV (6)        = "1")       AND
                           (T-ERR-06               = SPACE)
                           MOVE    ZERO                TO  W-YMD5
                           PERFORM VARYING IDX2  FROM  1  BY 1
                                   UNTIL   IDX2  >    31
                               IF  ACCT-DAY (1,IDX2)  >  ZERO
                                   MOVE    ACCT-SRYYM    TO  W-YMD5(1:6)
                                   MOVE    IDX2          TO  W-DD5
                                   MOVE    99            TO  IDX2
                               END-IF
                           END-PERFORM
                           IF  PTBYO-SRYYMD   >   W-YMD5
                               PERFORM T-520-RECECHEK-SEC
                           END-IF
                       END-IF
                       PERFORM T-510-RECECHEK-READ-SEC
                   END-PERFORM
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-510-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-510-RECECHEK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-510-RECECHEK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報テーブル 参照処理
      *****************************************************************
       T-520-RECECHEK-SEC                SECTION.
      *
           INITIALIZE                  SRYKARRK-REC.
           MOVE    ACCT-HOSPID         TO  KARRK-HOSPID.
           MOVE    ACCT-PTID           TO  KARRK-PTID.
           MOVE    PTBYO-SRYKA         TO  KARRK-SRYKA.
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-520-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-520-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-520-RECECHEK-READ-SEC
               IF  MCP-RC    NOT  =   ZERO
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE                 TO  DID2-REC
                   MOVE    ACCT-HOSPID           TO  DID2-REC(1:24)
                   MOVE    "-"                   TO  DID2-REC(25:1)
                   MOVE    ACCT-PTID             TO  DID2-REC(26:10)
                   MOVE    "-"                   TO  DID2-REC(36:1)
                   MOVE    PTBYO-SRYKA           TO  DID2-REC(37:2)
                   MOVE    T-MESSAGE-520-02      TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"                   TO  T-ERR-06
               ELSE
                   MOVE    MCPDATA-REC           TO  SRYKARRK-REC
                   IF  PTBYO-SRYYMD(1:6)   <   KARRK-SYOSINYMD1(1:6)
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                       MOVE    SPACE                 TO  DID2-REC
                       MOVE    ACCT-HOSPID           TO  DID2-REC(1:24)
                       MOVE    "-"                   TO  DID2-REC(25:1)
                       MOVE    ACCT-PTID             TO  DID2-REC(26:10)
                       MOVE    "-"                   TO  DID2-REC(36:1)
                       MOVE    PTBYO-SRYKA           TO  DID2-REC(37:2)
                       MOVE    T-MESSAGE-520-03      TO  DID2-REC(71:90)
                       WRITE   DID2-REC
                       MOVE    "*"                   TO  T-ERR-06
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-520-RECECHEK-EXT.
           EXIT.
      *
      *******************
      *
       T-520-RECECHEK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-520-RECECHEK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報テーブル 参照処理
      *****************************************************************
       T-530-RECECHEK-CHK1-SEC           SECTION.
      *
           INITIALIZE                  SRYACT-REC.
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID.
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN.
           MOVE    ACCT-PTID           TO  SRY-PTID.
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM.
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA.
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-02        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-02(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               MOVE    ZERO                    TO  FLG-END
               PERFORM T-530-RECECHEK-READ1-SEC
               PERFORM UNTIL   (MCP-RC    NOT =   ZERO)  OR
                               (FLG-END   NOT =   ZERO)
                   MOVE    MCPDATA-REC          TO  SRYACT-REC
                   IF  (SPA-GMN-CNV (12)   =  "1")    AND
                       (T-ERR-12           =  SPACE)  AND
                       (T-NYUGAIKBN-AREA   =  "2")    AND
                       (ACCT-SRYKBN    NOT <  "10")   AND
                       (ACCT-SRYKBN    NOT >  "80")   AND
                       ((SRY-SRYKBN         =  "11")   OR
                        (SRY-SRYKBN         =  "12"))
                       MOVE    ZERO                 TO  W-JITU-DAY4
                       PERFORM VARYING  IDX6  FROM  1  BY  1
                               UNTIL    IDX6  >    31
                           IF  (ACCT-DAY (1,IDX6)  NOT =  ZERO)  OR
                               (ACCT-DAY (2,IDX6)  NOT =  ZERO)  OR
                               (ACCT-DAY (3,IDX6)  NOT =  ZERO)  OR
                               (ACCT-DAY (4,IDX6)  NOT =  ZERO)
                               MOVE    "1"        TO  W-JITU-DAY1 (IDX6)
                           END-IF
                           IF  W-JITU-DAY1 (IDX6)   =    "1"
                               COMPUTE W-JITU-DAY4 = W-JITU-DAY4 + 1
                           END-IF
                       END-PERFORM
                   END-IF
                   PERFORM VARYING IDX3  FROM  1  BY 1
                           UNTIL   IDX3  >     5
                       IF  (SPA-GMN-CNV (12)   =  "1")    AND
                           (T-ERR-12           =  SPACE)  AND
                           (ACCT-SRYKBN    NOT <  "10")   AND
                           (ACCT-SRYKBN    NOT >  "80")   AND
                           (SRY-SRYKBN     NOT =  "11")   AND
                           (SRY-SRYKBN     NOT =  "12")   AND
                           (SRY-SRYCD (IDX3)(1:1)   =   "1") AND
                           (W-JITU-DAY5    NOT >  ACCT-ZAIKAISU)
                           PERFORM VARYING IDY1  FROM  1  BY 1
                                   UNTIL   IDY1  >  3000
                               IF  W-SRYCD-81 (IDY1)   =   SPACE
                                   MOVE    IDY1            TO  IDY5
                                   MOVE    4000            TO  IDY1
                                   PERFORM T-540-RECECHEK-CHK-SEC
                               ELSE
                                   IF  W-SRYCD-81 (IDY1)   =
                                                        SRY-SRYCD (IDX3)
                                       MOVE    W-SRYCD-82 (IDY1)
                                                      TO  W-SRYCD-S3-KBN
                                       MOVE    4000   TO  IDY1
                                   END-IF
                               END-IF
                           END-PERFORM
                           IF  (T-ERR-12          =  SPACE)  AND
                               (W-SRYCD-S3-KBN    =   "*")
                               PERFORM T-540-RECECHEK-COMP-SEC
                           END-IF
                       END-IF
                       IF  (SRY-SRYCD (IDX3)   NOT =   ZERO)   AND
                           (SRY-SRYCD (IDX3)   NOT =   SPACE)
                           IF  SPA-GMN-CNV (11)   =   "1"
                               MOVE    SPACE         TO  W-SRYCD-S3-KBN
                               MOVE    ZERO          TO  IDY5
                               PERFORM VARYING IDY1  FROM  1  BY 1
                                       UNTIL   IDY1  >  3000
                                   IF  W-SRYCD-31 (IDY1)   =   SPACE
                                       MOVE    IDY1            TO  IDY5
                                       MOVE    4000            TO  IDY1
                                   ELSE
                                       IF  W-SRYCD-31 (IDY1)   =
                                                        SRY-SRYCD (IDX3)
                                           MOVE    W-SRYCD-32 (IDY1)
                                                      TO  W-SRYCD-S3-KBN
                                           MOVE    4000   TO  IDY1
                                       END-IF
                                   END-IF
                               END-PERFORM
                               IF  (T-ERR-11            =  SPACE)    AND
                                   (W-SRYCD-S3-KBN  NOT =  "*")      AND
                                   (W-CHK-ST3-KBN   NOT =  "*")      AND
                                   (WK1-HKNNUM-S        =  W-HKNNUM1)
                                   PERFORM T-530-RECECHEK-CHK3-1-SEC
                               END-IF
                               MOVE    SPACE         TO  W-SRYCD-S3-KBN
                               MOVE    ZERO          TO  IDY5
                               PERFORM VARYING IDY1  FROM  1  BY 1
                                       UNTIL   IDY1  >  3000
                                   IF  W-SRYCD-41 (IDY1)   =   SPACE
                                       MOVE    IDY1            TO  IDY5
                                       MOVE    4000            TO  IDY1
                                   ELSE
                                       IF  W-SRYCD-41 (IDY1)   =
                                                        SRY-SRYCD (IDX3)
                                           MOVE    W-SRYCD-42 (IDY1)
                                                      TO  W-SRYCD-S3-KBN
                                           MOVE    4000   TO  IDY1
                                       END-IF
                                   END-IF
                               END-PERFORM
                               IF  (T-ERR-11-2          =  SPACE)    AND
                                   (W-SRYCD-S3-KBN  NOT =  "*")      AND
                                   (W-CHK-ST3-KBN   NOT =  "*")      AND
                                   (WK1-HKNNUM-S    NOT =  W-HKNNUM1)
                                   PERFORM T-530-RECECHEK-CHK3-2-SEC
                               END-IF
                           END-IF
                           MOVE    SPACE             TO  W-SRYCD-S3-KBN
                           MOVE    ZERO              TO  IDY5
                           PERFORM VARYING IDY1  FROM  1  BY 1
                                   UNTIL   IDY1  >  3000
                               IF  W-SRYCD-51 (IDY1)   =   SPACE
                                   MOVE    IDY1               TO  IDY5
                                   MOVE    4000               TO  IDY1
                               ELSE
                                   IF  W-SRYCD-51 (IDY1)   =
                                                        SRY-SRYCD (IDX3)
                                       MOVE    W-SRYCD-52 (IDY1)
                                                      TO  W-SRYCD-S3-KBN
                                       MOVE    4000   TO  IDY1
                                   END-IF
                               END-IF
                           END-PERFORM
                           IF  (SPA-GMN-CNV (10)      =  "1")    AND
                               (W-SRYCD-S3-KBN    NOT =  "*")    AND
                               (T-ERR-10              =  SPACE)
                               PERFORM T-530-RECECHEK-CHK4-1-SEC
                           END-IF
                           MOVE    SPACE             TO  W-SRYCD-S3-KBN
                           MOVE    ZERO              TO  IDY5
                           PERFORM VARYING IDY1  FROM  1  BY 1
                                   UNTIL   IDY1  >  3000
                               IF  W-SRYCD-61 (IDY1)   =   SPACE
                                   MOVE    IDY1               TO  IDY5
                                   MOVE    4000               TO  IDY1
                               ELSE
                                   IF  W-SRYCD-61 (IDY1)   =
                                                        SRY-SRYCD (IDX3)
                                       MOVE    W-SRYCD-62 (IDY1)
                                                      TO  W-SRYCD-S3-KBN
                                       MOVE    IDY1   TO  IDY5
                                       MOVE    4000   TO  IDY1
                                   END-IF
                               END-IF
                           END-PERFORM
                           IF  (SPA-GMN-CNV (9)      =  "1")    AND
                               (W-SRYCD-S3-KBN   NOT =  "*")    AND
                               (W-CHK-ST2-KBN    NOT =  "*")    AND
                               (T-ERR-09             =  SPACE)
                               PERFORM T-530-RECECHEK-CHK6-SEC
                           END-IF
                           MOVE    SPACE             TO  W-SRYCD-S3-KBN
                           MOVE    ZERO              TO  IDY5
                           PERFORM VARYING IDY1  FROM  1  BY 1
                                   UNTIL   IDY1  >  3000
                               IF  W-SRYCD-71 (IDY1)   =   SPACE
                                   MOVE    IDY1               TO  IDY5
                                   MOVE    4000               TO  IDY1
                               ELSE
                                   IF  W-SRYCD-71 (IDY1)   =
                                                        SRY-SRYCD (IDX3)
                                       MOVE    W-SRYCD-72 (IDY1)
                                                      TO  W-SRYCD-S3-KBN
                                       MOVE    IDY1   TO  IDY5
                                       MOVE    4000   TO  IDY1
                                   END-IF
                               END-IF
                           END-PERFORM
                           IF  (SPA-GMN-CNV (8)      =  "1")    AND
                               (W-SRYCD-S3-KBN   NOT =  "*")    AND
                               (W-CHK-ST1-KBN    NOT =  "*")    AND
                               (T-ERR-08             =  SPACE)
                               PERFORM T-530-RECECHEK-CHK7-SEC
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   MOVE    1                       TO  FLG-END
                   IF  (SPA-GMN-CNV (11)   =   "1")    AND
                       ((T-ERR-11          =   SPACE)  AND
                        (T-ERR-11-2        =   SPACE))
                       MOVE    ZERO                    TO  FLG-END
                   END-IF
                   IF  (SPA-GMN-CNV (10)   =   "1")  AND
                       (T-ERR-10           =   SPACE)
                       MOVE    ZERO                    TO  FLG-END
                   END-IF
                   IF  (SPA-GMN-CNV (9)    =   "1")  AND
                       (T-ERR-09           =   SPACE)
                       MOVE    ZERO                    TO  FLG-END
                   END-IF
                   IF  (SPA-GMN-CNV (8)    =   "1")  AND
                       (T-ERR-08           =   SPACE)
                       MOVE    ZERO                    TO  FLG-END
                   END-IF
                   IF  (SPA-GMN-CNV (12)   =   "1")  AND
                       (T-ERR-12           =   SPACE)
                       MOVE    ZERO                    TO  FLG-END
                   END-IF
                   IF  (T-HANNTEI6      =   SPACE)  AND
                       ((SRY-SRYCD (1)   =   W-YOYAKU1)   OR
                        (SRY-SRYCD (2)   =   W-YOYAKU1)   OR
                        (SRY-SRYCD (3)   =   W-YOYAKU1)   OR
                        (SRY-SRYCD (4)   =   W-YOYAKU1)   OR
                        (SRY-SRYCD (5)   =   W-YOYAKU1)   OR
                        (SRY-SRYCD (1)   =   W-YOYAKU2)   OR
                        (SRY-SRYCD (2)   =   W-YOYAKU2)   OR
                        (SRY-SRYCD (3)   =   W-YOYAKU2)   OR
                        (SRY-SRYCD (4)   =   W-YOYAKU2)   OR
                        (SRY-SRYCD (5)   =   W-YOYAKU2)   OR
                        (SRY-SRYCD (1)   =   W-YOYAKU3)   OR
                        (SRY-SRYCD (2)   =   W-YOYAKU3)   OR
                        (SRY-SRYCD (3)   =   W-YOYAKU3)   OR
                        (SRY-SRYCD (4)   =   W-YOYAKU3)   OR
                        (SRY-SRYCD (5)   =   W-YOYAKU3)   OR
                        (SRY-SRYCD (1)   =   W-YOYAKU4)   OR
                        (SRY-SRYCD (2)   =   W-YOYAKU4)   OR
                        (SRY-SRYCD (3)   =   W-YOYAKU4)   OR
                        (SRY-SRYCD (4)   =   W-YOYAKU4)   OR
                        (SRY-SRYCD (5)   =   W-YOYAKU4)   OR
                        (SRY-SRYCD (1)   =   W-YOYAKU5)   OR
                        (SRY-SRYCD (2)   =   W-YOYAKU5)   OR
                        (SRY-SRYCD (3)   =   W-YOYAKU5)   OR
                        (SRY-SRYCD (4)   =   W-YOYAKU5)   OR
                        (SRY-SRYCD (5)   =   W-YOYAKU5)   OR
                        (SRY-SRYCD (1)   =   W-YOYAKU6)   OR
                        (SRY-SRYCD (2)   =   W-YOYAKU6)   OR
                        (SRY-SRYCD (3)   =   W-YOYAKU6)   OR
                        (SRY-SRYCD (4)   =   W-YOYAKU6)   OR
                        (SRY-SRYCD (5)   =   W-YOYAKU6))
                       MOVE    "CH"                    TO  T-HANNTEI6
                       MOVE    "ER"                    TO  T-HANNTEI8
                       MOVE    ACCT-DAY-TBL2 (1)       TO  WK-SRYACCT
                   END-IF
                   IF  T-HANNTEI6   =   SPACE
                       MOVE    ZERO                    TO  FLG-END
                   END-IF
                   IF  SRY-SRYSYUKBN       =   W-SRYSYUKBN1
                       MOVE    "1"                     TO  WK-SRYSYUKBN
                   END-IF
                   PERFORM T-530-RECECHEK-READ1-SEC
               END-PERFORM
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-CHK1-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-READ1-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-READ1-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェク情報テーブル 参照処理
      *****************************************************************
       T-530-RECECHEK-CHK2-1-SEC         SECTION.
      *
           INITIALIZE                CHK-REC.
           MOVE    "5"               TO  CHK-CHKKBN.
           MOVE    "2"               TO  CHK-CDKBN.
           MOVE    SRY-SRYCD (IDX3)  TO  CHK-SRYCD.
           MOVE    CHK-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"        TO  MCP-FUNC.
           MOVE    "CHK-KEY3"        TO  ORC-DBPATH.
           MOVE    ACCT-SRYYM        TO  ORC-DBYMD(1:6).
           MOVE    "01"              TO  ORC-DBYMD(7:2).
      *
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-530-RECECHEK-READ2-SEC
               IF  IDY5   NOT =   ZERO
                   MOVE    SRY-SRYCD (IDX3)    TO  W-SRYCD-31 (IDY5)
                   IF  MCP-RC   NOT =   ZERO
                       MOVE    "*"                 TO  W-SRYCD-32 (IDY5)
                   ELSE
                       MOVE    "+"                 TO  W-SRYCD-32 (IDY5)
                   END-IF
               END-IF
               PERFORM UNTIL   MCP-RC    NOT =   ZERO
                   MOVE    MCPDATA-REC         TO  CHK-REC
                   PERFORM VARYING IDY2  FROM  1  BY 1
                           UNTIL   IDY2  >    10
                       IF  CHK-CD (IDY2)   NOT =   SPACE
                           MOVE    SRY-SRYCD (IDX3)
                                   TO  W-SRYCD-11 (IDY1)
                           MOVE    CHK-CD (IDY2)
                                   TO  W-SRYCD-12 (IDY1)
                           MOVE    SRY-ZAINUM
                                   TO  W-ZAINUM-11 (IDY1)
                           COMPUTE IDY1 = IDY1 + 1
                       END-IF
                   END-PERFORM
                   PERFORM T-530-RECECHEK-READ2-SEC
               END-PERFORM
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-CHK2-1-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-CHK2-2-SEC         SECTION.
      *
           INITIALIZE                CHK-REC.
           MOVE    "5"               TO  CHK-CHKKBN.
           MOVE    "2"               TO  CHK-CDKBN.
           MOVE    SRY-SRYCD (IDX3)  TO  CHK-SRYCD.
           MOVE    CHK-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"        TO  MCP-FUNC.
           MOVE    "CHK-KEY3"        TO  ORC-DBPATH.
           MOVE    ACCT-SRYYM        TO  ORC-DBYMD(1:6).
           MOVE    "01"              TO  ORC-DBYMD(7:2).
      *
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-530-RECECHEK-READ2-SEC
               IF  IDY5   NOT =   ZERO
                   MOVE    SRY-SRYCD (IDX3)    TO  W-SRYCD-41 (IDY5)
                   IF  MCP-RC   NOT =   ZERO
                       MOVE    "*"                 TO  W-SRYCD-42 (IDY5)
                   ELSE
                       MOVE    "+"                 TO  W-SRYCD-42 (IDY5)
                   END-IF
               END-IF
               PERFORM UNTIL   MCP-RC    NOT =   ZERO
                   MOVE    MCPDATA-REC         TO  CHK-REC
                   PERFORM VARYING IDY2  FROM  1  BY 1
                           UNTIL   IDY2  >    10
                       IF  CHK-CD (IDY2)   NOT =   SPACE
                           MOVE    SRY-SRYCD (IDX3)
                                   TO  W-SRYCD-13 (IDY1)
                           MOVE    CHK-CD (IDY2)
                                   TO  W-SRYCD-14 (IDY1)
                           MOVE    SRY-ZAINUM
                                   TO  W-ZAINUM-12 (IDY1)
                           COMPUTE IDY1 = IDY1 + 1
                       END-IF
                   END-PERFORM
                   PERFORM T-530-RECECHEK-READ2-SEC
               END-PERFORM
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-CHK2-2-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-READ2-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-READ2-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-CHK3-1-SEC         SECTION.
      *
           PERFORM VARYING IDX4  FROM  1  BY 1
                   UNTIL   IDX4  >  1000
               IF  W-SRYCD-12 (IDX4)   =   SRY-SRYCD (IDX3)
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                   MOVE    SPACE               TO  DID2-REC
                   MOVE    ACCT-HOSPID         TO  DID2-REC(1:24)
                   MOVE    "-"                 TO  DID2-REC(25:1)
                   MOVE    ACCT-PTID           TO  DID2-REC(26:10)
                   MOVE    "-"                 TO  DID2-REC(36:1)
                   MOVE    W-SRYCD-11 (IDX4)   TO  DID2-REC(37:9)
                   MOVE    "-"                 TO  DID2-REC(46:1)
                   MOVE    SRY-SRYCD (IDX3)    TO  DID2-REC(47:9)
                   MOVE    T-MESSAGE-530-07    TO  DID2-REC(71:90)
                   MOVE    W-ZAINUM-11 (IDX4)  TO  DID2-REC(171:9)
                   MOVE    "-"                 TO  DID2-REC(180:1)
                   MOVE    SRY-ZAINUM          TO  DID2-REC(181:9)
                   MOVE    "-"                 TO  DID2-REC(190:1)
                   MOVE    ACCT-SRYKA          TO  DID2-REC(191:2)
                   MOVE    "-"                 TO  DID2-REC(193:1)
                   MOVE    ACCT-NYUGAIKBN      TO  DID2-REC(194:1)
                   WRITE   DID2-REC
                   MOVE    "*"                 TO  T-ERR-11
                   MOVE    2000                TO  IDX4
                   MOVE    10                  TO  IDX3
               ELSE
                   IF  (W-SRYCD-11 (IDX4)   =   SPACE)  AND
                       (W-SRYCD-12 (IDX4)   =   SPACE)
                       MOVE    IDX4                TO  IDY1
                       MOVE    SRY-SRYCD (IDX3)    TO  W-SRYCD-11 (IDX4)
                       IF  W-SRYCD-S3-KBN  =  SPACE
                           PERFORM T-530-RECECHEK-CHK2-1-SEC
                       END-IF
                       MOVE    2000                TO  IDX4
                   ELSE
                       IF  W-SRYCD-11 (IDX4)   =   SRY-SRYCD (IDX3)
                           MOVE    2000                TO  IDX4
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
      *
       T-530-RECECHEK-CHK3-1-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-CHK3-2-SEC         SECTION.
      *
           PERFORM VARYING IDX4  FROM  1  BY 1
                   UNTIL   IDX4  >  1000
               IF  W-SRYCD-14 (IDX4)   =   SRY-SRYCD (IDX3)
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                   MOVE    SPACE               TO  DID2-REC
                   MOVE    ACCT-HOSPID         TO  DID2-REC(1:24)
                   MOVE    "-"                 TO  DID2-REC(25:1)
                   MOVE    ACCT-PTID           TO  DID2-REC(26:10)
                   MOVE    "-"                 TO  DID2-REC(36:1)
                   MOVE    W-SRYCD-13 (IDX4)   TO  DID2-REC(37:9)
                   MOVE    "-"                 TO  DID2-REC(46:1)
                   MOVE    SRY-SRYCD (IDX3)    TO  DID2-REC(47:9)
                   MOVE    T-MESSAGE-530-07    TO  DID2-REC(71:90)
                   MOVE    W-ZAINUM-12 (IDX4)  TO  DID2-REC(171:9)
                   MOVE    "-"                 TO  DID2-REC(180:1)
                   MOVE    SRY-ZAINUM          TO  DID2-REC(181:9)
                   MOVE    "-"                 TO  DID2-REC(190:1)
                   MOVE    ACCT-SRYKA          TO  DID2-REC(191:2)
                   MOVE    "-"                 TO  DID2-REC(193:1)
                   MOVE    ACCT-NYUGAIKBN      TO  DID2-REC(194:1)
                   WRITE   DID2-REC
                   MOVE    "*"                 TO  T-ERR-11-2
                   MOVE    2000                TO  IDX4
                   MOVE    10                  TO  IDX3
               ELSE
                   IF  (W-SRYCD-13 (IDX4)   =   SPACE)  AND
                       (W-SRYCD-14 (IDX4)   =   SPACE)
                       MOVE    IDX4                TO  IDY1
                       MOVE    SRY-SRYCD (IDX3)    TO  W-SRYCD-13 (IDX4)
                       IF  W-SRYCD-S3-KBN  =  SPACE
                           PERFORM T-530-RECECHEK-CHK2-2-SEC
                       END-IF
                       MOVE    2000                TO  IDX4
                   ELSE
                       IF  W-SRYCD-13 (IDX4)   =   SRY-SRYCD (IDX3)
                           MOVE    2000                TO  IDX4
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
      *
       T-530-RECECHEK-CHK3-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    相互作用テーブル 参照処理
      *****************************************************************
       T-530-RECECHEK-CHK4-SEC           SECTION.
      *
           INITIALIZE                INTERACT-REC.
           MOVE    SRY-SRYCD (IDX3)  TO  INTERACT-DRUGCD.
           MOVE    INTERACT-REC      TO  MCPDATA-REC.
           MOVE    "DBSELECT"        TO  MCP-FUNC.
           MOVE    "INTERACT-KEY"    TO  ORC-DBPATH.
      *
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-08        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-08(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-530-RECECHEK-READ4-SEC
               IF  IDY5   NOT =   ZERO
                   MOVE    SRY-SRYCD (IDX3)    TO  W-SRYCD-51 (IDY5)
                   IF  MCP-RC   NOT =   ZERO
                       MOVE    "*"                 TO  W-SRYCD-52 (IDY5)
                   ELSE
                       MOVE    "+"                 TO  W-SRYCD-52 (IDY5)
                   END-IF
               END-IF
               PERFORM UNTIL   MCP-RC    NOT =   ZERO
                   MOVE    MCPDATA-REC         TO  INTERACT-REC
                   MOVE    SRY-SRYCD (IDX3)    TO  W-SRYCD-21 (IDY1)
                   MOVE    INTERACT-DRUGCD2    TO  W-SRYCD-22 (IDY1)
                   MOVE    SRY-ZAINUM          TO  W-ZAINUM-21 (IDY1)
                   COMPUTE IDY1 = IDY1 + 1
                   PERFORM T-530-RECECHEK-READ4-SEC
               END-PERFORM
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "INTERACT-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-CHK4-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-READ4-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "INTERACT-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-READ4-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-CHK4-1-SEC         SECTION.
      *
           PERFORM VARYING IDX4  FROM  1  BY 1
                   UNTIL   IDX4  >  1000
               IF  W-SRYCD-22 (IDX4)   =   SRY-SRYCD (IDX3)
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                   MOVE    SPACE               TO  DID2-REC
                   MOVE    ACCT-HOSPID         TO  DID2-REC(1:24)
                   MOVE    "-"                 TO  DID2-REC(25:1)
                   MOVE    ACCT-PTID           TO  DID2-REC(26:10)
                   MOVE    "-"                 TO  DID2-REC(36:1)
                   MOVE    W-SRYCD-21 (IDX4)   TO  DID2-REC(37:9)
                   MOVE    "-"                 TO  DID2-REC(46:1)
                   MOVE    SRY-SRYCD (IDX3)    TO  DID2-REC(47:9)
                   MOVE    T-MESSAGE-530-06    TO  DID2-REC(71:90)
                   MOVE    W-ZAINUM-21 (IDX4)  TO  DID2-REC(171:9)
                   MOVE    "-"                 TO  DID2-REC(180:1)
                   MOVE    SRY-ZAINUM          TO  DID2-REC(181:9)
                   MOVE    "-"                 TO  DID2-REC(190:1)
                   MOVE    ACCT-SRYKA          TO  DID2-REC(191:2)
                   MOVE    "-"                 TO  DID2-REC(193:1)
                   MOVE    ACCT-NYUGAIKBN      TO  DID2-REC(194:1)
                   WRITE   DID2-REC
                   MOVE    "*"                 TO  T-ERR-10
                   MOVE    2000                TO  IDX4
                   MOVE    10                  TO  IDX3
               ELSE
                   IF  (W-SRYCD-21 (IDX4)   =   SPACE)  AND
                       (W-SRYCD-22 (IDX4)   =   SPACE)
                       MOVE    IDX4                TO  IDY1
                       MOVE    SRY-SRYCD (IDX3)    TO  W-SRYCD-21 (IDX4)
                       IF  W-SRYCD-S3-KBN  =  SPACE
                           PERFORM T-530-RECECHEK-CHK4-SEC
                       END-IF
                       MOVE    2000                TO  IDX4
                   ELSE
                       IF  W-SRYCD-21 (IDX4)   =   SRY-SRYCD (IDX3)
                           MOVE    2000                TO  IDX4
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
      *
       T-530-RECECHEK-CHK4-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名情報テーブル 参照処理
      *****************************************************************
       T-530-RECECHEK-CHK5-SEC           SECTION.
      *
           INITIALIZE                  PTBYOMEI-REC.
           MOVE    ACCT-HOSPID         TO  PTBYO-HOSPID.
           MOVE    ACCT-PTID           TO  PTBYO-PTID.
           MOVE    ACCT-SRYKA          TO  PTBYO-SRYKA.
           MOVE    CHK-CD (IDY2)       TO  PTBYO-BYOMEICD (1).
           MOVE    CHK-CD (IDY2)       TO  PTBYO-BYOMEICD (2).
           MOVE    CHK-CD (IDY2)       TO  PTBYO-BYOMEICD (3).
           MOVE    CHK-CD (IDY2)       TO  PTBYO-BYOMEICD (4).
           MOVE    CHK-CD (IDY2)       TO  PTBYO-BYOMEICD (5).
           MOVE    CHK-CD (IDY2)       TO  PTBYO-BYOMEICD (6).
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTBYOMEI-KEY5"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    1                    TO  FLG-TBL
           ELSE
               PERFORM T-530-RECECHEK-READ5-SEC
               IF  MCP-RC   NOT =   ZERO
                   MOVE    1                    TO  FLG-TBL
               ELSE
                   MOVE    2                    TO  FLG-TBL
                   MOVE    2                    TO  FLG-TENKIYMD-ER
                   MOVE    20                   TO  IDY2
                   MOVE    MCPDATA-REC          TO  PTBYOMEI-REC
                   IF  (PTBYO-TENKIYMD   NOT  =   SPACE)      AND
                       (PTBYO-TENKIYMD   NOT  =   ZERO)       AND
                       (PTBYO-TENKIYMD(1:6)   <   T-SINRYOU-AREA)
                       MOVE    1                  TO  FLG-TENKIYMD-ER
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTBYOMEI-KEY5"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-CHK5-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-READ5-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "PTBYOMEI-KEY5"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-READ5-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェク情報テーブル 参照処理
      *****************************************************************
       T-530-RECECHEK-CHK6-SEC         SECTION.
      *
           INITIALIZE                CHK-REC.
           MOVE    "2"               TO  CHK-CHKKBN.
           MOVE    "1"               TO  CHK-CDKBN.
           MOVE    SRY-SRYCD (IDX3)  TO  CHK-SRYCD.
           MOVE    CHK-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"        TO  MCP-FUNC.
           MOVE    "CHK-KEY3"        TO  ORC-DBPATH.
           MOVE    ACCT-SRYYM        TO  ORC-DBYMD(1:6).
           MOVE    "01"              TO  ORC-DBYMD(7:2).
      *
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               MOVE    ZERO                    TO  FLG-END
               MOVE    ZERO                    TO  FLG-TBL
               MOVE    ZERO                    TO  FLG-TENKIYMD-ER
               PERFORM T-530-RECECHEK-READ2-SEC
               IF  IDY5   NOT =   ZERO
                   MOVE    SRY-SRYCD (IDX3)        TO  W-SRYCD-61 (IDY5)
                   IF  MCP-RC   NOT =   ZERO
                       MOVE    "*"                 TO  W-SRYCD-62 (IDY5)
                   ELSE
                       MOVE    "+"                 TO  W-SRYCD-62 (IDY5)
                   END-IF
               END-IF
               PERFORM UNTIL   (MCP-RC    NOT =   ZERO)  OR
                               (FLG-END   NOT =   ZERO)
                   MOVE    MCPDATA-REC         TO  CHK-REC
                   PERFORM VARYING IDY2  FROM  1  BY 1
                           UNTIL   IDY2  >    10
                       PERFORM VARYING IDY1  FROM  1  BY 1
                               UNTIL   IDY1  >  1000
                           IF  W-SRYCD-91 (IDY1)   =   SPACE
                               MOVE    CHK-CD (IDY2)      TO  W-SRYCD-91
                                                                  (IDY1)
                               MOVE    2000               TO  IDY1
                               IF  (CHK-CD (IDY2)   NOT =   SPACE) AND
                                   (CHK-CD (IDY2)   NOT =   ZERO)
                                   PERFORM T-530-RECECHEK-CHK5-SEC
                               END-IF
                           ELSE
                               IF  W-SRYCD-91 (IDY1)  =  CHK-CD (IDY2)
                                   MOVE    2000               TO  IDY1
                               END-IF
                           END-IF
                       END-PERFORM
                       IF  (CHK-CD (IDY2)       =   SPACE) OR
                           (CHK-CD (IDY2)       =   ZERO)  OR
                           (FLG-TBL             =   2)
                           MOVE    1                    TO  FLG-END
                       END-IF
                   END-PERFORM
                   IF  FLG-END   =   ZERO
                       PERFORM T-530-RECECHEK-READ2-SEC
                   END-IF
               END-PERFORM
               PERFORM T-530-RECECHEK-CHK8-1-SEC
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-CHK6-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-CHK7-SEC         SECTION.
      *
           INITIALIZE                CHK-REC.
           MOVE    "1"               TO  CHK-CHKKBN.
           MOVE    "1"               TO  CHK-CDKBN.
           MOVE    SRY-SRYCD (IDX3)  TO  CHK-SRYCD.
           MOVE    CHK-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"        TO  MCP-FUNC.
           MOVE    "CHK-KEY3"        TO  ORC-DBPATH.
           MOVE    ACCT-SRYYM        TO  ORC-DBYMD(1:6).
           MOVE    "01"              TO  ORC-DBYMD(7:2).
      *
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-530-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-530-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               MOVE    ZERO                    TO  FLG-END
               MOVE    ZERO                    TO  FLG-TBL
               MOVE    ZERO                    TO  FLG-TENKIYMD-ER
               PERFORM T-530-RECECHEK-READ2-SEC
               IF  IDY5   NOT =   ZERO
                   MOVE    SRY-SRYCD (IDX3)        TO  W-SRYCD-71 (IDY5)
                   IF  MCP-RC   NOT =   ZERO
                       MOVE    "*"                 TO  W-SRYCD-72 (IDY5)
                   ELSE
                       MOVE    "+"                 TO  W-SRYCD-72 (IDY5)
                   END-IF
               END-IF
               PERFORM UNTIL   (MCP-RC    NOT =   ZERO)  OR
                               (FLG-END   NOT =   ZERO)
                   MOVE    MCPDATA-REC         TO  CHK-REC
                   PERFORM VARYING IDY2  FROM  1  BY 1
                           UNTIL   IDY2  >    10
                       PERFORM VARYING IDY1  FROM  1  BY 1
                               UNTIL   IDY1  >  1000
                           IF  W-SRYCD-92 (IDY1)   =   SPACE
                               MOVE    CHK-CD (IDY2)      TO  W-SRYCD-92
                                                                  (IDY1)
                               MOVE    2000               TO  IDY1
                               IF  (CHK-CD (IDY2)   NOT =   SPACE) AND
                                   (CHK-CD (IDY2)   NOT =   ZERO)
                                   PERFORM T-530-RECECHEK-CHK5-SEC
                               END-IF
                           ELSE
                               IF  W-SRYCD-92 (IDY1)  =  CHK-CD (IDY2)
                                   MOVE    2000               TO  IDY1
                               END-IF
                           END-IF
                       END-PERFORM
                       IF  (CHK-CD (IDY2)       =   SPACE) OR
                           (CHK-CD (IDY2)       =   ZERO)  OR
                           (FLG-TBL             =   2)
                           MOVE    1                    TO  FLG-END
                       END-IF
                   END-PERFORM
                   IF  FLG-END   =   ZERO
                       PERFORM T-530-RECECHEK-READ2-SEC
                   END-IF
               END-PERFORM
               PERFORM T-530-RECECHEK-CHK9-1-SEC
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-530-RECECHEK-CHK7-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-CHK8-1-SEC         SECTION.
      *
           IF  (FLG-TBL           =   1)  OR
               (FLG-TENKIYMD-ER   =   1)
               COMPUTE T-CNT-OUT = T-CNT-OUT + 1
               MOVE    SPACE               TO  DID2-REC
               MOVE    ACCT-HOSPID         TO  DID2-REC(1:24)
               MOVE    "-"                 TO  DID2-REC(25:1)
               MOVE    ACCT-PTID           TO  DID2-REC(26:10)
               MOVE    "-"                 TO  DID2-REC(36:1)
               MOVE    SRY-SRYCD (IDX3)    TO  DID2-REC(37:9)
               MOVE    "-"                 TO  DID2-REC(46:1)
               MOVE    CHK-CD (IDY2)       TO  DID2-REC(47:9)
               MOVE    T-MESSAGE-530-04    TO  DID2-REC(71:90)
               IF  FLG-TENKIYMD-ER   =   1
                   MOVE    T-MESSAGE-530-A   TO  DID2-REC(161:6)
               END-IF
               WRITE   DID2-REC
               MOVE    "*"                 TO  T-ERR-09
           END-IF.
      *
       T-530-RECECHEK-CHK8-1-EXT.
           EXIT.
      *
      *******************
      *
       T-530-RECECHEK-CHK9-1-SEC         SECTION.
      *
           IF  (FLG-TBL           =   1)  OR
               (FLG-TENKIYMD-ER   =   1)
               COMPUTE T-CNT-OUT = T-CNT-OUT + 1
               MOVE    SPACE               TO  DID2-REC
               MOVE    ACCT-HOSPID         TO  DID2-REC(1:24)
               MOVE    "-"                 TO  DID2-REC(25:1)
               MOVE    ACCT-PTID           TO  DID2-REC(26:10)
               MOVE    "-"                 TO  DID2-REC(36:1)
               MOVE    SRY-SRYCD (IDX3)    TO  DID2-REC(37:9)
               MOVE    "-"                 TO  DID2-REC(46:1)
               MOVE    CHK-CD (IDY2)       TO  DID2-REC(47:9)
               MOVE    T-MESSAGE-530-03    TO  DID2-REC(71:90)
               IF  FLG-TENKIYMD-ER   =   1
                   MOVE    T-MESSAGE-530-A   TO  DID2-REC(161:6)
               END-IF
               WRITE   DID2-REC
               MOVE    "*"                 TO  T-ERR-08
           END-IF.
      *
       T-530-RECECHEK-CHK9-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報テーブル 参照処理
      *****************************************************************
       T-540-RECECHEK-CHK-SEC            SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC.
           MOVE    SRY-SRYCD (IDX3)    TO  TNS-SRYCD.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "TENSU-KEY4"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-540-01        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-540-01(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-540-RECECHEK-READ2-SEC
               IF  MCP-RC   NOT =   ZERO
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE             TO  DID2-REC
                   MOVE    ACCT-HOSPID       TO  DID2-REC(1:24)
                   MOVE    "-"               TO  DID2-REC(25:1)
                   MOVE    ACCT-PTID         TO  DID2-REC(26:10)
                   MOVE    "-"               TO  DID2-REC(36:1)
                   MOVE    SRY-SRYCD (IDX3)  TO  DID2-REC(37:9)
                   MOVE    T-MESSAGE-540-02  TO  DID2-REC(71:90)
                   WRITE   DID2-REC
                   MOVE    "*"               TO  T-ERR-12
               ELSE
                   MOVE    MCPDATA-REC       TO  TNS-TENSU-REC
                   MOVE    SRY-SRYCD (IDX3)  TO  W-SRYCD-81 (IDY5)
                   IF  (TNS-SRYKBN    NOT =  "11")  AND
                       (TNS-SRYKBN    NOT =  "12")  AND
                       (TNS-JITUDAY       =   1)    AND
                       (TNS-DAYCNT        =   0)
                       MOVE    "*"               TO  W-SRYCD-S3-KBN
                       MOVE    "*"               TO  W-SRYCD-82 (IDY5)
                   ELSE
                       MOVE    SPACE             TO  W-SRYCD-S3-KBN
                       MOVE    SPACE             TO  W-SRYCD-82 (IDY5)
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "TENSU-KEY4"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-540-RECECHEK-CHK-EXT.
           EXIT.
      *
      *******************
      *
       T-540-RECECHEK-READ2-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "TENSU-KEY4"        TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC. 
      *
       T-540-RECECHEK-READ2-EXT.
           EXIT.
      *
      *******************
      *
       T-540-RECECHEK-CHK2-SEC           SECTION.
      *
           PERFORM VARYING  IDY3  FROM  1  BY  1
                   UNTIL    IDY3  >    20
               IF  W-JITU-SRYCD (IDY3)    NOT =   SPACE
                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                   MOVE    SPACE                TO  DID2-REC
                   MOVE    W1-SRY-HOSPID        TO  DID2-REC(1:24)
                   MOVE    "-"                  TO  DID2-REC(25:1)
                   MOVE    W1-SRY-PTID          TO  DID2-REC(26:10)
                   MOVE    "-"                  TO  DID2-REC(36:1)
                   MOVE    W-JITU-SRYCD (IDY3)  TO  DID2-REC(37:9)
                   MOVE    T-MESSAGE-540-03     TO  DID2-REC(71:90)
                   WRITE   DID2-REC
               ELSE
                   MOVE    30                   TO  IDY3
               END-IF
           END-PERFORM.
      *
       T-540-RECECHEK-CHK2-EXT.
           EXIT.
      *
      *******************
      *
       T-540-RECECHEK-COMP-SEC           SECTION.
      *
           MOVE    ZERO                TO  W-JITU-DAY3.
           PERFORM VARYING  IDX6  FROM  1  BY  1
                   UNTIL    IDX6  >    31
               IF  (ACCT-DAY (1,IDX6)  NOT =  ZERO)  OR
                   (ACCT-DAY (2,IDX6)  NOT =  ZERO)  OR
                   (ACCT-DAY (3,IDX6)  NOT =  ZERO)  OR
                   (ACCT-DAY (4,IDX6)  NOT =  ZERO)
                   COMPUTE W-JITU-DAY3 = W-JITU-DAY3 + 1
               END-IF
           END-PERFORM.
           IF  W-JITU-DAY3    =   W-JITU-DAY5
               PERFORM VARYING  IDY3  FROM  1  BY  1
                       UNTIL    IDY3  >    20
                   IF  W-JITU-SRYCD (IDY3)   =   SRY-SRYCD (IDX3)
                       MOVE    30                TO  IDY3
                   ELSE
                       IF  W-JITU-SRYCD (IDY3)   =   SPACE
                           MOVE    SRY-SRYCD (IDX3)
                                                 TO  W-JITU-SRYCD (IDY3)
                           MOVE    30            TO  IDY3
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
               IF  W-JITU-DAY3    >   W-JITU-DAY5
                   MOVE    W-JITU-DAY3       TO  W-JITU-DAY5
                   INITIALIZE                    WK-JITUDAY2
                   MOVE    SRY-SRYCD (IDX3)  TO  W-JITU-SRYCD (1)
               END-IF
           END-IF.
      *
       T-540-RECECHEK-COMP-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴情報テーブル 参照処理
      *****************************************************************
       T-540-RECECHEK-CHK3-SEC           SECTION.
      *
           MOVE    ZERO                TO  W-JITU-DAY4.
           MOVE    ALL SPACE           TO  WK-JITUDAY1.
      *    患者入院履歴を検索する
      *    医療機関、患者ＩＤ、転入日、転出日を参照
           INITIALIZE                  PTNYUINRRK-REC.
           MOVE    ACCT-HOSPID         TO  PTNYUINRRK-HOSPID.
           MOVE    ACCT-PTID           TO  PTNYUINRRK-PTID.
           MOVE    T-SINRYOU-AREA      TO  PTNYUINRRK-TENNYUYMD(1:6).
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD(7:2).
           MOVE    T-SINRYOU-AREA      TO  PTNYUINRRK-TENSTUYMD(1:6).
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:2).
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTNYUINRRK-KEY19"  TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           IF  MCP-RC   NOT =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               MOVE    SPACE                   TO  DID2-REC
               MOVE    T-MESSAGE-540-04        TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                   TO  DID3-REC
               MOVE    T-MESSAGE-540-04(1:60)  TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-540-RECECHEK-READ3-SEC
               PERFORM UNTIL   MCP-RC    NOT =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
                   IF  T-SINRYOU-AREA   =   PTNYUINRRK-TENNYUYMD(1:6)
                       MOVE    PTNYUINRRK-TENNYUYMD(7:2)  TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF  (SNUM-RC         NOT =   ZERO)    OR
                           (SNUM-MINKBN     NOT =   SPACE)   OR
                           (SNUM-SYOKBN     NOT =   SPACE)
                           MOVE    31              TO  IDW1
                       ELSE
                           MOVE    SNUM-OUTNUM     TO  IDW1
                       END-IF
                   ELSE
                       MOVE    1               TO  IDW1
                   END-IF
      *
                   IF  T-SINRYOU-AREA   =   PTNYUINRRK-TENSTUYMD(1:6)
                       MOVE    PTNYUINRRK-TENSTUYMD(7:2)  TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF  (SNUM-RC         NOT =   ZERO)    OR
                           (SNUM-MINKBN     NOT =   SPACE)   OR
                           (SNUM-SYOKBN     NOT =   SPACE)
                           MOVE     1               TO  IDW2
                       ELSE
                           IF  PTNYUINRRK-TAIINYMD   =
                                                 PTNYUINRRK-TENSTUYMD
                               MOVE    SNUM-OUTNUM     TO  IDW2
                           ELSE
                               COMPUTE   IDW2   =   SNUM-OUTNUM   -   1
                           END-IF
                       END-IF
                   ELSE
                       MOVE    31              TO  IDW2
                   END-IF
      *
                   PERFORM VARYING    IDX6    FROM    IDW1   BY  1
                           UNTIL      IDX6    >   IDW2
                       MOVE    "1"        TO  W-JITU-DAY1 (IDX6)
                   END-PERFORM
      *
                   PERFORM T-540-RECECHEK-READ3-SEC
               END-PERFORM
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTNYUINRRK-KEY19"  TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           INITIALIZE                       STS-AREA-DAY.
           INITIALIZE                       LNK-DAY7-AREA.
           MOVE    "71"                 TO  LNK-DAY7-IRAI.
           MOVE    T-SINRYOU-AREA       TO  LNK-DAY7-YM.
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                LNK-DAY7-AREA.
           MOVE    LNK-DAY7-KOYOMI(7:2) TO  SNUM-INX.
           CALL    "ORCSNUM"            USING
                                        ORCSNUMAREA.
           IF  (SNUM-RC         NOT =   ZERO)    OR
               (SNUM-MINKBN     NOT =   SPACE)   OR
               (SNUM-SYOKBN     NOT =   SPACE)
               MOVE    1               TO  IDW1
           ELSE
               COMPUTE   IDW1   =   SNUM-OUTNUM   +   1
           END-IF.
           PERFORM VARYING    IDX6     FROM    IDW1   BY  1
                   UNTIL      IDX6     >   31
               MOVE    ALL SPACE       TO  W-JITU-DAY1 (IDX6)
           END-PERFORM.
      *
           PERFORM VARYING  IDX6  FROM  1  BY  1
                   UNTIL    IDX6  >    31
               IF  W-JITU-DAY1 (IDX6)   =    "1"
                   COMPUTE W-JITU-DAY4 = W-JITU-DAY4 + 1
               END-IF
           END-PERFORM.
      *
       T-540-RECECHEK-CHK3-EXT.
           EXIT.
      *
      *******************
      *
       T-540-RECECHEK-READ3-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "PTNYUINRRK-KEY19"  TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC. 
      *
       T-540-RECECHEK-READ3-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計情報テーブル 参照処理
      *****************************************************************
       T-540-RECECHEK-CHK4-SEC           SECTION.
      *
           INITIALIZE                  NYUINACCT-REC.
           MOVE    SPA-GMN-HOSPID      TO  NACCT-HOSPID.
           MOVE    T-NYUGAIKBN-AREA    TO  NACCT-NYUGAIKBN.
           MOVE    T-SINRYOU-AREA      TO  NACCT-SRYYM.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY18"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                     TO    DID2-REC
               MOVE    T-MESSAGE-540-05          TO    DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                     TO    DID3-REC
               MOVE    T-MESSAGE-540-05(1:60)    TO    DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-540-RECECHEK-READ4-SEC
               PERFORM UNTIL      MCP-RC   NOT  =  ZERO
                   MOVE    MCPDATA-REC               TO    NYUINACCT-REC
                   COMPUTE T-CNT-IN    =   T-CNT-IN    +   1
                   IF  (NACCT-HOSPID  NOT =   W1-SRY-HOSPID)  OR
                       (NACCT-PTID    NOT =   W1-SRY-PTID)
                       PERFORM T-500-RECECHEK3-SEC
                       IF  PTINF-TSTPTNUMKBN   NOT =  "1"
                           IF  WK-ZAISKBKBN   =   "1"
                               MOVE    ZERO             TO  W-JITU-DAY5
                               PERFORM VARYING  IDX6  FROM  1  BY  1
                                       UNTIL    IDX6  >    31
                                   IF  W-JITU-DAY1 (IDX6)   =    "1"
                                       COMPUTE W-JITU-DAY5 = W-JITU-DAY5
                                                                     + 1
                                   END-IF
                               END-PERFORM
                           ELSE
                               MOVE    ZERO             TO  W-JITU-DAY4
                               PERFORM VARYING  IDX6  FROM  1  BY  1
                                       UNTIL    IDX6  >    31
                                   IF  W-JITU-DAY1 (IDX6)   =    "1"
                                       COMPUTE W-JITU-DAY4 = W-JITU-DAY4
                                                                     + 1
                                   END-IF
                               END-PERFORM
                           END-IF
                           IF  SPA-GMN-CNV (14)      =   "1"
                               MOVE    1             TO  IDX6
                               PERFORM UNTIL      IDX6    >    31
                                   IF  W-NACCT-DAY (IDX6)   NOT =
                                                                ALL ZERO
                                       PERFORM T-540-RECECHEK-CHK6-SEC
                                   END-IF
                                   COMPUTE IDX6 = IDX6 + 1
                               END-PERFORM
                               IF  WK-NYUINACCT    NOT =   ALL ZERO
                                   COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                                   MOVE    SPACE     TO  DID2-REC
                                   MOVE    W1-SRY-HOSPID
                                                     TO   DID2-REC(1:24)
                                   MOVE    "-"       TO   DID2-REC(25:1)
                                   MOVE    W1-SRY-PTID
                                                     TO  DID2-REC(26:10)
                                   MOVE    T-MESSAGE-500-06
                                                     TO  DID2-REC(71:90)
                                   MOVE    "入院"    TO  DID2-REC(115:4)
                                   WRITE   DID2-REC
                               END-IF
                           END-IF
                           IF  SPA-GMN-CNV (12)      =   "1"
                               IF  W-JITU-DAY5  <   W-JITU-DAY4
                                   PERFORM T-540-RECECHEK-CHK5-SEC
                               END-IF
                               PERFORM VARYING  IDY3  FROM  1  BY  1
                                       UNTIL    IDY3  >    20
                                   IF  W-JITU-SRYCD (IDY3)    NOT =
                                                                   SPACE
                                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                                       MOVE    SPACE TO  DID2-REC
                                       MOVE    W1-SRY-HOSPID
                                                     TO  DID2-REC(1:24)
                                       MOVE    "-"   TO  DID2-REC(25:1)
                                       MOVE    W1-SRY-PTID
                                                     TO  DID2-REC(26:10)
                                       MOVE    "-"   TO  DID2-REC(36:1)
                                       MOVE    W-JITU-SRYCD (IDY3)
                                                     TO  DID2-REC(37:9)
                                       MOVE    T-MESSAGE-540-03
                                                     TO  DID2-REC(71:90)
                                       WRITE   DID2-REC
                                   ELSE
                                       MOVE    30    TO  IDY3
                                   END-IF
                               END-PERFORM
                           END-IF
                           INITIALIZE                WK-NYUINACCT
                           MOVE    ZERO              TO  W-JITU-DAY4
                           MOVE    ZERO              TO  W-JITU-DAY5
                           MOVE    ALL SPACE         TO  WK-JITUDAY1
                           MOVE    ALL SPACE         TO  WK-JITUDAY2
                           MOVE    NACCT-ZAISKBKBN   TO  WK-ZAISKBKBN
                           MOVE    NACCT-NYUGAIKBN   TO  WK-NYUGAIKBN
                           MOVE    NACCT-SRYKA       TO  WK-SRYKA
                           MOVE    NACCT-ZAINUM      TO  WK-ZAINUM
                           PERFORM VARYING    IDX6    FROM    1   BY  1
                                   UNTIL      IDX6    >      31
                               IF  NACCT-DAY (IDX6)   NOT =    ZERO
                                   MOVE    "1"    TO  W-JITU-DAY1 (IDX6)
                               END-IF
                           END-PERFORM
                       END-IF
                   ELSE
                       IF  NACCT-ZAISKBKBN  =   WK-ZAISKBKBN
                           PERFORM VARYING    IDX6   FROM   1  BY  1
                                   UNTIL      IDX6    >    31
                               IF  NACCT-DAY (IDX6)   NOT =     ZERO
                                   MOVE    "1"    TO  W-JITU-DAY1 (IDX6)
                               END-IF
                           END-PERFORM
                       ELSE
                           IF  WK-ZAISKBKBN  =   "1"
                               MOVE    ZERO          TO  W-JITU-DAY5
                               PERFORM VARYING  IDX6  FROM  1  BY  1
                                       UNTIL    IDX6  >    31
                                   IF  W-JITU-DAY1 (IDX6)   =    "1"
                                       COMPUTE W-JITU-DAY5 =
                                                         W-JITU-DAY5 + 1
                                   END-IF
                               END-PERFORM
                           ELSE
                               MOVE    ZERO          TO  W-JITU-DAY4
                               PERFORM VARYING  IDX6  FROM  1  BY  1
                                       UNTIL    IDX6  >    31
                                   IF  W-JITU-DAY1 (IDX6)   =    "1"
                                       COMPUTE W-JITU-DAY4 =
                                                         W-JITU-DAY4 + 1
                                   END-IF
                               END-PERFORM
                           END-IF
                           IF  (SPA-GMN-CNV (12)  =   "1")         AND
                               (W-JITU-DAY5       <   W-JITU-DAY4)
                               PERFORM T-540-RECECHEK-CHK5-SEC
                           END-IF
                           MOVE    NACCT-ZAISKBKBN  TO  WK-ZAISKBKBN
                           MOVE    ALL SPACE        TO  WK-JITUDAY1
                           PERFORM VARYING    IDX6    FROM  1  BY  1
                                   UNTIL      IDX6    >    31
                               IF  NACCT-DAY (IDX6)   NOT =    ZERO
                                   MOVE    "1"    TO  W-JITU-DAY1 (IDX6)
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF
                   IF  NACCT-ZAISKBKBN  =   "5"
                       PERFORM VARYING    IDX6    FROM    1   BY  1
                               UNTIL      IDX6    >      31
                           IF  NACCT-DAY (IDX6)   NOT =    ZERO
                               MOVE    NACCT-DAY (IDX6)
                                                 TO  W-NACCT-DAY (IDX6)
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    NACCT-HOSPID      TO  W1-SRY-HOSPID
                   MOVE    NACCT-PTID        TO  W1-SRY-PTID
                   MOVE    NACCT-NYUGAIKBN   TO  WK-NYUGAIKBN
                   MOVE    NACCT-SRYKA       TO  WK-SRYKA
                   MOVE    NACCT-ZAINUM      TO  WK-ZAINUM
                   PERFORM T-540-RECECHEK-READ4-SEC
               END-PERFORM
               IF  WK-ZAISKBKBN  =   "1"
                   MOVE    ZERO             TO  W-JITU-DAY5
                   PERFORM VARYING  IDX6  FROM  1  BY  1
                           UNTIL    IDX6  >    31
                       IF  W-JITU-DAY1 (IDX6)   =    "1"
                           COMPUTE W-JITU-DAY5 = W-JITU-DAY5 + 1
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    ZERO             TO  W-JITU-DAY4
                   PERFORM VARYING  IDX6  FROM  1  BY  1
                           UNTIL    IDX6  >    31
                       IF  W-JITU-DAY1 (IDX6)   =    "1"
                           COMPUTE W-JITU-DAY4 = W-JITU-DAY4 + 1
                       END-IF
                   END-PERFORM
               END-IF
               IF  SPA-GMN-CNV (14)      =   "1"
                   MOVE    1             TO  IDX6
                   PERFORM UNTIL      IDX6    >    31
                       IF  W-NACCT-DAY (IDX6)   NOT =   ALL ZERO
                           PERFORM T-540-RECECHEK-CHK6-SEC
                       END-IF
                       COMPUTE IDX6 = IDX6 + 1
                   END-PERFORM
                   IF  WK-NYUINACCT    NOT =   ALL ZERO
                       COMPUTE T-CNT-OUT = T-CNT-OUT + 1
                       MOVE    SPACE                 TO  DID2-REC
                       MOVE    W1-SRY-HOSPID         TO  DID2-REC(1:24)
                       MOVE    "-"                   TO  DID2-REC(25:1)
                       MOVE    W1-SRY-PTID           TO  DID2-REC(26:10)
                       MOVE    T-MESSAGE-500-06      TO  DID2-REC(71:90)
                       MOVE    "入院"                TO  DID2-REC(115:4)
                       WRITE   DID2-REC
                   END-IF
               END-IF
               IF  SPA-GMN-CNV (12)      =   "1"
                   IF  W-JITU-DAY5  <   W-JITU-DAY4
                       PERFORM T-540-RECECHEK-CHK5-SEC
                   END-IF
                   PERFORM VARYING  IDY3  FROM  1  BY  1
                           UNTIL    IDY3  >    20
                       IF  W-JITU-SRYCD (IDY3)    NOT =   SPACE
                           COMPUTE T-CNT-OUT = T-CNT-OUT + 1 
                           MOVE    SPACE             TO  DID2-REC
                           MOVE    W1-SRY-HOSPID     TO  DID2-REC(1:24)
                           MOVE    "-"               TO  DID2-REC(25:1)
                           MOVE    W1-SRY-PTID       TO  DID2-REC(26:10)
                           MOVE    "-"               TO  DID2-REC(36:1)
                           MOVE    W-JITU-SRYCD (IDY3)
                                                     TO  DID2-REC(37:9)
                           MOVE    T-MESSAGE-540-03  TO  DID2-REC(71:90)
                           WRITE   DID2-REC
                       ELSE
                           MOVE    30                TO  IDY3
                       END-IF
                   END-PERFORM
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY18"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-540-RECECHEK-CHK4-EXT.
           EXIT.
      *
      *******************
      *
       T-540-RECECHEK-READ4-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY18"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-540-RECECHEK-READ4-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為情報テーブル 参照処理
      *****************************************************************
       T-540-RECECHEK-CHK5-SEC           SECTION.
      *
           INITIALIZE                  NYUINACT-REC.
           MOVE    W1-SRY-HOSPID       TO  NSRY-HOSPID.
           MOVE    WK-NYUGAIKBN        TO  NSRY-NYUGAIKBN.
           MOVE    W1-SRY-PTID         TO  NSRY-PTID.
           MOVE    WK-SRYKA            TO  NSRY-SRYKA.
           MOVE    T-SINRYOU-AREA      TO  NSRY-SRYYM.
           MOVE    WK-ZAINUM           TO  NSRY-ZAINUM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                     TO    DID2-REC
               MOVE    T-MESSAGE-540-06          TO    DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                     TO    DID3-REC
               MOVE    T-MESSAGE-540-06(1:60)    TO    DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-540-RECECHEK-READ5-SEC
               PERFORM UNTIL      MCP-RC   NOT  =  ZERO
                   MOVE    MCPDATA-REC               TO    NYUINACT-REC
                   PERFORM VARYING  IDX6  FROM  1  BY  1
                           UNTIL    IDX6  >     5
                       IF  NSRY-SRYCD (IDX6)   NOT =   SPACE
                           PERFORM VARYING  IDY3  FROM  1  BY  1
                                   UNTIL    IDY3  >    20
                               IF  W-JITU-SRYCD (IDY3)  =
                                                       NSRY-SRYCD (IDX6)
                                   MOVE    30            TO  IDY3
                               ELSE
                                   IF  W-JITU-SRYCD (IDY3)   =   SPACE
                                       MOVE    NSRY-SRYCD (IDX6)
                                                 TO  W-JITU-SRYCD (IDY3)
                                       MOVE    30            TO  IDY3
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
                   PERFORM T-540-RECECHEK-READ5-SEC
               END-PERFORM
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-540-RECECHEK-CHK5-EXT.
           EXIT.
      *
      *******************
      *
       T-540-RECECHEK-READ5-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-540-RECECHEK-READ5-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せテーブル 参照処理（入院）
      *****************************************************************
       T-540-RECECHEK-CHK6-SEC           SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC.
           MOVE    W1-SRY-HOSPID       TO  COMB-HOSPID.
           MOVE    W1-SRY-PTID         TO  COMB-PTID.
           MOVE    W-NACCT-DAY (IDX6)  TO  COMB-HKNCOMBINUM.
           MOVE    W-NACCT-DAY (IDX6)  TO  W-NACCT-DAY-D.
           MOVE    ZERO                TO  W-NACCT-DAY-H.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                  TO  DID2-REC
               MOVE    T-MESSAGE-200-01       TO  DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                  TO  DID3-REC
               MOVE    T-MESSAGE-200-01(1:60) TO  DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-200-RECECHEK-READ-SEC
               IF  MCP-RC    NOT  =   ZERO
                   MOVE    40                 TO  IDX6
               ELSE
                   MOVE    MCPDATA-REC        TO  HKNCOMBI-REC
                   MOVE    T-SINRYOU-AREA     TO  W-NACCT-DAY-B(1:6)
                   MOVE    IDX6(2:2)          TO  W-NACCT-DAY-B(7:2)
                   IF  (COMB-TEKSTYMD    <=  W-NACCT-DAY-B)  AND
                       (COMB-TEKEDYMD    >=  W-NACCT-DAY-B)
                       PERFORM VARYING    IDY3    FROM  IDX6   BY  1
                               UNTIL      IDY3    >       31
                           IF  W-NACCT-DAY (IDY3)  =  W-NACCT-DAY-D
                               MOVE    ALL ZERO  TO  W-NACCT-DAY (IDY3)
                           ELSE
                               MOVE    1         TO  W-NACCT-DAY-H
                               COMPUTE IDX6 = IDX6 - 1
                           END-IF
                           IF  W-NACCT-DAY-H  =  ZERO
                               COMPUTE IDX6 = IDX6 + 1
                           END-IF
                       END-PERFORM
                   ELSE
                       PERFORM VARYING    IDY3    FROM  IDX6   BY  1
                               UNTIL      IDY3    >       31
                           IF  W-NACCT-DAY (IDY3)  =  W-NACCT-DAY-D
                               COMPUTE IDX6 = IDX6 + 1
                           ELSE
                               MOVE    40        TO  IDY3
                               COMPUTE IDX6 = IDX6 - 1
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-540-RECECHEK-CHK6-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計情報テーブル 参照処理（診療会計の関連チェック）
      *****************************************************************
       T-540-RECECHEK-CHK7-SEC           SECTION.
      *
           INITIALIZE                  NYUINACCT-REC.
           MOVE    ACCT-HOSPID         TO  NACCT-HOSPID.
           MOVE    ACCT-PTID           TO  NACCT-PTID.
           MOVE    T-SINRYOU-AREA      TO  NACCT-SRYYM.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY16"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           IF  MCP-RC    NOT  =   ZERO
               MOVE    SPACE                     TO    DID2-REC
               MOVE    T-MESSAGE-540-05          TO    DID2-REC(10:90)
               WRITE   DID2-REC
               MOVE    SPACE                     TO    DID3-REC
               MOVE    T-MESSAGE-540-05(1:60)    TO    DID3-REC(1:60)
               WRITE   DID3-REC
           ELSE
               PERFORM T-540-RECECHEK-READ7-SEC
               PERFORM UNTIL      MCP-RC   NOT  =  ZERO
                   MOVE    MCPDATA-REC               TO    NYUINACCT-REC
                   IF  NACCT-ZAISKBKBN    =   "1"   OR   "2"
                       PERFORM VARYING  IDY3  FROM  1  BY  1
                               UNTIL    IDY3  >    31
                           IF  (NACCT-DAY (IDY3)   NOT =  ZERO)  AND
                               (NACCT-ZAISKBKBN        =  "1")   AND
                               (W-NACCT-K-DAY (IDY3)   =  ZERO)
                               MOVE    1    TO  W-NACCT-K-DAY (IDY3)
                           ELSE
                               IF  (NACCT-DAY (IDY3)   NOT =  ZERO)  AND
                                   (NACCT-ZAISKBKBN        =  "2")   AND
                                   (W-NACCT-K-DAY (IDY3)   =  1)
                                   MOVE    2    TO  W-NACCT-K-DAY (IDY3)
                               END-IF
                           END-IF
                       END-PERFORM
                       PERFORM T-540-RECECHEK-READ7-SEC
                   ELSE
                       MOVE    1      TO  MCP-RC
                   END-IF
               END-PERFORM
           END-IF.
      *
           INITIALIZE                  MCPAREA.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY16"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-540-RECECHEK-CHK7-EXT.
           EXIT.
      *
      *******************
      *
       T-540-RECECHEK-READ7-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY16"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-540-RECECHEK-READ7-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       T-900-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   DID3-FILE.
      *
           READ    DID3-FILE
                   NOT  AT  END
                        OPEN    OUTPUT              DID4-FILE
                        MOVE    "処理中止"          TO  DID4-REC
                        WRITE   DID4-REC
                        CLOSE   DID4-FILE
           END-READ.
      *
           CLOSE   DID3-FILE.
      *
       T-900-ERR-HENSYU-EXT.
           EXIT.
      *
