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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC96.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : エラーメッセージセット処理
      *                      （Ｋ０２、Ｋ０５、Ｊ０４のみ共通使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/01/09    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.00     MCC-多々納   02/03/20  Ｈ１４．４改正対応済み
      * 01.00.01     NACL-多々納  02/10/03  労災加算なしの追加
      * 01.00.02     NACL-多々納  02/10/18  入院追加
      * 01.00.03     NACL−多々納 02/11/11  ＤＵＭＭＹ診察料追加
      * 01.00.04     NACL−多々納 02/12/09  動脈注射エラー追加
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
      *01  WRK-AREA.
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    パラメタ
           COPY    "CPORCSC96.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSC96AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *    共通エラーメッセージ編集
           PERFORM 100-KYOUTU-ERRMSG-SEC
      *
           IF      SPA-ERRMSG          =   SPACE
      *        プログラム別メッセージ編集
               PERFORM 200-PG-ERRMSG-SEC
           END-IF
      *
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    共通エラーメッセージ編集処理
      *****************************************************************
       100-KYOUTU-ERRMSG-SEC            SECTION.
      *
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当する点数マスターが存在しません"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "診療行為は数量を入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "医薬品は必ず数量を入力してください"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "入力コードの文字エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "診療種別がありません"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE
                   "２００行を超えます。これ以上挿入はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "回数は１回のみです。"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "注射薬は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "時間外の入力はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "撮影部位コードは入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "数量・回数等の数字・桁数エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "手技料を入力して下さい" 
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "入院のコードです。外来では入力できません。" 
                                       TO  SPA-ERRMSG
               WHEN    "0114"
                   MOVE    "外来のコードです。入院では入力できません。" 
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "老人のコードです。入力できません。" 
                                       TO  SPA-ERRMSG
               WHEN    "0115"
                   MOVE    "老人のコードではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "入力できません。" 
                                       TO  SPA-ERRMSG(29:)
               WHEN    "0016"
                   MOVE    "病院のコードです。診療所では入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0116"
                   MOVE    "診療所のコードです。病院では入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "初診・再診は重複できません"
                                       TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "算定済です。算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "手技料は剤内に１つのみです"
                                       TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "行挿入エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "数量が下限値以下です"
                                       TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "数量が上限値以上です"
                                       TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "剤削除は剤開始からです"
                                       TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "施設基準ではありません。算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "時間入力の診療コードを入力してください"
                                       TO  SPA-ERRMSG
               WHEN    "0026"
               MOVE    "病理判断料を算定できる検査を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE
                   "病理学的検査判断料が算定済です。削除後再入力"
                                       TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "検査の包括対象が混在しています。"
                                       TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "コメントの入力値エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "時間加算はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0089"
                   MOVE    "既に違う時間外区分があります。"
                                       TO  SPA-ERRMSG
                   MOVE    "入力できません。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "0031"
                   MOVE    "乳幼児加算はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "算定できない年齢です。"
                                       TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "上記算定済です。併用算定はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE
                   "初診料算定日より、１月以内です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE
                   "剤内に手技料は１件です。剤を終了して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "部位コードを入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "数量が上限を超えています"
                                       TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "酸素を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE    "酸素・窒素は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0040"
                   MOVE    "未熟児加算が有ります"
                                       TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "未熟児加算はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0042"
      ************ MOVE    "この加算はできません"
                   MOVE    "既に同じ種類の加算があります。"
                                       TO  SPA-ERRMSG
                   MOVE    "この加算はできません"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "0043"
                   MOVE    "時間外加算は１件です"
                                       TO  SPA-ERRMSG
               WHEN    "0044"
                   MOVE    "小児科外来診療料以外です"
                                       TO  SPA-ERRMSG
               WHEN    "0045"
                   MOVE    "小児科外来診療料は算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0046"
                   MOVE    "小児科外来診療料は算定済です"
                                       TO  SPA-ERRMSG
               WHEN    "0047"
                   MOVE    "器材は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0048"
                   MOVE    "約束セットの時、診療種別を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0049"
                   MOVE    "約束セット内容と診療種別が違います"
                                       TO  SPA-ERRMSG
               WHEN    "0050"
                   MOVE    "入力コードがありません"
                                       TO  SPA-ERRMSG
               WHEN    "0051"
                   MOVE    "数量は５桁までです"
                                       TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE
                   "老人慢性疾患外来総合診療料を算定時、算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0053"
                   MOVE
                   "老人慢性疾患外来総合診療料は算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0054"
                   MOVE
                   "寝たきり老人在宅総合診療料を算定時、算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0055"
                   MOVE    "自費入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0056"
                   MOVE    "検査の逓減ができません。算定日が前後します"
                                       TO  SPA-ERRMSG
               WHEN    "0057"
                   MOVE    "点滴は１日１回しか算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0157"
                   MOVE    "動脈注射は１日１回しか算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0058"
                   MOVE    "外来管理加算は算定済です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0059"
                   MOVE
                   "加算は算定できません。診察料に加算して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0060"
                   MOVE
                   "検査の重複算定エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0061"
                   MOVE
                   "残量廃棄は注射以外は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0062"
                   MOVE
                   "粉砕は内服以外は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0063"
                   MOVE
                   "粉砕の次には錠剤を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0064"
                   MOVE
                   "同一検査があります。削除して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0065"
                   MOVE
                   "初診料算定日より、２月以内です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0066"
                   MOVE
                   "１５才未満です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0067"
                   MOVE
                   "厚生労働大臣が定める患者です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0068"
                   MOVE
                   "生活習慣病指導管理料を算定時、算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0069"
                   MOVE    "時間外特例医療機関ではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "時間外特例は算定できません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "0070"
                   MOVE    "同時併施分です。入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "K070"
                   MOVE    "警告！同時併施のコードです。"
                                       TO  SPA-ERRMSG
               WHEN    "0071"
      ************ MOVE    "撮影料が違います。"
      ***********                      TO  SPA-ERRMSG
                   MOVE    "剤内に撮影料が算定されています。"
                                       TO  SPA-ERRMSG
                   MOVE    "剤を分けて下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0072"
                   MOVE
               "撮影料はこれ以上入力できません。剤を変えて下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0073"
                   MOVE    "施設基準不適合の入力がありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "逓減コードは入力できません"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "0173"
                   MOVE    "施設基準不適合の手術ではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "逓減・加算コードは入力できません"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "0074"
                   MOVE
               "この種別は投薬料が算定できない時のみ入力できます。"
                                       TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE
                   "算定可能な病床数ではありません。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0076"
                   MOVE
                   "１日に３部位まで算定できます。既に算定済みです。"
                                       TO  SPA-ERRMSG
               WHEN    "0077"
                   MOVE
                   "電話再診時には、算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0078"
                   MOVE    "ＣＴ，ＭＲＩは既に算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "２回目以降のコードを入力して下さい"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0079"
                   MOVE    "撮影料がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0080"
                   MOVE    "名称は全角で入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0081"
                   MOVE    "個別療法が１０単位を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "分散して入力して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "K081"
                   MOVE    "警告！個別療法が１０単位を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "分散して入力して下さい。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "K082"
                   MOVE    "警告！個別療法が１０単位以下です。"
                                       TO  SPA-ERRMSG
               WHEN    "0082"
                   MOVE    "初診料算定日より、６ヶ月以降です。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "0083"
                   MOVE    "この加算は算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0084"
                   MOVE    "集団療法が８単位を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "入力できません。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "0085"
                   MOVE    "集団療法が１２単位を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "入力できません。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "0091"
                   MOVE
                   "算定上限回数を超えています。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0092"
                   MOVE    "リハビリの１日の上限単位を超えています。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(43:)
               WHEN    "K092"
                   MOVE    "警告！"
                                       TO  SPA-ERRMSG
                   MOVE    "リハビリの１日単位が４単位を超えています。"
                                       TO  SPA-ERRMSG(7:)
               WHEN    "0093"
                   MOVE    "個別療法と集団療法の混在はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0094"
                   MOVE    "既にこの労災では算定済です。算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0095"
                   MOVE    "個別療法と集団療法の混在している日が"
                                       TO  SPA-ERRMSG
                   MOVE    "あります。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "0096"
                   MOVE    "検査逓減になります。剤を分けて入力"
                                       TO  SPA-ERRMSG
                   MOVE    "して下さい"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "K097"
                   MOVE    "警告！早期リハビリ加算は"
                                       TO  SPA-ERRMSG
                   MOVE    "急性発症日から９０日以内に算定できます。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "0110"
                   MOVE    "他保険にて診察料算定済のコメントは"
                                       TO  SPA-ERRMSG
                   MOVE    "診察料と同時に算定できません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "0130"
                   MOVE    "特定薬剤治療管理料が３月目以内です。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "0131"
                   MOVE    "この特定薬剤治療管理料は算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "0132"
                   MOVE    "既に特定薬剤治療管理料は当月算定済です。"
                                       TO  SPA-ERRMSG
                   MOVE    "初回算定できません。"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "0133"
                   MOVE    "初回算定日には診療年月を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0134"
                   MOVE    "既に同じ診療コードが入力されています。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(39:)
               WHEN    "0135"
                   MOVE    "初回算定日は診療年月以前を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0136"
                   MOVE    "診察料のＤＵＭＭＹ算定用です。"
                                       TO  SPA-ERRMSG
                   MOVE    "初診と再診の診療コードを設定して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "0137"
                   MOVE    "診察料のＤＵＭＭＹが既に入力されています。"
                                       TO  SPA-ERRMSG
               WHEN    "0138"
                   MOVE    "既に当日算定済です。"
                                       TO  SPA-ERRMSG
                   MOVE    "再診料を入力して下さい。"
                                       TO  SPA-ERRMSG(21:)
               WHEN    "0139"
                   MOVE    "初診料が算定されていません。"
                                       TO  SPA-ERRMSG
                   MOVE    "初診料を入力して下さい。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "7007"
                   MOVE    "自動算定で２００行を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "分けて入力して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "7001"
                   MOVE    "２００行を超えた行です。"
                                       TO  SPA-ERRMSG
                   MOVE    "これ以上入力できません。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "7002"
                   MOVE    "最終ページです。"
                                       TO  SPA-ERRMSG
                   MOVE    "これ以上入力できません。"
                                       TO  SPA-ERRMSG(17:)
               WHEN    "7003"
                   MOVE    "最終ページです。これ以上入力すると"
                                       TO  SPA-ERRMSG
                   MOVE    "登録時にエラーとなることがあります。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "7004"
                   MOVE    "２００行をオーバーします。"
                                       TO  SPA-ERRMSG
                   MOVE    "２００行までを対象とします。"
                                       TO  SPA-ERRMSG(27:)
               WHEN    "7006"
                   MOVE    "剤の明細数は５０明細までです。"
                                       TO  SPA-ERRMSG
                   MOVE    "剤を分けて下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "8000"
                   MOVE    "労災保険の診療コードです。入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "8001"
                   MOVE    "労災保険の診療コードを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "8002"
                   MOVE    "この労災保険の診察は既に入力済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "初診料は算定できません。"
                                       TO  SPA-ERRMSG(39:)
               WHEN    "8003"
                   MOVE    "救急医療管理加算は既に算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "8004"
                   MOVE    "四肢加算ができないコードです。"
                                       TO  SPA-ERRMSG
                   MOVE    "労災加算を削除して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "8005"
                   MOVE    "労災加算は１．５倍のみです。"
                                       TO  SPA-ERRMSG
               WHEN    "8006"
                   MOVE    "アフターケアの時、継続管理加算は"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "8007"
                   MOVE    "アフターケアの時、外来管理加算は"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "8008"
                   MOVE    "アフターケアの時、在宅自己導尿指導管理料は"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(43:)
               WHEN    "8009"
                   MOVE    "アフターケアの時、てんかん指導料は"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "8010"
                   MOVE    "救急医療管理加算は初診料算定時にのみ"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できます。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "8011"
                   MOVE    "療養の給付請求書取扱料は既に算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(43:)
               WHEN    "8012"
                   MOVE    "アフターケアの時、再診時療養指導管理料は"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "8013"
                   MOVE    "再診時療養指導管理料は初診料算定時には"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(39:)
               WHEN    "8014"
                   MOVE    "初診時ブラッシング料は初診料算定時にのみ"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できます。"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "8015"
                   MOVE    "アフターケアの時、救急医療管理加算は"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "8016"
                   MOVE    "アフターケアの時、労災の指導管理等は"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "8017"
                   MOVE    "アフターケアの時、在宅料は算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "8018"
                   MOVE    "アフターケアの時、初診時ブラッシング料は"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "8019"
                   MOVE    "手指の創傷に係る機能回指導加算は"
                                       TO  SPA-ERRMSG
                   MOVE    "既に算定済です。算定できません"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "8020"
                   MOVE    "この診療種別には入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "8021"
                   MOVE    "労災保険の診療コードを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "8022"
                   MOVE    "救急医療管理加算は入院日のみに"
                                       TO  SPA-ERRMSG
                   MOVE    "算定可能です。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "8050"
                   MOVE    "外来診察料算定時は電話再診の算定は"
                                       TO  SPA-ERRMSG
                   MOVE    "できません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "8051"
                   MOVE    "継続管理加算、外来管理加算は算定"
                                       TO  SPA-ERRMSG
                   MOVE    "できません。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "S001"
                   MOVE    "約束セット内に使用できないコードがあります"
                                       TO  SPA-ERRMSG
                   MOVE    "約束セットを修正して下さい。"
                                       TO  SPA-ERRMSG(43:)
               WHEN    "A002"
                   MOVE    "数量ゼロは入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "診療日付では入院は存在しません。"
                                       TO  SPA-ERRMSG
                   MOVE    "診療日付を変更して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "9002"
                   MOVE    "時間外区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "入院の日付は行を変えて入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "9004"
                   MOVE    "回数の行は入院の時のみできます。"
                                       TO  SPA-ERRMSG
               WHEN    "9005"
                   MOVE    "回数は剤の最後に入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "9006"
                   MOVE    "日付は３１までです。"
                                       TO  SPA-ERRMSG
               WHEN    "9007"
                   MOVE    "入院中の日ではありません。"
                                       TO  SPA-ERRMSG
               WHEN    "9008"
                   MOVE    "訂正の時は訂正日のみの入力です。"
                                       TO  SPA-ERRMSG
               WHEN    "9009"
                   MOVE    "入院回数の／を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "9010"
                   MOVE    "入院回数の／は１回のみです。"
                                       TO  SPA-ERRMSG
               WHEN    "9012"
                   MOVE    "日付は２桁までです。"
                                       TO  SPA-ERRMSG
               WHEN    "9013"
                   MOVE    "日付−日付の期間が違います。"
                                       TO  SPA-ERRMSG
               WHEN    "9014"
                   MOVE    "消炎鎮痛処理は逓減になります。"
                                       TO  SPA-ERRMSG
                   MOVE    "分けて入力して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "9015"
                   MOVE    "点滴手技料の算定できない液量です。"
                                       TO  SPA-ERRMSG
                   MOVE    "削除して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "9016"
                   MOVE    "手術が算定されています。"
                                       TO  SPA-ERRMSG
                   MOVE    "点滴手技料は算定できません。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "9017"
                   MOVE    "生物学的製剤注射は加算できません。"
                                       TO  SPA-ERRMSG
                   MOVE    "削除して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "9018"
                   MOVE    "麻毒注射は加算できません。"
                                       TO  SPA-ERRMSG
                   MOVE    "削除して下さい。"
                                       TO  SPA-ERRMSG(27:)
               WHEN    "9019"
                   MOVE    "既に他の点滴手技が算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "削除して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "9020"
                   MOVE    "入院の点滴手技と薬剤は剤を分けて"
                                       TO  SPA-ERRMSG
                   MOVE    "入力して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "9021"
                   MOVE    "検査が逓減になります。剤を分けて"
                                       TO  SPA-ERRMSG
                   MOVE    "入力して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "9022"
                   MOVE    "既に点滴手技が算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "造影剤注入の点滴は算定できません。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "9023"
                   MOVE    "点滴手技料が算定ができます。"
                                       TO  SPA-ERRMSG
                   MOVE    "造影剤注入の点滴手技は算定できません。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "9024"
                   MOVE    "この診療種別は入院のみの使用です。"
                                       TO  SPA-ERRMSG
               WHEN    "9025"
                   MOVE    "中心静脈注射料を算定します。"
                                       TO  SPA-ERRMSG
                   MOVE    "点滴手技料は算定できません。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "9026"
                   MOVE    "既に中心静脈注射料が算定済です。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "9027"
                   MOVE    "手術が算定されています。"
                                       TO  SPA-ERRMSG
                   MOVE    "中心静脈注射料は算定できません。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "9028"
                   MOVE    "この診療種別は外来のみの使用です。"
                                       TO  SPA-ERRMSG
               WHEN    "9029"
                   MOVE    "アフターケアは入院では選択できません。"
                                       TO  SPA-ERRMSG
               WHEN    "9030"
                   MOVE    "時間加算区分が選択中の時間加算区分と"
                                       TO  SPA-ERRMSG
                   MOVE    "違います。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "9100"
                   MOVE    "診療内容が２００行を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "会計照会で訂正して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "9200"
                   MOVE    "入院・食事は外来では入力できません。"
                                       TO  SPA-ERRMSG
      *
               WHEN    "9300"
                   MOVE    "患者登録の業務権限はありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "患者登録はできません。"
                                                TO  SPA-ERRMSG(33:)
      *
               WHEN    "K140"
                   MOVE    "警告！手術が算定されています。"
                                                TO  SPA-ERRMSG
                   MOVE    "注射料は算定できません。"
                                                TO  SPA-ERRMSG(31:)
      *
               WHEN    "K130"
                   MOVE    "警告！特定薬剤治療管理料が３月目以内です。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "K131"
                   MOVE    "この特定薬剤治療管理料は算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "K132"
                   MOVE    "既に特定薬剤治療管理料は当月算定済です。"
                                       TO  SPA-ERRMSG
                   MOVE    "初回算定できません。"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "K133"
                   MOVE    "初回算定日には診療年月を入力して下さい。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       100-KYOUTU-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラム別メッセージ編集処理
      *****************************************************************
       200-PG-ERRMSG-SEC            SECTION.
      *
           EVALUATE    LNK-SC96-PG
               WHEN    "K02"
                   PERFORM 2001-K02-ERRMSG-SEC
               WHEN    "K05"
                   PERFORM 2002-K05-ERRMSG-SEC
               WHEN    "J04"
                   PERFORM 2003-J04-ERRMSG-SEC
           END-EVALUATE
           .
       200-PG-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為エラーメッセージ編集処理
      *****************************************************************
       2001-K02-ERRMSG-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1000"
                   MOVE    "ＤＯ選択エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "金額入力エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "金額は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "該当の患者が存在しません。" 
                                       TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "患者マスタがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "保険組合がありません。処理できません"
                                       TO  SPA-ERRMSG
               WHEN    "1007"
                   MOVE    "保険組合がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1008"
                   MOVE    "訂正の時、診療科の変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1009"
                   MOVE    "訂正の時、診療日の変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE    "診療日入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "誕生日前の診療日は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "対象のデータがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "訂正で継続確定済みの時、"
                                       TO  SPA-ERRMSG(1:24)
                   MOVE    "保険の変更はできません"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "1014"
                   MOVE    "処方箋料を算定しません。"
                                       TO  SPA-ERRMSG(1:)
                   MOVE    "特定疾患処方管理加算は算定できません"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "1015"
                   MOVE    "処方料を算定しません。"
                                       TO  SPA-ERRMSG(1:)
                   MOVE    "特定疾患処方管理加算は算定できません"
                                       TO  SPA-ERRMSG(23:)
               WHEN    "1016"
                   MOVE    "初診料への自動変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1017"
                   MOVE
                   "初診算定日を登録して、再診料自動算定をして下さい"
                                       TO  SPA-ERRMSG
               WHEN    "1018"
                   MOVE
                   "対象患者の中途終了データはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1019"
                   MOVE
                   "中途終了データはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1020"
                   MOVE    "初診料が算定されてません。"
                                       TO  SPA-ERRMSG
                   MOVE    "初診料を算定後、算定して下さい"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "1021"
                   MOVE    "処方料・処方せん料は１つのみの算定です。"
                                       TO  SPA-ERRMSG
               WHEN    "1022"
                   MOVE    "消炎鎮痛処置は既に算定済みです。"
                                       TO  SPA-ERRMSG
               WHEN    "1023"
                   MOVE    "初診算定日が自動算定されています。"
                                       TO  SPA-ERRMSG
                   MOVE    "中途終了はできません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1024"
                   MOVE    "訂正中です。中途終了はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1025"
                   MOVE    "初診料算定のできない算定があります。"
                                       TO  SPA-ERRMSG
                   MOVE    "初診料への変換はできません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "1026"
                   MOVE    "診療室入力の時は、訂正はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1027"
                   MOVE    "老人の保険割合が設定されていません。"
                                       TO  SPA-ERRMSG
                   MOVE    "保険を登録し直して下さい。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "1028"
                   MOVE    "保険組合に矛盾があります。"
                                       TO  SPA-ERRMSG
                   MOVE    "患者登録で保険を登録し直して下さい。"
                                       TO  SPA-ERRMSG(27:)
               WHEN    "1029"
                   MOVE    "訂正で継続確定済みの時、"
                                       TO  SPA-ERRMSG(1:24)
                   MOVE    "診療科の変更はできません"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "1030"
                   MOVE    "処方せんプログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "1031"
                   MOVE    "薬剤情報印刷プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1032"
                   MOVE    "公費の期間に矛盾があります。"
                                       TO  SPA-ERRMSG
                   MOVE    "患者登録で保険組合せを登録し直して下さい。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "1033"
                   MOVE    "訂正で包括分入力の変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1034"
                   MOVE    "保険組合せがありません。選択し直して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "1036"
                   MOVE    "受診履歴の伝票番号がゼロです。"
                                       TO  SPA-ERRMSG
                   MOVE    "訂正できません。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "1037"
                   MOVE    "処方料が算定されません。"
                                       TO  SPA-ERRMSG
                   MOVE    "長期投薬加算（処方料）は算定できません"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "1038"
                   MOVE    "処方せん料が算定されません。"
                                       TO  SPA-ERRMSG
                   MOVE    "長期投薬加算（処方せん料）は算定できません"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "K001"
                   MOVE    "警告！時間外算定がありません"
                                       TO  SPA-ERRMSG
               WHEN    "K002"
                   MOVE    "警告！初診算定日より１月以内です"
                                       TO  SPA-ERRMSG
               WHEN    "K003"
                   MOVE    "警告！撮影料は１回目の手技料が算定できます"
                                       TO  SPA-ERRMSG
               WHEN    "K004"
                   MOVE    "警告！既に今月算定済みです。"
                                       TO  SPA-ERRMSG
               WHEN    "K018"
                   MOVE    "警告！既に今月算定上限回数を超えています。"
                                       TO  SPA-ERRMSG
               WHEN    "K005"
                   MOVE    "警告！既に診察料を算定してます。"
                                       TO  SPA-ERRMSG
                   MOVE    "小児科外来診療料の自動発生はしません"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "K006"
                   MOVE    "警告！同日診療の点滴手技料を変更して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "K007"
                   MOVE    "警告！特定薬剤治療管理料は４月日以降"
                                       TO  SPA-ERRMSG
                   MOVE    "の算定に変更して下さい。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "K008"
                   MOVE    "警告！７５歳に到達しました。"
                                       TO  SPA-ERRMSG
                   MOVE    "老人公費を入力して下さい。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "K010"
                   MOVE    "警告！手術と点滴手技料が同日に"
                                       TO  SPA-ERRMSG
                   MOVE    "算定されています。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "K011"
                   MOVE    "警告！既に違う手技料が同日に"
                                       TO  SPA-ERRMSG
                   MOVE    "算定されています。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "K012"
                   MOVE    "警告！点滴手技料の算定できる"
                                       TO  SPA-ERRMSG
                   MOVE    "液量ではありません。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "K013"
                   MOVE    "警告！既に造影剤注入点滴手技料が"
                                       TO  SPA-ERRMSG
                   MOVE    "算定済です。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "K014"
                   MOVE    "警告！既に点滴手技料が算定済です。"
                                       TO  SPA-ERRMSG
                   MOVE    "中心静脈注射は自動算定しません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "K015"
                   MOVE    "警告！手術と中心静脈注射料が同日に"
                                       TO  SPA-ERRMSG
                   MOVE    "算定されています。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "K020"
                   MOVE    "警告！再診料の受診回数が違います。"
                                       TO  SPA-ERRMSG
               WHEN    "K016"
                   MOVE    "警告！診療情報提供料が"
                                       TO  SPA-ERRMSG
                   MOVE    "２回以上の算定になります。"
                                       TO  SPA-ERRMSG(23:)
               WHEN    "K017"
                   MOVE    "警告！既に外来で調剤技術基本料が算定済み"
                                       TO  SPA-ERRMSG
                   MOVE    "です。自動算定しません。"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "K019"
                   MOVE    "警告！既に特処加算を算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE  "長期投薬加算を算定する場合は削除して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "K021"
                   MOVE    "警告！２８日以上の投薬がありません。"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       2001-K02-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット登録エラーメッセージ編集処理
      *****************************************************************
       2002-K05-ERRMSG-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "金額入力エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "金額は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "5001"
                   MOVE    "セットコード入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "5002"
                   MOVE    "すでに入力済コードです"
                                       TO  SPA-ERRMSG
               WHEN    "5003"
                   MOVE    "セットコードが存在しません"
                                       TO  SPA-ERRMSG
               WHEN    "5004"
                   MOVE    "セットコードとセット名は必須入力です"
                                       TO  SPA-ERRMSG
               WHEN    "5005"
                   MOVE    "追加エラー"
                                       TO  SPA-ERRMSG
               WHEN    "5006"
                   MOVE    "更新エラー"
                                       TO  SPA-ERRMSG
               WHEN    "5007"
                   MOVE    "削除できません"
                                       TO  SPA-ERRMSG
               WHEN    "5008"
                   MOVE    "保険外は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "5009"
                   MOVE    "約束セットの時、セットは入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "5010"
                   MOVE    "約束セットの時、１剤のみ入力です"
                                       TO  SPA-ERRMSG
               WHEN    "5011"
               MOVE    "約束セットの時、診療種別の入力はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "5012"
                   MOVE    "修正時のみ複写ができます。"
                                       TO  SPA-ERRMSG
               WHEN    "5020"
                   MOVE    "入外区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "5021"
                   MOVE    "老人一般区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       2002-K05-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計照会エラーメッセージ編集処理
      *****************************************************************
       2003-J04-ERRMSG-SEC              SECTION.
      *
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "数字入力エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "金額は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1020"
                   MOVE    "診区は変更できません"
                                       TO  SPA-ERRMSG
               WHEN    "1021"
                   MOVE    "回数は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1022"
                   MOVE    "セットは入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1023"
                   MOVE    "剤は１剤のみです"
                                       TO  SPA-ERRMSG
               WHEN    "1024"
                   MOVE    "診療行為マスタが更新できません"
                                       TO  SPA-ERRMSG
               WHEN    "1025"
                   MOVE    "既に同じ診療行為の剤が存在します。"
                                       TO  SPA-ERRMSG(1:34)
                   MOVE    "回数の変更で修正して下さい"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1026"
                   MOVE    "診療会計マスタが追加できません"
                                       TO  SPA-ERRMSG
               WHEN    "1027"
                   MOVE    "診療会計マスタ読込みエラー"
                                       TO  SPA-ERRMSG
               WHEN    "1028"
                   MOVE    "剤番号取り込みエラー"
                                       TO  SPA-ERRMSG
               WHEN    "1029"
                   MOVE    "コメントマスター更新エラー"
                                       TO  SPA-ERRMSG
               WHEN    "1030"
                   MOVE    "剤削除は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "K001"
                   MOVE    "警告！時間外算定がありません"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       2003-J04-ERRMSG-EXT.
           EXIT.
      *
      *
