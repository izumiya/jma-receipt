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
       PROGRAM-ID.             ORCHC01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : カルテ（診療録）印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/04/10  保険者番号の編集
      *  01.00.02    MCC-藤原     01/04/11  病名・公費のテーブル化
      *  01.00.03    MCC-藤原     01/04/19  パラメタの変更・公費の編集  
      *  01.00.04    MCC-藤原     01/05/20  住所編集の変更
      *  01.00.05    MCC-森脇     01/09/10  続紙の処理追加
      *  01.00.06    MCC-藤原     01/10/03  出力先プリンタ名編集
      *  01.00.07    MCC-多々納   01/12/11  カルテ病名追加
      *  01.00.08    MCC-藤原     02/03/18  帳票ファイル名の変更
      *  01.00.09    MCC-多々納   02/03/27  保険組合せなし追加
      *  01.00.10    MCC-多々納   02/05/07  全角変換の追加
      *  01.00.11    MCC-多々納   02/05/23  労災の追加
      *  01.00.12    NACL-多々納  02/09/13  自賠責・印刷サプの追加
      *  01.00.13    NACL-多々納  02/11/01  受給者番号の全角８文字以上対応
      *  01.00.14    NACL-太田    03/03/20  フリガナ、年齢を表示する
      *  02.00.01    NACL-多々納  03/03/26  続紙の頭書の追加
      *                                     プログラムの整理
      *  02.00.02    NACL-多々納  03/06/30  出力先プリンタサブ追加
      *  02.00.03    NACL-多々納  03/10/27  継続病名のみ編集追加
      *  02.00.04    NACL-多々納  05/03/03  病名なし追加
      *  02.00.04    NACL-多々納  05/10/26  MONFUNCへ変更
      *  02.00.05    NACL-多々納  06/02/06  一括印刷追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/22  公害保険表示
      *  04.01.00    NACL-多々納  08/01/25  受給者番号１２文字
      *  04.01.00    NACL-多々納  08/02/04  オンラインファイル名確認追加
      *  04.02.00    NACL-多々納  08/02/XX  記号・番号の１６文字以上対応
      *  04.04.00    NACL-多々納  08/12/03  プログラムオプション追加
      *  04.05.00    NACL-多々納  09/07/XX  備考コメント追加
      *  04.05.00    NACL-多々納  09/12/XX  備考禁忌・アレルギー・感染症追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE       ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC                 PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    カルテ
           COPY    "HCM01.INC"     REPLACING   //HCM01//
                                   BY          //HC01//.
      *    続紙
           COPY    "HCM011.INC"    REPLACING   //HCM011//
                                   BY          //HC011//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *---- (01.00.01) LINE ADD START ----------------------------
           03  FLG-OK              PIC 9(01).
           03  FLG-ZOKUSHI         PIC 9(01).
      *---- (01.00.01) LINE ADD END   ----------------------------
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-INNO            PIC 9(01).
           03  FLG-COMEND          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *****03  SYS-TIME.
      *        05  SYS-THH         PIC 9(02).
      *        05  SYS-TMM         PIC 9(02).
      *        05  SYS-TSS         PIC 9(02).
      *********05  SYS-TDD         PIC 9(02).
      *    システム領域
      *01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
      *---- (01.00.01) LINE ADD START ----------------------------
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
      *---- (01.00.01) LINE ADD END   ----------------------------  
           03  IDX2-1              PIC 9(01).
      *    備考コメント
           03  IDX-COM                 PIC 9(04).
           03  IDXC1                   PIC 9(04).
           03  IDXC2                   PIC 9(04).
           03  IDX-STR                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
           03  WRK-WSYSYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-PAGE                PIC 9(03).
           03  WRK-PRT-CNT             PIC 9(03).
      *
           03  WRK-ADRS                PIC X(200).
           03  WRK-NENREI-X.
               05  WRK-NENREI          PIC 9(03).
      *
      *---- (01.00.01) LINE ADD START ----------------------------                          
           03  WRK-HKNJA           PIC X(08).
           03  WRK-HKNJA-G         PIC X(16).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
      *---- (01.00.11) LINE ADD START ----------------------------
           03  WRK-HEN-KIGONUM         PIC X(80).
      *---- (01.00.11) LINE ADD END   ----------------------------
           03  WRK-KIGONUM             PIC X(80).
           03  WRK-NUM                 PIC X(80).
      *
           03  WRK-WARI            PIC 9(02)V9.
           03  WRK-Z6              PIC ZZZZ.
           03  WRK-Z62             PIC Z9.9.
           03  WRK-Z3              PIC ZZ9.
      *
           03  WRK-HKNCOMBI        PIC X(40).
      *---- (01.00.01) LINE ADD END   ----------------------------                          
           03  WRK-HPTNUM          PIC X(08). 
           03  WRK-FTNSNO          PIC X(16).
      *****03  WRK-JKYSNO          PIC X(16).
           03  WRK-JKYSNO          PIC X(40).
           03  WRK-TEKSTYMD        PIC X(22).
           03  WRK-TEKEDYMD        PIC X(22).
      *
           03  WRK-STRYMD.
               05  WRK-STRGO       PIC X(04).
               05  WRK-STRYY       PIC X(04).
               05  WRK-STR1        PIC X(02).
               05  WRK-STRMM       PIC X(04).
               05  WRK-STR2        PIC X(02).
               05  WRK-STRDD       PIC X(04).
               05  WRK-STR3        PIC X(02).
           03  WRK-ENDYMD.
               05  WRK-ENDGO       PIC X(04).
               05  WRK-ENDYY       PIC X(04).
               05  WRK-END1        PIC X(02).
               05  WRK-ENDMM       PIC X(04).
               05  WRK-END2        PIC X(02).
               05  WRK-ENDDD       PIC X(04).
               05  WRK-END3        PIC X(02).
      *
           03  WRK-STRYMDW.
               05  WRK-STRGOW      PIC X(01).
               05  WRK-STRYYW      PIC X(02).
               05  WRK-STR1W       PIC X(01).
               05  WRK-STRMMW      PIC X(02).
               05  WRK-STR2W       PIC X(01).
               05  WRK-STRDDW      PIC X(02).
           03  WRK-ENDYMDW.
               05  WRK-ENDGOW      PIC X(01).
               05  WRK-ENDYYW      PIC X(02).
               05  WRK-END1W       PIC X(01).
               05  WRK-ENDMMW      PIC X(02).
               05  WRK-END2W       PIC X(01).
               05  WRK-ENDDDW      PIC X(02).
      *
           03  WRK-ERRMSG          PIC X(40).
      *    カルテ病名区分
           03  WRK-KARUTE-BYOMEI   PIC X(01).
      *
           03  WRK-BYOMEI          PIC X(100).
      *
           03  WRK-MCP-PATHNAME    PIC X(64).
      *    備考コメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT         PIC X(96)
                                       OCCURS   5.
           03  WRK-COMMENT-H           PIC X(200).
           03  COM-MAX                 PIC 9(03).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC X(02).
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
           03  WRK-SPA-SYSYYYY.
               05  WRK-SPA-SYSYY   PIC  9(04).
      *
      *明細最大行
       01  MAX-LINE1               PIC 9(02)   VALUE   8.
       01  MAX-LINE2               PIC 9(02)   VALUE   22.
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-BYOMEIKBN          PIC X(01).
           03  PARA-SRYKAKBN           PIC X(01).
           03  PARA-UTAGAIKBN          PIC X(01).
           03  PARA-NYUGAIKBN          PIC X(01).
           03  PARA-CHARTKBN           PIC X(01).
           03  PARA-COMMENT            PIC X(01).
           03  PARA-TABOO              PIC X(01).
           03  PARA-ALLERGY            PIC X(01).
           03  PARA-KANSENSYO          PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY  "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY  "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *一括用
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    病名編集サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
      *    カルテ出力パラメタ
           COPY    "CPORCHC01.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC01AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期　処理
           PERFORM 100-INIT-SEC
           IF      FLG-END             =   1
               GO      TO      000-PROC-EXT
           END-IF
      *
           EVALUATE    ORCHC01-SYORIKBN
      *        カルテ印刷（オンライン）
               WHEN    "1"
                   PERFORM 200-ALL-PRT-SEC
      *        続紙頭書（オンライン）
               WHEN    "2"
                   PERFORM 300-HEAD-PRT-SEC
      *---(02.00.05) LINE ADD START ------------------------------------
      *        一括印刷
               WHEN    "3"
                   PERFORM 400-BACHI-PRT-SEC
      *---(02.00.05) LINE ADD END   ------------------------------------
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCHC01    START  *** = "  SPA-TERMID
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  HC01
           MOVE    ZERO                TO  WRK-PAGE
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    1                   TO  FLG-END
           END-IF 
      *
      *    患者登録機能制御情報
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                  SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1017-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1017-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1017-REC
           ELSE
               INITIALIZE                  SYS-1017-REC
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
      *    病名編集
           MOVE    "BYOMEIKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-BYOMEIKBN
      *    診療科
           MOVE    "SRYKAKBN"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-SRYKAKBN
      *    病名疑い区分
           MOVE    "UTAGAIKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-UTAGAIKBN
      *    入外区分
           MOVE    "NYUGAIKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-NYUGAIKBN
      *    カルテ病名疑い区分
           MOVE    "CHARTKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-CHARTKBN
      *    備考コメント区分
           MOVE    "COMMENT"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-COMMENT
      *    備考禁忌区分
           MOVE    "TABOO"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-TABOO
      *    備考アレルギー区分
           MOVE    "ALLERGY"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-ALLERGY
      *    備考感染症区分
           MOVE    "KANSENSYO"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-KANSENSYO
      *
      *    病名編集オプション設定
           IF      PARA-BYOMEIKBN      =   "0"
                                       OR  "1"
                                       OR  "2"
               MOVE    PARA-BYOMEIKBN          TO  WRK-KARUTE-BYOMEI
           ELSE
               MOVE    SYS-1017-KARUTE-BYOMEI  TO  WRK-KARUTE-BYOMEI
           END-IF
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC01"           TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
           END-IF
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ印刷処理
      *****************************************************************
       200-ALL-PRT-SEC             SECTION.
      *
      *　　印刷項目編集
           PERFORM 2001-HC01-SYORI-SEC
      *    病名を印刷する
           IF      WRK-KARUTE-BYOMEI   =   "2"
      *        病名なしの時
      *        カルテ印刷
               PERFORM 20031-HC01-PRINT-SEC
           ELSE
               PERFORM 2002-BYOMEI-HEN-SEC
           END-IF
           .
       200-ALL-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ印刷処理
      *****************************************************************
       20031-HC01-PRINT-SEC             SECTION.
      *---(02.00.05) LINE UPD START ------------------------------------
      *
           EVALUATE    ORCHC01-SYORIKBN
      *        オンライン
               WHEN    "1"
               WHEN    "2"
                   PERFORM 20031-HC01-ONL-SEC
      *        一括印刷
               WHEN    "3"
                   PERFORM 40001-HC01-BAT-SEC
           END-EVALUATE
      *
           .
       20031-HC01-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    カルテ印刷処理（オンライン）
      *****************************************************************
       20031-HC01-ONL-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           PERFORM 200311-HC01-FILE-SEC
           OPEN    OUTPUT              PRT-FILE
      *    印刷用データ出力
           MOVE    HC01                TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCM01.red"         TO  MKPRT-DIA
           MOVE   SPACE               TO  MKPRT-DEF
           MOVE   HC01PARA            TO  MKPRT-DAT
           MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
           CALL   "ORCSMKPRT"         USING
                                      ORCSMKPRTAREA
                                      SPA-AREA
      *
           .
       20031-HC01-ONL-EXT.
           EXIT.
      *****************************************************************
      *    カルテファイル名処理（オンライン）
      *****************************************************************
       200311-HC01-FILE-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM
               UNTIL       FLG-OK      =   1
               MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
               MOVE    "HC01"              TO  HC01PARA-FORM-ID
               MOVE    SPA-TERMID          TO  HC01PARA-TERMID
               MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
               ADD     1                   TO  WRK-PAGE
               MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *
               OPEN    INPUT               PRT-FILE
               IF      STS-PARA        NOT =   ZERO
                   MOVE    1               TO  FLG-OK
               END-IF
               CLOSE                       PRT-FILE
           END-PERFORM
      *
           .
       200311-HC01-FILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    続紙印刷処理
      *****************************************************************
       20032-HC011-PRINT-SEC             SECTION.
      *
           EVALUATE    ORCHC01-SYORIKBN
      *        オンライン
               WHEN    "1"
               WHEN    "2"
                   PERFORM 20032-HC011-ONL-SEC
      *        一括印刷
               WHEN    "3"
                   PERFORM 40002-HC011-BAT-SEC
           END-EVALUATE
      *
           .
       20032-HC011-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    続紙印刷処理（オンライン）
      *****************************************************************
       20032-HC011-ONL-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           PERFORM 200311-HC01-FILE-SEC
           OPEN    OUTPUT              PRT-FILE
      *    印刷用データ出力
           MOVE    HC011               TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCM011.red"        TO  MKPRT-DIA
           MOVE   SPACE               TO  MKPRT-DEF
           MOVE   HC01PARA            TO  MKPRT-DAT
           MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
           CALL   "ORCSMKPRT"         USING
                                      ORCSMKPRTAREA
                                      SPA-AREA
      *
           .
       20032-HC011-ONL-EXT.
           EXIT.
      *---(02.00.05) LINE UPD END   ------------------------------------
      *
      *****************************************************************
      *    カルテ編集処理
      *****************************************************************
       2001-HC01-SYORI-SEC                SECTION.
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM       TO  PTINF-HOSPNUM
           MOVE    SPA-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               DISPLAY "*** PTINF  INVALIE:"   PTINF-KEY
               GO      TO      2001-HC01-SYORI-EXT
           END-IF
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
      *---- (01.00.03) LINE MOD END ------------------------------
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
      *    患者保険情報検索
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
      *    主保険−ＩＤ
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM 920-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF      NOT =   ZERO
               INITIALIZE              PTHKNINF-REC
           END-IF
      *
      *---- (01.00.01) LINE ADD START ---------------------------- 
      *    保険者番号変換処理
           MOVE    SPACE               TO  WRK-HKNJA
           MOVE    9                   TO  IDY
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    8   BY  -1
                   UNTIL       IDX     <   1
               IF      PTHKN-HKNJANUM(IDX:1)   NOT =   SPACE
                   MOVE    1               TO  FLG-OK
               END-IF
               IF      FLG-OK              =   1
                   COMPUTE IDY         =   IDY     -   1
                   MOVE    PTHKN-HKNJANUM(IDX:1)   TO  WRK-HKNJA (IDY:1)
               END-IF
           END-PERFORM
           MOVE    16                  TO  WRK-MOJISU
           MOVE    WRK-HKNJA           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:16) TO  WRK-HKNJA-G
      *
      *    保険番号マスター検索
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    COMB-HKNNUM         TO  HKN-HKNNUM
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKSTYMD
           PERFORM 930-HKNNUM-READ-SEC
           IF      FLG-HKNNUM      NOT =   ZERO
               INITIALIZE              HKNNUM-REC
           END-IF
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    PTHKN-HKNJA-G         TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
      *    帳票編集処理
           PERFORM 2002-HC01-HEN-SEC
      *
           .
       2001-HC01-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2002-HC01-HEN-SEC                SECTION.
      *
           INITIALIZE  HC01
      *
      *    患者マスターより
           MOVE    SPA-PTNUM           TO  HC01-PTNUM
           IF    ( PTINF-KANANAME(31:) =   SPACE )
               MOVE    PTINF-KANANAME      TO  HC01-KANANAME
           ELSE
               MOVE    PTINF-KANANAME      TO  HC01-KANANAME2
           END-IF
           IF      PTINF-NAME(31:)     =   SPACE
               MOVE    PTINF-NAME          TO  HC01-NAME
           ELSE
               MOVE    PTINF-NAME          TO  HC01-NAME2
           END-IF
      *
      *---- (01.00.01) LINE ADD START ----------------------------- 
      *    診療科
      *****MOVE    SPA-SRYKAMEI        TO  HC01-SRYKA
      *---- (01.00.01) LINE ADD END -------------------------------
      *
           EVALUATE    PTINF-SEX
               WHEN    "1"
      *            男
                   MOVE    "○"                  TO  HC01-OTOKO
               WHEN    "2"
      *            女
                   MOVE    "○"                  TO  HC01-ONNA
           END-EVALUATE
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:4)    TO  HC01-TANJYOG
           MOVE    WRK-HYY             TO  HC01-TANJYOY
           MOVE    WRK-HMM             TO  HC01-TANJYOM
           MOVE    WRK-HDD             TO  HC01-TANJYOD
      *　　年齢
           MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           COMPUTE WRK-NENREI          =   WRK-SYSYY
                                       -   WRK-SYY
           IF      SPA-SYSYMD(5:4)     <   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI      =   WRK-NENREI   -   1
           END-IF
           MOVE    WRK-NENREI          TO  WRK-Z3
           MOVE    WRK-Z3              TO  HC01-NENREI
      *
      *    自宅−住所
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                   INTO                    WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS            TO  HC01-ADRS
      *
      *    自宅−昼電話番号
           MOVE    PTINF-HOME-TEL1     TO  HC01-ADRSTEL
      *    続柄
           MOVE    PTINF-ZOKUGARA      TO  HC01-TUZUKI
      *    職業
           MOVE    PTINF-JOB           TO  HC01-WORK
      *
      *事業所
      *    名称
           MOVE    PTINF-OFFICE-NAME   TO  HC01-KINNAME
      *    勤務先−住所,番地方書
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-OFFICE-ADRS   DELIMITED   BY  SPACE
                   PTINF-OFFICE-BANTI  DELIMITED   BY  SPACE
                   INTO                    WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS            TO  HC01-KINADRS
      *    電話番号
           MOVE    PTINF-OFFICE-TEL    TO  HC01-KINADRSTEL
      *
      *---(04.01.00) LINE ADD START ------------------------------------
      *    公害保険は保険名を保険者番号へ
           IF      COMB-HKNNUM         =  "975"
               MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNJA-G
           END-IF
      *---(04.01.00) LINE ADD END   ------------------------------------
      *    保険者情報
      *    保険者番号
           MOVE    WRK-HKNJA-G         TO  HC01-HPTNUM
      *
      *    記号・番号
      *    全角変換
           MOVE    PTHKN-KIGO          TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT
                                       TO  WRK-KIGONUM
           MOVE    PTHKN-NUM           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT
                                       TO  WRK-NUM
      *
           IF      (WRK-KIGONUM        =   SPACE )  AND
                   (WRK-NUM            =   SPACE )
               MOVE    SPACE               TO  WRK-HEN-KIGONUM
           ELSE
               STRING  WRK-KIGONUM         DELIMITED   BY  SPACE
                       "・"                DELIMITED   BY  SIZE
                       WRK-NUM             DELIMITED   BY  SPACE
                   INTO                    WRK-HEN-KIGONUM
               END-STRING
           END-IF
      *---(04.02.00) LINE ADD START ------------------------------------
      *    後期高齢者保険は番号のみ
           IF     (COMB-HKNNUM         =  "039" )
              AND (WRK-KIGONUM         =   SPACE )
               MOVE    WRK-NUM             TO  WRK-HEN-KIGONUM
           END-IF
      *---(04.02.00) LINE END START ------------------------------------
      *
           MOVE    WRK-HEN-KIGONUM     TO  HC01-KIGONUM
      *---(04.02.00) LINE ADD START ------------------------------------
      *    記号・番号が１６文字以上は別行へ
           IF      WRK-HEN-KIGONUM(33:)    NOT =   SPACE
               MOVE    SPACE               TO  HC01-KIGONUM
               MOVE    WRK-KIGONUM         TO  HC01-KIGO
               MOVE    WRK-NUM             TO  HC01-NUM
           END-IF
      *---(04.02.00) LINE END START ------------------------------------
      *
      *---- (01.00.11) LINE UPD START ----------------------------
      *
      *---- (01.00.01) LINE MOD END   ---------------------------------
      *    被保険者名
           MOVE    PTHKN-HIHKNJANAME   TO  HC01-HIHKNJNAME
      *    適用開始年月日（資格取得日へ）
      *****MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
           IF      PTHKN-SKKGETYMD     =   SPACE
               CONTINUE
           ELSE
               MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:4)    TO  HC01-SIKAKUG
               MOVE    WRK-HYY             TO  HC01-SIKAKUY
               MOVE    WRK-HMM             TO  HC01-SIKAKUM
               MOVE    WRK-HDD             TO  HC01-SIKAKUD
           END-IF
      *    適用終了年月日
           IF      PTHKN-TEKEDYMD      =   SPACE  OR  ALL "9"
               CONTINUE
           ELSE
               MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:4)    TO  HC01-YUKOG
               MOVE    WRK-HYY             TO  HC01-YUKOY
               MOVE    WRK-HMM             TO  HC01-YUKOM
               MOVE    WRK-HDD             TO  HC01-YUKOD
           END-IF
      *
      *    保険者情報
           MOVE    HKNJA-HKNJANAME         TO  HC01-HKNNAME
           STRING  HKNJA-ADRS        DELIMITED  BY  SPACE
                   HKNJA-BANTI       DELIMITED  BY  SPACE
                   INTO                    HC01-HKNADRS
           END-STRING
           MOVE    HKNJA-TEL           TO  HC01-HKNADRSTEL
      *
      *    保険区分　主保険−短縮制度名
           MOVE    SPACE               TO  WRK-HKNCOMBI
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
           END-STRING
      *
      *    公費情報
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       3
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-PTID            TO  PTKOH-PTID
               MOVE    COMB-KOHID (IDY)    TO  PTKOH-KOHID
               PERFORM 940-PTKOHINF-READ-SEC
               IF      FLG-PTKOHINF          =   ZERO
                   PERFORM 3102-PTKOHINF-HEN-SEC
                   MOVE    WRK-FTNSNO      TO  HC01-FTNSNO (IDY)
      *************MOVE    WRK-JKYSNO      TO  HC01-JKYSNO (IDY)
                   IF      WRK-JKYSNO(15:)     =   SPACE
                       MOVE    WRK-JKYSNO      TO  HC01-JKYSNO (IDY)
                   ELSE
                       IF      WRK-JKYSNO(21:)     =   SPACE
                           MOVE    WRK-JKYSNO      TO  HC01-JKYSNON(IDY)
                       ELSE
                           MOVE    WRK-JKYSNO      TO  HC01-JKYSNO12
                                                                  (IDY)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *---- (01.00.03) LINE MOD END ------------------------------ 
      *---- (01.00.11) LINE ADD START ----------------------------
      *
      *    労災の時、保険者番号・記号の変更
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBCLOSE-SEC
               IF      FLG-PTRSIINF        =   ZERO
      *            保険者番号へ記号（労災区分名）
                   MOVE    WRK-HEN-KIGONUM     TO  HC01-HPTNUM
      *            記号へ患者交付番号
                   MOVE    PTRSI-KOFUNUM       TO  WRK-MAE-INPUT
                   PERFORM 2009-HENKAN-SEC
                   MOVE    WRK-OUT-INPUT
                                               TO  HC01-KIGONUM
                   MOVE    SPACE               TO  HC01-KIGO
                   MOVE    SPACE               TO  HC01-NUM
               END-IF
           END-IF
      *
      *    備考禁忌編集
           IF      PARA-TABOO          =   "1"
               PERFORM 200210-TABOO-HENSYU-SEC
           END-IF
      *    備考アレルギー編集
           IF      PARA-ALLERGY        =   "1"
               PERFORM 200210-ALLERGY-HENSYU-SEC
           END-IF
      *    備考感染症編集
           IF      PARA-KANSENSYO      =   "1"
               PERFORM 200210-KANSENSYO-HENSYU-SEC
           END-IF
      *    備考コメント編集
           IF      PARA-COMMENT        =   "1"
               PERFORM 200210-COMMENT-HENSYU-SEC
           END-IF
           .
       2002-HC01-HEN-EXT.
           EXIT.
      *****************************************************************
      *    備考禁忌編集処理
      *****************************************************************
       200210-TABOO-HENSYU-SEC         SECTION.
      *
      *    禁忌１
           MOVE    PTINF-TABOO-1       TO  WRK-COMMENT-G
           PERFORM 200210-H01-COMMENT-SEC
      *    禁忌２
           MOVE    PTINF-TABOO-2       TO  WRK-COMMENT-G
           PERFORM 200210-H01-COMMENT-SEC
           .
       200210-TABOO-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    備考アレルギー編集処理
      *****************************************************************
       200210-ALLERGY-HENSYU-SEC         SECTION.
      *
      *    アレルギー１
           MOVE    PTINF-ALLERGY-1     TO  WRK-COMMENT-G
           PERFORM 200210-H01-COMMENT-SEC
      *    アレルギー２
           MOVE    PTINF-ALLERGY-2     TO  WRK-COMMENT-G
           PERFORM 200210-H01-COMMENT-SEC
           .
       200210-ALLERGY-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    備考感染症編集処理
      *****************************************************************
       200210-KANSENSYO-HENSYU-SEC         SECTION.
      *
      *    感染症１
           MOVE    PTINF-KANSENSYO-1   TO  WRK-COMMENT-G
           PERFORM 200210-H01-COMMENT-SEC
      *    感染症２
           MOVE    PTINF-KANSENSYO-2   TO  WRK-COMMENT-G
           PERFORM 200210-H01-COMMENT-SEC
           .
       200210-KANSENSYO-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    備考欄編集処理
      *****************************************************************
       200210-H01-COMMENT-SEC        SECTION.
      *
           MOVE    6                   TO  IDX-STR
           PERFORM VARYING IDX-COM     FROM    1   BY  1
                   UNTIL   IDX-COM     >   5
               IF      HC01-COMMENT (IDX-COM)  =   SPACE
                   MOVE    IDX-COM         TO  IDX-STR
                   MOVE    5               TO  IDX-COM
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-COM     FROM    1   BY  1
                   UNTIL  (IDX-COM     >   5   )
                   OR     (IDX-STR     >   5   )
               IF      WRK-COMMENT(IDX-COM)    NOT =   SPACE
                   MOVE    WRK-COMMENT(IDX-COM) 
                                               TO  HC01-COMMENT
                                                           (IDX-STR)
                   ADD     1                   TO  IDX-STR
               END-IF
           END-PERFORM
           .
       200210-H01-COMMENT-EXT.
           EXIT.
      *****************************************************************
      *    備考コメント編集処理
      *****************************************************************
       200210-COMMENT-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-COMMENT-G
           MOVE    ZERO                TO  IDX-COM
           MOVE    96                  TO  COM-MAX
      *    コメント１
           MOVE    PTINF-COMMENT-1     TO  WRK-COMMENT-H
           PERFORM 2002101-COMMENT-TBLHEN-SEC
      *    コメント２
           MOVE    PTINF-COMMENT-2     TO  WRK-COMMENT-H
           PERFORM 2002101-COMMENT-TBLHEN-SEC
      *    カルテ編集
           PERFORM 200210-H01-COMMENT-SEC
           .
       200210-COMMENT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    コメントテーブル編集処理
      *****************************************************************
       2002101-COMMENT-TBLHEN-SEC        SECTION.
      *
           MOVE    1                   TO  IDXC1
           MOVE    ZERO                TO  FLG-COMEND
           PERFORM
                   UNTIL      (FLG-COMEND  =   1  )
                           OR (IDX-COM     >=  5  )
      *        全角半角桁数チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-COMMENT-H (IDXC1:COM-MAX)
                                           TO  KANACHK-MAE-INPUT
               MOVE    COM-MAX             TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               ADD     1                   TO  IDX-COM
               MOVE    KANACHK-MAE-INPUT   TO  WRK-COMMENT (IDX-COM)
      *
               PERFORM VARYING     IDXC2   FROM    COM-MAX   BY  -1
                       UNTIL       IDXC2   <   1
                   IF      WRK-COMMENT(IDX-COM)(IDXC2:1)
                                           NOT =   SPACE
                       ADD     IDXC2               TO  IDXC1
                       MOVE    1                   TO  IDXC2
                   END-IF
               END-PERFORM
               IF     (IDXC1              >    200   )  OR
                      (WRK-COMMENT-H (IDXC1:)  =    SPACE )
                   MOVE    1                   TO  FLG-COMEND
               END-IF
           END-PERFORM
           .
       2002101-COMMENT-TBLHEN-EXT.
           EXIT.
      *****************************************************************
      *    病名処理
      *****************************************************************
       2002-BYOMEI-HEN-SEC        SECTION.
      *
      *    患者病名マスタより編集
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PRT-CNT
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-SRYKA           TO  PTBYO-SRYKA
           IF      PARA-SRYKAKBN       =   "1"
      *        全科
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           ELSE
      *        指定科
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 950-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           PERFORM 
                   UNTIL   FLG-PTBYOMEI    NOT =   ZERO
               MOVE    ZERO                TO  FLG-INNO
      *
      *        継続病名のみの時、転帰日のあるものを対象外とする
               IF     (WRK-KARUTE-BYOMEI   =   "1"  )  AND
                      (PTBYO-TENKIYMD  NOT =   SPACE)
                   MOVE    1                   TO  FLG-INNO
               END-IF
      *        入院分は対象外
               IF     (PARA-NYUGAIKBN      =   "1" )  AND
                      (PTBYO-NYUGAIKBN     =   "1" )
                   MOVE    1                   TO  FLG-INNO
               END-IF
      *
               IF      FLG-INNO        =   ZERO
                   ADD     1               TO  IDX
                   IF      WRK-PRT-CNT     =   1
      *                カルテ
                       IF      IDX             >   MAX-LINE1
      *                    カルテ印刷
                           PERFORM 20031-HC01-PRINT-SEC
      *                    続紙初期編集
                           PERFORM 2004-HC011-HENSYU-SEC
                           ADD     1               TO  WRK-PRT-CNT
                           MOVE    1               TO  IDX
                       END-IF
                   ELSE
      *                続紙
                       IF      IDX             >   MAX-LINE2
      *                    続紙印刷
                           PERFORM 20032-HC011-PRINT-SEC
      *                    続紙初期編集
                           PERFORM 2004-HC011-HENSYU-SEC
                           ADD     1               TO  WRK-PRT-CNT
                           MOVE    1               TO  IDX
                       END-IF
                   END-IF
      *
                   IF      WRK-PRT-CNT         =   1
      *                カルテ病名編集処理
                       PERFORM 20021-HC01-BYOMEI-SET-SEC
                   ELSE
      *                続紙病名編集処理
                       PERFORM 20022-HC011-BYOMEI-SET-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 950-PTBYOMEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           IF      WRK-PRT-CNT     =   1
      *            カルテ印刷
               PERFORM 20031-HC01-PRINT-SEC
           ELSE
               IF      IDX             >   ZERO
      *            続紙印刷
                   PERFORM 20032-HC011-PRINT-SEC
               END-IF
           END-IF
      *
           .
       2002-BYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    続紙印刷処理
      *****************************************************************
       2004-HC011-HENSYU-SEC          SECTION.
      *
           INITIALIZE  HC011
           MOVE    SPA-PTNUM           TO  HC011-PTNUM
           IF      PTINF-NAME(31:)     =   SPACE
               MOVE    PTINF-NAME          TO  HC011-NAME
           ELSE
               MOVE    PTINF-NAME          TO  HC011-NAME2
           END-IF
           .
       2004-HC011-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    カルテ病名編集処理
      *****************************************************************
       20021-HC01-BYOMEI-SET-SEC          SECTION.
      *
      *    病名編集
           PERFORM 200211-BYOMEI-HEN-SEC
           MOVE    WRK-BYOMEI          TO  HC01-BYOMEIG (IDX)
      *
      *    病名期間編集処理
           PERFORM 200211-BYOMEI-SET-SEC
      *
           MOVE    WRK-STRYYW          TO  HC01-STRYMD  (IDX  1)
           MOVE    WRK-STRMMW          TO  HC01-STRYMD  (IDX  2)
           MOVE    WRK-STRDDW          TO  HC01-STRYMD  (IDX  3)
           MOVE    WRK-ENDYYW          TO  HC01-ENDYMD  (IDX  1)
           MOVE    WRK-ENDMMW          TO  HC01-ENDYMD  (IDX  2)
           MOVE    WRK-ENDDDW          TO  HC01-ENDYMD  (IDX  3)
           IF      IDX2                >   ZERO
               MOVE    "○"            TO  HC01-TENKI   (IDX IDX2)
           END-IF
      *
      *
           .
       20021-HC01-BYOMEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       200211-BYOMEI-HEN-SEC          SECTION.
      *
           IF      PTBYO-CHARTBYOMEI   =   SPACE
      *        病名
               MOVE    PTBYO-BYOMEI        TO  WRK-BYOMEI
               IF      PARA-UTAGAIKBN      =   "1"
      *            疑い区分編集
                   PERFORM 200211-BYOMEI-UTAGAI-SEC
               END-IF
           ELSE
      *        カルテ病名
               MOVE    PTBYO-CHARTBYOMEI   TO  WRK-BYOMEI
               IF      PARA-CHARTKBN       =   "1"
      *            疑い区分編集
                   PERFORM 200211-BYOMEI-UTAGAI-SEC
               END-IF
           END-IF
           .
       200211-BYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病名疑い編集処理
      *****************************************************************
       200211-BYOMEI-UTAGAI-SEC          SECTION.
      *
           INITIALIZE                  ORCSMKBYOMEIAREA
           MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
           MOVE    WRK-BYOMEI          TO  SMKBYO-BYOMEI
           CALL    "ORCSMKBYOMEI"      USING
                                           ORCSMKBYOMEIAREA
           MOVE    SMKBYO-BYOMEI       TO  WRK-BYOMEI
           .
       200211-BYOMEI-UTAGAI-EXT.
           EXIT.
      *****************************************************************
      *    カルテ病名編集処理
      *****************************************************************
       20022-HC011-BYOMEI-SET-SEC          SECTION.
      *
      *    病名編集
           PERFORM 200211-BYOMEI-HEN-SEC
           MOVE    WRK-BYOMEI          TO  HC011-BYOMEIG (IDX)
      *
      *    病名期間編集処理
           PERFORM 200211-BYOMEI-SET-SEC
      *
           MOVE    WRK-STRYYW          TO  HC011-STRYMD  (IDX  1)
           MOVE    WRK-STRMMW          TO  HC011-STRYMD  (IDX  2)
           MOVE    WRK-STRDDW          TO  HC011-STRYMD  (IDX  3)
           MOVE    WRK-ENDYYW          TO  HC011-ENDYMD  (IDX  1)
           MOVE    WRK-ENDMMW          TO  HC011-ENDYMD  (IDX  2)
           MOVE    WRK-ENDDDW          TO  HC011-ENDYMD  (IDX  3)
           IF      IDX2                >   ZERO
               MOVE    "○"            TO  HC011-TENKI   (IDX IDX2)
           END-IF
      *
           .
       20022-HC011-BYOMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *        病名編集処理
      *****************************************************************
       200211-BYOMEI-SET-SEC        SECTION.
      *
      *    開始年月日（診療年月日）
           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMD          TO  WRK-STRYMDW
      *
      *    終了年月日（転帰年月日）
           MOVE    SPACE               TO  WRK-ENDYMD
           MOVE    SPACE               TO  WRK-ENDYMDW
           IF      PTBYO-TENKIYMD  NOT =   SPACE
               MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMD          TO  WRK-ENDYMDW
           END-IF
      *
           EVALUATE    PTBYO-TENKIKBN
               WHEN    "1"
                   MOVE    1               TO  IDX2
               WHEN    "2"
                   MOVE    2               TO  IDX2
               WHEN    "3"
                   MOVE    3               TO  IDX2
               WHEN    OTHER
                   MOVE    ZERO            TO  IDX2
           END-EVALUATE
      *
           .
       200211-BYOMEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者公費変換処理
      *****************************************************************
       3102-PTKOHINF-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-TEKSTYMD
           MOVE    SPACE               TO  WRK-TEKEDYMD
           MOVE    SPACE               TO  WRK-FTNSNO
           MOVE    SPACE               TO  WRK-JKYSNO
      *    保険者情報
      *    全角変換
           MOVE    PTKOH-FTNJANUM      TO  WRK-MAE-INPUT
           MOVE    16                  TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:16) TO  WRK-FTNSNO
      *    受給者番号１　漢字変換
      *    全角変換
           MOVE    PTKOH-JKYSNUM       TO  WRK-MAE-INPUT
      *---- (04.01.00) LINE MOD START ---------------------------------
      **** MOVE    14                  TO  WRK-MOJISU
           MOVE    20                  TO  WRK-MOJISU
      *---- (04.01.00) LINE MOD END   ---------------------------------
           PERFORM 2009-HENKAN-SEC
           MOVE    SPACE               TO  WRK-JKYSNO
           MOVE    WRK-OUT-INPUT
                                       TO  WRK-JKYSNO
      *
      *    適用開始年月日
           MOVE    PTKOH-TEKSTYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-TEKSTYMD
      *    適用終了年月日
           IF      PTKOH-TEKEDYMD      =   SPACE  OR  ALL "9"
      *---- (01.00.01) LINE MOD START ---------------------------------
      **       MOVE    SPACE               TO  WRK-TEKEDYMD
               MOVE    "平成　　年　　月　　日"
                                           TO  WRK-TEKEDYMD
      *---- (01.00.01) LINE MOD END -----------------------------------
           ELSE
               MOVE    PTKOH-TEKEDYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-TEKEDYMD
           END-IF
      *
           .
       3102-PTKOHINF-HEN-EXT.
           EXIT.
      *---- (01.00.05) LINE MOD START ----------------------------
      *
      *****************************************************************
      *    続紙頭書印刷処理
      *****************************************************************
       300-HEAD-PRT-SEC        SECTION.
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM       TO  PTINF-HOSPNUM
           MOVE    SPA-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               DISPLAY "*** PTINF  INVALIE:"   PTINF-KEY
               GO      TO      300-HEAD-PRT-EXT
           END-IF
      *
      *    続紙初期編集
           PERFORM 2004-HC011-HENSYU-SEC
      *    続紙印刷
           PERFORM 20032-HC011-PRINT-SEC
      *
           .
       300-HEAD-PRT-EXT.
           EXIT.
      *---(02.00.05) LINE ADD START ------------------------------------
      *****************************************************************
      *    バッチ一括印刷処理
      *****************************************************************
       400-BACHI-PRT-SEC        SECTION.
      *
           IF      ORCHC01-BYOMEIFLG   =   "0"
                                       OR  "1"
                                       OR  "2"
               MOVE    ORCHC01-BYOMEIFLG   TO  WRK-KARUTE-BYOMEI
           END-IF
      *
      *    印刷項目編集
           PERFORM 2001-HC01-SYORI-SEC
      *    病名を印刷する
           IF      WRK-KARUTE-BYOMEI   =   "2"
      *        病名なしの時
      *        カルテ印刷
               PERFORM 20031-HC01-PRINT-SEC
           ELSE
               PERFORM 2002-BYOMEI-HEN-SEC
           END-IF
      *
           .
       400-BACHI-PRT-EXT.
           EXIT.
      *****************************************************************
      *    カルテ印刷処理（パッチ）
      *****************************************************************
       40001-HC01-BAT-SEC             SECTION.
      *
           MOVE    ORCHC01-PRTAREA     TO  ORCSPRTAREA
      *
           MOVE    HC01                TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           MOVE    "HCM01.red"         TO  SPRT-PRTID
           MOVE    "診療録（カルテ１号紙）（外来）一括"
                                       TO  SPRT-TITLE
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               ADD     1               TO  ORCHC01-CNT
           ELSE
               MOVE    9               TO  ORCHC01-RC
           END-IF
      *
           .
       40001-HC01-BAT-EXT.
           EXIT.
      *****************************************************************
      *    続紙印刷処理（パッチ）
      *****************************************************************
       40002-HC011-BAT-SEC             SECTION.
      *
           MOVE    ORCHC01-PRTAREA     TO  ORCSPRTAREA
      *
           MOVE    HC011               TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           MOVE    "HCM011.red"        TO  SPRT-PRTID
           MOVE    "診療録（カルテ１号紙）（外来）一括"
                                       TO  SPRT-TITLE
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               ADD     1               TO  ORCHC01-CNT
           ELSE
               MOVE    9               TO  ORCHC01-RC
           END-IF
      *
           .
       40002-HC011-BAT-EXT.
           EXIT.
      *---(02.00.05) LINE ADD END   ------------------------------------
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
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
      *---- (01.00.01) LINE INS START ---------------------------------
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
      *---- (01.00.01) LINE INS END -----------------------------------
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *---- (01.00.01) LINE ADD END -----------------------------------
      * 
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
     *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
     *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読み込み
      *****************************************************************
       930-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
     *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       930-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
     *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読み込み
      *****************************************************************
       950-PTBYOMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       950-PTBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO            TO  FLG-HKNJAINF
           ELSE
               INITIALIZE              HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクロース処理
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBCLOSE-EXT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
