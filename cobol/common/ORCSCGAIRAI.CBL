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
       PROGRAM-ID.                 ORCSCGAIRAI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 外来用マスタ更新処理(K03,K08で使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/12/22    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
           SELECT  PARA-FILE       ASSIGN  FILE-NAME2
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
       FD  PARA-FILE.
       01  PARA-REC.
           03  PARA-KEY.
               05  PARA-FILEMEI         PIC X(08).
               05  PARA-EDABAN          PIC 9(03).
           03  PARA-DATA-REC            PIC X(60000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
       01  FILE-NAME2.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(07)   VALUE   "K01PARA".
           03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
           03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
      *
           03  FLG-SYOSIN-DUMMY    PIC 9(01).
      *
           03  FLG-TOKTEI          PIC 9(01).
      *
           03  FLG-CHK-OK          PIC 9(01).
      *    複数科まとめ有無
           03  FLG-FUKU-FLG              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX-P               PIC 9(04).
           03  IDX-KOUI            PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-YAKUZAI         PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-1               PIC 9(04).
           03  IDX-2               PIC 9(02).
           03  IDX-3               PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    初診算定日保存
           03  WRK-KARRK-SYOSINYMD2    PIC X(08).
      *
      *    診療行為計算用
           03  WRK-TENSU           PIC 9(08).
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-SRYKAISUKEI     PIC 9(03).
           03  WRK-SRYSURYOKEI     PIC 9(07)V9(03).
           03  WRK-MEISAISU        PIC 9(07).
           03  WRK-SRYCD           PIC 9(09).
           03  WRK-SRYCDKEI        PIC 9(14).
           03  WRK-SRYYMD.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
               05  WRK-JIHIMONEY           PIC  9(08).
      *    回数
           03  WRK-KAISU           PIC 9(08).
      *
           03  WRK-PATH                    PIC X(64).
           03  WRK-MEIBAN                  PIC 9(03).
      *
           03  WRK-DENPNUM                 PIC 9(07).
      *
      *    診療種別
           03  WRK-SRYSYUKBN              PIC X(03).
           03  WRK-MAE-SRYSYUKBN          PIC X(03).
      *    末日
           03  WRK-MATU                    PIC 9(02).
           03  WRK-NEXT-SRYYMD             PIC X(08).
      *
           03  WRK-DEL-RENNUM              PIC 9(01).
           03  WRK-JYURRK-RENNUM           PIC 9(01).
           03  WRK-JYURRK-EDANUM           PIC 9(01).
           03  WRK-JYURRK-DOUJI-RENNUM     PIC 9(01).
           03  WRK-KAIKEI-RENNUM           PIC  9(03).
      *請求金額
           03  WRK-KON-SKYMONEY        PIC  9(07).
      *請求金額
           03  WRK-SKYMONEY            PIC  9(07).
      *入金合計額
           03  WRK-NYUKIN-TOTAL        PIC  9(07).
      *入金回数
           03  WRK-NYUKIN-KAISU        PIC  9(07).
      *伝票番号枝番
           03  WRK-DENPEDANUM          PIC  9(02).
      *入金連番
           03  WRK-NYUKINRENNUM        PIC  9(02).
      *伝票状態区分
           03  WRK-DENPJTIKBN          PIC  X(01).
      *
           03  WRK-RENNUM-KEY          PIC 9(01).
      *
      *
      *    受診時間
           03  WRK-TIME1.
               05  WRK-TIME11      PIC 9(08).
           03  WRK-TIME2.
               05  WRK-TIME22      PIC 9(04).
      *    保険組合せ保存
           03  WRK-HZN-HKNCOMBINUM     PIC X(04).
           03  WRK-HZN-HKNKBN          PIC X(01).
      *
      *    診療行為内容比較用テーブル
       01  TBL-AREA.
           03  TBL-MAE-AREA.
               05  TBL-MAE-REC     OCCURS   50.
                   07  TBL-MAE-SRYCD       PIC  X(09).
                   07  TBL-MAE-SRYSURYO    PIC  9(05)V9(03).
                   07  TBL-MAE-SRYKAISU    PIC  9(03).
                   07  TBL-MAE-INPUTCOMENT PIC  X(40).
                   07  TBL-MAE-ATAI-G.
                       09  TBL-MAE-ATAI    PIC  X(08)  OCCURS  3.
           03  TBL-ATO-AREA.
               05  TBL-ATO-REC     OCCURS   50.
                   07  TBL-ATO-SRYCD       PIC  X(09).
                   07  TBL-ATO-SRYSURYO    PIC  9(05)V9(03).
                   07  TBL-ATO-SRYKAISU    PIC  9(03).
                   07  TBL-ATO-INPUTCOMENT PIC  X(40).
                   07  TBL-ATO-ATAI-G.
                       09  TBL-ATO-ATAI    PIC  X(08)  OCCURS  3.
      *
      *    剤テーブル領域
       01  WRK-TBL-ZAINUM-AREA.
           03  WRK-TBL-ZAINUM-G            OCCURS   150.
               05  WRK-TBL-ZAINUM          PIC 9(08).
           03  WRK-TBL-IDX                 PIC 9(04).
           03  FLG-ZAINUM                  PIC 9(01).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK0041.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    収納明細マスタ−
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   50.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  WRK-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                    //WKSRY-// BY //WRK-WKSRY-//.
      *
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC        OCCURS   10.
           COPY    "CPJYURRK.INC"  REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *   診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    診療会計剤チェックパラメタ
           COPY    "CPORCSCZAICHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＳＰＡパラメタ（ORCSC97.CBL 用のDAMMY)
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCGAIRAI.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCGAIRAIAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *
           EVALUATE    ORCSCGAIRAI-KBN
      *        診療行為作成
               WHEN    "1"
                   PERFORM 100-SAKUSEI-SYORI-SEC
      *        削除
               WHEN    "2"
                   PERFORM 300-DELETE-SYORI-SEC
      *        初診算定日のみ設定
               WHEN    "3"
                   PERFORM 400-SYOSIN-SYORI-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療行為作成処理
      *****************************************************************
       100-SAKUSEI-SYORI-SEC           SECTION.
      *
      *    初診料算定のみの時初診料を算定する
           IF      SPA-SYOSIN-YMD  NOT =   SPACE
               PERFORM 400-SYOSIN-SYORI-SEC
           END-IF
      *
      *    訂正時、受診履歴等の削除
           IF      SPA-SYORIFLG         =   1
               PERFORM 1001-TEISEI-SYORI-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      100-SAKUSEI-SYORI-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-KARRK-SYOSINYMD2
      *    保険毎に処理する
           PERFORM 1001-DBKOUSIN-SYORI-SEC
      *
      *    包括まとめ入力以外
           IF      SPA-HKNCOMBINUM     =   "9999"
               CONTINUE
           ELSE
               IF      SPA-SYORIFLG        =   ZERO
      *            診療科履歴作成処理
                   PERFORM 45009-SRYKARRK-SYOKI-SEC
               ELSE
      *            診療科履歴変更
                   INITIALIZE                  ORCSKARRKAREA
                   MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
                   CALL    "ORCSKARRK"         USING
                                               ORCSKARRKAREA
                                               SPA-AREA
               END-IF
           END-IF
      *
      *    ワーク診療行為マスタ−が存在した時、削除する
           PERFORM 45002-WKSRYACT-DEL-SEC
      *    受診内容データに時間を設定し、更新する
           IF      SPA-SYORIFLG        =   ZERO
               PERFORM 45003-UKETUKE-SEC
           END-IF
           .
      *
       100-SAKUSEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    各マスタ更新処理
      *****************************************************************
       1001-DBKOUSIN-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    SPA-TERMID          TO  FILE-TERMID2
           OPEN    INPUT               PARA-FILE
           PERFORM 980-PARA-READ-SEC
      *
           PERFORM
               UNTIL   FLG-PARA        NOT =   ZERO 
               IF      PARA-FILEMEI        =   "SYUNOU"
      *            パラメタの収納データ読込
                   MOVE    PARA-DATA-REC       TO  SYUNOU-REC
                   MOVE    1                   TO  FLG-PARA
               ELSE
                   PERFORM 980-PARA-READ-SEC
               END-IF
           END-PERFORM
      *
           CLOSE                        PARA-FILE
      *
      *    収納マスタを更新する
           PERFORM 1002-SYUNOU-OUT-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO      1001-DBKOUSIN-SYORI-EXT
           END-IF
      *
      *    剤番号テーブル
           INITIALIZE                  WRK-TBL-ZAINUM-AREA
      *    受診履歴初期処理
           PERFORM 1003-JYURRK-SYORI1-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO      1001-DBKOUSIN-SYORI-EXT
           END-IF
           MOVE    SPACE               TO  WRK-SRYYMD
      *
      *    診療行為マスター更新
           MOVE    ZERO                TO  FLG-PARA
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX-KOUI
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID2
           OPEN    INPUT               PARA-FILE
           PERFORM 980-PARA-READ-SEC
      *
           PERFORM
                   UNTIL       FLG-PARA   =    1
               IF      PARA-FILEMEI        =   "WKSRYACT"
                   MOVE    PARA-DATA-REC    TO  WRK-WKSRYACT-REC
      *
                   IF     (IDX-KOUI        >   ZERO  )  AND
                          (WRK-WKSRY-ZAINUM     NOT =
                                           LNK-WKSRY-ZAINUM (IDX-KOUI))
      *                剤毎に処理を行う
                       PERFORM 4501-SRYACT-KOU-SEC
                       PERFORM 45001-JYURRK-SYORI2-SEC
                       MOVE    SPACE           TO  LNK-WKSRYACT-AREA
                       MOVE    ZERO            TO  IDX-KOUI
                   END-IF
                   ADD     1               TO  IDX-KOUI
                   MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
                                                            (IDX-KOUI)
               ELSE
                   IF      IDX-KOUI        >   ZERO
                       PERFORM 4501-SRYACT-KOU-SEC
                       PERFORM 45001-JYURRK-SYORI2-SEC
                       MOVE    SPACE           TO  LNK-WKSRYACT-AREA
                       MOVE    ZERO            TO  IDX-KOUI
                   END-IF
               END-IF
      *
               PERFORM 980-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
           IF      IDX-KOUI        >   ZERO
               PERFORM 4501-SRYACT-KOU-SEC
               PERFORM 45001-JYURRK-SYORI2-SEC
           END-IF
      *
      *    受診履歴作成終了処理
           PERFORM 45001-JYURRK-SYORI3-SEC
      *
      *
           .
       1001-DBKOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ更新処理
      *****************************************************************
       1002-SYUNOU-OUT-SEC             SECTION.
      *
           IF      SPA-SYORIFLG        =   ZERO
      *        システム管理より、伝票番号を取得する
               PERFORM 45001-DENPNUM-SET-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      1002-SYUNOU-OUT-EXT
               END-IF
      *
           ELSE
               MOVE    SPA-DENPNUM     TO  WRK-DENPNUM
      *        診療科変更時
               IF      SPA-OLD-SRYKA   NOT =   SPA-SRYKA
                   MOVE    SPA-SRYKA           TO  SYU-SRYKA
               END-IF
           END-IF
      *
      *    伝票番号を編集する
           MOVE    WRK-DENPNUM         TO  ORCSCGAIRAI-DENPNUM
      *
      *    包括保険の場合、伝票番号のみ取得
           IF      SPA-HKNCOMBINUM     =   "9999"
               MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
               GO      TO      1002-SYUNOU-OUT-EXT
           END-IF
      *
      *
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *    伝票番号発行日
           MOVE    SPA-SYSYMD          TO  SYU-DENPPRTYMD
      *    ドクター
           MOVE    SPA-DRCD(2:4)       TO  SYU-DRCD
      *
      *    調整金
           MOVE    ORCSCGAIRAI-CHOSEI  TO  SYU-CHOSEI
      *    請求書作成区分
           MOVE    ORCSCGAIRAI-HAKFLG  TO  SYU-SKYKUKBN
      *    請求額
           COMPUTE SYU-SKYMONEY        =   ORCSCGAIRAI-KON-SKYMONEY
                                       +   SYU-CHOSEI
           MOVE    SYU-SKYMONEY        TO  WRK-KON-SKYMONEY
      *    入金額
           MOVE    ORCSCGAIRAI-NYUKIN  TO  SYU-NYUKIN-TOTAL
      *    入金回数
           MOVE    1                   TO  SYU-NYUKIN-KAISU
      *    伝票状態区分
           MOVE    SYU-DENPJTIKBN      TO  WRK-DENPJTIKBN
           IF      SYU-SKYMONEY        =   ZERO
               MOVE    SPACE               TO  SYU-DENPJTIKBN
           ELSE
               IF      SYU-SKYMONEY        <=  SYU-NYUKIN-TOTAL
                   MOVE    "1"                 TO  SYU-DENPJTIKBN
               ELSE
                   MOVE    "2"                 TO  SYU-DENPJTIKBN
               END-IF
           END-IF
      *    継続区分判定
           IF      SPA-SYORIFLG        =   ZERO
               IF      SPA-CONTKBN         =   "1"
                   MOVE    "7"                 TO  SYU-DENPJTIKBN
                   IF      SPA-DOUJI-DENPNUM   =   ZERO
                       MOVE    SYU-DENPNUM     TO  SPA-DOUJI-DENPNUM
                   END-IF
               END-IF
      *        継続区分
               MOVE    SPA-CONTKBN         TO  SYU-CONTKBN
           ELSE
      *        継続区分
               IF      SPA-CONTKBN         =   SYU-CONTKBN
                   MOVE    SYU-DOUJI-DENPNUM   TO  SPA-DOUJI-DENPNUM
                   IF      SPA-CONTKBN         =   "1"
                       IF      WRK-DENPJTIKBN      =   "7"
                           MOVE    "7"             TO  SYU-DENPJTIKBN
                       END-IF
                   END-IF
               ELSE
                   EVALUATE    SPA-CONTKBN
      *                継続区分がなくなった時
                       WHEN    "0"
                           MOVE    ZERO        TO  SPA-DOUJI-DENPNUM
      *                継続区分に変更
                       WHEN    "1"
                           MOVE    "7"         TO  SYU-DENPJTIKBN
                           MOVE    SYU-DENPNUM TO  SPA-DOUJI-DENPNUM
                   END-EVALUATE
               END-IF
      *        継続区分
               MOVE    SPA-CONTKBN         TO  SYU-CONTKBN
           END-IF
      *
      *    院外処方区分
           MOVE    SPA-INGAIKBN        TO  SYU-INGAISHOHOKBN
      *    同時診療伝票番号
           MOVE    SPA-DOUJI-DENPNUM   TO  SYU-DOUJI-DENPNUM
      *
      *    複数科まとめ編集
           IF      ORCSCGAIRAI-SYORI   =   ZERO
      *        訂正の時はそのまま
               IF      SPA-SYORIFLG        =   ZERO
                   MOVE    ZERO                TO  SYU-FUKU-DENPNUM
                   MOVE    SPACE               TO  SYU-FUKU-KBN
               END-IF
           ELSE
               PERFORM 45011-SYUNOU-FUKU-SEC
           END-IF
      *
           IF      SPA-SYORIFLG        =   ZERO
      *
               MOVE    SMCNDATE-YMD        TO  SYU-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
               END-IF
      *
      *        収納明細マスター作成
               PERFORM 45001-SINMEI-SYORI-SEC
      *
               MOVE    SYUNOU-REC          TO  WRK-SYUNOU-REC
      *        継続が確定した時、伝票状態区分を更新する
               IF      SPA-CONTKBN         =   "2"
                   PERFORM 45011-SYUNOU-UPD-SEC
               END-IF
      *
               MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
           END-IF
      *
           MOVE    SYUNOU-REC          TO  WRK-SYUNOU-REC
      *
      *    訂正の時、収納を読み、あれば更新、なければ追加とする
           IF      SPA-SYORIFLG    NOT =   ZERO
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
                   PERFORM 980-SYUNOU-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *        請求金額
               MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
      *        入金合計額
               MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN-TOTAL
      *        入金回数
               MOVE    SYU-NYUKIN-KAISU    TO  WRK-NYUKIN-KAISU
      *
               MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
      *        入金合計額
               IF      ORCSCGAIRAI-NYUKIN   >   ZERO
                   COMPUTE SYU-NYUKIN-TOTAL    =   WRK-NYUKIN-TOTAL
                                               +   ORCSCGAIRAI-NYUKIN
                   MOVE    WRK-NYUKIN-KAISU    TO  SYU-NYUKIN-KAISU
                   ADD   1                     TO  SYU-NYUKIN-KAISU
               ELSE
                   MOVE    WRK-NYUKIN-TOTAL    TO  SYU-NYUKIN-TOTAL
                   MOVE    WRK-NYUKIN-KAISU    TO  SYU-NYUKIN-KAISU
               END-IF
      *        伝票状態区分
               IF      SYU-DENPJTIKBN  NOT =   "7"
                   IF      SYU-SKYMONEY        >   SYU-NYUKIN-TOTAL
                       MOVE    "2"             TO  SYU-DENPJTIKBN
                   ELSE
                       MOVE    "1"             TO  SYU-DENPJTIKBN
                   END-IF
      *            請求額がゼロの時空白
                   IF      SYU-SKYMONEY        =   ZERO
                       MOVE    SPACE               TO  SYU-DENPJTIKBN
                   END-IF
               END-IF
               IF      FLG-SYUNOU          =   ZERO
      *
                   MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
      *            収納明細マスター作成
                   PERFORM 45001-SINMEI-SYORI2-SEC
               ELSE
                   MOVE    SMCNDATE-YMD        TO  SYU-CREYMD
                   MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
      *
      *            収納明細マスター作成
                   PERFORM 45001-SINMEI-SYORI-SEC
               END-IF
           END-IF
      *
      *
           MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
      *
           .
       1002-SYUNOU-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票状態区分更新処理
      *****************************************************************
       45011-SYUNOU-UPD-SEC            SECTION.
      *
      *    同時診療伝票番号を検索する
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYUNOU-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYUNOU-KEY3"       TO  ORC-DBPATH
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
           END-IF
           PERFORM UNTIL    FLG-SYUNOU     =   1
               IF      SYU-DENPJTIKBN      =   "7"
      *            伝票状態区分
                   MOVE    "1"                 TO  SYU-DENPJTIKBN
               END-IF
      *        収納明細更新
               PERFORM 45012-SYUMEI-UPD-SEC
      *
      *
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SYUNOU UPD ERR:"  MCP-RC
               END-IF
      *
               MOVE    "SYUNOU-KEY3"       TO  ORC-DBPATH
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYUNOU-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       45011-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票状態区分更新処理
      *****************************************************************
       45012-SYUMEI-UPD-SEC                SECTION.
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SYUMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
           PERFORM UNTIL    FLG-SYUMEI     =   1
               IF      SMEI-JOUTAIKBN      =   "7"
      *            伝票状態区分
                   MOVE    "1"                 TO  SMEI-JOUTAIKBN
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SYUMEI UPD ERR:"  MCP-RC
               END-IF
      *
               MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SYUMEI-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       45012-SYUMEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ編集処理
      *****************************************************************
       45011-SYUNOU-FUKU-SEC            SECTION.
      *
           IF      ORCSCGAIRAI-SYORI   =   1
      *        継続
               IF      SPA-FUKU-DENPNUM    =   ZERO
                   MOVE    SYU-DENPNUM     TO  SPA-FUKU-DENPNUM
               END-IF
               MOVE    SPA-FUKU-DENPNUM    TO  SYU-FUKU-DENPNUM
               MOVE    "1"                 TO  SYU-FUKU-KBN
           ELSE
      *        確定
               MOVE    SPA-FUKU-DENPNUM    TO  SYU-FUKU-DENPNUM
               MOVE    "2"                 TO  SYU-FUKU-KBN
           END-IF
      *
           .
       45011-SYUNOU-FUKU-EXT.
           EXIT.
      *****************************************************************
      *    システム管理より、伝票番号取得処理
      *****************************************************************
       45001-DENPNUM-SET-SEC            SECTION.
      *
           INITIALIZE                  SYS-0041-REC.
           MOVE    "0041"              TO  SYS-0041-KANRICD
           MOVE    "*"                 TO  SYS-0041-KBNCD
           MOVE    ZERO                TO  SYS-0041-STYUKYMD
           MOVE    ALL "9"             TO  SYS-0041-EDYUKYMD
      *
           MOVE    SYS-0041-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 960-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-0041-REC
               ELSE
                   INITIALIZE                  SYS-0041-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-0041-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-SYSKANRI        =   ZERO
               ADD     1                   TO  SYS-0041-DENPNUMMAX
               MOVE    SYS-0041-DENPNUMMAX
                                           TO  WRK-DENPNUM
      *
               MOVE    SMCNDATE-YMD        TO  SYS-0041-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-0041-UPHMS
      *
      *
               MOVE    SYS-0041-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC         NOT  =   ZERO
                   MOVE    "0005"              TO  SPA-ERRCD
                   DISPLAY "K03 SYSKANRI UPDATE:" MCP-RC
               END-IF
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               DISPLAY "K03 DENPNUM-SET ERR:" MCP-RC
           END-IF
      *
           .
       45001-DENPNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター作成処理
      *****************************************************************
       45001-SINMEI-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    01                  TO  SMEI-DENPEDANUM
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPA-SYSYMD          TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    WRK-KON-SKYMONEY    TO  SMEI-SKYMONEY
      *入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD
      *入金連番
           MOVE    01                  TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *
      *    入金額がゼロの時、入金力なし
           IF      ORCSCGAIRAI-NYUKIN      =   ZERO
               CONTINUE
           ELSE
      *入返金区分
               MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *入返金額
               MOVE    ORCSCGAIRAI-NYUKIN  TO  SMEI-NYUHEN-MONEY
      *入金方法
               MOVE    "01"                TO  SMEI-NYUKIN-HOHO
           END-IF
      *状態区分
           IF      SYU-SKYMONEY        =   ZERO
               MOVE    SPACE               TO  SMEI-JOUTAIKBN
           ELSE
               MOVE    "1"                 TO  SMEI-JOUTAIKBN
           END-IF
      *    継続区分判定
           IF      SPA-CONTKBN         =   "1"
               MOVE    "7"                 TO  SMEI-JOUTAIKBN
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
               DISPLAY "K03 SINMEI-SYORI MCP-RC:" MCP-RC
                        ",KEY:" SMEI-KEY
           END-IF
      *
           .
       45001-SINMEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター作成処理（訂正時）
      *****************************************************************
       45001-SINMEI-SYORI2-SEC          SECTION.
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SYUMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
           MOVE    ZERO                TO  WRK-DENPEDANUM
           MOVE    ZERO                TO  WRK-NYUKINRENNUM
           PERFORM UNTIL    FLG-SYUMEI     =   1
               IF      SMEI-DENPEDANUM     >   WRK-DENPEDANUM
                   MOVE    SMEI-DENPEDANUM     TO  WRK-DENPEDANUM
               END-IF
               IF      SMEI-NYUKINRENNUM   >   WRK-NYUKINRENNUM
                   MOVE    SMEI-NYUKINRENNUM   TO  WRK-NYUKINRENNUM
               END-IF
      *
      *        診療科変更
               IF      SYU-SRYKA       NOT =   SMEI-SRYKA
                   MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *
                   MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
                   MOVE    SYUMEI-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "K03 SYUMEI UPD ERR:"  MCP-RC
                               ",KEY:" SMEI-KEY
                   END-IF
               END-IF
      *
               MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SYUMEI-READ-SEC
           END-PERFORM
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           ADD     1                   TO  WRK-DENPEDANUM
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *入金連番
           ADD     1                   TO  WRK-NYUKINRENNUM
           MOVE    WRK-NYUKINRENNUM    TO  SMEI-NYUKINRENNUM
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPA-SYSYMD          TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *請求金額
           IF      SYU-SKYMONEY    NOT =   WRK-SKYMONEY
               COMPUTE SMEI-SKYMONEY       =   SYU-SKYMONEY
                                           -   WRK-SKYMONEY
           ELSE
               MOVE    ZERO                TO  SMEI-SKYMONEY
           END-IF
      *
      *入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD 
      *
      *    入金額がゼロの時、入金力なし
           IF      ORCSCGAIRAI-NYUKIN  =   ZERO
               CONTINUE
           ELSE
      *入返金区分
               MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *入返金額
               MOVE    ORCSCGAIRAI-NYUKIN  TO  SMEI-NYUHEN-MONEY
      *入金方法
               MOVE    "01"                TO  SMEI-NYUKIN-HOHO
           END-IF
      *状態区分
           MOVE    "8"                 TO  SMEI-JOUTAIKBN
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
               DISPLAY "K03 SINMEI-SYORI MCP-RC:" MCP-RC
                       ",KEY:" SMEI-KEY
           END-IF
      *
           .
       45001-SINMEI-SYORI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理（初期処理）
      *****************************************************************
       1003-JYURRK-SYORI1-SEC          SECTION.
      *
      *    訂正時、受診履歴等の削除
           IF     (SPA-SYORIFLG         =   1  )  AND
                  (SPA-OLD-SRYKA        =   SPA-SRYKA)
               GO      TO      1003-JYURRK-SYORI1-EXT
           END-IF
      *
      *    受診履歴の同日作成分カウント
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
           IF      SPA-DOUJI-RENNUM    NOT =   ZERO
               MOVE    SPA-DOUJI-RENNUM    TO  WRK-JYURRK-RENNUM
           END-IF
           MOVE    ZERO                TO  WRK-KAIKEI-RENNUM
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           IF      SPA-DOUJI-RENNUM    =   ZERO
               MOVE    "JYURRK-KEY4"       TO  WRK-PATH
           ELSE
               MOVE    SPA-DOUJI-RENNUM    TO  JYURRK-RENNUM
               MOVE    "JYURRK-KEY5"       TO  WRK-PATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF      SPA-DOUJI-RENNUM    =   ZERO
                   MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
               ELSE
                   MOVE    JYURRK-DOUJI-RENNUM TO
                                           WRK-JYURRK-DOUJI-RENNUM
               END-IF
      *
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    連番の設定
           IF      SPA-DOUJI-RENNUM    =   ZERO
               ADD     1                   TO  WRK-JYURRK-RENNUM
           END-IF
           IF      SPA-CONTKBN         =   "1"  OR  "2"
               ADD     1                   TO  WRK-JYURRK-DOUJI-RENNUM
           ELSE
               MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
           END-IF
      *    初期編集
           PERFORM 45001-JYURRK-SYOKI-SEC
      *
           .
       1003-JYURRK-SYORI1-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理（初期）
      *****************************************************************
       45001-JYURRK-SYOKI-SEC          SECTION.
      *    枝番
           ADD     1                   TO  WRK-JYURRK-EDANUM
      *
      *受診履歴レコードの作成
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    WRK-JYURRK-RENNUM   TO  JYURRK-RENNUM
           MOVE    WRK-JYURRK-DOUJI-RENNUM
                                       TO  JYURRK-DOUJI-RENNUM
           MOVE    WRK-JYURRK-EDANUM   TO  JYURRK-EDANUM
      *    会計連番
           COMPUTE JYURRK-KAIKEI-RENNUM
                                       =   WRK-KAIKEI-RENNUM
                                       +   1
      *
           MOVE    "1"                 TO  JYURRK-LASTKBN
      *
           MOVE    SPA-DRCD            TO  JYURRK-DRCD
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
      *
           MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
      *
      *    労災用に保険区分をセットする
           MOVE    SPA-HKNKBN          TO  JYURRK-HKNKBN
           IF      SPA-HKNKBN          =   "1"
               EVALUATE    SPA-RSI-HKNKBN
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
      *                 労災
                       MOVE    "1"                 TO  JYURRK-HKNKBN
                   WHEN    "4"
      *                 自賠責
                       MOVE    "2"                 TO  JYURRK-HKNKBN
               END-EVALUATE
           END-IF
      *
           MOVE    ZERO                TO  IDX-YAKUZAI
           .
       45001-JYURRK-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正時、受診履歴等の削除処理
      *****************************************************************
       1001-TEISEI-SYORI-SEC          SECTION.
      *
      *    伝票番号がゼロの時エラーとする
           IF      SPA-DENPNUM         =   ZERO
               MOVE    "1036"              TO  SPA-ERRCD
               GO      TO      1001-TEISEI-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-DEL-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
           MOVE    ZERO                TO  WRK-KAIKEI-RENNUM
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY6"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY6"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF      SPA-DOUJI-RENNUM    =   JYURRK-DOUJI-RENNUM
                   MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-RENNUM       TO  WRK-DEL-RENNUM
                   MOVE    JYURRK-DOUJI-RENNUM TO
                                               WRK-JYURRK-DOUJI-RENNUM
      *
      *            診療会計マスタの診療回数をクリアする
                   PERFORM 4500112-TEISEI-SRYACCT-SEC
      *
      *            最終フラグクリア
                   MOVE    ZERO                TO  JYURRK-LASTKBN
      *            会計連番
                   MOVE    JYURRK-KAIKEI-RENNUM
                                               TO  WRK-KAIKEI-RENNUM
      *
      *            訂正の時受診履歴を削除する
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               END-IF
      *
               MOVE    "JYURRK-KEY6"            TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "JYURRK-KEY6"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    訂正で診療科に変更がある時対象の科から連番の変更
           IF      SPA-OLD-SRYKA   NOT =   SPA-SRYKA
               PERFORM 4500111-TEISEI-JYURRK-HEN-SEC
           END-IF
      *
      *    初期編集
           IF      SPA-CONTKBN         =   "1" OR  "2"
               IF      WRK-JYURRK-DOUJI-RENNUM =   ZERO
                   ADD     1               TO  WRK-JYURRK-DOUJI-RENNUM
               END-IF
           ELSE
               MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
           END-IF
           MOVE    SPA-DENPNUM         TO  SYU-DENPNUM
           MOVE    SPA-DENPNUM         TO  WRK-DENPNUM
           PERFORM 45001-JYURRK-SYOKI-SEC
      *
           .
       1001-TEISEI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタの診療回数クリア処理
      *****************************************************************
       4500111-TEISEI-JYURRK-HEN-SEC          SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY4"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY4"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *
               MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
               IF     (JYURRK-EDANUM       =   1 )  AND
                     ((JYURRK-DOUJI-RENNUM     =   ZERO )  OR
                      (JYURRK-DOUJI-RENNUM NOT =
                                           WRK-JYURRK-DOUJI-RENNUM))
                   ADD     1               TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-DOUJI-RENNUM
                                           TO  WRK-JYURRK-DOUJI-RENNUM
               END-IF
               MOVE    WRK-JYURRK-RENNUM   TO  JYURRK-RENNUM
               IF     (WRK-KAIKEI-RENNUM   =   JYURRK-KAIKEI-RENNUM) AND
                      (WRK-DEL-RENNUM      =   JYURRK-EDANUM  )
                   ADD     1               TO  JYURRK-KAIKEI-RENNUM
               END-IF
      *
      *
               MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
      *        キー変換
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "JYURRK-UPD1"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *!       IF      MCP-RC          NOT =   ZERO
                    DISPLAY "K03 JYURRK UPD1 ERR:" MCP-RC
                    DISPLAY "K03  KEY:" JYURRK-KEY
                    DISPLAY "K03 UKEY:" JYURRK-UPKEY
      *!       END-IF
      *
               MOVE    "JYURRK-KEY4"            TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "JYURRK-KEY4"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       4500111-TEISEI-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタの診療回数クリア処理
      *****************************************************************
       4500112-TEISEI-SRYACCT-SEC          SECTION.
      *
      *    保険組合せを保存する
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HZN-HKNCOMBINUM
           MOVE    SPA-HKNKBN          TO  WRK-HZN-HKNKBN
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-HKNCOMBINUM
           MOVE    JYURRK-HKNKBN       TO  SPA-HKNKBN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   15  )  OR
                              (JYURRK-ZAINUM (IDX) =   ZERO )
      *
               INITIALIZE                      SRYACCT-REC
               MOVE    JYURRK-HOSPID       TO  ACCT-HOSPID
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  ACCT-PTID
               MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
               MOVE    JYURRK-ZAINUM (IDX) TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
                   PERFORM 930-SRYACCT-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
               PERFORM
                   UNTIL   (FLG-SRYACCT        =   1    ) OR
                           (FLG-OK             =   1    )
      *
                   MOVE    JYURRK-SRYYMD(7:2)      TO  IDX-2
                   EVALUATE    JYURRK-RENNUM
                       WHEN    1
                           MOVE    2               TO  IDX-1
                       WHEN    2
                           MOVE    3               TO  IDX-1
                       WHEN    OTHER
                           MOVE    4               TO  IDX-1
                   END-EVALUATE
      *
      *            算定履歴の削除処理
                   MOVE    ACCT-DAY (IDX-1 IDX-2)  TO  WRK-KAISU
                   PERFORM 4500113-TEISEI-SANTEI-SEC
      *
      *            剤回数　変更
                   COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                               -   ACCT-DAY
                                                       (IDX-1 IDX-2)
                   COMPUTE ACCT-DAY (1 IDX-2)  =   ACCT-DAY (1 IDX-2)
                                               -   ACCT-DAY
                                                       (IDX-1 IDX-2)
                   MOVE    ZERO                TO  ACCT-DAY
                                                       (IDX-1 IDX-2)
      *            回数がゼロの時、削除
                   IF      ACCT-ZAIKAISU       =   ZERO
      *
                       INITIALIZE                  SRYACT-REC
                       MOVE    ACCT-HOSPID         TO  SRY-HOSPID
                       MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
                       MOVE    ACCT-PTID           TO  SRY-PTID
                       MOVE    ACCT-SRYKA          TO  SRY-SRYKA
                       MOVE    ACCT-SRYYM          TO  SRY-SRYYM
                       MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
                       MOVE    SRYACT-REC          TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"          USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                       IF      MCP-RC          NOT =   ZERO
                            DISPLAY "K03 SRYACT DEL ERR:" MCP-RC
                       END-IF
      *
                       MOVE    SRYACCT-REC         TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"          USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                       IF      MCP-RC          NOT =   ZERO
                            DISPLAY "K03 SRYACCT DEL ERR:" MCP-RC
                       END-IF
                   ELSE
      *
                       MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                       MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                       MOVE    SRYACCT-REC         TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"          USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                       IF      MCP-RC          NOT =   ZERO
                            DISPLAY "K03 SRYACCT UPD ERR:" MCP-RC
                       END-IF
                   END-IF
      *
                   MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
                   PERFORM 930-SRYACCT-NEXT-SEC
               END-PERFORM
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-PERFORM
      *
      *    保険組合せを戻す
           MOVE    WRK-HZN-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
           MOVE    WRK-HZN-HKNKBN          TO  SPA-HKNKBN
      *
           .
       4500112-TEISEI-SRYACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴の回数クリア処理
      *****************************************************************
       4500113-TEISEI-SANTEI-SEC          SECTION.
      *
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL      (IDX         >   15  )  OR
      *                       (JYURRK-ZAINUM (IDX) =   ZERO )
      *
               INITIALIZE                      SRYACT-REC
               MOVE    JYURRK-HOSPID       TO  SRY-HOSPID
               MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  SRY-PTID
               MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM (IDX) TO  SRY-ZAINUM
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
                   PERFORM 915-SRYACT-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-SRYACT
               END-IF
               PERFORM
                   UNTIL   FLG-SRYACT          =   1
      *
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL      (IDY     >   5   )  OR
                                      (SRY-SRYCD (IDY) =   SPACE)
      *
      *            手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDY)(1:1)    =   "1"    )
      *                システム予約コード
                       OR (SRY-SRYCD (IDY)(1:3)    =   "099" )
                       MOVE    SRY-SRYCD (IDY)     TO  WRK-SRYCD
      *
                       MOVE    WRK-SRYCD           TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                算定用の診療コードへ変更
                       PERFORM 45020-SANTEI-SRYCD-SEC
                       IF      SRY-SRYCD (IDY) NOT =   WRK-SRYCD
                           MOVE    WRK-SRYCD           TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                       END-IF
      *
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 45001131-SANTEI-DEL-SEC
                       END-IF
                   END-IF
      *
                   END-PERFORM
      *
                   MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
                   PERFORM 915-SRYACT-NEXT-SEC
      *
               END-PERFORM
      *
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
      *****END-PERFORM
      *
           .
       4500113-TEISEI-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴回数減処理
      *****************************************************************
       45001131-SANTEI-DEL-SEC          SECTION.
      *
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "3"                 TO  SSANTEI-SYORI
           MOVE    "K03"               TO  SSANTEI-PG
           MOVE    WRK-SRYCD           TO  SSANTEI-SRYCD
           MOVE    SPA-PTID            TO  SSANTEI-PTID
           MOVE    SPA-SRYYMD          TO  SSANTEI-SRYYMD
           MOVE    WRK-KAISU           TO  SSANTEI-KAISU
      *    診療科
           MOVE    SRY-SRYKA           TO  SSANTEI-SRYKA
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
           .
       45001131-SANTEI-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    受診履歴作成処理（明細編集）
      *****************************************************************
       45001-JYURRK-SYORI2-SEC          SECTION.
      *
      *    診療区分領域の編集
           EVALUATE    LNK-WKSRY-SRYKBN (1)
               WHEN   "11"
               WHEN   "12"
               WHEN   "13"
               WHEN   "14"
                   MOVE    "01"        TO  JYURRK-SRYKBN1
               WHEN   "21"
                   MOVE    "01"        TO  JYURRK-SRYKBN2
               WHEN   "22"
                   MOVE    "01"        TO  JYURRK-SRYKBN3
               WHEN   "23"
                   MOVE    "01"        TO  JYURRK-SRYKBN4
               WHEN   "31"
               WHEN   "32"
               WHEN   "33"
                   MOVE    "01"        TO  JYURRK-SRYKBN5
               WHEN   "40"
                   MOVE    "01"        TO  JYURRK-SRYKBN6
               WHEN   "50"
                   MOVE    "01"        TO  JYURRK-SRYKBN7
               WHEN   "54"
                   MOVE    "01"        TO  JYURRK-SRYKBN8
               WHEN   "60"
                   MOVE    "01"        TO  JYURRK-SRYKBN9
               WHEN   "70"
                   MOVE    "01"        TO  JYURRK-SRYKBN10
               WHEN   "80"
                   MOVE    "01"        TO  JYURRK-SRYKBN11
      *****    WHEN   "93"
      *        WHEN   "95"
      *        WHEN   "96"
      *        WHEN   "99"
      *****        MOVE    "01"        TO  JYURRK-SRYKBN11
           END-EVALUATE
      *
      *    同一の剤の判定
           MOVE    ZERO                TO  FLG-ZAINUM
           PERFORM VARYING WRK-TBL-IDX     FROM    1   BY  1
                   UNTIL  (WRK-TBL-IDX     >   100 )  OR
                          (FLG-ZAINUM      =   1   )
               IF      WRK-TBL-ZAINUM (WRK-TBL-IDX)    =   WRK-ZAINUM
                   MOVE    1               TO  FLG-ZAINUM
               ELSE
                   IF      WRK-TBL-ZAINUM (WRK-TBL-IDX)    =   ZERO
                       MOVE    WRK-ZAINUM      TO  WRK-TBL-ZAINUM
                                                      (WRK-TBL-IDX)
                       MOVE    100             TO  WRK-TBL-IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-ZAINUM          =   ZERO
               ADD     1       TO    IDX-YAKUZAI
               IF      IDX-YAKUZAI   >    15
      *            受診履歴作成処理
                   PERFORM 45001-JYURRK-SYORI3-SEC
                   PERFORM 45001-JYURRK-SYOKI-SEC
      *
                   MOVE    1             TO  IDX-YAKUZAI
               END-IF
               MOVE    WRK-ZAINUM        TO  JYURRK-ZAINUM(IDX-YAKUZAI)
           END-IF
      *
      *    初診料履歴のみ算定時、初診日２へセット
           IF      SPA-SYOSIN-YMD  NOT =   SPACE
               MOVE    SPA-SYOSIN-YMD      TO  WRK-KARRK-SYOSINYMD2
           END-IF
      *    初診料算定日設定処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *
      *        診療科更新（初診料算定時、初診日２へセット
               IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"
                   MOVE    SYU-SRYYMD          TO  WRK-KARRK-SYOSINYMD2
               END-IF
      *
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDY) =   SPACE)
                   EVALUATE    LNK-WKSRY-SRYCD (IDX IDY)
      *                小児科外来診療料
                       WHEN    "113003510"
                       WHEN    "113003710"
                           MOVE    SYU-SRYYMD      TO
                                                   WRK-KARRK-SYOSINYMD2
      *                検診等より
                       WHEN    "820000003"
                       WHEN    "820000004"
                       WHEN    "820000005"
                       WHEN    "820000006"
                           IF      WRK-KARRK-SYOSINYMD2    =   SPACE
                               MOVE    SYU-SRYYMD  TO
                                                   WRK-KARRK-SYOSINYMD2
                               MOVE    1           TO  FLG-SYOSIN-DUMMY
                           END-IF
                   END-EVALUATE
               END-PERFORM
           END-PERFORM
      *
           .
       45001-JYURRK-SYORI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理（ファイル書き出し）
      *****************************************************************
       45001-JYURRK-SYORI3-SEC          SECTION.
      *
      *
      *    受診履歴連番を編集する
           MOVE    JYURRK-RENNUM       TO  ORCSCGAIRAI-JYURRK-RENNUM
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
               DISPLAY "K03 JYURRK-SYORI3 ERR:" MCP-RC 
                        ",KEY:" JYURRK-KEY "."
           END-IF
      *
           .
       45001-JYURRK-SYORI3-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科初期処理（初期処理）
      *****************************************************************
       45009-SRYKARRK-SYOKI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPID          TO  KARRK-HOSPID
           MOVE    SPA-PTID            TO  KARRK-PTID
           MOVE    SPA-SRYKA           TO  KARRK-SRYKA
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      FLG-SRYKARRK        =   ZERO
      *        最終受診日更新
               IF      KARRK-LASTYMD       <   SPA-SRYYMD
                   MOVE    SPA-SRYYMD      TO  KARRK-LASTYMD
               END-IF
           ELSE
      *        診療科新規作成
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                      SRYKARRK-REC
               MOVE    SPA-HOSPID          TO  KARRK-HOSPID
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    SPA-SRYKA           TO  KARRK-SRYKA
               MOVE    SPA-SRYYMD          TO  KARRK-SYOSINYMD1
               MOVE    SPA-SRYYMD          TO  KARRK-LASTYMD
      *        初回算定日があればその日
               IF     (SPA-SYOSIN-YMD  NOT =   SPACE  )  AND
                      (SPA-SRYYMD          >   SPA-SYOSIN-YMD )
                   MOVE    SPA-SYOSIN-YMD      TO  KARRK-SYOSINYMD1
               END-IF
           END-IF
           IF      WRK-KARRK-SYOSINYMD2    NOT =   SPACE
               MOVE    WRK-KARRK-SYOSINYMD2    TO  KARRK-SYOSINYMD2
           END-IF
           PERFORM 45009-SRYKARRK-OUT-SEC
      *
           .
       45009-SRYKARRK-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴作成終了処理
      *****************************************************************
       45009-SRYKARRK-OUT-SEC            SECTION.
      *
           IF      FLG-SRYKARRK           =   ZERO
      *
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
      *        更新
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SRYKARRK UPD ERR:"  MCP-RC
                           ",KEY:" KARRK-KEY  ";"
               END-IF
           ELSE
      *        新規
               MOVE    SMCNDATE-YMD        TO  KARRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SRYKARRK INS ERR:"  MCP-RC
                           ",KEY:" KARRK-KEY  ";"
               END-IF
           END-IF
      *
           .
       45009-SRYKARRK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター更新処理
      *****************************************************************
       4501-SRYACT-KOU-SEC            SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-SINKI
      *    診療会計マスター処理
           PERFORM 4501-SRYACCT-SYORI-SEC
      *
      *    診療コードがない時、処理をしない
           IF      WRK-MEISAISU        =   ZERO
               GO      TO      4501-SRYACT-KOU-EXT
           END-IF
      *
      *    診療行為マスター処理 
           IF      FLG-SINKI           =   1
               PERFORM 4501-SRYACT-SYORI-SEC
           END-IF
      *
      *    算定履歴更新処理
           PERFORM 4502-SANTEI-SYORI-SEC
           .
       4501-SRYACT-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療会計マスター処理
      *****************************************************************
       4501-SRYACCT-SYORI-SEC              SECTION.
      *
      *    診療コード計計算
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYKAISUKEI
           MOVE    ZERO                TO  WRK-SRYSURYOKEI
           MOVE    ZERO                TO  WRK-MEISAISU
           MOVE    ZERO                TO  WRK-SRYCDKEI
           INITIALIZE                      WRK-WKSRY-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *        剤点数計
               MOVE    LNK-WKSRY-ZAITENKEI(IDX)    TO  WRK-ZAITEN
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)    TO  WRK-SRYKAISUKEI
      *        手技料１
               MOVE    LNK-WKSRY-SYUTEN1  (IDX)    TO  WRK-SYUTEN1
      *        手技料２
               MOVE    LNK-WKSRY-SYUTEN2  (IDX)    TO  WRK-SYUTEN2
      *        薬剤
               MOVE    LNK-WKSRY-YKZTEN  (IDX)     TO  WRK-YKZTEN
      *        器材
               MOVE    LNK-WKSRY-KIZAITEN (IDX)    TO  WRK-KIZAITEN
      *        自費
               IF      LNK-WKSRY-JIHIMONEYTOTAL (IDX)  NOT =   ZERO
      *************MOVE    LNK-WKSRY-JIHIMONEY (IDX)  TO  WRK-JIHIMONEY
                   MOVE    LNK-WKSRY-JIHIMONEYTOTAL (IDX) 
                                                   TO  WRK-JIHIMONEY
               END-IF
      *
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDY) =   SPACE)
                   IF      LNK-WKSRY-SRYCD (IDX IDY)     NUMERIC
                       MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                               TO  WRK-SRYCD
                       ADD     WRK-SRYCD       TO  WRK-SRYCDKEI
                       ADD     LNK-WKSRY-SRYSURYO (IDX IDY)
                                               TO  WRK-SRYSURYOKEI
                       ADD     1               TO  WRK-MEISAISU
                   ELSE
                       IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =  "S"
                           ADD     1               TO  WRK-MEISAISU
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    剤点数がゼロの時、自費を点数へ
           IF      WRK-ZAITEN          =   ZERO
               MOVE   WRK-JIHIMONEY        TO  WRK-ZAITEN
           END-IF
      *
      *    診療コードがない時、処理をしない
           IF      WRK-MEISAISU        =   ZERO
               GO      TO      4501-SRYACCT-SYORI-EXT
           END-IF 
      *
      *    診療会計剤チェックサブ
           INITIALIZE                      ORCSCZAICHKAREA
           MOVE    "1"                     TO  ORCSCZAICHK-KBN
           MOVE    LNK-WKSRY-SRYKA(01)     TO  ORCSCZAICHK-SRYKA
           MOVE    LNK-WKSRY-SRYYMD(01)(1:6)
                                           TO  ORCSCZAICHK-SRYYM
           MOVE    LNK-WKSRY-SRYKBN(01)    TO  ORCSCZAICHK-SRYKBN
           MOVE    LNK-WKSRY-SRYSYUKBN(01) TO  ORCSCZAICHK-SRYSYUKBN
           MOVE    SPA-HKNCOMBINUM         TO  ORCSCZAICHK-HKNCOMBI
           MOVE    WRK-SRYCDKEI            TO  ORCSCZAICHK-SRYCDTOTAL
           MOVE    WRK-ZAITEN              TO  ORCSCZAICHK-ZAITEN
           MOVE    WRK-MEISAISU            TO  ORCSCZAICHK-MEISAISU
           MOVE    WRK-SRYSURYOKEI         TO  ORCSCZAICHK-SURYOUTOTAL
           MOVE    WRK-SRYKAISUKEI         TO  ORCSCZAICHK-ZAIKAISU
           MOVE    ZERO                    TO  IDX-ATO
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1        >   IDX-KOUI
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5          )  OR
                                  (IDX-ATO     >=  50         )  OR
                                  (LNK-WKSRY-SRYCD(IDX1 IDX2) =  SPACE)
                   ADD     1                   TO  IDX-ATO
                   MOVE    LNK-WKSRY-SRYCD (IDX1 IDX2)
                                               TO  ORCSCZAICHK-SRYCD
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX1 IDX2)
                                               TO  ORCSCZAICHK-SRYSURYO
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-SRYKAISU(IDX1 IDX2)
                                               TO  ORCSCZAICHK-SRYKAISU
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-INPUTCOMENT(IDX1 IDX2)
                                           TO  ORCSCZAICHK-INPUTCOMENT
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-INPUTCHI-G(IDX1 IDX2)
                                           TO  ORCSCZAICHK-INPUTCHI-G
                                                             (IDX-ATO)
               END-PERFORM
           END-PERFORM
      *
      *
           CALL    "ORCSCZAICHK"       USING   SPA-AREA
                                               ORCSCZAICHKAREA
           MOVE    ORCSCZAICHK-ZAINUM  TO  WRK-ZAINUM
           IF      WRK-ZAINUM          =   ZERO
      *        剤番号取得処理
               PERFORM 45013-ZAINUM-SET-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4501-SRYACCT-SYORI-EXT
               END-IF
           END-IF
      *    診療会計マスター更新用読み込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-WKSRY-HOSPID (01)   TO  ACCT-HOSPID
           MOVE    LNK-WKSRY-NYUGAIKBN(01) TO  ACCT-NYUGAIKBN
           MOVE    LNK-WKSRY-PTID   (01)   TO  ACCT-PTID
           MOVE    LNK-WKSRY-SRYKA  (01)   TO  ACCT-SRYKA
           MOVE    LNK-WKSRY-SRYYMD (01)(1:6)
                                           TO  ACCT-SRYYM
           MOVE    LNK-WKSRY-SRYKBN (01)   TO  ACCT-SRYKBN
           MOVE    WRK-ZAINUM              TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-SRYACCT         =   1
      *        診療会計マスター新規編集処理
               PERFORM 45015-SRYACCT-SRY-SEC
      *        診療会計マスター編集処理
               PERFORM 45014-SRYACCT-HEN-SEC
      *
               MOVE    SMCNDATE-YMD        TO  ACCT-CREYMD
               MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
               MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "SYRACCT INS ERR:" MCP-RC
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF
      *        新規のときのみ、診療行為マスターを追加する
               MOVE    1                   TO  FLG-SINKI
           ELSE
      *        診療会計マスター編集処理
               PERFORM 45014-SRYACCT-HEN-SEC
      *
               MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
               MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "SYRACCT UPDTE ERR:" MCP-RC
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF   
               MOVE    ZERO                TO  FLG-SINKI
           END-IF
      *
           .
      *
       4501-SRYACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤番号取得処理
      *****************************************************************
       45013-ZAINUM-SET-SEC          SECTION.
      *
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "0002"          TO  SPA-ERRCD
               DISPLAY "K03 ZAINUM-SET ERR:" SPA-PTID
               GO      TO      45013-ZAINUM-SET-EXT
           END-IF
      *
      *    最大剤番号
           IF      PTINF-MAXZAINUM     =   ALL "9"
               MOVE    ZERO                TO  PTINF-MAXZAINUM
           END-IF
           ADD     1                   TO  PTINF-MAXZAINUM
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "PTINF UPDTE ERR:"   MCP-RC
               MOVE    "0005"              TO  SPA-ERRCD
           END-IF 
      *
           .
      *
       45013-ZAINUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスター新規編集処理
      *****************************************************************
       45015-SRYACCT-SRY-SEC          SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    LNK-WKSRY-HOSPID (01)   TO  ACCT-HOSPID
           MOVE    LNK-WKSRY-NYUGAIKBN(01) TO  ACCT-NYUGAIKBN
           MOVE    LNK-WKSRY-PTID   (01)   TO  ACCT-PTID
           MOVE    LNK-WKSRY-SRYKA  (01)   TO  ACCT-SRYKA
           MOVE    LNK-WKSRY-SRYYMD (01)(1:6)
                                           TO  ACCT-SRYYM
           MOVE    LNK-WKSRY-SRYKBN (01)   TO  ACCT-SRYKBN
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
           MOVE    SPA-HKNCOMBINUM     TO  ACCT-HKNCOMBI
      *    剤点数
           MOVE    WRK-ZAITEN          TO  ACCT-ZAITEN
      *    診療コード計
           MOVE    WRK-SRYCDKEI        TO  ACCT-SRYCDTOTAL
      *    数量計
           MOVE    WRK-SRYSURYOKEI     TO  ACCT-SURYOUTOTAL
      *    明細数
           MOVE    WRK-MEISAISU        TO  ACCT-MEISAISU
      *    手技点数１
           MOVE    WRK-SYUTEN1         TO  ACCT-SYUTEN1
      *    手技点数２
           MOVE    WRK-SYUTEN2         TO  ACCT-SYUTEN2
      *    薬剤点数
           MOVE    WRK-YKZTEN          TO  ACCT-YKZTEN
      *    器材点数
           MOVE    WRK-KIZAITEN        TO  ACCT-KIZAITEN
           .
      *
       45015-SRYACCT-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスター編集処理
      *****************************************************************
       45014-SRYACCT-HEN-SEC          SECTION.
      *
      *    剤回数
           ADD     WRK-SRYKAISUKEI     TO  ACCT-ZAIKAISU
      *    回数セット
           MOVE    SPA-SRYYMD(7:2)     TO  IDX2
           ADD     WRK-SRYKAISUKEI     TO  ACCT-DAY  (1 IDX2)
      *
           COMPUTE IDX1                =   WRK-JYURRK-RENNUM
                                       +   1
           IF      IDX1                >   4
               MOVE    4                   TO  IDX1
           END-IF
           ADD     WRK-SRYKAISUKEI     TO  ACCT-DAY  (IDX1 IDX2)
           .
      *
       45014-SRYACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター処理
      *****************************************************************
       4501-SRYACT-SYORI-SEC          SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-MEIBAN
      *    新規分のみ、作成する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
               INITIALIZE                          SRYACT-REC
               MOVE    LNK-WKSRY-HOSPID  (IDX) TO  SRY-HOSPID
               MOVE    LNK-WKSRY-NYUGAIKBN (IDX)   TO  SRY-NYUGAIKBN
               MOVE    LNK-WKSRY-PTID   (IDX)  TO  SRY-PTID
               MOVE    LNK-WKSRY-SRYKA  (IDX)  TO  SRY-SRYKA
               MOVE    LNK-WKSRY-SRYYMD (IDX)(1:6)
                                               TO  SRY-SRYYM
      *        剤番号
               MOVE    WRK-ZAINUM              TO  SRY-ZAINUM
      *        連番号
               MOVE    IDX                     TO  SRY-RENNUM
               MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                               TO  SRY-SRYSYUKBN
               MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  SRY-SRYKBN
               MOVE    LNK-WKSRY-JIHIMONEYTOTAL (IDX)
                                               TO  SRY-JIHIMONEYTOTAL
               PERFORM VARYING IDX-SIN     FROM    1   BY  1
                       UNTIL   IDX-SIN     >   5
                   MOVE    LNK-WKSRY-SRYCD (IDX IDX-SIN)
                                               TO  SRY-SRYCD (IDX-SIN)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX IDX-SIN)
                                               TO  SRY-SRYSURYO
                                                            (IDX-SIN)
                   MOVE    LNK-WKSRY-SRYKAISU(IDX IDX-SIN)
                                               TO  SRY-SRYKAISU
                                                            (IDX-SIN)
                   MOVE    LNK-WKSRY-AUTOKBN(IDX IDX-SIN)
                                               TO  SRY-AUTOKBN
                                                            (IDX-SIN)
                   MOVE    LNK-WKSRY-JIHIMONEY (IDX IDX-SIN)
                                               TO  SRY-JIHIMONEY
                                                            (IDX-SIN)
      *            患者コメント作成
                   IF      LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:1)  =   "8"
                       PERFORM 45011-PTCOM-SYROI-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    SMCNDATE-YMD        TO  SRY-CREYMD
               MOVE    SMCNDATE-YMD        TO  SRY-UPYMD
               MOVE    SMCNDATE-HMS        TO  SRY-UPHMS
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "SRYACT-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SRYACT INS ERR:" SRY-KEY  "."
                   DISPLAY "MCP-RC:" MCP-RC ":" 
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
      *
           END-PERFORM
           .
      *
       4501-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント作成処理
      *****************************************************************
       45011-PTCOM-SYROI-SEC           SECTION.
      *
      *    入力があれば作成
           IF     (LNK-WKSRY-INPUTCHI-G (IDX IDX-SIN)  =  SPACE ) AND
                  (LNK-WKSRY-INPUTCOMENT(IDX IDX-SIN)  =  SPACE )
               GO      TO      45011-PTCOM-SYROI-EXT
           END-IF
      *
           ADD     1                   TO  WRK-MEIBAN
           INITIALIZE                  PTCOM-REC
      *医療機関ＩＤ
           MOVE    SRY-HOSPID          TO  PTCOM-HOSPID
      *患者ＩＤ
           MOVE    SRY-PTID            TO  PTCOM-PTID
      *剤番号
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
      *診療コード
           MOVE    LNK-WKSRY-SRYCD (IDX IDX-SIN)
                                       TO  PTCOM-SRYCD
      *コメント枝番番号
           MOVE    WRK-MEIBAN          TO  PTCOM-RENNUM
      *入力コメント
           MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDX-SIN)
                                       TO  PTCOM-INPUTCOMENT
      *入力値
           MOVE    LNK-WKSRY-INPUTCHI-G  (IDX IDX-SIN)
                                       TO  PTCOM-INPUTCHI-AREA
      *
           MOVE    SMCNDATE-YMD        TO  PTCOM-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTCOM-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCOM-UPHMS
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K03 PTCOM INS ERR:" PTCOM-KEY
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *
      *    明細番号
           MOVE    WRK-MEIBAN          TO  SRY-INPUTNUM  (IDX-SIN)
      *
           .
       45011-PTCOM-SYROI-EXT.
           EXIT.
      *
      *****************************************************************
      *     算定履歴更新処理
      *****************************************************************
       4502-SANTEI-SYORI-SEC              SECTION.
      *
      *    包括まとめは登録しない
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      4502-SANTEI-SYORI-EXT
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDY) =   SPACE)
      *            手技料の時、算定歴を判定する
                   IF    ((LNK-WKSRY-SRYCD (IDX IDY)(1:1)  =   "1" )
      *                システム予約コード
                       OR (LNK-WKSRY-SRYCD (IDX IDY)(1:3)  =   "099"))
      *            自費の時、履歴なし
                   AND    (LNK-WKSRY-SRYKBN(IDX)   NOT =   "95"  AND
                                                           "96"  )
                       MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                                   TO  WRK-SRYCD
      *
                       MOVE    WRK-SRYCD           TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                算定用の診療コードへ変更
                       PERFORM 45020-SANTEI-SRYCD-SEC
                       IF      LNK-WKSRY-SRYCD (IDX IDY)
                                               NOT =   WRK-SRYCD
                           MOVE    WRK-SRYCD           TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                       END-IF
      *
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
      *                    特定薬剤治療管理の初回日があれば編集
                           MOVE    1               TO  FLG-TOKTEI
                           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                                   TO  WRK-KAISU
                           PERFORM 45021-SANTEI-UPD-SEC
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       4502-SANTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定用の診療コードへ変更処理
      *****************************************************************
       45020-SANTEI-SRYCD-SEC              SECTION.
      *
      *    包括逓減区分判定
           EVALUATE    TNS-HOUKATUTEIGENKBN
      *        頭部
               WHEN    201
                   MOVE    "099700101"         TO  WRK-SRYCD
      *        躯幹
               WHEN    202
                   MOVE    "099700102"         TO  WRK-SRYCD
      *        四肢
               WHEN    203
                   MOVE    "099700103"         TO  WRK-SRYCD
           END-EVALUATE
           .
       45020-SANTEI-SRYCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       45021-SANTEI-UPD-SEC              SECTION.
      *
           IF      WRK-SRYYMD          =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           END-IF
      *
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "K03"               TO  SSANTEI-PG
           MOVE    WRK-SRYCD           TO  SSANTEI-SRYCD
           MOVE    WRK-SRYYMD          TO  SSANTEI-SRYYMD
           MOVE    SPA-PTID            TO  SSANTEI-PTID
           IF     (FLG-TOKTEI          =   1   )  OR
                  (SPA-TOKTEI-YMD  NOT =   SPACE)
               MOVE    SPA-TOKTEI-YMD  TO  SSANTEI-SYOKAIYMD
           END-IF
           MOVE    WRK-KAISU           TO  SSANTEI-KAISU
      *    初診料算定日
           IF      KARRK-SYOSINYMD2    =   SPACE
               MOVE    KARRK-SYOSINYMD1    TO  SSANTEI-SYOYMD
           ELSE
               MOVE    KARRK-SYOSINYMD2    TO  SSANTEI-SYOYMD
           END-IF
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
           .
       45021-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為マスタ−削除処理
      *****************************************************************
       45002-WKSRYACT-DEL-SEC              SECTION.
      *
      *    作成済みのワーク診療行為マスタ−を削除する
           MOVE    SPACE               TO  WKSRYACT-REC
           MOVE    SPA-HOSPID          TO  WKSRY-HOSPID
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
      *****MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
           MOVE    SPA-WKSRY-HKNCOMBI  TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "WKSRYACT-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    IF      MCP-RC          NOT =   ZERO
      *D       DISPLAY "K03 WKSRYACT DEL ERR:" MCP-RC
      *D       MOVE    "0003"          TO  SPA-ERRCD
      *    END-IF
      *
           .
       45002-WKSRYACT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診内容ファイル更新処理
      *****************************************************************
       45003-UKETUKE-SEC            SECTION.
      *
      *
           ACCEPT  WRK-TIME11           FROM  TIME
           MOVE    WRK-TIME1(1:4)       TO  WRK-TIME2(1:4)
      *    受診内容ファイルを特定する
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPID          TO  UKE-HOSPID
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-PTID            TO  UKE-PTID
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "UKETUKE-KEY2"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "UKETUKE-KEY2"      TO  ORC-DBPATH
               PERFORM 976-UKETUKE-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
           PERFORM
                   UNTIL       FLG-UKETUKE     =    1
               IF      (SPA-SRYKA          =   UKE-SRYKA) AND
                       (UKE-UKEID      NOT =   ZERO     )
                   IF      UKE-KAIKEITIME      =   ZERO
                       MOVE    WRK-TIME22          TO  UKE-KAIKEITIME
      *
                       MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                       MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                       MOVE    UKETUKE-REC         TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "UKETUKE-KEY"       TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"          USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                       MOVE    1                   TO  FLG-UKETUKE
                   END-IF
               END-IF
      *
               IF      FLG-UKETUKE         =   ZERO
                   MOVE    "UKETUKE-KEY2"      TO  ORC-DBPATH
                   PERFORM 976-UKETUKE-NEXT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "UKETUKE-KEY2"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       45003-UKETUKE-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       300-DELETE-SYORI-SEC             SECTION.
      *
      *    削除処理
           PERFORM 4500121-TEISEI-SRYACTDEL-SEC
      *
      *    診療科履歴変更
           IF      SPA-HKNCOMBINUM     =   "9999"
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKARRKAREA
               MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
      *
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
           END-IF
      *
           .
       300-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4500121-TEISEI-SRYACTDEL-SEC          SECTION.
      *
      *    受診履歴の連番をすべて削除する
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    SPA-RENNUM          TO  JYURRK-RENNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               MOVE    ZERO                TO  WRK-JYURRK-RENNUM
               MOVE    ZERO                TO  WRK-JYURRK-EDANUM
               MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
               MOVE    ZERO                TO  WRK-KAIKEI-RENNUM
      *
      *        診療会計マスタの診療回数をクリアする
               PERFORM 4500112-TEISEI-SRYACCT-SEC
      *        会計連番
               MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
               MOVE    JYURRK-DOUJI-RENNUM TO  WRK-JYURRK-DOUJI-RENNUM
               MOVE    JYURRK-KAIKEI-RENNUM
                                           TO  WRK-KAIKEI-RENNUM
      *
      *        受診履歴・収納削除
               PERFORM 4500114-TEISEI-JYURRKDEL-SEC
      *
               MOVE    "JYURRK-KEY5"            TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
      *
      *    受診履歴の連番変更する
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY9"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY9"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    SPACE               TO  TBL-JYURRK-AREA
           MOVE    ZERO                TO  IDX 
           MOVE    ZERO                TO  WRK-RENNUM-KEY
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF      JYURRK-RENNUM       =   WRK-RENNUM-KEY
                   CONTINUE
               ELSE
                   ADD     1                   TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-RENNUM       TO  WRK-RENNUM-KEY
               END-IF
      *
               IF      JYURRK-RENNUM       =   WRK-JYURRK-RENNUM
                   CONTINUE
               ELSE
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
      *
                   MOVE    WRK-JYURRK-RENNUM   TO  JYURRK-RENNUM
      *
                   MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
                   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
      *            診療会計修正
                   IF      JYURRK-LASTKBN      =   1
                       PERFORM 4500115-TEISEI-SRYACTUPD-SEC
                   END-IF
      *
               END-IF
      *
               MOVE    "JYURRK-KEY9"            TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
      *
           .
       450012-TEISEI-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴削除処理
      *****************************************************************
       4500114-TEISEI-JYURRKDEL-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-KAIKEI-RENNUM
      *
               MOVE    ZERO                TO  FLG-JYURRK
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       FLG-JYURRK  NOT =   ZERO
                   MOVE    SPACE               TO  JYURRK-REC
                   INITIALIZE                      JYURRK-REC
                   MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
                   MOVE    SPA-PTID            TO  JYURRK-PTID
                   MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
                   MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
                   MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
                   MOVE    SPA-RENNUM          TO  JYURRK-RENNUM
                   MOVE    WRK-JYURRK-DOUJI-RENNUM
                                               TO  JYURRK-DOUJI-RENNUM
                   MOVE    IDX                 TO  JYURRK-KAIKEI-RENNUM
                   MOVE    IDY                 TO  JYURRK-EDANUM
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC              =   ZERO
                       MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
                       PERFORM 970-JYURRK-NEXT-SEC
                   ELSE
                       MOVE    1                   TO  FLG-JYURRK
                   END-IF
                   IF      FLG-JYURRK          =   ZERO
      *                収納削除
                       PERFORM 45001141-SYUNOU-DELETE-SEC
      *
      *                受診履歴削除
                       MOVE    JYURRK-REC          TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"         USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
      *
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-JYURRK
      *
           .
       4500114-TEISEI-JYURRKDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター削除処理
      *****************************************************************
       45001141-SYUNOU-DELETE-SEC         SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
           END-IF
           IF      FLG-SYUNOU          =   ZERO
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *        収納明細削除
               MOVE    SPACE               TO  SYUMEI-REC
               INITIALIZE                      SYUMEI-REC
               MOVE    SYU-HOSPID          TO  SMEI-HOSPID
               MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
               MOVE    SYU-PTID            TO  SMEI-PTID
               MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "SYUMEI-DEL2"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF
      *
           .
       45001141-SYUNOU-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計修正処理
      *****************************************************************
       4500115-TEISEI-SRYACTUPD-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   15  )  OR
                              (JYURRK-ZAINUM (IDX)   =   ZERO)
      *
               INITIALIZE                      SRYACCT-REC
               MOVE    JYURRK-HOSPID       TO  ACCT-HOSPID
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  ACCT-PTID
               MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
               MOVE    JYURRK-ZAINUM (IDX) TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
                   PERFORM 930-SRYACCT-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
               PERFORM
                   UNTIL   (FLG-SRYACCT        =   1    ) OR
                           (FLG-OK             =   1    )
      *
                   MOVE    JYURRK-SRYYMD(7:2)      TO  IDX-2
                   EVALUATE    JYURRK-RENNUM
                       WHEN    1
                           MOVE    2               TO  IDX-1
                       WHEN    2
                           MOVE    3               TO  IDX-1
                       WHEN    OTHER
                           MOVE    4               TO  IDX-1
                   END-EVALUATE
                       EVALUATE    WRK-RENNUM-KEY
                           WHEN    1
                               MOVE    2               TO  IDX-3
                           WHEN    2
                               MOVE    3               TO  IDX-3
                           WHEN    OTHER
                               MOVE    4               TO  IDX-3
                       END-EVALUATE
      *                剤回数　変更
                       MOVE    ACCT-DAY (IDX-3 IDX-2)
                                              TO  ACCT-DAY (IDX-1 IDX-2)
                       MOVE    ZERO           TO  ACCT-DAY (IDX-3 IDX-2)
      *
      *
                       MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                       MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                       MOVE    SRYACCT-REC       TO  MCPDATA-REC
                       MOVE    "DBUPDATE"        TO  MCP-FUNC
                       MOVE    "SRYACCT-KEY"     TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"       USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
      *
                       IF      MCP-RC          NOT =   ZERO
                            DISPLAY "K03 SRYACCT UPD ERR:" MCP-RC
                       END-IF
      *
                   MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
                   PERFORM 930-SRYACCT-NEXT-SEC
               END-PERFORM
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-PERFORM
      *
           .
       4500115-TEISEI-SRYACTUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診算定日のみ設定処理
      *****************************************************************
       400-SYOSIN-SYORI-SEC         SECTION.
      *
      *    初診料算定のみの時初診料を算定する
           MOVE    SPA-SYOSIN-YMD      TO  WRK-SRYYMD
           MOVE    SPA-SYOSIN-SRYCD    TO  WRK-SRYCD
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               MOVE    ZERO            TO  FLG-TOKTEI
               MOVE    1               TO  WRK-KAISU
      *        算定履歴作成
               PERFORM 45021-SANTEI-UPD-SEC
      *        診療科履歴処理
               PERFORM 45099-SRYKARRK-ADD-SEC
           END-IF
      *
           .
       400-SYOSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴のみ登録処理
      *****************************************************************
       45099-SRYKARRK-ADD-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPID          TO  KARRK-HOSPID
           MOVE    SPA-PTID            TO  KARRK-PTID
           MOVE    SPA-SRYKA           TO  KARRK-SRYKA
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    新規であること
           IF      FLG-SRYKARRK           =   1
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                      SRYKARRK-REC
               MOVE    SPA-HOSPID          TO  KARRK-HOSPID
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    SPA-SRYKA           TO  KARRK-SRYKA
               MOVE    SPA-SYOSIN-YMD      TO  KARRK-SYOSINYMD1
               MOVE    SPA-SYOSIN-YMD      TO  KARRK-SYOSINYMD2
               MOVE    SPA-SYOSIN-YMD      TO  KARRK-LASTYMD
           END-IF
      *    診療科履歴作成終了処理
           PERFORM 45009-SRYKARRK-OUT-SEC
      *
           .
       45099-SRYKARRK-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       915-SRYACT-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       915-SRYACT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
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
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
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
       940-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスター読込
      *****************************************************************
       950-PTCOM-READ-SEC         SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTCOM-REC
                   MOVE    ZERO               TO  FLG-PTCOM
               ELSE
                   MOVE    1                  TO  FLG-PTCOM
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTCOM
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
      *920-SANTEI-READ-SEC         SECTION.
      *
      *    MOVE    "DBFETCH"           TO  MCP-FUNC
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    MCPDATA-REC         TO  SANTEI-REC
      *        MOVE    ZERO                TO  FLG-SANTEI
      *    ELSE
      *        INITIALIZE                      SANTEI-REC
      *        MOVE    1                   TO  FLG-SANTEI
      *    END-IF
      *
      *    .
      *920-SANTEI-READ-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    受診内容データ読み込み
      *****************************************************************
       976-UKETUKE-NEXT-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  UKETUKE-REC 
               MOVE    ZERO                TO  FLG-UKETUKE
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       976-UKETUKE-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    収納明細マスター読込
      *****************************************************************
       900-SYUMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUMEI-REC
               MOVE    ZERO                TO  FLG-SYUMEI
           ELSE
               INITIALIZE                      SYUMEI-REC
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
       980-PARA-READ-SEC         SECTION.
      *
           READ    PARA-FILE 
               AT  END
                   MOVE    1           TO  FLG-PARA
               NOT AT  END 
                   MOVE    ZERO        TO  FLG-PARA
           END-READ
      *
           .
       980-PARA-READ-EXT.
           EXIT.
      *
