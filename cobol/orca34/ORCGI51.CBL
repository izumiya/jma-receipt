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
       PROGRAM-ID.                 ORCGI51.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 退院時仮計算
      *  コンポーネント名  : 退院時仮計算
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/10/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    02.12.16  前回患者ボタン追加
      *  01.00.02    NACL-太田    03.01.23  食事の集計方法修正
      *  01.00.03    NACL-太田    03.01.24  保険組合せリストの表示を修正
      *  01.00.04    NACL-太田    03.02.05  自院歴作成データを考慮する
      *  01.00.05    NACL-太田    03.03.25  算定開始日も入力可とする
      *                                     入院の保険適用外金額欄追加
      *  01.00.06    NACL-太田    03.04.02  負担金計算サブ引数修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    パラメタファイル名
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    退院時仮計算共通パラメタ
           COPY    "I5COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "I5SCR-SPA".
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
           03  FLG-DBRC            PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-ENTRYCHK        PIC 9(01).
           03  FLG-ORCSDAY                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(05).
           03  IDX11               PIC 9(05).
           03  IDX12               PIC 9(05).
           03  IDX-LST             PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TUKISU          PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                    PIC X(10). 
           03  WRK-HENYMDG                PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
      *
      *****03  WRK-Z9                      PIC Z,ZZZ,ZZZ.
           03  WRK-Z99                     PIC --,---,--9.
           03  WRK-Z9                      PIC --,---,---.
           03  WRK-Z92                     PIC --,---,---.
           03  WRK-ZZ9                     PIC ZZ9.
      *
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
      *    パス名
           03  WRK-DBPATH                  PIC X(20).
           03  WRK-CALC-YMD1.
               05  WRK-CALC-YY1            PIC 9(04).
               05  WRK-CALC-MM1            PIC 9(02).
               05  WRK-CALC-DD1            PIC 9(02).
      *
           03  WRK-CALC-YMD2.
               05  WRK-CALC-YY2            PIC 9(04).
               05  WRK-CALC-MM2            PIC 9(02).
               05  WRK-CALC-DD2            PIC 9(02).
           03  WRK-AGE-YY                  PIC S9(04).
           03  WRK-AGE-MM                  PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z               PIC ZZ9.
               05  WRK-AGE-MEI             PIC X(02).
               05                          PIC X(01).
           03  WRK-AGE-M-R     REDEFINES   WRK-AGE-G.
               05  WRK-AGE-M-Z             PIC Z9.
               05  WRK-AGE-M-MEI           PIC X(04).
           03  WRK-BIRTHDAY                PIC X(08).
           03  WRK-HKNCOMBI                PIC X(100).
           03  WRK-FTNRATE                 PIC X(05).
           03  WRK-HKNKBN-MEI              PIC X(02).
           03  WRK-I5IDMSG                 PIC X(80).
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-KBNCD                   PIC X(08).
           03  WRK-KJNYMD                  PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-SYU-SUM-IDX             PIC 9(05).
           03  WRK-LST-IDX                 PIC 9(05).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-SKYSTYM                 PIC X(08).
           03  WRK-SKYEDYMD                PIC X(08).
           03  WRK-NOW-SKYSTYMD            PIC X(08).
           03  WRK-MAE-SKYEDYMD            PIC X(08).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-EDIDX                   PIC 9(05).
           03  WRK-SANTEISTYMD             PIC X(08).
           03  WRK-SANTEIEDYMD             PIC X(08).
      *
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *    収納ワーク
       01  WKSYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //WKSYU-//.
      *    収納ワーク
       01  SUMSYUNOU-REC.
         02 SUMSYUNOU-CNT     PIC 9(05).
         02 SUMSYUNOU-OCC     OCCURS 13.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //SUMSYU-//.
      *    収納合計ワーク
       01  TTLSYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //TTLSYU-//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK0041.INC".
      *
      *    診療科コード
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1013.INC".
      *    ドクター
           COPY    "CPSK1010.INC".
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者定期請求履歴
        01 PTTEIKIRRK-REC.
           COPY     "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    負担金計算サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           COPY    "I51.INC".
           COPY    "I5ERR.INC".
           COPY    "I5ID1.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I5FREE
           MOVE    SPA-WORK        TO  SPA-I5KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           MOVE    SPA-I5KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I5FREE      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I5ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "I51"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-I5FREE
                   MOVE    SPA-WORK        TO  SPA-I5KYOUTU
                   MOVE    1               TO  SPA-GMN-I51-CUR
               END-IF
      *
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   PERFORM 3001-P97-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
           WHEN    "I5ID1"
      *        確認画面より
               PERFORM 3001-I5ID1-SET-SEC
               MOVE   SPACE        TO  SPA-I5IDCD
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-I5IDCD
      *
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-GMN-I51-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｉ５ＩＤ１）ＯＫ処理
      *****************************************************************
       3001-I5ID1-SET-SEC         SECTION.
      *
           IF    ( SPA-I5ID1-FLG   =   "OK" )
               EVALUATE    SPA-I5IDCD
      *        仮計算処理
               WHEN "3001"
                   PERFORM 451-KARIKEISAN-SYORI-SEC
               END-EVALUATE
           END-IF
           .
       3001-I5ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索画面（Ｐ９７）からの戻り値設定処理
      *****************************************************************
       3001-P97-SET-SEC                 SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *
      *    人が選ばれなかった場合、
      *    同一の人が選択された場合は、画面内容の変更は行わない
           IF    ( WRK-SPA-PTID    =   ZERO )
             OR  ( WRK-SPA-PTID    =   SPA-NAI-I51-PTID )
               GO  TO  3001-P97-SET-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-I51
           INITIALIZE                      SPA-I51
      *
           MOVE    WRK-SPA-PTID        TO  SPA-NAI-I51-PTID
           MOVE    WRK-SPA-PTNUM       TO  SPA-GMN-I51-PTNUM
      *
           PERFORM 3001-PTINF-SELECT-SEC
      *
           .
       3001-P97-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       3001-PTINF-SELECT-SEC              SECTION.
      *
      *    患者マスタ検索
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-NAI-I51-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
      *
      *        患者情報設定
               PERFORM 4101-PTINF-SET-SEC
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *
           .
       3001-PTINF-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    "1"             TO  SPA-NYUGAIKBN
      *
           MOVE    SPACE           TO  SPA-I51
                                       SPA-I5KYOUTU
           INITIALIZE                  SPA-I51
                                       SPA-I5KYOUTU
            .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  I51
      *
      *    患者番号
           MOVE    SPA-GMN-I51-PTNUM   TO  I51-PTNUM
      *    カナ氏名
           MOVE    SPA-GMN-I51-KANANAME
                                       TO  I51-KANANAME
      *    氏名
           MOVE    SPA-GMN-I51-NAME    TO  I51-NAME
      *    性別
           MOVE    SPA-GMN-I51-SEX     TO  I51-SEX
      *    保険組み合わせ
           MOVE    SPA-GMN-I51-HKNCOMBI
                                       TO  I51-HKNCOMBI
      *    負担率
           MOVE    SPA-GMN-I51-FTNRATE TO  I51-FTNRATE
      *    生年月日
           MOVE    SPA-GMN-I51-BIRTHDAY
                                       TO  I51-BIRTHDAY
      *    入院科
           MOVE    SPA-GMN-I51-NYUINKA TO  I51-SRYKA
      *    年齢
           MOVE    SPA-GMN-I51-AGE     TO  I51-NENREI
      *    入院日
           MOVE    SPA-GMN-I51-NYUINYMD
                                       TO  I51-NYUINYMD
      *    算定開始日
           MOVE    SPA-GMN-I51-SANTEISTYMD
                                       TO  I51-SANTEISTYMD
      *    算定終了日
           MOVE    SPA-GMN-I51-SANTEIEDYMD
                                       TO  I51-SANTEIEDYMD
      *    診療料
           MOVE    SPA-GMN-I51-HKNRYO (01)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SSUHKNTEN
      *    指導料
           MOVE    SPA-GMN-I51-HKNRYO (02)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SDOHKNTEN
      *    在宅料
           MOVE    SPA-GMN-I51-HKNRYO (03)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-ZTKHKNTEN
      *    投薬料
           MOVE    SPA-GMN-I51-HKNRYO (04)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-TYKHKNTEN
      *    注射料
           MOVE    SPA-GMN-I51-HKNRYO (05)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-CSYHKNTEN
      *    処置料
           MOVE    SPA-GMN-I51-HKNRYO (06)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SYCHKNTEN
      *    手術料
           MOVE    SPA-GMN-I51-HKNRYO (07)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SJTHKNTEN
      *    検査料
           MOVE    SPA-GMN-I51-HKNRYO (08)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-KNSHKNTEN
      *    Ｘ線料
           MOVE    SPA-GMN-I51-HKNRYO (09)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-GZUHKNTEN
      *    その他
           MOVE    SPA-GMN-I51-HKNRYO (10)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-ETCHKNTEN
      *    入院料
           MOVE    SPA-GMN-I51-HKNRYO (11)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-NYUHKNTEN
      *    合計点数
           MOVE    SPA-GMN-I51-HKNRYO (12)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-TOTALHKNTEN
      *    保険適用外
      *    診療料
           MOVE    SPA-GMN-I51-TGMONEY (01)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SUUTGMONEY
      *    指導料
           MOVE    SPA-GMN-I51-TGMONEY (02)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SDOTGMONEY
      *    在宅料
           MOVE    SPA-GMN-I51-TGMONEY (03)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-ZTKTGMONEY
      *    投薬料
           MOVE    SPA-GMN-I51-TGMONEY (04)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-TYKTGMONEY
      *    注射料
           MOVE    SPA-GMN-I51-TGMONEY (05)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-CSYTGMONEY
      *    処置料
           MOVE    SPA-GMN-I51-TGMONEY (06)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SYCTGMONEY
      *    手術料
           MOVE    SPA-GMN-I51-TGMONEY (07)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SJTGMONEY
      *    検査料
           MOVE    SPA-GMN-I51-TGMONEY (08)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-KNSTGMONEY
      *    Ｘ線料
           MOVE    SPA-GMN-I51-TGMONEY (09)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-GZUTGMONEY
      *    その他
           MOVE    SPA-GMN-I51-TGMONEY (10)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-ETCTGMONEY
      *
      *    入院料
           MOVE    SPA-GMN-I51-TGMONEY (11)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-NYUTGMONEY
      *
      *    自費
           MOVE    SPA-GMN-I51-JIHI1   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHI1
           MOVE    SPA-GMN-I51-JIHI2   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHI2
           MOVE    SPA-GMN-I51-JIHI3   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHI3
           MOVE    SPA-GMN-I51-JIHI4   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHI4
           MOVE    SPA-GMN-I51-JIHI5   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHI5
      *    自費計
           MOVE    SPA-GMN-I51-JIHIKEI TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHIGOK
      *    自費（消費税あり）
           MOVE    SPA-GMN-I51-JIHITAX1
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHITAX1
           MOVE    SPA-GMN-I51-JIHITAX2
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHITAX2
           MOVE    SPA-GMN-I51-JIHITAX3
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHITAX3
           MOVE    SPA-GMN-I51-JIHITAX4
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHITAX4
           MOVE    SPA-GMN-I51-JIHITAX5
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHITAX5
      *    自費計
           MOVE    SPA-GMN-I51-JIHITAXKEI  TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHITAXGOK
      *    自費税
           MOVE    SPA-GMN-I51-JIHIZEI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-JIHITAX
      *    食事療養費
           MOVE    SPA-GMN-I51-RYOYOHI-SKJ TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-RYOYOHI-SKJ
      *    食事負担額
           MOVE    SPA-GMN-I51-SKYMONEY-SKJ
                                           TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-SKYMONEY-SKJ
      *    老人一部負担金
           MOVE    SPA-GMN-I51-ROUFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-RJNFTN
      *    公費一部負担金
           MOVE    SPA-GMN-I51-KOHFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-KOHFTN
      *    一部負担金計
           MOVE    SPA-GMN-I51-ICHIFTN     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-FTNTOTAL
      *    保険適用分
           MOVE    SPA-GMN-I51-HKNTEKI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-HKNTEKMONEY
      *    保険適用外
           MOVE    SPA-GMN-I51-HKNGAI      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-HKNTEKGAI
      *    今回請求額
           MOVE    SPA-GMN-I51-SEIKYU  TO  WRK-Z99
           MOVE    WRK-Z99             TO  I51-SKYMONEY
      *
      *    前回未収（外来）
           MOVE    SPA-GMN-I51-ZENMISYU-GAI
                                       TO  WRK-Z92
           MOVE    WRK-Z92             TO  I51-MISYUZAN-GAI
      *
      *    前回未収（入院）
           MOVE    SPA-GMN-I51-ZENMISYU-NYU
                                       TO  WRK-Z92
           MOVE    WRK-Z92             TO  I51-MISYUZAN-NYU
      *    合計請求額
           MOVE    SPA-GMN-I51-SEIGOK  TO  WRK-Z99
           MOVE    WRK-Z99             TO  I51-GOKSKY
      *
      *
      *    自費名称
           MOVE    SPA-GMN-I51-JIHIMSG(1)
                                       TO  I51-JIHIMSG1
           MOVE    SPA-GMN-I51-JIHIMSG(2)
                                       TO  I51-JIHIMSG2
           MOVE    SPA-GMN-I51-JIHIMSG(3)
                                       TO  I51-JIHIMSG3
           MOVE    SPA-GMN-I51-JIHIMSG(4)
                                       TO  I51-JIHIMSG4
           MOVE    SPA-GMN-I51-JIHIMSG(5)
                                       TO  I51-JIHIMSG5
      *
      *    労災初診
           MOVE    SPA-GMN-I51-ROUSYOSIN
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-ROUSYOSIN
      *    労災再診
           MOVE    SPA-GMN-I51-ROUSAISIN
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-ROUSAISIN
      *    労災指導
           MOVE    SPA-GMN-I51-ROUSIDOU
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-ROUSIDOU
      *    労災その他
           MOVE    SPA-GMN-I51-ROUETC  TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-ROUETC
      *    室料差額
           MOVE    SPA-GMN-I51-RMSAGAKU
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I51-RMSAGAKU
      *    選択番号
           MOVE    SPA-GMN-I51-SELNUM  TO  I51-SELNUM
      *
      *    保険組み合わせリスト
           MOVE    SPA-GMN-I51-HKNCOMBILST-CNT
                                       TO  I51-HKNCOMBILST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I51-HKNCOMBILST-CNT )
      *
               MOVE    SPA-NAI-I51-TSELNUM (IDX1)
                                       TO  I51-TSELNUM (IDX1)
      *
               MOVE    SPA-GMN-I51-THKNCOMBI (IDX1)
                                       TO  I51-THKNCOMBI (IDX1)
               IF    ( SPA-GMN-I51-HKNCOMBILST-SEL =   IDX1 )
                   MOVE    "T"         TO  I51-HKNCOMBILST-SELECT (IDX1)
               ELSE
                   MOVE    "F"         TO  I51-HKNCOMBILST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
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
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I51-CUR     =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-I51-CUR
           WHEN    001
               MOVE    "PTNUM"         TO  MCP-WIDGET
           WHEN    002
               MOVE    "SANTEISTYMD"   TO  MCP-WIDGET
           WHEN    003
               MOVE    "SANTEIEDYMD"   TO  MCP-WIDGET
           WHEN    101
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "PTNUM"
               MOVE    001             TO  SPA-GMN-I51-CUR
           WHEN    "SANTEISTYMD"
               MOVE    002             TO  SPA-GMN-I51-CUR
           WHEN    "SANTEIEDYMD"
               MOVE    003             TO  SPA-GMN-I51-CUR
           WHEN    "SELNUM"
               MOVE    101             TO  SPA-GMN-I51-CUR
           END-EVALUATE
      *
      *
      *    次カーソル位置を設定
           EVALUATE    MCP-WIDGET
           WHEN    "SANTEIEDYMD"
               MOVE    212             TO  SPA-GMN-I51-CUR
           WHEN    "SELNUM"
               MOVE    101             TO  SPA-GMN-I51-CUR
           WHEN    OTHER
               COMPUTE SPA-GMN-I51-CUR =   SPA-GMN-I51-CUR
                                           +   1
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
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
               PERFORM 210-BACK
      *    前回患者
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 420-MAEPTINF-SET-SEC
      *    選択
           WHEN    "CLICKED"       ALSO    "B04"
               MOVE    101           TO    SPA-GMN-I51-CUR
      *    氏名検索
           WHEN    "CLICKED"       ALSO    "B09"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-KENSAKU-SEC
      *    仮計算
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 450-KARIKEISAN-SEC
           END-EVALUATE
          .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-I51-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    患者番号チェック
           WHEN    "ACTIVATE"      ALSO    "PTNUM"
               PERFORM 4100-PTNUM-CHK-SEC
      *    選択番号チェック
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN        ANY         ALSO    "HKNCOMBILST"
               PERFORM 4101-HKNCOMBILST-SEL-SEC
           WHEN        OTHER
               MOVE    ZERO        TO      SPA-GMN-I51-CUR
           END-EVALUATE
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴リスト選択処理
      *****************************************************************
       4101-HKNCOMBILST-SEL-SEC        SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO TO   4101-HKNCOMBILST-SEL-EXT
           END-IF
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   SPA-GMN-I51-HKNCOMBILST-CNT )
      *
               IF    ( I51-HKNCOMBILST-SELECT (IDX1)   =   "T"  )
                   MOVE    IDX1        TO  SPA-GMN-I51-HKNCOMBILST-SEL
                   MOVE    SPA-GMN-I51-TSELNUM (IDX1)
                                       TO  SPA-GMN-I51-SELNUM
      *
                   MOVE    SPA-NAI-I51-TSYUNOUIDX (SPA-GMN-I51-SELNUM)
                                       TO  WRK-LST-IDX
                   PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
                   MOVE    SPA-GMN-I51-HKNCOMBILST-CNT
                                           TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           MOVE    101                 TO  SPA-GMN-I51-CUR
           .
      *
       4101-HKNCOMBILST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      *    計算日
           MOVE    I51-SANTEISTYMD   TO  SPA-GMN-I51-SANTEISTYMD
           MOVE    I51-SANTEIEDYMD   TO  SPA-GMN-I51-SANTEIEDYMD
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    患者番号チェック処理
           IF    ( SPA-GMN-I51-PTNUM   =    SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           IF    ( SPA-NAI-I51-PTNUM-ERR
                               NOT =   SPACE )
               MOVE    SPA-NAI-I51-PTNUM-ERR
                                   TO  SPA-ERRCD
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    算定開始日チェック処理
           PERFORM 4100-SANTEISTYMD-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    算定終了日チェック処理
           PERFORM 4100-SANTEIEDYMD-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4100-PTNUM-CHK-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-I51
                                       SPA-I5KYOUTU
           INITIALIZE                  SPA-I51
                                       SPA-I5KYOUTU
      *
           MOVE    1               TO  SPA-GMN-I51-CUR
      *
           MOVE    I51-PTNUM       TO  SPA-GMN-I51-PTNUM
      *
      *    患者番号が入力されていること
           IF    ( SPA-GMN-I51-PTNUM    =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
                                       SPA-NAI-I51-PTNUM-ERR
               GO  TO  4100-PTNUM-CHK-EXT
           END-IF
      *
      *    患者番号変換サブ呼び出し
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-GMN-I51-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM    TO  SPA-GMN-I51-PTNUM
                   MOVE    SPTNUM-PTID     TO  SPA-NAI-I51-PTID
               WHEN   10
                   MOVE    "0002"          TO  SPA-ERRCD
                                               SPA-NAI-I51-PTNUM-ERR
                   GO  TO  4100-PTNUM-CHK-EXT
               WHEN    20
      *            氏名検索へ
                   MOVE    SPACE           TO  LINKAREA

                   MOVE    SPA-KYOUTU      TO  LNK-COMMON
                   MOVE    SPA-I5FREE      TO  LNK-SCRSPA
      *
                   MOVE    "I51"           TO  SPA-MOTOPG
                   MOVE    "P97"           TO  SPA-SAKIPG
                   MOVE    ZERO            TO  SPA-PTID
      *            患者氏名セット
                   MOVE    SPA-GMN-I51-PTNUM
                                           TO  SPA-NAME
                   MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"        TO  MCP-PUTTYPE
                   MOVE    "P97"           TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1               TO  FLG-END
                   GO  TO  4100-PTNUM-CHK-EXT
               WHEN    OTHER
                   MOVE    "0002"          TO  SPA-ERRCD
                                               SPA-NAI-I51-PTNUM-ERR
                   GO  TO  4100-PTNUM-CHK-EXT
           END-EVALUATE
      *
      *    患者マスタ検索
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPID      TO  PTINF-HOSPID
           MOVE    SPA-NAI-I51-PTID
                                   TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
      *
           MOVE    "PTINF-KEY"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
      *
      *        患者情報設定
               PERFORM 4101-PTINF-SET-SEC
      *
           ELSE
               MOVE    "0002"      TO  SPA-ERRCD
                                       SPA-NAI-I51-PTNUM-ERR
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "PTINF-KEY"     TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    3               TO  SPA-GMN-I51-CUR
      *
           .
       4100-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報設定処理
      *****************************************************************
       4101-PTINF-SET-SEC              SECTION.
      *
           MOVE    MCPDATA-REC     TO  PTINF-REC
      *    患者ＩＤ
           MOVE    PTINF-PTID      TO  SPA-NAI-I51-PTID
      *    患者氏名
           MOVE    PTINF-NAME      TO  SPA-GMN-I51-NAME
      *    患者カナ氏名
           MOVE    PTINF-KANANAME  TO  SPA-GMN-I51-KANANAME
      *    患者性別
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"        TO  SPA-GMN-I51-SEX
           WHEN    "2"
               MOVE    "女"        TO  SPA-GMN-I51-SEX
           END-EVALUATE
      *    患者生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-BIRTHDAY
      *    患者年齢
           MOVE    PTINF-BIRTHDAY  TO  WRK-BIRTHDAY
           PERFORM 4101-AGE-CALC-SEC
      *
           MOVE    WRK-AGE-G       TO  SPA-GMN-I51-AGE
      *
      *    入院履歴マスタより患者の入院情報を取得
           MOVE    ZERO            TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID      TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I51-PTID
                                   TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY3"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    1           TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "PTNYUINRRK-KEY3"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    存在しない場合はエラー
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    "0003"      TO  SPA-ERRCD
                                       SPA-NAI-I51-PTNUM-ERR
               GO  TO  4101-PTINF-SET-EXT
           END-IF
      *
      *    入院中でない場合はエラー
           IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" )
            AND  ( PTNYUINRRK-JTIKBN   NOT =   "6" )
            AND  ( PTNYUINRRK-TAIINYMD     =   "99999999"  )
               CONTINUE
           ELSE
               MOVE    "0003"      TO  SPA-ERRCD
                                       SPA-NAI-I51-PTNUM-ERR
               GO  TO  4101-PTINF-SET-EXT
           END-IF
      *
           INITIALIZE  SYSKANRI-REC
           MOVE    "1005"          TO  WRK-KANRICD
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  WRK-KBNCD
           MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
           PERFORM 4101-SYSKANRI-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME
                                   TO  SPA-GMN-I51-NYUINKA
           END-IF
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-PTINF-SET-EXT
           END-IF
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-NYUINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I51-NYUINYMD
      *
      *
      *    算定開始日
      *    前回定期請求日の翌日か入院日を設定
           PERFORM 4101-SKYSTYMD-GET-SEC
           MOVE    WRK-NOW-SKYSTYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-PTINF-SET-EXT
           END-IF
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-SANTEISTYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I51-SANTEISTYMD
      *
      *    算定終了日
           IF    ( SPA-SYSYMD      >   SPA-NAI-I51-SANTEISTYMD )
               MOVE    SPA-SYSYMD  TO  WRK-YMD
           ELSE
               MOVE    SPA-NAI-I51-SANTEISTYMD
                                   TO  WRK-YMD
           END-IF
      *
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-PTINF-SET-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-SANTEIEDYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I51-SANTEIEDYMD
      *
      *    保険組合せ
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  COMB-PTID
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "HKNCOMBI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    1           TO  FLG-HKNCOMBI
           ELSE
               MOVE    MCPDATA-REC
                                   TO  HKNCOMBI-REC
           END-IF
      *
           MOVE    "HKNCOMBI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-HKNCOMBI        =   ZERO )
      *        保険組み合わせ情報設定処理
               PERFORM 4101-HKNCOMBI-SET-SEC
               MOVE    WRK-HKNCOMBI    TO  SPA-GMN-I51-HKNCOMBI
               MOVE    WRK-FTNRATE     TO  SPA-GMN-I51-FTNRATE
           END-IF
      *
           MOVE    SPA-GMN-I51-PTNUM   TO  SPA-LAST-PTNUM
           MOVE    SPA-NAI-I51-PTID    TO  SPA-LAST-PTID
      *
           .
      *
       4101-PTINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求開始日取得処理
      *****************************************************************
       4101-SKYSTYMD-GET-SEC           SECTION.
      *
      *    前回定期請求終了日取得処理
           PERFORM 4101-MAE-TEIKI-SKYEDYMD-GET-SEC
           IF    ( WRK-MAE-SKYEDYMD    NOT =   SPACE )
               MOVE    WRK-MAE-SKYEDYMD
                                   TO  WRK-YMD
               MOVE    "1"         TO  WRK-ZOGENPTN
               MOVE    1           TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD
                                   TO  WRK-NOW-SKYSTYMD
               IF    ( PTNYUINRRK-TEIKI-SEIKYUKBN  =   "2" )
                   IF   ( PTNYUINRRK-NYUINYMD  (1 : 6)
                       <  WRK-NOW-SKYSTYMD     (1 : 6) )
                       MOVE    "01"    TO  WRK-NOW-SKYSTYMD (7 : 2)
                   ELSE
                       MOVE    PTNYUINRRK-NYUINYMD
                                       TO  WRK-NOW-SKYSTYMD
                   END-IF
               END-IF
           ELSE
               MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-NOW-SKYSTYMD
           END-IF
      *
           .
       4101-SKYSTYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    前回定期請求終了日取得処理
      *****************************************************************
       4101-MAE-TEIKI-SKYEDYMD-GET-SEC SECTION.
      *
           MOVE    SPACE               TO  WRK-MAE-SKYEDYMD
      *
           INITIALIZE  PTTEIKIRRK-REC
      *
           MOVE    SPA-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  PTTEIKIRRK-PTID
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "PTTEIKIRRK-KEY3"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
      *        現在入院分の定期請求データかチェック
               IF    ( PTNYUINRRK-NYUINYMD
                                      <=   PTTEIKIRRK-SKYEDYMD )
                   MOVE    PTTEIKIRRK-SKYEDYMD
                                       TO  WRK-MAE-SKYEDYMD
               END-IF
           END-IF
      *
           MOVE    "PTTEIKIRRK-KEY3"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
      *
       4101-MAE-TEIKI-SKYEDYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       4101-SYSKANRI-SEL-SEC       SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    WRK-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-KBNCD       TO  SYS-KBNCD
           MOVE    WRK-KJNYMD      TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               CONTINUE
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4101-SYSKANRI-SEL-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       4101-AGE-CALC-SEC               SECTION.
      *
           MOVE    WRK-BIRTHDAY        TO  WRK-CALC-YMD1
           MOVE    SPA-SYSYMD          TO  WRK-CALC-YMD2
      *
           COMPUTE WRK-AGE-YY          =   WRK-CALC-YY2
                                       -   WRK-CALC-YY1
           IF    ( WRK-CALC-YMD1(5:4)  >   WRK-CALC-YMD2(5:4) )
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       -   1
               COMPUTE WRK-AGE-MM      =  (WRK-CALC-MM2    +   12 )
                                       -   WRK-CALC-MM1
           ELSE
               COMPUTE WRK-AGE-MM      =   WRK-CALC-MM2
                                       -   WRK-CALC-MM1
           END-IF
      *
           IF    ( WRK-CALC-DD1        >   WRK-CALC-DD2 )
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       -   1
           END-IF
      *
      *    画面表示用
           IF    ( WRK-AGE-YY          =   ZERO )
               MOVE    WRK-AGE-MM          TO  WRK-AGE-M-Z
               MOVE    "ヶ月"              TO  WRK-AGE-M-MEI
           ELSE
               MOVE    WRK-AGE-YY          TO  WRK-AGE-Z
               MOVE    "才"                TO  WRK-AGE-MEI
           END-IF
           .
      *
       4101-AGE-CALC-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせ情報設定処理
      *****************************************************************
       4101-HKNCOMBI-SET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
                                           WRK-FTNRATE
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
           END-STRING
      *
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
               END-STRING
           END-IF
      *
      *    労災の時、コメントの追加
           IF    ( COMB-HKNNUM         =   "971" OR "973")
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    COMB-PTID           TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "PTRSIINF-KEY"      TO  WRK-DBPATH
               PERFORM 900-TBL-SELECT-SEC
               IF    ( FLG-DBRC            =   ZERO )
      *
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
      *
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   MOVE    SPACE               TO  WRK-HKNKBN-MEI
                   EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                        MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                        MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                        MOVE    "ア"        TO  WRK-HKNKBN-MEI
                   END-EVALUATE
                   IF    ( PTRSI-COMMENT   =   SPACE )
                       MOVE    SPACE       TO  WRK-HKNCOMBI
                       STRING COMB-SYU-TANSEIDONAME
                                               DELIMITED   BY  SPACE
                              " "              DELIMITED   BY  SIZE
                              WRK-HKNKBN-MEI   DELIMITED   BY  SPACE
                              INTO              WRK-HKNCOMBI
                       END-STRING
      *
                   ELSE
                       STRING  COMB-SYU-TANSEIDONAME
                                               DELIMITED   BY  SPACE
                           " "                 DELIMITED   BY  SIZE
                           WRK-HKNKBN-MEI      DELIMITED   BY  SPACE
                           "（"                DELIMITED   BY  SIZE
                           PTRSI-COMMENT       DELIMITED   BY  SPACE
                           "）"                DELIMITED   BY  SIZE
                           INTO                WRK-HKNCOMBI
                       END-STRING
                   END-IF
               END-IF
      *        患者保険情報クローズ
               MOVE    "PTRSIINF-KEY"      TO  WRK-DBPATH
               PERFORM 900-TBL-CLOSE-SEC
           END-IF
      *
           MOVE    COMB-NYU-KFTNRATE       TO  WRK-ZZ9
           MOVE    WRK-ZZ9                 TO  WRK-FTNRATE (1 : 3)
           MOVE    "%"                     TO  WRK-FTNRATE (4 : )
           .
      *
       4101-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    101                 TO  SPA-GMN-I51-CUR
           MOVE    I51-SELNUM          TO  SPA-GMN-I51-SELNUM
      *
           IF    ( SPA-GMN-I51-SELNUM  =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I51-SELNUM  >   SPA-GMN-I51-HKNCOMBILST-CNT )
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I51-SELNUM  TO  SPA-GMN-I51-HKNCOMBILST-SEL
      *
           MOVE    SPA-NAI-I51-TSYUNOUIDX (SPA-GMN-I51-SELNUM)
                                       TO  WRK-LST-IDX
           PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
           MOVE    101                 TO  SPA-GMN-I51-CUR
      *
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定開始日チェック処理
      *****************************************************************
       4100-SANTEISTYMD-CHK-SEC        SECTION.
      *
           MOVE    SPA-NAI-I51-SANTEISTYMD
                                   TO  WRK-SANTEISTYMD
           MOVE    SPACE           TO  SPA-NAI-I51-SANTEISTYMD
      *
           IF    ( SPA-GMN-I51-SANTEISTYMD
                                   =   SPACE )
               MOVE    WRK-SANTEISTYMD TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-SANTEISTYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I51-SANTEISTYMD
               GO  TO  4100-SANTEISTYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I51-SANTEISTYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    WRK-SANTEISTYMD TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-SANTEISTYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I51-SANTEISTYMD
               MOVE    2               TO  SPA-GMN-I51-CUR
               GO  TO  4100-SANTEISTYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-SANTEISTYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I51-SANTEISTYMD
      *
           .
       4100-SANTEISTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定終了日チェック処理
      *****************************************************************
       4100-SANTEIEDYMD-CHK-SEC        SECTION.
      *
           MOVE    SPA-NAI-I51-SANTEIEDYMD
                                   TO  WRK-SANTEIEDYMD
           MOVE    SPACE           TO  SPA-NAI-I51-SANTEIEDYMD
      *
           IF    ( SPA-GMN-I51-SANTEIEDYMD
                                   =   SPACE )
               MOVE    WRK-SANTEIEDYMD TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-SANTEIEDYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I51-SANTEIEDYMD
               GO  TO  4100-SANTEIEDYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I51-SANTEIEDYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    WRK-SANTEIEDYMD TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-SANTEIEDYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I51-SANTEIEDYMD
               MOVE    3           TO  SPA-GMN-I51-CUR
               GO  TO  4100-SANTEIEDYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I51-SANTEIEDYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I51-SANTEIEDYMD
      *
           .
       4100-SANTEIEDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "I51"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索処理
      *****************************************************************
       480-KENSAKU-SEC                 SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I5FREE          TO  LNK-SCRSPA
      *
           MOVE    ZERO                TO  SPA-PTID
      *
           MOVE    "I51"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
      *
       480-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    前回患者設定処理
      *****************************************************************
       420-MAEPTINF-SET-SEC            SECTION.
      *
      *    前回患者が設定されているばあは前回患者を表示する
           IF    ( SPA-LAST-PTNUM  NOT =   SPACE )
               MOVE    SPA-LAST-PTID   TO  SPA-NAI-I51-PTID
               MOVE    SPA-LAST-PTNUM  TO  SPA-GMN-I51-PTNUM
               PERFORM 4101-PTINF-SELECT-SEC 
           END-IF
      *
           .
      *
       420-MAEPTINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       4101-PTINF-SELECT-SEC              SECTION.
      *
      *    患者マスタ検索
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-NAI-I51-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
      *
      *        患者情報設定
               PERFORM 4101-PTINF-SET-SEC
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *
           .
       4101-PTINF-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    仮計算　処理
      *****************************************************************
       450-KARIKEISAN-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-KARIKEISAN-EXT
           END-IF
      *
           PERFORM 4801-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-KARIKEISAN-EXT
           END-IF
      *
           MOVE    "3001"          TO  SPA-I5IDCD
      *
           .
       450-KARIKEISAN-EXT.
           EXIT.
      *
           .
      *
      *****************************************************************
      *    仮計算　処理
      *****************************************************************
       451-KARIKEISAN-SYORI-SEC        SECTION.
      *
      *    負担金計算処理
           PERFORM 490-FTNKIN-CALC-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  451-KARIKEISAN-SYORI-EXT
           END-IF
      *
      *
      *    請求データ内部領域を初期化
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I5-SYUNOU-CNT )
                     OR  ( IDX1    >   15 )
               MOVE    SPA-I5-SYUNOU-DAT (IDX1)
                                       TO  WKSYUNOU-REC
               MOVE    WKSYU-SKYMONEY
                                       TO  SPA-NAI-I51-SEIKYU
                                                   (IDX1)
                                           SPA-NAI-I51-SEIKYU-KON
                                                   (IDX1)
           END-PERFORM
      *
      *    未収データ取得処理
           PERFORM 3100-MISYU-DAT-EDIT-SEC
      *
      *    保険組み合わせリスト編集処理
           PERFORM 3100-HKNCOMBILST-EDIT-SEC
      *
           MOVE    SPA-NAI-I51-TSYUNOUIDX(1)
                                   TO  WRK-LST-IDX
      *
      *    請求データ編集処理
           PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
           .
       451-KARIKEISAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担金計算処理
      *****************************************************************
       490-FTNKIN-CALC-SEC             SECTION.
      *
      *    通算日数を求める
           MOVE    SPA-NAI-I51-SANTEISTYMD
                                   TO  WRK-STYMD
           MOVE    SPA-NAI-I51-SANTEIEDYMD
                                   TO  WRK-EDYMD
           PERFORM 5002-KIKAN-CALC-SEC
           IF    ( FLG-ORCSDAY     NOT =   ZERO )
               MOVE    "2001"      TO  SPA-ERRCD
               GO  TO  490-FTNKIN-CALC-EXT
           END-IF
      *
           MOVE    SPACE           TO  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
      *
           INITIALIZE                  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
      *
           MOVE    SPA-HOSPID      TO  LNK-S02-HOSPID
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    SPA-NAI-I51-PTID
                                   TO  LNK-S02-PTID
           MOVE    WRK-NISSU1      TO  LNK-S02-NYUIN-TSUSAN-NISSU
      *
           MOVE    SPA-NAI-I51-SANTEISTYMD
                                   TO  LNK-S02-SKYSTYMD
           MOVE    SPA-NAI-I51-SANTEIEDYMD
                                   TO  LNK-S02-SKYEDYMD
           MOVE    SPA-SYSYMD      TO  LNK-S02-SYSYMD
           DISPLAY "LNK-S02-HOSPID             = " LNK-S02-HOSPID
           DISPLAY "LNK-S02-NYUGAIKBN          = " LNK-S02-NYUGAIKBN
           DISPLAY "LNK-S02-PTID               = " LNK-S02-PTID
           DISPLAY "LNK-S02-NYUIN-TSUSAN-NISSU = "
                    LNK-S02-NYUIN-TSUSAN-NISSU
           DISPLAY "LNK-S02-SKYSTYMD           = " LNK-S02-SKYSTYMD
           DISPLAY "LNK-S02-SKYEDYMD           = " LNK-S02-SKYEDYMD
           CALL    "ORCSNYUFTN"    USING
                                   SPA-AREA
                                   LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
           DISPLAY "LNK-S02-RC = " LNK-S02-RC
           IF    ( LNK-S02-RC      NOT = ZERO )
               EVALUATE LNK-S02-RC
               WHEN    13
                   MOVE    "2002"      TO  SPA-ERRCD
                   GO  TO  490-FTNKIN-CALC-EXT
               WHEN    OTHER
                   MOVE    "2001"      TO  SPA-ERRCD
                   GO  TO  490-FTNKIN-CALC-EXT
               END-EVALUATE
           END-IF

      *
      *    収納データ退避処理
           PERFORM 490-SYUNOU-TAIHI-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-FTNKIN-CALC-EXT
           END-IF
      *
           .
       490-FTNKIN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    収納データ退避処理
      *****************************************************************
       490-SYUNOU-TAIHI-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  WRK-SKYSTYM
           MOVE    SPACE           TO  SUMSYUNOU-REC
                                       TTLSYUNOU-REC
           INITIALIZE                  SUMSYUNOU-REC
                                       TTLSYUNOU-REC
                                       CNT-TUKISU
      *
           PERFORM VARYING IDX11   FROM    1   BY  1
                   UNTIL ( IDX11           >   10 )
                   OR    ( SUMSYUNOU-CNT    >=  13 )
                   OR    ( LNK-SYU-SKYSTYMD (IDX11)    =   SPACE )
      *
               IF    ( LNK-SYU-SKYSTYMD (IDX11) (1 : 6)
                                   NOT =   WRK-SKYSTYM )
      *
                   MOVE    LNK-SYU-SKYSTYMD (IDX11) (1 : 6)
                                   TO      WRK-SKYSTYM
                   COMPUTE SUMSYUNOU-CNT
                                       =   SUMSYUNOU-CNT +   1
                   MOVE    SUMSYUNOU-CNT
                                   TO  WRK-SYU-SUM-IDX
      *
                   MOVE    1       TO    SPA-I5-SYUNOU-TTLDAT-FLG
                                                 (WRK-SYU-SUM-IDX)
      *
                   MOVE    LNK-SYU-SKYSTYMD (IDX11)
                                   TO   WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG (1 : 6)
                           TO SPA-I5-SYUNOU-YM-W (WRK-SYU-SUM-IDX)
                   MOVE    WRK-SYMD    (1 : 6)
                           TO SPA-I5-SYUNOU-YM-S (WRK-SYU-SUM-IDX)
      *
                   MOVE    ZERO    TO    SPA-I5-SYUNOU-HKNCOMBI-SU
                                                 (WRK-SYU-SUM-IDX)
      *
                   COMPUTE CNT-TUKISU  =   CNT-TUKISU  +   1
      *
               END-IF
      *
               IF    ( SUMSYUNOU-CNT <   13 )
                   COMPUTE SUMSYUNOU-CNT    =   SUMSYUNOU-CNT +   1
      *
                   MOVE    0       TO    SPA-I5-SYUNOU-TTLDAT-FLG
                                                 (SUMSYUNOU-CNT)
      *
                   MOVE    SPA-I5-SYUNOU-YM-W (WRK-SYU-SUM-IDX)
                                       TO  SPA-I5-SYUNOU-YM-W
                                                 (SUMSYUNOU-CNT)
                   MOVE    SPA-I5-SYUNOU-YM-S (WRK-SYU-SUM-IDX)
                                       TO  SPA-I5-SYUNOU-YM-S
                                                 (SUMSYUNOU-CNT)
      *
      *            保険組み合わせ名称取得
                   INITIALIZE              HKNCOMBI-REC
                   MOVE    ZERO            TO   FLG-HKNCOMBI
      *
                   MOVE    SPA-HOSPID      TO   COMB-HOSPID
                   MOVE    SPA-NAI-I51-PTID
                                           TO   COMB-PTID
                   MOVE    LNK-SYU-HKNCOMBINUM (IDX11)
                                           TO   COMB-HKNCOMBINUM
                   MOVE    HKNCOMBI-REC    TO   MCPDATA-REC
                   MOVE    "HKNCOMBI-KEY"  TO   WRK-DBPATH
                   PERFORM 900-TBL-SELECT-SEC 
                   IF    ( FLG-DBRC    =   ZERO )
                       MOVE    MCPDATA-REC
                                           TO   HKNCOMBI-REC
                   ELSE
                       MOVE    1           TO   FLG-HKNCOMBI
                   END-IF
      *
                   MOVE    "HKNCOMBI-KEY"  TO   WRK-DBPATH
                   PERFORM 900-TBL-CLOSE-SEC
      *
                   IF    ( FLG-HKNCOMBI    =    ZERO )
                       PERFORM 4101-HKNCOMBI-SET-SEC
                       MOVE    WRK-HKNCOMBI
                                           TO  SPA-I5-SYUNOU-HKNCOMBI
                                                        (SUMSYUNOU-CNT)
                   END-IF
      *
                   COMPUTE SPA-I5-SYUNOU-HKNCOMBI-SU (WRK-SYU-SUM-IDX)
                       =   SPA-I5-SYUNOU-HKNCOMBI-SU (WRK-SYU-SUM-IDX)
                       +   1
      *
                   MOVE    LNK-SYUNOU-REC (IDX11)
                                       TO  SUMSYUNOU-OCC (SUMSYUNOU-CNT)
      *            保険料
                   PERFORM VARYING     IDX12   FROM    1   BY  1
                           UNTIL       IDX12   >   11
                     COMPUTE SUMSYU-HKNTEN   (WRK-SYU-SUM-IDX IDX12)
                         =   SUMSYU-HKNTEN   (WRK-SYU-SUM-IDX IDX12)
                         +   LNK-SYU-HKNTEN (IDX11 IDX12)
                     COMPUTE TTLSYU-HKNTEN (IDX12)
                         =   TTLSYU-HKNTEN (IDX12)
                         +   LNK-SYU-HKNTEN (IDX11 IDX12)
      *
                     COMPUTE SUMSYU-TGMONEY   (WRK-SYU-SUM-IDX IDX12)
                         =   SUMSYU-TGMONEY   (WRK-SYU-SUM-IDX IDX12)
                         +   LNK-SYU-TGMONEY (IDX11 IDX12)
                     COMPUTE TTLSYU-TGMONEY  (IDX12)
                         =   TTLSYU-TGMONEY  (IDX12)
                         +   LNK-SYU-TGMONEY (IDX11 IDX12)
      *
                     COMPUTE SUMSYU-TGMONEY-TAX (WRK-SYU-SUM-IDX IDX12)
                         =   SUMSYU-TGMONEY-TAX (WRK-SYU-SUM-IDX IDX12)
                         +   LNK-SYU-TGMONEY-TAX (IDX11 IDX12)
                     COMPUTE TTLSYU-TGMONEY-TAX  (IDX12)
                         =   TTLSYU-TGMONEY-TAX  (IDX12)
                         +   LNK-SYU-TGMONEY-TAX (IDX11 IDX12)
                   END-PERFORM
      *
      *            保険適用金額
                   COMPUTE SUMSYU-HKNTEKMONEY   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-HKNTEKMONEY   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-HKNTEKMONEY (IDX11)
                   COMPUTE TTLSYU-HKNTEKMONEY
                       =   TTLSYU-HKNTEKMONEY
                       +   LNK-SYU-HKNTEKMONEY (IDX11)
      *
      *            老人一部負担金
                   COMPUTE SUMSYU-RJN-FTN    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RJN-FTN    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RJN-FTN  (IDX11)
                   COMPUTE TTLSYU-RJN-FTN
                       =   TTLSYU-RJN-FTN
                       +   LNK-SYU-RJN-FTN  (IDX11)
      *
      *            公費一部負担金
                   COMPUTE SUMSYU-KOH-FTN    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-KOH-FTN    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-KOH-FTN  (IDX11)
                   COMPUTE TTLSYU-KOH-FTN
                       =   TTLSYU-KOH-FTN
                       +   LNK-SYU-KOH-FTN  (IDX11)
      *
      *            自費
                   COMPUTE SUMSYU-JIHI-1   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-1   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-1 (IDX11)
                   COMPUTE TTLSYU-JIHI-1
                       =   TTLSYU-JIHI-1
                       +   LNK-SYU-JIHI-1 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-2   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-2   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-2 (IDX11)
                   COMPUTE TTLSYU-JIHI-2
                       =   TTLSYU-JIHI-2
                       +   LNK-SYU-JIHI-2 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-3   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-3   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-3 (IDX11)
                   COMPUTE TTLSYU-JIHI-3
                       =   TTLSYU-JIHI-3
                       +   LNK-SYU-JIHI-3 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-4   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-4   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-4 (IDX11)
                   COMPUTE TTLSYU-JIHI-4
                       =   TTLSYU-JIHI-4
                       +   LNK-SYU-JIHI-4 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-5   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-5   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-5 (IDX11)
                   COMPUTE TTLSYU-JIHI-5
                       =   TTLSYU-JIHI-5
                       +   LNK-SYU-JIHI-5 (IDX11)
      *
      *            自費計
                   COMPUTE SUMSYU-JIHI-TOTAL   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-TOTAL   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-TOTAL (IDX11)
                   COMPUTE TTLSYU-JIHI-TOTAL
                       =   TTLSYU-JIHI-TOTAL
                       +   LNK-SYU-JIHI-TOTAL (IDX11)
      *
      *            自費（消費税あり）
                   COMPUTE SUMSYU-JIHI-1-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-1-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-1-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-1-TAX
                       =   TTLSYU-JIHI-1-TAX
                       +   LNK-SYU-JIHI-1-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-2-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-2-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-2-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-2-TAX
                       =   TTLSYU-JIHI-2-TAX
                       +   LNK-SYU-JIHI-2-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-3-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-3-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-3-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-3-TAX
                       =   TTLSYU-JIHI-3-TAX
                       +   LNK-SYU-JIHI-3-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-4-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-4-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-4-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-4-TAX
                       =   TTLSYU-JIHI-4-TAX
                       +   LNK-SYU-JIHI-4-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-5-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-5-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-5-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-5-TAX
                       =   TTLSYU-JIHI-5-TAX
                       +   LNK-SYU-JIHI-5-TAX  (IDX11)
      *
      *            自費計（消費税あり）
                   COMPUTE SUMSYU-JIHI-TOTAL-TAX (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-TOTAL-TAX (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-TOTAL-TAX (IDX11)
                   COMPUTE TTLSYU-JIHI-TOTAL-TAX
                       =   TTLSYU-JIHI-TOTAL-TAX
                       +   LNK-SYU-JIHI-TOTAL-TAX (IDX11)
      *
      *            自費消費税
                   COMPUTE SUMSYU-JIHI-TAX  (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-TAX  (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-TAX
                       =   TTLSYU-JIHI-TAX
                       +   LNK-SYU-JIHI-TAX  (IDX11)
      *
      *            労災初診
                   COMPUTE SUMSYU-RSISHOSHIN-MONEY (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RSISHOSHIN-MONEY (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RSISHOSHIN-MONEY (IDX11)
                   COMPUTE TTLSYU-RSISHOSHIN-MONEY
                       =   TTLSYU-RSISHOSHIN-MONEY
                       +   LNK-SYU-RSISHOSHIN-MONEY (IDX11)
      *
      *            労災再診
                   COMPUTE SUMSYU-RSISAISHIN-MONEY (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RSISAISHIN-MONEY (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RSISAISHIN-MONEY (IDX11)
                   COMPUTE TTLSYU-RSISAISHIN-MONEY
                       =   TTLSYU-RSISAISHIN-MONEY
                       +   LNK-SYU-RSISAISHIN-MONEY (IDX11)
      *
      *            労災指導
                   COMPUTE SUMSYU-RSISDO-MONEY  (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RSISDO-MONEY  (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RSISDO-MONEY (IDX11)
                   COMPUTE TTLSYU-RSISDO-MONEY
                       =   TTLSYU-RSISDO-MONEY
                       +   LNK-SYU-RSISDO-MONEY (IDX11)
      *
      *            労災その他
                   COMPUTE SUMSYU-RSIETC-MONEY (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RSIETC-MONEY (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RSIETC-MONEY (IDX11)
                   COMPUTE TTLSYU-RSIETC-MONEY
                       =   TTLSYU-RSIETC-MONEY
                       +   LNK-SYU-RSIETC-MONEY (IDX11)
      *
      *
      *            食事療養費（保険）
                   COMPUTE SUMSYU-RYOYOHI-SKJ (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RYOYOHI-SKJ (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RYOYOHI-SKJ (IDX11)
                   COMPUTE TTLSYU-RYOYOHI-SKJ
                       =   TTLSYU-RYOYOHI-SKJ
                       +   LNK-SYU-RYOYOHI-SKJ (IDX11)
      *
      *            食事負担額（保険：自己負担）
                   COMPUTE SUMSYU-SKYMONEY-SKJ (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ
                       =   TTLSYU-SKYMONEY-SKJ
                       +   LNK-SYU-SKYMONEY-SKJ (IDX11)
      *
      *            食事負担額（保険：自己負担消費税）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-TAX (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-TAX (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-TAX (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-TAX
                       =   TTLSYU-SKYMONEY-SKJ-TAX
                       +   LNK-SYU-SKYMONEY-SKJ-TAX (IDX11)
      *
      *            食事負担額（保険：自己負担合計）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-KEI (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-KEI (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-KEI (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-KEI
                       =   TTLSYU-SKYMONEY-SKJ-KEI
                       +   LNK-SYU-SKYMONEY-SKJ-KEI (IDX11)
      *
      *            食事療養費（自費）
                   COMPUTE SUMSYU-RYOYOHI-SKJ-JIHI (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RYOYOHI-SKJ-JIHI (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RYOYOHI-SKJ-JIHI (IDX11)
                   COMPUTE TTLSYU-RYOYOHI-SKJ-JIHI
                       =   TTLSYU-RYOYOHI-SKJ-JIHI
                       +   LNK-SYU-RYOYOHI-SKJ-JIHI (IDX11)
      *
      *            食事負担額（自費：自己負担）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-JIHI (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI
                       =   TTLSYU-SKYMONEY-SKJ-JIHI
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI (IDX11)
      *
      *            食事負担額（自費：自己負担消費税）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI-TAX
                                                    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-JIHI-TAX
                                                    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI-TAX (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI-TAX
                       =   TTLSYU-SKYMONEY-SKJ-JIHI-TAX
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI-TAX (IDX11)
      *
      *            食事負担額（自費：自己負担合計）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI-KEI
                                                    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-JIHI-KEI
                                                    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI-KEI (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI-KEI
                       =   TTLSYU-SKYMONEY-SKJ-JIHI-KEI
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI-KEI (IDX11)
      *
      *            室料差額
                   COMPUTE SUMSYU-RMSAGAKU (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RMSAGAKU (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RMSAGAKU (IDX11)
                   COMPUTE TTLSYU-RMSAGAKU
                       =   TTLSYU-RMSAGAKU
                       +   LNK-SYU-RMSAGAKU (IDX11)
      *
      *            請求金額
                   COMPUTE SUMSYU-SKYMONEY (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY (IDX11)
                   COMPUTE TTLSYU-SKYMONEY
                       =   TTLSYU-SKYMONEY
                       +   LNK-SYU-SKYMONEY (IDX11)
      *
               END-IF
      *
           END-PERFORM
      *
      *    ゼロ件の場合はエラーとする
           IF    ( SUMSYUNOU-CNT    <=  ZERO )
               MOVE    "2001"      TO  SPA-ERRCD
               GO  TO  490-SYUNOU-TAIHI-EXT
           END-IF
      *
           COMPUTE SUMSYUNOU-CNT    =   SUMSYUNOU-CNT +   1
      *
      *    テーブル最大件数オーバー時はエラーとする
           IF    ( SUMSYUNOU-CNT    >   13 )
               MOVE    "2001"      TO  SPA-ERRCD
               GO  TO  490-SYUNOU-TAIHI-EXT
           END-IF
      *
      *
           MOVE    SUMSYUNOU-CNT    TO  SPA-I5-SYUNOU-CNT
      *
           MOVE    2               TO  SPA-I5-SYUNOU-TTLDAT-FLG
                                                   (SUMSYUNOU-CNT)
           MOVE    "999999"        TO  SPA-I5-SYUNOU-YM-S
                                                   (SUMSYUNOU-CNT)
           MOVE    "999999"        TO  SPA-I5-SYUNOU-YM-W
                                                   (SUMSYUNOU-CNT)
           MOVE    CNT-TUKISU      TO  SPA-I5-SYUNOU-TUKI-SU
                                                   (SUMSYUNOU-CNT)
           MOVE    TTLSYUNOU-REC   TO  SUMSYUNOU-OCC (SUMSYUNOU-CNT)
      *
           PERFORM VARYING IDX11   FROM    1   BY  1
                   UNTIL ( IDX11   >   SPA-I5-SYUNOU-CNT )
      *
               MOVE    SUMSYUNOU-OCC (IDX11)
                                   TO  SPA-I5-SYUNOU-DAT (IDX11)
      *
           END-PERFORM
      *
           .
       490-SYUNOU-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    未収データ取得処理
      *****************************************************************
       3100-MISYU-DAT-EDIT-SEC         SECTION.
      *
           MOVE    ZERO                TO   SPA-GMN-I51-ZENMISYU-NYU
                                            SPA-GMN-I51-ZENMISYU-GAI
      *    入院分未収集計
           INITIALIZE                       SYUNOU-REC
           MOVE    SPA-HOSPID          TO   SYU-HOSPID
           MOVE    "1"                 TO   SYU-NYUGAIKBN
           MOVE    SPA-NAI-I51-PTID    TO   SYU-PTID
           MOVE    SYUNOU-REC          TO   MCPDATA-REC
           MOVE    "SYUNOU-KEY2"       TO   ORC-DBPATH
           PERFORM  910-DB-SELECT-SEC
           IF      MCP-RC              =    ZERO
               MOVE    "SYUNOU-KEY2"   TO   ORC-DBPATH
               PERFORM   910-DB-READ-SEC
               PERFORM   UNTIL  MCP-RC  NOT =  ZERO
                      MOVE    MCPDATA-REC     TO  SYUNOU-REC
                      IF      SYU-SKYMONEY     >   SYU-NYUKIN-TOTAL
                              COMPUTE   SPA-GMN-I51-ZENMISYU-NYU   =
                                        SPA-GMN-I51-ZENMISYU-NYU   +
                                        (SYU-SKYMONEY   -
                                         SYU-NYUKIN-TOTAL)
                      END-IF
                      MOVE    "SYUNOU-KEY2"   TO   ORC-DBPATH
                      PERFORM   910-DB-READ-SEC
               END-PERFORM
           ELSE
               MOVE    ZERO            TO   SPA-GMN-I51-ZENMISYU-NYU
           END-IF
      *
           MOVE    "SYUNOU-KEY2"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    外来分未収集計
           INITIALIZE                       SYUNOU-REC
           MOVE    SPA-HOSPID          TO   SYU-HOSPID
           MOVE    "2"                 TO   SYU-NYUGAIKBN
           MOVE    SPA-NAI-I51-PTID    TO   SYU-PTID
           MOVE    SYUNOU-REC          TO   MCPDATA-REC
           MOVE    "SYUNOU-KEY2"       TO   ORC-DBPATH
           PERFORM  910-DB-SELECT-SEC
           IF      MCP-RC              =    ZERO
               MOVE    "SYUNOU-KEY2"   TO   ORC-DBPATH
               PERFORM   910-DB-READ-SEC
               PERFORM   UNTIL  MCP-RC  NOT =  ZERO
                      MOVE    MCPDATA-REC     TO  SYUNOU-REC
                      IF      SYU-SKYMONEY     >   SYU-NYUKIN-TOTAL
                              COMPUTE   SPA-GMN-I51-ZENMISYU-GAI   =
                                        SPA-GMN-I51-ZENMISYU-GAI   +
                                        (SYU-SKYMONEY   -
                                         SYU-NYUKIN-TOTAL)
                      END-IF
                      MOVE    "SYUNOU-KEY2"   TO   ORC-DBPATH
                      PERFORM   910-DB-READ-SEC
               END-PERFORM
           ELSE
               MOVE    ZERO            TO   SPA-GMN-I51-ZENMISYU-GAI
           END-IF
      *
           MOVE    "SYUNOU-KEY2"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
            .
       3100-MISYU-DAT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求データ編集処理
      *****************************************************************
       3100-SEIKYU-DAT-EDIT-SEC        SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-I51-SEIKYU-AREA
           INITIALIZE              SPA-GMN-I51-SEIKYU-AREA
      *
           MOVE    SPA-I5-SYUNOU-DAT (WRK-LST-IDX)
                                       TO  WKSYUNOU-REC
      *
      *
      *    保険料
           MOVE    ZERO                TO  SPA-GMN-I51-HKNRYO (12)
           PERFORM VARYING     IDX1     FROM    1   BY  1
                   UNTIL     ( IDX1     >   11 )
               MOVE    WKSYU-HKNTEN (IDX1)
                                      TO  SPA-GMN-I51-HKNRYO (IDX1)
               COMPUTE SPA-GMN-I51-HKNRYO (12)
                   =   SPA-GMN-I51-HKNRYO (12)
                   +   WKSYU-HKNTEN (IDX1)
      *    保険適用外
               COMPUTE SPA-GMN-I51-TGMONEY (IDX1)
                   =   WKSYU-TGMONEY (IDX1)
                   +   WKSYU-TGMONEY-TAX(IDX1)
      *    保険適用外
               COMPUTE SPA-GMN-I51-HKNGAI
                   =   SPA-GMN-I51-HKNGAI
                   +   WKSYU-TGMONEY (IDX1)
               COMPUTE SPA-GMN-I51-HKNGAI
                   =   SPA-GMN-I51-HKNGAI
                   +   WKSYU-TGMONEY-TAX (IDX1)
           END-PERFORM
      *
      *    保険適用金額
           MOVE    WKSYU-HKNTEKMONEY       TO  SPA-GMN-I51-HKNTEKI
      *    老人一部負担金
           MOVE    WKSYU-RJN-FTN           TO  SPA-GMN-I51-ROUFTN
           MOVE    WKSYU-KOH-FTN           TO  SPA-GMN-I51-KOHFTN
      *    自費１
           MOVE    WKSYU-JIHI-1            TO  SPA-GMN-I51-JIHI1
           MOVE    WKSYU-JIHI-2            TO  SPA-GMN-I51-JIHI2
           MOVE    WKSYU-JIHI-3            TO  SPA-GMN-I51-JIHI3
           MOVE    WKSYU-JIHI-4            TO  SPA-GMN-I51-JIHI4
           MOVE    WKSYU-JIHI-5            TO  SPA-GMN-I51-JIHI5
      *    自費計
           MOVE    WKSYU-JIHI-TOTAL        TO  SPA-GMN-I51-JIHIKEI
      *    自費（消費税あり）
           MOVE    WKSYU-JIHI-1-TAX        TO  SPA-GMN-I51-JIHITAX1
           MOVE    WKSYU-JIHI-2-TAX        TO  SPA-GMN-I51-JIHITAX2
           MOVE    WKSYU-JIHI-3-TAX        TO  SPA-GMN-I51-JIHITAX3
           MOVE    WKSYU-JIHI-4-TAX        TO  SPA-GMN-I51-JIHITAX4
           MOVE    WKSYU-JIHI-5-TAX        TO  SPA-GMN-I51-JIHITAX5
      *    自費計（消費税あり）
           MOVE    WKSYU-JIHI-TOTAL-TAX    TO  SPA-GMN-I51-JIHITAXKEI
      *    自費消費税
           MOVE    WKSYU-JIHI-TAX          TO  SPA-GMN-I51-JIHIZEI
      *    食事負担額
           COMPUTE SPA-GMN-I51-SKYMONEY-SKJ
               =   WKSYU-SKYMONEY-SKJ-KEI
               +   WKSYU-SKYMONEY-SKJ-JIHI-KEI
      *    食事療養費
           COMPUTE SPA-GMN-I51-RYOYOHI-SKJ
               =   WKSYU-RYOYOHI-SKJ
               +   WKSYU-RYOYOHI-SKJ-JIHI
      *
      *    一部負担金
           COMPUTE SPA-GMN-I51-ICHIFTN =   SPA-GMN-I51-ROUFTN
                                       +   SPA-GMN-I51-KOHFTN
                                       +   SPA-GMN-I51-SKYMONEY-SKJ
      *
      *    労災初診
           MOVE    WKSYU-RSISHOSHIN-MONEY  TO  SPA-GMN-I51-ROUSYOSIN
      *    労災再診
           MOVE    WKSYU-RSISAISHIN-MONEY  TO  SPA-GMN-I51-ROUSAISIN
      *    労災指導
           MOVE    WKSYU-RSISDO-MONEY      TO  SPA-GMN-I51-ROUSIDOU
      *    労災その他
           MOVE    WKSYU-RSIETC-MONEY      TO  SPA-GMN-I51-ROUETC
      *
      *    室料差額
           MOVE    WKSYU-RMSAGAKU          TO  SPA-GMN-I51-RMSAGAKU
      *
      *    今回請求額
           MOVE    SPA-NAI-I51-SEIKYU (WRK-LST-IDX)
                                           TO  SPA-GMN-I51-SEIKYU
      *
      *
      *    合計請求額
           COMPUTE SPA-GMN-I51-SEIGOK  =   SPA-NAI-I51-SEIKYU
                                                (SPA-I5-SYUNOU-CNT)
                                       +   SPA-GMN-I51-ZENMISYU-GAI
                                       +   SPA-GMN-I51-ZENMISYU-NYU
      *    自費名称編集
           PERFORM 3100-JIHIMSG-HEN-SEC
      *
            .
       3100-SEIKYU-DAT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3100-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    ZERO                TO  SYS-1013-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1013-EDYUKYMD
           MOVE    SPA-SYSYMD
                                       TO  ORC-DBYMD
      *    システム管理検索
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY2"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYS-1013-REC
      *
               IF    ( SPA-NYUGAIKBN       =   SYS-1013-KBNCD(1:1) )
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME   
                                           TO  SPA-GMN-I51-JIHIMSG (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I51-JIHIMSG (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I51-JIHIMSG (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I51-JIHIMSG (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I51-JIHIMSG (5)
                   END-EVALUATE
               END-IF
      *
               MOVE    "SYSKANRI-KEY2"      TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
           END-PERFORM
      *
           MOVE    "SYSKANRI-KEY2"     TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       3100-JIHIMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせリスト編集処理
      *****************************************************************
       3100-HKNCOMBILST-EDIT-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-I51-HKNCOMBILST
                                       SPA-NAI-I51-HKNCOMBILST
      *
           INITIALIZE                  SPA-GMN-I51-HKNCOMBILST
                                       SPA-NAI-I51-HKNCOMBILST
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I5-SYUNOU-CNT )
               IF    ( SPA-I5-SYUNOU-TTLDAT-FLG (IDX1) =   1 )
                   IF    ( SPA-I5-SYUNOU-HKNCOMBI-SU (IDX1)    =   1 )
                       COMPUTE IDX2    =   IDX1    +   1
                       COMPUTE IDX-LST =   IDX-LST     +   1
                       MOVE  IDX2  TO  SPA-NAI-I51-TSYUNOUIDX  (IDX-LST)
                       STRING  SPA-I5-SYUNOU-YM-W (IDX2)
                               "　"
                               SPA-I5-SYUNOU-HKNCOMBI (IDX2) (1 : 44 )
                               DELIMITED BY  SIZE
                       INTO    SPA-GMN-I51-THKNCOMBI (IDX-LST)
                       END-STRING
                   ELSE
                       COMPUTE IDX-LST =   IDX-LST     +   1
                       MOVE  IDX1  TO  SPA-NAI-I51-TSYUNOUIDX  (IDX-LST)
      *
                       MOVE  SPA-I5-SYUNOU-YM-W (IDX1)
                                   TO  SPA-GMN-I51-THKNCOMBI (IDX-LST)
      *
                       COMPUTE WRK-STIDX   =   IDX1    +   1
                       COMPUTE WRK-EDIDX
                           =   IDX1
                           +   SPA-I5-SYUNOU-HKNCOMBI-SU (IDX1)
                       PERFORM VARYING IDX2    FROM WRK-STIDX  BY  1
                               UNTIL ( IDX2    >    WRK-EDIDX )
                           COMPUTE IDX-LST =   IDX-LST     +   1
                           MOVE    IDX2
                                   TO  SPA-NAI-I51-TSYUNOUIDX (IDX-LST)
                           STRING  "　"
                                   SPA-I5-SYUNOU-HKNCOMBI (IDX2)
                                                          (1 : 44 )
                                   DELIMITED BY  SIZE
                           INTO    SPA-GMN-I51-THKNCOMBI (IDX-LST)
                           END-STRING
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-I5-SYUNOU-TUKI-SU (SPA-I5-SYUNOU-CNT)
                                       >   1 )
               COMPUTE IDX-LST =   IDX-LST     +   1
               MOVE    "合計"          TO  SPA-GMN-I51-THKNCOMBI
                                                               (IDX-LST)
               MOVE    SPA-I5-SYUNOU-CNT
                                       TO  SPA-NAI-I51-TSYUNOUIDX
                                                               (IDX-LST)
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >    IDX-LST )
               MOVE    IDX1        TO  SPA-GMN-I51-HKNCOMBILST-CNT
                                       SPA-GMN-I51-TSELNUM (IDX1)
                                       SPA-NAI-I51-TSELNUM (IDX1)
           END-PERFORM
      *
           MOVE    1               TO  SPA-GMN-I51-HKNCOMBILST-SEL
                                       SPA-GMN-I51-SELNUM
      *
            .
       3100-HKNCOMBILST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4801-KANREN-CHK-SEC         SECTION.
      *
           IF    ( SPA-NAI-I51-SANTEIEDYMD
                                   >=  SPA-NAI-I51-SANTEISTYMD )
               CONTINUE
           ELSE
               MOVE    "0005"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-I51-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    算定開始日〜算定開始日の翌月までの日付が入力されていること
           MOVE    SPA-NAI-I51-SANTEISTYMD
                                   TO  WRK-YMD
           MOVE    "2"             TO  WRK-ZOGENPTN
           MOVE    1               TO  WRK-ZOGEN
           PERFORM 5002-CALENDAR-SEC
      *
           IF    ( SPA-NAI-I51-SANTEIEDYMD (1 : 6)
                                   <= WRK-KEISANYMD (1 : 6) )
               CONTINUE
           ELSE
               MOVE    "0006"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-I51-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *
           .
       4801-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-I5IDCD      NOT =   SPACE
               PERFORM 520-I5IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I51"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力エラー"    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "該当の患者は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "該当の患者は現在入院中ではありません"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "算定終了日を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "算定開始日以降の日付を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "算定開始日の翌月末日までの日付を入力して"
                                       TO  SPA-ERRMSG (1  : 40)
               MOVE    "ください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1001"
               MOVE    "患者番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "2001"
               MOVE    "負担金計算処理に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "2002"
               MOVE    "退院計算に必要な会計情報が作成されていません"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I5ERR
           MOVE    SPA-ERRCD           TO  I5ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I5ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I51"               TO  SPA-MOTOPG
           MOVE    "I5ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I5ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-I5IDSET-SEC                 SECTION.
      *
           EVALUATE    SPA-I5IDCD
           WHEN    "3001"
               MOVE    "退院時請求金額の仮計算を行います"
                                       TO  WRK-I5IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I5ID1
           MOVE    SPA-I5IDCD          TO  I5ID1-ID1CODE
           MOVE    WRK-I5IDMSG         TO  I5ID1-ID1MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I51"               TO  SPA-MOTOPG
           MOVE    "I5ID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I5ID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-I5IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSDAY
                                           WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1   NOT =   ZERO )
               MOVE    1           TO  FLG-ORCSDAY
               GO  TO              5002-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       5002-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       5002-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-SELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    WRK-DBPATH  TO  ORC-DBPATH
               PERFORM 910-DB-READ-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       900-TBL-READ-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-READ-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル更新処理
      *****************************************************************
       900-TBL-UPDATE-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-UPDATE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル追加処理
      *****************************************************************
       900-TBL-INSERT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-INSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル削除処理
      *****************************************************************
       900-TBL-DELETE-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-DELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-CLOSE-SEC
      *
           .
       900-TBL-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DB-DELETE-SEC               SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DB-UPDATE-SEC               SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DB-INSERT-SEC               SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DB-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DB-READ-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSE"            TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCMCPSUB"   USING
                                MCPAREA
                                ORCMCPAREA
                                MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
