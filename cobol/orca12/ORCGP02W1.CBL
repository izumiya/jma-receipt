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
       PROGRAM-ID.             ORCGP02W1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者基本情報（Ｐ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/04/12  保険番号マスタ確認の方法
      *  01.00.02    MCC-太田     01/04/16  組合せ作成のパラメータ変更
      *  01.00.03    MCC-太田     01/04/18  生年月日未来日チェック追加
      *  01.00.04    MCC-太田     01/05/02  性別必須入力削除
      *  01.00.05    MCC-太田     01/05/10  保険削除不能の対応/タイトル
      *  01.00.06    MCC-太田     01/05/10  日付変換後ゼロ表示変更
      *  01.00.07    MCC-太田     01/05/16  減免事由未存在時の対応
      *  01.00.08    MCC-太田     01/05/23  患者番号構成管理の追加
      *  01.00.09    MCC-太田     01/05/23  スパ領域転送追加
      *  01.00.10    MCC-太田     01/05/24  HOKENJANAME 
      *  01.00.11    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  01.00.12    MCC-多々納   01/07/02  日付桁数変更・年齢追加
      *  01.00.13    MCC-多々納   02/04/05  労災保険の追加
      *  01.00.14    MCC-多々納   02/05/02  半角チェックの追加
      *  01.00.15    MCC-太田     02/05/31  カナ氏名候補画面を追加
      *  02.00.00    MCC-多々納   02/06/20  画面レイアウト変更による修正
      *  02.00.01    NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      *  02.00.02    NACL-多々納  02/09/09  平成１４年１０月改正対応
      *                                     自賠責保険の追加
      *  02.00.03    NACL-多々納  02/10/07  船員の老人補助区分の初期表示修正
      *  02.00.04    NACL-多々納  02/10/31  船員保険補助区分追加
      *  02.00.05    NACL-多々納  02/11/06  保険・公費の確認日修正
      *  02.00.06    NACL-多々納  02/11/14  船員保険の変更初期表示修正
      *  02.00.07    NACL-多々納  02/11/21  共済下船３月追加
      *  02.00.08    NACL-多々納  03/03/24  Ｈ１５年４月改正対応追加
      *                                     公費負担額追加
      *  02.00.09    NACL-多々納  03/07/08  保険者異動日チェック追加
      *  02.00.10    NACL-多々納  03/08/28  公費負担額に’０１２’と
      *                                     ’０５１’を追加
      *  02.00.11    NACL-多々納  03/10/04  公費負担額に
      *                                     東京の特定疾患公費追加
      *  02.00.12    NACL-多々納  03/10/09  公費負担額に東京の'451'追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  PAR-KEY
      *                            FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PAR-KEY.
      *        05  PAR-FILEMEI         PIC X(08).
      *        05  PAR-EDANUM          PIC 9(04).
      *    03  PAR-DATA-REC            PIC X(3000).
      *
       WORKING-STORAGE             SECTION.
      *    パラメタファイル名
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "P01PARA".
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      **    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-LNKPARA         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-ADRS            PIC 9(01).
      *
      *    入力の有無
           03  FLG-IN-KANANAME        PIC 9(01).
           03  FLG-IN-NAME            PIC 9(01).
           03  FLG-IN-SEX             PIC 9(01).
           03  FLG-IN-BIRTHDAY        PIC 9(01).
           03  FLG-IN-POST            PIC 9(01).
      *    二重登録疑い用
           03  FLG-P02A-SYORI         PIC 9(01).
           03  FLG-CHK                PIC 9(01).
           03  FLG-ERR                PIC 9(01).
           03  FLG-OK                 PIC 9(01).
      *    登録フラグ
           03  FLG-TOUROKU            PIC 9(01).
      *    登録フラグ
           03  FLG-TOUROKU2           PIC 9(01).
      *
      *    保険入力
           03  FLG-IN-HKN             PIC 9(01).
      *    保険削除
           03  FLG-HKN-DEL            PIC 9(01).
      *    公費削除
           03  FLG-KOH-DEL            PIC 9(01).
      *    公費追加
           03  FLG-KOH-ADD            PIC 9(01).
      *    公費追加
           03  FLG-KOH-ADD2           PIC 9(01).
      *
      *----(01.00.01) LINE ADD START ---------------------------------
           03  FLG-RJN             PIC 9(01).
      *----(01.00.15) LINE ADD START ---------------------------------
           03  FLG-JINMEI          PIC 9(01).
      *    カナ氏名検索用フラグ
      *    0:対象１件/1:対象なし/2:複数件該当
           03  FLG-NAME            PIC 9(01)   OCCURS 2.
      *----(01.00.15) LINE ADD END   ---------------------------------
      *
           03  FLG-RJNCHK          PIC 9(01).
           03  FLG-HKNCHK          PIC 9(01).
           03  FLG-SEIHOGO         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *----(01.00.15) LINE ADD START ---------------------------------
           03  CNT-JINMEI          PIC 9(06).
           03  CNT-KANANAME        PIC 9(06).
      *----(01.00.15) LINE ADD END   ---------------------------------
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDA                 PIC 9(04).
      *----(01.00.15) LINE ADD START ---------------------------------
           03  IDX1                PIC 9(05).
      *----(01.00.15) LINE ADD END   ---------------------------------
           03  IDX2                PIC 9(04).
           03  IDX-H               PIC 9(04).
      *
           03  IDT                 PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-KOH             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SYMD6           PIC 9(08).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-RENNUMZ         PIC ZZZZ.
      *    基準日
           03  WRK-BASEYMD.
               05  WRK-BYY         PIC 9(04).
               05  WRK-BMM         PIC 9(02).
               05  WRK-BDD         PIC 9(02).
      *    保存年齢
           03  WRK-MAE-NENREI.
               05  WRK-MAE-NENREI-YY       PIC 9(03).
               05  WRK-MAE-NENREI-MM       PIC 9(02).
      *    入力項目名
      *****03  WRK-MCP-WIDGET          PIC X(64).
      *    保険開始位置
           03  WRK-HKNSTR              PIC 9(04).
      *    公費開始位置
           03  WRK-KOHSTR              PIC 9(04).
      *
           03  WRK-SELNUM              PIC X(04).
           03  WRK-GMN-SELNUM          PIC 9(04).
      *
           03  WRK-HKNCOMBI        PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
           03  WRK-NAIBU-IDX-AREA.
               05  WRK-G-IDX       PIC 9(04).
               05  WRK-N-IDX       PIC 9(04).
      *        1:保険テーブル /2:公費テーブル
               05  WRK-W-TBL       PIC 9(01).
      *
           03  WRK-KENSAKU-AREA.
               05  WRK-KEN-HKNNUM      PIC X(03).
      *----(01.00.01) LINE MOD START ---------------------------------
      *         05  WRK-KEN-TEKYMD      PIC X(08).
               05  WRK-KEN-TEKSTYMD    PIC X(08).
               05  WRK-KEN-TEKEDYMD    PIC X(08).
      *----(01.00.01) LINE MOD END   ---------------------------------
               05  WRK-KEN-ERFLG       PIC X(01).
               05  WRK-KEN-SBTKBN      PIC X(01).
      *
           03  WRK-PARACNT-AREA.
               05  WRK-PCNT-HKN        PIC 9(04).
               05  WRK-PCNT-KOH        PIC 9(04).
      *
           03  WRK-SYSKAN-AREA.
               05  WRK-SYSKAN-KANRICD  PIC X(04).
               05  WRK-SYSKAN-KBNCD    PIC X(08).
               05  WRK-SYSKAN-STYUKYMD PIC X(08).
               05  WRK-SYSKAN-EDYUKYMD PIC X(08).
      *
           03  WRK-HKNKOH-FLG          PIC 9(01).
      *
           03  WRK-MEISAI1             PIC 9(06).
           03  WRK-MEISAI2             PIC 9(06).
           03  WRK-MEISAI3             PIC 9(06).
           03  WRK-HST                 PIC 9(04).
           03  WRK-HED                 PIC 9(04).
           03  WRK-KST                 PIC 9(04).
           03  WRK-KED                 PIC 9(04).
           03  WRK-DISCOUNT            PIC Z9.
           03  WRK-KYUSEI              PIC 9(01).
           03  WRK-RATEKBN             PIC X(02).
           03  WRK-KBNCD               PIC X(02).
           03  WRK-OK                  PIC 9(01).
      *----(01.00.06) LINE MOD START ---------------------------------
           03  WRK-WWYMD               PIC X(10).
      *----(01.00.06) LINE MOD END   ---------------------------------
      *
      *    労災保険ＩＤ
           03  WRK-PTRSI-HKNID         PIC 9(10).
      *
      *----(01.00.15) LINE ADD START ---------------------------------
           03  WRK-NAME-AREA.
               05  WRK-NAME-X-AREA.
                   07  WRK-NAME-X  PIC X(02)   OCCURS 25.
               05  WRK-NAME-VAL    PIC X(50)   OCCURS  3.
               05  WRK-NAME-KANJI  PIC X(50)   OCCURS  2.
               05  WRK-NAME-KANA   PIC X(50)   OCCURS  2.
               05  WRK-KANJINAME-AREA.
                   07  WRK-KANJINAME-OCC       OCCURS  50
                                               INDEXED BY  IDX-NAME.
                       09  WRK-KANJINAME-T
                                   PIC X(50).
               05  WRK-KANJINAME   PIC X(50).
      *----(01.00.15) LINE ADD END   ---------------------------------
      *
      *    保険公費種別
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNKOHSBTKBN        PIC X(01).
           03  WRK-TANSEIDONAME        PIC X(20).
           03  WRK-TEKSTYMD            PIC X(08).
           03  WRK-TEKEDYMD            PIC X(08).
      *
           03  WRK-NAI-HKNSTR       PIC 9(04).
           03  WRK-NAI-KOHSTR       PIC 9(04).
      *
      *    公費ＳＯＲＴ用（ＳＰＡと同じ領域）
       01  WRK-KOUHI-AREA.
      *    公費情報
           03  WRK-GMN-KOH-AREA.
               05  WRK-GMN-KOH-TBL         OCCURS   3.
                   07  WRK-GMN-HOK-G.
                       09  WRK-GMN-KOHNUM-G.
                          11  WRK-GMN-KOHNUM      PIC X(03).
                          11  WRK-GMN-KOHKBN      PIC X(01).
      ****************    11  WRK-GMN-H1          PIC X(01).
                          11  WRK-GMN-KOHNUMMEI   PIC X(16).
      *
                       09  WRK-GMN-KTEKSTYMD        PIC X(10).
                       09  WRK-GMN-KTEKEDYMD        PIC X(10).
                       09  WRK-GMN-KKAKUNINYMD      PIC X(10).
                       09  WRK-GMN-FTNJANUM         PIC X(08).
                       09  WRK-GMN-JKYSNUM          PIC X(20).
      *
                   07  WRK-NAI-KTEKSTYMD        PIC X(08).
                   07  WRK-NAI-KTEKEDYMD        PIC X(08).
                   07  WRK-NAI-KKAKUNINYMD      PIC X(08).
      *
                   07  WRK-NAI-PTKOH-EDANUM       PIC 9(04).
                   07  WRK-NAI-PTKOH-KOHID        PIC 9(10).
                   07  WRK-NAI-PTKOH-SBTKBN       PIC X(01).
                   07  WRK-NAI-PTKOH-PAYKBN       PIC X(02).
      *
      *                登録時の負担者番号・公費種類
                   07  WRK-MAE-KOHNUM-G.
                       09  WRK-MAE-KOHNUM      PIC X(03).
                       09  WRK-MAE-KOHKBN      PIC X(01).
      **************** 09  WRK-MAE-H1          PIC X(01).
                       09  WRK-MAE-KOHNUMMEI   PIC X(16).
                   07  WRK-MAE-FTNJANUM            PIC X(08).
                   07  WRK-MAE-KKAKUNINYMD         PIC X(08).
                   07  WRK-MAE-KKAKUNINYMDW        PIC X(09).
      *            ６５歳以上警告
                   07  WRK-NAI-AGE65               PIC 9(01).
      *            保険者異動日警告
                   07  WRK-PTKOH-IDOYMD            PIC X(08).
                   07  WRK-NAI-PTKOH-KEIFLG        PIC X(01).
               05  WRK-KOH-MAX             PIC 9(04).
      *
      *    保険組合せ（ＳＰＡと同じ）
       01  WRK-HKNCOBMI-SORT-AREA.
               05  WRK-HKNCOMBI-AREA.
                   07  WRK-GMN-HKNCOMBI-LIST       OCCURS   15.
                       09  WRK-GMN-THKNCOMBINUM    PIC 9(04).
                       09  WRK-GMN-THKNCOMBIMEI    PIC X(40).
                       09  WRK-GMN-TTEKSTYMD       PIC X(09).
                       09  WRK-GMN-TTEKEDYMD       PIC X(09).
                       09  WRK-GMN-THKNNUM         PIC X(03).
                       09  WRK-GMN-THKNID          PIC 9(10).
      *
                       09  WRK-NAI-TTEKSTYMD       PIC X(08).
                       09  WRK-NAI-TTEKEDYMD       PIC X(08).
            05  WRK-SORT-KEY.
                09  WRK-SRT-TEKISTYMD      PIC X(08).
                09  WRK-SRT-HKNCOMBINUM    PIC X(04).
                09  WRK-SRT-IDX            PIC 9(04).
      *
       01  WRK-HENSYU-AREA.
      *
           03  WRK-HONKA-TBL.
               05  WRK-HONKA-REC-G.
                   07  FILLER          PIC X(06) VALUE "1 本人".
                   07  FILLER          PIC X(06) VALUE "2 家族".
               05  WRK-HONKA-REC-R     REDEFINES   WRK-HONKA-REC-G.
                   07  WRK-HONKA-REC   PIC X(06) OCCURS  2.
      *
           03  WRK-HJO-TBL.
               05  WRK-HJO-MAX         PIC 9(04).
               05  WRK-HJO-REC-G1.
                   07  FILLER          PIC X(10) VALUE "1 職務".
                   07  FILLER          PIC X(10) VALUE "2 下船".
                   07  FILLER          PIC X(10) VALUE "3 通勤".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "A １割職務".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "C １割通勤".
                   07  FILLER          PIC X(10) VALUE "D ２割職務".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "F ２割通勤".
               05  WRK-HJO-REC-R1      REDEFINES   WRK-HJO-REC-G1.
                   07  WRK-HJO-REC1    PIC X(10) OCCURS  11.
               05  WRK-HJO-REC-G2.
                   07  FILLER          PIC X(06) VALUE "0 無し".
                   07  FILLER          PIC X(06) VALUE "1 １割".
                   07  FILLER          PIC X(06) VALUE "2 ２割".
                   07  FILLER          PIC X(06) VALUE "3 ３割".
               05  WRK-HJO-REC-R2      REDEFINES   WRK-HJO-REC-G2.
                   07  WRK-HJO-REC2    PIC X(06) OCCURS  4.
               05  WRK-HJO-REC-G3.
                   07  FILLER          PIC X(06) VALUE "1 課税".
                   07  FILLER          PIC X(06) VALUE "2 非課".
               05  WRK-HJO-REC-R3      REDEFINES   WRK-HJO-REC-G3.
                   07  WRK-HJO-REC3    PIC X(06) OCCURS  2.
      *        老人の補助区分追加
               05  WRK-HJO-REC-G4.
                   07  FILLER          PIC X(06) VALUE "8 ２割".
                   07  FILLER          PIC X(06) VALUE "9 １割".
               05  WRK-HJO-REC-R4      REDEFINES   WRK-HJO-REC-G4.
                   07  WRK-HJO-REC4    PIC X(06) OCCURS  2.
      *        共済の下船後補助区分追加
               05  WRK-HJO-REC-G5.
                   07  FILLER          PIC X(10) VALUE "2 下船３月".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
               05  WRK-HJO-REC-R5      REDEFINES   WRK-HJO-REC-G5.
                   07  WRK-HJO-REC5    PIC X(10) OCCURS  5.
      *
           03  WRK-KEI-TBL.
               05  WRK-KEI-REC-G.
                   07  FILLER          PIC X(06) VALUE "1 継続".
                   07  FILLER          PIC X(06) VALUE "2 任継".
                   07  FILLER          PIC X(06) VALUE "3 特別".
               05  WRK-KEI-REC-R       REDEFINES   WRK-KEI-REC-G.
                   07  WRK-KEI-REC     PIC X(06) OCCURS  3.
               05  WRK-CONT-MAX        PIC 9(04).
      *
           03  WRK-HENYMD.
               05  WRK-HGO             PIC X(01).
               05  WRK-HYY             PIC Z9.
               05  FILLER              PIC X(01)   VALUE  ".".
               05  WRK-HMM             PIC Z9.
               05  FILLER              PIC X(01)   VALUE  ".".
               05  WRK-HDD             PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH             PIC 99.
               05  FILLER              PIC X(01)   VALUE  ":".
               05  WRK-TMM             PIC 99.
      *
           03  WRK-KEI1                PIC X(06)   VALUE  "（継）".
           03  WRK-KEI2                PIC X(06)   VALUE  "（任）".
      *
           03  WRK-RATE-AREA.
               05  WRK-RATE-KBN        PIC X(02).
               05  WRK-RATE-SP         PIC X(01).
               05  WRK-RATE-EDIT.
                   07  WRK-RATE-EDITZ  PIC ZZZZZZZ.
                   07  WRK-RATE-TANI   PIC X(04).
               05  WRK-RATE-EDIT-R  REDEFINES    WRK-RATE-EDIT.
                   07  WRK-RATE-EDITR  PIC X(11).
      *----(01.00.12) LINE ADD START ---------------------------------
      *
       01  WRK-NENREI-AREA.
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(04).
               05  WRK-NENREI-MM       PIC 9(02).
               05  WRK-NENREI-DD       PIC 9(02).
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *----(01.00.12) LINE ADD END   ---------------------------------
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT       PIC X(100).
           03  WRK-ATO-INPUT       PIC X(100).
           03  WRK-MAX             PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    状態情報
      *    COPY    "CPSK1018.INC".
      *    減免事由情報
      *    COPY    "CPSK1026.INC".
      *    割引率情報
      *****COPY    "CPSK1027.INC".
      *    周辺住所
           COPY    "CPSK1015.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    保険番号情報  *
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    受付   *
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者保険情報  *
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    組合せ作成時使用項目
           03  PTHKN-SBT          PIC X(01).
           03  PTHKN-SEIDO        PIC X(10).
      *
      *    患者公費情報  *
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    組合せ作成時使用項目
           03  PTKOH-SBT          PIC X(01).
           03  PTKOH-SEIDO        PIC X(10).
           03  PTKOH-HKNKOHKBN    PIC X(01).
           03  PTKOH-GENHKNNUM1   PIC X(03).
           03  PTKOH-GENHKNNUM2   PIC X(03).
           03  PTKOH-GENHKNNUM3   PIC X(03).
      *
      *    患者情報  *
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *
      *    患者情報  *
      *01  LNK-PTINF-REC.
      *    COPY "CPPTINF.INC" REPLACING  //PTINF-// BY //LNK-PTINF-//.
      *
      *    旧姓履歴   *
      *01  LNK-KYUSEIRRK-AREA.
      *    02  LNK-KYUSEIRRK-REC.
      *    COPY  "CPKYUSEIRRK.INC" REPLACING  //KYUSEI-//
      *                                    BY //LNK-KYU-//.
      *    患者番号変換  *
      *01  LNK-PTNUM-AREA.
      *    02  LNK-PTNUM-REC.
      *    COPY "CPPTNUM.INC" REPLACING //PTNUM-// BY //LNK-PTNUM-//.
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *----(01.00.15) LINE ADD START ---------------------------------
      *    人名
       01  JINMEI-REC.
           COPY    "CPJINMEI.INC".
      *----(01.00.15) LINE ADD END   ---------------------------------
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *----(01.00.12) LINE ADD START ----------------------------------
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *----(01.00.12) LINE ADD END   ----------------------------------
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
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
           COPY    "P02.INC".
           COPY    "P02A.INC".
      *****COPY    "P02B.INC".
      *****COPY    "P02C.INC".
           COPY    "P02D.INC".
           COPY    "P02E.INC".
           COPY    "P02F.INC".
           COPY    "P014.INC".
           COPY    "P98.INC".
           COPY    "P981.INC".
           COPY    "P99.INC".
           COPY    "P0201.INC".
           COPY    "P982.INC".
           COPY    "P100.INC".
           COPY    "P02X.INC".
           COPY    "PERR.INC".
           COPY    "PID1.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
           MOVE    SPACE           TO  SPA-MCP-WIDGET
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
      *    画面編集後、表示へ
           IF      SPA-FLG-END             =   ZERO
      ******** PERFORM 500-GMNHEN-SEC
               PERFORM 5001-MAPCUR-SEC
      *
           END-IF
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期画面セット
           PERFORM 300-SCREEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
           MOVE    ZERO            TO  SPA-FLG-END
      *
           EVALUATE    SPA-MOTOPG
      *        住所検索より
               WHEN    "P99"
                   IF      JPAR-POST     NOT =   SPACE
                       MOVE    JPAR-POST   TO  SPA-GMN-POST
                       MOVE    JPAR-EDITADRS-NAME
                                           TO  SPA-GMN-EDITADRS-NAME
                       MOVE    42          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPACE       TO  SPA-GMN-POST
                       MOVE    40          TO  SPA-GMN-CUR
                   END-IF
      *        住所検索より
               WHEN    "P98" 
                   IF      JPAR-POST   NOT =   SPACE
                       MOVE    JPAR-POST   TO  SPA-GMN-POST
                       MOVE    JPAR-EDITADRS-NAME
                                           TO  SPA-GMN-EDITADRS-NAME
                       MOVE    42          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPACE       TO  SPA-GMN-POST
                       MOVE    40          TO  SPA-GMN-CUR
                   END-IF
      *        住所検索より
               WHEN    "P982" 
                   IF      SPA-GMN982-TOWNNAME     NOT =   SPACE
                       MOVE    SPA-GMN982-POST
                                           TO  SPA-GMN-POST
                       MOVE    SPA-GMN982-TOWNNAME
                                           TO  SPA-GMN-EDITADRS-NAME
                       MOVE    42          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    40          TO  SPA-GMN-CUR
                   END-IF
      *        二重登録確認画面処理
               WHEN    "P02A"
                   MOVE    1               TO  SPA-GMN-CUR
      *        労災保険更新画面処理
               WHEN    "P02D"
                   PERFORM 4910-RSKOH-PARAOUT-SEC
      *        カナ氏名検索
               WHEN    "P100"
                   IF  ( SPA-GMN-P100-FLG-BTN  NOT =   ZERO )
                       MOVE    SPA-GMN-P100-KAKUTEI-NAME
                                               TO  SPA-GMN-NAME
                                                   SPA-GMN-OLD-NAME
                                                   SPA-GMN-NAMEH
                       IF  SPA-GMN-SETAINUSI      =   SPACE
                           MOVE    SPA-GMN-NAME   TO  SPA-GMN-SETAINUSI
                       END-IF
                   END-IF
      *
                   MOVE    2                   TO  SPA-GMN-CUR
      *----(01.00.15) LINE ADD END   ---------------------------------
      *        患者複写
               WHEN    "P02E"
      *            保険組合せ作成
                   PERFORM 4503-HKNCOMBI-SYORI-SEC
                   PERFORM 310-SPASET-SEC
                   MOVE    1                  TO  SPA-GMN-CUR
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
      *        初期画面編集
               WHEN    OTHER
      *            保険・公費編集処理
                   PERFORM 310-SPASET-SEC
                   MOVE    1                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0104"
               WHEN    "0105"
      *            保険重複処理
                   CALL    "ORCSP03A"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
               WHEN    "0106"
               WHEN    "0107"
      *            公費重複処理
                   MOVE    ZERO                TO  SPA-KOH-ADD
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
      *            老人公費追加（Ｈ１４．１０改正対応）
                   IF       SPA-KOH-ADD        =   1
      *                最初の１行を追加する
                       MOVE    ZERO            TO  SPA-HKNKOH-FLG
                       PERFORM 4104-KOH-ADD-SEC
      *                公費重複処理
                       CALL    "ORCSP03B"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
                   END-IF
      *        保険終了処理
               WHEN    "0201"
               WHEN    "0203"
                   CALL    "ORCSP03A"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
               WHEN    "0202"
               WHEN    "0204"
                   IF      SPA-HKN-IDX1        =   ZERO
      *                新規の時、１行開ける
                       PERFORM 410311-HKNTBL-INS-SEC
                       MOVE    1                   TO  IDX
                   ELSE
                       MOVE    SPA-HKN-IDX1        TO  IDX
                   END-IF
                   PERFORM 410312-HKN-SPATBL-SET-SEC
                   CALL    "ORCSP03A"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    年齢計算
           IF     (SPA-NAI-BIRTHDAY    =   SPACE  OR  ZERO )  OR
                  (SPA-BYMD-70     NOT =   ZERO            )
               CONTINUE
           ELSE
               PERFORM 410121-NENREI-SET-SEC
           END-IF
      *
      *    テーブル内の保険情報に名称を編集する
           PERFORM 3100-HKNTBL-KBNHEN-SEC
      *    保険表示連番
           MOVE    1                   TO  SPA-NAI-HKNSTR
      *    保険情報ＴＢＬから編集
           PERFORM 3101-HKN-TBL-SPA-SEC
      *
      *    公費表示連番開始
           MOVE    1                   TO  SPA-NAI-KOHSTR
      *    公費ＴＢＬから編集
           PERFORM 3101-KOH-TBL-SPA-SEC
      *
      *    保険組合せ表示（現在対象分のみ）
           PERFORM 3103-HKNCOMB-PARA-SEC
      *
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    本人家族区分等の名称編集処理
      *****************************************************************
       3100-HKNTBL-KBNHEN-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-PTHKN-MAX
      *
      *        本人家族
               PERFORM VARYING     IDT     FROM    1  BY  1
                       UNTIL       IDT     >   2
                   IF      WRK-HONKA-REC (IDT)(1:1)
                                           =   SPA-PTHKN-HONKZKKBN(IDX)
                       MOVE    WRK-HONKA-REC (IDT)(3:)
                                           TO  SPA-PTHKN-HONKZKKBN-MEI
                                                                  (IDX)
                   END-IF
               END-PERFORM
      *
      *        継続区分
               IF      SPA-PTHKN-HKNNUM (IDX)  =   "003"
                   MOVE    3                   TO  WRK-CONT-MAX
               ELSE
                   MOVE    2                   TO  WRK-CONT-MAX
               END-IF
               PERFORM VARYING IDT     FROM    1  BY  1
                       UNTIL   IDT     >   WRK-CONT-MAX
                   IF      WRK-KEI-REC (IDT)(1:1)
                                           =   SPA-PTHKN-CONTKBN (IDX)
                       MOVE    WRK-KEI-REC (IDT)(3:)
                                               TO  SPA-PTHKN-CONTKBN-MEI
                                                                  (IDX)
                   END-IF
               END-PERFORM
      *
      *        負担割合
               EVALUATE    SPA-PTHKN-HKNNUM (IDX)
      *        船員
               WHEN    "002"
                   IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  "20021001") AND
                          (SPA-BYMD-70             >   ZERO      )  AND
                          (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-70)
                       MOVE    4               TO  IDY
                       MOVE    11              TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    3               TO  WRK-HJO-MAX
                   END-IF
      *************MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       IF      WRK-HJO-REC1 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC1 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    WRK-HJO-MAX     TO  IDT
                       END-IF
                   END-PERFORM
      *        国保
               WHEN    "060"
                   PERFORM VARYING     IDT     FROM    1  BY  1
                           UNTIL       IDT     >   4
                       IF      WRK-HJO-REC2 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC2 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    4           TO  IDT
                       END-IF
                   END-PERFORM
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  "20021001") AND
                          (SPA-BYMD-70             >   ZERO      )  AND
                          (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-70)
                       MOVE    2               TO  IDY
                       MOVE    5               TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    1               TO  WRK-HJO-MAX
                   END-IF
      *************MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       IF      WRK-HJO-REC5 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC5 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    WRK-HJO-MAX     TO  IDT
                       END-IF
                   END-PERFORM
      *        その他
               WHEN    OTHER
      *            自費
                   IF      SPA-PTHKN-HKNNUM (IDX)(1:2) =   "98"
                       PERFORM VARYING     IDT     FROM    1  BY  1
                               UNTIL       IDT     >   2
                           IF      WRK-HJO-REC3 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                               MOVE    WRK-HJO-REC3 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                               MOVE    2           TO  IDT
                           END-IF
                       END-PERFORM
                   ELSE
      *                Ｈ１４．１０ 改正より
                       IF      SPA-PTHKN-TEKSTYMD(IDX) >=  "20021001"
                           PERFORM 31001-HKNTBL-RJNHEN-SEC
                       END-IF
                   END-IF
               END-EVALUATE
           END-PERFORM
      *
           .
       3100-HKNTBL-KBNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人補助区分編集処理
      *****************************************************************
       31001-HKNTBL-RJNHEN-SEC            SECTION.
      *
      *    適用開始日が７０歳以上で、労災・治験・自費
           IF      (SPA-PTHKN-TEKSTYMD(IDX)    >=  SPA-BYMD-70)  AND
                   (SPA-BYMD-70                >   ZERO      )  AND
                   (SPA-PTHKN-HKNNUM(IDX)(1:1) NOT =   "9")
               PERFORM VARYING     IDT     FROM    1  BY  1
                       UNTIL       IDT     >   2
                   IF      WRK-HJO-REC4 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                       MOVE    WRK-HJO-REC4 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                       MOVE    2               TO  IDT
                   END-IF
               END-PERFORM
           END-IF
           .
       31001-HKNTBL-RJNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報パラメタファイル検索処理
      *****************************************************************
       3101-HKN-TBL-SPA-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-HKN-AREA
           MOVE    ZERO                TO  FLG-CHK
           IF      SPA-NAI-HKNSTR      =   ZERO
               CONTINUE
           ELSE
               PERFORM VARYING IDX     FROM    SPA-NAI-HKNSTR   BY  1
                   UNTIL      (IDX         >   SPA-PTHKN-MAX )  OR
                              (FLG-CHK NOT =   ZERO )
                   IF      SPA-PTHKN-SAKJOKBN (IDX)    =   SPACE
                       MOVE    IDX             TO  SPA-NAI-HKNSTR
                       PERFORM 3101-HOKEN-HEN-SEC
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
           END-IF
      *    対象の保険がない時、新規とする
           IF      FLG-CHK             =   ZERO
               MOVE    ZERO                TO  SPA-NAI-HKNSTR
           END-IF
      *
           .
       3101-HKN-TBL-SPA-EXT.
           EXIT.
      *****************************************************************
      *    公費情報パラメタファイル検索処理
      *****************************************************************
       3101-KOH-TBL-SPA-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOH-AREA
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  SPA-NAI-KOHEND
           IF      SPA-NAI-KOHSTR      =   ZERO
               CONTINUE
           ELSE
      *
           PERFORM VARYING     IDX     FROM    SPA-NAI-KOHSTR BY  1
                   UNTIL      (IDX         >   SPA-PTKOH-MAX )  OR
                              (IDY         >=  3    )
               IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                   IF      IDY             =   ZERO
                       MOVE    IDX             TO  SPA-NAI-KOHSTR
                   END-IF
                   MOVE    IDX                 TO  SPA-NAI-KOHEND
                   ADD     1                   TO  IDY
                   MOVE    1                   TO  FLG-CHK
                   PERFORM 3102-KOUHI-HEN-SEC
               END-IF
           END-PERFORM
           END-IF
      *
      *    対象の保険がない時、新規とする
           IF      FLG-CHK             =   ZERO
               MOVE    ZERO                TO  SPA-NAI-KOHSTR
               MOVE    ZERO                TO  SPA-NAI-KOHEND
           END-IF
      *
           .
       3101-KOH-TBL-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険編集処理
      *****************************************************************
       3101-HOKEN-HEN-SEC              SECTION.
      *
           MOVE    SPA-PTHKN-HKNNUM (IDX)  TO  SPA-GMN-HKNNUM
      *    保険ＩＤ（労災保険のとの関連）
           MOVE    SPA-PTHKN-HKNID  (IDX)  TO  SPA-NAI-HKNID
      *
           MOVE    SPA-PTHKN-HKNJANUM(IDX) TO  SPA-GMN-HKNJANUM
           MOVE    SPA-PTHKN-HIHKNJANAME (IDX)
                                           TO  SPA-GMN-HIHKNJANAME
           MOVE    SPA-PTHKN-HONKZKKBN (IDX)
                                           TO  SPA-GMN-HONKZKKBN
           MOVE    SPA-PTHKN-HOJOKBN (IDX) TO  SPA-GMN-HOJOKBN
           MOVE    SPA-PTHKN-CONTKBN (IDX) TO  SPA-GMN-CONTKBN
           MOVE    SPA-PTHKN-KIGO    (IDX) TO  SPA-GMN-KIGO
           MOVE    SPA-PTHKN-NUM     (IDX) TO  SPA-GMN-NUM
      *
           MOVE    SPA-PTHKN-HKN-SBTKBN(IDX)
                                           TO  SPA-NAI-PTHKN-SBTKBN
           MOVE    SPA-PTHKN-SAKJOKBN(IDX) TO  SPA-NAI-SAKJOKBN
           MOVE    SPA-PTHKN-SKKGETYMD  (IDX)
                                           TO  SPA-NAI-SKKGETYMD
           MOVE    SPA-PTHKN-TEKSTYMD(IDX) TO  SPA-NAI-HTEKSTYMD
           MOVE    SPA-PTHKN-TEKEDYMD(IDX) TO  SPA-NAI-HTEKEDYMD
           MOVE    SPA-PTHKN-KAKUNINYMD (IDX)
                                           TO  SPA-NAI-HKAKUNINYMD
      *
           MOVE    SPA-PTHKN-SKKGETYMDW (IDX)
                                           TO  SPA-GMN-SKKGETYMD
           MOVE    SPA-PTHKN-TEKSTYMDW (IDX)
                                           TO  SPA-GMN-HTEKSTYMD
           MOVE    SPA-PTHKN-TEKEDYMDW (IDX)
                                           TO  SPA-GMN-HTEKEDYMD
           MOVE    SPA-PTHKN-KAKUNINYMDW (IDX)
                                           TO  SPA-GMN-HKAKUNINYMD
      *
      *    前保存
           MOVE    SPA-PTHKN-HKNNUM  (IDX) TO  SPA-MAE-HKNNUM
           MOVE    SPA-PTHKN-HKNJANUM(IDX) TO  SPA-MAE-HKNJANUM
           MOVE    SPA-PTHKN-HOJOKBN (IDX) TO  SPA-MAE-HOJOKBN
           MOVE    SPA-PTHKN-MAEKAKUNINYMD (IDX)
                                           TO  SPA-MAE-HKAKUNINYMD
           MOVE    SPA-PTHKN-MAEKAKUNINYMDW(IDX)
                                           TO  SPA-MAE-HKAKUNINYMDW
           MOVE    SPA-PTHKN-KIGO    (IDX) TO  SPA-MAE-KIGO
           MOVE    SPA-PTHKN-NUM     (IDX) TO  SPA-MAE-NUM
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPID            TO  HKNJA-HOSPID
           MOVE    SPA-GMN-HKNJANUM      TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "HKNJAINF-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNJAINF-KEY"      TO  ORC-DBPATH
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
      *    保険者番号あり
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNJANAME     TO  SPA-HKNJA-HKNJANAME
               MOVE    HKNJA-ADRS          TO  SPA-HKNJA-ADRS
               MOVE    HKNJA-BANTI         TO  SPA-HKNJA-BANTI
               MOVE    HKNJA-TEL           TO  SPA-HKNJA-TEL
      *
               MOVE    HKNJA-IDOYMD        TO  SPA-HKNJA-IDOYMD
               MOVE    SPA-PTHKN-KEIFLG(IDX)
                                           TO  SPA-HKNJA-KEIFLG
           ELSE
               MOVE    SPACE               TO  SPA-HKNJA-HKNJANAME
               MOVE    SPACE               TO  SPA-HKNJA-ADRS
               MOVE    SPACE               TO  SPA-HKNJA-BANTI
               MOVE    SPACE               TO  SPA-HKNJA-TEL
      *
               MOVE    SPACE               TO  SPA-HKNJA-IDOYMD
               MOVE    SPACE               TO  SPA-HKNJA-KEIFLG
           END-IF
      *
      *
      *    保険情報
           MOVE    SPA-GMN-HKNNUM      TO  WRK-HKNNUM
           MOVE    SPA-NAI-HTEKSTYMD   TO  WRK-TEKSTYMD
           MOVE    SPA-NAI-HTEKEDYMD   TO  WRK-TEKEDYMD
           MOVE    ZERO                TO  WRK-PAYKBN
           PERFORM 31012-HKNNUM-SET-SEC
           IF      WRK-HKNKOHSBTKBN    NOT =   SPACE
               MOVE    WRK-TANSEIDONAME    TO  SPA-GMN-HKNNUMMEI
               MOVE    WRK-HKNKOHSBTKBN    TO  SPA-NAI-PTHKN-SBTKBN
           END-IF
      *****PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   SPA-GMN-HKNMAX
      *        IF      SPA-GMN-HKNNUML (IDX)   =   SPA-GMN-HKNNUM
      *            MOVE    SPA-GMN-HKNNUMLST (IDX)
      *                                        TO  SPA-GMN-HKNNUM-G
      *            識別
      *            MOVE    SPA-NAI-HKNSBTKBN (IDX)
      *                                        TO  SPA-NAI-PTHKN-SBTKBN
      *            MOVE    SPA-GMN-HKNMAX      TO  IDX
      *        END-IF
      *****END-PERFORM
      *
      *    本人家族区分等
           PERFORM 31011-HKN-HEN-SEC
      *
      *    労災保険の番号設定
           IF      SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"
               PERFORM 310111-PARA-RSIHKN-SET-SEC
           END-IF
      *
           .
       3101-HOKEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費区分編集処理
      *****************************************************************
       31012-HKNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPID          TO  HKN-HOSPID
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           IF      WRK-PAYKBN          =   SPACE
               MOVE    ZERO                TO  HKN-PAYKBN
           ELSE
               MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           END-IF
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKSTYMD
           MOVE    WRK-TEKEDYMD        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY"    TO  ORC-DBPATH
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    SPACE               TO  WRK-HKNKOHSBTKBN
           MOVE    SPACE               TO  WRK-TANSEIDONAME
           IF      FLG-HKNNUM          =   ZERO
               IF     (WRK-TEKSTYMD        >=  HKN-TEKSTYMD )  AND
                      (WRK-TEKEDYMD        <=  HKN-TEKEDYMD )
                   MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
                   MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
               END-IF
           END-IF
      *
           .
       31012-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    本人家族区分等編集処理
      *****************************************************************
       31011-HKN-HEN-SEC      SECTION.
      *
      *    本人家族
           MOVE    ZERO                TO  SPA-GMN-HONKZK-MAX
           PERFORM VARYING     IDT     FROM    1  BY  1
                   UNTIL       IDT     >   2
               MOVE    WRK-HONKA-REC (IDT)
                                       TO  SPA-GMN-HONKZKKBNL (IDT)
               MOVE    IDT             TO  SPA-GMN-HONKZK-MAX
               IF      SPA-GMN-HONKZKKBNL (IDT)(1:1)
                                           =   SPA-GMN-HONKZKKBN
                   MOVE    SPA-GMN-HONKZKKBNL (IDT)
                                           TO  SPA-GMN-HONKZKKBN-G
               END-IF
      *        自官は本人のみ
               IF      SPA-GMN-HKNNUM      =   "007"
                   MOVE    2               TO  IDT
               END-IF
           END-PERFORM
      *
      *    継続区分
           IF    ( SPA-NAI-PTHKN-SBTKBN    =   "1"  ) AND
                 ( SPA-GMN-HKNNUM      NOT =   "060"  AND "067" )
               IF      SPA-GMN-HKNNUM      =   "002"
                   MOVE    3               TO  WRK-CONT-MAX
               ELSE
                   MOVE    2               TO  WRK-CONT-MAX
               END-IF
               PERFORM VARYING IDT     FROM    1  BY  1
                       UNTIL   IDT     >   WRK-CONT-MAX
                   MOVE    WRK-KEI-REC (IDT)
                                           TO  SPA-GMN-CONTKBNL (IDT)
                   IF      WRK-KEI-REC (IDT)(1:1)
                                           =   SPA-GMN-CONTKBN
                       MOVE    SPA-GMN-CONTKBNL (IDT)
                                               TO  SPA-GMN-CONTKBN-G
                   END-IF
                   MOVE    IDT             TO  SPA-GMN-CONT-MAX
               END-PERFORM
           ELSE
               MOVE    ZERO                TO  SPA-GMN-CONT-MAX
           END-IF
      *
      *    負担割合
           EVALUATE    SPA-GMN-HKNNUM
      *        船員
               WHEN    "002"
                   IF     (SPA-NAI-HTEKSTYMD       >=  "20021001") AND
                          (SPA-BYMD-70             >   ZERO      )  AND
                          (SPA-NAI-HTEKSTYMD       >=  SPA-BYMD-70)
                       MOVE    4               TO  IDY
                       MOVE    11              TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    3               TO  WRK-HJO-MAX
                   END-IF
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1           TO  SPA-GMN-HOJO-MAX
                       MOVE    WRK-HJO-REC1 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                   END-PERFORM
      *        国保
               WHEN    "060"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   4
                       MOVE    WRK-HJO-REC2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  "20021001") AND
                          (SPA-BYMD-70             >   ZERO      )  AND
                          (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-70)
                       MOVE    2               TO  IDY
                       MOVE    5               TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    1               TO  WRK-HJO-MAX
                   END-IF
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1           TO  SPA-GMN-HOJO-MAX
                       MOVE    WRK-HJO-REC5 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                   END-PERFORM
      *        その他
               WHEN    OTHER
      *            自費
                   IF      SPA-GMN-HKNNUM (1:2)    =   "98"
                       PERFORM VARYING     IDT     FROM    1  BY  1
                               UNTIL       IDT     >   2
                           MOVE    WRK-HJO-REC3 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                           MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                       END-PERFORM
                   ELSE
                       MOVE    ZERO        TO  SPA-GMN-HOJO-MAX
      *                Ｈ１４．１０ 改正より
                       IF      SPA-NAI-HTEKSTYMD   >=  "20021001"
                           PERFORM 310111-HKN-RJNHEN-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           PERFORM VARYING     IDT     FROM    1  BY  1
                   UNTIL       IDT     >   SPA-GMN-HOJO-MAX
               IF      SPA-GMN-HOJOKBNL (IDT)(1:1)
                                           =   SPA-GMN-HOJOKBN
                   MOVE    SPA-GMN-HOJOKBNL (IDT)
                                           TO  SPA-GMN-HOJOKBN-G
               END-IF
           END-PERFORM
      *
           .
       31011-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人補助区分編集処理
      *****************************************************************
       310111-HKN-RJNHEN-SEC            SECTION.
      *
      *    適用開始日が７０歳以上で、労災・治験・自費
           IF      (SPA-NAI-HTEKSTYMD      >=  SPA-BYMD-70)  AND
                   (SPA-BYMD-70            >   ZERO       )  AND
                   (SPA-GMN-HKNNUM(1:1) NOT =   "9")
               PERFORM VARYING     IDT     FROM    1  BY  1
                       UNTIL       IDT     >   2
                   MOVE    WRK-HJO-REC4 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                   MOVE    IDT             TO  SPA-GMN-HOJO-MAX
               END-PERFORM
           END-IF
           .
       310111-HKN-RJNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険編集処理
      *****************************************************************
       310111-PARA-RSIHKN-SET-SEC      SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
               IF      PTRSI-HKNID         =   SPA-NAI-HKNID
                   MOVE    PARA-EDANUM     TO  SPA-NAI-RSHKN-EDANUM
                   MOVE    1               TO  FLG-PARA
               ELSE
                   MOVE    "PARA-KEY2"         TO  ORC-DBPATH
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
      *
           .
       310111-PARA-RSIHKN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       3102-KOUHI-HEN-SEC              SECTION.
      *
           MOVE    IDY                 TO  SPA-KOH-MAX
           MOVE    IDX                 TO  SPA-NAI-PTKOH-EDANUM (IDY)
      *
           MOVE    SPA-PTKOH-KOHNUM (IDX)  TO  SPA-GMN-KOHNUM  (IDY)
           MOVE    SPA-PTKOH-FTNJANUM(IDX) TO  SPA-GMN-FTNJANUM (IDY)
           MOVE    SPA-PTKOH-JKYSNUM (IDX) TO  SPA-GMN-JKYSNUM   (IDY)
      *
           IF      SPA-PTKOH-PAYKBN (IDX)  NOT =   ZERO
               MOVE    SPA-PTKOH-PAYKBN (IDX)(2:1)
                                           TO  SPA-GMN-KOHKBN  (IDY)
           ELSE
               MOVE    SPACE               TO  SPA-GMN-KOHKBN  (IDY)
           END-IF
           MOVE    SPA-PTKOH-PAYKBN (IDX)  TO  SPA-NAI-PTKOH-PAYKBN(IDY)
      *
           MOVE    SPA-PTKOH-TEKSTYMD(IDX) TO  SPA-NAI-KTEKSTYMD(IDY)
           MOVE    SPA-PTKOH-TEKEDYMD(IDX) TO  SPA-NAI-KTEKEDYMD (IDY)
           MOVE    SPA-PTKOH-KAKUNINYMD (IDX)
                                           TO  SPA-NAI-KKAKUNINYMD(IDY)
      *
           MOVE    SPA-PTKOH-TEKSTYMDW (IDX)
                                           TO  SPA-GMN-KTEKSTYMD (IDY)
           MOVE    SPA-PTKOH-TEKEDYMDW (IDX)
                                           TO  SPA-GMN-KTEKEDYMD (IDY)
           MOVE    SPA-PTKOH-KAKUNINYMDW (IDX)
                                           TO  SPA-GMN-KKAKUNINYMD (IDY)
      *
           MOVE    SPA-PTKOH-KOHID (IDX)   TO SPA-NAI-PTKOH-KOHID (IDY)
      *
           MOVE    SPA-PTKOH-KOHNUM(IDX)   TO  SPA-MAE-KOHNUM  (IDY)
           IF      SPA-PTKOH-PAYKBN(IDX)   NOT =   ZERO
               MOVE    SPA-PTKOH-PAYKBN(IDX)(2:1)
                                           TO  SPA-MAE-KOHKBN  (IDY)
           END-IF
           MOVE    SPA-PTKOH-FTNJANUM(IDX)
                                           TO  SPA-MAE-FTNJANUM   (IDY)
           MOVE    SPA-PTKOH-MAEKAKUNINYMD (IDX)
                                           TO  SPA-MAE-KKAKUNINYMD(IDY)
           MOVE    SPA-PTKOH-MAEKAKUNINYMDW(IDX)
                                           TO  SPA-MAE-KKAKUNINYMDW(IDY)
      *    新規以外の時老人チェック済みとする
           IF      SPA-MAE-KOHNUM (IDY)    NOT =   SPACE
               MOVE    1                   TO  SPA-NAI-AGE65 (IDY)
           END-IF
      *
      *    公費情報
           MOVE    SPA-GMN-KOHNUM (IDY)    TO  WRK-HKNNUM
           MOVE    SPA-NAI-KTEKSTYMD(IDY)  TO  WRK-TEKSTYMD
           MOVE    SPA-NAI-KTEKEDYMD(IDY)  TO  WRK-TEKEDYMD
           MOVE    SPA-NAI-PTKOH-PAYKBN(IDY)
                                           TO  WRK-PAYKBN
           IF     (WRK-HKNNUM          =   "027"  )  AND
                  (WRK-PAYKBN          =   ZERO  OR  SPACE )
               MOVE    SPA-HOSPSBT         TO  WRK-PAYKBN(1:1)
               MOVE    SPA-ROUPAYKBN       TO  WRK-PAYKBN(2:1)
           END-IF
           PERFORM 31012-HKNNUM-SET-SEC
           IF     (SPA-GMN-KOHNUM (IDY)        =   "027" )  AND
                  (SPA-NAI-PTKOH-PAYKBN(IDY)   =   ZERO  )
               MOVE    "老人保健"          TO  WRK-TANSEIDONAME
               MOVE    "3"                 TO  WRK-HKNKOHSBTKBN
           END-IF
           MOVE    WRK-TANSEIDONAME    TO  SPA-GMN-KOHNUMMEI(IDY)
           MOVE    WRK-TANSEIDONAME    TO  SPA-MAE-KOHNUMMEI(IDY)
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-NAI-PTKOH-SBTKBN(IDY)
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPID            TO  HKNJA-HOSPID
           MOVE    SPA-PTKOH-FTNJANUM(IDX)
                                         TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "HKNJAINF-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNJAINF-KEY"      TO  ORC-DBPATH
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
      *    保険者番号あり
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-IDOYMD        TO  SPA-PTKOH-IDOYMD(IDY)
               MOVE    SPA-PTKOH-KEIFLG(IDX)
                                           TO  SPA-NAI-PTKOH-KEIFLG(IDY)
           ELSE
               MOVE    SPACE               TO  SPA-PTKOH-IDOYMD(IDY)
               MOVE    SPACE               TO  SPA-NAI-PTKOH-KEIFLG(IDY)
           END-IF
      *
      *
      **** PERFORM VARYING     IDX-K   FROM    1   BY  1
      *            UNTIL       IDX-K   >   SPA-GMN-KOHMAX
      *        IF     (SPA-GMN-KOHNUML (IDX-K)
      *                                    =   SPA-GMN-KOHNUM (IDY)) AND
      *               (SPA-GMN-KOHKBNL (IDX-K)
      *                                    =   SPA-GMN-KOHKBN (IDY))
      *            MOVE    SPA-GMN-KOHNUMLST (IDX-K)
      *                                        TO  SPA-GMN-KOHNUM-G(IDY)
      *            MOVE    SPA-GMN-KOHNUMLST (IDX-K)
      *                                        TO  SPA-MAE-KOHNUM-G(IDY)
      *            MOVE    SPA-GMN-KOHMAX      TO  IDX-K
      *        END-IF
      *****END-PERFORM
           .
       3102-KOUHI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示（現在対象分のみ）処理
      *****************************************************************
       3103-HKNCOMB-PARA-SEC        SECTION.
      *
           INITIALIZE                  SPA-HKNCOMBI-AREA
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   (FLG-PARA       NOT =   ZERO      )  OR
                       (IDX                >=  15        )
               MOVE    PARA-DATA-REC       TO  HKNCOMBI-REC
      *
               IF     (COMB-DELKBN     NOT =   "1"       )  AND
                      (SPA-SYSYMD          >=  COMB-TEKSTYMD  AND
                       SPA-SYSYMD          <=  COMB-TEKEDYMD )
                   ADD     1               TO  SPA-HKNCOMBI-MAX
                   ADD     1               TO  IDX
      *
                   MOVE    COMB-HKNCOMBINUM    TO  SPA-GMN-THKNCOMBINUM
                                                              (IDX)
                   MOVE    COMB-HKNNUM         TO  SPA-GMN-THKNNUM
                                                              (IDX)
                   MOVE    COMB-HKNID          TO  SPA-GMN-THKNID
                                                              (IDX)
      *
                   MOVE    COMB-TEKSTYMD       TO  WRK-SYMD
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  SPA-GMN-TTEKSTYMD
                                                              (IDX)
                   MOVE    COMB-TEKEDYMD       TO  WRK-SYMD
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  SPA-GMN-TTEKEDYMD
                                                              (IDX)
      *
                   PERFORM 31031-HKNCOMBI-HEN-SEC
                   MOVE    WRK-HKNCOMBI        TO  SPA-GMN-THKNCOMBIMEI
                                                              (IDX)
      *            内部
                   MOVE    COMB-TEKSTYMD       TO  SPA-NAI-TTEKSTYMD
                                                              (IDX)
                   MOVE    COMB-TEKEDYMD       TO  SPA-NAI-TTEKEDYMD
                                                              (IDX)
               END-IF
      *
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    労災保険の時コメントを表示する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNCOMBI-MAX
               IF      SPA-GMN-THKNNUM (IDX)   =   "971"
                                               OR  "973"
                   PERFORM 31032-RSI-HKNCOMBI-HEN-SEC
               END-IF
           END-PERFORM
      *
      *    保険適用日順にソートする
           PERFORM 30133-HKNCOMBI-SORT-SEC
      *
           .
       3103-HKNCOMB-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せソート処理
      *****************************************************************
       30133-HKNCOMBI-SORT-SEC             SECTION.
      *
           INITIALIZE                  WRK-HKNCOBMI-SORT-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   15
               MOVE    ALL "9"         TO  WRK-SRT-TEKISTYMD
               MOVE    ALL "9"         TO  WRK-SRT-HKNCOMBINUM
               MOVE    ZERO            TO  WRK-SRT-IDX
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2     >   SPA-HKNCOMBI-MAX
                   IF      SPA-GMN-THKNCOMBINUM (IDX2) =   ZERO
                       CONTINUE
                   ELSE
                       PERFORM 301331-SORTSYORI-SEC
                   END-IF
               END-PERFORM
               IF      WRK-SRT-IDX     NOT =   ZERO
                   MOVE    SPA-GMN-HKNCOMBI-LIST (WRK-SRT-IDX)
                                       TO  WRK-GMN-HKNCOMBI-LIST(IDX)
                   INITIALIZE              SPA-GMN-HKNCOMBI-LIST
                                                         (WRK-SRT-IDX)
               END-IF
           END-PERFORM
           MOVE    ZERO                TO   SPA-HKNCOMBI-MAX
           INITIALIZE                  SPA-HKNCOMBI-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   15
               IF      WRK-GMN-THKNCOMBINUM (IDX) NOT =   ZERO
                   ADD     1           TO   SPA-HKNCOMBI-MAX
                   MOVE    WRK-GMN-HKNCOMBI-LIST (IDX)
                                       TO  SPA-GMN-HKNCOMBI-LIST
                                                  (SPA-HKNCOMBI-MAX)
               END-IF
           END-PERFORM
      *
           .
       30133-HKNCOMBI-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       301331-SORTSYORI-SEC             SECTION.
      *
           IF      WRK-SRT-TEKISTYMD   >   SPA-NAI-TTEKSTYMD  (IDX2)
               MOVE    IDX2                    TO  WRK-SRT-IDX
               MOVE    SPA-NAI-TTEKSTYMD (IDX2) TO  WRK-SRT-TEKISTYMD
               MOVE    SPA-GMN-THKNCOMBINUM(IDX2)
                                               TO  WRK-SRT-HKNCOMBINUM
           ELSE
               IF      WRK-SRT-TEKISTYMD   =   SPA-NAI-TTEKSTYMD(IDX2)
                   IF      WRK-SRT-HKNCOMBINUM
                                               >   SPA-GMN-THKNCOMBINUM
                                                              (IDX2)
                       MOVE    IDX2                TO  WRK-SRT-IDX
                       MOVE    SPA-NAI-TTEKSTYMD (IDX2)
                                                   TO  WRK-SRT-TEKISTYMD
                       MOVE    SPA-GMN-THKNCOMBINUM(IDX2)
                                                   TO
                                                   WRK-SRT-HKNCOMBINUM
                   END-IF
               END-IF
           END-IF
           .
       301331-SORTSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       31031-HKNCOMBI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
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
           .
       31031-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険組合せ編集処理
      *****************************************************************
       31032-RSI-HKNCOMBI-HEN-SEC      SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
               IF      PTRSI-HKNID         =   SPA-GMN-THKNID (IDX)
                   IF      PTRSI-COMMENT       =   SPACE
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
                       STRING  SPA-GMN-THKNCOMBIMEI (IDX)
                                                   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBI
                       END-STRING
                   ELSE
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
                       STRING  SPA-GMN-THKNCOMBIMEI (IDX)
                                                   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI
                       END-STRING
                   END-IF
                   MOVE    WRK-HKNCOMBI    TO  SPA-GMN-THKNCOMBIMEI
                                                                   (IDX)
                   MOVE    1               TO  FLG-PARA
               END-IF
      *
               IF      FLG-PARA         =   ZERO
                   MOVE    "PARA-KEY2"         TO  ORC-DBPATH
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       31032-RSI-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険更新画面処理
      *****************************************************************
       4910-RSKOH-PARAOUT-SEC         SECTION.
      *    処理なし
           IF      SPA-RSPAR-FLG      =   "1"
               MOVE    8              TO  SPA-GMN-CUR
               GO      TO      4910-RSKOH-PARAOUT-EXT
           END-IF
      *
      *    削除処理
           IF      SPA-NAI-SAKJOKBN    =   "1"
               MOVE    "/D"            TO  SPA-GMN-HKNJANUM(1:2)
               PERFORM 4101-KHNJANUM-CHK-SEC
           END-IF
      *    保険画面の内容をＴＢＬへ編集する
           PERFORM 41031-HKN-SPA-TBL-SET-SEC
      *    登録ボタンへ
           MOVE    60                  TO  SPA-GMN-CUR
      *
           .
       4910-RSKOH-PARAOUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    死亡区分
               WHEN    "CLICKED"       ALSO    "P02_DEATHKBN"
                   PERFORM 490-DEATHKBN-SEC
      *    保険組合せ
               WHEN    "CLICKED"       ALSO    "B05"
                   MOVE    ZERO        TO  FLG-TOUROKU 
                   MOVE    1           TO  FLG-TOUROKU2
                   PERFORM 450-HKNCOMBI-SEC
      *    保険追加
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 460-HKN-TUIKA-SEC
      *    公費追加
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 470-KOH-TUIKA-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    [前↑]押下
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 440-ZEN-GMNHYOUJI-SEC
      *    [次↓]押下
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 450-TUGI-GMNHYOUJI-SEC
      *    保険チェック
               WHEN    "CLICKED"       ALSO    "BH1"
                   PERFORM 450-HKNKAKUNINYMD-ZUMI-SEC
      *    労災・自賠責入力画面へ
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 481-ROUSAI-SEC
      *    公費チェック
               WHEN    "CLICKED"       ALSO    "BK1"
               WHEN    "CLICKED"       ALSO    "BK2"
               WHEN    "CLICKED"       ALSO    "BK3"
                   PERFORM 460-KOHKAKUNINYMD-ZUMI-SEC
      *    別画面
               WHEN    "CLICKED"       ALSO     ANY
                   PERFORM 4101-BETUGAMEN-SEC
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
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
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
      *****MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    保険組合せ選択
               WHEN    ANY             ALSO    "HKNLIST"
                   PERFORM 4001-HKNLIST-SEL-SEC
           END-EVALUATE
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       4001-HKNLIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-GMN-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   30
               IF      P02-HKNLIST-SELECT (IDX)    =   "T"
                   IF     (P02-HKNCOBNUM (IDX)     =   SPACE ) AND
                          (IDX                     >   1     )
                       MOVE    P02-HKNCOBNUM (IDX - 1) TO  WRK-SELNUM
                   ELSE
                       MOVE    P02-HKNCOBNUM (IDX)     TO  WRK-SELNUM
                   END-IF
                   IF     (WRK-SELNUM          NUMERIC   )  AND
                          (WRK-SELNUM      NOT =   SPA-GMN-SELNUM )
                       MOVE    WRK-SELNUM      TO  WRK-GMN-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-GMN-SELNUM      >   ZERO
               MOVE    WRK-GMN-SELNUM      TO  P02-P02-SELNUM
               MOVE    38                  TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4001-HKNLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU         =   1
               PERFORM 4109-TOUROKU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      410-INPUT-CHK-EXT
               END-IF
           END-IF
      *    保険・公費のテーブルセットとチェック
           IF      SPA-HKNKOH-FLG      =   1
               PERFORM 4103-HKNKOH-TBL-SEC
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    公費に追加があった時、最初の１行を追加する
           IF      FLG-KOH-ADD         =   1
               PERFORM 4104-KOH-ADD-SEC
           END-IF
      *
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
           MOVE    1                   TO  SPA-P02A-FLG1
           MOVE    1                   TO  SPA-P02A-FLG2
      *
      *    かな氏名
           IF      P02-P02-KANANAME    =   SPA-MAE-KANANAME
               MOVE    ZERO                TO  FLG-IN-KANANAME
           ELSE
               MOVE    1                   TO  FLG-IN-KANANAME
               MOVE    ZERO                TO  SPA-P02A-FLG1
               MOVE    ZERO                TO  SPA-P02A-FLG2
           END-IF
           MOVE    P02-P02-KANANAME    TO  SPA-GMN-KANANAME
      *    漢字氏名
           IF      P02-P02-NAME        =   SPA-MAE-NAME
               MOVE    ZERO                TO  FLG-IN-NAME
           ELSE
               MOVE    1                   TO  FLG-IN-NAME
               MOVE    ZERO                TO  SPA-P02A-FLG1
               MOVE    ZERO                TO  SPA-P02A-FLG2
           END-IF
           MOVE    P02-P02-NAME        TO  SPA-GMN-NAME
      *    氏名変更
           IF      SPA-GMN-NAME    NOT =   SPA-HZN-NAME
               MOVE    1                   TO  SPA-HEN-FLG
           END-IF
      *
           MOVE    P02-P02-NICKNAME    TO  SPA-GMN-NICKNAME
      *
           IF      P02-P02-SEX(1:1)    =   SPA-MAE-SEX
               MOVE    ZERO                TO  FLG-IN-SEX
           ELSE
               MOVE    1                   TO  FLG-IN-SEX
               MOVE    ZERO                TO  SPA-P02A-FLG2
           END-IF
           MOVE    P02-P02-SEX         TO  SPA-GMN-SEX-G
      *
           IF      P02-P02-BIRTHDAY    =   SPA-MAE-BIRTHDAY
               MOVE    ZERO                TO  FLG-IN-BIRTHDAY
           ELSE
               MOVE    1                   TO  FLG-IN-BIRTHDAY
               MOVE    ZERO                TO  SPA-P02A-FLG2
           END-IF
           MOVE    P02-P02-BIRTHDAY    TO  SPA-GMN-BIRTHDAYW
      *
           MOVE    P02-TSTPTNUMKBN     TO  SPA-GMN-TSTPTNUMKBN-G
           MOVE    P02-P02-HAKKO       TO  SPA-GMN-HAKKO-G
      *
      *
           IF      P02-P02-POST        =   SPA-MAE-POST
               MOVE    ZERO                TO  FLG-IN-POST
           ELSE
               MOVE    1                   TO  FLG-IN-POST
           END-IF
           MOVE    P02-P02-POST        TO  SPA-GMN-POST
           MOVE    P02-P02-ADRS        TO  SPA-GMN-EDITADRS-NAME
           MOVE    P02-P02-TEL2        TO  SPA-GMN-TEL2
           MOVE    P02-P02-BANTI       TO  SPA-GMN-BANTI
           MOVE    P02-P02-TEL1        TO  SPA-GMN-TEL1
           MOVE    P02-P02-SETAINUSI   TO  SPA-GMN-SETAINUSI
           MOVE    P02-P02-ZOKUGARA    TO  SPA-GMN-ZOKUGARA
           MOVE    P02-P02-DISCOUNTKBN TO  SPA-GMN-DISCOUNTKBN-G
           MOVE    P02-P02-DISCOUNT    TO  SPA-GMN-DISCOUNT
           MOVE    P02-P02-CONDITION1  TO  SPA-GMN-CONDITION(1)
           MOVE    P02-P02-CONDITION2  TO  SPA-GMN-CONDITION(2)
           MOVE    P02-P02-CONDITION3  TO  SPA-GMN-CONDITION(3)
           MOVE    P02-P02-COMMENT1    TO  SPA-GMN-COMMENT-1
           MOVE    P02-P02-COMMENT2    TO  SPA-GMN-COMMENT-2
           MOVE    P02-P02-TABOO1      TO  SPA-GMN-TABOO-1
           MOVE    P02-P02-TABOO2      TO  SPA-GMN-TABOO-2
           MOVE    P02-P02-ALLERGY1    TO  SPA-GMN-ALLERGY-1
           MOVE    P02-P02-ALLERGY2    TO  SPA-GMN-ALLERGY-2
           MOVE    P02-P02-KANSENSYO1  TO  SPA-GMN-KANSENSYO-1
           MOVE    P02-P02-KANSENSYO2  TO  SPA-GMN-KANSENSYO-2
      *
           MOVE    P02-P02-SELNUM      TO  SPA-GMN-SELNUM
           MOVE    P02-P02-SRYKA       TO  SPA-GMN-SRYKA-G
      *    保険情報
           IF      SPA-MAE-HKNNUM      =   "971"
                                       OR  "973"
      *        労災の時、保険情報に入力がないこと
               PERFORM 41011-ROUSAI-CHK-SEC
           ELSE
      *        保険者番号変更時に異動チェックをクリアする
               IF      P02-P02-HKNJANUM    NOT =   SPA-GMN-HKNJANUM
                   MOVE    SPACE               TO  SPA-HKNJA-KEIFLG
               END-IF
               MOVE    P02-P02-HKNJANUM    TO  SPA-GMN-HKNJANUM
               MOVE    P02-P02-HKNNUM      TO  SPA-GMN-HKNNUM-G
               MOVE    P02-P02-HONKZKBN    TO  SPA-GMN-HONKZKKBN-G
               MOVE    P02-P02-HOJOKBN     TO  SPA-GMN-HOJOKBN-G
               MOVE    P02-P02-CONTKBN     TO  SPA-GMN-CONTKBN-G
               MOVE    P02-P02-KIGO        TO  SPA-GMN-KIGO
               MOVE    P02-P02-NUM         TO  SPA-GMN-NUM
               MOVE    P02-P02-SKKGETYMD   TO  SPA-GMN-SKKGETYMD
               MOVE    P02-P02-HTEKSTYMD   TO  SPA-GMN-HTEKSTYMD
               MOVE    P02-P02-HTEKEDYMD   TO  SPA-GMN-HTEKEDYMD
               MOVE    P02-P02-HIHKNJANAME TO  SPA-GMN-HIHKNJANAME
               MOVE    P02-P02-HKAKUNINYMD TO  SPA-GMN-HKAKUNINYMD
      *
           END-IF
      *
      *    保険者番号変更時に異動チェックをクリアする
           IF      P02-P02-FTNJANUM1   NOT =   SPA-GMN-FTNJANUM (1)
               MOVE    SPACE               TO  SPA-NAI-PTKOH-KEIFLG(1)
           END-IF
           IF      P02-P02-FTNJANUM2   NOT =   SPA-GMN-FTNJANUM (2)
               MOVE    SPACE               TO  SPA-NAI-PTKOH-KEIFLG(2)
           END-IF
           IF      P02-P02-FTNJANUM3   NOT =   SPA-GMN-FTNJANUM (3)
               MOVE    SPACE               TO  SPA-NAI-PTKOH-KEIFLG(3)
           END-IF
      *    公費１
           MOVE    P02-P02-FTNJANUM1   TO  SPA-GMN-FTNJANUM (1)
           MOVE    P02-P02-KOHNUM1     TO  SPA-GMN-KOHNUM-G (1)
           MOVE    P02-P02-JKYSNUM1    TO  SPA-GMN-JKYSNUM  (1)
           IF      P02-P02-KTEKSTYMD1  NOT =   SPA-GMN-KTEKSTYMD (1)
               MOVE    ZERO             TO  SPA-NAI-AGE65    (1)
           END-IF
           MOVE    P02-P02-KTEKSTYMD1  TO  SPA-GMN-KTEKSTYMD (1)
           MOVE    P02-P02-KTEKEDYMD1  TO  SPA-GMN-KTEKEDYMD (1)
           MOVE    P02-P02-KKAKUNINYMD1
                                       TO  SPA-GMN-KKAKUNINYMD(1)
      *    公費２
           MOVE    P02-P02-FTNJANUM2   TO  SPA-GMN-FTNJANUM (2)
           MOVE    P02-P02-KOHNUM2     TO  SPA-GMN-KOHNUM-G (2)
           MOVE    P02-P02-JKYSNUM2    TO  SPA-GMN-JKYSNUM  (2)
           IF      P02-P02-KTEKSTYMD2  NOT =   SPA-GMN-KTEKSTYMD (2)
               MOVE    ZERO             TO  SPA-NAI-AGE65    (2)
           END-IF
           MOVE    P02-P02-KTEKSTYMD2  TO  SPA-GMN-KTEKSTYMD (2)
           MOVE    P02-P02-KTEKEDYMD2  TO  SPA-GMN-KTEKEDYMD (2)
           MOVE    P02-P02-KKAKUNINYMD2
                                       TO  SPA-GMN-KKAKUNINYMD(2)
      *    公費３
           MOVE    P02-P02-FTNJANUM3   TO  SPA-GMN-FTNJANUM (3)
           MOVE    P02-P02-KOHNUM3     TO  SPA-GMN-KOHNUM-G (3)
           MOVE    P02-P02-JKYSNUM3    TO  SPA-GMN-JKYSNUM  (3)
           IF      P02-P02-KTEKSTYMD3  NOT =   SPA-GMN-KTEKSTYMD (3)
               MOVE    ZERO             TO  SPA-NAI-AGE65    (3)
           END-IF
           MOVE    P02-P02-KTEKSTYMD3  TO  SPA-GMN-KTEKSTYMD (3)
           MOVE    P02-P02-KTEKEDYMD3  TO  SPA-GMN-KTEKEDYMD (3)
           MOVE    P02-P02-KKAKUNINYMD3
                                       TO  SPA-GMN-KKAKUNINYMD(3)
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の保険情報入力チェック処理
      *****************************************************************
       41011-ROUSAI-CHK-SEC          SECTION.
      *
           IF     (P02-P02-HKNJANUM    =   SPA-GMN-HKNJANUM ) AND
                  (P02-P02-HKNNUM      =   SPA-GMN-HKNNUM-G ) AND
                  (P02-P02-HONKZKBN    =   SPA-GMN-HONKZKKBN-G) AND
                  (P02-P02-HOJOKBN     =   SPA-GMN-HOJOKBN-G) AND
                  (P02-P02-CONTKBN     =   SPA-GMN-CONTKBN-G) AND
                  (P02-P02-KIGO        =   SPA-GMN-KIGO     ) AND
                  (P02-P02-NUM         =   SPA-GMN-NUM      ) AND
                  (P02-P02-SKKGETYMD   =   SPA-GMN-SKKGETYMD) AND
                  (P02-P02-HTEKSTYMD   =   SPA-GMN-HTEKSTYMD) AND
                  (P02-P02-HTEKEDYMD   =   SPA-GMN-HTEKEDYMD) AND
                  (P02-P02-HIHKNJANAME =   SPA-GMN-HIHKNJANAME) AND
                  (P02-P02-HKAKUNINYMD =   SPA-GMN-HKAKUNINYMD)
               MOVE    ZERO            TO  FLG-IN-HKN
           ELSE
               MOVE    P02-P02-HKNJANUM    TO  SPA-GMN-HKNJANUM
               MOVE    1               TO  FLG-IN-HKN
           END-IF
           .
      *
       41011-ROUSAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    カナ氏名
           PERFORM 4101-KANANAME-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    漢字氏名
           PERFORM 4101-NAME-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    性別
           PERFORM 4101-SEX-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    生年月日
           PERFORM 4101-BIRTHDAY-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    通称
           PERFORM 4101-NICKNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    テスト患者
           PERFORM 4101-TSTPTNUMKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    カルテ発行
           PERFORM 4101-HAKKO-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    保険者番号
           PERFORM 4101-KHNJANUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           IF      FLG-HKN-DEL         =   ZERO
      *        保険情報チェックサブ
               CALL    "ORCSP03A"          USING   SPA-AREA
                                                   SPA-P02SCRAREA
                                                   SPA-P02KYOUTU
      *        タグ切り替えの時、期間重複はクリアする
               IF      SPA-TAGU-FLG        =   1
                   IF      SPA-ERRCD       =   "0064"  OR
                                               "0089"
                       MOVE   SPACE            TO  SPA-ERRCD
                   END-IF
                   IF      SPA-PIDCD   NOT =   SPACE
                       MOVE   SPACE            TO  SPA-PIDCD
                   END-IF
               END-IF
      *
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO      TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    公費
           PERFORM 4102-KOH-CHKALL-SEC
      *
      *    公費情報チェックサブ
           CALL    "ORCSP03B"          USING   SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
      *    タグ切り替えの時、期間重複はクリアする
           IF      SPA-TAGU-FLG        =   1
               IF      SPA-ERRCD       =   "0088"  OR
                                           "0089"
                   MOVE   SPACE            TO  SPA-ERRCD
               END-IF
               IF      SPA-PIDCD   NOT =   SPACE
                   MOVE   SPACE            TO  SPA-PIDCD
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    郵便番号
           PERFORM 4101-POST-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    住所
           PERFORM 4101-ADRS-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    番地方書
           PERFORM 4101-BANTI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    世帯主
           PERFORM 4101-SETAINUSI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    続柄
           PERFORM 4101-ZOKUGARA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    減免事由
           PERFORM 4101-DISCOUNTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    割引率
           PERFORM 4101-DISCOUNT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    状態１から３
           PERFORM 4101-CONDITIONH-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    状態１から３
           PERFORM 4101-CONDITIONH-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    保険組合せ選択
           PERFORM 4101-SELNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    診療科選択
           PERFORM 4101-SRYKA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号入力チェック処理
      *****************************************************************
       4101-KHNJANUM-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HKN-DEL
      *    労災で入力があった時、エラー
           IF      FLG-IN-HKN          =   1
      *
      *        追加はＯＫ
               IF      SPA-GMN-HKNJANUM(1:2)   =   "/A"  OR  "/a"
                   INITIALIZE                  SPA-GMN-HKN-AREA
                   MOVE    ZERO                TO  SPA-NAI-HKNSTR
                   MOVE    8                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  FLG-HKN-DEL
                   GO      TO      4101-KHNJANUM-CHK-EXT
               END-IF
               MOVE    "0043"          TO  SPA-ERRCD
               MOVE    61              TO  SPA-GMN-CUR
               GO      TO      4101-KHNJANUM-CHK-EXT
           END-IF
      *    削除の判定
           IF      SPA-GMN-HKNJANUM(1:2)   =   "/D"  OR  "/d"
      *        労災の時、労災保険画面へ
               IF      SPA-GMN-HKNNUM      =   "971"
                                           OR  "973"
                   MOVE    "0043"          TO  SPA-ERRCD
                   MOVE    61              TO  SPA-GMN-CUR
                   GO      TO      4101-KHNJANUM-CHK-EXT
               END-IF
      *        新規の時、ＴＢＬも削除
               IF     (SPA-NAI-HKNSTR      >   ZERO      ) AND
                      (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                           =    "1"           )
                   MOVE    SPACE           TO  SPA-PTHKNINF-TBL
                                                     (SPA-NAI-HKNSTR)
                   INITIALIZE                  SPA-PTHKNINF-TBL
                                                     (SPA-NAI-HKNSTR)
      *            保険テーブル空白行削除処理
                   PERFORM 410311-HKNTBL-DEL-SEC
               ELSE
                   IF      SPA-NAI-HKNSTR      >   ZERO
                       MOVE    "1"             TO  SPA-PTHKN-SAKJOKBN
                                                       (SPA-NAI-HKNSTR)
                       MOVE    "1"             TO  SPA-NAI-SAKJOKBN
                   END-IF
               END-IF
               MOVE    1                   TO  FLG-HKN-DEL
               INITIALIZE                  SPA-GMN-HKN-AREA
               IF      SPA-NAI-HKNSTR      =   ZERO
                   MOVE    1               TO  SPA-NAI-HKNSTR
               END-IF
      *        次の対象を表示する
      *        保険情報ＴＢＬから編集
               PERFORM 3101-HKN-TBL-SPA-SEC
               MOVE    8                   TO  SPA-GMN-CUR
               GO      TO      4101-KHNJANUM-CHK-EXT
           END-IF
      *
      *    追加の判定
           IF      SPA-GMN-HKNJANUM(1:2)   =   "/A"  OR  "/a"
               INITIALIZE                  SPA-GMN-HKN-AREA
               MOVE    ZERO                TO  SPA-NAI-HKNSTR
               MOVE    8                   TO  SPA-GMN-CUR
               MOVE    1                   TO  FLG-HKN-DEL
           END-IF
      *
           .
       4101-KHNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費入力チェック処理
      *****************************************************************
       4102-KOH-CHKALL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KOH-ADD
           MOVE    ZERO                TO  FLG-KOH-DEL
      *    削除の判定
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL       IDX-K   >   3
               PERFORM 41021-KOH-DEL-CHK-SEC
           END-PERFORM
      *    Ｂ０７Ｓ押下時、最後の行が入力済みであること
           IF      FLG-KOH-ADD2        =   1
               IF     (SPA-GMN-KOHNUM(3)    NOT =   SPACE ) AND
                      (SPA-GMN-KTEKSTYMD(3) NOT =   SPACE )
                   MOVE    1                TO  FLG-KOH-ADD
               END-IF
           END-IF
      *
      *    画面の空白行をつめる
           PERFORM 41022-PTKOH-TBL-SET-SEC
      *    次のデータを編集する
           IF      FLG-KOH-DEL         =   1
               PERFORM 41023-PTKOH-SAI-SET-SEC
           END-IF
           .
      *
       4102-KOH-CHKALL-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費削除処理
      ****************************************************************
       41021-KOH-DEL-CHK-SEC      SECTION.
      *
      *    削除の判定
           IF      SPA-GMN-FTNJANUM(IDX-K)(1:2)
                                        =   "/D"  OR  "/d"
               MOVE    SPA-NAI-PTKOH-EDANUM(IDX-K)  TO  IDX-KOH
      *        新規の時、ＴＢＬも削除
               IF     (IDX-KOH             >   ZERO      ) AND
                      (SPA-PTKOH-SINKIKBN (IDX-KOH)
                                           =    "1"      )
                   MOVE    SPACE           TO  SPA-PTKOHINF-TBL
                                                     (IDX-KOH)
                   INITIALIZE                  SPA-PTKOHINF-TBL
                                                     (IDX-KOH)
               ELSE
                   IF      IDX-KOH         >   ZERO
                       MOVE    "1"             TO  SPA-PTKOH-SAKJOKBN
                                                       (IDX-KOH)
                   END-IF
               END-IF
               MOVE    1                   TO  FLG-KOH-DEL
               INITIALIZE                  SPA-GMN-KOH-TBL(IDX-K)
               EVALUATE    IDX-K
                   WHEN    1
                       MOVE    20          TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    26          TO  SPA-GMN-CUR
                   WHEN    3
                        MOVE    32          TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *    追加の判定（どの行で入力されても、最初の1行のみを開ける）
           IF      SPA-GMN-FTNJANUM(IDX-K)(1:2)
                                        =   "/A"  OR  "/a"
      *        最後の行が入力済みであること
               IF     (SPA-GMN-KOHNUM(3)    NOT =   SPACE ) AND
                      (SPA-GMN-KTEKSTYMD(3) NOT =   SPACE )
                   MOVE    1                TO  FLG-KOH-ADD
               END-IF
               MOVE    SPA-MAE-FTNJANUM(IDX-K)
                                        TO  SPA-GMN-FTNJANUM(IDX-K)
               EVALUATE    IDX-K
                   WHEN    1
                       MOVE    20          TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    26          TO  SPA-GMN-CUR
                   WHEN    3
                        MOVE    32          TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
           .
      *
       41021-KOH-DEL-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費空白削除編集処理
      ****************************************************************
       41022-PTKOH-TBL-SET-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOH-AREA    TO  WRK-GMN-KOH-AREA
           INITIALIZE                      SPA-GMN-KOH-AREA
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
              IF      WRK-GMN-HOK-G (IDX)  NOT =   SPACE
                   ADD     1               TO  IDX-K
                   MOVE    WRK-GMN-KOH-TBL (IDX)
                                           TO  SPA-GMN-KOH-TBL (IDX-K)
               END-IF
           END-PERFORM
      *    最大を編集する
           MOVE    IDX-K               TO  SPA-KOH-MAX
           .
      *
       41022-PTKOH-TBL-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費削除編集処理
      ****************************************************************
       41023-PTKOH-SAI-SET-SEC      SECTION.
      *
      *    空いた行へ次ぎのデータを編集する
           MOVE    SPA-KOH-MAX         TO  IDX-K
           IF      IDX-K               <   3
               IF     (IDX-K               >   ZERO )  AND
                      (SPA-NAI-PTKOH-EDANUM(IDX-K) NOT =   ZERO)
                   MOVE    SPA-NAI-PTKOH-EDANUM(IDX-K) TO  IDX-K2
                   ADD     1                   TO  IDX-K2
               ELSE
                   MOVE    1                   TO  IDX-K2
               END-IF
               MOVE    IDX-K               TO  IDY
               PERFORM VARYING     IDX     FROM    IDX-K2 BY  1
                       UNTIL      (IDX     >   SPA-PTKOH-MAX )  OR
                                  (IDY     >=  3    )
                   IF     (SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE) AND
                          (SPA-PTKOH-KOHNUM (IDX)  NOT =   SPACE )
                       ADD     1                   TO  IDY
                       MOVE    IDY                 TO  SPA-KOH-MAX
                       PERFORM 3102-KOUHI-HEN-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           .
      *
       41023-PTKOH-SAI-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    カナ氏名チェック処理
      ****************************************************************
       4101-KANANAME-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-KANANAME    =   SPACE
               MOVE    SPA-GMN-KANANAME    TO  SPA-MAE-KANANAME
               GO      TO      4101-KANANAME-CHK-EXT
           END-IF
      *
      *    半角空白を全角空白へ
           MOVE    SPA-GMN-KANANAME    TO  WRK-MAE-INPUT
           PERFORM 890-HALF-ZEN-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           IF      FLG-ERR             =   1
               MOVE    "0025"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4101-KANANAME-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-KANANAME    TO  SPA-MAE-KANANAME
      *
      *    漢字氏名検索処理
           IF     (FLG-IN-KANANAME         =   1    )  AND
                  (SPA-GMN-KANANAME    NOT =   SPA-GMN-OLD-KANANAME)
               PERFORM 4101-KANJI-NAME-EDIT-SEC
           END-IF
      *
           .
       4101-KANANAME-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    漢字氏名チェック処理
      ****************************************************************
       4101-NAME-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-NAME    NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-NAME        TO  WRK-MAE-INPUT
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
               IF      FLG-ERR             =   1
                   MOVE    "0026"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
                   GO      TO      4101-NAME-CHK-EXT
               END-IF
           END-IF
      *
      *    氏名変更時、保険本人を表示中の場合
           IF     (SPA-HEN-FLG         =   1   )    AND
                  (SPA-GMN-HKNNUM  NOT =   SPACE )  AND
                  (SPA-GMN-HONKZKKBN   =   "1"   )  AND
                  (SPA-GMN-HIHKNJANAME NOT =   SPACE )
      *        本人の場合、氏名を編集する
               IF      (SPA-HZN-NAME       =   SPA-GMN-HIHKNJANAME) OR
                       (SPA-MAE-NAME       =   SPA-GMN-HIHKNJANAME)
                   MOVE    SPA-GMN-NAME    TO  SPA-GMN-HIHKNJANAME
               END-IF
           END-IF
      *
      *
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
      *    見出しへ
           MOVE    SPA-GMN-NAME        TO  SPA-GMN-NAMEH
      *    世帯主へ
           IF      SPA-GMN-SETAINUSI   =   SPACE
               MOVE    SPA-GMN-NAME        TO  SPA-GMN-SETAINUSI
           END-IF
      *
      *    同姓同名
           IF       SPA-P02A-FLG1      =   ZERO
               MOVE    1               TO  FLG-P02A-SYORI
               MOVE    1               TO  SPA-P02A-FLG1
               PERFORM 41012-NIJYU-TRKGMN-SEC
           END-IF
      *
           .
       4101-NAME-CHK-EXT.
           EXIT.
      ****************************************************************
      *    性別チェック処理
      ****************************************************************
       4101-SEX-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-SEX         =   SPACE
               GO      TO      4101-SEX-CHK-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-GMN-SEXH
               WHEN    "2"
                   MOVE    "女"            TO  SPA-GMN-SEXH
               WHEN    OTHER
                   MOVE    "0021"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-CUR
                   GO      TO      4101-SEX-CHK-EXT
           END-EVALUATE
      *
           MOVE    SPA-GMN-SEX         TO  SPA-MAE-SEX
      *
           .
       4101-SEX-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    生年月日チェック処理
      ****************************************************************
       4101-BIRTHDAY-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-BIRTHDAYW   =   SPACE
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-BIRTHDAYW   TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-BIRTHDAYW
               MOVE    SGDAY-SDAY          TO  SPA-NAI-BIRTHDAY
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0028"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
      *    システム日付とのチェック
           IF      SPA-NAI-BIRTHDAY    >   SPA-SYSYMD
               MOVE    SPACE           TO  SPA-NAI-BIRTHDAY
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-MAE-BIRTHDAY
      *
      *    年齢計算
           PERFORM 410121-NENREI-SET-SEC
      *    見出しに編集
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-GMN-BIRTHDAYH
      *
      *    二重登録疑いチェック
           IF      SPA-P02A-FLG2       =   ZERO
               MOVE    2               TO  FLG-P02A-SYORI
               MOVE    1               TO  SPA-P02A-FLG2
               PERFORM 41012-NIJYU-TRKGMN-SEC
           END-IF
           .
       4101-BIRTHDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       410121-NENREI-SET-SEC             SECTION.
      *
           IF      SPA-NAI-BIRTHDAY    =   SPACE  OR  ZERO
               MOVE    SPACE               TO  SPA-GMN-NENREI
               MOVE    ZERO                TO  SPA-NAI-NENREI
               GO      TO      410121-NENREI-SET-EXT
           END-IF
      *
           MOVE    SPA-NAI-NENREI      TO  WRK-MAE-NENREI
      *
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
           MOVE    WRK-NENREI-YY       TO  SPA-NAI-NENREI-YY
           MOVE    WRK-NENREI-MM       TO  SPA-NAI-NENREI-MM
      *    画面表示用
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI
      *
      *    ７０歳到達日
      *    生年月日から基準日を求める
      *    １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-BASEYMD
           IF      WRK-BDD         NOT =   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY     =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01                  TO  WRK-BDD
           END-IF
      *    ７０歳を迎える日
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-70
           COMPUTE SPA-B70-YY          =   SPA-B70-YY  +   70
      *    ７５歳を迎える日
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-75
           COMPUTE SPA-B75-YY          =   SPA-B75-YY  +   75
      *
      *    新規複写後、生年月日変更時に補助区分の再編集をおこなう
           IF     (SPA-P02-SYORI       =   1    )  AND
                  (WRK-MAE-NENREI      =   ZERO )
               PERFORM 31011-HKN-HEN-SEC
           END-IF
           .
       410121-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    通称チェック処理
      *****************************************************************
       4101-NICKNAME-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-NICKNAME    NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-NICKNAME    TO  WRK-MAE-INPUT
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NICKNAME
               IF      FLG-ERR             =   1
                   MOVE    "0029"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-NICKNAME-CHK-EXT.
           EXIT.
      *
      ***************************************************************
      *    テスト患者処理
      ***************************************************************
       4101-TSTPTNUMKBN-CHK-SEC         SECTION.
      *
      *    テスト患者
           IF      SPA-GMN-TSTPTNUMKBN =   "0"  OR  "1"
               CONTINUE
           ELSE
      *        エラーの時すべて、患者へ
               MOVE    SPA-GMN-TSTPTNUMKBN-LST(1)
                                       TO  SPA-GMN-TSTPTNUMKBN-G
           END-IF
      *
           .
       4101-TSTPTNUMKBN-CHK-EXT.
           EXIT.
      *
      ***************************************************************
      *    カルテ発行チェック処理
      ***************************************************************
       4101-HAKKO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-HAKKO   =   SPA-GMN-HAKKO-LST(IDX)(1:1)
                   MOVE    SPA-GMN-HAKKO-LST (IDX)
                                           TO  SPA-GMN-HAKKO-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0029"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-HAKKO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    郵便番号 チェック処理
      *****************************************************************
       4101-POST-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-POST        =   SPACE
               MOVE    SPA-GMN-POST        TO  SPA-MAE-POST
               GO      TO      4101-POST-CHK-EXT
           END-IF
      *
      *    周辺住所一覧へ
           IF      SPA-GMN-POST (1:1)  =   "/"
               IF      SPA-GMN-POST (2:3)  NUMERIC
                   PERFORM 41011-SYUHEN-JYUSYO-SEC
               ELSE
                   PERFORM 440-JYUSYO-SEC
               END-IF
               GO      TO      4101-POST-CHK-EXT
           END-IF
      *
      *    入力があった時のみ、行う
           IF      FLG-IN-POST         =   ZERO
               GO      TO      4101-POST-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-POST        TO  SPA-MAE-POST
      *
      *    郵便番号より、住所一覧を表示する
           INITIALIZE                  SPA-P982-AREA
           MOVE    SPA-GMN-POST        TO  SPA-GMN982-POST
           MOVE    "P02"               TO  SPA-GMN982-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP982"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *    対象なし（警告とする）
           IF      SPA-GMN982-MAX      =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE     40                 TO  SPA-GMN-CUR
               GO  TO  4101-POST-CHK-EXT
           END-IF
      *    １件の時、対象
           IF      SPA-GMN982-MAX      =   1
               MOVE    SPA-GMN982-POST     TO  SPA-GMN-POST
               MOVE    SPA-GMN982-TOWNNAME TO  SPA-GMN-EDITADRS-NAME
               GO  TO  4101-POST-CHK-EXT
           END-IF
      *
      *    住所一覧画面へ
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
      *
      *    一覧へ
           MOVE    "P982"              TO  SPA-SAKIPG
           MOVE    "P02"               TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P982"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       4101-POST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    周辺住所チェック処理
      *****************************************************************
       41011-SYUHEN-JYUSYO-SEC         SECTION.
      *
      *    システム管理より周辺住所を検索
           INITIALIZE                  SYS-1015-REC
           MOVE    "1015"              TO  SYS-1015-KANRICD
           MOVE    SPA-GMN-POST (2:3)  TO  SYS-1015-KBNCD
           MOVE    SYS-1015-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1015-REC
           ELSE
      *        周辺住所一覧へ
               PERFORM 440-JYUSYO-SEC
               GO      TO      41011-SYUHEN-JYUSYO-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *    住所データ読み出し
           INITIALIZE                      ADRS-REC
           MOVE    SYS-1015-LPUBCD     TO  ADRS-LPUBCD
           MOVE    SYS-1015-POST       TO  ADRS-POST
           MOVE    SYS-1015-RENNUM     TO  ADRS-EDANUM
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE   "DBSELECT"           TO  MCP-FUNC
           MOVE   "ADRS-KEY"           TO  ORC-DBPATH
           CALL   "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "ADRS-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRS-REC
                   MOVE    ADRS-POST           TO  SPA-GMN-POST
                   MOVE    ADRS-EDITADRS-NAME  TO  SPA-GMN-EDITADRS-NAME
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-IF
      *
           IF      FLG-OK              =   ZERO
      *        周辺住所一覧へ
               PERFORM 440-JYUSYO-SEC
           END-IF
           .
       41011-SYUHEN-JYUSYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所チェック処理
      *****************************************************************
       4101-ADRS-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-EDITADRS-NAME   NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-EDITADRS-NAME   TO  WRK-MAE-INPUT
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-EDITADRS-NAME
               IF      FLG-ERR             =   1
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    41                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-ADRS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番地チェック処理
      *****************************************************************
       4101-BANTI-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-BANTI   NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-BANTI       TO  WRK-MAE-INPUT
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-BANTI
               IF      FLG-ERR             =   1
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    42                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-BANTI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    世帯主チェック処理
      *****************************************************************
       4101-SETAINUSI-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-SETAINUSI   NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-SETAINUSI   TO  WRK-MAE-INPUT
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-SETAINUSI
               IF      FLG-ERR             =   1
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    47                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-SETAINUSI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    続柄チェック処理
      *****************************************************************
       4101-ZOKUGARA-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-ZOKUGARA    NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-ZOKUGARA    TO  WRK-MAE-INPUT
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ZOKUGARA
               IF      FLG-ERR             =   1
                   MOVE    "0034"              TO  SPA-ERRCD
                   MOVE    48                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-ZOKUGARA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    減免事由チェック処理
      *****************************************************************
       4101-DISCOUNTKBN-CHK-SEC        SECTION.
      *
      *    減免事由が未入力
           IF      SPA-GMN-DISCOUNTKBN =   SPACE
               GO  TO                  4101-DISCOUNTKBN-CHK-EXT
           END-IF
      *    減免事由より割引率区分を取得
           MOVE    SPACE               TO  WRK-RATEKBN
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX       >   10 )
                   OR      ( SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                                             =   SPACE )
               IF      SPA-GMN-DISCOUNTKBN
                                   =   SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                   MOVE    SPA-GMN-DISCOUNTKBN-RATEKBN (IDX)
                                       TO  WRK-RATEKBN
                   MOVE    10          TO  IDX
               END-IF
           END-PERFORM
      *    見つからない場合、セットしない
           IF  WRK-RATEKBN             =   SPACE
               GO  TO                  4101-DISCOUNTKBN-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-RATE-AREA
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL  ( IDX     >     10 )
                     OR     ( SPA-GMN-DISCOUNT-KBNCD (IDX)  =   SPACE )
      *
             IF  WRK-RATEKBN           =   SPA-GMN-DISCOUNT-KBNCD (IDX)
                 MOVE    SPA-GMN-DISCOUNT-KBNCD(IDX)
                                       TO  WRK-RATE-KBN
                 MOVE    SPA-GMN-DISCOUNT-RITU (IDX)
                                       TO  WRK-RATE-EDITZ
                 EVALUATE    SPA-GMN-DISCOUNT-TEIRITU (IDX)
                 WHEN    "0"
                     MOVE    "該当なし"    TO  WRK-RATE-EDIT-R
                 WHEN    "1"
                     MOVE    "(%)"         TO  WRK-RATE-TANI
                 WHEN    "2"
                     MOVE    "(円)"        TO  WRK-RATE-TANI
                 END-EVALUATE
                 MOVE    10                TO  IDX
             END-IF
           END-PERFORM
           MOVE    WRK-RATE-AREA       TO  SPA-GMN-DISCOUNT
      *
           .
       4101-DISCOUNTKBN-CHK-EXT.
           EXIT.
      *
      ***************************************************************
      *    割引率チェック処理
      ***************************************************************
       4101-DISCOUNT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )  OR
                              (SPA-GMN-DISCOUNTKBN-KBNCD(IDX)
                                           =   SPACE  )
               IF      SPA-GMN-DISCOUNTKBN
                                       =   SPA-GMN-DISCOUNTKBN-KBNCD
                                                              (IDX)
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    46                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-DISCOUNT-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    状態名称セット処理
      ****************************************************************
       4101-CONDITIONH-SET-SEC         SECTION.
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX       >  3    )  OR
                            (SPA-ERRCD NOT =   SPACE )
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING   IDY       FROM    1   BY  1
                       UNTIL   ( IDY       >   10    )
                       OR      ( SPA-GMN-CONDITION-KBNCD (IDX IDY)
                                           =   SPACE )
                   IF      SPA-GMN-CONDITION (IDX)(1:2)
                                           =   SPA-GMN-CONDITION-KBNCD
                                                            (IDX IDY)
                       MOVE    SPA-GMN-CONDITION-WORD (IDX IDY)
                                           TO  SPA-GMN-CONDITIONH (IDX)
                       MOVE    1           TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK         =   ZERO
                   MOVE    "0036"          TO  SPA-ERRCD
                   EVALUATE    IDX
                       WHEN    1
                           MOVE    49          TO  SPA-GMN-CUR
                       WHEN    2
                           MOVE    50          TO  SPA-GMN-CUR
                       WHEN    3
                           MOVE    51          TO  SPA-GMN-CUR
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           .
       4101-CONDITIONH-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    選択番号チェック処理
      ****************************************************************
       4101-SELNUM-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-SELNUM  NOT =   ZERO
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING   IDX       FROM    1   BY  1
                       UNTIL     IDX       >  SPA-HKNCOMBI-MAX
                   IF      SPA-GMN-SELNUM  =   SPA-GMN-THKNCOMBINUM(IDX)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0037"              TO  SPA-ERRCD
                   MOVE    38                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療科チェック処理
      ****************************************************************
       4101-SRYKA-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-SRYKA   NOT =   SPACE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING   IDX       FROM    1   BY  1
                       UNTIL     IDX       >   SPA-SRYKA-MAX
                   IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKA-LST (IDX)
                                                              (1:2)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0038"              TO  SPA-ERRCD
                   MOVE    39                  TO  SPA-GMN-CUR
               END-IF
           ELSE
      *        カルテ発行時は、必須とする
               IF      SPA-GMN-HAKKO       =   "1"
                   MOVE    "0039"              TO  SPA-ERRCD
                   MOVE    39                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-SRYKA-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC      SECTION.
      *
      *    登録時、必須入力チェック
           IF      SPA-GMN-KANANAME    =   SPACE
               MOVE    "0100"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-NAME        =   SPACE
               MOVE    "0101"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-SEX         =   SPACE
               MOVE    "0103"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-BIRTHDAYW   =   SPACE
               MOVE    "0102"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    旧姓履歴の更新
           PERFORM 41091-KYUSEI-KOU-SEC
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    旧姓履歴の更新処理
      ****************************************************************
       41091-KYUSEI-KOU-SEC      SECTION.
      *    新規の時、旧姓履歴は出力しない
           IF      SPA-P02-SYORI       =   1
               GO      TO      41091-KYUSEI-KOU-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDX
           IF     (SPA-OLD-NAME    NOT =   SPACE   )  AND
                  (SPA-GMN-NAME    NOT =   SPACE   )  AND
                  (SPA-GMN-NAME    NOT =   SPA-OLD-NAME)
               MOVE    SPA-NAI25-KYU-MAX   TO  IDX
               IF    ( IDX                         >   ZERO      ) AND
                     ((SPA-GMN25-KYU-CHGYMD(IDX)   =   SPA-SYSYMD) OR
                      (SPA-GMN25-KYU-NAME  (IDX)   =   SPA-OLD-NAME
                  AND  SPA-GMN25-KYU-KANANAME(IDX) =   SPA-OLD-KANANAME
                  AND  SPA-GMN25-KYU-NICKNAME(IDX) =  SPA-OLD-NICKNAME))
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX
                   IF      IDX             >   20
                      MOVE    "2510"           TO  SPA-ERRCD
                      MOVE    1                TO  SPA-GMN-CUR
                      GO      TO      41091-KYUSEI-KOU-EXT
                   END-IF
                   ADD     1               TO  SPA-NAI25-KYU-MAX
      *
                   MOVE    SPA-OLD-NAME    TO  SPA-GMN25-KYU-NAME
                                                               (IDX)
                   MOVE    SPA-OLD-KANANAME
                                           TO  SPA-GMN25-KYU-KANANAME
                                                                (IDX)
                   MOVE    SPA-OLD-NICKNAME
                                           TO  SPA-GMN25-KYU-NICKNAME
                                                                (IDX)
                   MOVE    SPA-SYSYMD      TO  SPA-GMN25-KYU-CHGYMD
                                                               (IDX)
                   MOVE    SPA-SYSYMDWH    TO  SPA-GMN25-KYU-CHGYMDW
                                                              (IDX)
                   MOVE    SPA-SYSYMD      TO  SPA-GMN25-KYU-UPYMD(IDX)
               END-IF
           END-IF
      *
           .
      *
       41091-KYUSEI-KOU-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険・公費のテーブルセット処理
      ****************************************************************
       4103-HKNKOH-TBL-SEC      SECTION.
      *
      *    保険画面の内容をＴＢＬへ編集する
           PERFORM 41031-HKN-SPA-TBL-SET-SEC
      *
      *    公費画面の内容をＴＢＬへ編集する
           PERFORM 41032-KOH-SPA-TBL-SET-SEC
      *
      *    保険と公費の期間重複チェック
           PERFORM 41033-HKNKOH-CHK-SEC
      *
      *    Ｈ１４．１０ 改正
      *!   IF      FLG-TOUROKU2         =   1
      *        ７０歳以上の時、老人割合が設定されていること
               IF     (SPA-SYSYMD          >=  "20021001" )  AND
                      (SPA-BYMD-70         >   ZERO       )  AND
                      (SPA-SYSYMD          >=  SPA-BYMD-70)
                   PERFORM 410331-RJN-HKNCHK-SEC
               END-IF
      *!   END-IF
           .
      *
       4103-HKNKOH-TBL-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険画面の内容をＴＢＬへ編集処理
      ****************************************************************
       41031-HKN-SPA-TBL-SET-SEC      SECTION.
      *
           IF      SPA-GMN-HKNNUM      =   SPACE
      *        空白行削除
               PERFORM 410311-HKNTBL-DEL-SEC
               GO      TO      41031-HKN-SPA-TBL-SET-EXT
           END-IF
      *
           IF      SPA-NAI-HKNSTR      =   ZERO
      *        新規の時、１行開ける
               PERFORM 410311-HKNTBL-INS-SEC
               MOVE    1                   TO  IDX
           ELSE
               MOVE    SPA-NAI-HKNSTR      TO  IDX
           END-IF
           PERFORM 410312-HKN-SPATBL-SET-SEC
      *
      *    空白行削除
           PERFORM 410311-HKNTBL-DEL-SEC
      *
      *    保険情報ＴＢＬから編集
           IF      SPA-NAI-HKNSTR      =   ZERO
               MOVE    1               TO  SPA-NAI-HKNSTR
           END-IF
           PERFORM 3101-HKN-TBL-SPA-SEC
           .
      *
       41031-HKN-SPA-TBL-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険テーブル空白行削除処理
      ****************************************************************
       410311-HKNTBL-DEL-SEC      SECTION.
      *
      *    空白行をつめる
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               IF     (IDX                 <   40  )  AND
                      (SPA-PTHKN-HKNNUM (IDX)  =   SPACE)
                   MOVE    SPA-PTHKNINF-TBL (IDX + 1)
                                           TO  SPA-PTHKNINF-TBL (IDX)
                   INITIALIZE              SPA-PTHKNINF-TBL (IDX + 1)
               END-IF
               IF      SPA-PTHKN-HKNNUM (IDX)  NOT =   SPACE
                   MOVE    IDX             TO  IDY
               END-IF
           END-PERFORM
           MOVE    IDY                 TO  SPA-PTHKN-MAX
      *
           .
      *
       410311-HKNTBL-DEL-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険テーブル１行挿入処理
      ****************************************************************
       410311-HKNTBL-INS-SEC           SECTION.
      *
      *    １行開ける
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX     FROM    SPA-PTHKN-MAX BY -1
                   UNTIL       IDX     <   1
               MOVE    SPA-PTHKNINF-TBL (IDX)
                                       TO  SPA-PTHKNINF-TBL (IDX + 1)
           END-PERFORM
           INITIALIZE                  SPA-PTHKNINF-TBL (01)
           ADD     1                   TO  SPA-PTHKN-MAX
           .
       410311-HKNTBL-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険画面→ＳＰＡＴＢＬ編集処理
      *****************************************************************
       410312-HKN-SPATBL-SET-SEC              SECTION.
      *
           MOVE    SPA-GMN-HKNNUM      TO  SPA-PTHKN-HKNNUM (IDX)
           MOVE    SPA-GMN-HKNJANUM    TO  SPA-PTHKN-HKNJANUM (IDX)
           MOVE    SPA-GMN-HONKZKKBN   TO  SPA-PTHKN-HONKZKKBN (IDX)
           MOVE    SPA-GMN-HOJOKBN     TO  SPA-PTHKN-HOJOKBN (IDX)
           MOVE    SPA-GMN-CONTKBN     TO  SPA-PTHKN-CONTKBN (IDX)
           MOVE    SPA-GMN-KIGO        TO  SPA-PTHKN-KIGO    (IDX)
           MOVE    SPA-GMN-NUM         TO  SPA-PTHKN-NUM     (IDX)
           MOVE    SPA-GMN-HIHKNJANAME TO  SPA-PTHKN-HIHKNJANAME (IDX)
           MOVE    SPA-NAI-SKKGETYMD   TO  SPA-PTHKN-SKKGETYMD (IDX)
           MOVE    SPA-NAI-HTEKSTYMD   TO  SPA-PTHKN-TEKSTYMD (IDX)
           MOVE    SPA-NAI-HTEKEDYMD   TO  SPA-PTHKN-TEKEDYMD (IDX)
           MOVE    SPA-NAI-HKAKUNINYMD TO  SPA-PTHKN-KAKUNINYMD (IDX)
      *
           MOVE    SPA-NAI-SAKJOKBN    TO  SPA-PTHKN-SAKJOKBN (IDX)
           MOVE    SPA-NAI-PTHKN-SBTKBN
                                       TO  SPA-PTHKN-HKN-SBTKBN(IDX)
      *
           MOVE    SPA-HKNJA-KEIFLG    TO  SPA-PTHKN-KEIFLG  (IDX)
      *
      *    新規の時保険ＩＤ取得（労災は労災画面で取得する）
           IF      SPA-NAI-HKNID       =   ZERO
               ADD     1                   TO  SPA-PTNUM-HKNID
               MOVE    SPA-PTNUM-HKNID     TO  SPA-NAI-HKNID
           END-IF
           MOVE    SPA-NAI-HKNID       TO  SPA-PTHKN-HKNID (IDX)
      *
           IF      SPA-NAI-HKNSTR      =   ZERO
               MOVE    "1"             TO  SPA-PTHKN-SINKIKBN (IDX)
           END-IF
      *
           MOVE    SPA-GMN-HKNNUMMEI   TO  SPA-PTHKN-HKNNUM-MEI(IDX)
           MOVE    SPA-GMN-HONKZKKBNMEI
                                       TO  SPA-PTHKN-HONKZKKBN-MEI(IDX)
           MOVE    SPA-GMN-HOJOKBNMEI  TO  SPA-PTHKN-HOJOKBN-MEI(IDX)
           MOVE    SPA-GMN-CONTKBNMEI  TO  SPA-PTHKN-CONTKBN-MEI(IDX)
           MOVE    SPA-GMN-SKKGETYMD   TO  SPA-PTHKN-SKKGETYMDW(IDX)
           MOVE    SPA-GMN-HTEKSTYMD   TO  SPA-PTHKN-TEKSTYMDW(IDX)
           MOVE    SPA-GMN-HTEKEDYMD   TO  SPA-PTHKN-TEKEDYMDW(IDX)
           MOVE    SPA-GMN-HKAKUNINYMD TO  SPA-PTHKN-KAKUNINYMDW(IDX)
      *
      *
           .
       410312-HKN-SPATBL-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費画面の内容をＴＢＬへ編集処理
      ****************************************************************
       41032-KOH-SPA-TBL-SET-SEC      SECTION.
      *
      *    変更分のみ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
              IF     (SPA-GMN-KOHNUM      (IDX)  NOT =   SPACE ) AND
                     (SPA-NAI-PTKOH-EDANUM(IDX)  NOT =   ZERO  )
                   MOVE    SPA-NAI-PTKOH-EDANUM(IDX)   TO  IDY
                   PERFORM 3102-KOH-SPATBL-HEN-SEC
              END-IF
           END-PERFORM
      *    新規分のみ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
              IF     (SPA-GMN-KOHNUM      (IDX)  NOT =   SPACE ) AND
                     (SPA-NAI-PTKOH-EDANUM(IDX)      =   ZERO  )
                   IF     (IDX             =   1  )  AND
                          (SPA-PTKOH-MAX   >   ZERO )
      *                最初の行追加の時、１行目をあける
                       PERFORM 410321-KOHTBL-INS-SEC
                       MOVE    1               TO  IDY
                   ELSE
                       ADD     1               TO  SPA-PTKOH-MAX
                       MOVE    SPA-PTKOH-MAX       TO  IDY
                   END-IF
                   PERFORM 3102-KOH-SPATBL-HEN-SEC
              END-IF
           END-PERFORM
      *    空白行削除
           PERFORM 410323-KOHTBL-DEL-SEC
      *    公費情報ＴＢＬから編集
           IF      SPA-NAI-KOHSTR      =   ZERO
               MOVE    1                   TO  SPA-NAI-KOHSTR
           END-IF
           PERFORM 3101-KOH-TBL-SPA-SEC
           .
      *
       41032-KOH-SPA-TBL-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費テーブル１行挿入処理
      ****************************************************************
       410321-KOHTBL-INS-SEC      SECTION.
      *
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX2    FROM    SPA-PTKOH-MAX BY -1
                   UNTIL       IDX2    <   1
               MOVE    SPA-PTKOHINF-TBL (IDX2)
                                       TO  SPA-PTKOHINF-TBL (IDX2 + 1)
           END-PERFORM
           INITIALIZE                  SPA-PTKOHINF-TBL (01)
           ADD     1                   TO  SPA-PTKOH-MAX
      *
           .
       410321-KOHTBL-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費画面→ＳＰＡＴＢＬ編集処理
      *****************************************************************
       3102-KOH-SPATBL-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-KOHNUM   (IDX)  TO  SPA-PTKOH-KOHNUM (IDY)
           MOVE    SPA-GMN-FTNJANUM (IDX)  TO  SPA-PTKOH-FTNJANUM (IDY)
           MOVE    SPA-GMN-JKYSNUM  (IDX)  TO  SPA-PTKOH-JKYSNUM (IDY)
           MOVE    SPA-NAI-KTEKSTYMD(IDX)  TO  SPA-PTKOH-TEKSTYMD (IDY)
           MOVE    SPA-NAI-KTEKEDYMD(IDX)  TO  SPA-PTKOH-TEKEDYMD (IDY)
           MOVE    SPA-NAI-KKAKUNINYMD(IDX)
                                           TO  SPA-PTKOH-KAKUNINYMD(IDY)
           MOVE    SPA-NAI-PTKOH-PAYKBN(IDX)
                                           TO  SPA-PTKOH-PAYKBN (IDY)
      *
      ****MOVE    SPA-NAI-KSAKJOKBN (IDX) TO  SPA-PTKOH-SAKJOKBN(IDY)
      *
      *    新規の時公費ＩＤ取得
           IF      SPA-NAI-PTKOH-KOHID (IDX)   =   ZERO
               ADD     1                   TO  SPA-PTNUM-KOHID
               MOVE    SPA-PTNUM-KOHID     TO  SPA-NAI-PTKOH-KOHID(IDX)
      *
               MOVE    "1"                 TO  SPA-PTKOH-SINKIKBN (IDY)
           END-IF
           MOVE    SPA-NAI-PTKOH-KOHID (IDX)   TO  SPA-PTKOH-KOHID (IDY)
      *    保険公費種別区分
           MOVE   SPA-NAI-PTKOH-SBTKBN (IDX)   TO  SPA-PTKOH-HKN-SBTKBN
                                                                 (IDY)
           MOVE   SPA-GMN-KOHNUMMEI (IDX)  TO  SPA-PTKOH-KOHNUM-MEI(IDY)
           MOVE   SPA-GMN-KTEKSTYMD (IDX)  TO  SPA-PTKOH-TEKSTYMDW(IDY)
           MOVE   SPA-GMN-KTEKEDYMD (IDX)  TO  SPA-PTKOH-TEKEDYMDW(IDY)
           MOVE   SPA-GMN-KKAKUNINYMD(IDX) TO  SPA-PTKOH-KAKUNINYMDW
                                                                  (IDY)
      *
           MOVE   SPA-NAI-PTKOH-KEIFLG(IDX)    TO  SPA-PTKOH-KEIFLG
                                                                  (IDY)
      *
           .
       3102-KOH-SPATBL-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費テーブル空白行削除処理
      ****************************************************************
       410323-KOHTBL-DEL-SEC      SECTION.
      *
      *    空白行をつめる
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
               IF      SPA-PTKOH-KOHNUM (IDX)  =   SPACE
                   MOVE    SPA-PTKOHINF-TBL (IDX + 1)
                                           TO  SPA-PTKOHINF-TBL (IDX)
                   INITIALIZE              SPA-PTKOHINF-TBL (IDX + 1)
               END-IF
               IF      SPA-PTKOH-KOHNUM (IDX)  NOT =   SPACE
                   MOVE    IDX             TO  IDY
               END-IF
           END-PERFORM
           MOVE    IDY                 TO  SPA-PTKOH-MAX
      *
           .
      *
       410323-KOHTBL-DEL-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険と公費の期間重複チェック処理
      ****************************************************************
       41033-HKNKOH-CHK-SEC      SECTION.
      *
      *    国保（退職者含む）・生活保護 重複チェック
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   SPA-PTHKN-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-PTHKN-SAKJOKBN(IDX) =   SPACE    )  AND
                      (SPA-PTHKN-HKNNUM  (IDX) =   "060" OR "067")
                   PERFORM VARYING  IDY     FROM    1   BY  1
                           UNTIL   (IDY     >   SPA-PTKOH-MAX )  OR
                                   (SPA-ERRCD   NOT =   SPACE )
                      IF       SPA-PTKOH-SAKJOKBN (IDY)    =   SPACE
                           PERFORM 410331-HKNKOH-JYUFUKU-SEC
                      END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           IF      SPA-GMN-HKNNUM      =   "060" OR "067"
               MOVE    16              TO  SPA-GMN-CUR
           ELSE
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-KOH-MAX
                   IF      SPA-NAI-PTKOH-SBTKBN (IDY)  =   "5"
                       EVALUATE    IDY
                           WHEN    1
                               MOVE    23          TO  SPA-GMN-CUR
                           WHEN    2
                               MOVE    29          TO  SPA-GMN-CUR
                           WHEN    3
                               MOVE    35          TO  SPA-GMN-CUR
                       END-EVALUATE
                       MOVE    SPA-KOH-MAX         TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       41033-HKNKOH-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    ７０歳以上の老人保険チェック処理
      ****************************************************************
       410331-RJN-HKNCHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-RJNCHK
           MOVE    ZERO                TO  FLG-HKNCHK
           MOVE    ZERO                TO  FLG-SEIHOGO
           MOVE    ZERO                TO  SPA-HKN-IDX1
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               IF     (SPA-PTHKN-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-SAKJOKBN (IDX) NOT =  "1"        )
      *            国保
                   IF     (SPA-PTHKN-HKNNUM  (IDX) =   "060" )  AND
                          (SPA-PTHKN-HOJOKBN (IDX) =   "1"  OR  "2")
                       MOVE    1                   TO  FLG-RJNCHK
                   END-IF
      *            国保以外
                   IF     (SPA-PTHKN-HKNNUM(IDX)       =   "060" )  OR
                          (SPA-PTHKN-HKNNUM(IDX)(1:1)  =   "9"   )
                       CONTINUE
                   ELSE
                       IF      SPA-PTHKN-HOJOKBN (IDX) =   "8"  OR  "9"
                           MOVE    1                   TO  FLG-RJNCHK
                       END-IF
                   END-IF
      *            船員で補助区分あり
                   IF     (SPA-PTHKN-HKNNUM(IDX)       =   "002" )  AND
                          (SPA-PTHKN-HOJOKBN (IDX)     =   "8" OR "9"
                                                       OR  "A" OR "B"
                                                       OR  "C" OR "D"
                                                       OR  "E" OR "F" )
                       MOVE    1                   TO  FLG-RJNCHK
                   END-IF
      *            共済で老人区分あり
                   IF     (SPA-PTHKN-HKNNUM(IDX)       =   "031"
                                                       OR  "032"
                                                       OR  "033"
                                                       OR  "034")  AND
                          (SPA-PTHKN-HOJOKBN (IDX)     =   "8" OR "9"
                                                       OR  "B" OR "E")
                       MOVE    1                   TO  FLG-RJNCHK
                   END-IF
               END-IF
      *        保険が存在すること
               IF     (SPA-PTHKN-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-SAKJOKBN (IDX) NOT =  "1"        )
                   IF      SPA-PTHKN-HKN-SBTKBN (IDX)   =   "1"
                       MOVE    1               TO  FLG-HKNCHK
                       MOVE    IDX             TO  SPA-HKN-IDX1
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-RJNCHK          =   1
               GO      TO      410331-RJN-HKNCHK-EXT
           END-IF
      *
      *    公費
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
               IF     (SPA-PTKOH-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
                      (SPA-PTKOH-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTKOH-SAKJOKBN (IDX) NOT =  "1"        )
      *            老人
                   IF     (SPA-PTKOH-KOHNUM  (IDX) =   "027" )  OR
                          (SPA-PTKOH-HKN-SBTKBN (IDX)  =   "3" )
                       MOVE    1                   TO  FLG-RJNCHK
                   END-IF
      *            生活保護
                   IF      SPA-PTKOH-KOHNUM  (IDX) =   "012"
                       MOVE    1                   TO  FLG-SEIHOGO
                   END-IF
               END-IF
           END-PERFORM
      *
      *    老人保険がなく、保険が存在する時
           IF     (FLG-RJNCHK          =   ZERO  )  AND
                  (FLG-HKNCHK          =   1     )
      *        Ｓ７．９．３０以前の時老人公費があることとする
               IF     (SPA-NAI-BIRTHDAY    <=  "19320930" )  OR
                      (SPA-SYSYMD          >=  SPA-BYMD-75)
                   MOVE    "0105"              TO  SPA-ERRCD
                   MOVE    20                  TO  SPA-GMN-CUR
               ELSE
      *        高齢者保険を自動変換へ
                   MOVE    SPA-PTHKN-HKNNUM-MEI(SPA-HKN-IDX1)
                                               TO  SPA-PIDMSG
                   MOVE    "0104"              TO  SPA-PIDCD
                   MOVE    8                   TO  SPA-GMN-CUR
               END-IF
               GO      TO      410331-RJN-HKNCHK-EXT
           END-IF
      *
           .
       410331-RJN-HKNCHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険と公費の期間重複チェック処理
      ****************************************************************
       410331-HKNKOH-JYUFUKU-SEC      SECTION.
      *
           IF     (SPA-PTKOH-TEKEDYMD (IDY)
                                       <   SPA-PTHKN-TEKSTYMD (IDX)) OR
                  (SPA-PTKOH-TEKSTYMD (IDY)
                                       >   SPA-PTHKN-TEKEDYMD (IDX))
               CONTINUE
           ELSE
      *        生活保護の場合エラー
               IF      SPA-PTHKN-HKN-SBTKBN (IDY)  =   "5"
                   MOVE    "0089"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       410331-HKNKOH-JYUFUKU-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費追加処理
      ****************************************************************
       4104-KOH-ADD-SEC      SECTION.
      *
           IF      SPA-HKNKOH-FLG  NOT =   1
      *        公費画面の内容をＴＢＬへ編集する
               PERFORM 41032-KOH-SPA-TBL-SET-SEC
           END-IF
      *
      *    最初の１行を開ける
           IF      SPA-NAI-PTKOH-EDANUM(1) NOT =   ZERO
               MOVE    SPA-NAI-PTKOH-EDANUM(1) TO  IDX-K2
               INITIALIZE                  SPA-GMN-KOH-AREA
               MOVE    1                   TO  IDY
               PERFORM VARYING     IDX     FROM    IDX-K2 BY  1
                       UNTIL      (IDX     >   SPA-PTKOH-MAX )  OR
                                  (IDY     >=  3    )
                   IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                       ADD     1                   TO  IDY
                       MOVE    IDY                 TO  SPA-KOH-MAX
                       PERFORM 3102-KOUHI-HEN-SEC
                   END-IF
               END-PERFORM
           ELSE
               INITIALIZE                  SPA-GMN-KOH-AREA
           END-IF
           MOVE    20                  TO  SPA-GMN-CUR
      *
           .
       4104-KOH-ADD-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    漢字氏名編集処理
      ****************************************************************
       4101-KANJI-NAME-EDIT-SEC     SECTION.
      *
           MOVE    SPA-GMN-KANANAME
                                   TO  SPA-GMN-OLD-KANANAME
           MOVE    SPA-GMN-NAME
                                   TO  SPA-GMN-OLD-NAME
      *
           MOVE    SPACE           TO  WRK-NAME-AREA
      *
           MOVE    SPACE           TO  SPA-P100-AREA
           INITIALIZE              SPA-P100-AREA
      *
      *    全角スペースを"@@"に置き換える
      *    （これをしておかないと"ァー"を含む文字列が
      *    うまく分解できない。）
           MOVE    SPA-GMN-KANANAME
                                   TO  WRK-NAME-X-AREA
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   25 )
                OR       ( WRK-NAME-X (IDX1) = SPACE )
      *
               IF  ( WRK-NAME-X (IDX1) =   "　" )
                   MOVE    "@@"    TO  WRK-NAME-X (IDX1)
               END-IF
      *
           END-PERFORM
      *
      *    ミドルネームがある人を考慮して編集を行う
           UNSTRING    WRK-NAME-X-AREA
                                   DELIMITED BY    "@@" OR SPACE
           INTO    WRK-NAME-VAL (1)
                   WRK-NAME-VAL (2)
                   WRK-NAME-VAL (3)
           END-UNSTRING
      *
           IF ( WRK-NAME-VAL (3)   NOT =   SPACE )
               MOVE    WRK-NAME-VAL (1)
                                   TO  WRK-NAME-KANA (1)
               MOVE    WRK-NAME-VAL (3)
                                   TO  WRK-NAME-KANA (2)
           ELSE
               MOVE    WRK-NAME-VAL (1)
                                   TO  WRK-NAME-KANA (1)
               MOVE    WRK-NAME-VAL (2)
                                   TO  WRK-NAME-KANA (2)
           END-IF
      *
      *    氏編集
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >       2 )
      *
               PERFORM 4101-KANJI-NAME-SELECT-SEC
      *
               MOVE    WRK-KANJINAME-T (1)
                                   TO  WRK-NAME-KANJI (IDX1)
               MOVE    WRK-KANJINAME-AREA
                                   TO  SPA-GMN-P100-KANJILST-ITM-AREA
                                                                (IDX1)
               MOVE    CNT-JINMEI  TO  SPA-GMN-P100-KANJILST-COUNT
                                                                (IDX1)
      *
           END-PERFORM
      *
      *    漢字氏名検索画面呼出
           PERFORM SPA-GMN-P100-CALL-SEC
      *
           .
      *
       4101-KANJI-NAME-EDIT-EXT.
           EXIT.
      ****************************************************************
      *    漢字氏名検索処理
      ****************************************************************
       4101-KANJI-NAME-SELECT-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-NAME (IDX1)
      *
           MOVE    SPACE               TO  WRK-KANJINAME-AREA
           MOVE    ZERO                TO  FLG-JINMEI
                                           CNT-JINMEI
      *
           IF  ( WRK-NAME-KANA (IDX1)  =   SPACE )
               MOVE    1               TO  FLG-NAME (IDX1)
               GO  TO  4101-KANJI-NAME-SELECT-EXT
           END-IF
      *
           MOVE    WRK-NAME-KANA (IDX1)
                                       TO  JINMEI-KANA
      *
      *    人名テーブル検索処理
           PERFORM 941-JINMEI-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-JINMEI  =   1 )
                    OR   ( CNT-JINMEI >=   50 )
      *
      *        人名テーブル読込処理
               PERFORM 941-JINMEI-NEXT-SEC
               IF  ( FLG-JINMEI        =   ZERO )
                   COMPUTE CNT-JINMEI  =   CNT-JINMEI  +   1
                   MOVE    JINMEI-KANJI
                                       TO  WRK-KANJINAME-T (CNT-JINMEI)
               END-IF
      *
           END-PERFORM
      *
           PERFORM 941-JINMEI-CLOSE-SEC
      *
           IF  ( CNT-JINMEI   >=   50 )
               GO  TO  4101-KANJI-NAME-SELECT-EXT
           END-IF
      *
      *    カタカナを一覧の最後に載せる
      *    （既にリストに存在している場合は載せない）
           SET     IDX-NAME    TO  1
           SEARCH  WRK-KANJINAME-OCC   VARYING  IDX-NAME
           AT  END
               COMPUTE CNT-JINMEI  =   CNT-JINMEI  +   1
               MOVE    WRK-NAME-KANA (IDX1)
                                   TO  WRK-KANJINAME-T (CNT-JINMEI)
           WHEN    WRK-KANJINAME-T (IDX-NAME)
                                   =   WRK-NAME-KANA (IDX1)
               CONTINUE
           END-SEARCH
           .
      *
       4101-KANJI-NAME-SELECT-EXT.
           EXIT.
      ****************************************************************
      *    カナ氏名検索画面呼出処理
      ****************************************************************
       SPA-GMN-P100-CALL-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >       2 )
               MOVE    "　　　　　　　　　　　　　　"
                                   TO  SPA-GMN-P100-KANANAME (IDX1)
      *
               STRING  WRK-NAME-KANA (IDX1)
                                   DELIMITED   BY  SPACE
               INTO    SPA-GMN-P100-KANANAME (IDX1)
               END-STRING
      *
      *
               IF  ( FLG-NAME (IDX1)   =   ZERO )
                   MOVE    WIDGET-NORMAL
                               TO  SPA-GMN-P100-SELNUM-STATE  (IDX1)
                   MOVE   1        TO  SPA-GMN-P100-SELNUM (IDX1)
                   MOVE   SPA-GMN-P100-KANJILST-KANJI (IDX1 1)
                               TO  SPA-GMN-P100-KANJINAME (IDX1)
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                               TO  SPA-GMN-P100-SELNUM-STATE (IDX1)
                   MOVE    1   TO  SPA-GMN-P100-KANJILST-COUNT (IDX1)
                   MOVE    "該当なし"
                               TO  SPA-GMN-P100-KANJILST-KANJI(IDX1 1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP100"          USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           IF (SPA-GMN-P100-SELNUM-STATE (1)
                                       =   WIDGET-NORMAL )
               MOVE  "SELNUM1"         TO  MCP-WIDGET
           ELSE
               MOVE  "SELNUM2"         TO  MCP-WIDGET
           END-IF
      *
           MOVE  "P02"                 TO  SPA-MOTOPG
           MOVE  "P100"                TO  SPA-SAKIPG
      *
           MOVE  "NEW"                 TO  MCP-PUTTYPE
           MOVE  "P100"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  SPA-FLG-END
      *
           .
       SPA-GMN-P100-CALL-EXT.
           EXIT.
      *----(01.00.15) LINE ADD END   ---------------------------------
      ****************************************************************
      *    死亡区分チェック処理
      ****************************************************************
       490-DEATHKBN-SEC                 SECTION.
      *
           EVALUATE    SPA-GMN-DEATHKBN
               WHEN    "1"
                   MOVE    SPACE           TO  SPA-GMN-DEATHKBN
               WHEN    SPACE
               WHEN    "0"
                   MOVE    "1"             TO  SPA-GMN-DEATHKBN
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  SPA-GMN-CUR
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-DEATHKBN    =   "1"
      *            登録ボタンへ
                   MOVE    60                  TO  SPA-GMN-CUR
               ELSE
      *            保険へ
                   MOVE    8                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       490-DEATHKBN-EXT.
           EXIT.
      *
      ***************************************************************
      *    保険確認年月日入力済み処理
      ***************************************************************
       450-HKNKAKUNINYMD-ZUMI-SEC      SECTION.
      *
      *    労災の時、労災保険画面へ
           IF      SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"
              GO      TO      450-HKNKAKUNINYMD-ZUMI-EXT
           END-IF
      *
           IF      P02-P02-HKNNUM  NOT =   SPACE
               IF     (SPA-SYSYMD          =   SPA-NAI-HKAKUNINYMD) AND
                      (SPA-MAE-HKAKUNINYMD NOT =   SPACE         )
                   MOVE    SPA-MAE-HKAKUNINYMDW
                                               TO  P02-P02-HKAKUNINYMD
               ELSE
                   MOVE    SPA-SYSYMDWH        TO  P02-P02-HKAKUNINYMD
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       450-HKNKAKUNINYMD-ZUMI-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費確認年月日入力済み処理
      *****************************************************************
       460-KOHKAKUNINYMD-ZUMI-SEC             SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "BK1"
                   IF       SPA-GMN-KOHNUM (1) NOT =   SPACE
                       IF     (SPA-SYSYMD      =   SPA-NAI-KKAKUNINYMD
                                                             (1) ) AND
                              (SPA-MAE-KKAKUNINYMD (1)
                                               NOT =   SPACE )
                           MOVE    SPA-MAE-KKAKUNINYMDW (1)
                                               TO  P02-P02-KKAKUNINYMD1
                           MOVE    SPA-MAE-KKAKUNINYMDW (1)
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (1)
                           MOVE    SPA-MAE-KKAKUNINYMD (1)
                                               TO  SPA-NAI-KKAKUNINYMD
                                                                    (1)
                       ELSE
                           MOVE    SPA-SYSYMDWH
                                               TO  P02-P02-KKAKUNINYMD1
                           MOVE    SPA-SYSYMDWH
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (1)
                           MOVE    SPA-SYSYMD  TO  SPA-NAI-KKAKUNINYMD
                                                                    (1)
                       END-IF
                   END-IF
               WHEN    "BK2"
                   IF       SPA-GMN-KOHNUM (2) NOT =   SPACE
                       IF     (SPA-SYSYMD      =   SPA-NAI-KKAKUNINYMD
                                                             (2) ) AND
                              (SPA-MAE-KKAKUNINYMD (2)
                                               NOT =   SPACE )
                           MOVE    SPA-MAE-KKAKUNINYMDW (2)
                                               TO  P02-P02-KKAKUNINYMD2
                           MOVE    SPA-MAE-KKAKUNINYMDW (2)
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (2)
                           MOVE    SPA-MAE-KKAKUNINYMD (2)
                                               TO  SPA-NAI-KKAKUNINYMD
                                                                    (2)
                       ELSE
                           MOVE    SPA-SYSYMDWH
                                               TO  P02-P02-KKAKUNINYMD2
                           MOVE    SPA-SYSYMDWH
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (2)
                           MOVE    SPA-SYSYMD  TO  SPA-NAI-KKAKUNINYMD
                                                                    (2)
                       END-IF
                   END-IF
               WHEN    "BK3"
                   IF       SPA-GMN-KOHNUM (3) NOT =   SPACE
                       IF     (SPA-SYSYMD      =   SPA-NAI-KKAKUNINYMD
                                                             (3) ) AND
                              (SPA-MAE-KKAKUNINYMD (3)
                                               NOT =   SPACE )
                           MOVE    SPA-MAE-KKAKUNINYMDW (3)
                                               TO  P02-P02-KKAKUNINYMD3
                           MOVE    SPA-MAE-KKAKUNINYMDW (3)
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (3)
                           MOVE    SPA-MAE-KKAKUNINYMD (3)
                                               TO  SPA-NAI-KKAKUNINYMD
                                                                    (3)
                       ELSE
                           MOVE    SPA-SYSYMDWH
                                               TO  P02-P02-KKAKUNINYMD3
                           MOVE    SPA-SYSYMDWH
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (3)
                           MOVE    SPA-SYSYMD  TO  SPA-NAI-KKAKUNINYMD
                                                                    (3)
                       END-IF
                   END-IF
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       460-KOHKAKUNINYMD-ZUMI-EXT.
           EXIT.
      *
      ****************************************************************
      *    二重登録疑い画面処理
      ****************************************************************
       41012-NIJYU-TRKGMN-SEC          SECTION.
      *
           IF      FLG-P02A-SYORI      =   1
               IF     (SPA-GMN-KANANAME    =   SPACE )  OR
                      (SPA-GMN-NAME        =   SPACE )
                   GO      TO              41012-NIJYU-TRKGMN-EXT
               END-IF
           ELSE
               IF  ( SPA-GMN-KANANAME      =   SPACE )  OR
                   ( SPA-NAI-BIRTHDAY      =   SPACE )  OR
                   ( SPA-GMN-SEX           =   SPACE )
                   GO      TO              41012-NIJYU-TRKGMN-EXT
               END-IF
           END-IF
      *
           PERFORM 41013-NIJYU-GMN-SET-SEC
           IF      SPA-GMN2A-MAX       =   ZERO
               GO      TO              41012-NIJYU-TRKGMN-EXT
           END-IF
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02A"              TO  SPA-SAKIPG
      *
      *    二重登録疑い画面へ
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02A"              TO  MCP-WINDOW
           MOVE    "B01"               TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       41012-NIJYU-TRKGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    二重登録画面　患者マスタ検索処理
      *****************************************************************
       41013-NIJYU-GMN-SET-SEC   SECTION.
      *
           INITIALIZE                  SPA-P02A-AREA
           MOVE    FLG-P02A-SYORI      TO  SPA-P02A-SYORI
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02A"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
      *---- 以下削除  −−−−−−−
           GO      TO              41013-NIJYU-GMN-SET-EXT
      *
           MOVE    ZERO                TO  SPA-GMN2A-MAX
           MOVE    SPACE               TO  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-KANANAME    TO  PTINF-KANANAME
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY3"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTINF-KEY3"    TO  ORC-DBPATH
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM   UNTIL  ( FLG-PTINF    =   1  )
                     OR     ( IDY          >=  20 )
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-CHK
               IF      FLG-P02A-SYORI      =   1
      *            氏名のみ一致
                   IF      PTINF-PTID  NOT =   SPA-GMN-PTID
                       MOVE    1               TO  FLG-CHK
                   END-IF
               ELSE
      *            氏名・性別・生年月日一致
                   IF      (PTINF-SEX          =   SPA-GMN-SEX )   AND
                           (PTINF-BIRTHDAY     =   SPA-NAI-BIRTHDAY) AND
                           (PTINF-PTID     NOT =   SPA-GMN-PTID   )
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
               IF      SPA-GMN-KANANAME    NOT =   PTINF-KANANAME
                   MOVE    ZERO                TO  FLG-CHK
               END-IF
      *
               IF      FLG-CHK             =   1
                   ADD     1               TO  IDY
                   MOVE    IDY             TO  SPA-GMN2A-NUM      (IDY)
                   MOVE    PTINF-PTID      TO  SPA-GMN2A-PTID     (IDY)
      *
      *            患者番号変換(管理）を検索し、患者IDより患者NOを取得
                   INITIALIZE                      ORCSPTIDAREA
                   MOVE    SPA-HOSPID          TO  SPTID-HOSPID
                   MOVE    PTINF-PTID          TO  SPTID-PTID
                   CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                   SPA-1009-TBL
                   IF      SPTID-RC        NOT =   ZERO
                       MOVE    SPACE           TO  SPA-GMN2A-PTNUM(IDY)
                   ELSE
                       MOVE    SPTID-PTNUM     TO  SPA-GMN2A-PTNUM(IDY)
                   END-IF
                   MOVE    PTINF-NAME          TO  SPA-GMN2A-NAME (IDY)
      *************MOVE    PTINF-HOME-ADRS     TO  SPA-GMN2A-ADRS (IDY)
                   STRING  PTINF-HOME-ADRS     DELIMITED  BY  SPACE
                           PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                           INTO    SPA-GMN2A-ADRS (IDY)
                   END-STRING
                   MOVE    PTINF-HOME-TEL1     TO  SPA-GMN2A-TEL1 (IDY)
               END-IF
      *
               MOVE    "PTINF-KEY3"    TO  ORC-DBPATH
               PERFORM 900-PTINF-READ-SEC
           END-PERFORM
           MOVE    IDY                 TO  SPA-GMN2A-MAX
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY3"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-GMN2A-MAX
               MOVE    SPA-GMN2A-NUM   (IDX)  TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ            TO  P02A-NUM   (IDX)
               MOVE    SPA-GMN2A-PTNUM (IDX)  TO  P02A-PTNUM (IDX)
               MOVE    SPA-GMN2A-NAME  (IDX)  TO  P02A-NAME  (IDX)
               MOVE    SPA-GMN2A-ADRS  (IDX)  TO  P02A-ADRS  (IDX)
               MOVE    SPA-GMN2A-TEL1  (IDX)  TO  P02A-TEL   (IDX)
           END-PERFORM
           MOVE    SPA-GMN2A-MAX       TO  P02A-DUALLIST-COUNT
           IF      SPA-GMN2A-MAX       >   ZERO
               IF      FLG-P02A-SYORI      =   1
                   MOVE    "同姓同名者リスト"      TO  P02A-MIDASI
               ELSE
                   MOVE    "二重登録者疑いリスト"  TO  P02A-MIDASI
               END-IF
           END-IF
      *
           .
       41013-NIJYU-GMN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       450-HKNCOMBI-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  SPA-GMN-CUR
           IF      FLG-TOUROKU2        =   2
               CONTINUE
           ELSE
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO      TO      450-HKNCOMBI-EXT
               END-IF
           END-IF
      *
      *    登録の時、新規患者の時すべての保険・公費をチェックする
      *    Ｈ１５．４改正対応のため、毎回、すべてのチェックを行う
      *!   IF    ( SPA-P02-SYORI       =   1     )  AND
      *!         ( FLG-TOUROKU2    NOT =   ZERO  )
           IF      FLG-TOUROKU     NOT =   ZERO
               PERFORM 4501-ALLHKN-CHK-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO      TO      450-HKNCOMBI-EXT
               END-IF
           END-IF
      *
      *
      *    保険ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4501-PTHKNINF-LNK-SEC
      *    公費ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4502-PTKOHINF-LNK-SEC
      *    公費ＴＢＬから公費負担へ編集
           PERFORM 4504-PTKOHFTN-HEN-SEC
      *
      *    保険組合せ作成
           PERFORM 4503-HKNCOMBI-SYORI-SEC
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       450-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険・公費チェック処理
      *****************************************************************
       4501-ALLHKN-CHK-SEC          SECTION.
      *
      *    エラーがない時は、元へもどす
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPA-NAI-HKNSTR      TO  WRK-NAI-HKNSTR
           IF      SPA-PTHKN-MAX       >   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-PTHKN-MAX )  OR
                              (SPA-PIDCD   NOT =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
                   INITIALIZE                  SPA-GMN-HKN-AREA
                   IF      SPA-PTHKN-SAKJOKBN (IDX)    =   SPACE
                       MOVE    IDX             TO  SPA-NAI-HKNSTR
                       PERFORM 3101-HOKEN-HEN-SEC
      *                保険情報チェックサブ
                       CALL    "ORCSP03A"          USING   SPA-AREA
                                                   SPA-P02SCRAREA
                                                   SPA-P02KYOUTU
                   END-IF
               END-PERFORM
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )
                   GO      TO      4501-ALLHKN-CHK-EXT
               END-IF
               MOVE    WRK-NAI-HKNSTR      TO  SPA-NAI-HKNSTR
               MOVE    SPA-NAI-HKNSTR      TO  IDX
               IF      IDX                 =   ZERO
                   MOVE    1               TO  IDX
               END-IF
               PERFORM 3101-HOKEN-HEN-SEC
           END-IF
      *
      *    公費は３件以上の時行う
           IF     (SPA-PTKOH-MAX       =   ZERO )  OR
                  (SPA-NAI-KOHEND      =   SPA-PTKOH-MAX )
               GO      TO      4501-ALLHKN-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-KOHSTR      TO  WRK-NAI-KOHSTR
           MOVE    ZERO                TO  IDY
           MOVE    SPA-GMN-KOH-AREA    TO  WRK-GMN-KOH-AREA
           IF      WRK-NAI-KOHSTR      =   ZERO
               MOVE    1                   TO  WRK-NAI-KOHSTR
           END-IF
      *
           PERFORM VARYING     IDX     FROM    WRK-NAI-KOHSTR BY  1
                   UNTIL      (IDX         >   SPA-PTKOH-MAX )  OR
                              (SPA-PIDCD   NOT =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDY                 =   ZERO
                   INITIALIZE                  SPA-GMN-KOH-AREA
                   MOVE    ZERO                TO  SPA-NAI-KOHEND
               END-IF
               IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                   IF      IDY             =   ZERO
                       MOVE    IDX             TO  SPA-NAI-KOHSTR
                   END-IF
                   MOVE    IDX                 TO  SPA-NAI-KOHEND
                   ADD     1                   TO  IDY
                   MOVE    1                   TO  FLG-CHK
                   PERFORM 3102-KOUHI-HEN-SEC
               END-IF
               IF      IDY                 >=  3
      *            公費情報チェックサブ
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    ZERO                TO  IDY
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDY                 >   ZERO
      *        公費情報チェックサブ
               CALL    "ORCSP03B"          USING   SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4501-ALLHKN-CHK-EXT
           END-IF
      *    元へ戻す
           MOVE    WRK-GMN-KOH-AREA    TO  SPA-GMN-KOH-AREA
           .
       4501-ALLHKN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険追加処理
      *****************************************************************
       4501-PTHKNINF-LNK-SEC          SECTION.
      *
      *    削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTHKNINF"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               PERFORM 45011-HKN-SPA-PARA-SEC
      *        追加
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "PTHKNINF"          TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    PTHKNINF-REC        TO  PARA-DATA-REC
      *
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02W1 PARA HKNINF INS ERR:"  MCP-RC
               END-IF
           END-PERFORM
      *
      *
           .
       4501-PTHKNINF-LNK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険ＳＰＡ→パラメタ編集処理
      *****************************************************************
       45011-HKN-SPA-PARA-SEC          SECTION.
      *
           MOVE    SPACE               TO  PTHKNINF-REC
           INITIALIZE                      PTHKNINF-REC
      *
           MOVE    SPA-HOSPID          TO  PTHKN-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
           MOVE    SPA-PTHKN-HKNID     (IDX)   TO  PTHKN-HKNID
           MOVE    SPA-PTHKN-HKNNUM    (IDX)   TO  PTHKN-HKNNUM
           MOVE    SPA-PTHKN-HKNJANUM  (IDX)   TO  PTHKN-HKNJANUM
           MOVE    SPA-PTHKN-HIHKNJANAME(IDX)  TO  PTHKN-HIHKNJANAME
           MOVE    SPA-PTHKN-HONKZKKBN  (IDX)  TO  PTHKN-HONKZKKBN
           MOVE    SPA-PTHKN-HOJOKBN    (IDX)  TO  PTHKN-HOJOKBN
           MOVE    SPA-PTHKN-CONTKBN    (IDX)  TO  PTHKN-CONTKBN
           MOVE    SPA-PTHKN-KIGO       (IDX)  TO  PTHKN-KIGO
           MOVE    SPA-PTHKN-NUM        (IDX)  TO  PTHKN-NUM
           MOVE    SPA-PTHKN-SKKGETYMD  (IDX)  TO  PTHKN-SKKGETYMD
           MOVE    SPA-PTHKN-TEKSTYMD   (IDX)  TO  PTHKN-TEKSTYMD
           MOVE    SPA-PTHKN-TEKEDYMD   (IDX)  TO  PTHKN-TEKEDYMD
           MOVE    SPA-PTHKN-KAKUNINYMD (IDX)  TO  PTHKN-KAKUNINYMD
           MOVE    SPA-PTHKN-SAKJOKBN   (IDX)  TO  PTHKN-SAKJOKBN
      *
           .
       45011-HKN-SPA-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費追加処理
      *****************************************************************
       4502-PTKOHINF-LNK-SEC          SECTION.
      *
      *    削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHINF"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
               PERFORM 45021-KOH-SPA-PARA-SEC
      *
      *        追加
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "PTKOHINF"          TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    PTKOHINF-REC        TO  PARA-DATA-REC
      *
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "PARA-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02W1 PARA PTKOHINF INS ERR:"  MCP-RC
               END-IF
           END-PERFORM
      *
           .
       4502-PTKOHINF-LNK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険ＳＰＡ→パラメタ編集処理
      *****************************************************************
       45021-KOH-SPA-PARA-SEC          SECTION.
      *
           MOVE    SPACE               TO  PTKOHINF-REC
           INITIALIZE                      PTKOHINF-REC
      *
           MOVE    SPA-HOSPID          TO  PTKOH-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
           MOVE    SPA-PTKOH-KOHID     (IDX)   TO  PTKOH-KOHID
           MOVE    SPA-PTKOH-KOHNUM    (IDX)   TO  PTKOH-KOHNUM
           MOVE    SPA-PTKOH-PAYKBN    (IDX)   TO  PTKOH-PAYKBN
           MOVE    SPA-PTKOH-FTNJANUM  (IDX)   TO  PTKOH-FTNJANUM
           MOVE    SPA-PTKOH-JKYSNUM   (IDX)   TO  PTKOH-JKYSNUM
           MOVE    SPA-PTKOH-TEKSTYMD  (IDX)   TO  PTKOH-TEKSTYMD
           MOVE    SPA-PTKOH-TEKEDYMD  (IDX)   TO  PTKOH-TEKEDYMD
           MOVE    SPA-PTKOH-KAKUNINYMD(IDX)   TO  PTKOH-KAKUNINYMD
           MOVE    SPA-PTKOH-SAKJOKBN  (IDX)   TO  PTKOH-SAKJOKBN
      *
           .
       45021-KOH-SPA-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費ＴＢＬから公費負担へ編集処理
      *****************************************************************
       4504-PTKOHFTN-HEN-SEC          SECTION.
      *
      *    すべてを削除としておく
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN1-MAX
               MOVE    "1"             TO  SPA-NAI24-FTNDELKBN (IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
      *        更正、育成
               IF     (SPA-PTKOH-KOHNUM(IDX)   =   "015"
                                               OR  "016"
      *            生活保護
                                               OR  "012" )
      *            特定負有（Ｈ１５．１０以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "051" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20031001"))
      *            東京の特定疾患公費（Ｈ１５．１０以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "151" )  AND
                      (SPA-PREFNUM             =   13    )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20031001"))
      *            東京の４５１公費（Ｈ１５．１０以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "451" )  AND
                      (SPA-PREFNUM             =   13    )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20031001"))
      *
                   MOVE    ZERO                TO  IDY
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   SPA-FTN1-MAX
                       IF      SPA-PTKOH-KOHID(IDX)
                                               =   SPA-NAI24-KOHID(IDX2)
                           MOVE    IDX2            TO  IDY
                           MOVE    SPA-FTN1-MAX    TO  IDX2
                       END-IF
                   END-PERFORM
                   IF      IDY             =   ZERO
                       COMPUTE IDY         =   SPA-FTN1-MAX     +  1
                       MOVE    IDY         TO  SPA-FTN1-MAX
                       INITIALIZE              SPA-GMN24-FTNTBLO (IDY)
                   END-IF
      *
                   PERFORM 45041-PTKOHFTN-TBLHEN-SEC
               END-IF
           END-PERFORM
      *
           .
       4504-PTKOHFTN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担へ編集処理
      *****************************************************************
       45041-PTKOHFTN-TBLHEN-SEC          SECTION.
      *
           MOVE    SPA-PTKOH-KOHID     (IDX)   TO  SPA-NAI24-KOHID (IDY)
           MOVE    SPA-PTKOH-KOHNUM    (IDX)   TO  SPA-NAI24-KOHNUM(IDY)
           MOVE    SPA-PTKOH-PAYKBN    (IDX)   TO  SPA-NAI24-PAYKBN(IDY)
           MOVE    SPA-PTKOH-TEKSTYMD  (IDX)   TO  SPA-NAI24-TKOHSTYMD
                                                                   (IDY)
           MOVE    SPA-PTKOH-TEKEDYMD  (IDX)   TO  SPA-NAI24-TKOHEDYMD
                                                                   (IDY)
           MOVE    SPA-PTKOH-SAKJOKBN  (IDX)   TO  SPA-NAI24-FTNDELKBN
                                                                   (IDY)
      *
           MOVE    SPA-PTKOH-KOHNUM-MEI(IDX)   TO  SPA-GMN24-TFTNKOHMEI
                                                                   (IDY)
           MOVE    SPA-PTKOH-TEKSTYMDW (IDX)   TO  SPA-GMN24-TKOHSTYMD
                                                                   (IDY)
           MOVE    SPA-PTKOH-TEKEDYMDW (IDX)   TO  SPA-GMN24-TKOHEDYMD
                                                                   (IDY)
           .
       45041-PTKOHFTN-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ作成処理
      *****************************************************************
       4503-HKNCOMBI-SYORI-SEC          SECTION.
      *
      *    保険組合せ作成
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           CALL    "ORCGP02W2"         USING    SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4503-HKNCOMBI-SYORI-EXT
           END-IF
      *    保険組合ＩＤ設定処理
           CALL    "ORCGP02W3"     USING    SPA-AREA
                                            SPA-P02KYOUTU
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4503-HKNCOMBI-SYORI-EXT
           END-IF
      *
      *    保険組合せ表示（現在対象分のみ）
           PERFORM 3103-HKNCOMB-PARA-SEC
      *
           .
       4503-HKNCOMBI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険追加処理
      *****************************************************************
       460-HKN-TUIKA-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      460-HKN-TUIKA-EXT
           END-IF
      *
      *    追加の判定
           INITIALIZE                  SPA-GMN-HKN-AREA
           MOVE    ZERO                TO  SPA-NAI-HKNSTR
           MOVE    8                   TO  SPA-GMN-CUR
      *
           .
       460-HKN-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費追加処理
      *****************************************************************
       470-KOH-TUIKA-SEC          SECTION.
      *
      *    追加の判定
           MOVE    1                   TO  FLG-KOH-ADD2
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      470-KOH-TUIKA-EXT
           END-IF
      *
           IF      FLG-KOH-ADD         =   ZERO
               MOVE    "0090"          TO  SPA-ERRCD
           END-IF
      *
           EVALUATE    TRUE
               WHEN    SPA-GMN-KOHNUM(1)   =   SPACE
                   MOVE    20                  TO  SPA-GMN-CUR
               WHEN    SPA-GMN-KOHNUM(2)   =   SPACE
                   MOVE    26                  TO  SPA-GMN-CUR
               WHEN    SPA-GMN-KOHNUM(3)   =   SPACE
                   MOVE    32                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       470-KOH-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ変更・別画面切替　表示処理
      *****************************************************************
       4101-BETUGAMEN-SEC          SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU 
           MOVE    ZERO                TO  FLG-TOUROKU2
      *    保険組合せを作成する（入力チェック込み）
           PERFORM 450-HKNCOMBI-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      4101-BETUGAMEN-EXT
           END-IF
           .
       4101-BETUGAMEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前表示（基本：保険公費情報）処理
      *****************************************************************
       440-ZEN-GMNHYOUJI-SEC           SECTION.
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  SPA-GMN-CUR
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      440-ZEN-GMNHYOUJI-EXT
           END-IF
      *
      *    保険情報
           IF      SPA-NAI-HKNSTR      >   1
               MOVE    SPA-NAI-HKNSTR      TO  WRK-HKNSTR
      *        保険情報ＴＢＬから編集
               COMPUTE SPA-NAI-HKNSTR      =   SPA-NAI-HKNSTR  -  1
               PERFORM 3101-HKN-TBL-SPA-SEC
      *        対象がない時元へ
               IF      SPA-NAI-HKNSTR      =   ZERO
                   MOVE    WRK-HKNSTR          TO  SPA-NAI-HKNSTR
                   PERFORM 3101-HKN-TBL-SPA-SEC
               END-IF
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
      *    公費情報
           IF      SPA-NAI-KOHSTR      >   1
      *        対象開始位置を求める
               MOVE    ZERO                TO  WRK-KOHSTR
               MOVE    ZERO                TO  IDY
               PERFORM VARYING     IDX     FROM    1  BY  1
                       UNTIL      (IDX     >   SPA-PTKOH-MAX) OR
                                  (IDX     =   SPA-NAI-KOHSTR)
                   IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                       ADD     1               TO  IDY
                       IF      IDY             >   3
                           MOVE   1                TO  IDY
                       END-IF
                       IF      IDY             =   1
                           MOVE    IDX             TO  WRK-KOHSTR
                       END-IF
                   END-IF
               END-PERFORM
      *        公費情報ＴＢＬから編集
               IF      WRK-KOHSTR          >   ZERO
                   MOVE    WRK-KOHSTR          TO  SPA-NAI-KOHSTR
                   PERFORM 3101-KOH-TBL-SPA-SEC
               END-IF
               MOVE    20              TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       440-ZEN-GMNHYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次表示（基本：保険公費情報）処理
      *****************************************************************
       450-TUGI-GMNHYOUJI-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  SPA-GMN-CUR
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      450-TUGI-GMNHYOUJI-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *    保険情報
           IF      SPA-NAI-HKNSTR      <   SPA-PTHKN-MAX
               MOVE    SPA-NAI-HKNSTR      TO  WRK-HKNSTR
      *        保険情報ＴＢＬから編集
               ADD     1                   TO  SPA-NAI-HKNSTR
               PERFORM 3101-HKN-TBL-SPA-SEC
      *        対象がない時元へ
               IF     (SPA-NAI-HKNSTR      =   ZERO )  AND
                      (WRK-HKNSTR          >   ZERO )
                   MOVE    WRK-HKNSTR          TO  SPA-NAI-HKNSTR
                   PERFORM 3101-HKN-TBL-SPA-SEC
               END-IF
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
      *    公費情報
           IF     (SPA-NAI-KOHEND      <   SPA-PTKOH-MAX )  AND
                  (SPA-GMN-KOHNUM(3)   NOT =   SPACE )
               MOVE    SPA-NAI-KOHSTR      TO  WRK-KOHSTR
               MOVE    SPA-NAI-KOHEND      TO  SPA-NAI-KOHSTR
      *        保険情報ＴＢＬから編集
               ADD     1                   TO  SPA-NAI-KOHSTR
               PERFORM 3101-KOH-TBL-SPA-SEC
      *        対象がない時元へ
               IF      SPA-NAI-KOHSTR      =   ZERO
                   MOVE    WRK-KOHSTR          TO  SPA-NAI-KOHSTR
                   PERFORM 3101-KOH-TBL-SPA-SEC
               END-IF
               MOVE    20                  TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       450-TUGI-GMNHYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所検索へ
      *****************************************************************
       440-JYUSYO-SEC             SECTION.
      *
           INITIALIZE                            P98
           INITIALIZE                            P99
           INITIALIZE                            JPARA-REC
           CALL  "ORCSADR"                       USING
                                                 SPA-P02KYOUTU
                                                 SPA-P02SCRAREA
                                                 P98
                                                 P99
                                                 SPA-SYSYMD
           MOVE    "P02"                         TO  SPA-GMN98-MOTOPG
      *
           MOVE    SPA-P02KYOUTU                 TO  SPA-WORK
           MOVE    SPA-P02SCRAREA                TO  SPA-FREE
           MOVE    SPA-AREA                      TO  SPA-COMMON
      *
           MOVE  "P98"                           TO  SPA-SAKIPG
           MOVE  "P02"                           TO  SPA-MOTOPG
           MOVE  "NEW"                           TO  MCP-PUTTYPE
           MOVE  "SELNUM"                        TO  MCP-WIDGET
           MOVE  SPA-SAKIPG                      TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE  1                               TO  SPA-FLG-END
      *
           .
       440-JYUSYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災入力画面処理
      *****************************************************************
       481-ROUSAI-SEC             SECTION.
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "1001"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-CUR
               GO      TO      481-ROUSAI-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      481-ROUSAI-EXT
           END-IF
      *
      *    労災か自賠責が登録されていること
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPID          TO  HKN-HOSPID
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    "971"               TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           IF      FLG-HKNNUM          =   ZERO
               MOVE    1               TO  SPA-ROSAI-FLG
           ELSE
               MOVE    ZERO            TO  SPA-ROSAI-FLG
           END-IF
      *
      *    自賠責が登録されていること
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPID          TO  HKN-HOSPID
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    "973"               TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           IF      FLG-HKNNUM          =   ZERO
               MOVE    1               TO  SPA-JIBAI-FLG
           ELSE
               MOVE    ZERO            TO  SPA-JIBAI-FLG
           END-IF
      *
           IF     (SPA-ROSAI-FLG       =   ZERO )  AND
                  (SPA-JIBAI-FLG       =   ZERO )
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
               GO      TO              481-ROUSAI-EXT
           END-IF
      *
      *    現在表示されているかの判定
           IF      SPA-GMN-HKNNUM      =   "971"  OR
                                           "973"  OR
                                           SPACE
               CONTINUE
           ELSE
               MOVE    "0022"        TO  SPA-ERRCD
               MOVE    8             TO  SPA-GMN-CUR
               GO      TO            481-ROUSAI-EXT
           END-IF
      *
      *    保険　遷移パラメータセット
           MOVE    SPA-P02KYOUTU         TO  SPA-WORK
      *
      *    保険　画面情報セット処理
           PERFORM 481-RSIHKNGMN-SET-SEC
      *
           MOVE    "P02"                 TO  SPA-MOTOPG
           MOVE    "P02D"                TO  SPA-SAKIPG
      *
      *    保険入力画面へ
           MOVE   "NEW"                  TO  MCP-PUTTYPE.
           MOVE   "P02D"                 TO  MCP-WINDOW.
      *    カーソル位置指定
           EVALUATE    SPA-GMN2D-HKNKBN
               WHEN    "1"
                   MOVE   3                    TO  SPA-GMN2D-CUR
                   MOVE   "KOHNUM1"            TO  MCP-WIDGET
               WHEN    "2"
                   MOVE   4                    TO  SPA-GMN2D-CUR
                   MOVE   "KOHNUM2"            TO  MCP-WIDGET
               WHEN    "3"
                   MOVE   16                   TO  SPA-GMN2D-CUR
                   MOVE   "KOHNUM3"            TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE   1                    TO  SPA-GMN2D-CUR
                   MOVE   "HKNKBN"             TO  MCP-WIDGET
           END-EVALUATE
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                     TO  SPA-FLG-END
      *
      *
           .
       481-ROUSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険　画面情報セット処理
      *****************************************************************
       481-RSIHKNGMN-SET-SEC           SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02D"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           .
       481-RSIHKNGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU 
           MOVE    1                   TO  FLG-TOUROKU2
      *
           EVALUATE    SPA-P02-GAMEN
      *        保険組合せ履歴
               WHEN    "P027"
                   MOVE    2               TO  FLG-TOUROKU2
           END-EVALUATE
      *
      *    保険組合せを作成する（入力チェック込み）
           PERFORM 450-HKNCOMBI-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
               ADD     1               TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE   SPA-GMN-CUR
              WHEN    01
                  MOVE    "P02_KANANAME"         TO  MCP-WIDGET
              WHEN    02
                  MOVE    "P02_NAME"             TO  MCP-WIDGET
              WHEN    03
                  MOVE    "P02_SEX"              TO  MCP-WIDGET
              WHEN    04
                  MOVE    "P02_BIRTHDAY"         TO  MCP-WIDGET
              WHEN    05
                  MOVE    "P02_NICKNAME"         TO  MCP-WIDGET
              WHEN    06
                  MOVE    "P02_HAKKO"            TO  MCP-WIDGET
              WHEN    07
                  MOVE    "TSTPTNUMKBN"          TO  MCP-WIDGET
      *       保険情報
              WHEN    08
                  MOVE    "P02_HKNJANUM"         TO  MCP-WIDGET
              WHEN    09
                  MOVE    "P02_HKNNUM"         TO  MCP-WIDGET
              WHEN    10
                  MOVE    "P02_HONKZKBN"         TO  MCP-WIDGET
              WHEN    11
                  MOVE    "P02_HOJOKBN"         TO  MCP-WIDGET
              WHEN    12
                  MOVE    "P02_CONTKBN"         TO  MCP-WIDGET
              WHEN    13
                  MOVE    "P02_KIGO"             TO  MCP-WIDGET
              WHEN    14
                  MOVE    "P02_NUM"              TO  MCP-WIDGET
              WHEN    15
                  MOVE    "P02_SKKGETYMD"        TO  MCP-WIDGET
              WHEN    16
                  MOVE    "P02_HTEKSTYMD"        TO  MCP-WIDGET
              WHEN    17
                  MOVE    "P02_HTEKEDYMD"        TO  MCP-WIDGET
              WHEN    18
                  MOVE    "P02_HIHKNJANAME"      TO  MCP-WIDGET
              WHEN    19
                  MOVE    "P02_HKAKUNINYMD"      TO  MCP-WIDGET
      *    公費１
              WHEN    20
                  MOVE    "P02_FTNJANUM1"        TO  MCP-WIDGET
              WHEN    21
                  MOVE    "P02_KOHNUM1"          TO  MCP-WIDGET
              WHEN    22
                  MOVE    "P02_JKYSNUM1"         TO  MCP-WIDGET
              WHEN    23
                  MOVE    "P02_KTEKSTYMD1"       TO  MCP-WIDGET
              WHEN    24
                  MOVE    "P02_KTEKEDYMD1"       TO  MCP-WIDGET
              WHEN    25
                  MOVE    "P02_KKAKUNINYMD1"     TO  MCP-WIDGET
      *    公費１
              WHEN    26
                  MOVE    "P02_FTNJANUM2"        TO  MCP-WIDGET
              WHEN    27
                  MOVE    "P02_KOHNUM2"          TO  MCP-WIDGET
              WHEN    28
                  MOVE    "P02_JKYSNUM2"         TO  MCP-WIDGET
              WHEN    29
                  MOVE    "P02_KTEKSTYMD2"       TO  MCP-WIDGET
              WHEN    30
                  MOVE    "P02_KTEKEDYMD2"       TO  MCP-WIDGET
              WHEN    31
                  MOVE    "P02_KKAKUNINYMD2"     TO  MCP-WIDGET
      *    公費３
              WHEN    32
                  MOVE    "P02_FTNJANUM3"        TO  MCP-WIDGET
              WHEN    33
                  MOVE    "P02_KOHNUM3"          TO  MCP-WIDGET
              WHEN    34
                  MOVE    "P02_JKYSNUM3"         TO  MCP-WIDGET
              WHEN    35
                  MOVE    "P02_KTEKSTYMD3"       TO  MCP-WIDGET
              WHEN    36
                  MOVE    "P02_KTEKEDYMD3"       TO  MCP-WIDGET
              WHEN    37
                  MOVE    "P02_KKAKUNINYMD3"     TO  MCP-WIDGET
      *    
              WHEN    38
                  MOVE    "P02_SELNUM"           TO  MCP-WIDGET
              WHEN    39
                  MOVE    "P02_SRYKA"            TO  MCP-WIDGET
      *
              WHEN    40
                  MOVE    "P02_POST"             TO  MCP-WIDGET
              WHEN    41
                  MOVE    "P02_ADRS"            TO  MCP-WIDGET
              WHEN    42
                  MOVE    "P02_BANTI"            TO  MCP-WIDGET
              WHEN    43
                  MOVE    "P02_TEL1"             TO  MCP-WIDGET
              WHEN    44
                  MOVE    "P02_TEL2"             TO  MCP-WIDGET
              WHEN    45
                  MOVE    "P02_DISCOUNTKBN"     TO  MCP-WIDGET
              WHEN    46
                  MOVE    "P02_DISCOUNT"        TO  MCP-WIDGET
              WHEN    47
                  MOVE    "P02_SETAINUSI"        TO  MCP-WIDGET
              WHEN    48
                  MOVE    "P02_ZOKUGARA"        TO  MCP-WIDGET
              WHEN    49
                  MOVE    "P02_CONDITION1"      TO  MCP-WIDGET
              WHEN    50
                  MOVE    "P02_CONDITION2"      TO  MCP-WIDGET
              WHEN    51
                  MOVE    "P02_CONDITION3"      TO  MCP-WIDGET
              WHEN    52
                  MOVE    "P02_TABOO1"          TO  MCP-WIDGET
              WHEN    53
                  MOVE    "P02_TABOO2"          TO  MCP-WIDGET
              WHEN    54
                  MOVE    "P02_ALLERGY1"        TO  MCP-WIDGET
              WHEN    55
                  MOVE    "P02_ALLERGY2"        TO  MCP-WIDGET
              WHEN    56
                  MOVE    "P02_KANSENSYO1"      TO  MCP-WIDGET
              WHEN    57
                  MOVE    "P02_KANSENSYO2"      TO  MCP-WIDGET
              WHEN    58
                  MOVE    "P02_COMMENT1"        TO  MCP-WIDGET
              WHEN    59
                  MOVE    "P02_COMMENT2"        TO  MCP-WIDGET
      *       登録
              WHEN    60
                  MOVE    "B12"                 TO  MCP-WIDGET
      *       労災・自賠責
              WHEN    61
                  MOVE    "B09S"                TO  MCP-WIDGET
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
      *
           EVALUATE    SPA-MCP-WIDGET
      *    保険組合せ選択
               WHEN    "HKNLIST"
                   MOVE    38          TO  SPA-GMN-CUR
      *    かな氏名
               WHEN    "P02_KANANAME"
                   MOVE    1           TO  SPA-GMN-CUR
      *    漢字氏名
               WHEN    "P02_NAME"
                   MOVE    2           TO  SPA-GMN-CUR
      *    性別
               WHEN    "P02_SEX"
                   MOVE    3           TO  SPA-GMN-CUR
      *    生年月日
               WHEN    "P02_BIRTHDAY"
      *            カルテ発行へ
                   MOVE    5           TO  SPA-GMN-CUR
      *    通称
               WHEN    "P02_NICKNAME"
                   MOVE    5           TO  SPA-GMN-CUR
      *    カルテ発行
               WHEN    "P02_HAKKO"
      *            保険情報へ
                   MOVE    7           TO  SPA-GMN-CUR
      *    テスト患者
               WHEN    "TSTPTNUMKBN"
                   MOVE    7           TO  SPA-GMN-CUR
      *    保険情報
               WHEN    "P02_HKNJANUM"
                   MOVE    8           TO  SPA-GMN-CUR
               WHEN    "P02_HKNNUM"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "P02_HONKZKBN"
                   MOVE    10          TO  SPA-GMN-CUR
      *            補助がない時、継続へ
                   IF      SPA-GMN-HOJO-MAX    =   ZERO
                       MOVE    11              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_HOJOKBN"
                   MOVE    11          TO  SPA-GMN-CUR
      *            継続がない時、記号へ
                   IF      SPA-GMN-CONT-MAX    =   ZERO
                       MOVE    12              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_CONTKBN"
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    "P02_KIGO"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "P02_NUM"
                   MOVE    14          TO  SPA-GMN-CUR
               WHEN    "P02_SKKGETYMD"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "P02_HTEKSTYMD"
                   MOVE    16          TO  SPA-GMN-CUR
               WHEN    "P02_HTEKEDYMD"
                   MOVE    17          TO  SPA-GMN-CUR
               WHEN    "P02_HIHKNJANAME"
                   MOVE    18          TO  SPA-GMN-CUR
               WHEN    "P02_HKAKUNINYMD"
                   MOVE    19          TO  SPA-GMN-CUR
      *    公費１
               WHEN    "P02_FTNJANUM1"
                   MOVE    20          TO  SPA-GMN-CUR
                   IF     (SPA-GMN-FTNJANUM(1) =   SPACE) AND
                          (SPA-GMN-KOHNUM (1)  =   SPACE)
                       MOVE    39          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_KOHNUM1"
                   MOVE    21          TO  SPA-GMN-CUR
               WHEN    "P02_JKYSNUM1"
                   MOVE    22          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKSTYMD1"
                   MOVE    23          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKEDYMD1"
                   MOVE    24          TO  SPA-GMN-CUR
               WHEN    "P02_KKAKUNINYMD1"
                   MOVE    25          TO  SPA-GMN-CUR
      *    公費２
               WHEN    "P02_FTNJANUM2"
                   MOVE    26          TO  SPA-GMN-CUR
                   IF     (SPA-GMN-FTNJANUM(2) =   SPACE) AND
                          (SPA-GMN-KOHNUM (2)  =   SPACE)
                       MOVE    39          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_KOHNUM2"
                   MOVE    27          TO  SPA-GMN-CUR
               WHEN    "P02_JKYSNUM2"
                   MOVE    28          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKSTYMD2"
                   MOVE    29          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKEDYMD2"
                   MOVE    30          TO  SPA-GMN-CUR
               WHEN    "P02_KKAKUNINYMD2"
                   MOVE    31          TO  SPA-GMN-CUR
      *    公費３
               WHEN    "P02_FTNJANUM3"
                   MOVE    32          TO  SPA-GMN-CUR
                   IF     (SPA-GMN-FTNJANUM(3) =   SPACE) AND
                          (SPA-GMN-KOHNUM (3)  =   SPACE)
                       MOVE    39          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_KOHNUM3"
                   MOVE    33          TO  SPA-GMN-CUR
               WHEN    "P02_JKYSNUM3"
                   MOVE    34          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKSTYMD3"
                   MOVE    35          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKEDYMD3"
                   MOVE    36          TO  SPA-GMN-CUR
               WHEN    "P02_KKAKUNINYMD3"
                   MOVE    37          TO  SPA-GMN-CUR
      *    選択番号
               WHEN    "P02_SELNUM"
                   MOVE    38          TO  SPA-GMN-CUR
      *    診療科
               WHEN    "P02_SRYKA"
                   MOVE    39          TO  SPA-GMN-CUR
      *    郵便番号
               WHEN    "P02_POST"
                   MOVE    40          TO  SPA-GMN-CUR
      *            住所があれば次ぎへ
                   IF      SPA-GMN-EDITADRS-NAME   NOT =   SPACE
                       MOVE    41          TO  SPA-GMN-CUR
                   END-IF
      *    住所
               WHEN    "P02_ADRS"
                   MOVE    41          TO  SPA-GMN-CUR
      *    番地方書
               WHEN    "P02_BANTI"
                   MOVE    42          TO  SPA-GMN-CUR
      *    電話昼
               WHEN    "P02_TEL1"
                   MOVE    43          TO  SPA-GMN-CUR
      *    電話夜
               WHEN    "P02_TEL2"
                   MOVE    44          TO  SPA-GMN-CUR
      *    減免事由
               WHEN    "P02_DISCOUNTKBN"
                   MOVE    45          TO  SPA-GMN-CUR
      *    割引率
               WHEN    "P02_DISCOUNT"
                   MOVE    46          TO  SPA-GMN-CUR
      *    世帯主
               WHEN    "P02_SETAINUSI"
                   MOVE    47          TO  SPA-GMN-CUR
      *    続柄
               WHEN    "P02_ZOKUGARA"
                   MOVE    48          TO  SPA-GMN-CUR
      *    状態１
               WHEN    "P02_CONDITION1"
                   MOVE    49          TO  SPA-GMN-CUR
      *    状態２
               WHEN    "P02_CONDITION2"
                   MOVE    50          TO  SPA-GMN-CUR
      *    状態３
               WHEN    "P02_CONDITION3"
                   MOVE    51                  TO  SPA-GMN-CUR
      *    禁忌
               WHEN    "P02_TABOO1"
                   MOVE    52                  TO  SPA-GMN-CUR
               WHEN    "P02_TABOO2"
                   MOVE    53                  TO  SPA-GMN-CUR
      *    アレルギー
               WHEN    "P02_ALLERGY1"
                   MOVE    54                  TO  SPA-GMN-CUR
               WHEN    "P02_ALLERGY2"
                   MOVE    55                  TO  SPA-GMN-CUR
      *    感染症
               WHEN    "P02_KANSENSYO1"
                   MOVE    56                  TO  SPA-GMN-CUR
               WHEN    "P02_KANSENSYO2"
                   MOVE    57                  TO  SPA-GMN-CUR
      *    コメント
               WHEN    "P02_COMMENT1"
                   MOVE    58                  TO  SPA-GMN-CUR
      *    コメント２
               WHEN    "P02_COMMENT2"
                   MOVE    59                  TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       410112-SE-WAFORMAT-YMDEDIT-SEC  SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HENYMD
               GO      TO      410112-SE-WAFORMAT-YMDEDIT-EXT
           END-IF
           IF      WRK-SYMD            =   ZERO
               MOVE    "0000000"       TO  WRK-HENYMD
               GO      TO      410112-SE-WAFORMAT-YMDEDIT-EXT
           ELSE
               IF      WRK-SYMD            =   ALL "9"
                   MOVE    "9999999"       TO  WRK-HENYMD
                   GO      TO      410112-SE-WAFORMAT-YMDEDIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMD
           END-IF
           .
       410112-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角空白を全角空白へ変換処理
      *****************************************************************
       890-HALF-ZEN-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    WRK-MAE-INPUT       TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSTEM-READ-SEC         SECTION.
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
      *
           .
       900-SYSTEM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ検索処理
      *****************************************************************
       5510-PTINF-KENSAKU-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTINF-KEY"     TO  ORC-DBPATH
               PERFORM 900-PTINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       5510-PTINF-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      **    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険番号マスタ次検索処理
      ****************************************************************
       921-HKNNUM-NEXT-SEC              SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNNUM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付（受診内容）読み込み
      *****************************************************************
       930-UKETUKE-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       930-UKETUKE-NEXT-EXT.
           EXIT.
      *
      *----(01.00.15) LINE ADD START ---------------------------------
      *****************************************************************
      *    人名テーブル検索
      *****************************************************************
       941-JINMEI-SELECT-SEC           SECTION.
      *
           MOVE    "JINMEI-KEY2"       TO  ORC-DBPATH
           MOVE    JINMEI-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JINMEI-REC
           ELSE
               INITIALIZE                  JINMEI-REC
               MOVE    1               TO  FLG-JINMEI
           END-IF
           .
      *
       941-JINMEI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    人名テーブル読み込み
      *****************************************************************
       941-JINMEI-NEXT-SEC           SECTION.
      *
           MOVE    "JINMEI-KEY2"      TO  ORC-DBPATH
           MOVE    "DBFETCH"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JINMEI-REC
           ELSE
               INITIALIZE                  JINMEI-REC
               MOVE    1               TO  FLG-JINMEI
           END-IF
           .
      *
       941-JINMEI-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    人名テーブルクローズ処理
      *****************************************************************
       941-JINMEI-CLOSE-SEC           SECTION.
      *
           MOVE    "JINMEI-KEY2"       TO  ORC-DBPATH
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
      *
       941-JINMEI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO                TO  FLG-HKNJAINF
           ELSE
               INITIALIZE               HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *----(01.00.15) LINE ADD END   ---------------------------------
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE
      *        INVALID
      *            MOVE    1           TO  FLG-PARA
      *        NOT INVALID
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      **** EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *990-PARA-NEXT-SEC           SECTION.
      *
      *    READ    PARA-FILE       NEXT
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *990-PARA-NEXT-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW           SECTION.
      *
           MOVE    "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"       USING
                                     MCPAREA
                                     ORCMCPAREA
                                     MCPDATA-REC.
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
