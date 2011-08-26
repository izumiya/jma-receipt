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
      ******************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0031.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 点数金額モジュールからワーク診療行為へ変換
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   02/04/30  改正に伴う修正
      *  01.00.02    NACL-多々納  03/05/07  投薬を院内へ
      *  01.00.03    NACL-藤原    03/09/30  病名コードの取り込み
      *  01.00.04    NACL-藤原    04/01/29  転帰区分の編集
      *  01.00.05    NACL-多々納  04/02/27  保険修正
      *  01.00.06    NACL-藤原    04/03/19  病名検索にサブパラメタ追加
      *  01.00.07    NACL-藤原    04/03/22  更新エラー時の処理終了
      *  01.00.08    NACL-多々納  04/03/29  外用回数編集修正
      *  01.00.09    NACL-多々納  04/08/24  入院回数修正
      *  01.00.10    NACL-多々納  04/10/21  ドクター追加
      *  01.00.11    NACL-多々納  04/11/09  入院追加
      *  01.00.12    NACL-藤原    05/03/10  病名コードの取り込み
      *                                     未コード化傷病名コードの場合
      *  01.00.13    NACL-藤原    05/05/25  疑いフラグ、主病名フラグによ
      *                                     る病名の更新
      *  01.00.14    NACL-藤原    05/08/25  更新時の疑いフラグの設定
      *  01.00.15    NACL-多々納  05/09/26  加算のみの剤を有効とする
      *  01.00.16    NACL-多々納  05/12/06  受付対応
      *                                     MONFUNC対応
      *  01.00.17    NACL-多々納  06/01/16  乳幼児特例対応
      *  01.00.18    NACL-藤原    06/01/25  疑いフラグからの病名の編集方
      *                                     法の変更
      *  01.00.19    NACL-多々納  06/07/03  置換え処理追加他
      *  03.05.00    NACL-多々納  07/05/10  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/25  UIDテーブル対応
      *  04.04.00    NACL-竹田　  09/02/25  公害対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/06/25  薬剤情報提供料・調基判定追加
      *  04.05.00    NACL-多々納  09/11/30  入院回数一括入力対応
      *  04.05.00    NACL-多々納  10/01/15  治験対応
      *  04.05.01    NACL-多々納  10/12/06  ４０剤、４０明細対応他
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SPECIAL-NAMES.
      *    CONSOLE         IS  STDOUT.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療行為テーブル
      *    SELECT  SRYKBN-FILE     ASSIGN  "SRYKBNNAME.TXT,T"
      *                            FILE    STATUS  IS  STS-SRYKBN.
      *    保険情報
      **   SELECT  LF03-FILE       ASSIGN  "/var/tmp/LF03V01"
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  LF03-KEY
      *                            FILE    STATUS  IS  STS-LF03.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為テーブル
      *FD  SRYKBN-FILE.
      *01  SRYKBN-REC.
      *    03  SYRKBN-DATA.
      *        05  SYRKBN-SRYKBN         PIC X(02).
      *        05                        PIC X(01).
      *        05  SYRKBN-SRYSYUKBN      PIC X(03).
      *********05  SYRKBN-SRYMEI         PIC X(40).
      *
      *FD  LF03-FILE.
      *01  LF03-REC. 
      ***  COPY    "CPRLF003.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-SRYKBN          PIC X(02).
      ***  03  STS-LF03            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-CLMUID          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-PTBYOMEI-UMU    PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-UKE             PIC 9(01).
      *
           03  FLG-ZAIEND          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *    0:正常 1:エラー  2:単独病名エラー
      *    3:コード順エラー 4:修飾語のみ
      *    5:未コード化傷病名コード
           03  FLG-BYOMEI-CHK      PIC 9(01). 
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK1             PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-KOHARI          PIC 9(01).
      *    点滴・中心静脈
           03  FLG-TENTEKI         PIC 9(01). 
           03  FLG-CHUSIN          PIC 9(01). 
      *
           03  FLG-HKN-CHK         PIC 9(01). 
      *
           03  FLG-NYUINIDOU       PIC 9(01). 
      *
           03  FLG-SURYO           PIC 9(01). 
      *    薬剤情報提供料対象
           03  FLG-YKJYO           PIC 9(01). 
           03  FLG-CHOKI           PIC 9(01). 
      *
      *    キー領域
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-SRYKA     PIC X(02).
           03  KEY-OLD.
               05  KEY-O-SRYKA     PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-Y               PIC 9(04).
      *
           03  IDK                 PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDK-MAX             PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDH1                PIC 9(04).
           03  IDH-MAX             PIC 9(04).
      *
           03  TBL-MAX             PIC 9(04).
           03  TBL-IDX             PIC 9(04).
      *
           03  IDX-I               PIC 9(04).
           03  IDX-I1              PIC 9(04).
           03  IDX-I2              PIC 9(04).
      *
           03  IDK1                PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT01           PIC 9(04).
           03  CNT-OUT02           PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-HOSPNUM         PIC  9(02).
           03  WRK-HOSPID          PIC  X(24).
           03  WRK-SYOHOKBN        PIC  X(01).
           03  WRK-HKNCOMBI        PIC  9(04).
           03  WRK-SRYYMD          PIC  X(08).
      *
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-FTNJANUM        PIC X(08).
           03  WRK-JKSNUM          PIC X(20).
      *
           03  WRK-KOH-AREA.
               05  WRK-KOHID           PIC 9(10)   OCCURS  4.
               05  WRK-TANSEIDONAME    PIC X(20)   OCCURS  4.
      *
           03  WRK-RENNUM          PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-JIKAN           PIC X(01).
           03  WRK-TIMEKBN         PIC X(01).
      *
           03  WRK-CNT             PIC 9(05).
           03  WRK-KAISU           PIC 9(05).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-PTID            PIC 9(10).
           03  WRK-SRYKA           PIC X(02).
           03  WRK-SRYKA-KEY       PIC X(02).
           03  WRK-NYUGAIKBN       PIC X(01).
      *
           03  WRK-DRCD            PIC X(05).
      *
           03  WRK-KASAN           PIC 9(04).
           03  WRK-SYUGI           PIC 9(04).
           03  WRK-YAKUZAI         PIC 9(04).
      *
           03  WRK-SURYO           PIC 9(05)V9(5).
      *
           03  WRK-BYOYMD.
               05  WRK-BYOYY           PIC 9(04).
               05  WRK-BYOMM           PIC 9(02).
               05  WRK-BYODD           PIC 9(02).
           03  WRK-TENKIYMD.
               05  WRK-TENKIYY         PIC X(04).
               05  WRK-TENKIMM         PIC X(02).
               05  WRK-TENKIDD         PIC X(02).
           03  WRK-TENKI               PIC X(01).
           03  WRK-SYUBYOFLG           PIC X(01).
           03  WRK-UTAGAIFLG           PIC X(01).
           03  WRK-UTAGAIFLG-KETASU    PIC 9(02).
      *
           03  WRK-BYOMEI-TBL. 
               05  WRK-BYOMEI-G    OCCURS  7.
                   07  WRK-TBYOMEICD   PIC X(10).
                   07  WRK-TBYOMEI     PIC X(100).
                   07  WRK-TTANDOKUKBN PIC 9(02).
                   07  WRK-TUTAGAIFLG  PIC X(01).
           03  WRK-BYOMEICD        PIC X(07).
           03  WRK-BYOMEI.
               05  WRK-BYOMEI1     PIC X(80).
               05  WRK-BYOMEI2     PIC X(20). 
           03  WRK-LENGTH          PIC 9(02).
           03  WRK-TANDOKUKBN      PIC 9(01).
           03  WRK-TOKSKNCD        PIC 9(02).
           03  WRK-RECECD-TBL.
               05  WRK-RECECD      PIC X(01)   OCCURS  3.
           03  WRK-MOJISU          PIC 9(03).
      *
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(04).
               05  WRK-WMM         PIC X(02).
               05  WRK-WDD         PIC X(02).
           03  WRK-IN              PIC X(01).
           03  WRK-TEN             PIC 9(13)V9(05).
      *
           03  WRK-INPUTCD         PIC X(50).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC X(04).
               05  WRK-SYSMM       PIC X(02).
               05  WRK-SYSDD       PIC X(02).
      *                             
           03  WRK-PATHNAME             PIC X(64).
      *                             
           03  WRK-DBPATH.
               05  FILLER              PIC S9(9)   BINARY.
               05  FILLER              PIC S9(9)   BINARY.
               05  FILLER              PIC S9(9)   BINARY.
      *
      *    コメント入力値編集
           03  WRK-INPUTCHI-G.
               05  WRK-INPUTCHI        PIC X(12)   OCCURS  4.
      *
           03  SYS-YMD.
               05  SYS-SYSYY1      PIC 9(02) VALUE  20.
               05  SYS-SYSYMD. 
                 07  SYS-SYSYY2    PIC 9(02).
                 07  SYS-SYSMM     PIC 9(02).
                 07  SYS-SYSDD     PIC 9(02).
      *
       01  WRK-HENYMD.
           03  WRK-HENYY           PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "-".  
           03  WRK-HENMM           PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "-".
           03  WRK-HENDD           PIC X(02).
      *
      *    ワーク診療行為マスタ−テーブル
       01  TBL-WKSRYACT-AREA.
           02  TBL-WKSRYACT-REC      OCCURS   40.
           COPY    "CPWKSRYACT.INC"  REPLACING  //WKSRY//
                                     BY         //TBL-WKSRY//.
      *
      *    最小診療科病名情報
       01  WRK-PTBYOMEI-SRYKA-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO-SRYKA//.
      *
      *    更新対象病名情報
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO//.
      *
      *    診療科別更新対象病名情報
       01  WRK-PTBYOMEI-DOUKI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO-DOUKI//.
      *
      *    病名情報退避
       01  WRK-PTBYOMEI-TAIHI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO-TAIHI//.
      *
      *    対象外レセ電コード
           COPY    "CPJIDO.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    転帰レセ電コード
           COPY    "CPMML0016.INC".
      *
      *    用法レセ電コード
           COPY    "CPCLAIM006.INC".
      *
      *    未コード化傷病名コード
       01  WRK-CONS-BYOMEICD       PIC X(07)   VALUE   "0000999".
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
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
           COPY    "CPSK1007.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    ＣＬＡＩＭ接続情報
           COPY  "CPSK9000.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    公費マスタ（テーブル）
       01  TBL-PTKOHINF-AREA.
         02  TBL-PTKOHINF-REC           OCCURS  40.
           COPY    "CPPTKOHINF.INC"     REPLACING
                                        //PTKOH-// BY //TBL-PTKOH-//.
      *
      *    保険組み合わせ
       01  TBL-HKNCOMBI-AREA.
         02  TBL-HKNCOMBI-REC           OCCURS  40.
           COPY    "CPHKNCOMBI.INC"     REPLACING
                                        //COMB-// BY //TBL-COMB-//.
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *    受付退避
       01  WRK-UKETUKE-REC.
           COPY    "CPUKETUKE.INC"  REPLACING   //UKE-//
                                     BY         //WRK-UKE-//.
      *
      *    ＵＩＤテーブル
      *01  CLMUID-REC.
           COPY    "CPCLMUID.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    病名コード検索サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *    入院サブ
           COPY    "CPORCL0031N.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    診療行為算定日一括入力パラメタ 
           COPY    "CPORCSNDAYCHG.INC".
      * 
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  LF04-REC. 
           COPY    "CPRLF004.INC".
      *
      *    claim連携リターンエリア
       01  LNK-ERROR-AREA.
           03  LNK-KBN-CODE            PIC X(01).
           03  LNK-ERR-CODE            PIC 9(02).
           03  LNK-ERR-MSG             PIC X(100).
           03  LNK-OUT-HKNCOMBI        PIC X(04).
           03  LNK-OUT-CNT1            PIC 9(04).
           03  LNK-OUT-CNT2            PIC 9(04).
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                               LF04-REC
                                               LNK-ERROR-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
            PERFORM 100-INIT-SEC
      *
            PERFORM 200-MAIN-SEC
      *
      *     PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
      *D   DISPLAY "LF04-PTID1 = " "[" LF04-PTID1 "]"
      *D   DISPLAY "LF04-PERFORM-TIME = " "[" LF04-PERFORM-TIME "]"
      *
      *****診療種別テーブルセット
      *    OPEN    INPUT               SRYKBN-FILE
      *    IF      STS-SRYKBN      NOT =   ZERO
      *        DISPLAY "SRYKBN-FILE OPEN ERR :" STS-SRYKBN 
      *                                UPON  CONSOLE
      *     END-IF
      *    MOVE    SPACE               TO  MSYU-DATA-REC
      *    INITIALIZE                  MSYU-DATA-REC
      *    MOVE    ZERO                TO  FLG-SRYKBN
      *    MOVE    ZERO                TO  IDX
      *    PERFORM UNTIL    FLG-SRYKBN     =   1
      *        READ                    SRYKBN-FILE
      *            AT  END
      *                MOVE    1           TO  FLG-SRYKBN
      *            NOT AT  END
      *               ADD     1           TO  IDX
      *               IF      IDX         >   100
      *                    CONTINUE
      *                ELSE
      *                    MOVE    SRYKBN-REC      TO  MSYU-TBL-OCC
      *                                                (IDX)
      *                END-IF
      *        END-READ
      *    END-PERFORM
      *
      *****CLOSE                   SRYKBN-FILE
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           INITIALIZE                  SPA-AREA
      *    診療日決定
      *TEST
          IF       LF04-PERFORM-TIME   =   SPACE
            MOVE   LF04-ORDER-TIME     TO  LF04-PERFORM-TIME
          END-IF
      *TEST
           MOVE    LF04-PERFORM-TIME(1:4)
                                       TO  WRK-SYSYY
           MOVE    LF04-PERFORM-TIME(6:2)
                                       TO  WRK-SYSMM
           MOVE    LF04-PERFORM-TIME(9:2)
                                       TO  WRK-SYSDD
           IF      WRK-SYSYMD          =   SPACE
               ACCEPT SYS-SYSYMD       FROM  DATE
               MOVE   20               TO    SYS-SYSYY1
               MOVE   SYS-YMD          TO    WRK-SYSYMD          
           END-IF
           DISPLAY "========================================="
           DISPLAY "医療機関ID = [" LF04-CRE001-FACILITYID "]"
           DISPLAY "診療年月日 = [" WRK-SYSYMD "]"
           MOVE    WRK-SYSYMD          TO  WRK-SRYYMD
           MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           MOVE    WRK-SYSYMD          TO  SPA-SRYYMD
      *
           MOVE    "1"                 TO  SPA-NOCHK-HOSPNUM
      *
      *    システム管理検索（医療機関情報）
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key19"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL   FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *
      *        施設IDから該当の医療機関を決定する
               IF      LF04-CRE001-FACILITYID  =   SYS-1001-HOSPID
      *            医療機関ＩＤ
                   MOVE    SYS-1001-HOSPNUM    TO  WRK-HOSPNUM
      *            医療機関ＩＤ
                   MOVE    SYS-1001-HOSPID     TO  WRK-HOSPID
      *            処方区分
                   MOVE    SYS-1001-SYOHOKBN   TO  WRK-SYOHOKBN
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key19"             TO  MCP-PATHNAME
                   PERFORM 900-KANRIMST-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-HOSPNUM         TO  SPA-HOSPNUM
           DISPLAY "医療機関番号 = [" SPA-HOSPNUM "]"
      *
           MOVE    SPACE               TO  SPA-NOCHK-HOSPNUM
      *
      *    システム管理検索（ＣＬＡＩＭ接続情報）
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                      SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-9000-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-9000-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-9000-REC
               INITIALIZE                  SYS-9000-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9000-REC
           ELSE
               INITIALIZE                  SYS-9000-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      SYS-1001-HOSPSBT1    =   1
               MOVE    SPACE            TO  SYS-9000-BYOMEI-DOUKI-FLG
                                            SYS-9000-BYOMEI-SYU-FLG
           END-IF
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1009-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1009-REC
               INITIALIZE                  SYS-1009-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               MOVE    SYS-1009-JIYKSIKBN
                                           TO  SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                           TO  SPA-1009-JIYKSIKETA
      *拡張構成区分
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
           END-IF
      *
           MOVE    SPACE               TO  SYS-1007-REC
           INITIALIZE                  SYS-1007-REC
           MOVE    "1007"              TO  SYS-1007-KANRICD
           MOVE    "*"                 TO  SYS-1007-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1007-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1007-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1007-HOSPNUM
           MOVE    SYS-1007-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1007-REC
               INITIALIZE                  SYS-1007-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-CHOKI
           MOVE    ZERO                TO  FLG-YKJYO
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1007-REC
      *        調基対象
               IF      SYS-1007-SANTEI (1:1)   =   "1"
                   MOVE    1                   TO  FLG-CHOKI
               END-IF
      *        薬剤情報提供料対象
               IF      SYS-1007-SANTEI (2:1)   =   SPACE
                   MOVE    "0"                 TO  SYS-1007-SANTEI (2:1)
               END-IF
               IF      SYS-1007-SANTEI (3:1)   =   SPACE
                   MOVE    "0"                 TO  SYS-1007-SANTEI (3:1)
               END-IF
               IF      SYS-1007-SANTEI (4:1)   =   SPACE
                   MOVE    "0"                 TO  SYS-1007-SANTEI (4:1)
               END-IF
               IF     (SYS-1007-SANTEI (2:1)   =   "0"  )  AND
                      (SYS-1007-SANTEI (3:1)   =   "0"  )  AND
                      (SYS-1007-SANTEI (4:1)   =   "0"  )
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-YKJYO
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    患者ＩＤ，診療科編集
           PERFORM 2000-SYOKI-SET-SEC
      *    保険者マスター編集
           PERFORM 2001-HKNCOMBI-SET-SEC
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    時間外区分
           MOVE    LF04-JIKANNGAIKBN   TO  WRK-JIKAN
      *    剤毎の処理
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  FLG-ERR
      *    入院の時、同じワーク診療行為の判定
           IF      WRK-NYUGAIKBN       =   "1"
               PERFORM 2009-WKSRYACT-CHK-SEC
           ELSE
      *    外来の時、同じワーク診療行為の判定
               PERFORM 20010-WKSRYACT-CHK2-SEC
           END-IF
      *
           DISPLAY "診療情報 ================== " 
           DISPLAY " BUNDLE 情報"
      *
           MOVE    ZERO                TO  FLG-NYUINIDOU
           INITIALIZE                  TBL-WKSRYACT-AREA
           MOVE    ZERO                TO  TBL-MAX
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   40  )  OR
      *            UNTIL      (IDX1    >   20  )  OR
                             (LF04-SRYCD (IDX1)   =   SPACE)  OR
                             (FLG-END             =   1    )
                         OR  (FLG-ERR             =   1    )
               DISPLAY "  " IDX1 " " LF04-SRYCD (IDX1) 
      *        入院移動判定
               IF     (WRK-NYUGAIKBN       =   "1"   ) AND
                      (LF04-SRYCDTBL (IDX1)    =   "Claim010" )
                   MOVE    1               TO  FLG-NYUINIDOU
               ELSE
      *
                   DISPLAY "   ITEM 情報"
      *
                   PERFORM 2002-ZAICHK-SEC
                   IF      FLG-ZAIEND          =   1
                       IF      TBL-MAX         >   ZERO
                           PERFORM 2004-WKSRYACT-OUT-SEC
                       END-IF
                       IF      FLG-END         =   ZERO
                           PERFORM 2003-TBLSET-SEC
                       END-IF
                   ELSE
                       PERFORM 2005-TBLSET-NEXT-SEC
                   END-IF
               END-IF
               DISPLAY "  " 
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
              AND (FLG-ERR             =   ZERO )
               IF      TBL-MAX         >   ZERO
                   PERFORM 2004-WKSRYACT-OUT-SEC
               END-IF
      *
      ***      病名セット処理
      ****     PERFORM 2006-BYOMEI-SET-SEC
      *
      *        入院移動処理
               IF      FLG-NYUINIDOU       =   1
                   PERFORM 2007-ORCL0031N-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
      *        病名セット処理
               PERFORM 2006-BYOMEI-SET-SEC
           END-IF
      *
      *    外来の時、受付に会計待を設定
           IF      WRK-NYUGAIKBN       =   "2"
              AND (FLG-ERR             =   ZERO )
               PERFORM 2010-UKETUKE-CHK-SEC
           END-IF
      *
           IF      LNK-ERR-CODE        =   ZERO
      *        登録あり
               MOVE    CNT-OUT01           TO  LNK-OUT-CNT1
               MOVE    CNT-OUT02           TO  LNK-OUT-CNT2
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院移動処理
      *****************************************************************
       2007-ORCL0031N-SEC                SECTION.
      *
           INITIALIZE                  SCL31N-LNK
           MOVE    SPA-HOSPNUM         TO  SCL31N-HOSPNUM
           MOVE    WRK-SRYKA           TO  SCL31N-SRYKA
           MOVE    WRK-DRCD            TO  SCL31N-DRCD
           MOVE    SPTID-PTNUM         TO  SCL31N-PTNUM
           MOVE    WRK-PTID            TO  SCL31N-PTID
           MOVE    WRK-HKNCOMBI        TO  SCL31N-HKNCOMBI
           MOVE    WRK-NYUGAIKBN       TO  SCL31N-NYUGAIKBN
           MOVE    WRK-SRYYMD          TO  SCL31N-SRYYMD
      *
            CALL    "ORCL0031N"         USING
                                        SCL31N-LNK
                                        LF04-REC
      *
           .
       2007-ORCL0031N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者ＩＤ，診療科初期編集処理
      *****************************************************************
       2000-SYOKI-SET-SEC                SECTION.
      *
      *    患者ＩＤ
      *    INITIALIZE                      ORCSNUMAREA
      *    MOVE    LF04-PTID1          TO  SNUM-INX
      *    CALL    "ORCSNUM"           USING
      *                                ORCSNUMAREA
      *    IF     (SNUM-RC         NOT =   ZERO  )   OR
      *           (SNUM-MINKBN         =   "1"   )
      *        MOVE    LF04-PTID1          TO  WRK-PTID
      *    ELSE
      *        MOVE    SNUM-OUTNUM         TO  WRK-PTID
      *    END-IF
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           DISPLAY "患者番号 = ["    LF04-PTID1 "]"
           MOVE    LF04-PTID1          TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
      *---(01.00.02) LINE ADD START  ----------------------------------
                                               SPA-AREA
      *---(01.00.02) LINE ADD END    ----------------------------------
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
               DISPLAY  "PRNUM ERR SPA-HOSPNUM = " "[" SPA-HOSPNUM "]"
               DISPLAY  "PRNUM ERR PTID = " "[" LF04-PTID1 "]"
           END-IF
           MOVE    SPTID-PTID          TO  WRK-PTID
      *    診療科
           IF      LF04-CRE001-DPERTID =   SPACE
               MOVE    "01"                TO  WRK-SRYKA
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    LF04-CRE001-DPERTID TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    "01"            TO  WRK-SRYKA
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)
                                           TO  WRK-SRYKA
               END-IF
           END-IF
      *
           DISPLAY "診療科   = ["    WRK-SRYKA "]"
      *
           MOVE    WRK-SRYKA           TO  WRK-SRYKA-KEY
      *    レセ電の科から診療科へ変更
           IF      SYS-9000-RECEDEN-FLG    =   "1"
               PERFORM 2002-RECDEN-SRYKA-SEC
           END-IF
      *
      *
      *    入院外来区分 (true OR false) (claim:admitFlag)
           IF      LF04-NYUGAIKBN      =   "true"
               MOVE   "1"              TO  WRK-NYUGAIKBN
           ELSE
               MOVE   "2"              TO  WRK-NYUGAIKBN
           END-IF
      *
      *    ドクター
           DISPLAY "ドクタ   = ["  LF04-DOCCD "]"
           IF     (LF04-DOCCD (1:5)    NUMERIC )  AND
                  (LF04-DOCCD (1:1)    =   "1" )
               MOVE    LF04-DOCCD(1:5)     TO  WRK-DRCD
               PERFORM 2003-DRCD-CHK-SEC
           ELSE
               MOVE    SPACE               TO  WRK-DRCD
           END-IF
      *
           DISPLAY "ドクタ   = ["  WRK-DRCD "]"
      *
           .
       2000-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       2003-DRCD-CHK-SEC                SECTION.
      *
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   1
               MOVE    SPACE               TO  WRK-DRCD
           END-IF
      *
      *
           .
       2003-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電の科から診療科へ変更処理
      *****************************************************************
       2002-RECDEN-SRYKA-SEC                SECTION.
      *
      *
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                      SYS-1005-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           PERFORM
               UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *        レセ電科
               IF      SYS-1005-RECESRYKA  =   WRK-SRYKA
      *            診療科置換え
                   MOVE    SYS-1005-KBNCD      TO  WRK-SRYKA
                   MOVE    1                   TO  FLG-SYSKANRI
               ELSE
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-KANRIMST-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2002-RECDEN-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター編集処理
      *****************************************************************
       2001-HKNCOMBI-SET-SEC                SECTION.
      *
      ***  OPEN    INPUT               LF03-FILE
      *    OPEN    I-O                 LF03-FILE
      *
      *    MOVE    SPA-HOSPNUM         TO  LF03-HOSPNUM
      *    MOVE    WRK-PTID            TO  LF03-PTID
      *    MOVE    WRK-SRYKA-KEY       TO  LF03-SRYKA
      *D   DISPLAY "LF03-HOSPNUM = " "[" LF03-HOSPNUM "]"
      *D   DISPLAY "LF03-PTID = " "[" LF03-PTID "]"
      *D   DISPLAY "LF03-SRYKA = " "[" LF03-SRYKA "]"
      *    PERFORM 900-LF03-READ-SEC
      *    IF      FLG-LF03        =   1
      *        DISPLAY "*** ORCL0031 LF03 INVALID ***"
      *        ACCEPT  WRK-IN
      *****END-IF
      *    UID検索
           INITIALIZE                      CLMUID-REC
           MOVE    SPA-HOSPNUM         TO  CLMUID-HOSPNUM
           MOVE    WRK-PTID            TO  CLMUID-PTID
           MOVE    WRK-SRYKA-KEY       TO  CLMUID-SRYKA
           PERFORM 903-CLMUID-READ-SEC
           IF      FLG-CLMUID          =   1
               DISPLAY "*** ORCL0031 CLMUID INVALID ***"
           END-IF
      *
           MOVE    ZERO                TO  WRK-HKNCOMBI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               IF      CLMUID-UID (IDX)    =   LF04-HOKEN-UID
                   MOVE    CLMUID-HKNCOMBI (IDX)   TO  WRK-HKNCOMBI
                   MOVE    20                  TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HKNCOMBI        =   ZERO
      *        保険情報から保険の決定
               PERFORM 20011-HKNSYORI-SEC
           END-IF
      *
           DISPLAY "保険組合せ = [" WRK-HKNCOMBI "]"
      **** CLOSE                       LF03-FILE
      *
           MOVE    WRK-HKNCOMBI        TO  LNK-OUT-HKNCOMBI
      *
           .
       2001-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報から保険の決定処理
      *****************************************************************
       20011-HKNSYORI-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNNUM
           MOVE    SPACE               TO  WRK-FTNJANUM
           MOVE    SPACE               TO  WRK-JKSNUM
      *    主保険
      *    健康保険種別判定
           EVALUATE    LF04-HKN-HKNSBTCODE (1)
               WHEN    SPACE
                   MOVE    SPACE               TO  WRK-HKNNUM
               WHEN    "00"
      *            国保
                   MOVE    "060"               TO  WRK-HKNNUM
               WHEN    "R1"
      *            労災
                   MOVE    "971"               TO  WRK-HKNNUM
               WHEN    "R3"
      *            自賠責
                   MOVE    "973"               TO  WRK-HKNNUM
               WHEN    "K5"
      *            公害
                   MOVE    "975"               TO  WRK-HKNNUM
               WHEN    "99"
      *            包括入力
                   MOVE    9999                TO  WRK-HKNCOMBI
                   GO      TO      20011-HKNSYORI-EXT
               WHEN    "XX"
      *            公費単独
                   MOVE    SPACE               TO  WRK-HKNNUM
               WHEN    OTHER
                   EVALUATE    LF04-HKN-HKNSBTCODE (1)(1:1)
      *            自費
                   WHEN    "Z"
                       MOVE    "98"                TO  WRK-HKNNUM(1:2)
                       MOVE    LF04-HKN-HKNSBTCODE (1)(2:1)
                                                   TO  WRK-HKNNUM(3:1)
      *            治験
                   WHEN    "A"
                       MOVE    "90"                TO  WRK-HKNNUM(1:2)
                       MOVE    LF04-HKN-HKNSBTCODE (1)(2:1)
                                                   TO  WRK-HKNNUM(3:1)
      *            治験
                   WHEN    "B"
                       MOVE    "91"                TO  WRK-HKNNUM(1:2)
                       MOVE    LF04-HKN-HKNSBTCODE (1)(2:1)
                                                   TO  WRK-HKNNUM(3:1)
                   WHEN    OTHER
      *                保険
                       MOVE    ZERO                    TO  WRK-HKNNUM
                       MOVE    LF04-HKN-HKNSBTCODE(1)
                                                   TO  WRK-HKNNUM(2:2)
                   END-EVALUATE
           END-EVALUATE
      *
      *    保険組合せ検索
           INITIALIZE                  TBL-HKNCOMBI-AREA
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    WRK-PTID            TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-HKNCOMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           MOVE    ZERO                TO  IDH-MAX
           PERFORM UNTIL   (FLG-HKNCOMBI   =   1   )  OR
                           (IDH-MAX        >   20  )
               IF     (WRK-SRYYMD            >=  COMB-TEKSTYMD ) AND
                      (WRK-SRYYMD            <=  COMB-TEKEDYMD ) AND
                      (COMB-DELKBN           =   SPACE         )
                   ADD     1                 TO  IDH-MAX
                   MOVE    HKNCOMBI-REC      TO  TBL-HKNCOMBI-REC
                                                         (IDH-MAX)
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-HKNCOMB-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *TEST
      *     DISPLAY "LF04-HKNKOH-PROVIDER1:" LF04-HKNKOH-PROVIDER(1 1)
      *     DISPLAY "LF04-HKNKOH-PROVIDER2:" LF04-HKNKOH-PROVIDER(1 2)
      *     DISPLAY "LF04-HKNKOH-PROVIDER3:" LF04-HKNKOH-PROVIDER(1 3)
      *TEST
      *    公費
           INITIALIZE                  WRK-KOH-AREA
      *    公費負担名称がある場合、公費をテーブルへ
           IF    ( WRK-HKNNUM                  =   "971"  OR
                                                   "973" )
               MOVE    ZERO                TO  FLG-KOHARI
               MOVE    ZERO                TO  IDK-MAX
           ELSE
               MOVE    ZERO                TO  FLG-KOHARI
               PERFORM 200111-KOH-HENSYU-SEC
           END-IF
      *TEST
      *        DISPLAY "FLG-KOHARI:" FLG-KOHARI
      *TEST
      *
      *    保険組合せ決定
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   IDH-MAX
               IF      TBL-COMB-HKNNUM (IDH)   =   WRK-HKNNUM
                   IF      FLG-KOHARI          =   ZERO
                       MOVE    TBL-COMB-HKNCOMBINUM(IDH)
                                               TO  WRK-HKNCOMBI
                       MOVE    IDH-MAX         TO  IDH
                   ELSE
                       PERFORM 200112-KOH-CHK-SEC
                       IF      WRK-HKNCOMBI    NOT =   ZERO
                           MOVE    IDH-MAX         TO  IDH
                       END-IF
                   END-IF
               ELSE
      *            公費単独の時
                   IF      FLG-KOHARI          =   1
                       PERFORM 200112-KOH-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20011-HKNSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       200111-KOH-HENSYU-SEC                SECTION.
      *
      *    対象の公費ＩＤを決定する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *TEST
      *        DISPLAY "LF04-HKNKOH-FTNJANUM:" 
      *                      LF04-HKNKOH-FTNJANUM (1 IDX)
      *        DISPLAY "LF04-HKNKOH-JKYSNUM:" 
      *                      LF04-HKNKOH-JKYSNUM (1 IDX)
      *TEST
      *        未設定を空白へ
               IF      LF04-HKNKOH-FTNJANUM (1 IDX)(1:7) =   "mikinyu"
                   MOVE    SPACE       TO  LF04-HKNKOH-FTNJANUM(1 IDX)
               END-IF
               IF      LF04-HKNKOH-JKYSNUM (1 IDX)(1:7)  =   "mikinyu"
                   MOVE    SPACE       TO  LF04-HKNKOH-JKYSNUM (1 IDX)
               END-IF
      *        公費の有無
               IF     (LF04-HKNKOH-PROVIDER(1 IDX) =   SPACE ) AND
                      (LF04-HKNKOH-FTNJANUM(1 IDX) =   SPACE ) AND
                      (LF04-HKNKOH-JKYSNUM (1 IDX) =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-KOHARI
               END-IF
           END-PERFORM
      *    公費なし
           IF      FLG-KOHARI          =   ZERO
               GO      TO      200111-KOH-HENSYU-EXT
           END-IF
      *
      *    公費編集
           INITIALIZE                  TBL-PTKOHINF-AREA
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    WRK-PTID            TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    ZERO                TO  IDK-MAX 
           PERFORM     UNTIL   ( FLG-PTKOHINF  =   1 )  OR
                               ( IDK-MAX       >=  40)
      *        当日有効であること
               IF     (PTKOH-SAKJOKBN        =   SPACE )  AND
                      (PTKOH-TEKSTYMD        <=  WRK-SRYYMD )  AND
                      (PTKOH-TEKEDYMD        >=  WRK-SRYYMD )
                   ADD     1                   TO  IDK-MAX 
                   MOVE    PTKOHINF-REC        TO  TBL-PTKOHINF-REC
                                                            (IDK-MAX)
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    対象の公費ＩＤを決定する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        負担者番号、受給者番号がある時の判定
               IF     (LF04-HKNKOH-FTNJANUM(1 IDX) NOT =   SPACE )  OR
                      (LF04-HKNKOH-JKYSNUM (1 IDX) NOT =   SPACE )
                   MOVE    LF04-HKNKOH-FTNJANUM(1 IDX) TO  WRK-FTNJANUM
                   MOVE    LF04-HKNKOH-JKYSNUM (1 IDX) TO  WRK-JKSNUM
                   PERFORM 2001112-KOH-SET-SEC
               ELSE
      *            短縮名称のみ
                   IF      LF04-HKNKOH-PROVIDER(1 IDX) NOT =   SPACE
                       MOVE    LF04-HKNKOH-PROVIDER(1 IDX)
                                           TO  WRK-TANSEIDONAME(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200111-KOH-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費から保険組合せ決定処理
      *****************************************************************
       200112-KOH-CHK-SEC                SECTION.
      *
      *    名称がある時、名称の判定
           IF    ((LF04-HKNKOH-PROVIDER(1 1)   =   SPACE ) OR
                  (LF04-HKNKOH-PROVIDER(1 1)
                                       =   TBL-COMB-KOH-TANSEIDONAME
                                                      (IDH 1)))
             AND ((LF04-HKNKOH-PROVIDER(1 2)   =   SPACE )  OR
                  (LF04-HKNKOH-PROVIDER(1 2)
                                       =   TBL-COMB-KOH-TANSEIDONAME
                                                       (IDH 2)))
             AND ((LF04-HKNKOH-PROVIDER(1 3)   =   SPACE )  OR
                  (LF04-HKNKOH-PROVIDER(1 3)
                                       =   TBL-COMB-KOH-TANSEIDONAME
                                                        (IDH 3)))
             AND ((LF04-HKNKOH-PROVIDER(1 4)   =   SPACE )  OR
                  (LF04-HKNKOH-PROVIDER(1 4)
                                       =   TBL-COMB-KOH-TANSEIDONAME
                                                        (IDH 4)))
               CONTINUE
           ELSE
               GO      TO      200112-KOH-CHK-EXT
           END-IF
      *
           IF      (WRK-KOHID (1)      =   TBL-COMB-KOH1ID(IDH)) AND
                   (WRK-KOHID (2)      =   TBL-COMB-KOH2ID(IDH)) AND
                   (WRK-KOHID (3)      =   TBL-COMB-KOH3ID(IDH)) AND
                   (WRK-KOHID (4)      =   TBL-COMB-KOH4ID(IDH))
      *        公費対象
               MOVE    TBL-COMB-HKNCOMBINUM(IDH)
                                           TO  WRK-HKNCOMBI
           ELSE
               MOVE    ZERO                TO  FLG-HKN-CHK
               PERFORM VARYING     IDK1    FROM    1   BY  1
                       UNTIL       IDK1    >   4
                   IF      (WRK-KOHID (IDK1)   =   ZERO )  AND
                           (WRK-TANSEIDONAME(IDK1) =   SPACE )
                       CONTINUE
                   ELSE
      *                ＩＤか短縮名称が同じ時対象とする
                       IF     ((WRK-KOHID (IDK1)   NOT =   ZERO  )  AND
                               (WRK-KOHID (IDK1)   =   TBL-COMB-KOHID
                                                      (IDH IDK1))) OR
                              ((WRK-TANSEIDONAME(IDK1)
                                               NOT =   SPACE  )  AND
                               (WRK-TANSEIDONAME(IDK1)
                                       =   TBL-COMB-KOH-TANSEIDONAME
                                                       (IDH IDK1)))
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-HKN-CHK
                       END-IF
                   END-IF
               END-PERFORM
               IF      FLG-HKN-CHK         =   ZERO
      *            公費対象
                   MOVE    TBL-COMB-HKNCOMBINUM(IDH)
                                           TO  WRK-HKNCOMBI
               END-IF
           END-IF
           .
       200112-KOH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費のチェック処理
      *****************************************************************
       2001112-KOH-SET-SEC                SECTION.
      *
      *TEST
      *    DISPLAY "WRK-FTNJANUM:" WRK-FTNJANUM
      *           ",WRK-JKSNUM:" WRK-JKSNUM
      *TEST
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING      IDK    FROM    1   BY  1
                   UNTIL       (IDK    >   IDK-MAX )  OR
                               (FLG-CHK    =   1   )
               MOVE    ZERO                TO  FLG-OK1
               MOVE    ZERO                TO  FLG-OK2
      *        負担者番号
               IF      (WRK-FTNJANUM   NOT =   SPACE   )   AND
                       (WRK-FTNJANUM       =   TBL-PTKOH-FTNJANUM (IDK))
                   MOVE    1               TO  FLG-OK1
               END-IF
      *        受給者番号
               IF      (WRK-JKSNUM     NOT =   SPACE   )   AND
                       (WRK-JKSNUM         =   TBL-PTKOH-JKYSNUM (IDK))
                   MOVE    1               TO  FLG-OK2
               END-IF
               IF     (WRK-FTNJANUM        =   SPACE )  AND
                      (FLG-OK2             =   1     )
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF     (WRK-JKSNUM          =   SPACE )  AND
                      (FLG-OK1             =   1     )
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF     (WRK-FTNJANUM    NOT =   SPACE )  AND
                      (WRK-JKSNUM      NOT =   SPACE )  AND
                      (FLG-OK1             =   1     )  AND
                      (FLG-OK2             =   1     )
                   MOVE    1               TO  FLG-CHK
               END-IF
      *
               IF      FLG-CHK             =   1
                   MOVE     TBL-PTKOH-KOHID (IDK)  TO  WRK-KOHID(IDX)
               END-IF
      *
           END-PERFORM
      *TEST
      *    DISPLAY "IDX:" IDX
      *            ",WRK-KOHID:" WRK-KOHID(IDX)
      *TEST
      *
           .
       2001112-KOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤内容のチェック処理
      *****************************************************************
       2002-ZAICHK-SEC                SECTION.
      *
      *    診療種別検索
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBN
                   WHEN    LF04-SRYCD (IDX1)   =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYSYUKBN (MSYU-IDX)
                                                TO  WRK-SRYSYUKBN
           END-SEARCH 
      *
           IF      WRK-SRYSYUKBN       =   SPACE
               MOVE    LF04-SRYCD (IDX1)(1:2)  TO  WRK-SRYKBN
           END-IF
      *
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  FLG-TENTEKI
           MOVE    ZERO                TO  WRK-KASAN
           MOVE    ZERO                TO  WRK-SYUGI
           MOVE    ZERO                TO  WRK-YAKUZAI
      *    明細が１件で、加算のものは前の剤に付ける
           MOVE    ZERO                TO  WRK-CNT
      *    入院の点滴用
           MOVE    ZERO                TO  FLG-TENTEKI
           MOVE    ZERO                TO  FLG-CHUSIN
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
      **********   UNTIL       IDX2    >   20
                   UNTIL       IDX2    >   40
      *        対象外のコードをはずす
               IF      LF04-IJICD(IDX1 IDX2)   =  LOW-VALUE
                       DISPLAY "LOW-VALUE DATA IDX1 = " 
                       "[" IDX1 "]" "  IDX2 = [" IDX2 "]"
               END-IF
               IF      LF04-IJICD(IDX1 IDX2)   NOT =   SPACE
      *        IF      LF04-IJICD(IDX1 IDX2)(1:9)   NUMERIC
      ***     DISPLAY  "LF04-IJICD(IDX1 IDX2)(1:9) = "  
      ***               LF04-IJICD(IDX1 IDX2)(1:9)
                   DISPLAY "    " IDX2 " " LF04-IJICD(IDX1 IDX2)(1:9)
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    LF04-IJICD(IDX1 IDX2)(1:9)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN         =   "1"   )
                       MOVE    LF04-IJICD(IDX1 IDX2)(1:9)
                                                   TO  WRK-SRYCD
                   ELSE
                        MOVE    SNUM-OUTEDT(7:9)   TO  WRK-SRYCD
                   END-IF
                   MOVE    WRK-SRYCD           TO  LF04-IJICD(IDX1 IDX2)
                                                             (1:9)
                   PERFORM 20021-SRYCD-CHK-SEC
      *            対象外の時、コードをクリアする
                   IF      FLG-CHK             =   1
                       MOVE    SPACE           TO  LF04-IJICD(IDX1 IDX2)
                       MOVE    SPACE           TO  WRK-SRYCD
                   END-IF
               ELSE
                   MOVE    SPACE           TO  WRK-SRYCD
               END-IF
      *
               IF      WRK-SRYCD       NOT =   SPACE
      *        IF      WRK-SRYCD       NUMERIC
      *            点数マスタ検索
                   MOVE    WRK-SRYCD           TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   1
                       DISPLAY "L0031 TENSUU INV KEY:" "[" WRK-SRYCD "]"
                   END-IF
                   EVALUATE    WRK-SRYCD(1:1)
                       WHEN    "1"
                           IF      TNS-DATAKBN     =   "2"
                               ADD     1           TO  WRK-KASAN
                           ELSE
                               ADD     1           TO  WRK-SYUGI
                           END-IF
                       WHEN    "6"
                           ADD     1           TO  WRK-YAKUZAI
                           ADD     1           TO  WRK-SYUGI
                       WHEN    OTHER
                           ADD     1           TO  WRK-SYUGI
                   END-EVALUATE
      *            時間外の判定
                   IF     (LF04-SRYCD (IDX1)(1:2)  =   "11"
                                                   OR  "12"
                                                   OR  "13") AND
                           (TNS-DATAKBN     =   "2"  )
      **************  AND  (TNS-TIMEKSNKBN  =   "1" OR "2"
      **************                                OR "3"  )
      *                時間外加算区分変更
                       PERFORM 20029-JIKAN-HEN-SEC
                       IF      WRK-TIMEKBN     NOT =   SPACE
                         IF      WRK-JIKAN       =   SPACE
      ********************* MOVE    TNS-TIMEKSNKBN      TO  WRK-JIKAN
                            MOVE    WRK-TIMEKBN         TO  WRK-JIKAN
                         ELSE
                           IF      TNS-TIMEKSNKBN   =   WRK-JIKAN
                               OR  WRK-TIMEKBN      =   WRK-JIKAN
                               MOVE    SPACE        TO  LF04-IJICD
                                               (IDX1 IDX2)
                           END-IF
                         END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  FLG-ZAIEND
           IF      WRK-SYUGI           =   ZERO
               IF      WRK-KASAN       >   ZERO
      *            加算のみ１件の時、前の剤に付ける（剤終了としない）
                   MOVE    ZERO            TO  FLG-ZAIEND
      *            入院で点滴加算・入院料・食事は加算のみの剤
                   IF    (WRK-NYUGAIKBN        =   "1" )  AND
                         (WRK-SRYKBN           =   "90"  OR
                                                   "92"  OR
                                                   "97"  OR
                                                   "33" )
                       MOVE    1                   TO  FLG-ZAIEND
                   END-IF
      *            前の剤と診療区分が同じ時
                   IF     (TBL-MAX            >   ZERO )  AND
                          (WRK-SRYKBN         =   TBL-WKSRY-SRYKBN
                                                          (TBL-MAX)) AND
                         ((WRK-SRYSYUKBN      =   TBL-WKSRY-SRYSYUKBN
                                                          (TBL-MAX))  OR
                          (WRK-SRYSYUKBN      =   SPACE            ))
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-ZAIEND
                   END-IF
              END-IF
           END-IF
           .
       2002-ZAICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外加算区分変更処理
      *****************************************************************
       20029-JIKAN-HEN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-TIMEKBN
      *    ６歳未満の乳幼児加算
           EVALUATE    WRK-SRYCD
      *        夜間（時間外）加算
               WHEN    "111011570"
                   MOVE    "5"                 TO  WRK-TIMEKBN
      *        休日加算
               WHEN    "111011670"
                   MOVE    "6"                 TO  WRK-TIMEKBN
      *        深夜加算
               WHEN    "111011770"
                   MOVE    "7"                 TO  WRK-TIMEKBN
      *        再診
      *        夜間（時間外）加算
               WHEN    "112014170"
                   MOVE    "5"                 TO  WRK-TIMEKBN
      *        休日加算
               WHEN    "112014270"
                   MOVE    "6"                 TO  WRK-TIMEKBN
      *        深夜加算
               WHEN    "112014370"
                   MOVE    "7"                 TO  WRK-TIMEKBN
      *        外来診察料
      *        夜間（時間外）加算
               WHEN    "112014470"
                   MOVE    "5"                 TO  WRK-TIMEKBN
      *        休日加算
               WHEN    "112014570"
                   MOVE    "6"                 TO  WRK-TIMEKBN
      *        深夜加算
               WHEN    "112014670"
                   MOVE    "7"                 TO  WRK-TIMEKBN
      *        小児科外来診察料（初診）
      *        時間外加算
               WHEN    "113009760"
                   MOVE    "1"                 TO  WRK-TIMEKBN
      *        夜間（時間外）加算
               WHEN    "113007070"
                   MOVE    "5"                 TO  WRK-TIMEKBN
      *        休日加算
               WHEN    "113007170"
                   MOVE    "6"                 TO  WRK-TIMEKBN
      *        深夜加算
               WHEN    "113007270"
                   MOVE    "7"                 TO  WRK-TIMEKBN
      *        小児科外来診察料（再診）
      *        時間外加算
               WHEN    "113009770"
                   MOVE    "1"                 TO  WRK-TIMEKBN
      *        夜間（時間外）加算
               WHEN    "113007370"
                   MOVE    "5"                 TO  WRK-TIMEKBN
      *        休日加算
               WHEN    "113007470"
                   MOVE    "6"                 TO  WRK-TIMEKBN
      *        深夜加算
               WHEN    "113007570"
                   MOVE    "7"                 TO  WRK-TIMEKBN
      *        小児科外来診察料（外来診察料）
      *        時間外加算
               WHEN    "113009870"
                   MOVE    "1"                 TO  WRK-TIMEKBN
      *        夜間（時間外）加算
               WHEN    "113007670"
                   MOVE    "5"                 TO  WRK-TIMEKBN
      *        休日加算
               WHEN    "113007770"
                   MOVE    "6"                 TO  WRK-TIMEKBN
      *        深夜加算
               WHEN    "113007870"
                   MOVE    "7"                 TO  WRK-TIMEKBN
      *
               WHEN    OTHER
                   EVALUATE    TNS-TIMEKSNKBN
                       WHEN    "1"
                           MOVE    "1"                 TO  WRK-TIMEKBN
                       WHEN    "2"
                       WHEN    "3"
                           MOVE    "2"                 TO  WRK-TIMEKBN
                       WHEN    "4"
                           MOVE    "3"                 TO  WRK-TIMEKBN
                       WHEN    "5"
                           MOVE    "4"                 TO  WRK-TIMEKBN
                       WHEN    "6"
      *                    夜間・早朝
                           MOVE    "8"                 TO  WRK-TIMEKBN
                   END-EVALUATE
           END-EVALUATE
      *
           .
       20029-JIKAN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象のコードチェック処理
      *****************************************************************
       20021-SRYCD-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
      *    包括入力時、全部対象
           IF      WRK-HKNCOMBI        =   "9999"
               GO      TO      20021-SRYCD-CHK-EXT
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   JIDO-MAX
               IF      WRK-SRYCD       =   JIDOTBL-CODE (IDX)
                   MOVE    1               TO  FLG-CHK
                   MOVE    JIDO-MAX        TO  IDX
               END-IF
           END-PERFORM
      *    薬剤情報提供料はシステム管理設定による
           IF      FLG-YKJYO           =   ZERO
               IF      WRK-SRYCD           =   "113701310"
                                           OR  "120002370"
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
           END-IF
      *    調基はシステム管理設定による
           IF      FLG-CHOKI           =   ZERO
               IF      WRK-SRYCD           =   "120001810"
                                           OR  "120001710"
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
           END-IF
      *
      *    入院で点滴手技料は自動発生とする
           IF     (WRK-NYUGAIKBN       =   "1" )  AND
                  (WRK-SRYKBN          =   "33")
               IF      WRK-SRYCD       =   "130003810"
                                       OR  "130003710"
                                       OR  "130700310"
                                       OR  "130700410"
                                       OR  "130003710"
                   MOVE    1               TO  FLG-TENTEKI
               END-IF
      *        中心静脈注射
               IF      WRK-SRYCD       =   "130004410"
                   MOVE    1               TO  FLG-CHUSIN
               END-IF
           END-IF
      *
           .
       20021-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＴＢＬセット処理
      *****************************************************************
       2003-TBLSET-SEC                SECTION.
      *
      *    入院の点滴は手技料と薬剤は別剤とする
           IF     (WRK-NYUGAIKBN       =   "1" )  AND
                  (WRK-SRYKBN          =   "33")  AND
                  (WRK-YAKUZAI         >   ZERO)  AND
                 ((FLG-TENTEKI         =   1   ) OR
                  (FLG-CHUSIN          =   1   ) OR
                  (WRK-KASAN           >   ZERO))
               PERFORM 2004-TBL-BUNKATU-SEC
               IF      FLG-END         =   1
                   GO  TO  2003-TBLSET-EXT
               END-IF
           END-IF
      *
      *    入院の時、１明細１剤とする
           IF     (WRK-NYUGAIKBN       =   "1" )  AND
                  (WRK-SRYKBN          =   "90"
                                       OR  "92" )
               PERFORM 2005-NYUIN-BUNKATU-SEC
               GO      TO      2003-TBLSET-EXT
           END-IF
      *
           INITIALIZE                  TBL-WKSRYACT-AREA
           MOVE    ZERO                TO  TBL-MAX
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  WRK-RENNUM
      *
           PERFORM VARYING     IDX2     FROM    1   BY  1
                   UNTIL       IDX2     >   20
               IF      LF04-IJICD(IDX1 IDX2)   NOT =   SPACE
                   IF      TBL-MAX         =   ZERO
      *                ワーク診療行為初期編集
                       PERFORM 20031-WKSRYACT-HEN-SEC
                   END-IF
      *            ワーク診療行為内部編集
                   PERFORM 20032-ZAITBLSET-SEC
               END-IF
           END-PERFORM
      *
           .
       2003-TBLSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技と薬剤の別剤処理
      *****************************************************************
       2004-TBL-BUNKATU-SEC                SECTION.
      *
           INITIALIZE                  TBL-WKSRYACT-AREA
           MOVE    ZERO                TO  TBL-MAX
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  WRK-RENNUM
      *
      *    診察コードのみの剤
           PERFORM VARYING     IDX2     FROM    1   BY  1
                   UNTIL       IDX2     >   20
               IF      LF04-IJICD(IDX1 IDX2)(1:1)   =   "1"
                   IF      TBL-MAX         =   ZERO
      *                ワーク診療行為初期編集
                       PERFORM 20031-WKSRYACT-HEN-SEC
                   END-IF
      *            ワーク診療行為内部編集
                   PERFORM 20032-ZAITBLSET-SEC
      *
                   MOVE    SPACE           TO  LF04-IJICD(IDX1 IDX2)
               END-IF
           END-PERFORM
           IF      TBL-MAX         >   ZERO
                PERFORM 2004-WKSRYACT-OUT-SEC
           END-IF
      *
      *    中心静脈の場合、薬剤のみの剤は診療種別を変更する
           IF     (FLG-CHUSIN          =   1   )   AND
                  (WRK-SRYSYUKBN   NOT =   "350")
               MOVE    "350"           TO  WRK-SRYSYUKBN
           END-IF
      *
           .
       2004-TBL-BUNKATU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料を１剤１明細編集処理
      *****************************************************************
       2005-NYUIN-BUNKATU-SEC                SECTION.
      *
           PERFORM VARYING     IDX2     FROM    1   BY  1
                   UNTIL       IDX2     >   20
      *
               INITIALIZE                  TBL-WKSRYACT-AREA
               MOVE    ZERO                TO  TBL-MAX
               MOVE    ZERO                TO  IDX-S
               MOVE    ZERO                TO  WRK-RENNUM
               IF      LF04-IJICD(IDX1 IDX2)   NOT =   SPACE
      *            ワーク診療行為初期編集
                   PERFORM 20031-WKSRYACT-HEN-SEC
      *            ワーク診療行為内部編集
                   PERFORM 20032-ZAITBLSET-SEC
      *            ワーク診療行為出力
                   PERFORM 2004-WKSRYACT-OUT-SEC
      *
                   MOVE    SPACE           TO  LF04-IJICD(IDX1 IDX2)
               END-IF
           END-PERFORM
      *
           INITIALIZE                  TBL-WKSRYACT-AREA
           MOVE    ZERO                TO  TBL-MAX
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  WRK-RENNUM
      *
           .
       2005-NYUIN-BUNKATU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為初期編集処理
      *****************************************************************
       20031-WKSRYACT-HEN-SEC                SECTION.
      *
           ADD     1                   TO  TBL-MAX
           ADD     1                   TO  WRK-ZAINUM
           ADD     1                   TO  WRK-RENNUM
      *
      *    医療機関ＩＤ
           MOVE    SPA-HOSPNUM         TO  TBL-WKSRY-HOSPNUM (TBL-MAX)
      *    入外区分
      *****MOVE    "2"                 TO  TBL-WKSRY-NYUGAIKBN (TBL-MAX)
           MOVE    WRK-NYUGAIKBN       TO  TBL-WKSRY-NYUGAIKBN (TBL-MAX)
      *    患者ＩＤ
           MOVE    WRK-PTID            TO  TBL-WKSRY-PTID (TBL-MAX)
      *    診療科
           MOVE    WRK-SRYKA           TO  TBL-WKSRY-SRYKA (TBL-MAX)
      *    ドクター
           MOVE    WRK-DRCD            TO  TBL-WKSRY-DRCD  (TBL-MAX)
      *
      *    診療日付
           MOVE    WRK-SRYYMD          TO  TBL-WKSRY-SRYYMD (TBL-MAX)
      *    保険組合せ
           MOVE    WRK-HKNCOMBI        TO  TBL-WKSRY-HKNCOMBI(TBL-MAX)
      *    剤番号
           MOVE    WRK-ZAINUM          TO  TBL-WKSRY-ZAINUM(TBL-MAX)
      *    連番号
           MOVE    WRK-RENNUM          TO  TBL-WKSRY-RENNUM(TBL-MAX)
      *    診療種別
           MOVE    WRK-SRYSYUKBN       TO  TBL-WKSRY-SRYSYUKBN(TBL-MAX)
      *    診療区分
           MOVE    WRK-SRYKBN          TO  TBL-WKSRY-SRYKBN   (TBL-MAX)
      *    投薬で院外の時、院外へ
      *DD  DISPLAY "LF04-MEMO (IDX1) = " "[" LF04-MEMO (IDX1) "]"
      *
           IF     (WRK-NYUGAIKBN       =   "2"  )  AND
                  (WRK-SRYKBN          =   "21" )
               IF     (LF04-MEMO (IDX1)    =    "院外処方"  )  AND
                      (WRK-SYOHOKBN        =    "0"         )
      *            院外とする
                   MOVE    "212"           TO  TBL-WKSRY-SRYSYUKBN
                                                            (TBL-MAX)
               END-IF
               IF     (LF04-MEMO (IDX1)    =    "院内処方"  )  AND
                      (WRK-SYOHOKBN        =    "1"         )
      *            院内とする
                   MOVE    "211"           TO  TBL-WKSRY-SRYSYUKBN
                                                            (TBL-MAX)
               END-IF
           END-IF
      *    院内分は剤点数を設定する
           IF    ((WRK-NYUGAIKBN       =   "1"  )  OR
                  (WRK-SYOHOKBN        =   "0"       ))  AND
                  (WRK-SRYKBN          =   "21"  OR  "22"  OR  "23")
               IF      TBL-WKSRY-SRYSYUKBN (TBL-MAX)   =   "212"
                   CONTINUE
               ELSE
                   MOVE    1000                TO  TBL-WKSRY-ZAITENKEI
                                                              (TBL-MAX)
               END-IF
           END-IF
      *
           MOVE    IDX1                TO  TBL-IDX
      *
           .
       20031-WKSRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＴＢＬセット処理
      *****************************************************************
       20032-ZAITBLSET-SEC             SECTION.
      *
            IF     IDX-S               =   5
                MOVE    TBL-WKSRYACT-REC (TBL-MAX)
                                       TO  TBL-WKSRYACT-REC
                                                    (TBL-MAX + 1)
                ADD     1              TO  TBL-MAX
                ADD     1              TO  WRK-RENNUM
      *         連番号
                MOVE    WRK-RENNUM     TO  TBL-WKSRY-RENNUM(TBL-MAX)
                MOVE    ZERO           TO  IDX-S
                INITIALIZE             TBL-WKSRY-SINRYO-TBL-G(TBL-MAX)
           END-IF
           ADD     1                   TO  IDX-S
      *    診療コード
           INITIALIZE                      ORCSNUMAREA
           MOVE    LF04-IJICD(IDX1 IDX2)(1:9)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    LF04-IJICD(IDX1 IDX2)(1:9)
                                           TO  TBL-WKSRY-SRYCD
                                                     (TBL-MAX IDX-S)
           ELSE
               MOVE    SNUM-OUTEDT(7:9)    TO  TBL-WKSRY-SRYCD
                                                     (TBL-MAX IDX-S)
           END-IF
      *    点数マスタ検索
           MOVE    TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    入力コード
      *    診察で、手技料の時、時間外の自動加算
           IF      (WRK-JIKAN          =   "1"  OR  "2"  OR  "3"
                                       OR  "4"  OR  "5"  OR  "6"
                                       OR  "7"  OR  "8"           ) AND
                   (TBL-WKSRY-SRYKBN (TBL-MAX)
                                       =   "11"  OR  "12"  OR "13") AND
                   (TNS-DATAKBN        =   "1"   )
               MOVE    WRK-JIKAN       TO  TBL-WKSRY-INPUTCD
                                             (TBL-MAX IDX-S)(1:1)
               MOVE    LF04-IJICD(IDX1 IDX2)(1:9)
                                       TO  TBL-WKSRY-INPUTCD
                                             (TBL-MAX IDX-S)(3:)
               MOVE    SPACE           TO  WRK-JIKAN
           ELSE
               MOVE    LF04-IJICD(IDX1 IDX2)(1:9)
                                       TO  TBL-WKSRY-INPUTCD
                                             (TBL-MAX IDX-S)
           END-IF
           MOVE    ZERO                 TO  FLG-SURYO
      *    コメント値
           IF    ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:2)
                                       =   "84"   )  OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0084"  ))  AND
                  (LF04-SURYO (IDX1 IDX2)  NOT =   SPACE )
               PERFORM 200321-INPUTCHI-HEN-SEC
               MOVE    1               TO  FLG-SURYO
           END-IF
      *    画像診断のフィルム
           IF    ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                       =   "7"   )  OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                       =   "059"  ))  AND
                  (TNS-DATAKBN         =   "3"    )   AND
                  ( WRK-SRYKBN         =   "70"   )   AND
                  (LF04-SURYO (IDX1 IDX2)  NOT =   SPACE )
               PERFORM 200322-FIRUMU-HEN70-SEC
               MOVE    1               TO  FLG-SURYO
           END-IF
      *
           IF      FLG-SURYO           =   ZERO
      *        数量
               MOVE    ZERO                TO  WRK-SURYO
               INITIALIZE                      ORCSNUMAREA
               MOVE    LF04-SURYO (IDX1 IDX2)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   3     )
      *            MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    SNUM-OUTNUM         TO  WRK-SURYO
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-SURYO
               END-IF
               IF      WRK-SURYO           =   ZERO
                   MOVE    1               TO  WRK-SURYO
               END-IF
               MOVE    WRK-SURYO           TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
      *
           END-IF
      *    フリーコメントの内容
           IF     ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                       =   "810000001")  OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:2)
                                       =   "83"      )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0083"    )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0085"    )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0086"    ))  AND
                   (LF04-ALSNAME (IDX1 IDX2)
                                   NOT =   SPACE )
      *        コメント
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    LF04-ALSNAME (IDX1 IDX2)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT
                                       TO  TBL-WKSRY-INPUTCOMENT
                                                       (TBL-MAX IDX-S)
               END-IF
           END-IF
      *
      *    外用薬で薬剤の時、回数を編集
           IF      (TBL-WKSRY-SRYKBN (TBL-MAX)     =   "23" )
      ******** AND (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1) =   "6" )
      *     入院で”＊”の時はなし
               IF      (WRK-NYUGAIKBN          =   "1" )  AND
                       (LF04-KAISU (IDX1)(1:1) =   "*" )
                   MOVE    LF04-KAISU (IDX1)   TO  WRK-INPUTCD
                   PERFORM 200421-KAISU-HEN-SEC
               ELSE
      *            回数
                   MOVE    ZERO                TO  WRK-KAISU
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    LF04-KAISU (IDX1)   TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN         =   "1"   )   OR
                          (SNUM-SEISUU         >   7     )   OR
                          (SNUM-SYOSUU         >   3     )
                       MOVE    1                   TO  WRK-KAISU
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-KAISU
                   END-IF
               END-IF
               IF      WRK-KAISU           =   ZERO
                   MOVE    1                   TO  WRK-KAISU
               END-IF
      *        総量へ変更
               IF      TBL-WKSRY-SRYSURYO (TBL-MAX IDX-S)  >   ZERO
                   COMPUTE WRK-SURYO       =   TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
                                           *   WRK-KAISU
                   MOVE    WRK-SURYO       TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
               END-IF
               MOVE    WRK-KAISU       TO  TBL-WKSRY-SRYKAISU
                                                       (TBL-MAX IDX-S)
           END-IF
      *
           .
       20032-ZAITBLSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント値編集処理
      *****************************************************************
       200321-INPUTCHI-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCHI-G
           MOVE    1                   TO  IDX-I1
           MOVE    ZERO                TO  IDX-I2
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL      (IDX-I   >   12  )   OR
                              (IDX-I1  >   4   )
               IF      LF04-SURYO (IDX1 IDX2)(IDX-I:1) =   "-"
                   ADD     1               TO  IDX-I1
                   MOVE    ZERO            TO  IDX-I2
               ELSE
                   ADD     1               TO  IDX-I2
                   MOVE    LF04-SURYO (IDX1 IDX2)(IDX-I:1)
                                           TO  WRK-INPUTCHI
                                                 (IDX-I1)(IDX-I2:1)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL       IDX-I   >   4
               IF      WRK-INPUTCHI(IDX-I) NOT =   SPACE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-INPUTCHI(IDX-I)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
      *            整数のみ対象
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN         =   "1"   )   OR
                          (SNUM-SEISUU         >   12    )   OR
                          (SNUM-SYOSUU         >   5     )
                        CONTINUE
                   ELSE
      *                整数がゼロの時、ゼロ
                       IF      SNUM-SEISUU     =   ZERO
                           MOVE    "0"         TO  TBL-WKSRY-INPUTCHI
                                                   (TBL-MAX IDX-S IDX-I)
                       ELSE
                           COMPUTE IDX-I2      =   15  -   SNUM-SEISUU
                                               +   1
                           MOVE    SNUM-OUTEDT(IDX-I2:SNUM-SEISUU)
                                               TO  TBL-WKSRY-INPUTCHI
                                                  (TBL-MAX IDX-S IDX-I)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200321-INPUTCHI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画像診断のフィルム数量編集処理
      *****************************************************************
       200322-FIRUMU-HEN70-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCHI-G
           MOVE    1                   TO  IDX-I1
           MOVE    ZERO                TO  IDX-I2
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL      (IDX-I   >   12  )   OR
                              (IDX-I1  >   4   )
               IF      LF04-SURYO (IDX1 IDX2)(IDX-I:1) =   "-"
                   ADD     1               TO  IDX-I1
                   MOVE    ZERO            TO  IDX-I2
               ELSE
                   ADD     1               TO  IDX-I2
                   MOVE    LF04-SURYO (IDX1 IDX2)(IDX-I:1)
                                           TO  WRK-INPUTCHI
                                                 (IDX-I1)(IDX-I2:1)
               END-IF
           END-PERFORM
      *    数量
           MOVE    ZERO                TO  WRK-SURYO
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-INPUTCHI(1)     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   5     )
               MOVE    ZERO                TO  WRK-SURYO
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-SURYO
           END-IF
           IF      WRK-SURYO           =   ZERO
               MOVE    1               TO  WRK-SURYO
           END-IF
           MOVE    WRK-SURYO           TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
      *    分画数
           IF      WRK-INPUTCHI(2) NOT =   SPACE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-INPUTCHI(2)     TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   5     )
                   CONTINUE
               ELSE
                   MOVE    SNUM-OUTNUM         TO  TBL-WKSRY-SRYKAISU
                                                     (TBL-MAX IDX-S)
               END-IF
           END-IF
      *
           .
       200322-FIRUMU-HEN70-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＴＢＬセット処理（続きへ）
      *****************************************************************
       2005-TBLSET-NEXT-SEC                SECTION.
      *
           PERFORM VARYING     IDX2     FROM    1   BY  1
                   UNTIL       IDX2     >   20
               IF      LF04-IJICD(IDX1 IDX2)   NOT =   SPACE
      *            ワーク診療行為内部編集
                   PERFORM 20032-ZAITBLSET-SEC
               END-IF
           END-PERFORM
      *
           .
       2005-TBLSET-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為作成処理
      *****************************************************************
       2004-WKSRYACT-OUT-SEC                SECTION.
      *
      *    投薬で用法がある時、用法を追加する
      *****COMPUTE IDX3                =   IDX1    -  1
           MOVE    TBL-IDX             TO  IDX3
           IF      IDX3                =   ZERO
               COMPUTE IDX3                =   IDX1    -  1
           END-IF
      ***  DISPLAY "LF04-ADMINCODE(1) = " LF04-ADMINCODE(1)
      *    DISPLAY "LF04-ADMINCODE(2) = " LF04-ADMINCODE(2)
      *    DISPLAY "LF04-ADMINCODE(3) = " LF04-ADMINCODE(3)
      *    DISPLAY "LF04-ADMINCODE(4) = " LF04-ADMINCODE(4)
      *    DISPLAY "LF04-ADMINCODE(5) = " LF04-ADMINCODE(5)
      *    DISPLAY "LF04-ADMINCODE(6) = " LF04-ADMINCODE(6)
      *    DISPLAY "LF04-ADMINCODE(7) = " LF04-ADMINCODE(7)
      *    DISPLAY "LF04-ADMINCODE(IDX3) = " LF04-ADMINCODE(IDX3)
      ***  DISPLAY "IDX3 = " "[" IDX3 "]"
           IF     (LF04-ADMINCODE(IDX3)    NOT =   SPACE )  AND
                  (TBL-WKSRY-SRYKBN(01)    =   "21"  OR  "22" OR "23")
               PERFORM 20041-YOUHOU-SET-SEC
           END-IF
      *    入院で”＊”の時
           IF      (WRK-NYUGAIKBN          =   "1"   )  AND
                   (LF04-KAISU (IDX3)(1:1) =   "*"   )
               PERFORM 20042-IKKATU-HEN-SEC
           ELSE
      *
      *    剤回数計
               MOVE    ZERO                TO  WRK-KAISU
               INITIALIZE                      ORCSNUMAREA
               MOVE    LF04-KAISU (IDX3)   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   3     )
      **           MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    SNUM-OUTNUM         TO  WRK-KAISU
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-KAISU
               END-IF
           END-IF
      *
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *    剤回数計
           IF      TBL-WKSRY-SRYKBN(01)    =   "23" 
      *        外用薬
               MOVE    1                   TO  TBL-WKSRY-ZAIKAIKEI 
                                                     (TBL-MAX)
           ELSE
      *        外用薬以外
               MOVE    WRK-KAISU           TO  TBL-WKSRY-ZAIKAIKEI 
                                                     (TBL-MAX)
           END-IF
      *
      *    ワーク診療行為作成
           PERFORM 20042-WKSRYCD-INS-SEC
      *
           INITIALIZE                  TBL-WKSRYACT-AREA
           MOVE    ZERO                TO  TBL-MAX
           MOVE    ZERO                TO  WRK-RENNUM
           .
       2004-WKSRYACT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤用法作成処理
      *****************************************************************
       20041-YOUHOU-SET-SEC            SECTION.
      *
      *     用法コードより診療コードの確定
      *
      * 用法コード (claim:adminCode)
           IF      LF04-ADMINCODE (IDX3)(1:1)  NUMERIC
               INITIALIZE                      ORCSNUMAREA
               MOVE    LF04-ADMINCODE (IDX3)   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    LF04-ADMINCODE (IDX3)
                                               TO  WRK-SRYCD
               ELSE
                   MOVE    SNUM-OUTEDT(7:9)    TO  WRK-SRYCD
               END-IF
           ELSE
               MOVE    SPACE               TO  WRK-SRYCD
               IF      LF04-ADMINCODE (IDX3)(1:1)  NOT =   SPACE
      *            用法コードで変換
                   PERFORM VARYING     IDX-Y   FROM    1   BY  1
                           UNTIL       IDX-Y   >   55
                       IF      CLAIM006-CLAIM (IDX-Y)
                                               =   LF04-ADMINCODE
                                                              (IDX3)
                           MOVE    CLAIM006-ORCA (IDX-Y)  TO  WRK-SRYCD
                           MOVE    55                 TO  IDX-Y
                       END-IF
                   END-PERFORM
               ELSE
      *            用法名で変換
                   PERFORM VARYING     IDX-Y   FROM    1   BY  1
                           UNTIL       IDX-Y   >   55
                       IF      CLAIM006-NAME (IDX-Y)  =    LF04-ADMMEMO
                                                              (IDX3)
                           MOVE    CLAIM006-ORCA (IDX-Y)  TO  WRK-SRYCD
                           MOVE    55                 TO  IDX-Y
                       END-IF
                   END-PERFORM
               END-IF
               IF      WRK-SRYCD           =   SPACE
                   MOVE    "001000101"        TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *
            IF     IDX-S               =   5
                MOVE    TBL-WKSRYACT-REC (TBL-MAX)
                                       TO  TBL-WKSRYACT-REC
                                                    (TBL-MAX + 1)
                ADD     1              TO  TBL-MAX
                ADD     1              TO  WRK-RENNUM
      *         連番号
                MOVE    WRK-RENNUM     TO  TBL-WKSRY-RENNUM(TBL-MAX)
                MOVE    ZERO           TO  IDX-S
                INITIALIZE             TBL-WKSRY-SINRYO-TBL-G(TBL-MAX)
           END-IF
      *
           ADD      1                  TO  IDX-S
           MOVE     WRK-SRYCD          TO  TBL-WKSRY-SRYCD
                                           (TBL-MAX IDX-S)
      *    外用の時、回数を設定
           IF     (TBL-WKSRY-SRYKBN(01)    =   "23" )  AND
                  (IDX-S                   >   1    )
               MOVE    TBL-WKSRY-SRYKAISU  (TBL-MAX IDX-S - 1)
                                       TO  TBL-WKSRY-SRYKAISU
                                                       (TBL-MAX IDX-S)
           END-IF
      *
           .
      *
       20041-YOUHOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   　入院回数一括チェック処理
      *****************************************************************
       200421-KAISU-HEN-SEC            SECTION.
      *
           INITIALIZE                      ORCSNDAYCHGAREA
           MOVE    SPA-SRYYMD          TO  ORCSNDAYCHG-SRYYMD
           MOVE    WRK-INPUTCD         TO  ORCSNDAYCHG-INPUTCD
           MOVE    22                  TO  ORCSNDAYCHG-KETA
           CALL    "ORCSNDAYCHG"       USING   SPA-AREA
                                               ORCSNDAYCHGAREA
           IF      ORCSNDAYCHG-ERRCD   =   "0001"
      *        ２２文字オーバーはエラーとする
               MOVE    "*/99"              TO  WRK-INPUTCD
               MOVE    1                   TO  WRK-KAISU
           ELSE
      ******   MOVE    LF04-KAISU (IDX3)   TO  WRK-INPUTCD
               MOVE    ORCSNDAYCHG-KAISU   TO  WRK-KAISU
           END-IF
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
           .
      *
       200421-KAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院回数まとめ編集処理
      *****************************************************************
       20042-IKKATU-HEN-SEC            SECTION.
      *
            MOVE    LF04-KAISU (IDX3)   TO  WRK-INPUTCD
            PERFORM 200421-KAISU-HEN-SEC
      *
            IF     IDX-S               =   5
                MOVE    TBL-WKSRYACT-REC (TBL-MAX)
                                       TO  TBL-WKSRYACT-REC
                                                    (TBL-MAX + 1)
                ADD     1              TO  TBL-MAX
                ADD     1              TO  WRK-RENNUM
      *         連番号
                MOVE    WRK-RENNUM     TO  TBL-WKSRY-RENNUM(TBL-MAX)
                MOVE    ZERO           TO  IDX-S
                INITIALIZE             TBL-WKSRY-SINRYO-TBL-G(TBL-MAX)
           END-IF
      *
           ADD      1                  TO  IDX-S
           MOVE     WRK-INPUTCD        TO  TBL-WKSRY-INPUTCD
                                           (TBL-MAX IDX-S)
      *    剤回数計
           IF      TBL-WKSRY-SRYKBN(01)    =   "23" 
      *        外用薬
               MOVE    1                   TO  TBL-WKSRY-ZAIKAIKEI 
                                                     (TBL-MAX)
           ELSE
      *        外用薬以外
               MOVE    WRK-KAISU           TO  TBL-WKSRY-ZAIKAIKEI 
                                                     (TBL-MAX)
           END-IF
           .
      *
       20042-IKKATU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為作成処理
      *****************************************************************
       20042-WKSRYCD-INS-SEC            SECTION.
      *
           PERFORM VARYING     IDX      FROM    1   BY  1
                   UNTIL       IDX      >   TBL-MAX
               MOVE    TBL-WKSRYACT-REC (IDX)
                                           TO  WKSRYACT-REC
      *
               MOVE    SMCNDATE-YMD        TO  WKSRY-CREYMD
               MOVE    SMCNDATE-YMD        TO  WKSRY-UPYMD
               MOVE    SMCNDATE-HMS        TO  WKSRY-UPHMS
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   ADD     1                   TO  CNT-OUT01
                   CONTINUE
               ELSE
                   DISPLAY "L0031 WKSRYACT INS ERR:"  MCP-RC
                           ",KEY:"   WKSRY-KEY  "!!!"
                   MOVE    1                   TO  FLG-END
                   MOVE    10                  TO  LNK-ERR-CODE
                   MOVE    "中途終了データ登録エラー"
                                               TO  LNK-ERR-MSG
                   MOVE    TBL-MAX             TO  IDX 
               END-IF
           END-PERFORM
      *
           .
       20042-WKSRYCD-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為から剤番号検索処理
      *****************************************************************
       2009-WKSRYACT-CHK-SEC                 SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    WRK-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    ZERO                TO  WRK-ZAINUM
           PERFORM
               UNTIL   FLG-WKSRYACT   =   1
               IF      WKSRY-ZAINUM        >   WRK-ZAINUM
                   MOVE    WKSRY-ZAINUM        TO  WRK-ZAINUM
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2009-WKSRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為の登録済みチェック処理
      *****************************************************************
       20010-WKSRYACT-CHK2-SEC                 SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    WRK-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-WKSRYACT        =   ZERO
               IF     (WKSRY-MOD-FLG       =   ZERO )
                   IF      SYS-9000-RESEND-FLG =   "1"
                       IF      WKSRY-KARTE-FLG     =   1
      *                    中途終了データは置換えなし
                           MOVE    1               TO  FLG-ERR
                           DISPLAY "L0031 WKSRYACT K02 INS"
                                  ",HOSPNUM:"   SPA-HOSPNUM
                                  ",NYUGAIKBN:" WRK-NYUGAIKBN
                                  ",PTID:" WRK-PTID
                                  ",SRYKA:" WRK-SRYKA
                                  ",SRYYMD:" WRK-SRYYMD
                                  ",HKNCOMBI:" WRK-HKNCOMBI
                         IF  LNK-KBN-CODE  = "1"
                           MOVE  01        TO  LNK-ERR-CODE
                           MOVE
                       "内容が変更されているため置換できませんでした"
                                           TO  LNK-ERR-MSG
                         END-IF
                       ELSE
      *                    置換えの為、データ削除
                           PERFORM 200101-WKSRYACT-DEL-SEC
      *
                           DISPLAY "L0031 WKSRYACT UPDATE "
                                  ",HOSPNUM:"   SPA-HOSPNUM
                                  ",NYUGAIKBN:" WRK-NYUGAIKBN
                                  ",PTID:" WRK-PTID
                                  ",SRYKA:" WRK-SRYKA
                                  ",SRYYMD:" WRK-SRYYMD
                                  ",HKNCOMBI:" WRK-HKNCOMBI
                         IF  LNK-KBN-CODE  = "1"
                           MOVE  02        TO  LNK-ERR-CODE
                           MOVE
                       "内容を置き換えました"
                                           TO  LNK-ERR-MSG
                         END-IF
                       END-IF
                   ELSE
                       MOVE    1               TO  FLG-ERR
                       DISPLAY "L0031 WKSRYACT SELECT ERR "
                              ",HOSPNUM:"   SPA-HOSPNUM
                              ",NYUGAIKBN:" WRK-NYUGAIKBN
                              ",PTID:" WRK-PTID
                              ",SRYKA:" WRK-SRYKA
                              ",SRYYMD:" WRK-SRYYMD
                              ",HKNCOMBI:" WRK-HKNCOMBI
                              ",WKSRY-KARTE-FLG:" WKSRY-KARTE-FLG
                         IF  LNK-KBN-CODE  = "1"
                           MOVE  03        TO  LNK-ERR-CODE
                           MOVE
                       "既に同日の診療データが登録されています"
                                           TO  LNK-ERR-MSG
                         END-IF
                   END-IF
               ELSE
      *            展開中エラー
                   DISPLAY "L0031 WKSRYACT MOD ERR "
                              ",HOSPNUM:"   SPA-HOSPNUM
                              ",NYUGAIKBN:" WRK-NYUGAIKBN
                              ",PTID:" WRK-PTID
                              ",SRYKA:" WRK-SRYKA
                              ",SRYYMD:" WRK-SRYYMD
                              ",HKNCOMBI:" WRK-HKNCOMBI
                              ",WKSRY-MOD-FLG," WKSRY-MOD-FLG
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       20010-WKSRYACT-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為削除処理
      *****************************************************************
       200101-WKSRYACT-DEL-SEC          SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    WRK-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       200101-WKSRYACT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名セット処理
      *****************************************************************
       2006-BYOMEI-SET-SEC                 SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
      *
               INITIALIZE                  WRK-BYOMEI-TBL
                                           FLG-BYOMEI-CHK
                                           WRK-TOKSKNCD
                                           WRK-BYOMEI
      *
               IF    ( LF04-SNDSPITBL-SPICOD2 (IDX 1)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 1)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 2)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 2)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 3)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 3)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 4)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 4)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 5)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 5)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 6)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 6)  =   SPACE )
                   IF    ( LF04-SND-SPICOD1 (IDX)  =   SPACE )
                   AND   ( LF04-SND-SPINAME1(IDX)  =   SPACE )
                       CONTINUE
                   ELSE  
      *                連結式病名編集
                       PERFORM 20061-SPICOD1-HENSYU-SEC
                   END-IF
               ELSE
      *            単独式病名編集
                   PERFORM 20061-SPICOD2-HENSYU-SEC
               END-IF
           END-PERFORM    
           .
       2006-BYOMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    連結式病名編集処理
      *****************************************************************
       20061-SPICOD1-HENSYU-SEC                 SECTION.
      *
      *    疾患コードが設定されていないとき
           IF         LF04-SND-SPICOD1 (IDX)  =   SPACE
               MOVE    1                      TO  FLG-BYOMEI-CHK
           ELSE  
      *        疾患コードを分割する
               UNSTRING   LF04-SND-SPICOD1 (IDX)  DELIMITED  BY  "."
                                           INTO   WRK-TBYOMEICD (1)
                                                  WRK-TBYOMEICD (2)
                                                  WRK-TBYOMEICD (3)
                                                  WRK-TBYOMEICD (4) 
                                                  WRK-TBYOMEICD (5)
                                                  WRK-TBYOMEICD (6)
                                                  WRK-TBYOMEICD (7)
               END-UNSTRING
           END-IF
      *
      *    ７個以上のコードが設定されているとき
           IF      WRK-TBYOMEICD (7)   NOT =   SPACE
               MOVE    1                   TO  FLG-BYOMEI-CHK
           END-IF
      *
      *    病名コードチェック
           PERFORM 200611-BYOMEICD-CHK-SEC
      *
      *    病名コードがエラー又は未コード化傷病名コードのときは疾患名を編集
           IF      FLG-BYOMEI-CHK      =   1   OR  5
               MOVE    LF04-SND-SPINAME1 (IDX) TO  WRK-BYOMEI
           END-IF 
      *
      *    患者病名マスタ作成
           PERFORM 200612-BYOMEI-HEN-SEC
           .
       20061-SPICOD1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    単独式病名編集処理
      *****************************************************************
       20061-SPICOD2-HENSYU-SEC                 SECTION.
      *
      *    病名のみ設定のときコードチェック
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       6
                   OR      LF04-SNDSPITBL-SPINAME2(IDX IDY) 
                                               =   SPACE
                   OR      FLG-BYOMEI-CHK      =   1 
               IF      LF04-SNDSPITBL-SPICOD2 (IDX IDY)
                                           =   SPACE
                   MOVE    1                   TO  FLG-BYOMEI-CHK 
               ELSE
                   MOVE    LF04-SNDSPITBL-SPICOD2 (IDX IDY)
                                               TO  WRK-TBYOMEICD (IDY)
               END-IF
           END-PERFORM
      *     
      *    病名コードチェック
           PERFORM 200611-BYOMEICD-CHK-SEC
      *
      *    病名コードがエラー又は未コード化傷病名コードのときは疾患名を編集
           IF      FLG-BYOMEI-CHK      =   1   OR  5
               STRING  LF04-SNDSPITBL-SPINAME2(IDX 1) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 2) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 3) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 4) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 5) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 6) DELIMITED BY SPACE
                                               INTO    WRK-BYOMEI
               END-STRING
           END-IF
      *
      *    患者病名マスタ作成
           PERFORM 200612-BYOMEI-HEN-SEC
           .
       20061-SPICOD2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       200611-BYOMEICD-CHK-SEC             SECTION.
      *
           IF      FLG-BYOMEI-CHK      =   1
               GO  TO  200611-BYOMEICD-CHK-EXT
           END-IF 
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       6
                   OR      WRK-TBYOMEICD (IDY) =   SPACE
                   OR      FLG-BYOMEI-CHK      =   1
      *
      *        病名コードの長さチェック
               MOVE    ZERO                TO  WRK-LENGTH 
               PERFORM VARYING IDZ FROM    10  BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TBYOMEICD (IDY)(IDZ:1)
                                           NOT =   SPACE
                       MOVE    IDZ             TO  WRK-LENGTH
                       MOVE    1               TO  IDZ
                   END-IF
               END-PERFORM
               EVALUATE    WRK-LENGTH
                   WHEN    4
                       MOVE    SPACE       TO  WRK-BYOMEICD
                       MOVE    "ZZZ"       TO  WRK-BYOMEICD (1:3)
                       MOVE    WRK-TBYOMEICD (IDY)(1:4)
                                           TO  WRK-BYOMEICD (4:4)
                       MOVE    WRK-BYOMEICD
                                           TO  WRK-TBYOMEICD (IDY)
                   WHEN    7
                       CONTINUE
                   WHEN    OTHER            
                       MOVE    1                   TO  FLG-BYOMEI-CHK
               END-EVALUATE
      *
      *        病名マスタ存在チェック
               IF      FLG-BYOMEI-CHK          =   ZERO
                   IF      WRK-TBYOMEICD (IDY)     =   WRK-CONS-BYOMEICD
                       MOVE    5               TO  FLG-BYOMEI-CHK
                   ELSE
                       INITIALIZE                      BYOMEI-REC
                       MOVE    WRK-TBYOMEICD (IDY) TO  BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      FLG-BYOMEI          =   1  
                           MOVE    1               TO  FLG-BYOMEI-CHK
                       ELSE
                           MOVE    BYO-BYOMEI      TO
                                                   WRK-TBYOMEI    (IDY)
                           MOVE    BYO-TANDOKUKBN  TO
                                                   WRK-TTANDOKUKBN(IDY)
                           MOVE    BYO-UTAGAIFLG   TO
                                                   WRK-TUTAGAIFLG (IDY)
                           IF      WRK-TBYOMEICD (IDY) (1:1)
                                               NOT =   "Z"
                               MOVE    BYO-TOKSKNCD    TO  WRK-TOKSKNCD
                           END-IF
                       END-IF      
                   END-IF
               END-IF
           END-PERFORM     
      * 
      *    接尾語・接頭語チェック
           IF      FLG-BYOMEI-CHK       =   ZERO
               MOVE    SPACE                   TO  WRK-RECECD-TBL
      * 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
                       OR      FLG-BYOMEI-CHK      =   3
                   EVALUATE    WRK-TBYOMEICD (IDY)(1:3)
                                   ALSO    WRK-TBYOMEICD (IDY)(4:1)
                       WHEN    "ZZZ"         ALSO    "1"  THRU    "7"
                           IF    ( WRK-RECECD (2)    NOT =   SPACE )
                           OR    ( WRK-RECECD (3)    NOT =   SPACE ) 
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (1)
                           END-IF            
                       WHEN    "ZZZ"       ALSO    "8"  
                           IF      WRK-RECECD (2)    =   SPACE 
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (3)
                           END-IF
                       WHEN    OTHER                 
                           IF    ( WRK-RECECD (3)    NOT =   SPACE ) 
                           OR    ( WRK-RECECD (2)    NOT =   SPACE )
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (2)
                           END-IF    
                   END-EVALUATE
               END-PERFORM
               IF    ( FLG-BYOMEI-CHK      =   ZERO  )
               AND   ( WRK-RECECD (2)      =   SPACE ) 
                   MOVE    4                   TO  FLG-BYOMEI-CHK
                END-IF
           END-IF 
      *
      *    単独使用禁止の病名チェック
           IF      FLG-BYOMEI-CHK      =   ZERO
               MOVE    ZERO                TO  WRK-TANDOKUKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
                   IF      WRK-TTANDOKUKBN (IDY)   =   ZERO
                       MOVE    1                   TO  WRK-TANDOKUKBN
                   END-IF
               END-PERFORM
      *
               IF      WRK-TANDOKUKBN      =   ZERO
                   MOVE    2                   TO  FLG-BYOMEI-CHK
               END-IF
           END-IF
      *           
      *    マスタから病名を組み立てる
           MOVE    SPACE                   TO  WRK-BYOMEI
           IF      FLG-BYOMEI-CHK          =   2
               STRING  "＊"                DELIMITED   BY  SIZE
                       WRK-TBYOMEI (1)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (2)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (3)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (4)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (5)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (6)     DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
               END-STRING
               INITIALIZE                      WRK-TBYOMEICD (1)
                                               WRK-TBYOMEICD (2)
                                               WRK-TBYOMEICD (3)
                                               WRK-TBYOMEICD (4) 
                                               WRK-TBYOMEICD (5)
                                               WRK-TBYOMEICD (6)
                                               WRK-TBYOMEICD (7)
               MOVE    WRK-CONS-BYOMEICD   TO  WRK-TBYOMEICD (1) 
           ELSE
               STRING  WRK-TBYOMEI (1)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (2)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (3)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (4)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (5)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (6)     DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
               END-STRING
           END-IF
           .
       200611-BYOMEICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ作成処理
      *****************************************************************
       200612-BYOMEI-HEN-SEC                 SECTION.
      *
      *    病名全角チェック
           IF      WRK-BYOMEI      NOT =   SPACE    
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI 
               MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-BYOMEI
               ELSE
                   MOVE    "？？？？？"        TO  WRK-BYOMEI
               END-IF
           END-IF
      *
      *    疾患開始日
           MOVE    LF04-SND-SKNSTARTDATE(IDX)(1:4)
                                       TO  WRK-BYOYY
           MOVE    LF04-SND-SKNSTARTDATE(IDX)(6:2)
                                       TO  WRK-BYOMM
           MOVE    LF04-SND-SKNSTARTDATE(IDX)(9:2)
                                       TO  WRK-BYODD
      *    疾患終了日
           MOVE    LF04-SND-SKNENDDATE(IDX)(1:4)
                                       TO  WRK-TENKIYY
           MOVE    LF04-SND-SKNENDDATE(IDX)(6:2)
                                       TO  WRK-TENKIMM
           MOVE    LF04-SND-SKNENDDATE(IDX)(9:2)
                                       TO  WRK-TENKIDD
      *    転帰
           MOVE    SPACE               TO  WRK-TENKI
           IF      LF04-SND-SKNENDDATE(IDX)    NOT =   SPACE
      *        転帰が空白の時、治癒
               IF      LF04-SND-TENKI  (IDX)       =   SPACE 
                   MOVE    "1"              TO  WRK-TENKI
               ELSE
                   PERFORM 2006121-TENKSET-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-SYUBYOFLG
                                           WRK-UTAGAIFLG
      *    疑いフラグ・主病名フラグ
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
               IF    ( LF04-SNDBNRTBL-BNRTBL1 (IDX IDY)
                                       =   "MML0012"       )
               AND   ( LF04-SNDBNRTBL-BNRNAME1 (IDX IDY)
                                       =   "mainDiagnosis" )
                   MOVE    "1"             TO WRK-SYUBYOFLG
               END-IF
               IF    ( LF04-SNDBNRTBL-BNRTBL1 (IDX IDY)
                                       =   "MML0015"            )
               AND   ( LF04-SNDBNRTBL-BNRNAME1 (IDX IDY)
                                       =   "suspectedDiagnosis" )
                   MOVE    "1"             TO  WRK-UTAGAIFLG
               END-IF
           END-PERFORM
      *
      *    更新処理
           INITIALIZE                  FLG-PTBYOMEI-UMU
                                       WRK-PTBYOMEI-REC
                                       WRK-PTBYOMEI-SRYKA-REC
                                       WRK-PTBYOMEI-DOUKI-REC
                                       WRK-UTAGAIFLG-KETASU  
      *
      *    「の疑い」を検索
           PERFORM VARYING IDY FROM    1  BY  2
                   UNTIL   IDY >       75
               IF      WRK-BYOMEI1 (IDY:6)  =   "の疑い"
                   MOVE    IDY              TO  WRK-UTAGAIFLG-KETASU
                   MOVE    80               TO  IDY
               END-IF
           END-PERFORM
      *
      *    患者病名マスタ検索（同一開始日・病名）
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           IF      WRK-UTAGAIFLG-KETASU    =   ZERO
               MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
               STRING  WRK-BYOMEI1 (1:80)  DELIMITED BY  SPACE
                       "の疑い"            DELIMITED BY  SIZE  
                                           INTO    PTBYO-XXBYOMEI
               END-STRING
           ELSE
               IF      WRK-UTAGAIFLG-KETASU    =   1
                   MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                                   PTBYO-XXBYOMEI
               ELSE
                   MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                   STRING  WRK-BYOMEI1 (1:WRK-UTAGAIFLG-KETASU - 1)
                                               DELIMITED BY  SIZE
                           WRK-BYOMEI1 (WRK-UTAGAIFLG-KETASU + 6:)
                                               DELIMITED BY  SPACE
                                               INTO    PTBYO-XXBYOMEI
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           PERFORM             UNTIL   FLG-PTBYOMEI        =   1
      *        最小診療科の病名情報を取得
               IF      WRK-PTBYO-SRYKA     =   SPACE
                   MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-REC
               END-IF
      *        最小診療科で同一病名があれば病名情報を取得
               IF    ( PTBYO-SRYKA         =   WRK-PTBYO-SRYKA    )
               AND   ( PTBYO-BYOMEI        =   WRK-BYOMEI1 (1:80) )
                   MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-REC
               END-IF
      *        同一診療科の病名情報を取得
               IF      PTBYO-SRYKA         =   WRK-SRYKA
                   IF      FLG-PTBYOMEI-UMU    >   2
                       CONTINUE
                   ELSE 
                       MOVE    PTBYOMEI-REC    TO
                                               WRK-PTBYOMEI-SRYKA-REC
                       IF      FLG-PTBYOMEI-UMU    =   ZERO
                           MOVE    1               TO  FLG-PTBYOMEI-UMU
                       END-IF
                       IF      PTBYO-BYOMEI        =   WRK-BYOMEI1(1:80)
                           IF      FLG-PTBYOMEI-UMU    =   2
                               MOVE    4           TO  FLG-PTBYOMEI-UMU
                           ELSE
                               MOVE    3           TO  FLG-PTBYOMEI-UMU
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   MOVE    2                   TO  FLG-PTBYOMEI-UMU
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTBYOMEI-UMU     =   ZERO
      *        同一開始日・病名で存在しないとき
               PERFORM 2006123-KOUSIN-01-SEC
           ELSE
               IF    ( SYS-9000-BYOMEI-SYU-FLG
                                            =   SPACE )
               AND   ( SYS-9000-BYOMEI-DOUKI-FLG
                                            =   SPACE )
      *            同期・集約フラグの設定がないとき
                   IF      WRK-PTBYO-SRYKA-RENNUM   =   ZERO
                       PERFORM 2006123-KOUSIN-01-SEC
                   ELSE
                       MOVE    WRK-PTBYOMEI-SRYKA-REC   TO  PTBYOMEI-REC
                       PERFORM 2006124-KOUSIN-02-SEC
                   END-IF
               ELSE
      *            集約フラグ
                   IF      SYS-9000-BYOMEI-SYU-FLG
                                               =   SPACE
                       IF      WRK-PTBYO-SRYKA-RENNUM  =   ZERO
                           PERFORM 2006123-KOUSIN-01-SEC
                       END-IF
                   ELSE
                       IF      WRK-PTBYO-SRYKA-SRYKA   =   SPACE
                           MOVE    WRK-PTBYOMEI-REC    TO  PTBYOMEI-REC
                       ELSE
                           MOVE    WRK-PTBYOMEI-SRYKA-REC
                                                       TO  PTBYOMEI-REC
                       END-IF
                       PERFORM 2006124-KOUSIN-02-SEC
                   END-IF
      *            同期フラグ
                   IF      SYS-9000-BYOMEI-DOUKI-FLG
                                               =   "1"
                       IF      FLG-PTBYOMEI-UMU    =   2
                           PERFORM 2006125-KOUSIN-03-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       200612-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰編集処理
      *****************************************************************
       2006121-TENKSET-SEC                 SECTION.
      *
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   14
      *        転帰が空白の時、治癒
               IF      LF04-SND-TENKI (IDX)    =   MML0016-TENKI (IDX-T)
                   MOVE    MML0016-ORCA-TENKI (IDX-T)
                                               TO  WRK-TENKI
                   MOVE    14                  TO  IDX-T
               END-IF
           END-PERFORM
      *
      *    該当の転帰が無い場合は治癒にする
           IF      WRK-TENKI           =   SPACE
               MOVE    "1"              TO  WRK-TENKI
           END-IF
      *
           .
       2006121-TENKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ追加処理
      *****************************************************************
       2006123-KOUSIN-01-SEC           SECTION.
      *
      *    連番取得
           MOVE    ZERO                TO  WRK-PTBYO-RENNUM
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "key7"              TO  WRK-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               MOVE    PTBYO-RENNUM    TO  WRK-PTBYO-RENNUM
           END-IF
           ADD     1                   TO  WRK-PTBYO-RENNUM
      *    病名追加
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-PTBYO-RENNUM    TO  PTBYO-RENNUM
           PERFORM 20061231-PTBYOMEI-HENSYU-SEC
           IF      WRK-TENKIYMD    NOT =   SPACE
      *        転帰日
               MOVE    WRK-TENKIYMD        TO  PTBYO-TENKIYMD
      *        転帰区分
               MOVE    WRK-TENKI           TO  PTBYO-TENKIKBN
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               DISPLAY "INS PTBYOMEI-KEY =>" PTBYO-KEY  "."
               MOVE    11                  TO  LNK-ERR-CODE
               MOVE    "患者病名登録エラー"
                                           TO  LNK-ERR-MSG 
           ELSE
               ADD     1                   TO  CNT-OUT02
           END-IF
           .
       2006123-KOUSIN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       20061231-PTBYOMEI-HENSYU-SEC                 SECTION.
      *
      *    疑いフラグ・主病名フラグ
           MOVE    WRK-UTAGAIFLG   TO  PTBYO-UTAGAIFLG
           MOVE    WRK-SYUBYOFLG   TO  PTBYO-SYUBYOFLG
      *
      *    疾患コードがエラー又は未コード化傷病名コード以外のとき
           IF      FLG-BYOMEI-CHK      NOT =   1   AND 5
               MOVE    ZERO                TO  IDZ
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
      *           病名コード
                   MOVE    WRK-TBYOMEICD (IDY) 
                                           TO  PTBYO-BYOMEICD (IDY)
      *            疑いフラグ（病名コードより）
                   IF      PTBYO-UTAGAIFLG     =   "3"
                       CONTINUE
                   ELSE
                       EVALUATE    PTBYO-UTAGAIFLG ALSO
                                               WRK-TUTAGAIFLG (IDY)
                           WHEN    SPACE           ALSO    "1"
                           WHEN    SPACE           ALSO    "2"
                               MOVE    WRK-TUTAGAIFLG (IDY)
                                                   TO  PTBYO-UTAGAIFLG
                           WHEN    "2"             ALSO    "1"
                           WHEN    "1"             ALSO    "2"
                               MOVE    "3"         TO  PTBYO-UTAGAIFLG
                       END-EVALUATE
                   END-IF      
      *
      *************IF      FLG-BYOMEI-CHK      =   ZERO
      *                病名コード数
                       ADD     1                   TO  PTBYO-BYOMEICDSU
      *                基本病名コード・基本部位コード
                       IF      PTBYO-BYOMEICD (IDY)(1:1)
                                               NOT =   "Z" 
                           MOVE    PTBYO-BYOMEICD (IDY)
                                               TO  PTBYO-KHNBYOMEICD
                       ELSE
                           IF      PTBYO-BYOMEICD (IDY) (1:4)
                                                   =   "ZZZ1"
                               ADD     1       TO  IDZ
                               IF      IDZ         <   4
                                   MOVE    PTBYO-BYOMEICD (IDY)
                                               TO  PTBYO-KHNBUICD(IDZ)
                               END-IF
                           END-IF
                       END-IF
      *************END-IF  
               END-PERFORM
      *
      *        病名
      *****    MOVE    LF04-SND-SPINAME1 (IDX)    TO  PTBYO-BYOMEI
               MOVE    WRK-BYOMEI1 (1:80)         TO  PTBYO-BYOMEI
      *
      *        病名文字数
               PERFORM VARYING     IDY     FROM    80 BY  -1
                       UNTIL       IDY     <   1
                   IF      PTBYO-BYOMEI(IDY:1)     NOT =   SPACE
                       COMPUTE PTBYO-BYOMEIMOJI    =   IDY /   2
                       MOVE    1                   TO  IDY
                   END-IF
               END-PERFORM
      *
      *        慢性区分
               MOVE    WRK-TOKSKNCD        TO  PTBYO-MANSEIKBN
      *
      *        病名編集フラグ（コードがエラーのとき、病名が長いとき）
               IF    ( FLG-BYOMEI-CHK  NOT =   ZERO  )
               OR    ( WRK-BYOMEI2     NOT =   SPACE )    
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               END-IF
      *
      *        レセ電エラーフラグ（編集病名のとき）
               IF      PTBYO-BYOMEIHENFLG          =   "1"
                   EVALUATE    PTBYO-SYUBYOFLG       
                       WHEN    "1"
                           IF      PTBYO-BYOMEIMOJI        >   17
                               MOVE    "1" TO  PTBYO-RECEDENFLG       
                           END-IF
                       WHEN    SPACE
                           IF      PTBYO-BYOMEIMOJI        >   20
                               MOVE    "1" TO  PTBYO-RECEDENFLG       
                           END-IF
                   END-EVALUATE
               END-IF
      *
      *        レセ電エラーフラグ（疑いフラグが設定されているとき）
               IF    ( PTBYO-RECEDENFLG        =   SPACE ) 
               AND   ( PTBYO-UTAGAIFLG     NOT =   SPACE )
                   INITIALIZE                      ORCSMKBYOMEIAREA
                   MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
                   MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
                   MOVE    PTBYO-BYOMEIHENFLG  TO  SMKBYO-BYOMEIHENFLG
                   MOVE    PTBYO-BYOMEICD-G    TO  SMKBYO-BYOMEICD-G
                   CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
                   MOVE    SMKBYO-BYOMEIHENFLG TO  PTBYO-BYOMEIHENFLG
      *
                   COMPUTE WRK-MOJISU      =   SMKBYO-LENGTH  /   2
                   IF      PTBYO-BYOMEIHENFLG    =   "1"
                       EVALUATE    PTBYO-SYUBYOFLG
                           WHEN    "1"
                               IF      WRK-MOJISU  >   17
                                   MOVE    "1"     TO  PTBYO-RECEDENFLG
                               END-IF
                           WHEN    SPACE
                               IF      WRK-MOJISU  >   20
                                   MOVE    "1"     TO  PTBYO-RECEDENFLG
                               END-IF
                       END-EVALUATE
                   END-IF    
               END-IF    
           ELSE
      *        疾患コードがエラー又は未コード化傷病名コードのとき
      *        病名からコード検索
               INITIALIZE                  ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    PTBYO-SRYYMD        TO  LNK-BYO-SRYYMD-IN
               MOVE    PTBYO-UTAGAIFLG     TO  LNK-BYO-UTAGAIFLG-IN
               MOVE    WRK-BYOMEI1         TO  LNK-BYO-BYOMEI
               MOVE    PTBYO-SYUBYOFLG     TO  LNK-BYO-SYUBYOFLG 
               CALL    "ORCSBYOMEI"        USING    ORCSBYOMEIAREA
                                                    SPA-AREA
      *  
      *        疑いフラグ
               IF      PTBYO-UTAGAIFLG     =   "3"
                   CONTINUE
               ELSE
                   EVALUATE    PTBYO-UTAGAIFLG ALSO
                                           LNK-BYO-UTAGAIFLG
                       WHEN    SPACE           ALSO    "1"
                       WHEN    SPACE           ALSO    "2"
                           MOVE    LNK-BYO-UTAGAIFLG
                                                   TO  PTBYO-UTAGAIFLG
                       WHEN    "2"             ALSO    "1"
                       WHEN    "1"             ALSO    "2"
                           MOVE    "3"         TO  PTBYO-UTAGAIFLG
                   END-EVALUATE
               END-IF      
      *        慢性区分
               IF      LNK-BYO-MANSEIKBN   NOT =   ZERO
                   MOVE    LNK-BYO-MANSEIKBN   TO  PTBYO-MANSEIKBN
               END-IF
      *       病名コード
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL   IDX4    >       LNK-BYO-BYOMEI-MAX
                   MOVE    LNK-BYO-TBYOMEICD (IDX4)
                                       TO  PTBYO-BYOMEICD  (IDX4)
               END-PERFORM
      *        病名コード数
               MOVE    LNK-BYO-BYOMEICDSU  TO  PTBYO-BYOMEICDSU
      *        基本病名コード
               MOVE    LNK-BYO-KHNBYOMEICD TO  PTBYO-KHNBYOMEICD
      *        基本部位コード
               MOVE    LNK-BYO-KHNBUICD (1)
                                           TO  PTBYO-KHNBUICD (1)
               MOVE    LNK-BYO-KHNBUICD (2)
                                           TO  PTBYO-KHNBUICD (2)
               MOVE    LNK-BYO-KHNBUICD (3)
                                           TO  PTBYO-KHNBUICD (3)
      *        レセ電エラーフラグ
               MOVE    LNK-BYO-RECEDENFLG  TO  PTBYO-RECEDENFLG
      *        病名文字数
               MOVE    LNK-BYO-BYOMEIMOJI  TO  PTBYO-BYOMEIMOJI
      *        病名
      *********IF    ( LNK-BYO-BYOMEICD-ERR    =   1  )
      *********OR    ( LNK-BYO-TANDOKUKBN-ERR  =   1  )
      *********単独病名・修飾語コードのみのとき
               IF      LNK-BYO-TANDOKUKBN-ERR  =   1
      *        単独病名のみのとき
                   STRING  "＊"                DELIMITED   BY  SIZE
                           LNK-BYO-BYOMEI      DELIMITED   BY  SPACE
                                               INTO    PTBYO-BYOMEI
                   END-STRING
                   COMPUTE PTBYO-BYOMEIMOJI    =   PTBYO-BYOMEIMOJI
                                               +   2
               ELSE
                   MOVE    LNK-BYO-BYOMEI      TO  PTBYO-BYOMEI
               END-IF
      *        病名編集フラグ
               IF    ( LNK-BYO-BYOMEICD-ERR    =   1  )
               OR    ( LNK-BYO-TANDOKUKBN-ERR  =   1  )
               OR    ( LNK-BYO-BYOMEIMOJI      >   80 )
      *            単独病名・修飾語コードのみ・病名が長いとき
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   MOVE    LNK-BYO-BYOMEIHENFLG
                                               TO  PTBYO-BYOMEIHENFLG
               END-IF
           END-IF
      *
           .
       20061231-PTBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ更新処理
      *****************************************************************
       2006124-KOUSIN-02-SEC           SECTION.
      *
      *    転帰日
           MOVE    WRK-TENKIYMD        TO  PTBYO-TENKIYMD
      *    転帰区分
           MOVE    WRK-TENKI           TO  PTBYO-TENKIKBN
      *
      *    主病名フラグ
           MOVE    WRK-SYUBYOFLG       TO  PTBYO-SYUBYOFLG
      *
      *    疑いフラグ
           EVALUATE    PTBYO-UTAGAIFLG ALSO    WRK-UTAGAIFLG
               WHEN    SPACE           ALSO    "1"
               WHEN    "1"             ALSO    "1"
                   MOVE    "1"             TO  PTBYO-UTAGAIFLG
               WHEN    "2"             ALSO    "1"
               WHEN    "3"             ALSO    "1"
                   MOVE    "3"             TO  PTBYO-UTAGAIFLG
               WHEN    SPACE           ALSO    SPACE
               WHEN    "1"             ALSO    SPACE
                   MOVE    SPACE           TO  PTBYO-UTAGAIFLG
               WHEN    "2"             ALSO    SPACE
               WHEN    "3"             ALSO    SPACE
                   MOVE    "2"             TO  PTBYO-UTAGAIFLG
           END-EVALUATE
      *
      *    病名
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
      *
           INITIALIZE                      PTBYO-BYOMEICD-G
                                           PTBYO-KHNBUICD-G
      *
      *    疾患コードがエラー又は未コード化傷病名コード以外のとき
           IF      FLG-BYOMEI-CHK      NOT =   1   AND 5
               MOVE    ZERO                TO  IDZ
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
      *            病名コード
                   MOVE    WRK-TBYOMEICD (IDY) 
                                           TO  PTBYO-BYOMEICD (IDY)
      *            疑いフラグ（病名コードより）
                   IF      PTBYO-UTAGAIFLG     =   "3"
                       CONTINUE
                   ELSE
                       EVALUATE    PTBYO-UTAGAIFLG ALSO
                                               WRK-TUTAGAIFLG (IDY)
                           WHEN    SPACE           ALSO    "1"
                           WHEN    SPACE           ALSO    "2"
                               MOVE    WRK-TUTAGAIFLG (IDY)
                                                   TO  PTBYO-UTAGAIFLG
                           WHEN    "2"             ALSO    "1"
                           WHEN    "1"             ALSO    "2"
                               MOVE    "3"         TO  PTBYO-UTAGAIFLG
                       END-EVALUATE
                   END-IF      
      *            病名コード数
                   ADD     1                   TO  PTBYO-BYOMEICDSU
      *            基本病名コード・基本部位コード
                   IF      PTBYO-BYOMEICD (IDY)(1:1)
                                               NOT =   "Z" 
                       MOVE    PTBYO-BYOMEICD (IDY)
                                           TO  PTBYO-KHNBYOMEICD
                   ELSE
                       IF      PTBYO-BYOMEICD (IDY) (1:4)
                                               =   "ZZZ1"
                           ADD     1       TO  IDZ
                           IF      IDZ         <   4
                               MOVE    PTBYO-BYOMEICD (IDY)
                                               TO  PTBYO-KHNBUICD(IDZ)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
      *        病名文字数
               PERFORM VARYING     IDY     FROM    80 BY  -1
                       UNTIL       IDY     <   1
                   IF      PTBYO-BYOMEI(IDY:1)     NOT =   SPACE
                       COMPUTE PTBYO-BYOMEIMOJI    =   IDY /   2
                       MOVE    1                   TO  IDY
                   END-IF
               END-PERFORM
      *
      *        病名編集フラグ（コードがエラーのとき、病名が長いとき）
               IF    ( FLG-BYOMEI-CHK  NOT =   ZERO  )
               OR    ( WRK-BYOMEI2     NOT =   SPACE )    
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               END-IF
      *
      *        レセ電エラーフラグ（編集病名のとき）
               IF      PTBYO-BYOMEIHENFLG          =   "1"
                   EVALUATE    PTBYO-SYUBYOFLG       
                       WHEN    "1"
                           IF      PTBYO-BYOMEIMOJI        >   17
                               MOVE    "1" TO  PTBYO-RECEDENFLG       
                           END-IF
                       WHEN    SPACE
                           IF      PTBYO-BYOMEIMOJI        >   20
                               MOVE    "1" TO  PTBYO-RECEDENFLG       
                           END-IF
                   END-EVALUATE
               END-IF
      *
      *        レセ電エラーフラグ（疑いフラグが設定されているとき）
               IF    ( PTBYO-RECEDENFLG        =   SPACE ) 
               AND   ( PTBYO-UTAGAIFLG     NOT =   SPACE )
                   MOVE    SPACE               TO  WRK-HENFLG1
                                                   WRK-HENFLG2
      *     
      *            病名文字数を計算
                   COMPUTE WRK-MOJISU      =   PTBYO-BYOMEIMOJI       
                                           *   2
                   IF      PTBYO-UTAGAIFLG         =   "1" OR  "3"
                       IF      WRK-MOJISU      <   8
                           MOVE    1               TO  IDZ
                       ELSE    
                           COMPUTE IDZ     =   WRK-MOJISU    -   7
                       END-IF    
                       PERFORM VARYING IDY FROM    IDZ BY  2
                               UNTIL   IDY >       WRK-MOJISU
                           IF      PTBYO-BYOMEI        (IDY:2) =   "疑"
                               MOVE    "1"              TO  WRK-HENFLG1
                               MOVE    WRK-MOJISU       TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF                
                   IF      PTBYO-UTAGAIFLG         =   "2" OR  "3"
                       PERFORM VARYING IDY FROM    1   BY  2
                               UNTIL   IDY >       WRK-MOJISU
                           IF      PTBYO-BYOMEI        (IDY:4) =  "急性"
                               MOVE    "1"              TO  WRK-HENFLG2
                               MOVE    WRK-MOJISU       TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *            疑いフラグがセットされているとき
                   IF    ( PTBYO-UTAGAIFLG     =   "1" OR  "3" )
                   AND   ( WRK-HENFLG1         =   SPACE       )                         
                       COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-UTAGAI-MOJISU
                       IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                           EVALUATE    PTBYO-SYUBYOFLG       
                               WHEN    "1"
                                   IF      WRK-MOJISU      >   17
                                       MOVE    "1"     TO
                                                       PTBYO-RECEDENFLG       
                                   END-IF
                               WHEN    SPACE
                                   IF      WRK-MOJISU      >   20
                                       MOVE    "1"     TO
                                                       PTBYO-RECEDENFLG       
                                   END-IF
                           END-EVALUATE
                       END-IF
                   END-IF    
                   IF    ( PTBYO-UTAGAIFLG     =   "2" OR  "3" )
                   AND   ( WRK-HENFLG2         =   SPACE       )                         
                       COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-KYUSEI-MOJISU
                       IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                           EVALUATE    PTBYO-SYUBYOFLG       
                               WHEN    "1"
                                   IF      WRK-MOJISU      >   17
                                       MOVE    "1"     TO
                                                       PTBYO-RECEDENFLG       
                                   END-IF
                               WHEN    SPACE
                                   IF      WRK-MOJISU      >   20
                                       MOVE    "1"     TO
                                                       PTBYO-RECEDENFLG       
                                   END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF    
           ELSE
      *        疾患コードがエラー又は未コード化傷病名コードのとき
      *        病名からコード検索
               INITIALIZE                  ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    PTBYO-SRYYMD        TO  LNK-BYO-SRYYMD-IN
               MOVE    PTBYO-UTAGAIFLG     TO  LNK-BYO-UTAGAIFLG-IN
               MOVE    PTBYO-BYOMEI        TO  LNK-BYO-BYOMEI
               MOVE    PTBYO-SYUBYOFLG     TO  LNK-BYO-SYUBYOFLG 
               CALL    "ORCSBYOMEI"        USING    ORCSBYOMEIAREA
                                                    SPA-AREA
      *  
      *        疑いフラグ
               IF      PTBYO-UTAGAIFLG     =   "3"
                   CONTINUE
               ELSE
                   EVALUATE    PTBYO-UTAGAIFLG ALSO
                                           LNK-BYO-UTAGAIFLG
                       WHEN    SPACE           ALSO    "1"
                       WHEN    SPACE           ALSO    "2"
                           MOVE    LNK-BYO-UTAGAIFLG
                                                   TO  PTBYO-UTAGAIFLG
                       WHEN    "2"             ALSO    "1"
                       WHEN    "1"             ALSO    "2"
                           MOVE    "3"         TO  PTBYO-UTAGAIFLG
                   END-EVALUATE
               END-IF      
      *        病名コード
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL   IDX4    >       LNK-BYO-BYOMEI-MAX
                   MOVE    LNK-BYO-TBYOMEICD (IDX4)
                                       TO  PTBYO-BYOMEICD  (IDX4)
               END-PERFORM
      *        病名コード数
               MOVE    LNK-BYO-BYOMEICDSU  TO  PTBYO-BYOMEICDSU
      *        基本病名コード
               MOVE    LNK-BYO-KHNBYOMEICD TO  PTBYO-KHNBYOMEICD
      *        基本部位コード
               MOVE    LNK-BYO-KHNBUICD (1)
                                           TO  PTBYO-KHNBUICD (1)
               MOVE    LNK-BYO-KHNBUICD (2)
                                           TO  PTBYO-KHNBUICD (2)
               MOVE    LNK-BYO-KHNBUICD (3)
                                           TO  PTBYO-KHNBUICD (3)
      *        レセ電エラーフラグ
               MOVE    LNK-BYO-RECEDENFLG  TO  PTBYO-RECEDENFLG
      *        病名文字数
               MOVE    LNK-BYO-BYOMEIMOJI  TO  PTBYO-BYOMEIMOJI
      *        病名編集フラグ
               IF    ( LNK-BYO-BYOMEICD-ERR    =   1  )
               OR    ( LNK-BYO-TANDOKUKBN-ERR  =   1  )
               OR    ( LNK-BYO-BYOMEIMOJI      >   80 )
      *            単独病名・修飾語コードのみ・病名が長いとき
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   MOVE    LNK-BYO-BYOMEIHENFLG
                                               TO  PTBYO-BYOMEIHENFLG
               END-IF
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO 
               DISPLAY "UPD PTBYOMEI-KEY =>" PTBYO-KEY  "."
           ELSE
               ADD     1                   TO  CNT-OUT02
           END-IF
           .
       2006124-KOUSIN-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ更新処理（同期フラグ設定時）
      *****************************************************************
       2006125-KOUSIN-03-SEC           SECTION.
      *
      *    患者病名マスタ検索（同一開始日・病名）
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           IF      WRK-UTAGAIFLG-KETASU    =   ZERO
               MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
               STRING  WRK-BYOMEI1 (1:80)  DELIMITED BY  SPACE
                       "の疑い"            DELIMITED BY  SIZE  
                                           INTO    PTBYO-XXBYOMEI
               END-STRING
           ELSE
               MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
               STRING  WRK-BYOMEI1 (1:WRK-UTAGAIFLG-KETASU - 1)
                                           DELIMITED BY  SIZE
                       WRK-BYOMEI1 (WRK-UTAGAIFLG-KETASU + 6:)
                                           DELIMITED BY  SPACE
                                           INTO    PTBYO-XXBYOMEI
               END-STRING
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL   FLG-PTBYOMEI    =   1
               MOVE    PTBYO-SRYKA         TO  KEY-N-SRYKA
               INITIALIZE                  WRK-PTBYOMEI-DOUKI-REC
      *
               PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                               OR      PTBYO-SRYKA NOT =   KEY-N-SRYKA    
      *
                   IF      WRK-PTBYO-DOUKI-RENNUM
                                               =   ZERO
                       MOVE    PTBYOMEI-REC    TO WRK-PTBYOMEI-DOUKI-REC
                   END-IF 
                   IF      WRK-BYOMEI1 (1:80)  =   PTBYO-BYOMEI
                       MOVE    PTBYOMEI-REC    TO WRK-PTBYOMEI-DOUKI-REC
                   END-IF 
      *
                   MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
                   MOVE    "key29"             TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               END-PERFORM
      *
      *        対象病名の更新
               MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-TAIHI-REC
      *
               MOVE    WRK-PTBYOMEI-DOUKI-REC
                                           TO  PTBYOMEI-REC
               PERFORM 2006124-KOUSIN-02-SEC
      *
               MOVE    WRK-PTBYOMEI-TAIHI-REC
                                           TO  PTBYOMEI-REC 
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2006125-KOUSIN-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付に会計待を設定処理
      *****************************************************************
       2010-UKETUKE-CHK-SEC                 SECTION.
      *
      *    受付テーブル読み込み
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    WRK-PTID            TO  UKE-PTID
           MOVE    WRK-SRYYMD          TO  UKE-UKEYMD
           MOVE    WRK-SRYKA           TO  UKE-SRYKA
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           INITIALIZE                  WRK-UKETUKE-REC
           MOVE    ZERO                TO  FLG-UKE
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL     (FLG-UKETUKE      =  1  )  OR
                             (FLG-OK           =  1  )
      *
               IF     (UKE-UKEID           >   ZERO  )   AND
                      (UKE-KAIKEITIME      =   ZERO  )
                   IF      FLG-UKE             =   ZERO
                       MOVE    UKETUKE-REC         TO  WRK-UKETUKE-REC
                       MOVE    1                   TO  FLG-UKE
                   END-IF
                   IF      (WRK-HKNCOMBI   NOT =   ZERO  )  AND
                           (WRK-HKNCOMBI       =   UKE-HKNCOMBI)
      *                会計待
                       MOVE    1                   TO  FLG-OK
                       MOVE    "03"                TO  UKE-SRFLG
                       MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_uketuke"       TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                      IF      MCP-RC          NOT =   ZERO 
                           DISPLAY "ORCL0031 UKETUKE UPD ERR:"  MCP-RC
                                   ",KEY:" UKE-KEY
                       END-IF
                   END-IF
               END-IF
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK              =   ZERO
      *        保険が決定しない時、１件目
               IF      FLG-UKE         =   1
                   MOVE    WRK-UKETUKE-REC     TO  UKETUKE-REC
                   MOVE    "03"                TO  UKE-SRFLG
                   MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                  IF      MCP-RC          NOT =   ZERO 
                       DISPLAY "ORCL0031 UKETUKE UPD ERR:"  MCP-RC
                               ",KEY:" UKE-KEY
                   END-IF
               END-IF
           END-IF
           .
       2010-UKETUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
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
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  920-DBFETCH-SEC 
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
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込（主キー）
      *****************************************************************
       900-PTBYO-INV-SEC         SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE                  PTBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
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
           PERFORM  920-DBFETCH-SEC 
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
      *    公費履歴読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       940-HKNCOMB-READ-SEC        SECTION.
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       940-HKNCOMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC          SECTION.
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスター読込
      *****************************************************************
       910-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-UKETUKE
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       910-UKETUKE-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＵＩＤデータ読込処理
      *****************************************************************
       903-CLMUID-READ-SEC         SECTION.
      *
           MOVE    CLMUID-REC          TO  MCPDATA-REC
           MOVE    "tbl_clm_uid"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_clm_uid"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  CLMUID-REC
                   MOVE    ZERO                TO  FLG-CLMUID
               ELSE
                   MOVE    1                   TO  FLG-CLMUID
                   INITIALIZE                  CLMUID-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-CLMUID
               INITIALIZE                  CLMUID-REC
           END-IF
      *
           MOVE    "tbl_clm_uid"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       903-CLMUID-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
