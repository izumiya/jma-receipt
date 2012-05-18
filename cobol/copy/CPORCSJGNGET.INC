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
      ************************************************************
      *    公費月上限額取得用パラメタ　　　 COPY (CPORCSJGNGET.INC)
      *                          使用プログラム  (ORCSJGNGET)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    06/03/20  他医療機関等一部負担累計追加
      *  01.00.02    NACL-門脇    09/07/10  他医療機関等一部負担累計
      *                                     編集フラグ追加
      *  01.00.03    NACL-門脇    12/05/16  都道府県番号追加
      ************************************************************
       01  LNK-ORCSJGNGET-REC.
      *
      *医療機関ＩＤ
           03  LNK-ORCSJGNGET-HOSPNUM       PIC 9(2).
      *入外区分
           03  LNK-ORCSJGNGET-NYUGAIKBN     PIC  X(01).
      *患者ＩＤ
           03  LNK-ORCSJGNGET-PTID          PIC  9(10).
      *公費保険番号
           03  LNK-ORCSJGNGET-KOHHKNNUM     PIC  X(03).
      *公費ＩＤ
           03  LNK-ORCSJGNGET-KOHID         PIC  9(10).
      *診療月
           03  LNK-ORCSJGNGET-SRYYM         PIC  X(06).
      *入院請求終了日
           03  LNK-ORCSJGNGET-SKYEDYMD      PIC  X(08).
      *都道府県番号
           03  LNK-ORCSJGNGET-PREFNUM       PIC  X(02).
      *
      *返却値
      *
           03  LNK-ORCSJGNGET-OUT-AREA.
      *外来上限額
             05  LNK-ORCSJGNGET-GAIJGNGAK   PIC  9(07).
      *入院上限額
             05  LNK-ORCSJGNGET-NYUJGNGAK   PIC  9(07).
      *他医療機関等一部負担累計（024,079:食事療養上限額）
             05  LNK-ORCSJGNGET-TAFTNRUIKEI PIC  9(07).
      *他医療機関等一部負担累計編集フラグ
             05  LNK-ORCSJGNGET-TAFTNFLG    PIC  9(01).
      *
      *復帰コード
      *
           03  LNK-ORCSJGNGET-RC            PIC  9(02).
      *
