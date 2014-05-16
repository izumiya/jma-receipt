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
      ******************************************************************
      *    ＡＰＩレスポンス医療機関情報取得
      *                                 COPY (CPAPISYS1001V2RES.INC)
      ******************************************************************
        01  XML-SYSTEM1001RES.
            02  SYS101RES-CONTEXT            PIC S9(9)   BINARY.
            02  SYS101RES-OBJECT             PIC X(8).
            02  SYS101RES-MODE               PIC S9(9)   BINARY.
            02  SYS101RES-RECORDNAME         PIC X(128).
            02  SYS101RES-APPOINTRES.
      *       実施日
              03  SYS101RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  SYS101RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  SYS101RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  SYS101RES-API-RESULT-MSG      PIC X(100).
      *
              03  SYS101RES-RESKEY              PIC X(100).
      *       基準日
              03  SYS101RES-BASE-DATE           PIC X(10).
      *       医療機関情報
              03  SYS101RES-MEDICAL-INFOMATION.
      *           都道府県番号
                  05  SYS101RES-PREFECTURES-NUMBER  PIC X(02).
      *           点数表
                  05  SYS101RES-POINT-LIST          PIC X(01).
      *           医療機関コード
                  05  SYS101RES-INSTITUTION-CODE    PIC X(07).
      *           医療機関種別
                  05  SYS101RES-INSTITUTION-SPECIATION PIC X(01).
      *           医療機関ＩＤ
                  05  SYS101RES-INSTITUTION-ID         PIC X(24).
      *           医療機関名称
                  05  SYS101RES-INSTITUTION-WHOLENAME  PIC X(120).
      *           開設者名称
                  05  SYS101RES-ESTABLISHER-WHOLENAME  PIC X(120).
      *           管理者名称
                  05  SYS101RES-ADMINISTRATOR-WHOLENAME PIC X(80).
      *           医療機関所在地
                  05  SYS101RES-ADDRESS-INFORMATION.
      *           所在地
                      07  SYS101RES-WHOLEADDRESS       PIC X(200).
      *           郵便番号
                      07  SYS101RES-ADDRESS-ZIPCODE    PIC X(07).
      *           電話番号
                      07  SYS101RES-PHONENUMBER        PIC X(15).
      *           FAX番号
                      07  SYS101RES-FAXNUMBER          PIC X(15).
      *           ｅメールアドレス
                      07  SYS101RES-EMAIL-ADDRESS      PIC X(100).
      *           ＨＰアドレス
                      07  SYS101RES-HOMEPAGE-ADDRESS   PIC X(100).
      * 
