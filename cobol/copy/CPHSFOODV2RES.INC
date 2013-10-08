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
      *    ＡＰＩレスポンス食事情報     COPY (CPHSFOODV2RES.INC)
      ******************************************************************
       01  XML-HSFOODRES.
           03  HSFDRES-CONTEXT                     PIC S9(9)   BINARY.
           03  HSFDRES-OBJECT                      PIC X(8).
           03  HSFDRES-MODE                        PIC S9(9)   BINARY.
           03  HSFDRES-RECORDNAME                  PIC X(128).
      *
           03  HSFDRES-PRIVATE-OBJECTS.
               05  HSFDRES-INFORMATION-DATE        PIC X(10).
               05  HSFDRES-INFORMATION-TIME        PIC X(8).
               05  HSFDRES-API-RESULT              PIC X(2).
               05  HSFDRES-API-RESULT-MSG          PIC X(100).
               05  HSFDRES-PTINF-AREA.
                   07  HSFDRES-PTNUM               PIC X(20).
                   07  HSFDRES-NAME                PIC X(100).
                   07  HSFDRES-KANANAME            PIC X(100).
                   07  HSFDRES-BIRTHDAY            PIC X(10).
                   07  HSFDRES-SEX                 PIC X(01).
               05  HSFDRES-NYUIN-TAIIN-YMD-AREA.
                   07  HSFDRES-NYUIN-TAIIN-YMD-OCC OCCURS  5.
                       09  HSFDRES-NYUINYMD        PIC X(10).
                       09  HSFDRES-TAIINYMD        PIC X(10).
               05  HSFDRES-SRYYM                   PIC X(07).
               05  HSFDRES-CALENDAR-AREA.
                   07  HSFDRES-CALENDAR-OCC        OCCURS  31.
                       09  HSFDRES-SRYYMD              PIC X(10).
                       09  HSFDRES-SRYKA-G.
                           11  HSFDRES-SRYKA-LBL       PIC X(20).
                           11  HSFDRES-SRYKA-DATA      PIC X(02).
                           11  HSFDRES-SRYKA-NAME      PIC X(30).
                       09  HSFDRES-BTUNUM-G.
                           11  HSFDRES-BTUNUM-LBL      PIC X(20).
                           11  HSFDRES-BTUNUM-DATA     PIC X(2).
                       09  HSFDRES-BTUNAME-G.
                           11  HSFDRES-BTUNAME-LBL     PIC X(20).
                           11  HSFDRES-BTUNAME-DATA    PIC X(20).
                       09  HSFDRES-BRMNUM-G.
                           11  HSFDRES-BRMNUM-LBL      PIC X(10).
                           11  HSFDRES-BRMNUM-DATA     PIC X(06).
                       09  HSFDRES-HKNCOMBI            PIC X(04).
                       09  HSFDRES-PTSTATUS-G.
                           11  HSFDRES-PTSTATUS-LBL    PIC X(40).
                           11  HSFDRES-PTSTATUS-DATA   PIC X(02).
                           11  HSFDRES-PTSTATUS-NAME   PIC X(50).
                       09  HSFDRES-SKJ-ASA-G.
                           11  HSFDRES-SKJ-ASA-LBL     PIC X(20).
                           11  HSFDRES-SKJ-ASA-DATA    PIC X(02).
                           11  HSFDRES-SKJ-ASA-NAME    PIC X(30).
                       09  HSFDRES-SKJ-HIRU-G.
                           11  HSFDRES-SKJ-HIRU-LBL    PIC X(20).
                           11  HSFDRES-SKJ-HIRU-DATA   PIC X(02).
                           11  HSFDRES-SKJ-HIRU-NAME   PIC X(30).
                       09  HSFDRES-SKJ-YU-G.
                           11  HSFDRES-SKJ-YU-LBL      PIC X(20).
                           11  HSFDRES-SKJ-YU-DATA     PIC X(02).
                           11  HSFDRES-SKJ-YU-NAME     PIC X(30).
                       09  HSFDRES-SAGAKU-G.
                           11  HSFDRES-SAGAKU-LBL      PIC X(20).
                           11  HSFDRES-SAGAKU-DATA     PIC X(07).
                           11  HSFDRES-SAGAKU-NAME     PIC X(10).
               05  HSFDRES-HKNCOMBI-INF-AREA.
                   07  HSFDRES-HKNCOMBI-INF-OCC         OCCURS 10.
                       09  HSFDRES-HKNCOMBI-KEY             PIC X(04).
                       09  HSFDRES-HKNNUM                   PIC X(03).
                       09  HSFDRES-HKNJANUM                 PIC X(08).
                       09  HSFDRES-HKNJANAME                PIC X(20).
                       09  HSFDRES-HKIGO                    PIC X(80).
                       09  HSFDRES-HNUM                     PIC X(80).
                       09  HSFDRES-KOHINF-AREA.
                           11  HSFDRES-KOHINF-OCC           OCCURS 4.
                               13  HSFDRES-KOHNUM           PIC X(03).
                               13  HSFDRES-KOHNAME          PIC X(20).
                               13  HSFDRES-FTNJANUM         PIC X(08).
                               13  HSFDRES-JKYSNUM          PIC X(20).
