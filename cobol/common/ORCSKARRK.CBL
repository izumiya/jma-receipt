      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
       PROGRAM-ID.             ORCSKARRK.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¿ÇÎÅ²ÊÍúÎò¡¡½é¿Ç»»ÄêÆü¹¹¿·½èÍý
      *  ÊÖµÑ¥¨¥é¡¼¥³¡¼¥É  : 00:Àµ¾ï¡¡10:¥Ñ¥é¥á¡¼¥¿ÉÔÀµ¡¦ÂÐ¾Ý¥Ç¡¼¥¿¤Ê¤·
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  01.00.01    MCC-Â¿¡¹Ç¼   02/05/15  Ï«ºÒÍÑÄÉ²Ã
      *  01.00.02    NACL-Â¿¡¹Ç¼  02/12/15  ½é¿Ç»»ÄêÆüÈ½Äê½¤Àµ
      *  01.00.03    NACL-Â¿¡¹Ç¼  03/02/13  ÄûÀµ½é¿Ç»»ÄêÆüÈ½Äê½¤Àµ
      *  01.00.05    NACL-Â¿¡¹Ç¼  06/01/12  ºï½ü½èÍýÊÑ¹¹
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-JYURRK-ARI      PIC 9(01).
           03  FLG-JYURRK-CHK      PIC 9(01).
           03  FLG-SRYACCT-CHK     PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SRYCD               PIC X(09). 
           03  WRK-SYOSINYMD1          PIC X(08). 
           03  WRK-SYOSINYMD2          PIC X(08). 
           03  WRK-LST-SRYYMD          PIC X(08). 
           03  WRK-LST-SRYYMD2         PIC X(08). 
      *    Æþ±¡ÍúÎò
           03  WRK-NYULST-SRYYMD          PIC X(08). 
           03  WRK-NYULST-SRYYMD2         PIC X(08). 
      *
           03  WRK-SANTEIYMD           PIC X(08). 
           03  WRK-DAY                 PIC 9(02).
           03  WRK-DD                  PIC 9(02).
      *
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SYOSINYMD-RRK       PIC X(08).
           03  WRK-SYOSINYMD-RRK2      PIC X(08).
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ÊÝ¸±ÊÌ»»ÄêÍúÎò¥Æ¡¼¥Ö¥ë
           COPY   "CMSANTEITBL.INC".
      *
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
           COPY    "CPTENSU.INC".
      *
      *    ¿ÇÎÅ²ÊÍúÎò
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *    ¿ÇÎÅ²ÊÍúÎò
       01  HZN-SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC"  REPLACING
                                     //KARRK-// BY //HZN-KARRK-//.
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSKARRK.INC".
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSKARRKAREA
           SPA-AREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  SKARRK-RC
           MOVE    SPACE               TO  SKARRK-ERRMSG
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ¹¹¿·Æü¼èÆÀ
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SKARRK-SRYYM        =   SPACE
               MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
           END-IF
      *
      *    ¿ÇÎÅ²ÊÍúÎò½é´ü½èÍý
           PERFORM 100-SRYKA-SYOKI-SEC
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
           PERFORM 200-SANTEIRRK-CHK-SEC
      *    ¿ÇÎÅ²ÊÍúÎò¹¹¿·½èÍý
           PERFORM 300-SRYKARRK-HEN-SEC
      *
           IF      SPA-ERRCD           NOT =   SPACE
               MOVE    "¿ÇÎÅ²ÊÍúÎò¹¹¿·¥¨¥é¡¼"      TO  SKARRK-ERRMSG
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    10                  TO  SKARRK-RC
           END-IF
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò½é´ü½èÍý
      *****************************************************************
       100-SRYKA-SYOKI-SEC                SECTION.
      *
      *    ¿ÇÎÅ²ÊÍúÎòÆÉ¹þ
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *    ¿ÇÎÅ²Ê¿·µ¬ºîÀ®
           IF      FLG-SRYKARRK           =   1
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                      SRYKARRK-REC
               MOVE    SPA-HOSPID          TO  KARRK-HOSPID
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    SPA-SRYKA           TO  KARRK-SRYKA
           END-IF
           .
      *
       100-SRYKA-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       200-SANTEIRRK-CHK-SEC                SECTION.
      *
      *    ½é¿ÇÎÁ¤Î»»ÄêÍúÎò¤Î¥Á¥§¥Ã¥¯
           MOVE    SPACE               TO  WRK-SYOSINYMD1
           MOVE    SPACE               TO  WRK-SYOSINYMD2
      *
      **** EVALUATE    SPA-HOSPSBT
      *        ÉÂ±¡
      *******  WHEN    1
      *            Ï·¿Í
                   MOVE    "111700110"         TO  WRK-SRYCD
                   PERFORM 2001-SANTEI-CHK-SEC
      *            Ï·¿Í³°
                   MOVE    "111000110"         TO  WRK-SRYCD
                   PERFORM 2001-SANTEI-CHK-SEC
      *        ¿ÇÎÅ½ê
      ******   WHEN    2
      *            Ï·¿Í
                   MOVE    "111700210"         TO  WRK-SRYCD
                   PERFORM 2001-SANTEI-CHK-SEC
      *            Ï·¿Í³°
                   MOVE    "111003610"         TO  WRK-SRYCD
                   PERFORM 2001-SANTEI-CHK-SEC
      *****END-EVALUATE
      *    ¾®»ù²Ê¿Ç»¡ÎÁ
      *    ¡¡¡¡±¡³°½èÊý   ½é¿Ç
           MOVE    "113003510"         TO  WRK-SRYCD
           PERFORM 2001-SANTEI-CHK-SEC
      *    ¡¡¡¡±¡Æâ½èÊý   ½é¿Ç
           MOVE    "113003710"         TO  WRK-SRYCD
           PERFORM 2001-SANTEI-CHK-SEC
      *
      *    ²¾¤Î½é¿ÇÎÁ
           MOVE    "099110001"         TO  WRK-SRYCD
           PERFORM 2001-SANTEI-CHK-SEC
      *
      *    Ï«ºÒ½é¿ÇÎÁ
           MOVE    "101110010"         TO  WRK-SRYCD
           PERFORM 2001-SANTEI-CHK-SEC
      *
           .
      *
       200-SANTEIRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-SANTEI-CHK-SEC                SECTION.
      *
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
      *****MOVE    SKARRK-SRYYM        TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL       FLG-SANTEI          =   1
      *
               IF      SANTEI-MSANTEICNT   >   ZERO
                   MOVE    ZERO            TO  WRK-DAY
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF      SANTEI-DAY (IDY)    NOT =   ZERO
                           MOVE    IDY             TO  WRK-DAY
                       END-IF
                   END-PERFORM
                   MOVE    SANTEI-SRYYM            TO  WRK-SANTEIYMD
                                                               (1:6)
                   MOVE    WRK-DAY                 TO  WRK-SANTEIYMD
                                                               (7:2)
                   IF     (WRK-SYOSINYMD1          =   SPACE  )  OR
                          (WRK-SANTEIYMD           <   WRK-SYOSINYMD1)
                       MOVE    WRK-SANTEIYMD       TO  WRK-SYOSINYMD1
                   END-IF
                   IF      WRK-SANTEIYMD           >   WRK-SYOSINYMD2
                       MOVE    WRK-SANTEIYMD       TO  WRK-SYOSINYMD2
                   END-IF
               END-IF
      *
               MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       2001-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò¹¹¿·½èÍý
      *****************************************************************
       300-SRYKARRK-HEN-SEC                SECTION.
      *
      *    ºÇ½ª¼õ¿ÇÍúÎò
           MOVE    SPACE               TO  WRK-LST-SRYYMD
           MOVE    SPACE               TO  WRK-LST-SRYYMD2
           MOVE    SPACE               TO  WRK-NYULST-SRYYMD
           MOVE    SPACE               TO  WRK-NYULST-SRYYMD2
           MOVE    SPACE               TO  WRK-SYOSINYMD-RRK
           MOVE    SPACE               TO  WRK-SYOSINYMD-RRK2
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
      *****MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY8"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY8"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-JYURRK-ARI
           MOVE    ZERO                TO  FLG-JYURRK-CHK
           PERFORM
               UNTIL      (FLG-JYURRK      =   1   )  OR
                         ((WRK-LST-SRYYMD  NOT =   SPACE  ) AND
                          (WRK-LST-SRYYMD2 NOT =   SPACE  ) AND
                          (FLG-JYURRK-CHK  NOT =   ZERO   ))
      *      Êñ³ç¤ÏÂÐ¾Ý³°
             IF      JYURRK-HKNCOMBINUM    NOT =   "9999"
      *
      *        ¿ÇÎÅ²ÊÍúÎò¤Ï³°Íè¤Î¤ß¤ÇºîÀ®¤¹¤ë
               IF      JYURRK-NYUGAIKBN    =   "2"
                   IF      SPA-SRYKA       =   JYURRK-SRYKA
                       IF      WRK-LST-SRYYMD      =   SPACE
                           MOVE    JYURRK-SRYYMD   TO  WRK-LST-SRYYMD
                       END-IF
                       IF     (JYURRK-SRYKBN1      NOT =   SPACE ) AND
                              (WRK-SYOSINYMD-RRK       =   SPACE )
                           PERFORM 3001-SYOSIN-SENTEI-SEC
                           MOVE    1               TO  FLG-JYURRK-CHK
                       END-IF
                   ELSE
                       IF      WRK-LST-SRYYMD2     =   SPACE
                           MOVE    JYURRK-SRYYMD   TO  WRK-LST-SRYYMD2
                       END-IF
                       IF     (JYURRK-SRYKBN1      NOT =   SPACE ) AND
                              (WRK-SYOSINYMD-RRK2      =   SPACE )
                           PERFORM 3001-SYOSIN-SENTEI-SEC
                       END-IF
                   END-IF
      *            Æþ±¡ÍúÎò
               ELSE
                   IF      SPA-SRYKA       =   JYURRK-SRYKA
                       IF      WRK-NYULST-SRYYMD   =   SPACE
                           MOVE    JYURRK-SRYYMD   TO  WRK-NYULST-SRYYMD
                       END-IF
                       IF     (JYURRK-SRYKBN1      NOT =   SPACE ) AND
                              (WRK-SYOSINYMD-RRK       =   SPACE )
                           PERFORM 3001-SYOSIN-SENTEI-SEC
                           MOVE    1               TO  FLG-JYURRK-CHK
                       END-IF
                   ELSE
                       IF      WRK-NYULST-SRYYMD2  =   SPACE
                           MOVE    JYURRK-SRYYMD   TO
                                                   WRK-NYULST-SRYYMD2
                       END-IF
                       IF     (JYURRK-SRYKBN1      NOT =   SPACE ) AND
                              (WRK-SYOSINYMD-RRK2      =   SPACE )
                           PERFORM 3001-SYOSIN-SENTEI-SEC
                       END-IF
                   END-IF
               END-IF
               MOVE    1                   TO  FLG-JYURRK-ARI
      *
               END-IF
      *
               MOVE    "JYURRK-KEY8"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "JYURRK-KEY8"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    ³°Íè¤¬¤Ê¤¯¡¢Æþ±¡¤Î¤Î¤ß¤Î»þ¡¢Æþ±¡¤ÎºÇ½ªÍúÎòÆü¤òÊÔ½¸¤¹¤ë
           IF     (WRK-LST-SRYYMD      =   SPACE )  AND
                  (WRK-LST-SRYYMD2     =   SPACE )
               IF     (WRK-NYULST-SRYYMD       =   SPACE )  AND
                      (WRK-NYULST-SRYYMD2      =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    WRK-NYULST-SRYYMD   TO  WRK-LST-SRYYMD
                   MOVE    WRK-NYULST-SRYYMD2  TO  WRK-LST-SRYYMD2
               END-IF
           ELSE
      *        Æþ±¡¤ÎºÇ½ªÍúÎòÆü¤òÊÔ½¸¤¹¤ë
               IF     (WRK-NYULST-SRYYMD   NOT =   SPACE ) AND
                      (WRK-LST-SRYYMD          <   WRK-LST-SRYYMD)
                   MOVE    WRK-NYULST-SRYYMD   TO  WRK-LST-SRYYMD
               END-IF
           END-IF
      *
      *    ºÇ½ª¼õ¿ÇÆü
           MOVE    WRK-LST-SRYYMD      TO  KARRK-LASTYMD
      *
           IF      WRK-SYOSINYMD-RRK   NOT =   SPACE
      *        ¿ÇÎÅ²Ê¤Î½é¿ÇÎÁ¤¢¤ê
               IF      WRK-SYOSINYMD-RRK       <   WRK-SYOSINYMD2
      *            »»ÄêÍúÎò¤Î¤ß¤Î¾ì¹ç
                   MOVE    WRK-SYOSINYMD2      TO  KARRK-SYOSINYMD2
               ELSE
                   MOVE    WRK-SYOSINYMD-RRK   TO  KARRK-SYOSINYMD2
               END-IF
               IF      WRK-SYOSINYMD-RRK       <   KARRK-SYOSINYMD1
                   MOVE    WRK-SYOSINYMD-RRK   TO  KARRK-SYOSINYMD1
               END-IF
           ELSE
      *        ¿ÇÎÅ²Ê¤Î½é¿ÇÎÁ¤Ê¤·
               IF      WRK-LST-SRYYMD  NOT =   SPACE
                   IF      WRK-SYOSINYMD-RRK2  =   SPACE
                       MOVE    WRK-SYOSINYMD2      TO  KARRK-SYOSINYMD2
                   ELSE
                       MOVE    SPACE           TO  KARRK-SYOSINYMD2
                   END-IF
               END-IF
      *        Â¾²Ê¼õ¿Ç¤¢¤ê
               IF     (WRK-LST-SRYYMD      =   SPACE )   AND
                      (WRK-LST-SRYYMD2 NOT =   SPACE )
                   MOVE    SPACE           TO  KARRK-SYOSINYMD1
                   MOVE    SPACE           TO  KARRK-SYOSINYMD2
                   MOVE    SPACE           TO  KARRK-LASTYMD
               END-IF
      *        »»ÄêÍúÎò¤Î¤ß
               IF     (WRK-LST-SRYYMD      =   SPACE )   AND
                      (WRK-LST-SRYYMD2     =   SPACE )
                   MOVE    WRK-SYOSINYMD1      TO  KARRK-SYOSINYMD1
                   MOVE    WRK-SYOSINYMD2      TO  KARRK-SYOSINYMD2
                   MOVE    WRK-SYOSINYMD2      TO  KARRK-LASTYMD
               END-IF
           END-IF
      *    ½é¿ÇÎÁ£±
      *    IF    ( KARRK-SYOSINYMD1    =   SPACE    )  OR
      *          ( SKARRK-SRYYM        =   KARRK-SYOSINYMD1(1:6))
      *        IF      WRK-SYOSINYMD1      =   SPACE
      *            IF      WRK-LST-SRYYMD      =   SPACE  AND
      *                    WRK-LST-SRYYMD2     =   SPACE
      *                 ¤¹¤Ù¤Æ¤ÎÍúÎò¤Ê¤·
      *                MOVE    SPACE           TO  KARRK-SYOSINYMD1
      *            END-IF
      *        ELSE
      *            MOVE   WRK-SYOSINYMD1   TO  KARRK-SYOSINYMD1
      *        END-IF
      *    END-IF
      *    ½é¿ÇÎÁ£²
      *    IF    ( KARRK-SYOSINYMD2    =   SPACE    )  OR
      *          ( SKARRK-SRYYM        =   KARRK-SYOSINYMD2(1:6))
      *        IF      WRK-SYOSINYMD2      =   SPACE
      *            IF      WRK-LST-SRYYMD      =   SPACE
      *                MOVE    SPACE           TO  KARRK-SYOSINYMD2
      *            END-IF
      *        ELSE
      *            MOVE   WRK-SYOSINYMD2   TO  KARRK-SYOSINYMD2
      *        END-IF
      **** END-IF
      *    IF      KARRK-LASTYMD       =   SPACE
      *        ºÇ½ª¼õ¿ÇÆü
      *        MOVE    KARRK-SYOSINYMD2    TO  KARRK-LASTYMD
      *    END-IF
      *
      *    ¤¹¤Ù¤Æ¤ÎÍúÎò¤¬¤Ê¤¤»þ¡¢Â¾¤Î¿ÇÎÅ²ÊÍúÎò¤âºï½ü¤¹¤ë
           IF     (FLG-JYURRK-ARI      =   ZERO  )  AND
                  (WRK-SYOSINYMD1      =   SPACE )  AND
                  (WRK-SYOSINYMD2      =   SPACE )
               PERFORM 3009-SRYKARRK-DEL-SEC
               GO      TO      300-SRYKARRK-HEN-EXT
           END-IF
      *
      *    ¤¹¤Ù¤Æ¤ÎÆüÉÕ¤¬¶õÇò¤Î»þ¡¢ºï½ü¤¹¤ë
           IF     (KARRK-SYOSINYMD1        =   SPACE )  AND
                  (KARRK-SYOSINYMD2        =   SPACE )  AND
                  (KARRK-LASTYMD           =   SPACE )
               IF      FLG-SRYKARRK           =   ZERO
                   MOVE    SRYKARRK-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "ORCSCKARRK SRYKARRK DEL ERR:"
                                                    MCP-RC  "."
                       MOVE    "3000"          TO  SPA-ERRCD
                   END-IF
      *            Â¾¤Î¿ÇÎÅ²Ê¤ÎÆâÍÆ³ÎÇ§
                   PERFORM 3005-DEL-SYORI-SEC
              END-IF
              GO      TO      300-SRYKARRK-HEN-EXT
           END-IF
      *
      *
           IF      FLG-SRYKARRK           =   ZERO
      *        ¹¹¿·
               MOVE    SPA-HOSPID          TO  KARRK-HOSPID
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    SPA-SRYKA           TO  KARRK-SRYKA
      *
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "ORCSCKARRK SRYKARRK UPD ERR:"  MCP-RC  "."
                   MOVE    "3000"          TO  SPA-ERRCD
               END-IF
           ELSE
      *        ¿·µ¬
               MOVE    SPA-HOSPID          TO  KARRK-HOSPID
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    SPA-SRYKA           TO  KARRK-SRYKA
      *
      *
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
                   DISPLAY "ORCSCKARRK SRYKARRK INS ERR:"  MCP-RC  "."
                   MOVE    "3000"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       300-SRYKARRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿Ç»»ÄêÆü¤òÍúÎò¤è¤ê½èÍý
      *****************************************************************
       3001-SYOSIN-SENTEI-SEC         SECTION.
      *
           MOVE    "11"                TO  WRK-SRYKBN
           PERFORM 30011-SRYACCT-CHK-SEC
           IF      FLG-SRYACCT-CHK     =   ZERO
               MOVE    "13"                TO  WRK-SRYKBN
               PERFORM 30011-SRYACCT-CHK-SEC
           END-IF
      *
           .
       3001-SYOSIN-SENTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎòºï½ü½èÍý
      *****************************************************************
       30011-SRYACCT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT-CHK
      *
      *    ¿ÇÎÅÇ¯·î¤Î¿ÇÎÅ²ñ·×¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    JYURRK-HOSPID       TO  ACCT-HOSPID
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY3"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY3"      TO  ORC-DBPATH
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL       (FLG-SRYACCT         =   1    )  OR
                           (FLG-SRYACCT-CHK     =   1    )
               IF     (ACCT-ZAIKAISU       >   ZERO  )  AND
                      (ACCT-HKNCOMBI   NOT =   "9999")
      *            ¿ÇÎÅ¹Ô°Ù¤è¤ê
                   PERFORM 300112-SRYACT-HEN-SEC
                   IF      FLG-SRYACCT-CHK     =   1
      *            ºÇ¸å¤ÎÆü¤òÊÝÂ¸
                       PERFORM VARYING     IDY2    FROM    1   BY  1
                               UNTIL       IDY2    >   31
                           IF      ACCT-DAY (1 IDY2)   >   ZERO
                               MOVE    IDY2                TO  WRK-DD
                           END-IF
                       END-PERFORM
                       IF      SPA-SRYKA       =   JYURRK-SRYKA
                           MOVE    ACCT-SRYYM      TO  WRK-SYOSINYMD-RRK
                                                                 (1:6)
                           MOVE    WRK-DD          TO  WRK-SYOSINYMD-RRK
                                                                 (7:2)
                       ELSE
                           MOVE    ACCT-SRYYM  TO  WRK-SYOSINYMD-RRK2
                                                                 (1:6)
                           MOVE    WRK-DD      TO  WRK-SYOSINYMD-RRK2
                                                                 (7:2)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "SRYACCT-KEY3"      TO  ORC-DBPATH
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACCT-KEY3"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *
           .
       30011-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÈ½Äê½èÍý
      *****************************************************************
       300112-SRYACT-HEN-SEC         SECTION.
      *
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼¤«¤é¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¤ò¸¡º÷¤¹¤ë
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT       =   1 )  OR
                             (FLG-SRYACCT-CHK  =   1 )
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2            >   5   )  OR
                                  (SRY-SRYCD(IDX2) =   SPACE )
      *            ¿Ç»¡ÎÁ¤Ç¤¢¤ë¤³¤È
                   EVALUATE    SRY-SRYCD (IDX2)
                       WHEN    "111700110"
                       WHEN    "111000110"
                       WHEN    "111700210"
                       WHEN    "111003610"
                       WHEN    "113003510"
                       WHEN    "113003710"
                       WHEN    "099110001"
                       WHEN    "101110010"
                           MOVE    1               TO  FLG-SRYACCT-CHK
                           MOVE    5               TO  IDX2
                   END-EVALUATE
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 920-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       300112-SRYACT-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    Â¾¤Î¿ÇÎÅ²Ê¤ÎÆâÍÆ³ÎÇ§½èÍý
      *****************************************************************
       3005-DEL-SYORI-SEC         SECTION.
      *
      *    ¿ÇÎÅ²ÊÍúÎòÆÉ¹þ
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPID          TO  KARRK-HOSPID
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYKARRK-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYKARRK-KEY2"     TO  ORC-DBPATH
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  HZN-SRYKARRK-REC
           PERFORM UNTIL    FLG-SRYKARRK   =   1
               IF      KARRK-SYOSINYMD2    NOT =   SPACE
                   MOVE    1                   TO  FLG-OK
                   MOVE    1                   TO  FLG-SRYKARRK
               ELSE
                   IF     (HZN-SRYKARRK-REC    =   SPACE )  OR
                          (KARRK-SYOSINYMD1    =   WRK-SYOSINYMD2)
                       MOVE    SRYKARRK-REC        TO  HZN-SRYKARRK-REC
                   END-IF
                   MOVE    "SRYKARRK-KEY2"     TO  ORC-DBPATH
                   PERFORM 975-SRYKARRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYKARRK-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF     (HZN-SRYKARRK-REC    NOT =   SPACE )  AND
                  (FLG-OK              =   ZERO      )
               MOVE    HZN-SRYKARRK-REC    TO  SRYKARRK-REC
               MOVE    WRK-SYOSINYMD2      TO  KARRK-SYOSINYMD2
      *
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "ORCSCKARRK SRYKARRK UPD ERR:"  MCP-RC  "."
                   MOVE    "3000"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       3005-DEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎòºï½ü½èÍý
      *****************************************************************
       3009-SRYKARRK-DEL-SEC         SECTION.
      *
      *    ¿ÇÎÅ²ÊÍúÎò
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPID          TO  KARRK-HOSPID
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SRYKARRK-DEL1"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *
           .
       3009-SRYKARRK-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      **** MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           MOVE    "TENSU-KEY22"       TO  ORC-DBPATH
           MOVE    SKARRK-SRYYM        TO  ORC-DBYMD(1:6)
           MOVE    "01"                TO  ORC-DBYMD(7:2)
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *******  MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               MOVE    "TENSU-KEY22"       TO  ORC-DBPATH
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      **** MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           MOVE    "TENSU-KEY22"       TO  ORC-DBPATH
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
      *    »»ÄêÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ¿ÇÎÅ²ÊÎò¥Ç¡¼¥¿¼ç¥­¡¼¸¡º÷
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
      *****************************************************************
      *    ¼õ¿ÇÍúÎò½çÆÉ¤ß
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
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       920-SRYACT-NEXT-SEC         SECTION.
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
       920-SRYACT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
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
       930-SRYACCT-READ-EXT.
           EXIT.
      *
      *
