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
       PROGRAM-ID.             ORCSSYUBETUGET.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»¥×¥È¼ïÊÌÊÖµÑ½èÍý
      *  ´ÉÍý¼Ô            : 
      *  06/10/06    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  WRK-CONS-AREA.
      *    Ï«ºÒÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    ¼«ÇåÀÕÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *
           03  WRK-CONS-SRYYM-200209   PIC 9(06)   VALUE   200209.      
           03  WRK-CONS-SRYYM-200609   PIC 9(06)   VALUE   200609.      
      *
      *****************************************************************
      *    Ï¢ÍíÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSYUBETUGET.INC".
      *****************************************************************
     *
       PROCEDURE                    DIVISION    USING
                                    ORCSSYUBETUGETAREA.
     *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      LNK-SYUBETUGET-OT
           .
       100-INIT-EXT.
           EXIT
           .
      *     
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
      *    DISPLAY "SRYYM     : " LNK-SYUBETUGET-SRYYM
      *    DISPLAY "HKNNUM    : " LNK-SYUBETUGET-HKNNUM
      *    DISPLAY "HONKZKKBN : " LNK-SYUBETUGET-HONKZKKBN 
      *    DISPLAY "-NYUGAIKBN: " LNK-SYUBETUGET-NYUGAIKBN 
      *    DISPLAY "IHO       : " LNK-SYUBETUGET-IHO 
      *    DISPLAY "KOKUHO    : " LNK-SYUBETUGET-KOKUHO 
      *    DISPLAY "RJN       : " LNK-SYUBETUGET-RJN 
      *    DISPLAY "KOHI-CNT  : " LNK-SYUBETUGET-KOHI-CNT
      *
      *    Ê¿À®£±£´Ç¯£¹·î¤Þ¤Ç
           IF      LNK-SYUBETUGET-SRYYM    <=  WRK-CONS-SRYYM-200209
             EVALUATE  LNK-SYUBETUGET-HONKZKKBN ALSO
                                    LNK-SYUBETUGET-NYUGAIKBN  ALSO
                       LNK-SYUBETUGET-IHO  ALSO
                                    LNK-SYUBETUGET-KOKUHO     ALSO
                       LNK-SYUBETUGET-RJN  ALSO
                                    LNK-SYUBETUGET-KOHI-CNT
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1311     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1312     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1321     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1322     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1331     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1332     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1411     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1412     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1415     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1416     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1421     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1422     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1425     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1426     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1431     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1432     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1435     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1436     TO  LNK-SYUBETUGET-RECESYUBETU
             END-EVALUATE
           END-IF
      *
      *    Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
           IF    ( LNK-SYUBETUGET-SRYYM    >   WRK-CONS-SRYYM-200209 )
           AND   ( LNK-SYUBETUGET-SRYYM    <=  WRK-CONS-SRYYM-200609 )
             EVALUATE  LNK-SYUBETUGET-HONKZKKBN ALSO
                                    LNK-SYUBETUGET-NYUGAIKBN  ALSO
                       LNK-SYUBETUGET-IHO  ALSO
                                    LNK-SYUBETUGET-KOKUHO     ALSO
                       LNK-SYUBETUGET-RJN  ALSO
                                    LNK-SYUBETUGET-KOHI-CNT
      *        ÊÝ¸±Ã±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ¸øÈñÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212     TO  LNK-SYUBETUGET-RECESYUBETU
      *       £²¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222     TO  LNK-SYUBETUGET-RECESYUBETU
      *       £³¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Ï·¿ÍÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Ï·¿Í¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Ï·¿Í¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Âà¿¦¼ÔÃ±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1417     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1418     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1419     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1410     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Âà¿¦¼Ô¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1427     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1428     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1429     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1420     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Âà¿¦¼Ô¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1437     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1438     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1439     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1430     TO  LNK-SYUBETUGET-RECESYUBETU
             END-EVALUATE
           END-IF
      *
      *    Ê¿À®£±£¸Ç¯£±£°·î²þÀµÂÐ±þ
           IF    ( LNK-SYUBETUGET-SRYYM    >   WRK-CONS-SRYYM-200609 )
             EVALUATE  LNK-SYUBETUGET-HONKZKKBN ALSO
                                    LNK-SYUBETUGET-NYUGAIKBN  ALSO
                       LNK-SYUBETUGET-IHO  ALSO
                                    LNK-SYUBETUGET-KOKUHO     ALSO
                       LNK-SYUBETUGET-RJN  ALSO
                                    LNK-SYUBETUGET-KOHI-CNT
      *        ÊÝ¸±Ã±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
      *ÊÝ¸±¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸øÈñÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212     TO  LNK-SYUBETUGET-RECESYUBETU
      *£²¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222     TO  LNK-SYUBETUGET-RECESYUBETU
      *£³¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232     TO  LNK-SYUBETUGET-RECESYUBETU
      *Ï·¿ÍÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
      *Ï·¿Í¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
      *Ï·¿Í¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼ÔÃ±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1417     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1418     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1419     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1410     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1427     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1428     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1429     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1420     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1437     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1438     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1439     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1430     TO  LNK-SYUBETUGET-RECESYUBETU
             END-EVALUATE
           END-IF
      *     
      *    ÆÃÊÌÎÅÍÜÈñ¤Î¤È¤­¤Î¥ì¥»¥×¥È¼ïÊÌ¤òÊÔ½¸
           IF      LNK-SYUBETUGET-HKNNUM        =   "068"   OR  "069"
               MOVE    LNK-SYUBETUGET-RECESYUBETU
                                        TO  LNK-SYUBETUGET-RECESYUBETU-X 
               MOVE    6666             TO  LNK-SYUBETUGET-RECESYUBETU    
           END-IF
      *??
      *     DISPLAY "SYUBETU   : " LNK-SYUBETUGET-RECESYUBETU
      *     DISPLAY "SYUBETU-X : " LNK-SYUBETUGET-RECESYUBETU-X
      *??          
           .
        200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT. 
