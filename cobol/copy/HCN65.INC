        01  HCN65.
            02  HCN65-TEISYUTUSAKI  PIC X(90).
            02  HCN65-HOSPADRS  PIC X(100).
            02  HCN65-KAISETUNAME   PIC X(80).
            02  HCN65-HOSPTEL   PIC X(15).
            02  HCN65-HOSPCD    PIC X(7).
            02  HCN65-HOSPNAME  PIC X(100).
            02  HCN65-SANKANUM  PIC X(10).
            02  HCN65-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN65-SKYYM.
              03  HCN65-SKYY  PIC X(2).
              03  HCN65-SKYM  PIC X(2).
            02  HCN65-RECEYMD.
              03  HCN65-PRTY  PIC X(2).
              03  HCN65-PRTM  PIC X(2).
              03  HCN65-PRTD  PIC X(2).
            02  HCN65-PAGE  PIC X(10).
            02  HCN65-KENSU PIC X(10).
            02  HCN65-BIRTHCNT  PIC X(10).
            02  HCN65-DAIRI-MONEY   PIC X(10).
            02  HCN65-BIKOU PIC X(100).
