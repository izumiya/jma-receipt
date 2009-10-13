        01  HCN64.
            02  HCN64-TEISYUTUSAKI  PIC X(90).
            02  HCN64-HOSPADRS  PIC X(100).
            02  HCN64-KAISETUNAME   PIC X(80).
            02  HCN64-HOSPTEL   PIC X(15).
            02  HCN64-HOSPCD    PIC X(7).
            02  HCN64-HOSPNAME  PIC X(100).
            02  HCN64-SANKANUM  PIC X(10).
            02  HCN64-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN64-SKYYM.
              03  HCN64-SKYY  PIC X(2).
              03  HCN64-SKYM  PIC X(2).
            02  HCN64-RECEYMD.
              03  HCN64-PRTY  PIC X(2).
              03  HCN64-PRTM  PIC X(2).
              03  HCN64-PRTD  PIC X(2).
            02  HCN64-DATAKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN64-DATACNT   PIC X(2).
            02  HCN64-BIKOU PIC X(100).
