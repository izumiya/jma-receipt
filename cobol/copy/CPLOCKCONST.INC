      ******************************************************************
      *    CPLOCKCONST.INC
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORC-LOCKCONST.
           02  ORC-LOCKCONST-T.
             03  ORC-LOCKCONST-T01.
               05  ORC-LOCKCONST-WINDOW-T01   PIC  X(10)   
                   VALUE   "M         " .
               05  ORC-LOCKCONST-LD-T01  PIC  X(20)   
                   VALUE   "ORCA00              " .
               05  ORC-LOCKCONST-NAME-T01   PIC  X(40)  
                   VALUE   "メニュー" .
             03  ORC-LOCKCONST-T02.
               05  ORC-LOCKCONST-WINDOW-T02   PIC  X(10)   
                   VALUE   "XA        " .
               05  ORC-LOCKCONST-LD-T02  PIC  X(20)   
                   VALUE   "ORCA01" .
               05  ORC-LOCKCONST-NAME-T02   PIC  X(40)  
                   VALUE   "再印刷" .
             03  ORC-LOCKCONST-T03.
               05  ORC-LOCKCONST-WINDOW-T03   PIC  X(10)   
                   VALUE   "XB       " .
               05  ORC-LOCKCONST-LD-T03  PIC  X(20)   
                   VALUE   "ORCA02" .
               05  ORC-LOCKCONST-NAME-T03   PIC  X(40)  
                   VALUE   "情報削除" .
             03  ORC-LOCKCONST-T04.
               05  ORC-LOCKCONST-WINDOW-T04   PIC  X(10)   
                   VALUE   "U" .
               05  ORC-LOCKCONST-LD-T04  PIC  X(20)   
                   VALUE   "ORCA11" .
               05  ORC-LOCKCONST-NAME-T04   PIC  X(40)  
                   VALUE   "受付" .
             03  ORC-LOCKCONST-T05.
               05  ORC-LOCKCONST-WINDOW-T05   PIC  X(10)   
                   VALUE   "P" .
               05  ORC-LOCKCONST-LD-T05  PIC  X(20)   
                   VALUE   "ORCA12" .
               05  ORC-LOCKCONST-NAME-T05   PIC  X(40)  
                   VALUE   "患者登録" .
             03  ORC-LOCKCONST-T06.
               05  ORC-LOCKCONST-WINDOW-T06   PIC  X(10)   
                   VALUE   "Q" .
               05  ORC-LOCKCONST-LD-T06  PIC  X(20)   
                   VALUE   "ORCA13" .
               05  ORC-LOCKCONST-NAME-T06   PIC  X(40)  
                   VALUE   "照会" .
             03  ORC-LOCKCONST-T07.
               05  ORC-LOCKCONST-WINDOW-T07   PIC  X(10)   
                   VALUE   "Y" .
               05  ORC-LOCKCONST-LD-T07  PIC  X(20)   
                   VALUE   "ORCA14" .
               05  ORC-LOCKCONST-NAME-T07   PIC  X(40)  
                   VALUE   "予約" .
             03  ORC-LOCKCONST-T08.
               05  ORC-LOCKCONST-WINDOW-T08   PIC  X(10)   
                   VALUE   "U01" .
               05  ORC-LOCKCONST-LD-T08  PIC  X(20)
                   VALUE   "ORCA18" .
               05  ORC-LOCKCONST-NAME-T08   PIC  X(40)  
                   VALUE   "受付一覧" .
             03  ORC-LOCKCONST-T09.
               05  ORC-LOCKCONST-WINDOW-T09   PIC  X(10)
                   VALUE   "P97" .
               05  ORC-LOCKCONST-LD-T09  PIC  X(20)
                   VALUE   "ORCA19" .
               05  ORC-LOCKCONST-NAME-T09   PIC  X(40)  
                   VALUE   "氏名検索" .
             03  ORC-LOCKCONST-T10.
               05  ORC-LOCKCONST-WINDOW-T10   PIC  X(10)
                   VALUE   "K" .
               05  ORC-LOCKCONST-LD-T10  PIC  X(20)
                   VALUE   "ORCA21" .
               05  ORC-LOCKCONST-NAME-T10   PIC  X(40)  
                   VALUE   "診療行為入力" .
             03  ORC-LOCKCONST-T11.
               05  ORC-LOCKCONST-WINDOW-T11   PIC  X(10)
                   VALUE   "C" .
               05  ORC-LOCKCONST-LD-T11  PIC  X(20)
                   VALUE   "ORCA22" .
               05  ORC-LOCKCONST-NAME-T11   PIC  X(40)  
                   VALUE   "病名" .
             03  ORC-LOCKCONST-T12.
               05  ORC-LOCKCONST-WINDOW-T12   PIC  X(10)
                   VALUE   "S" .
               05  ORC-LOCKCONST-LD-T12  PIC  X(20)
                   VALUE   "ORCA24" .
               05  ORC-LOCKCONST-NAME-T12   PIC  X(40)  
                   VALUE   "収納" .
             03  ORC-LOCKCONST-T13.
               05  ORC-LOCKCONST-WINDOW-T13   PIC  X(10)
                   VALUE   "J" .
               05  ORC-LOCKCONST-LD-T13  PIC  X(20)
                   VALUE   "ORCA24" .
               05  ORC-LOCKCONST-NAME-T13   PIC  X(40)  
                   VALUE   "会計照会" .
             03  ORC-LOCKCONST-T14.
               05  ORC-LOCKCONST-WINDOW-T14   PIC  X(10)
                   VALUE   "D" .
               05  ORC-LOCKCONST-LD-T14  PIC  X(20)
                   VALUE   "ORCA41" .
               05  ORC-LOCKCONST-NAME-T14   PIC  X(40)  
                   VALUE   "データチェック" .
             03  ORC-LOCKCONST-T15.
               05  ORC-LOCKCONST-WINDOW-T15   PIC  X(10)
                   VALUE   "R" .
               05  ORC-LOCKCONST-LD-T15  PIC  X(20)
                   VALUE   "ORCA42" .
               05  ORC-LOCKCONST-NAME-T15   PIC  X(40)  
                   VALUE   "明細書" .
             03  ORC-LOCKCONST-T16.
               05  ORC-LOCKCONST-WINDOW-T16   PIC  X(10)
                   VALUE   "H" .
               05  ORC-LOCKCONST-LD-T16  PIC  X(20)
                   VALUE   "ORCA43" .
               05  ORC-LOCKCONST-NAME-T16   PIC  X(40)  
                   VALUE   "請求管理" .
             03  ORC-LOCKCONST-T17.
               05  ORC-LOCKCONST-WINDOW-T17   PIC  X(10)
                   VALUE   "T" .
               05  ORC-LOCKCONST-LD-T17  PIC  X(20)
                   VALUE   "ORCA44" .
               05  ORC-LOCKCONST-NAME-T17   PIC  X(40)  
                   VALUE   "総括・公費請求書" .
             03  ORC-LOCKCONST-T18.
               05  ORC-LOCKCONST-WINDOW-T18   PIC  X(10)
                   VALUE   "L" .
               05  ORC-LOCKCONST-LD-T18  PIC  X(20)
                   VALUE   "ORCA51" .
               05  ORC-LOCKCONST-NAME-T18   PIC  X(40)  
                   VALUE   "日次統計" .
             03  ORC-LOCKCONST-T19.
               05  ORC-LOCKCONST-WINDOW-T19   PIC  X(10)
                   VALUE   "G" .
               05  ORC-LOCKCONST-LD-T19  PIC  X(20)
                   VALUE   "ORCA71" .
               05  ORC-LOCKCONST-NAME-T19   PIC  X(40)  
                   VALUE   "月次統計" .
             03  ORC-LOCKCONST-T20.
               05  ORC-LOCKCONST-WINDOW-T20   PIC  X(10)
                   VALUE   "E" .
               05  ORC-LOCKCONST-LD-T20  PIC  X(20)
                   VALUE   "ORCA71" .
               05  ORC-LOCKCONST-NAME-T20   PIC  X(40)  
                   VALUE   "データ出力" .
             03  ORC-LOCKCONST-T21.
               05  ORC-LOCKCONST-WINDOW-T21   PIC  X(10)
                   VALUE   "F" .
               05  ORC-LOCKCONST-LD-T21  PIC  X(20)
                   VALUE   "ORCA82" .
               05  ORC-LOCKCONST-NAME-T21   PIC  X(40)  
                   VALUE   "外部媒体" .
             03  ORC-LOCKCONST-T22.
               05  ORC-LOCKCONST-WINDOW-T22   PIC  X(10)
                   VALUE   "W" .
               05  ORC-LOCKCONST-LD-T22  PIC  X(20)
                   VALUE   "ORCA101" .
               05  ORC-LOCKCONST-NAME-T22   PIC  X(40)  
                   VALUE   "システム管理" .
             03  ORC-LOCKCONST-T23.
               05  ORC-LOCKCONST-WINDOW-T23   PIC  X(10)
                   VALUE   "Z" .
               05  ORC-LOCKCONST-LD-T23  PIC  X(20)
                   VALUE   "ORCA102" .
               05  ORC-LOCKCONST-NAME-T23   PIC  X(40)  
                   VALUE   "点数マスタ" .
             03  ORC-LOCKCONST-T24.
               05  ORC-LOCKCONST-WINDOW-T24   PIC  X(10)
                   VALUE   "X" .
               05  ORC-LOCKCONST-LD-T24  PIC  X(20)
                   VALUE   "ORCA103" .
               05  ORC-LOCKCONST-NAME-T24   PIC  X(40)  
                   VALUE   "チェックマスタ" .
             03  ORC-LOCKCONST-T25.
               05  ORC-LOCKCONST-WINDOW-T25   PIC  X(10)
                   VALUE   "V" .
               05  ORC-LOCKCONST-LD-T25  PIC  X(20)
                   VALUE   "ORCA104" .
               05  ORC-LOCKCONST-NAME-T25   PIC  X(40)  
                   VALUE   "保険番号マスタ" .
             03  ORC-LOCKCONST-T26.
               05  ORC-LOCKCONST-WINDOW-T26   PIC  X(10)
                   VALUE   "N" .
               05  ORC-LOCKCONST-LD-T26  PIC  X(20)
                   VALUE   "ORCA105" .
               05  ORC-LOCKCONST-NAME-T26   PIC  X(40)  
                   VALUE   "保険者マスタ" .
             03  ORC-LOCKCONST-T27.
               05  ORC-LOCKCONST-WINDOW-T27   PIC  X(10)
                   VALUE   "N11" .
               05  ORC-LOCKCONST-LD-T27  PIC  X(20)
                   VALUE   "ORCA106" .
               05  ORC-LOCKCONST-NAME-T27   PIC  X(40)  
                   VALUE   "人名辞書マスタ" .
             03  ORC-LOCKCONST-T28.
               05  ORC-LOCKCONST-WINDOW-T28   PIC  X(10)
                   VALUE   "I0" .
               05  ORC-LOCKCONST-LD-T28  PIC  X(20)
                   VALUE   "ORCA31" .
               05  ORC-LOCKCONST-NAME-T28   PIC  X(40)  
                   VALUE   "入退院登録" .
             03  ORC-LOCKCONST-T29.
               05  ORC-LOCKCONST-WINDOW-T29   PIC  X(10)
                   VALUE   "I4" .
               05  ORC-LOCKCONST-LD-T29  PIC  X(20)
                   VALUE   "ORCA32" .
               05  ORC-LOCKCONST-NAME-T29   PIC  X(40)  
                   VALUE   "入院会計照会" .
             03  ORC-LOCKCONST-T30.
               05  ORC-LOCKCONST-WINDOW-T30   PIC  X(10)
                   VALUE   "I5" .
               05  ORC-LOCKCONST-LD-T30  PIC  X(20)
                   VALUE   "ORCA34" .
               05  ORC-LOCKCONST-NAME-T30   PIC  X(40)  
                   VALUE   "退院時仮計算" .
             03  ORC-LOCKCONST-T31.
               05  ORC-LOCKCONST-WINDOW-T31   PIC  X(10)
                   VALUE   "O" .
               05  ORC-LOCKCONST-LD-T31  PIC  X(20)
                   VALUE   "ORCA33" .
               05  ORC-LOCKCONST-NAME-T31   PIC  X(40)  
                   VALUE   "定期請求" .
           02  ORC-LOCKCONST-R  REDEFINES   ORC-LOCKCONST-T.
             03  ORC-LOCKCONST-TX           OCCURS  31 
                                            INDEXED  TBLLOCK-IDX.
               05  ORC-LOCKCONST-WINDOW       PIC  X(10).
               05  ORC-LOCKCONST-LD      PIC  X(20).
               05  ORC-LOCKCONST-NAME       PIC  X(40).
           02  ORC-LOCKCONST-COUNT       PIC  S9(09)  
                   VALUE   31.
