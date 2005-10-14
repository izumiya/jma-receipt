-- 統計覚書テーブル
--
-- 保険請求確認リスト
--
-- 作成日 2005/09/30


DELETE from tbl_toukeimemo where pgid like 'A%' and kanricd = '0000' and kbncd = '000' and styukymd = '00000000' and edyukymd = '99999999';
DELETE from tbl_toukeimemo where pgid =    'ORCBG014' and kanricd = '0000' and kbncd = '000' and styukymd = '00000000' and edyukymd = '99999999';

COPY "tbl_toukeimemo" FROM stdin;
ORCBG014	0000	000     	00000000	99999999	請求年月　　　請求年月を入力します\n提出先区分　　０：全部　１：社保　　　２：国保\n保険者番号　　保険者番号を入力します（国保のみ）\n処理区分　　　０：全部　１：通常請求　２：返戻分　３：月遅れ分　４：請求しない\n入外区分　　　０：全部　１：入院分　　２：外来分\n在総診区分　　０：全部　１：一般　　　２：在総診および在医総\nレセプト種別（レセプト種別２桁＋詳細区分１桁）\n　　　　社保　０１：医保（７０歳以上９割）　　国保　１０：一般（７０歳以上９割）\n　　　　　　　０２：医保（７０歳以上８割）　　　　　１１：一般（７０歳以上８割）\n　　　　　　　０３：医保（本人）　　　　　　　　　　１２：一般被保険者\n　　　　　　　０４：医保（家族）　　　　　　　　　　１３：一般（３歳未満）\n　　　　　　　０５：医保（３歳未満）　　　　　　　　１４：退職（本人）\n　　　　　　　　　　詳細区分（０１〜０５）　　　　　１５：退職（７０歳以上９割）\n　　　　　　　　　　　　　１：医保と公費の併用　　　１６：退職（７０歳以上８割）\n　　　　　　　　　　　　　２：医保単独（政）　　　　１７：退職（被扶養者）\n　　　　　　　　　　　　　３：医保単独（船）　　　　１８：退職（３歳未満）\n　　　　　　　　　　　　　４：医保単独（日）　　　　１９：老人（９割）\n　　　　　　　　　　　　　５：医保単独（日特）　　　２０：老人（８割）\n　　　　　　　　　　　　　６：医保単独（共）　　　　\n　　　　　　　　　　　　　７：医保単独（組）\n　　　　　　　　　　　　　８：医保単独（自）\n　　　　　　　　　　　　　９：医保単独（退）\n　　　　　　　０６：老人（９割）\n　　　　　　　０７：老人（８割）\n　　　　　　　　　　詳細区分（０６〜０７）\n　　　　　　　　　　　　　１：老人と公費の併用\n　　　　　　　　　　　　　２：老人単独\n　　　　　　　０８：公費と公費の併用\n　　　　　　　０９：公費単独\n並び順　　　　０：カナ氏名順　１：患者番号順　２：点数順\n			20051014	20051014	130945
A00000D600	0000	000     	00000000	99999999	集計日    集計日を入力します。			20050217	20050217	144653
A00000L100	0000	000     	00000000	99999999	出力帳票          出力帳票を指定します。\n                          空白：受診\n                          ０：初診\n                          １：在院\n                          ２：入院\n                          ３：退院\n                          ４：未納\n                          ５：前納\n                          ６：病名（※病名、診療行為パラメタで、病名コード入力必須。）\n                          ７：診療行為（※病名、診療行為パラメタで、診療科コード入力必須。）       \n\n開始日              対象期間の開始年月日を入力します。\n\n終了日              対象期間の終了年月日を入力します。\n\n診療科コード   診療科コードを入力します。\n                        空白：全科\n                        ＊＊（２桁の診療科コード）：指定科のみ\n\n医師コード       職員コードを入力します。\n                        空白：全医師 \n                        ＊＊＊＊＊＊＊（５桁の職員コード）：指定医師\n     \n病名、診療行為        出力帳票パラメタで\n                                       「６：病名」を選択した場合、\n                                             ＊＊＊＊＊＊＊＊（８桁の病名コード）：病名指定\n                                       「７：診療科」を選択した場合、\n                                             ＊＊＊＊＊＊＊＊＊（９桁の診療科コード）：診療科指定\n                                 を入力します。                 \n\n印刷順序           空白：患者番号順で全患者を印字\n                         ０：カナ氏名順で全患者を印字\n                         １：保険者番号順で全患者を印字\n                         ２：出力帳票パラメタで\n                                   「空白：受診」「０：初診」を選択の場合、該当患者を頭書登録日順で印字\n                                   「１：在院」「２：入院」を選択の場合、該当患者を入院日順で印字\n                                   「３：退院」を選択の場合、該当患者を退院日順で印字\n                                   「４：未納」「５：前納」を選択の場合、該当患者を頭書登録日／患者来院日順で印字\n                                   「６：病名」「７：診療行為」を選択の場合、該当患者を最終来院日順で印字\n                         ３：金額（高額→低額）順で全患者を印字			20050222	20050222	092835
A00000A010	0000	000     	00000000	99999999	診療分類指定    診療分類を指定します。\n                           空白：全診療区分（９９は除く）\n                           ０：全診療区分（９９は除く）\n                           １０：初診、再診、指導、在宅\n                           ２１：内服薬\n                           ２２：注射薬\n                           ２３：外用薬\n                           ２９：投薬\n                           ３０：注射\n                           ４０：処置\n                           ５０：手術、麻酔\n                           ６０：検査\n                           ７０：画像診断\n                           ７１：フィルム\n                           ８０：理学療法\n                           ９０：入院\n                           ９１：機材\n                           ９２：その他材料\n                           ９５：自費（税無）\n                           ９６：自費（税有）\n                           ９９：用法、画像診断部位、コメント\n\n診療年月     診療年月を入力します。  \n\n入外区分    空白：入院・外来\n                  １：入院\n                  ２：外来\n                  ３：入外合計\n\n印字順序    印字順序を指定します。\n                    空白：回数（降順）  ※0回の診療行為は未印字\n                    １：回数×点数（降順）  ※0回の診療行為は未印字\n                    ２：カナ（昇順）  ※0回の診療行為は未印字\n                    ８：診療行為コード（昇順）  ※0回の診療行為のみを印字\n                    ９：カナ（昇順）  ※0回の診療行為のみを印字			20050222	20050222	093250
A00000L200	0000	000     	00000000	99999999	対象年月      対象年月を入力します。\n\n入外区分      空白：入院・外来\n                    １：入院\n                    ２：外来\n\n保険者番号   保険者番号を入力します。\n                       空白：未入力で登録されている患者\n                      １２３４５６７８：指定された番号の患者\n                      ＠：番号が登録されている患者全て\n                      ２７＊＊＊＊＊＊：２７老人の患者\n                                “＊”は同じ桁数の全数字、文字（全角）を対象\n\n公費負担者番号    公費負担者番号を入力します。\n                              空白：未入力で登録されている患者\n                              １２３４５６７８：指定された番号の患者\n                              ＠：番号が登録されている患者全て\n                              ２７＊＊＊＊＊＊：２７老人の患者\n                                        “＊”は同じ桁数の全数字、文字（全角）を対象\n\n本人・家族      空白：本人・家族\n                        １：本人\n                        ２：家族\n              \n診療科コード    診療科コードを入力します。\n\n印字順序    印字順序を指定します。\n                    空白：患者番号順に印字\n                    ０：カナ氏名順に印字\n                    １：保険者番号順に印字\n                    ２：公費負担者番号順に印字			20050222	20050222	093408
A00000L300	0000	000     	00000000	99999999	診療年月          診療年月を入力します。\n                        \n診療科コード   診療科コードを入力します。\n                        空白：全て\n                        ＊＊（２桁の診療科コード）：指定科のみ			20050222	20050222	093744
A00000KOHS	0000	000     	00000000	99999999	請求年月    請求年月を入力します。\n			20050222	20050222	093849
A21011A05	0000	000     	00000000	99999999	出力区分    出力区分を選択します。\n                  １：保留\n                  ２：月遅れ\n                  ３：再請求\n                  ９：全て\n\n請求年月     請求年月を入力します。			20050222	20050222	094020
A00000D501	0000	000     	00000000	99999999	集計日    集計日を入力します。\n\n\n※受診者数は、同日再診をカウントしません。（同日再診があった場合でも、患者数は１人）\n※再診件数は、同日再診も再診としてカウントします。			20050225	20050225	163236
A00000D500	0000	000     	00000000	99999999	集計日    集計日を入力します。\n			20050225	20050225	163520
A00000D601	0000	000     	00000000	99999999	集計日    集計日を入力します。\n\n\n※受診者数は、同日再診をカウントしません。（同日再診があった場合でも、患者数は１人）\n※再診件数は、同日再診も再診としてカウントします。			20050225	20050225	163753
A00000M500	0000	000     	00000000	99999999	集計年月           集計年月を入力します。\n\n入外区分           空白：全て／１：入院／２：外来\n\n診療科コード    診療科コードを入力します。\n                            空白：全診療科\n                            ００：全科合計\n                            ＊＊（２桁の診療科コード）：指定科\n\n\n※受診者数は、同日再診をカウントしません。（同日再診があった場合でも、患者数は１人）\n※再診件数は、同日再診も再診としてカウントします。			20050225	20050225	163839
A00000M501	0000	000     	00000000	99999999	集計年月    集計年月を入力します。\n\n\n※受診者数は、同日再診をカウントしません。（同日再診があった場合でも、患者数は１人）\n※再診件数は、同日再診も再診としてカウントします。			20050225	20050225	163941
A00000M700	0000	000     	00000000	99999999	集計年月    集計年月を入力します。\n\n\n※受診者数は、同日再診をカウントしません。（同日再診があった場合でも、患者数は１人）\n※再診件数は、同日再診も再診としてカウントします。			20050225	20050225	164034
A01014M01	0000	000     	00000000	99999999	対象年月    対象年月を入力します。\n\n\n※再診件数は、同日再診も再診としてカウントします。			20050225	20050225	165237
A00000C100	0000	000     	00000000	99999999	出力帳票    出力帳票を指定します。\n                  空白：診療科別／\n                  ０：医師別／１：年齢別／２：地区別／３：保険別／４：病棟別\n\n集計区分    空白：日集計（日報）\n                  ０：月集計（月報）\n\n開始日        対象期間の開始年月日を入力します。\n\n終了日        対象期間の終了年月日を入力します。\n                  \n入外区分    １：入院／２：外来\n\n\n※再診件数は、同日再診も再診としてカウントします。			20050908	20050908	142001
A00000D100	0000	000     	00000000	99999999	出力帳票    出力帳票を指定します。\n                    空白：日報／０：月報／\n                    １：診療科別／２：医師別／３：年齢別／４：地区別／５：保険別\n\n開始日        対象期間の開始日を入力します。\n\n終了日        対象期間の終了日を入力します。      \n\n入外区分    １：入院／２：外来\n\n診療科コード    診療科コードを入力します。\n                           空白：全科\n                           ＊＊（２桁の診療科コード）：指定科\n\n医師コード       職員コードを入力します。\n                           空白：全医師\n                           ＊＊＊＊＊＊（５桁の職員コード）：指定医師\n\n\n※再診件数は、同日再診をカウントしません。			20050908	20050908	142039
A00000S100	0000	000     	00000000	99999999	出力帳票    出力帳票を指定します。\n                    空白：全て\n                    ０：社保\n                    １：国保\n                    ２：総括表\n\n対象年月    対象期間の年月を入力します。\n                  \n入外区分    空白：入院・外来\n                  １：入院\n                  ２：外来			20050908	20050908	142100
A00000K100	0000	000     	00000000	99999999	出力帳票        出力帳票を指定します。\n                      空白：診療科別／\n                      ０：医師別／１：年齢別／２：地区別／３：保険別／４：病棟別\n\n対象年月        対象年月を入力します。\n\n入外区分         １：入院／２：外来\n\n保険適用外      空白：含まない\n                       ０：含む			20050908	20050908	142125
\.

