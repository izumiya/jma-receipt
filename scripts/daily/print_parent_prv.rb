#!/usr/bin/ruby # print_parent2.rb のプレビュー用対応 
# 文字列の改行区切りで配列にして、それを引数のプログラムに渡す処理 
# (2002/03/07 竹田氏追加依頼) 
# 引数指定のものを、一部dataファイルに含めるように変更(2002/04/16 :竹田氏依頼 :曳野対応) 
# 
# red2ps対応済み 
# site-lib区分対応 (2002/10/16) 
#   woody版では、site-libのdia/redファイルも見なければならないので、それに関する対応 
#   基準ディレクトリを追加することで対応 
#   dataファイルフォーマットを修正(LP名のあとに、site区分を追加) 
# ディレクトリ位置対応 (2002/11/13)
#   コマンドライン引数3〜6のディレクトリ位置情報を使用するように修正
# プレビュー対応
# (2003/01/20 ) 引渡しファイルのレイアウト変更
# postscript出力対応 (2003/03/03)
# (2004/12/05 ) 印刷出力停止機能追加
# (2007/05/09 ) オンライン再印刷処理追加(UUID)
# (2007/05/31 ) グループ診療対応(hospnum追加)
# (2007/06/19 ) 労災枠なし対応
# (2007/12/27 ) patch-lib 対応
# (2008/07/30 ) 労災、自賠枠なし対応
# (2008/11/12 ) レイアウトオプション対応
# (2011/11/14 ) 労災枠有り対応
# (2012/04/11 ) 改正対応
#

# ※複数のプロセスの実行はできない


# 引数指定方法
# 引数1  = 一時ファイル名
# 引数2  = 起動プログラム
# 引数3  = 日医標準レセプトformディレクトリ
# 引数4  = 日医標準レセプトrecordディレクトリ
# 引数5  = 日医標準レセプトsite固有formディレクトリ
# 引数6  = 日医標準レセプトsite固有recordディレクトリ
# 引数7  = dataファイル
# 引数8  = 起動用スクリプト名1
# 引数9  = 起動用スクリプト名2
# 引数10 = 日医標準レセプトpatch固有formディレクトリ

# dataファイルフォーマット
# +------------------+------------+---------------+---------------+---------------+---------------+-----------------------+
# |ファイル名(30Byte)|LP名(20Byte)|offset-x(5byte)|offset-y(5byte)|site区分(1Byte)|出力区分(1Byte)|出力先フォルダ(165byte)|
# +------------------+------------+---------------+---------------+---------------+---------------+-----------------------+
# +------------------+-----------+----------------+-----------------+-------------+------------+--------------------------+
# |tbl_key情報(8byte)|連番(4byte)|グループ(14byte)|処理内連番(4byte)|ページ(5byte)|UUID(36byte)|帳票データ(Max20000バイト)|
# +------------------+-----------+----------------+-----------------+-------------+------------+--------------------------+
# ファイル名に、拡張子とディレクトリ名を付加したものを子プロセスの引数に渡す
#    site区分
#      1 = orca標準
#      2 = site-libのカスタマイズ帳票
#      3 = Postscriptファイルの出力(印刷イメージ表示対応)
#
# ※LP名は空白の場合、子プロセスの引数を省略する




#	puts '<temp>        = 一時ファイル名'

# ============================================================

# ヘルプ表示

help_flg = 0

case	ARGV[0]
when	nil
	help_flg = 1
when	'-?', '/?', '-h', '/h', '--help'
	help_flg = 1
else
	help_flg = 0
end

if (help_flg != 0)
	puts 'このスクリプトは、改行で区切って、引数のプログラムに渡すプログラムです'
	puts $0 + ' <temp> <exec> <form> <record> <site-form> <site-record> <data>'
	puts '<temp>        = 一時ファイル名'
	puts '<exec>        = 起動プログラム'
	puts '<form>        = 日医標準レセプトformディレクトリ'
	puts '<record>      = 日医標準レセプトrecordディレクトリ'
	puts '<site-form>   = 日医標準レセプトsite固有formディレクトリ'
	puts '<site-record> = 日医標準レセプトsite固有recordディレクトリ'
	puts '<data>        = dataファイル'
	exit 0
end


std_form = ''; std_record = ''; site_form = ''; site_record = ''; patch_form = ''

# 定数の指定(最後に、かならず「/」を付けた形にしてください)
std_form = ARGV[2] + '/'   		# 日レセ標準formディレクトリ
std_record = ARGV[3] + '/' 		# 日レセ標準recordディレクトリ
site_form = ARGV[4] + '/'  		# 日レセsite固有formディレクトリ
site_record = ARGV[5] + '/'		# 日レセsite固有recordディレクトリ
patch_form = ARGV[9] + '/'  		# 日レセsite固有formディレクトリ


# .redからpsファイルに変換する際に使用するプログラム
RED_EXEC = 'red2ps'
# .redファイルで、LP名を省略された際に使用するプリンタ名
DEFAULT_LP = 'lp1'



# ============================================================
# 領域の初期化

# ファイル名領域
temp_file = ''; exec_file = ''; dia_file = ''; def_file = ''; data_file = ''; lp_name = ''; offset_x = ''; offset_y = ''
temp_file2 = ''; exec_file2 = ''; dia_file2 = ''; def_file2 = ''; data_file2 = ''; lp_name2 = ''
site_flag = ''; base_dir = ''
#---- (2003/01/20 ) start
prt_flg = '';psfile_name = '';psfile_name_folder = '';exec_file_prv = '';exec_file_prv = ''
#---- (2003/01/20 ) end
# 文字列領域
word1 = ''; word2 = []; word3 = ''
# exec用文字列領域
w_exec = ''
# fork用領域
pid = []
# カウンタ用領域
li_cnt1 = 0
# 文字列長用領域
d2_len = 0


# ============================================================
# 引数のセット

# 引数の数チェック
for li_cnt1 in 1..9 do
	if ARGV[li_cnt1] == nil
		puts '引数の数が足りません'
		exit 1
	end
end


# 引数の領域から、文字列の取得
#temp_file = String(ARGV[0])		# 一時ファイル
exec_file = String(ARGV[1])		# 起動プログラムファイル
data_file = String(ARGV[6])		# dataファイル名
#---- (2003/07/03 ) add start
exec_file_prv = String(ARGV[7])		# 起動プログラムファイル
RED_EXECPS = String(ARGV[8])		# 起動プログラムファイル
#---- (2003/07/03 ) add end  


# 一時ファイルのファイル名を生成
temp_file = data_file + '.tmp'


# ============================================================
# 関数定義部


# ファイル名の前後の「'」「"」を取り除く処理
def filename_change(name)
	filename_change = name.gsub /\A[\'\"]([\s\S]*?)[\'\"][\s]*?\z/, '\1'
end


# ファイル名に空白がある場合、前後に「"」をセットする処理
def filename_unchange(name)
	if name =~ / /
		filename_unchange = "\"" + name + "\""
	else
		filename_unchange = name
	end
end


# slashを付ける
def add_slash(str)
	if str =~ /[^\/]$/
		return (str += '/')
	else
		return str
	end
end

# ファイル名から医療機関番号を取得する
def get_hospnum(str)
  b = str.split(/\//)
  return b[b.size-1][0,2].strip
#  return str.scan(/\/\S\S/).last.sub(/\//, "")
end



# ============================================================
# 引数の加工処理

# 関数を呼び出して、前後の「'」「"」を削除
temp_file = filename_change(temp_file)
exec_file = filename_change(exec_file)
data_file = filename_change(data_file)
#---- (2003/07/03 ) add start
exec_file_prv = filename_change(exec_file_prv)
#---- (2003/07/03 ) add end  
#---- (2007/05/30 ) add start
hospnum = get_hospnum(data_file)
puts "医療機関番号   = [" + hospnum + "]"
#---- (2007/05/30 ) add end


# 逆に、間に空白がある文字列に、「"」を追加したものを用意
temp_file2 = filename_unchange(temp_file)
#exec_file2 = filename_unchange(exec_file_prv)
data_file2 = filename_unchange(data_file)


# コマンド引数のformやrecordのディレクトリ名に「/」がない場合、「/」を付ける処理
std_form = add_slash(std_form)
std_record = add_slash(std_record)
site_form = add_slash(site_form)
site_record = add_slash(site_record)
patch_form = add_slash(patch_form)




# デバッグ用の表示
puts "一時ファイル   = [" + temp_file + "]"
puts "起動プログラム = [" + exec_file + "]"
puts "dataファイル   = [" + data_file + "]"


# ============================================================

# 一時ファイル存在チェック
begin
	if nil != File.size(temp_file)
		puts '一時ファイルが存在するので削除します'
		puts 'ファイル名 = [' + temp_file + ']'
		# 一時ファイルの削除
		File.delete(temp_file)
		puts '削除しました'
#		puts '一時ファイルが存在するため、書き込みできません'
#		puts 'ファイル名 = [' + temp_file + ']'
#		exit 2
	end
rescue
# 例外が発生すれば、一時ファイルが存在しないので、ＯＫ
end


# dataファイルの存在チェック
begin
	if nil == File.size(data_file)
		puts 'dataファイルが存在しません'
		puts 'ファイル名 = [' + data_file + ']'
		exit 3
	end
rescue
# 例外が発生すれば、dataファイルが存在しない
	puts 'dataファイルが存在しません'
	puts 'ファイル名 = [' + data_file + ']'
	exit 3
end


# ============================================================

# dataファイルの読み込み
# 入力ファイルの読み込み処理
open(data_file, "r") do |fp|
	word1 = fp.read
end

# LAYEROPTION 設定


# -----------------------------------

# 改行区切り処理
#word2 = word1.split(/\s*\n\s*/)
word2 = word1.split(/\n/)


# -----------------------------------

# 印刷停止制御用ファイル生成
stop_file = ''
stop_file_1 = ''
stop_file_2 = ''


li_cnt1 = 0

# 実行処理
	puts '[start ' + `date` + ']'
word2.each do |d2|
  d2.scan(/MonpeLayerIn(.*)MonpeLayerOut/)
  LAYEROPTION = $1.to_s
#  puts  'layer = '
#  puts  LAYEROPTION
# 印刷処理実行可能判定
stop_file_1 = d2[227, 8].strip		# key情報の取得
stop_file_2 = d2[239, 14].strip		# group情報の取得
puts stop_file_1
puts stop_file_2
stop_file = '/tmp/' + stop_file_1 + stop_file_2 + '.tmp'
#stop_file = '/tmp/take.stop'
puts '[' + stop_file + ']'
if FileTest::exists?(stop_file)
        puts '印刷処理停止'
        File.delete(stop_file)
        if FileTest::exists?(temp_file)
           puts 'tempファイルを削除します'
           File.delete(temp_file)
        end
	exit 3
else
   puts 'no file'
end
# デバッグ用コード
#	puts '[' + d2 + ']'
	li_cnt1 = li_cnt1 + 1
	d2_len = d2.size	# 文字列の長さの取得


	# 先頭30バイトは、.diaと.defファイルのファイル名部分
	# あるいは、.redファイルの拡張子を含むファイル名
	dia_file = ''
	def_file = ''
	red_file = ''
	ls_w1 = ''	# 一時用文字列領域
	ls_w1 = d2[0, 30].strip	# ファイル名を後ろの空白を除いて取得
	site_flag = d2[60, 1].strip		# site固有判定フラグの取得
#   diaファイルを強制的にredに変更 by  take siteも見る必要がある
	if site_flag == '1'
#		if ls_w1 == 'HC06V01' || 'HC06V02'
		if ls_w1 == 'HC06V01'
			ls_w1 = ls_w1 + '.red'
   			puts 'dia hit!!'
		end
	end
#   diaファイルを強制的にredに変更
	if ls_w1 =~ /.red$/
		# .redファイルである

		# Redファイル名のセット
		case	site_flag
		when	'1'
			red_file = std_form + ls_w1
		when	'2'
			red_file = site_form + ls_w1
		else
			red_file = std_form + ls_w1
		end

		lp_name = d2[30, 20].strip				# LP名のセット
		offset_x = d2[50, 5].strip				# offset-xのセット
		offset_y = d2[55, 5].strip				# offset-yのセット
#---- (2003/07/03 ) add start
		prt_flg = d2[61, 1].strip                       	# 出力フラグのセット
		psfile_name_folder = d2[62, 165].strip	                # PSファイル格納フォルダのセット
		psfile_name_folder = add_slash(psfile_name_folder) 	#/を付加
		psfile_name = psfile_name_folder.strip + hospnum + d2[227, 8].strip + d2[235, 4].strip \
		+ d2[239, 14].strip + d2[253, 4].strip + d2[257, 5].strip + d2[262,36].strip
									# PSファイル名のセット
#		word3 = d2[51, (d2_len - 51)]				# 一時ファイルへ出力する内容のセット
#		word3 = d2[262, (d2_len - 262)]				# 一時ファイルへ出力する内容のセット
		word3 = d2[298, (d2_len - 298)]				# 一時ファイルへ出力する内容のセット
#---- (2003/07/03 ) add end

		# 一時ファイルへの書き込み
		open(temp_file, "w") do |fp|
			fp.print word3
			fp.print ' ' * 20000
		end

# デバッグ用の表示
		case	word3
		when	nil
			puts	'改行のみです' + '[' + String(li_cnt1) + ']'
		when	''
			puts	'改行のみです' + '[' + String(li_cnt1) + ']'
		else
			if d2 =~ /\A\s*\z/
				puts	'空白・改行です' + '[' + String(li_cnt1) + ']'
#			else
#				puts	'OK [' + String(li_cnt1) + ']'
			end
		end
		# LP名が指定されていなかったら、LP名をlp1にする
		if lp_name == ''
			lp_name = DEFAULT_LP
		end
		# offset-xが指定されていなかったら、0にする
		if offset_x == ''
			offset_x = '0'
		end
		# offset-yが指定されていなかったら、0にする
		if offset_y == ''
			offset_y = '0'
		end

#---- (2003/01/20 ) start
#		w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -p ' + lp_name
#   労災の枠なし帳票をスイッチする st
	puts prt_flg
	puts ls_w1
	if prt_flg == '3' || prt_flg == '4'
		case	ls_w1
#   -----------> 労災分
		when	'HCM22V04.red'
			ls_w1 = 'HCM22V04P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
		when	'HCM24V04.red'
			ls_w1 = 'HCM24V04P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
		when	'HCM21V04.red'
			ls_w1 = 'HCM21V04P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
		when	'HCM18V04.red'
			ls_w1 = 'HCM18V04P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
		when	'HCM20V04.red'
			ls_w1 = 'HCM20V04P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
		when	'HCN22V04.red'
			ls_w1 = 'HCN22V04P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
		when	'HCN21V04.red'
			ls_w1 = 'HCN21V04P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
		when	'HCN50.red'
			ls_w1 = 'HCN50P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
		when	'HCN51.red'
			ls_w1 = 'HCN51P.red'
			red_file = std_form + ls_w1
   			puts 'rousai hit!!'
#   -----------> 労災分(2008/7/30)
		when	'HCM22V05.red'
			ls_w1 = 'HCM22V05P.red'
			red_file = std_form + ls_w1
		when	'HCM24V05.red'
			ls_w1 = 'HCM24V05P.red'
			red_file = std_form + ls_w1
		when	'HCM21V05.red'
			ls_w1 = 'HCM21V05P.red'
			red_file = std_form + ls_w1
		when	'HCM24V05.red'
			ls_w1 = 'HCM24V05P.red'
			red_file = std_form + ls_w1
		when	'HCM18V05.red'
			ls_w1 = 'HCM18V05P.red'
			red_file = std_form + ls_w1
		when	'HCM20V05.red'
			ls_w1 = 'HCM20V05P.red'
			red_file = std_form + ls_w1
		when	'HCN22V05.red'
			ls_w1 = 'HCN22V05P.red'
			red_file = std_form + ls_w1
		when	'HCM24V05.red'
			ls_w1 = 'HCM24V05P.red'
			red_file = std_form + ls_w1
		when	'HCN21V05.red'
			ls_w1 = 'HCN21V05P.red'
			red_file = std_form + ls_w1
		when	'HCM24V05.red'
			ls_w1 = 'HCM24V05P.red'
			red_file = std_form + ls_w1
		when	'HCN50V01.red'
			ls_w1 = 'HCN50V01P.red'
			red_file = std_form + ls_w1
		when	'HCM24V05.red'
			ls_w1 = 'HCM24V05P.red'
			red_file = std_form + ls_w1
		when	'HCN51V01.red'
			ls_w1 = 'HCN51V01P.red'
			red_file = std_form + ls_w1
		when	'HCM24V05.red'
			ls_w1 = 'HCM24V05P.red'
			red_file = std_form + ls_w1
#   -----------> 自賠分
		when	'HCM23V04.red'
			ls_w1 = 'HCM48.red'
			red_file = std_form + ls_w1
   			puts 'jibai hit!!'
		when	'HCM29V02.red'
			ls_w1 = 'HCM49.red'
			red_file = std_form + ls_w1
   			puts 'jibai hit!!'
		when	'HCM53.red'
			ls_w1 = 'HCM54.red'
			red_file = std_form + ls_w1
   			puts 'jibai hit!!'
		when	'HCN23V04.red'
			ls_w1 = 'HCN48.red'
			red_file = std_form + ls_w1
   			puts 'jibai hit!!'
		when	'HCN53.red'
			ls_w1 = 'HCN54.red'
			red_file = std_form + ls_w1
   			puts 'jibai hit!!'
#   -----------> 自賠分(2008/7/30)
		when	'HCN23V05.red'
			ls_w1 = 'HCN48V01.red'
			red_file = std_form + ls_w1
		when	'HCM29V03.red'
			ls_w1 = 'HCM49V01.red'
			red_file = std_form + ls_w1
		when	'HCM53V01.red'
			ls_w1 = 'HCM54V01.red'
			red_file = std_form + ls_w1
		when	'HCM29V03.red'
			ls_w1 = 'HCM49V01.red'
			red_file = std_form + ls_w1
		when	'HCM23V05.red'
			ls_w1 = 'HCM48V01.red'
			red_file = std_form + ls_w1
		when	'HCM29V03.red'
			ls_w1 = 'HCM49V01.red'
			red_file = std_form + ls_w1
		when	'HCN53V01.red'
			ls_w1 = 'HCN54V01.red'
			red_file = std_form + ls_w1
		when	'HCM29V03.red'
			ls_w1 = 'HCM49V01.red'
			red_file = std_form + ls_w1
#   -----------> 労災分(2011/8/05)
                when    'HCM22V06.red'
                        ls_w1 = 'HCM22V06P.red'
                        red_file = std_form + ls_w1
                when    'HCN50V02.red'
                        ls_w1 = 'HCN50V02P.red'
                        red_file = std_form + ls_w1
                when    'HCM24V06.red'
                        ls_w1 = 'HCM24V06P.red'
                        red_file = std_form + ls_w1
                when    'HCM24V06.red'
                        ls_w1 = 'HCM24V06P.red'
                        red_file = std_form + ls_w1
                when    'HCM21V06.red'
                        ls_w1 = 'HCM21V06P.red'
                        red_file = std_form + ls_w1
                when    'HCN51V02.red'
                        ls_w1 = 'HCN51V02P.red'
                        red_file = std_form + ls_w1
                when    'HCM24V06.red'
                        ls_w1 = 'HCM24V06P.red'
                        red_file = std_form + ls_w1
                when    'HCM24V06.red'
                        ls_w1 = 'HCM24V06P.red'
                        red_file = std_form + ls_w1
                when    'HCM18V06.red'
                        ls_w1 = 'HCM18V06P.red'
                        red_file = std_form + ls_w1
                when    'HCM20V06.red'
                        ls_w1 = 'HCM20V06P.red'
                        red_file = std_form + ls_w1
#   -----------> 自賠分(2011/8/05)
                when    'HCM23V06.red'
                        ls_w1 = 'HCM48V02.red'
                        red_file = std_form + ls_w1
                when    'HCN23V06.red'
                        ls_w1 = 'HCN48V02.red'
                        red_file = std_form + ls_w1
                when    'HCM29V04.red'
                        ls_w1 = 'HCM49V02.red'
                        red_file = std_form + ls_w1
                when    'HCM29V04.red'
                        ls_w1 = 'HCM49V02.red'
                        red_file = std_form + ls_w1
                when    'HCM53V02.red'
                        ls_w1 = 'HCM54V02.red'
                        red_file = std_form + ls_w1
                when    'HCN53V02.red'
                        ls_w1 = 'HCN54V02.red'
                        red_file = std_form + ls_w1
                when    'HCM29V04.red'
                        ls_w1 = 'HCM49V02.red'
                        red_file = std_form + ls_w1
                when    'HCM29V04.red'
                        ls_w1 = 'HCM49V02.red'
                        red_file = std_form + ls_w1
#   -----------> 改正対応(2012/04/11)
                when    'HCN50V03.red'
                        ls_w1 = 'HCN50V03P.red'
                        red_file = std_form + ls_w1
                when    'HCN51V03.red'
                        ls_w1 = 'HCN51V03P.red'
                        red_file = std_form + ls_w1
                when    'HCN23V07.red'
                        ls_w1 = 'HCN48V03.red'
                        red_file = std_form + ls_w1
                when    'HCN53V03.red'
                        ls_w1 = 'HCN54V03.red'
                        red_file = std_form + ls_w1
		end
	end
#   労災の枠なし帳票をスイッチする ed

# take patch-lib にformが存在する場合は、pathを切替える
site_red_file = site_form + ls_w1
patch_red_file = patch_form + ls_w1
if File.exist?(site_red_file)
     puts 'Site  Hit!!' + ls_w1 + ' ' + site_form
else
  if File.exist?(patch_red_file)
     red_file = patch_red_file
     puts 'Patch Hit!!' + ls_w1 + ' ' + patch_form
  else
     puts 'Normal Hit!!' + ls_w1
  end
end

#---- (2003/07/03) start
		case	prt_flg
		when	nil
			puts	'出力区分未設定'
		when	''
			puts	'出力区分未設定'
		when	'1'     # 印刷のみの指示
		puts	'take1 Start [' + psfile_name + ']'
			w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -x ' + offset_x + ' -y ' + offset_y + ' -p ' + lp_name + ' ' + LAYEROPTION
		when	'2'     # 印刷＆PSファイル出力の指示
		puts	'take2 Start [' + psfile_name + ']'
			# 出力ファイル名が指定されていなかったら、通常の印刷処理を行う
			if psfile_name == ''
				puts	'出力ファイル名未設定'
				w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -x ' + offset_x + ' -y ' + offset_y + ' -p ' + lp_name + ' ' + LAYEROPTION
			else
#				w_exec = RED_EXECPS + ' ' + red_file + ' ' + temp_file + ' -p ' + lp_name + ' -o ' + psfile_name
				w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -x ' + offset_x + ' -y ' + offset_y + ' -p ' + lp_name + ' -o ' + psfile_name + ' '  + LAYEROPTION
			end
		when	'3'     # PSファイル出力の指示
		puts	'take3 Start [' + psfile_name + ']'
			# 出力ファイル名が指定されていなかったら、通常の印刷処理を行う
			if psfile_name == ''
				puts	'出力ファイル名未設定'
			else
				w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -o ' + psfile_name + ' ' + LAYEROPTION
			end
		when	'4'     # PSファイル出力の指示
			# 出力ファイル名が指定されていなかったら、通常の印刷処理を行う
			if psfile_name == ''
				puts	'出力ファイル名未設定'
			else
				w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -o ' + psfile_name + ' ' + LAYEROPTION
			end
		else
			puts	'出力区分内容設定エラー'
		end
#---- (2003/07/03) end  
#		if prt_flg == '3'
#			# 出力ファイル名が指定されていなかったら、通常の印刷処理を行う
#			if psfile_name == ''
#				w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -p ' + lp_name
#			else
#				w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -o ' + psfile_name
#			end
#		else
#			w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -p ' + lp_name
#		end
#---- (2003/03/20) end
#---- (2003/01/20 ) end

# デバッグ用の表示
#		puts w_exec
# **
		# 実行前メッセージ出力
		puts	'Print Start [' + String(li_cnt1) + ']'
		# プログラムの実行
		pid = fork do
			exec w_exec
		end
		sleep 0.01	# 予期せぬエラーの回避のため、待つ(これを行わないと、ruby ver1.4上で呼び出されたスクリプトにエラーが発生する)
		# 実行したプログラムが終わるまで待つ(引数の２番目は、1.4でのエラー回避のため)
		Process.waitpid(pid, 0)


	else
		# .redファイル以外である

		# Dia・defファイル名のセット
		case	site_flag
		when	'1'
			dia_file = std_form + ls_w1 + '.dia'
			def_file = std_record + ls_w1 + '.def'
		when	'2'
			dia_file = site_form + ls_w1 + '.dia'
			def_file = site_record + ls_w1 + '.def'
		else
			dia_file = std_form + ls_w1 + '.dia'
			def_file = std_record + ls_w1 + '.def'
		end

		lp_name = d2[30, 20].strip	# LP名のセット
#---- (2003/01/20 ) start
		prt_flg = d2[61, 1].strip                       	# 出力フラグのセット
#---- (2003/07/03 ) add start
		psfile_name_folder = d2[62, 165].strip	                # PSファイル格納フォルダのセット
		psfile_name_folder = add_slash(psfile_name_folder) 	#/を付加
		psfile_name = psfile_name_folder.strip + hospnum + d2[227, 8].strip + d2[235, 4].strip \
		+ d2[239, 14].strip + d2[253, 4].strip + d2[257, 5].strip + d2[262,36].strip
									# PSファイル名のセット
#		word3 = d2[51, (d2_len - 51)]				# 一時ファイルへ出力する内容のセット
#		word3 = d2[262, (d2_len - 262)]				# 一時ファイルへ出力する内容のセット
		word3 = d2[298, (d2_len - 298)]				# 一時ファイルへ出力する内容のセット
#---- (2003/07/03 ) add end
#---- (2003/01/20 ) end


		# 一時ファイルへの書き込み
		open(temp_file, "w") do |fp|
			fp.print word3
			fp.print ' ' * 20000
		end
# デバッグ用の表示
		case	word3
		when	nil
			puts	'改行のみです' + '[' + String(li_cnt1) + ']'
		when	''
			puts	'改行のみです' + '[' + String(li_cnt1) + ']'
		else
			if d2 =~ /\A\s*\z/
				puts	'空白・改行です' + '[' + String(li_cnt1) + ']'
#			else
#				puts	'OK [' + String(li_cnt1) + ']'
			end
		end
#---- (2003/01/20 ) start
#		w_exec = exec_file + ' ' + dia_file + ' ' + def_file + ' ' + temp_file
##		w_exec = exec_file + ' ' + dia_file2 + ' ' + def_file2 + ' ' + temp_file2

#---- (2003/07/03) start
		case	prt_flg
		when	nil
			puts	'出力区分未設定'
		when	''
			puts	'出力区分未設定'
		when	'1'     # 印刷のみの指示
			w_exec = exec_file + ' ' + dia_file + ' ' + def_file + ' ' + temp_file
			# LP名が指定されていたら、引数に追加
			if lp_name != ''
				w_exec = w_exec + ' ' + lp_name
			end
			puts	'OKtake [' + w_exec + ']'
		when	'2'     # 印刷＆PSファイル出力の指示
			# 出力ファイル名が指定されていなかったら、通常の印刷処理を行う
			if psfile_name == ''
				puts	'出力ファイル名未設定'
				w_exec = exec_file + ' ' + dia_file + ' ' + def_file + ' ' + temp_file
				# LP名が指定されていたら、引数に追加
				if lp_name != ''
					w_exec = w_exec + ' ' + lp_name
				end
			else
				w_exec = exec_file_prv + ' ' + dia_file + ' ' + def_file + ' ' + temp_file + ' ' + psfile_name
				# LP名が指定されていたら、引数に追加
				if lp_name != ''
					w_exec = w_exec + ' ' + lp_name
				end
			end
		when	'3'     # PSファイル出力の指示
			# 出力ファイル名が指定されていなかったら、通常の印刷処理を行う
			if psfile_name == ''
				puts	'出力ファイル名未設定'
			else
				w_exec = exec_file_prv + ' ' + dia_file + ' ' + def_file + ' ' + temp_file + ' ' + psfile_name
				# LP名はダミーを設定(印刷しないモード)
				w_exec = w_exec + ' ' + '@@@@@@@@@@'
			end
		when	'4'     # PSファイル出力の指示
			# 出力ファイル名が指定されていなかったら、通常の印刷処理を行う
			if psfile_name == ''
				puts	'出力ファイル名未設定'
			else
				w_exec = exec_file_prv + ' ' + dia_file + ' ' + def_file + ' ' + temp_file + ' ' + psfile_name
				# LP名はダミーを設定(印刷しないモード)
				w_exec = w_exec + ' ' + '@@@@@@@@@@'
			end
		else
			puts	'出力区分内容設定エラー'
		end
#---- (2003/07/03) end  
#---- (2003/03/20) start
#		if prt_flg == '3'
#			# 出力ファイル名が指定されていなかったら、通常の印刷処理を行う
#			if psfile_name == ''
#				w_exec = exec_file + ' ' + dia_file + ' ' + def_file + ' ' + temp_file
#			# LP名が指定されていたら、引数に追加
#				if lp_name != ''
#					w_exec = w_exec + ' ' + lp_name
#				end
#			else
#				w_exec = exec_file + ' ' + dia_file + ' ' + def_file + ' ' + temp_file + ' -o ' + psfile_name
#			end
#		else
#			w_exec = exec_file + ' ' + dia_file + ' ' + def_file + ' ' + temp_file
#			# LP名が指定されていたら、引数に追加
#			if lp_name != ''
#				w_exec = w_exec + ' ' + lp_name
#			end
#		end
#---- (2003/03/20) end
		# LP名が指定されていたら、引数に追加
#		if lp_name != ''
#			w_exec = w_exec + ' ' + lp_name
#		end
#---- (2003/01/20 ) end
# デバッグ用の表示
#		puts w_exec
# **
		# 実行前メッセージ出力
		puts	'Print Start [' + String(li_cnt1) + ']'
		# プログラムの実行
		pid = fork do
			exec w_exec
		end
		sleep 0.01	# 予期せぬエラーの回避のため、待つ(これを行わないと、ruby ver1.4上で呼び出されたスクリプトにエラーが発生する)
		# 実行したプログラムが終わるまで待つ(引数の２番目は、1.4でのエラー回避のため)
		Process.waitpid(pid, 0)

	end
end



	puts '[end ' + `date` + ']'
# ============================================================

# 一時ファイル削除
begin
	if nil != File.size(temp_file)
		# 一時ファイルが存在する
		# 一時ファイルの削除
		File.delete(temp_file)
	end
rescue
# 例外が発生すれば、一時ファイルが存在しない
end



# ============================================================


