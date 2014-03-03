#!/usr/bin/ruby


# Claim�̿��С������2
# ����XML�Υ������󥷥��ե�����ؤ��Ѵ�����

# 2004/01/28 version2.00
#   ��XML�ǡ�����ꥹ�ȹ�¤�ˤ��뤳�Ȥǹ�®��
#     ���������ǡ����̤ξ��ʤ��ե�����ˤĤ��Ƥϡ�®�٤��Ѥ��ʤ�
#     �礭��XML�ե�������Ф�����̤��⤯��
#     �Ǥ��礭��ʪ��90�ܤι�®�����ǧ���Ƥ���
#     �ʤ���®�٤�����ե�������礭���˰�¸���Ƥ��뤿�ᡢ
#     ����ե�����Υ��������礭���ʤ���٤��ʤ�Τǡ�
#     �߷פˤ����դ��Ƥۤ���
#   ������ե�����򥳥�С��Ȥ��������������Ȥ뤳�Ȥǡ������������
#     ���ޤޤǡ�����ե�����θ��ɤ������ʤ��ü���ä�����
#     ���Ҥ�XML�ѥ��˻��������ˤ������ϰ��־����ʤ������Ȥ�
#     ����ե�����ϸ��䤹���ʤäƤ��롣



# =============================================================================
# =============================================================================
# ľ�ܼ¹Ԥ����Ȥ��ν���
if __FILE__ == $0


#	$debug = 0	# �ǥХå��⡼�ɤǤϤʤ�
#	$debug = 1	# �ǥХå��⡼�ɤǤ��� (��٥�1)
#	$debug = 2	# �ǥХå��⡼�ɤǤ��� (��٥�2)
	$debug = 3	# �ǥХå��⡼�ɤǤ��� (��٥�3)

	# Debian�Ķ��Ǥ�EUC�Ķ���Windows�Ķ��Ǥϥ��ե�JIS�Ķ������򤷤Ƥ���������
	$lang_conf = '1'	# EUC�Ķ�
#	$lang_conf = '2'	# ���ե�JIS�Ķ�



	# LineTarget����ǡ�����ˤ���XML�ѥ����֤�����ʸ��
	$nodata_word = '<none>'

	help_flg = 0		# �إ��ɽ���ե饰 [0 = �إ�פ�ɽ�����ʤ�, 1 = �إ�פ�ɽ������]
	$output_mode = 0	# ���ϥ⡼�� [0 = �������󥷥��ե��������, 1 = CSV�ե��������]
						# �嵭�ν��ϥ⡼�ɤ��ǥե����
	w_file_name = []		# �ե�����̾�ΰ��Ū�ʳ�Ǽ�ΰ�

	$hir_match_mode = 0		# ���ؾ����ͤ���碌�⡼��[0 = �������󸡺�, 1 = ���إꥹ�ȸ���]



	# �����Υ��åȽ���
	ARGV.each do |w_a1|
		case	w_a1.strip
		when	'-s'
			# ���Ϥ���ե�����ϥ������󥷥��ե���������Ǥ���
			$output_mode = 0
		when	'-c'
			# ���Ϥ���ե������CSV�ե���������Ǥ���
			$output_mode = 1
		when	'--lang=euc-jp'
			# ����Ķ��ϡ�EUC-JP�Ǥ���
			$lang_conf = '1'
		when	'--lang=shift_jis'
			# ����Ķ��ϡ����ե�JIS�Ǥ���
			$lang_conf = '2'
		when	'--hierarcy-match=array'
			# ���ؾ���ȤΥޥå��󥰤ϡ���������Ǥ���
			$hir_match_mode = 0
		when	'--hierarcy-match=list'
			# ���ؾ���ȤΥޥå��󥰤ϡ����ؾ�������Ǥ���
			$hir_match_mode = 1
		when	'--help', '-?'
			help_flg = 1
		else
			if (w_a1.strip)[0, 1] != '-'
				# ��ʸ���ܤ��ޥ��ʥ��ʳ��Τ���¾�Υ��ޥ�ɥ��ץ����ϥե�����̾�Ȥ��ƥ��åȤ���
				w_file_name.push w_a1.strip
			end
		end
	end

	# �ե�����̾�ο������ʤ��Ȥ��ϡ����顼ɽ��������
	case	w_file_name.size
	when	0
		# �ե�����̾�λ��꤬�ʤ�
		help_flg = 1
	when	1, 2
		# �ե�����ο������ʤ�
		$stderr.puts '���ޥ�ɥ饤��ΰ����˥ե�����̾�����ꤵ��Ƥ��ޤ���'
		$stderr.puts ''
		help_flg = 1
	else
		# �ե�����̾�λ��꤬���İʾ�ʤΤǡ����åȤ��줿���Ƥ��Ϥ�
		$hir_file = w_file_name[0]
		$xml_file = w_file_name[1]
		$seq_file = w_file_name[2]
	end


	if help_flg == 1
		$stderr.puts 'claim2_decode.rb [option] [��������ե�����] [����XML�ե�����] [���ϥե�����]'
		$stderr.puts ''
		$stderr.puts '  [option]'
		$stderr.puts '    -s  �������󥷥��ե�������� [default]'
		$stderr.puts '    -c  CSV�ե��������'
		$stderr.puts '    --lang=euc-jp     �¹ԴĶ���ʸ�������ɤ�EUC-JP�Ǥ���(Default)'
		$stderr.puts '    --lang=shift_jis  �¹ԴĶ���ʸ�������ɤ�Shift_JIS�Ǥ���(Windows�Ǥμ¹���)'
		exit 0
	end


	# �ѿ���ǧ�Ѥ�ɽ������
	$stderr.puts '��������ե�����           = [' + $hir_file + ']'
	$stderr.puts '����XML�ե�����            = [' + $xml_file + ']'
	$stderr.puts '���ϥ������󥷥��ե����� = [' + $seq_file + ']'



# =============================================================================
	# �̥⥸�塼������߽���



end


require 'claim2_lib'
require 'xmlparser'
require 'kconv'
require 'uconv'
include Uconv



# =============================================================================
# =============================================================================
# ���饹��


# XML�ꥹ�ȴ��ܥ��饹
class Xml_baselist


	# ------------------------------------------------------------
	# �����
	def initialize
		@flg = 0	# �ե饰(0=�ǡ������ޤ��ϡ��ǡ����ʤ�, 1=°��)
		@name = ''	# ����̾��°��̾
		@data = nil	# �¥ǡ���
		@list_value = 0	# ����XML�ꥹ�ȿ�
		@xmllist = []	# ����XML�ꥹ��
	end


	# ------------------------------------------------------------
	# XML�ꥹ�ȴ��ܥ��饹������
	#    ����
	# name - ����̾���ޤ��ϡ�°��̾ [IN / String��]
	# flg - �ե饰 [IN / Numeric��]
	#          0 = �ǡ������ޤ��ϡ��ǡ����ʤ�
	#          1 = °��
	# data - �������ޤ��ϡ�°���Υǡ������� [IN / String��]
	def childadd(name, flg, data)
		xml_newlist = Xml_baselist.new
		xml_newlist.name = name
		xml_newlist.flg = flg
		xml_newlist.data = data
		@xmllist.push xml_newlist
		@list_value += 1
		@xml_newlist = nil
		return	xml_newlist
	end


	# ------------------------------------------------------------
	# XML�ѥ���XML�ѥ�����������Ѵ�
	#    ����
	# xmlpath - �Ѵ�����XML�ѥ� [IN / String��]
	#    �����
	# XML�ѥ�����
	#    ����
	# ���δؿ��ϡ����Хѥ����θ�������ˤϤʤäƤ��ޤ���Τǡ�
	# ���Хѥ��ǵ��Ҥ��Ƥ�����ϡ����Хѥ����Ѵ����Ƥ���������
	#    ���
	# ���С��ؿ��Ȥ��Ƥϡ����ޤ깥�ޤ����ʤ�����
	# ��������ǤΤ��Ȥ������ˤ��Ƥ���Τǡ��Ȥꤢ�������С��ؿ��Ȥ��ƺ�������
	def convert_xml_path_to_patharray(xmlpath)
		xml_array = [] ; xml_array_size = 0 ; w_xmlpath = ''
		# ����ζ����������ǡ���/�פζ��ڤ�����󲽤���
		w_xmlpath = xmlpath.strip
		xml_array = w_xmlpath.split(/\//)
		# �������Ƭ���������ʤ顢��������Τߺ��
		case	xml_array[0]
		when	nil, ''
			xml_array.delete_at(0)
		end
		xml_array_size = xml_array.size
		if xml_array[xml_array_size - 1] =~ /\S@\S/
			word1 = '' ; word2 = []
			word1 = xml_array[xml_array_size - 1]
			word2 = word1.split(/@/)
			# ����@�פ�����ʾ�и����뤳�ȤϹ�θ���Ƥ��ޤ���
			xml_array[xml_array_size - 1] = word2[0]	# ��@�פ�����򥻥å�
			xml_array.push('@' + word2[1])	# ��@�װʹߤ򥻥å�
		end
		return	xml_array
	end


	# ------------------------------------------------------------
	# ����XML�ѥ�(���Хѥ������Τ�)����Υǡ����μ���
	#    ����
	# xmlpatharray - XML�ѥ����� [IN / ����(String)��]
	#    �����
	# ���ꤷ��XML�ѥ�����Υǡ���(nil�ξ�硢�����Ǥ��ʤ��ä�) [OUT / String��]
	#    ����
	# ����������ϴؿ��¹Ը���ͤ��ݾڤ��ޤ���
	# ����XML�ꥹ�ȤΥǡ����������������Ǥ�
	def get_data_from_xmlpatharray(xmlpatharray)
		xml_tag = '' ; array_size = 0 ; xmllist_size = 0
		ret_data = nil
		xml_tag = xmlpatharray.shift
		if xml_tag == nil
			# nil�ξ��֤ξ��ϡ��㳰��ȯ��������
			raise 'get_data_from_xmlpatharray���㳰��ȯ�����ޤ�����' + "\n" + '������XML�ѥ�����˥ǡ���������ޤ���'
			# �㳰��ȯ���Ǥ��ʤ������θ����
			return	nil
		end
		array_size = xmlpatharray.size
		xmllist_size = @xmllist.size
		if array_size > 0
			# ����XML�ꥹ�Ȥμ���
			for x in 1..xmllist_size do

# ��XML�ǡ������ɤ߹���������ǡ����������̾�����֤������Ƥ��뤳�Ȥ�����Ȥ��Ƥ��롣

				if @xmllist[x - 1].name == xml_tag
					# ���եȸ��XML�ѥ����������˻��ꤷ�ơ�����XML�Υǡ��������ؿ���ƤӽФ�
					ret_data = @xmllist[x - 1].get_data_from_xmlpatharray(xmlpatharray.clone)
					break	# forʸ����ȴ����
				end
				if x == xmllist_size
					# ����XML�ꥹ�Ȥ˳������륿�������Ĥ���ʤ��ä����ϡ�nil���֤���
					ret_data = nil
				end
			end
		else
			# ����XML�ꥹ�Ȥϸ������ʤ�
			# ���ꤷ������̾���ͤ����
			if xml_tag[0, 1] == '@'
				# °���Υǡ��������Ǥ���
				attr_tag = xml_tag
#				attr_tag = ''
#				attr_tag = xml_tag[1..-1]	# ��ʸ���ܰʹߤ�°��̾�Ȥ��ƻ��Ѥ���
				for x in 1..xmllist_size do
					if @xmllist[x - 1].name == attr_tag
						ret_data = @xmllist[x - 1].data	# �¥ǡ������֤�
						break	# forʸ����ȴ����
					end
					if x == xmllist_size
						# ����XML�ꥹ�Ȥ˳������륿�������Ĥ���ʤ��ä����ϡ�nil���֤���
						ret_data = nil
					end
				end
			else
				# �����Υǡ��������Ǥ���

# ��XML�ǡ������ɤ߹���������ǡ����������̾�����֤������Ƥ��뤳�Ȥ�����Ȥ��Ƥ��롣

				
				for x in 1..xmllist_size do
					if @xmllist[x - 1].name == xml_tag
						ret_data = @xmllist[x - 1].data	# �¥ǡ������֤�
						break	# forʸ����ȴ����
					end
					if x == xmllist_size
						# ����XML�ꥹ�Ȥ˳������륿�������Ĥ���ʤ��ä����ϡ�nil���֤���
						ret_data = nil
					end
				end
			end
		end


	#    ���
	# ������Ѥ�Τϡ�/***[**]/****[**]/****[**]@***�Ȥ��ä�XML�ѥ�
	# join�ǡ�/�פ���ڤä����ȡ�@��¸�ߤ��뤫������å����ơ�¸�ߤ���С�
	# ��@�פǶ��ڤä����ȡ��Ǹ������ǡ������ܰ�����Ƭ�ˡ�@�����ˤ��դ���
	# ����nil�ˤʤä������ǡ��ǡ������������

		return ret_data
	end


	# ------------------------------------------------------------
	# LineTarget����ǻ��Ѥ���XML�ѥ�����Υǡ����μ���
	#    ����
	# xmlpatharray - �����о�XML�ѥ����� [IN / ����(String)��]
	# hierarcy_xmlpath - �ƤӽФ���XML�ѥ����� [IN / ����(String)�� / �ǥե���� = []]
	#    �����
	# []�ξ�� - �����Ǥ��ʤ��ä�
	# []�ʳ��ξ��
	#  [x][0] - �����ǻ��ꤷ��XML�ѥ������Ĥ��ä����ºݤ�XML�ѥ� [OUT / String��]
	#  [x][1] - ���ꤷ��XML�ѥ�����Υǡ��� [OUT / String��]
	#    ����
	# ����������ϴؿ��¹Ը���ͤ��ݾڤ��ޤ���
	# ���Хѥ��ˤ��б����Ƥ��ޤ���
	# ��������ꤷ�ʤ�ɽ���ϡ�***[x]�פȤ����褦�ˡ�����������ˡ�x�פ���ꤷ�ޤ�
	# ����XML�ꥹ�ȤΥǡ����������������Ǥ�
	# �̾�ϡ���hierarcy_xmlpath�פϻ��ꤷ�ޤ���
	#   ���ꤹ�륱�����ϡ����δؿ�����ǺƵ��ƤӽФ��򤷤����ΤߤǤ��礦��
	def get_data_from_xmlpatharray_reg(xmlpatharray, hierarcy_xmlpath = [])
		xml_tag = '' ; array_size = 0 ; xmllist_size = 0
		xml_tag2 = ''
		xml_tag3 = ''		# �����ֹ�ʤ��Υ���̾
		w_xml_path1 = ''	# XML�ѥ������å���ʸ������Ѵ��������Ƥ��Ǽ������
		w_xml_path2 = []	# ��XML�ꥹ�Ȥ��Ϥ�XML�ѥ�����
		attr_tag = ''
		reg_flg = 0		# ����ɽ���ե饰(0 = ����ɽ���ǤϤʤ�, 1 = ����ɽ���Ǥ���)
		ret_data = []
		xml_tag = xmlpatharray.shift
		if xml_tag == nil
			# nil�ξ��֤ξ��ϡ��㳰��ȯ��������
			raise 'get_data_from_xmlpatharray_reg���㳰��ȯ�����ޤ�����' + "\n" + '������XML�ѥ�����˥ǡ���������ޤ���'
			# �㳰��ȯ���Ǥ��ʤ������θ����
			return	[]
		end

		# �����������������̾��xml_tag3�˥��åȤ���
		xml_tag =~ /^(\S+?)\[[0-9x]+?\]/i
		xml_tag3 = $1

		array_size = xmlpatharray.size
		xmllist_size = @xmllist.size

		if xml_tag =~ /^(\S+?)\[x\]/i
			# ----------------------------------------
			# ����������ɽ������ꤷ�Ƥ���
			# $1 = ������ʬ�����������̾
			xml_tag2 = Regexp.quote($1) + '\[[0-9]+?\]'	# ����ɽ������ӤǤ���褦�ˤ���
			if array_size > 0
				# ����XML�ꥹ�Ȥμ���
				for x in 1..xmllist_size do

# ��XML�ǡ������ɤ߹���������ǡ����������̾�����֤������Ƥ��뤳�Ȥ����ΤȤ��Ƥ��롣

					if @xmllist[x - 1].name =~ /#{xml_tag2}/
						# ��XML�ꥹ�Ȥˡ����ߤΥ���̾��ä����Ϥ�
						w_xml_path2 = hierarcy_xmlpath.clone
						w_xml_path2.push @xmllist[x - 1].name
						# ���եȸ��XML�ѥ����������˻��ꤷ�ơ�����XML�Υǡ��������ؿ���ƤӽФ�
						ret_data += @xmllist[x - 1].get_data_from_xmlpatharray_reg(xmlpatharray.clone, w_xml_path2)
					end
					# �������륿�������Ĥ���ʤ��Ƥ⡢ret_data���Ф��Ƥϲ��⤷�ʤ�
				end
			else
				# ����XML�ꥹ�Ȥϸ������ʤ�
				# ���ꤷ������̾���ͤ����
				if xml_tag =~ /^(@\S+?)\[x\]/
					# °���Υǡ��������Ǥ���
					# $1 = ��@�פ�������ʬ��ޤ᤿°��̾
					attr_tag = Regexp.quote($1)	# ����ɽ������ӤǤ���褦�ˤ���
#					# $1 = ��@�פ�������ʬ�������°��̾
#					attr_tag = Regexp.quote($1) + '\[[0-9]\]'	# ����ɽ������ӤǤ���褦�ˤ���
					for x in 1..xmllist_size do
						if @xmllist[x - 1].name =~ /#{attr_tag}/
							w_xml_path1 = '/' + hierarcy_xmlpath.join('/') + @xmllist[x - 1].name
#							w_xml_path1 = '/' + hierarcy_xmlpath.join('/') + '@' + @xmllist[x - 1].name
							ret_data.push [w_xml_path1, @xmllist[x - 1].data]	# �¥ǡ����򥹥��å����Ѥ�
						end
						# �������륿�������Ĥ���ʤ��Ƥ⡢ret_data���Ф��Ƥϲ��⤷�ʤ�
					end
				else
					# �����Υǡ��������Ǥ���
					for x in 1..xmllist_size do
						if @xmllist[x - 1].name =~ /#{xml_tag2}/
							w_xml_path1 = '/' + hierarcy_xmlpath.join('/') + '/' + @xmllist[x - 1].name
							ret_data.push [w_xml_path1, @xmllist[x - 1].data]	# �¥ǡ����򥹥��å����Ѥ�
						end
						# �������륿�������Ĥ���ʤ��Ƥ⡢ret_data���Ф��Ƥϲ��⤷�ʤ�
					end
				end
			end
			# ----------------------------------------
		else
			# ----------------------------------------
			# ����������ɽ������ꤷ�Ƥ��ʤ�
			if array_size > 0
				# ����XML�ꥹ�Ȥμ���
				for x in 1..xmllist_size do

# ��XML�ǡ������ɤ߹���������ǡ����������̾�����֤������Ƥ��뤳�Ȥ�����Ȥ��Ƥ��롣

					if @xmllist[x - 1].name == xml_tag
						# ��XML�ꥹ�Ȥˡ����ߤΥ���̾��ä����Ϥ�
						w_xml_path2 = hierarcy_xmlpath.clone
						w_xml_path2.push @xmllist[x - 1].name
						# ���եȸ��XML�ѥ����������˻��ꤷ�ơ�����XML�Υǡ��������ؿ���ƤӽФ�
						ret_data += @xmllist[x - 1].get_data_from_xmlpatharray_reg(xmlpatharray.clone, w_xml_path2)
						break	# forʸ����ȴ����
					end
					# �������륿�������Ĥ���ʤ��Ƥ⡢ret_data���Ф��Ƥϲ��⤷�ʤ�
				end
			else
				# ����XML�ꥹ�Ȥϸ������ʤ�
				# ���ꤷ������̾���ͤ����
				if xml_tag[0, 1] == '@'
					# °���Υǡ��������Ǥ���
					attr_tag = xml_tag
#					attr_tag = ''
#					attr_tag = xml_tag[1..-1]	# ��ʸ���ܰʹߤ�°��̾�Ȥ��ƻ��Ѥ���
					for x in 1..xmllist_size do
						if @xmllist[x - 1].name == attr_tag
							w_xml_path1 = '/' + hierarcy_xmlpath.join('/') + '@' + @xmllist[x - 1].name
							ret_data.push [w_xml_path1, @xmllist[x - 1].data]	# �¥ǡ����򥹥��å����Ѥ�
							break	# forʸ����ȴ����(����ɽ���Ǥʤ���в�)
						end
						# �������륿�������Ĥ���ʤ��Ƥ⡢ret_data���Ф��Ƥϲ��⤷�ʤ�
					end
				else
					# �����Υǡ��������Ǥ���

# ��XML�ǡ������ɤ߹���������ǡ����������̾�����֤������Ƥ��뤳�Ȥ�����Ȥ��Ƥ��롣

					for x in 1..xmllist_size do
						if @xmllist[x - 1].name == xml_tag
							w_xml_path1 = '/' + hierarcy_xmlpath.join('/') + '/' + @xmllist[x - 1].name
							ret_data.push [w_xml_path1, @xmllist[x - 1].data]	# �¥ǡ����򥹥��å����Ѥ�
							break	# forʸ����ȴ����(����ɽ���Ǥʤ���в�)
						end
						# �������륿�������Ĥ���ʤ��Ƥ⡢ret_data���Ф��Ƥϲ��⤷�ʤ�
					end
				end
			end
			# ----------------------------------------
		end

		# ����ǡ���������ͤȤ��ƻ��Ѥ���
		return ret_data
	end

	# ��reg = regular expression��ά



	# ------------------------------------------------------------
	# ���ǡ���������XML�ꥹ�Ȥκ��
	#    ����
	# ������������Ƥ���XML�ꥹ�Ȥ��θ
	def del_nodataxmllist
		if @list_value > 0
			# �����������ȡ�@list_value���ͤ��Ѥ�äƤ��ޤ��Τǡ�����ΰ�˸����ͤ򥻥åȤ���
			w_list_value1 = @list_value
			for m in 1..w_list_value1 do
				n = w_list_value1 - m
				if @xmllist[n].list_value > 0
					# ����XML�ꥹ�Ȥ��餵����������Ƥ���Τǡ�����XML�ꥹ�Ȥζ��ǡ��������¹Ԥ��롣
					@xmllist[n].del_nodataxmllist
				end

				# ������������XML�ꥹ�Ȥ����뤫���ޤ��ϥǡ���������å�����
				# ��������XML�ꥹ�Ȥ��ʤ����ǡ�����ʤ���С�������������XML�ꥹ�Ȥ�������
				if @xmllist[n].list_value == 0
					# ����XML�ꥹ�Ȥ����������Ƥ���XML�ꥹ�ȤϤʤ�
					case	@xmllist[n].data
					when	nil, ''
						# ����nil�Ǥ���
						# ������������XML�ꥹ�Ȥ������������
						@xmllist.delete_at(n)
						@list_value -= 1	# ���ߤ�������򥻥åȤ���
						# ��forʬ�ǡ�������¹Ԥ���ȡ����󥪡��С���Ƚ��ϳ��򵯤����Τǡ�
						#   ���ʤ餺�Ǹ�����󤫤�¹Ԥ��뤳��
					end
				end

			end

    	end
	end


	# ------------------------------------------------------------
	# ����XML�ꥹ�Ȥκ��
	#    ����
	# ����XML�ꥹ�Ȥ����������Ƥ����Τ��Ф��Ƥ��������򤷤ޤ���
	# ����XML�ꥹ�Ȥ��Ф��Ƥϡ�����XML�ꥹ�ȥ��֥������ȤΥ᥽�åɤ�¹Ԥ��ޤ���
	def del_child
		if @list_value > 0
			# ����XML�ꥹ�Ȥ�¸�ߤ���
			# �ҥ��֥������Ȥ�����XML�ꥹ�Ȥκ��
			for x in 1..@list_value do
				@xmllist[x - 1].del_child
			end
			# ����XML�ꥹ�Ȥκ��
			for x in 1..@list_value do
				@xmllist[x - 1] = nil
			end
			@xmllist = []
			# �����󥿤Υ��ꥢ
			@list_value = 0
    	end
    end


	# ------------------------------------------------------------
	# ���֥������ȳ�����Υǡ������С��ؤΥ����������ǽ�ˤ���
	attr_accessor :flg, :name, :data, :list_value, :xmllist
#	public :flg, :name, :data, :list_value, :xmllist
end



# =============================================================================


# ���إǡ����ꥹ�ȥ��饹
# ���Υ��饹�ϡ����إǡ�����ĥ꡼��¤���Ѵ������ĥ꡼�������뤳�Ȥǹ�®�����뤿��Υ��饹�Ǥ�
# ��������������Ѵ���Ԥ����Ӥ˼¹Ԥ���ȥե�åȷ��������٤��ʤ뤿�ᡢ
# ��®�����뤿��ˤϡ����������ФȤ����礬ɬ�ܤǤ���

# �����ˡ�ϡ�XML�ꥹ�Ȥ���˹�¤ñ�̤���Ӥ�Ԥ�����Ӳ���򸺤餹���Ȥǹ�®����������롣
# �����ͤϡ��ե����뤫����ɤ߹��߻��˹Ԥ���


#    def initialize - �����
#    def child_create(name, byte) - ����˻ҳ��ؾ�����ɲ�
#    def convert_xml_path_to_patharray(xmlpath) - XML�ѥ������������XML�ѥ�������Ѵ�
#    def get_data_from_xmlpatharray(xmlpatharray) - ����XML�ѥ�����ε������֤ȥХ��ȿ��μ���
#    @flg - �ե饰(0=�ǡ������ޤ��ϡ��ǡ����ʤ�, 1=°��)
#    @name - �������ؤΥ���̾ [String��]
#    @position - �������ؤΥ������󥷥��ե�����ε������� [Numeric��]
#    @byte - �������ؤΥǡ����ΥХ��ȿ� [Numeric��]
#    @seq_num - �����ֹ� [Numeric��](CSV���Ϥ˻���)
#    @hirlist_value - �ҳ��ؾ�����礭�� [Numeric��]
#    @hirlist - �ҳ��ؾ��� [����(���֥�������)��]


# ���إǡ����ꥹ�ȥ��饹
class HierarcydataList

	# ------------------------------------------------------------
	# �����
	def initialize
		@flg = 0			# �ե饰(0=�ǡ������ޤ��ϡ��ǡ����ʤ�, 1=°��)
		@name = ''			# �������ؤΥ���̾ [String��]
		@position = 0		# �������ؤΥ������󥷥��ե�����ε������� [Numeric��]
		@byte = 0			# �������ؤΥǡ����ΥХ��ȿ� [Numeric��]
		@seq_num = 0		# �����ֹ� [Numeric��](CSV���Ϥ˻���)
		@hirlist_value = 0	# �ҳ��ؾ�����礭�� [Numeric��]
		@hirlist = []		# �ҳ��ؾ��� [����(���֥�������)��]
	end


	# ------------------------------------------------------------

	# ����˻ҳ��ؾ�����ɲ�
	#    ����
	# name - ����̾���ޤ��ϡ�°��̾ [IN / String��]
	# flg - �ե饰 [IN / Numeric��]
	#          0 = �ǡ������ޤ��ϡ��ǡ����ʤ�
	#          1 = °��
	# position - ����������°����񤭹��൯������ [Numeric��]
	# byte - �񤭹���Х��ȿ� [Numeric��]
	# seq_num - �����ֹ� [Numeric��]
	#    �����
	# �ʤ�
	def childadd(name, flg, position, byte, seq_num = 0)
		hir_newlist = HierarcydataList.new
		hir_newlist.name = name
		hir_newlist.flg = flg
		hir_newlist.position = position
		hir_newlist.byte = byte
		hir_newlist.seq_num = seq_num
		@hirlist.push hir_newlist
		@hirlist_value += 1
		hir_newlist = nil
	end


	# ------------------------------------------------------------

	# XML�ѥ������������XML�ѥ�������Ѵ�
	#    ����
	# xmlpath - �Ѵ�����XML�ѥ� [IN / String��]
	#    �����
	# XML�ѥ�����
	#    ����
	# ���δؿ��ϡ����Хѥ����θ�������ˤϤʤäƤ��ޤ���Τǡ�
	# ���Хѥ��ǵ��Ҥ��Ƥ�����ϡ����Хѥ����Ѵ����Ƥ���������
	#    ���
	# ���С��ؿ��Ȥ��Ƥϡ����ޤ깥�ޤ����ʤ�����
	# ��������ǤΤ��Ȥ������ˤ��Ƥ���Τǡ��Ȥꤢ�������С��ؿ��Ȥ��ƺ�������
	# �����̤Υ��饹��Ʊ�����С��ؿ�������Τǡ������ؿ������Ƥ��ޤ��Τ�������
	def convert_xml_path_to_patharray(xmlpath)
		xml_array = [] ; xml_array_size = 0 ; w_xmlpath = ''
		# ����ζ����������ǡ���/�פζ��ڤ�����󲽤���
		w_xmlpath = xmlpath.strip
		xml_array = w_xmlpath.split(/\//)
		# �������Ƭ���������ʤ顢��������Τߺ��
		case	xml_array[0]
		when	nil, ''
			xml_array.delete_at(0)
		end
		xml_array_size = xml_array.size
		if xml_array[xml_array_size - 1] =~ /\S@\S/
			word1 = '' ; word2 = []
			word1 = xml_array[xml_array_size - 1]
			word2 = word1.split(/@/)
			# ����@�פ�����ʾ�и����뤳�ȤϹ�θ���Ƥ��ޤ���
			xml_array[xml_array_size - 1] = word2[0]	# ��@�פ�����򥻥å�
			xml_array.push('@' + word2[1])	# ��@�װʹߤ򥻥å�
		end
		return	xml_array
	end


	# ------------------------------------------------------------

	# ����XML�ѥ�����ε������֤ȥХ��ȿ��μ���
	#    ����
	# xmlpatharray - XML�ѥ����� [IN / ����(String)��]
	#    �����
	# ���ꤷ��XML�ѥ�����ε������֤ȥХ��ȿ�([]�ξ�硢�����Ǥ��ʤ��ä�) [����(Numeric)��]
	#  [0] - �������� [Numeric��]
	#  [1] - �Х��ȿ� [Numeric��]
	#    ����
	# ����������ϴؿ��¹Ը���ͤ��ݾڤ��ޤ���
	# ����XML�ꥹ�ȤΥǡ����������������Ǥ�
	def get_data_from_xmlpatharray(xmlpatharray)
		xml_tag = '' ; array_size = 0 ; hirlist_size = 0
		ret_data = []
		xml_tag = xmlpatharray.shift
		if xml_tag == nil
			# nil�ξ��֤ξ��ϡ��㳰��ȯ��������
			raise 'get_data_from_xmlpatharray���㳰��ȯ�����ޤ�����' + "\n" + '������XML�ѥ�����˥ǡ���������ޤ���'
			# �㳰��ȯ���Ǥ��ʤ������θ����
			return	nil
		end
		array_size = xmlpatharray.size
		hirlist_size = @hirlist.size
		if array_size > 0
			# ����XML�ꥹ�Ȥμ���
			for x in 1..hirlist_size do

# ��XML�ǡ������ɤ߹���������ǡ����������̾�����֤������Ƥ��뤳�Ȥ�����Ȥ��Ƥ��롣

				if @hirlist[x - 1].name == xml_tag
					# ���եȸ��XML�ѥ����������˻��ꤷ�ơ�����XML�Υǡ��������ؿ���ƤӽФ�
					ret_data = @hirlist[x - 1].get_data_from_xmlpatharray(xmlpatharray.clone)
					break	# forʸ����ȴ����
				end
				if x == hirlist_size
					# ����XML�ꥹ�Ȥ˳������륿�������Ĥ���ʤ��ä����ϡ�[]���֤���
					ret_data = []
				end
			end
		else
			# ����XML�ꥹ�Ȥϸ������ʤ�
			# ���ꤷ������̾���ͤ����
			if xml_tag[0, 1] == '@'
				# °���Υǡ��������Ǥ���
				attr_tag = xml_tag
#				attr_tag = ''
#				attr_tag = xml_tag[1..-1]	# ��ʸ���ܰʹߤ�°��̾�Ȥ��ƻ��Ѥ���
				for x in 1..hirlist_size do
					if @hirlist[x - 1].name == attr_tag
						# °��̾�����Ĥ��ä�
						ret_data.push @hirlist[x - 1].position	# �������֤򥻥å�
						ret_data.push @hirlist[x - 1].byte		# �Х��ȿ��򥻥å�
						break	# forʸ����ȴ����
					end
					if x == hirlist_size
						# ����XML�ꥹ�Ȥ˳������륿�������Ĥ���ʤ��ä����ϡ�[]���֤���
						ret_data = []
					end
				end
			else
				# �����Υǡ��������Ǥ���

# ��XML�ǡ������ɤ߹���������ǡ����������̾�����֤������Ƥ��뤳�Ȥ�����Ȥ��Ƥ��롣

				for x in 1..hirlist_size do
					if @hirlist[x - 1].name == xml_tag
						# ����̾�����Ĥ��ä�
						ret_data.push @hirlist[x - 1].position	# �������֤򥻥å�
						ret_data.push @hirlist[x - 1].byte		# �Х��ȿ��򥻥å�
						break	# forʸ����ȴ����
					end
					if x == hirlist_size
						# ����XML�ꥹ�Ȥ˳������륿�������Ĥ���ʤ��ä����ϡ�[]���֤���
						ret_data = []
					end
				end
			end
		end


		return ret_data
	end


	# ------------------------------------------------------------

	# ����XML�ѥ�����ε������֤ȥХ��ȿ��γ�Ǽ
	#    ����
	# xmlpatharray - XML�ѥ����� [IN / ����(String)��]
	# position - �������� [Numeric��]
	# byte - �Х��ȿ� [Numeric��]
	# seq_num - �����ֹ� [Numeric��]
	#    �����
	# �ʤ�
	#    ����
	# ����������ϴؿ��¹Ը���ͤ��ݾڤ��ޤ���
	# ����XML�ꥹ�ȤΥǡ����������������Ǥ�
	def put_data_from_xmlpatharray(xmlpatharray, position, byte, seq_num = 0)
		xml_tag = '' ; array_size = 0 ; hirlist_size = 0
		ret_data = nil	# 0 = ���ｪλ��

		xml_tag = xmlpatharray.shift
		if xml_tag == nil
			# nil�ξ��֤ξ��ϡ��㳰��ȯ��������
			raise 'get_data_from_xmlpatharray���㳰��ȯ�����ޤ�����' + "\n" + '������XML�ѥ�����˥ǡ���������ޤ���'
			# �㳰��ȯ���Ǥ��ʤ������θ����
			return	nil
		end
		array_size = xmlpatharray.size
		hirlist_size = @hirlist.size
		if array_size > 0

			# x1 = forʸ��
			x2 = 0	# �ե饰(0=�������륿�������Ĥ���ʤ��ä�,1=�ҳ��ؤ˳������륿�������Ĥ��ä�)
			x3 = 0	# ���Ĥ��ä����������ֹ�
			if hirlist_size > 0
				# ���˥�����¸�ߤ��뤫��Ĵ�٤�
				for x1 in 0..(hirlist_size - 1) do
					if @hirlist[x1].name == xml_tag
						# �������륿�����ҳ��ؤ��鸫�Ĥ��ä�
						x2 = 1	# �ե饰�Υ��å�
						x3 = x1	# �����ֹ�Υ��å�
						break	# forʸ����ȴ����
					end
				end
			end

			# ���åȤ��줿�ե饰�򸫤ơ��ҳ��ؤ��ä��ꡢ��¸�λҳ��ؤ򥿡����åȤˤ���
			case	x2
			when	0
				# ����XML�ꥹ�Ȥ˳������륿�������Ĥ���ʤ��ä����ϡ�������������롣

				# �ҳ��ؾ�����ɲ�(�������֤Ƚ񤭹���Х��ȿ��ϡ�ξ���Ȥ�0�ˤ���)
#				self.childadd(xml_tag, 0, 0, 0, seq_num)
				childadd(xml_tag, 0, 0, 0, seq_num)
				# ���������ҳ��ؤ�XML�Υǡ������Ǽ����
				ret_data = @hirlist[-1].put_data_from_xmlpatharray(xmlpatharray.clone, position, byte, seq_num)
				# ���Ǹ��������Ф��ƹԤ��Τǡ���������-1
				# �������ΤߤʤΤǡ�°��Ƚ��Ϥ��ʤ�
			when	1
				# ���եȸ��XML�ѥ����������˻��ꤷ�ơ�����XML�Υǡ�����Ǽ�ؿ���ƤӽФ�
				ret_data = @hirlist[x3].put_data_from_xmlpatharray(xmlpatharray.clone, position, byte, seq_num)
			else
				raise 'get_data_from_xmlpatharray���㳰��ȯ�����ޤ�����' + "\n" + '�ե饰(x2)���������ͤ����åȤ���ޤ�����x2 = [' + String(x2) + ']'
			end
		else
			# ����XML�ꥹ�Ȥϸ������ʤ�
			# ���ꤷ������̾���ͤ����
			if xml_tag[0, 1] == '@'
				# °���Υǡ��������Ǥ���
				attr_tag = xml_tag
#				attr_tag = ''
#				attr_tag = xml_tag[1..-1]	# ��ʸ���ܰʹߤ�°��̾�Ȥ��ƻ��Ѥ���

				# x1 = forʸ��
				x2 = 0	# �ե饰(0=�������륿�������Ĥ���ʤ��ä�,1=�ҳ��ؤ˳������륿�������Ĥ��ä�)
				x3 = 0	# ���Ĥ��ä����������ֹ�
				if hirlist_size > 0
					# ���˥���(°��)��¸�ߤ��뤫��Ĵ�٤�
					for x1 in 0..(hirlist_size - 1) do
						if @hirlist[x1].name == attr_tag
							# �������륿��(°��)���ҳ��ؤ��鸫�Ĥ��ä�
							x2 = 1	# �ե饰�Υ��å�
							x3 = x1	# �����ֹ�Υ��å�
							break	# forʸ����ȴ����
						end
					end
				end

				# ���åȤ��줿�ե饰�򸫤�
				# ���Ǥ˥���(°��)��¸�ߤ�����ϡ����顼��ȯ�������롣
				case	x2
				when	0
					# ����XML�ꥹ�Ȥ˳������륿��(°��)���ʤ��Τ��ǧ�����Τǡ��ҳ��ؤ�������롣
					childadd(xml_tag, 1, position, byte, seq_num)
				when	1
					# °��̾�����Ĥ��ä�
#					# ��񤭽񤭹���
#					@hirlist[x].position = position
#					@hirlist[x].byte = byte
					# ��񤭽񤭹��ߤ򤻤��ˡ����顼��ȯ��������
					raise 'put_data_from_xmlpatharray���㳰��ȯ�����ޤ�����' + "\n" + 'Ʊ��XML�ѥ����Ф��ơ���񤭽񤭹��ߤ��褦�Ȥ��ޤ�����' + "\n" + 'XML�ѥ����� = [' + xml_tag + ']'
				else
					raise 'get_data_from_xmlpatharray���㳰��ȯ�����ޤ�����' + "\n" + '�ե饰(x2)���������ͤ����åȤ���ޤ�����x2 = [' + String(x2) + ']'
				end
			else
				# �����Υǡ��������Ǥ���

# ��XML�ǡ������ɤ߹���������ǡ����������̾�����֤������Ƥ��뤳�Ȥ�����Ȥ��Ƥ��롣

				# x1 = forʸ��
				x2 = 0	# �ե饰(0=�������륿�������Ĥ���ʤ��ä�,1=�ҳ��ؤ˳������륿�������Ĥ��ä�)
				x3 = 0	# ���Ĥ��ä����������ֹ�
				if hirlist_size > 0
					# ���˥�����¸�ߤ��뤫��Ĵ�٤�
					for x1 in 0..(hirlist_size - 1) do
						if @hirlist[x1].name == xml_tag
							# �������륿�����ҳ��ؤ��鸫�Ĥ��ä�
							x2 = 1	# �ե饰�Υ��å�
							x3 = x1	# �����ֹ�Υ��å�
							break	# forʸ����ȴ����
						end
					end
				end

				# ���åȤ��줿�ե饰�򸫤�
				# ���Ǥ˥�����¸�ߤ�����ϡ����顼��ȯ�������롣
				case	x2
				when	0
					# ����XML�ꥹ�Ȥ˳������륿�����ʤ��Τ��ǧ�����Τǡ��ҳ��ؤ�������롣
					childadd(xml_tag, 0, position, byte, seq_num)

				when	1
					# ����̾�����Ĥ��ä�
					if (@hirlist[x3].position == 0) and (@hirlist[x3].byte == 0)
						# �ޤ�����񤫤�Ƥ��ʤ������Ǥ���
						# ��񤭽񤭹���
						@hirlist[x3].position = position
						@hirlist[x3].byte = byte
					else
						# ���Ǥ˽񤭹��ޤ�Ƥ��륿���Ǥ���
						# ��񤭽񤭹��ߤ򤻤��ˡ����顼��ȯ��������
						raise 'put_data_from_xmlpatharray���㳰��ȯ�����ޤ�����' + "\n" + 'Ʊ��XML�ѥ����Ф��ơ���񤭽񤭹��ߤ��褦�Ȥ��ޤ�����' + "\n" + 'XML�ѥ����� = [' + xml_tag + ']'
					end
				else
					raise 'get_data_from_xmlpatharray���㳰��ȯ�����ޤ�����' + "\n" + '�ե饰(x2)���������ͤ����åȤ���ޤ�����x2 = [' + String(x2) + ']'
				end

			end
		end


		return ret_data
	end


	# ------------------------------------------------------------
	# ���֥������ȳ�����Υǡ������С��ؤΥ����������ǽ�ˤ���
	attr_accessor :flg, :name, :position, :byte, :seq_num, :hirlist_value, :hirlist
end



# =============================================================================


# XML�ꥹ�ȥ��饹
# ���Υ��饹�ϡ�XML�ǡ�����ĥ꡼��¤����¸�����ĥ꡼�������뤳�Ȥǹ�®�����뤿��Υ��饹�Ǥ�

# ���С��ؿ�
#   �����
#   XML�ǡ����μ�����
#   XML�ѥ�����Υǡ�������

# XML�ꥹ�ȥ��饹
class Xml_listdata

	# �ǡ������С�
	attr_accessor :xmllist


	# ------------------------------------------------------------
	# �����
#	def initialize(x)
	def initialize
		# xml�Υꥹ�ȥǡ����ν����
		@xmllist = nil
	end


	# ------------------------------------------------------------
	# XML�ǡ����μ�����
	#    ����
	# xml_data - ������XML�ǡ��� [IN / String��]
	# out_word_code - ����ʸ�������� [IN / String��]
	#                    '1' - EUC������
	#                    '2' - ���ե�JIS������
	#    �����
	# XML�ꥹ�ȴ��ܥǡ������֥�������(nil�ξ�硢�����ߤǤ��ʤ��ä�)
	#    ����
	# ���ΥС������Ǥϡ�XML�ѥ����Ѵ�����������ä�����
	#   �����XML�ꥹ�ȥ��֥������Ȥ���������������Ȥ�
	#   ����ˤ�ꡢ����θ����оݤιʤ����
	def xmldata_setup(xml_data, out_word_code)
		ret_data = nil

		idx1 = 0	# �إå�Ƚ��ǻ��Ѥ���
		idx2 = 0	# ���󲽽����ǻ��Ѥ���
		idx3 = 0	# ���󲽽����ǻ��Ѥ���


		flg1 = 0	# ���󲽽����ǻ��Ѥ���

		now_xmllist = nil	# ���ߥ������åȤˤ��Ƥ��륿����xmllist���֥�������
		xmllist_stack = []	# ����Ҥ��б����뤿���xmllist���֥������ȤΥ����å��ΰ�

		wd_name1 = ''	# ̾�ΤΥ���ΰ�(�����ֹ�ʤ�)
		wd_name2 = ''	# ̾�ΤΥ���ΰ�(�����ֹ椢��)
		wd_name3 = ''	# ̾�ΤΥ���ΰ�(�����ֹ椢��)

		wd_atrname1 = ''	# °��̾�ΤΥ���ΰ�
		wd_atrdata1 = ''	# °���ǡ����Υ���ΰ�

		wd_cdata1 = ''	# �����ǡ����Υ���ΰ�

		w_xml1 = ''	# XMLʸ����γ�Ǽ���
		i_encoding = ''	# ����ʸ����δ���������
		o_encoding = ''	# ����ʸ����δ���������


		# ����ʸ����δ��������ɤΥ��å�
		case String(out_word_code)
		when '1'
			o_encoding = 'EUC-JP'
		when '2'
			o_encoding = 'Shift_JIS'
		when /EUC-JP/i
			o_encoding = 'EUC-JP'
		when /Shift_JIS/i
			o_encoding = 'Shift_JIS'
		end


		# �����ܤμ���
		idx1 = xml_data.index("\n")
		w_xml1 = xml_data[0, idx1]

		# �إå����Ѵ�
		# ������ : UTF��Ƚ�����˹Ԥ�����()
		# encoding�ε��Ҥ��ʤ���С�UTF-8�Ȥ��ư���
		if w_xml1 =~ /^.*?(<\?xml\sversion\s*?=\s*?\"[\.\d]+?\")\s*?(\?\>)/i 
#		if w_xml1 =~ /^.*?(<\?xml\sversion\s*?=\s*?\".+?\")\s*?(\?\>)/i 
			w_xml1 = $1 + " encoding=\"UTF-8\"" + $2 + "\n"
		end
		if w_xml1 =~ /^.*?(<\?xml\sversion\s*?=.+\sencoding\s*?=\s*?.UTF-8.*?)/i
			w_xml1.gsub!(/UTF-8/i, "UTF-8")
			i_encoding = "UTF-8"
		end
		if w_xml1 =~ /^<\?xml\sversion\s*?=.+\sencoding\s*?=\s*?.EUC-JP./i
			w_xml1.gsub!(/EUC-JP/i, "UTF-8")
			i_encoding = "EUC-JP"
		end
#		if w_xml1 =~ /^<\?xml\sversion=.+\sencoding=.Shift_JIS./i
		if w_xml1 =~ /^<\?xml\s*?version\s*?=.+\sencoding\s*?=\s*?.Shift_JIS./i
			w_xml1.gsub!(/Shift_JIS/i, "UTF-8")
			i_encoding = "Shift_JIS"
		end
		if w_xml1 =~ /^<\?xml\s*?version\s*?=.+\sencoding\s*?=\s*?.JIS./i
			w_xml1.gsub!(/JIS/i, "UTF-8")
			i_encoding = "JIS"
		end

		# �Ĥ��ʸ�����������ơ��Ѵ�
		w_xml1 += xml_data[idx1 .. -1]
		case i_encoding
		when "EUC-JP"
			w_xml1 = euctou8(w_xml1)
		when "Shift_JIS"
			w_xml1 = Uconv.euctou8(Kconv::toeuc(w_xml1))
#			w_xml1 = sjistou8(w_xml1)
		when "JIS"
			w_xml1 = Uconv::euctou8(Kconv::toeuc(w_xml1))
		when "UTF-8"
#			w_xml1 = w_xml1
			w_xml1 = euctou8(Uconv.u8toeuc(w_xml1))
#			if w_xml1[0] != '<'
#				w_xml1 = w_xml1[3 .. -1]		# ���ߤ������äƤ����顢������
#			end
		else
			# ̤�б���ʸ�������ɤʤΤǡ��㳰��ȯ��������
			raise '���Ϥ��줿ʸ�������ɤ��б����Ƥ��ޤ��� [' + String(i_encoding) + ']'
		end


		# XML�ꥹ�ȴ��ܥ��饹������
		@xmllist = Xml_baselist.new


		parser = XMLParser.new
		def parser.default
		end

		now_xmllist = xmllist

		parser.parse(w_xml1) do |type, name, data|
			# ----------------------------------------
			case type
			when XMLParser::START_ELEM
				# ���ϥ���
				case o_encoding
				when "EUC-JP"
					wd_name1 = Uconv.u8toeuc(name)
				when "Shift_JIS"
					wd_name1 = Kconv::tosjis(Uconv.u8toeuc(name))
#				when "UTF-8"
#					wd_name1 = name
				end

				# ���ߤΥ���̾������
				idx2 = 1	# �����ֹ�ν����(1���ꥸ��)
				if now_xmllist.list_value > 0
					# ����XML�ꥹ�Ȥ�����Τǡ�̾�������֤�ʤ���������å�
					flg1 = 0	# ������̥ե饰�ν����(0 = ������³, 1 = ������λ)
					while flg1 == 0 do
						flg1 = 1	# �ǥե���ȤǸ�����λ�Ȥ���
						wd_name3 = wd_name1 + '[' + String(idx2) + ']'	# �������륿��̾
						for idx3 in 0..(now_xmllist.list_value - 1) do
							if now_xmllist.xmllist[idx3].name == wd_name3
								# ���פ���̾�������Ĥ��ä�
								flg1 = 0
								idx2 += 1	# �����ֹ��û�����
								break	# forʸ����ȴ����
							end
						end
					end
				end
				# ����̾�������ֹ���ղä���
				wd_name2 = wd_name1 + '[' + String(idx2) + ']'


				xmllist_stack.push now_xmllist	# �����å��ˡ�ľ����xmllist���֥������Ȥ�����

				# ����xmllist�κ���
				now_xmllist = now_xmllist.childadd(wd_name2, 0, nil)	# data�����Ƥˤϡ��ǡ����ʤ������


				# °���Υ��å�
				# ���嵭������xmllist����ˡ�°���ǡ����򥻥åȤ���
				#
				data.each do |key, value|
					wd_atrname1 = '' ; wd_atrdata1 = ''		# �ǥХå����Ѥ����ä�����
					case o_encoding
					when "EUC-JP"
						wd_atrname1 = Uconv.u8toeuc(key)
						wd_atrdata1 = Uconv.u8toeuc(value)
					when "Shift_JIS"
						wd_atrname1 = Kconv::tosjis(Uconv.u8toeuc(key))
						wd_atrdata1 = Kconv::tosjis(Uconv.u8toeuc(value))
#					when "UTF-8"
#						wd_atrname1 = key
#						wd_atrdata1 = value
					end
					# ����xmllist��°���ǡ����򥻥å�
					now_xmllist.childadd(('@' + wd_atrname1), 1, wd_atrdata1)
				end

			# ----------------------------------------
			when XMLParser::END_ELEM
				# ��λ����
				# �оݥ����򡢤ҤȤľ�̤Υ������᤹
				now_xmllist = xmllist_stack.pop

			# ----------------------------------------
			when XMLParser::CDATA
				# �����Υǡ�������
				if data != nil
					# data�����Ƥ�nil�ǤϤʤ�
					case o_encoding
					when "EUC-JP"
						wd_cdata1 = Uconv.u8toeuc data
					when "Shift_JIS"
#						wd_cdata1 = Uconv.u8toeuc data
#						wd_cdata1 = Kconv::tosjis c_data
						wd_cdata1 = Kconv::tosjis(Uconv.u8toeuc(data))
#						wd_cdata1 = Uconv.u8tosjis data
#					when "UTF-8"
#						wd_cdata1 = data
					end
					wd_cdata1.strip!	# ����ζ��������
					# ���Ƥ�����ξ��ϡ�nil�ˤ���
					wd_cdata1 = nil			if wd_cdata1 == ''
				else
					# data�����Ƥ�nil�Ǥ���
					wd_cdata1 = nil
				end
				if wd_cdata1 != nil
					# data��nil�Ǥʤ���硢�оݥ��֥������Ȥ����Ƥ򥻥å�
					now_xmllist.data = wd_cdata1
				end
			# ----------------------------------------
			when XMLParser::PI
#			when XMLParser::START_DOCTYPE_DECL
#			when XMLParser::END_DOCTYPE_DECL
			when XMLParser::DEFAULT
			else
			end
		end


		return ret_data
	end


	# ------------------------------------------------------------
	# XML�ѥ��Υǡ����μ���(���Хѥ������Τ�)
	#    ����
	# xmlpath - ����������XML�ѥ� [IN / String��]
	#    �����
	# ���ꤷ��XML�ѥ�������(nil�ξ�硢�����Ǥ��ʤ��ä�)
	#    ����
	# ������XML�ѥ��ϴؿ��¹Ը���ͤ��ݾڤ��ޤ���
	# ����XML�ꥹ�ȤΥǡ����������������Ǥ�
	# �����˻��ꤹ��Τϡ����Хѥ������ǹԤäƤ�������
	#   ���Хѥ��ˤ��б����Ƥ��ޤ���
	def get_xmlpath_data(xml_path)
		return (@xmllist.get_data_from_xmlpatharray(@xmllist.convert_xml_path_to_patharray(xml_path)))
	end


	# ------------------------------------------------------------
	# XML�ǡ��������������γ�ǧ(�ǥХå���)
	#    ����
	# show_mode - ɽ���⡼�� [IN / Numeric�� / �ǥե������ = 10]
	#                10 - �ǥХå�������ɽ���⡼��
	#    �����
	# �ʤ�
	#    ����
	# ���Υ��С��ؿ��ϡ������ǡ�������������Ǽ����Ƥ��뤫���ǧ���뤿��˺������ޤ���
	# ���δؿ����ΤΥǥХå��⡢�����դ��뤳��
	def print_xmllist(show_mode = 10)
		now_xmllist = nil			# ���ߤ�XML�ꥹ�ȥ��֥�������
		now_xmllist_index = 0		# ���ߤ��о�����XML�ꥹ�ȥ��֥������ȤΥ���ǥå�����
		xmllist_stack = []			# XML�ꥹ�ȥ��֥������ȤΥ����å�
		xmllist_index_stack = []	# XML�ꥹ�ȥ��֥������ȤΥ���ǥå����ͤΥ����å�

		case	show_mode
		# ----------------------------------------
		# �ǥХå�������ɽ���⡼��
		when	10
			now_xmllist = @xmllist
			now_xmllist_index = 0
			if now_xmllist == nil
				puts 'XML�ꥹ�ȥ��֥������Ȥ�nil�Ǥ�'
			elsif now_xmllist.list_value == 0
				# XML�ꥹ�Ȥο������Ǥ���
				puts 'XML�ꥹ�ȥǡ�����¸�ߤ��ޤ���'
			else
				# XML�ꥹ�Ȥ�ɽ������
				while true do
					case	now_xmllist.flg
					when	0
						# �������ޤ��ϡ��ǡ����ʤ�
						if now_xmllist_index == 0
							# ����ǥå�����0�λ��Τ�ɽ������
							# (��XML�ꥹ�Ȥ�����äƤ����Ȥ��ˤ�ɽ�������Τ��ɤ�����)
							case	now_xmllist.name
							when	nil
								# ����̾��°��̾��nil�����åȤ���Ƥ���
								puts '����̾��nil�Ǥ�'
							when	''
								# ����̾��°��̾�˶��򤬥��åȤ���Ƥ���
								puts '����̾������Ǥ�'
							else
								# ����̾��°��̾��ʸ���󤬥��åȤ���Ƥ���
								case	now_xmllist.data
								when	nil, ''
									# �����ΤߤǤ���
								else
									# ���Ƥ�¸�ߤ���
									puts ':' + now_xmllist.name + ' = [' + now_xmllist.data + ']'
								end
							end
						end

					when	1
						# °��
						case	now_xmllist.name
						when	nil
							# °��̾��nil�����åȤ���Ƥ���
							puts '°��̾��nil�Ǥ�'
						when	''
							# °��̾�˶��򤬥��åȤ���Ƥ���
							puts '°��̾������Ǥ�'
						else
							# °��̾��ʸ���󤬥��åȤ���Ƥ���
							case	now_xmllist.data
							when	nil
								# °����nil�����äƤ���
								puts ':' + now_xmllist.name + ' = [nil]'
							when	''
								# °���˶������äƤ���
								puts ':' + now_xmllist.name + ' = [����]'
							else
								# ���Ƥ�¸�ߤ���
								puts ':' + now_xmllist.name + ' = [' + now_xmllist.data + ']'
							end
						end
					else
						puts 'XML�ꥹ�ȤΥե饰���������ͤ����åȤ���Ƥ��ޤ�'
						puts '   flg = [' + String(now_xmllist.flg) + ']'
					end

					# ����XML�ꥹ�Ȥ�����С������å��˥���ǥå����ȥ��֥������Ȥ򥻥åȤ��ơ����ߤ�XML�ꥹ�ȥ��֥��������ΰ�ˡ���XML�ꥹ�Ȥ򥻥åȤ���
					if now_xmllist_index < now_xmllist.list_value
						# ���ߤ����Ƥ򥹥��å��˥��å�
						xmllist_stack.push now_xmllist
						xmllist_index_stack.push now_xmllist_index
						# ���ߤ����Ƥ򡢻�XML�ꥹ�Ȥˤ���
						now_xmllist_index = 0
						now_xmllist = now_xmllist.xmllist[0]
						puts '>' + now_xmllist.name
					else
						# ��XML�ꥹ�Ȥ��ʤ��Τǡ����Υ��֥������Ȥˤ���
						while	((now_xmllist != nil) && (now_xmllist_index >= now_xmllist.list_value)) do
							puts '<' + now_xmllist.name
							now_xmllist_index = xmllist_index_stack.pop
							now_xmllist_index += 1	# ����XML�ꥹ�Ȥ˰ܤ�
							now_xmllist = xmllist_stack.pop
						end
						# �Ǿ�̤�XML�ꥹ�Ȥ���ȴ�����顢whileʸ����ȴ���ƽ�����λ����
						if now_xmllist == nil
							puts 'XML End'
							break
						end
					end
				end
			end
		# ----------------------------------------
		end

		nil
	end


	# ------------------------------------------------------------
	# LineTarget�������
	#    ����
	# hierarcy_data - LineTarget���������Ԥ����ؾ��� [IN / String��]
	#    �����
	# nil - ���顼��ȯ������
	# nil�ʳ� - �Ѵ���γ��ؾ���
	#    ����
	# ���Ȥ���XML�ꥹ�ȥǡ����ϡ�����XML���֥������Ȥ�XML�ꥹ�ȥǡ����Ǥ�
	# �¹����ˡ������Ȥ򳰤����ꡢ̵�̤ʶ���Ԥ������Ƥ�������
	# ���Υ��С��ؿ��ϡ����ؾ��󥪥֥������Ȥ����ä���硢
	#   ������Υ��С��ؿ��˴ޤ�٤���Ρ�
	#   �⤷�⡢���ؾ��󥪥֥������ȤΥ��饹������������ϡ�
	#   �������ؾ��󤫤�XML�ꥹ�ȥ��֥������Ȥ��Ѥ��뤳�ȡ�
	# ���Хѥ����Ѵ��Ǥ��ޤ���
	def exec_LineTargetDefinition(hierarcy_data)
		ret_data = nil ; h_data = ''
		define_startline = '' ; define_symbol = '' ; define_param = ''
		define_endline = ''

		h_data1 = hierarcy_data.clone	# ǰ�Τ��ᡢ�������ΰ����ݤ��ƤΥ��ԡ�

		while h_data1 =~ /^(\s*?\$LineTargetStart\s*?,\s*?(\S+?)\s*?,(.+?)\n)/i
			# LineTarget������ѥ�᡼���ǡ�����¸�ߤ���
			# $1 = LineTarget������и�������
			# $2 = LineTarget����Υ���ܥ�̾
			# $3 = ����ܥ�̾�����Υѥ�᡼������
			define_startline = $1
			define_symbol = $2
			define_param = $3

			# LineTarget����ν�λ���֤μ���
			if h_data1 =~ /^(\s*?\$LineTargetEnd\s*?,\s*?#{define_symbol}\s*?)/i
				# $1 = ��λLineTargetEnd������и�������
				define_endline = $1
			else
				$stderr.puts '$LineTargetEnd�����¸�ߤ��ޤ��� (����ܥ�̾ = ' + String(define_symbol) + ')'
				exit 2
			end

			# �����������Ƥ�����ɽ���ǻȤ���褦���Ѵ�
			define_startline = Regexp.quote(define_startline)
			define_endline = Regexp.quote(define_endline)

			define_before = ''	# LineTarget����Ѵ������о�
			define_after = ''	# LineTarget����Ѵ�����о�
			if h_data1 =~ /(#{define_startline}([\s\S]*?)#{define_endline})/
				# $1 - �ִ����о�
				# $2 - �ִ����о�
				define_before = $1.clone			# �ִ������Ƥγ�Ǽ�ΰ�
 				define_after = $2.clone			# �ִ������Ƥγ�Ǽ�ΰ�
			end


			# �ѥ�᡼������μ���
			w_arg1 = []
			w_arg2 = []
			w_arg1 = define_param.split ','		# ��,�׶��ڤ��ȴ���Ф�
			w_arg1.each_index do |w_idx1|
				# ������ζ���������
				w_arg1[w_idx1].strip!
			end
			w_arg2 = w_arg1[0].split '='	# �����ܤΰ����Τߡ�=�׶��ڤ�Ǥ����ȴ���Ф�
			w_arg2.each_index do |w_idx2|
				# ������ζ���������
				w_arg2[w_idx2].strip!
			end
			base_hierarcy = w_arg1[2]			# ��೬�ؤμ���
			mean_decision = '='					# Ƚ����ˡ�ʸ��ߡ���=�פǸ����
			decision_hierarcy = w_arg2[0]		# Ƚ�곬��
			decision_value = w_arg2[1]			# Ƚ������
			if decision_value[0, 1] == "\"" and decision_value[-1, 1] == "\""
				decision_value = decision_value[1, (decision_value.size - 2)]
			end
			arg_num = Integer(w_arg1[1]) - 1	# �о������ֹ�(���ؾ���ե�����ˤϡ������ꥸ��ǵ���)

			# +++++
			find_flg1 = 0	# �����ե饰
			# +++++


			# Ƚ�곬�ؤ�̾�Τ����Ƥ����
			w_arg3 = []
			w_arg3 = @xmllist.get_data_from_xmlpatharray_reg(@xmllist.convert_xml_path_to_patharray(decision_hierarcy))
			if w_arg3 == nil or w_arg3.size == 0
				# +++++
				find_flg1 = 1	# �����ե饰�Υ��å�
				# +++++
#				$stderr.puts '���إǡ�����¸�ߤ��ޤ��� (Ƚ�곬�� = ' + String(decision_hierarcy) + ')'
#				exit 2
			end

			# �����������Ƥ�Ƚ���������
			# ���Ƥ�LineTarget�ΰ����Ρ����ơפ���Ӥ���Ʊ��ʪ�򥹥��å��˥��å�
			# +++++
			if find_flg1 == 0
			# +++++
				h_name_arg1 = []
				h_value_arg1 = []
				w_arg3.each do |e1|
					if e1[1] == decision_value
						# �������Ƥ�Ƚ�����Ƥ����פ����Τǡ�����˥��åȤ���
						h_name_arg1.push e1[0]
						h_value_arg1.push e1[1]
					end
				end
				if h_value_arg1 == nil or h_value_arg1 == []
					# +++++
					find_flg1 = 2	# �����ե饰�Υ��å�
					# +++++
#					$stderr.puts '���ꤵ�줿���ؤ˳��������Ƥ�¸�ߤ��ޤ��� (Ƚ�곬�� = ' + String(decision_hierarcy) + ') = (' + decision_value + ')'
#					exit 2
				end
			# +++++
			end
			# +++++


			# �������å���������
			# �����Ρ֣����ܡפ�����������Ĥ��ä���Τ��ѿ��ΰ�˥��å�
			# +++++
			if find_flg1 == 0
			# +++++
				h_name1 = ''
				h_value1 = ''
				h_name1 = h_name_arg1[arg_num]
				h_value1 = h_value_arg1[arg_num]
				if h_name1 == nil
					# +++++
					find_flg1 = 3	# �����ե饰�Υ��å�
					# +++++
#					$stderr.puts '���ꤵ�줿����γ��ؤ˳��������Ƥ�¸�ߤ��ޤ��� (Ƚ�곬�� = ' + String(decision_hierarcy) + ') = (' + decision_value + ') (���� = ' + String(arg_num) + ')'
#					exit 2
				end
			# +++++
			end
			# +++++


			# +++++
			if find_flg1 == 0
			# +++++
				# ���ؤ����Ĥ��ä�
				# ���λ����ǡ�h_name1�˳������볬�ؾ������äƤ���

				# LineTarget�δ�೬�ؤ���ӡʹ��ʤ����ϡ���̿Ū���顼�ˤ���
				# ���θ塢��೬����ʬ������ɽ���Ȱ��פ��볺�����ؤȡ�
				# �����������δ֡�LineTarget�����ޤ�ʤ��ˤ��ѿ����Ф��ơ��ִ���Ԥ���
				# ���θ塢���ؾ������Τ��Ф��ơ��ִ�������Ԥ���
				# ���ִ������ϡ�gsub��Ȥ�ʤ���ˡ����ѡ�

#				h_name2 = exchange_hierarcy_name(1, base_hierarcy)		# ��೬�ؤ��Ѵ�����
				# ��೬�ؤ�����ɽ���ǻ���Ǥ���褦���Ѵ�����
				h_name2 = Regexp.quote(base_hierarcy).gsub /\\\[x\\\]/i, '\[[0-9]+?\]'

				# Ʊ����Τ����Ĥ��ä���硢��೬�ؤ����ɽ���򡢼�ɽ�����Ѵ�����
				if h_name1 =~ /(#{h_name2})/
					# ��೬�ؤ�����ɽ����Ƚ�곬�ؤ����פ���
					h_name3 = $1	# ��೬�ؤμ³���̾�μ���
					# ��೬��̾�ȼ³���̾���ִ�����
					define_after.gsub! /#{Regexp.quote(base_hierarcy)}/, h_name3
					# ���Τ��Ф��Ƥ��ִ�����(�����������ִ������Ǥ���)
#					h_data1.gsub! /#{Regexp.quote(define_before)}/, define_after
#					h_data1.gsub! Regexp.quote(define_before), define_after

					w_idx1 = 0 ; w_idx2 = 0 ; w_idx3 = 0
					w_idx1 = h_data1.index define_before		# �Ѵ���ʸ����򸡺�����
					w_idx2 = define_before.size
					w_idx3 = h_data1.size
					h_data1 = h_data1[0 .. (w_idx1 - 1)] + define_after + h_data1[(w_idx1 + w_idx2) ... w_idx3]


				else
					# ��೬�ؤ�����ɽ����Ƚ�곬�ؤ����פ��ʤ��ä�
					$stderr.puts 'Ƚ�곬�ؤϡ���೬�ؤ���ˤʤ���Фʤ�ޤ��� (Ƚ�곬�� = ' + String(decision_hierarcy) + '), (��೬�� = ' + base_hierarcy + ')'
					exit 2
				end
			# +++++
			else
				# ���ؤ����Ĥ���ʤ��ä�
# �����ޤޤǡ������Ǻ����������å����Ȥ߹���Ǥ���������������ȡ�
#   ���ؾ���˵������־��󤬤ʤ����ᡢ����ʥХ���ȯ�����롣
#   �н���ˡ�ϡ�����ʸ����XML�ǡ������֤���������ˡ��Ȥ�

				w_define_after1 = []
				w_define_after1 = define_after.split "\n"

				w_define_after1.each_index do |w_aryidx1|
					if w_define_after1[w_aryidx1] =~ /^\s*?\S+?\s*?\=\s*?([\S\s]+?)\s*?$/
						# $1 - �Х��ȿ�����ʸ����
						w_define_after1[w_aryidx1] = $nodata_word + '=' + $1
					end
				end
				define_after = w_define_after1.join "\n"
				# �Ǹ��ʸ�������ԤǤʤ����ϡ����Ԥ��ɲä���
				if define_after[-1, 1] != "\n"
					define_after += "\n"
				end
				w_idx1 = 0 ; w_idx2 = 0 ; w_idx3 = 0
				w_idx1 = h_data1.index define_before		# �Ѵ���ʸ����򸡺�����
				w_idx2 = define_before.size
				w_idx3 = h_data1.size
				h_data1 = h_data1[0 .. (w_idx1 - 1)] + define_after + h_data1[(w_idx1 + w_idx2) ... w_idx3]
				# �����ϡ������Υ�����
				# ----------
# 				# �����������Ƥ������Ƥ��ޤ�(���ؤ��ʤ��Ȥߤʤ�)
# #				h_data1.gsub! /#{Regexp.quote(define_before)}/, ''
# 				# �嵭gsub��Ȥ��ȡ�Ĺ�����ư۾ｪλ����Τǡ��ʲ�����ˡ������
# 				w_idx1 = 0 ; w_idx2 = 0 ; w_idx3 = 0
# 				w_idx1 = h_data1.index define_before		# �Ѵ���ʸ����򸡺�����
# 				w_idx2 = define_before.size
# 				w_idx3 = h_data1.size
# 				h_data1 = h_data1[0 .. (w_idx1 - 1)] + h_data1[(w_idx1 + w_idx2) ... w_idx3]
				# ----------
			end
			# +++++

		end		# whileʸ
				# �����ޤǤϡ�LineTarget����β��ʬ�����֤���whileʸ��


# ǰ�Τ��ᡢ���ޤޤǤΥ����ɤˤ��ä�����ݳ��ؾ���(***[x])��³��ؾ�����Ѵ��פ��Ȥ߹���
# # ���Υ����ɤ˰�̣������Τ�����
# # ��ǡ��ǥХå������ɤ��Ȥ߹���ǡ��Τ���Ƥߤ뤳��

		# ��ݳ��ؾ����³��ؾ�����Ѵ�
		while h_data1 =~ /^\s*?(.*?\([xX]\).*?)\s*?=.*?\n/
			# $1 = ����̾
			w_hname = $1.strip
			r1_data = []
			r1_data = @xmllist.get_data_from_xmlpatharray_reg(@xmllist.convert_xml_path_to_patharray(w_hname))
			if r1_data == nil or r1_data.size == 0
				# ���ꤵ�줿���إǡ����ϡ�¸�ߤ��ʤ�
				$stderr.puts '���ꤵ�줿���إǡ�����¸�ߤ��ޤ��� {' + String(w_hname) + ']'
				exit 2
			end
			r2_data = r1_data[0]
			w_hname = Regexp.quote(w_hname)	# ����ɽ���˻Ȥ���������Ѵ�
			h_data1.gsub! /#{w_hname}/, r2_data[0]	# �ִ�����
		end


		return	h_data1
	end



	# ------------------------------------------------------------
	# ���ؾ���ǡ������Ѵ�(�ե�å�����ǡ�������)
	#    ����
	# hierarcy_data - ���ؾ���ǡ��� [IN / String��]
	# out_word_code - ����ʸ�������� [IN / String��]
	#                    '1' - EUC������
	#                    '2' - ���ե�JIS������
	#    �����
	# []�ξ��
	# []�Ǥʤ����
	#   [x][0] - XML�ѥ� [OUT / String��]
	#   [x][1] - �Х��ȿ� [OUT / Integer��]
	#    ����
	# ���Υ��С��ؿ��Ǥϡ��������Ϥ������γ��ؾ���ǡ������Ф���
	# LineTarget���������Ԥ���XML�ѥ��ȥХ��ȿ���ʬ����������������Ƥ��֤��ޤ�
	# �¹Ը�ΰ����γ��ؾ���ǡ������ͤ��ݾڤ���ޤ���
	def exchange_hierarcy_to_harray(hierarcy_data, out_word_code)
		ret_data = []
		h_data0 = ''
		h_data1 = ''
		h_data2 = []
		xml_path1 = ''
		seq_byte1 = ''
		w_idx1 = 0	# ���顼ɽ������Ω�Ƥ뤿����ΰ�



		# �С�����󡦸�������å����Ѵ�
		first_line = ''	# �����ܤ����Ƥ򥻥åȤ����ΰ�
		hfdef_flg = []	# ��Ƭ������Ϥ������Ƥ򥻥åȤ����ΰ�
		hierarcy_data =~ /^([^\n]+?)\n/
		first_line = $1	# �����ܤ����Ƥ򥻥å�
		# ��Ƭǧ�������μ¹�
		hfdef_flg = realize_topline(first_line)
		if hfdef_flg[0] != 5
			$stderr.puts '�����˻��ꤵ�줿���إե�����Υإå������Ƥ���̵���Ǥ���'
			$stderr.puts '�¹Ի��Ѥγ��إե����뤫���ޤ��ϸŤ����إǡ����ǤϤʤ�����ǧ���Ƥ���������'
			exit 1
		end
		if hfdef_flg[1] != 2
			$stderr.puts '���إե�����Υإå��Υ᥸�㡼�С������2�ʳ��Ǥ���'
			$stderr.puts '���Υ�����ץȤϡ��᥸�㡼�С������2�ʳ��ˤ��б����Ƥ��ޤ���'
			exit 1
		end
		case	hfdef_flg[3]
		when	0
			# ̤�����̤�����ʸ�������ɤǤ���
			$stderr.puts '�إå���ʸ�������ɤ����ꤵ��Ƥ��ޤ���'
			$stderr.puts '�إå���ʸ�������ɤ򵭽Ҥ��Ƥ��顢�¹Ԥ��Ƥ���������'
			exit 1
		when	1
			# ���ϳ��إե������EUC-JP������
			case	out_word_code
			when	'1'
				# �Ѵ����ʸ�������ɤ�EUC������
				h_data0 = hierarcy_data
#				h_data0 = Kconv::toeuc(hierarcy_data)
			when	'2'
				# �Ѵ����ʸ�������ɤϥ��ե�JIS������
				h_data0 = Kconv::tosjis(hierarcy_data)
				# ���ԥ����ɤ��Ѵ�
				while	h_data0 =~ /[^\x0d]\x0a/
					h_data0.gsub! /([^\x0d])\x0a/, '\1'+"\x0d\x0a"
				end
			end
		when	2
			# ���ϳ��إե������Shift_JIS������
			case	out_word_code
			when	'1'
				# �Ѵ����ʸ�������ɤ�EUC������
				h_data0 = Kconv::toeuc(hierarcy_data)
				# ���ԥ����ɤ��Ѵ� (windows�Ǥ����פ�����debian¦�Ǥ�ɬ��)
				while	h_data0 =~ /\x0d\x0a/
					h_data0.gsub! /\x0d\x0a/, "\x0a"
				end
			when	'2'
				# �Ѵ����ʸ�������ɤϥ��ե�JIS������
				h_data0 = hierarcy_data
#				h_data0 = Kconv::tosjis(hierarcy_data)
			end
		when	3
			# ���ϳ��إե������UTF-8������
			case	out_word_code
			when	'1'
				# �Ѵ����ʸ�������ɤ�EUC������
				h_data0 = Uconv.u8toeuc(hierarcy_data)
			when	'2'
				# �Ѵ����ʸ�������ɤϥ��ե�JIS������
				h_data0 = Kconv::tosjis(Uconv.u8toeuc(hierarcy_data))
				# ���ԥ����ɤ��Ѵ�
				while	h_data0 =~ /[^\x0d]\x0a/
					h_data0.gsub! /([^\x0d])\x0a/, '\1'+"\x0d\x0a"
				end
			end
		when	4
			# ���ϳ��إե������JIS������
			case	out_word_code
			when	'1'
				# �Ѵ����ʸ�������ɤ�EUC������
				h_data0 = Kconv::toeuc(hierarcy_data)
			when	'2'
				# �Ѵ����ʸ�������ɤϥ��ե�JIS������
				h_data0 = Kconv::tosjis(hierarcy_data)
				# ���ԥ����ɤ��Ѵ�
				while	h_data0 =~ /[^\x0d]\x0a/
					h_data0.gsub! /([^\x0d])\x0a/, '\1'+"\x0d\x0a"
				end
			end
		end


		# �����Ȥȹ�����ζ�������
		h_data1 = record_del_comment_space(h_data0)

		# ��=�פ�����ζ�������
		while  /\=[^\S\n]+?/ =~ h_data1
			h_data1.gsub! /\=[^\S\n]+?/ , '='
		end
		while  /\s+?\=/ =~ h_data1
			h_data1.gsub! /\s+?\=/ , '='
		end

		# LineTarget����
		h_data1 = exec_LineTargetDefinition(h_data1)
		if h_data1 == nil
			# �ؿ���ǥ��顼��ȯ������
			raise '��exec_LineTargetDefinition�״ؿ��ǡ����顼��ȯ�����ޤ�����(exchange_hierarcy_to_harray)'
			# �㳰��ȯ���Ǥ��ʤ������θ����
			return	nil
		end


		# ����ñ�̤�����
		h_data2 = h_data1.split("\n")

		w_idx1 = 0

		h_data2.each do |h_data_ex1|
			w_idx1 += 1
			if h_data_ex1 =~ /^\s*?(\S+?)\s*?\=\s*?(\S+?)\s*?$/
				# $1 = XML�ѥ�
				# $2 = �Х��ȿ�
				xml_path1 = $1
				seq_byte1 = $2
#				seq_byte1.gsub /^([0-9]*?)[^0-9]??\S*?$/, '\1'	# �񼰤��Ѥ�äƤ�����פʤ褦�����򤦤�
				seq_byte1.gsub! /^\s*?([0-9]+?)\s*?,[\S\s]*?$/, '\1'	# �񼰤��Ѥ�äƤ�����פʤ褦�����򤦤�
				ret_data.push [xml_path1, Integer(seq_byte1)]
			else
				raise '���ؾ������ˡ������ʵ��Ҥ�ȯ�����ޤ�����(�ۤ�' + String(w_idx1) + '�����ն�)' + "\n" + '[' + h_data_ex1 + ']'
				# �㳰��ȯ���Ǥ��ʤ������θ����
				return	nil
			end
		end


		return ret_data
	end


	# ------------------------------------------------------------
	# XML�ǡ����Υ������󥷥�����ؤ��Ѵ�(�ե�å�����ǡ�������)
	#    ����
	# hierarcy_data - ���ؾ���ǡ��� [IN / String��]
	# output_mode   - ���ϥ⡼�� [IN / Integer��]
	#                    0 - �������󥷥��ե��������
	#                    1 - CSV�ե��������
	# out_word_code - ����ʸ�������� [IN / String��]
	#                    '1' - EUC������
	#                    '2' - ���ե�JIS������
	#    �����
	# �������󥷥��ǡ��� [OUT / ����(String)��] ([]�ξ�硢���顼)
	#    ����
	# ���δؿ��μ¹����ˡ�xmldata_setup�ץ��С��ؿ���¹Ԥ��Ƥ���������
	# �¹Ը�ΰ��������Ƥ��ݾڤ���ޤ���
	# ��®���Τ��ᡢ�������󥷥��ǡ��������󷿤ˤ��Ƥ��ޤ���
	# �ե�����񤭹��ߤκݤˡ����֤˽񤭹���Ǥ�館���̾��ʸ�����񤭹���Τ�Ʊ���ˤʤ�ޤ�
	def convert_xmldata_to_sequenthdata(hierarcy_data, output_mode, out_word_code)
		ret_data = []
		h_array1 = []
		w_idx1 = 0
		xml_data1 = ''
		xml_size1 = 0


		# ��xmldata_setup�פμ¹ԥ����å�
		if @xmllist == nil
			raise '��xmllist�ץǡ������С���nil�Ǥ�����xmldata_setup�פ��¹Ԥ���ʤ��ä��������顼��ȯ�����Ƥ���Ȼפ��ޤ���'
			# �㳰��ȯ���Ǥ��ʤ������θ����
			return	[]
		end


		# ���ؾ���ǡ������ؾ�������ǡ������Ѵ�����
		h_array1 = exchange_hierarcy_to_harray(hierarcy_data, out_word_code)
		if h_array1 == []
			# ���ؾ���¸�ߤ��ʤ�
			return []
		end

		w_idx1 = 0	# ��������
		h_array1.each do |h_data1|
			# h_data1[0] - XML�ѥ� [OUT / String��]
			# h_data1[1] - ʸ���� [OUT / Integer��]
			if h_data1[0] != $nodata_word
				# LineTarget����Ǿä���Ƥ��ʤ��ǡ����Ǥ���
				xml_data1 = @xmllist.get_data_from_xmlpatharray(@xmllist.convert_xml_path_to_patharray(h_data1[0]))
			else
				# LineTarget����Ǿä��줿�ǡ����Ǥ���
				xml_data1 = nil
			end

			if xml_data1 != nil
				# XML�ǡ��������Ĥ��ä�
				xml_size1 = xml_data1.size	# ʸ�����μ���
				if xml_size1 < h_data1[1]
					# ����XML�ѥ��Υǡ����ϡ��������󥷥��ե�����˽񤭹���ʸ������꾮����
					ret_data.push xml_data1
					if output_mode != 1		# CSV���Ϥξ��ϡ�����ϥ��åȤ��ʤ�
						ret_data.push(' ' * (h_data1[1] - xml_size1))	# ­��ʤ�ʸ����ʬ����򥻥å�
					end
				elsif xml_size1 > h_data1[1]
					# ����XML�ѥ��Υǡ����ϡ��������󥷥��ե�����˽񤭹���ʸ��������礭��
					$stderr.puts '����ʸ����Ĺ������Τǡ��ͤ��ڤ�ΤƤޤ�' + "\n" + '�ڤ�Τ���[' + xml_data1 + ']'
					ret_data.push xml_data1[0, h_data1[1]]	# �񤭹����ʸ�����Τ߽񤭹���
					$stderr.puts '�ڤ�ΤƸ�[' + xml_data1[0, h_data1[1]] + ']'
				else
					# ����XML�ѥ��Υǡ����ϡ��������󥷥��ե�����˽񤭹���ʸ������Ʊ��
					ret_data.push xml_data1	# ���Τޤ�ʸ����򥻥å�
				end
				w_idx1 += h_data1[1]
			else
				# XML�ǡ��������Ĥ���ʤ��ä�
				if output_mode != 1		# CSV���Ϥξ��ϡ�����ϥ��åȤ��ʤ�
					ret_data.push(' ' * h_data1[1])	# ����ʸ����������򥻥å�
					w_idx1 += h_data1[1]
				end
			end
			if output_mode == 1
				# CSV�ե�������ϤǤ���
				ret_data.push ','
			end

		end
		if output_mode == 1
			# CSV�ե�������ϤǤ���
			if ret_data[-1] == ','
				# �Ǹ�Ρ�,�פ�������
				ret_data.pop
			end
		end


		return ret_data
	end

	# �쥳���ɤΡ�set_sequenth_xmldata�״ؿ�������




	# ------------------------------------------------------------
	# ���ؾ���ǡ������Ѵ�(���إꥹ�ȥǡ�������)
	#    ����
	# hierarcy_data - ���ؾ���ǡ��� [IN / String��]
	# out_word_code - ����ʸ�������� [IN / String��]
	#                    '1' - EUC������
	#                    '2' - ���ե�JIS������
	#    �����
	# nil��[]�ξ�� - ���ؾ�����Ѵ��˼��Ԥ���
	# nil��[]�Ǥʤ����
	#  [0] - ���ؾ���ꥹ�ȥǡ��� [OUT / ���֥������ȷ�]
	#  [1] - ���Ϸ�� [OUT / Numeric��]
	#  [2] - ���ΤΥ������󥷥��ǡ����ΥХ��ȿ� [OUT / Numeric��]
	#    ����
	# ���Υ��С��ؿ��Ǥϡ��������Ϥ������γ��ؾ���ǡ������Ф���
	# LineTarget���������Ԥ������إꥹ�ȥǡ������֤��ޤ�
	# �¹Ը�ΰ����γ��ؾ���ǡ������ͤ��ݾڤ���ޤ���
	def exchange_hierarcy_to_hierarcylist(hierarcy_data, out_word_code)
		ret_data = nil
		h_data0 = ''
		h_data1 = ''
		h_data2 = []
		xml_path1 = ''
		seq_byte1 = ''
		seq_byte2 = 0
		seq_point1 = 0
		w_idx1 = 0	# CSV���Ϥ˻Ȥ��ֹ�ȡ����顼ɽ������Ω�Ƥ뤿����ΰ�



		# �С�����󡦸�������å����Ѵ�
		first_line = ''	# �����ܤ����Ƥ򥻥åȤ����ΰ�
		hfdef_flg = []	# ��Ƭ������Ϥ������Ƥ򥻥åȤ����ΰ�
		hierarcy_data =~ /^([^\n]+?)\n/
		first_line = $1	# �����ܤ����Ƥ򥻥å�
		# ��Ƭǧ�������μ¹�
		hfdef_flg = realize_topline(first_line)
		if hfdef_flg[0] != 5
			$stderr.puts '�����˻��ꤵ�줿���إե�����Υإå������Ƥ���̵���Ǥ���'
			$stderr.puts '�¹Ի��Ѥγ��إե����뤫���ޤ��ϸŤ����إǡ����ǤϤʤ�����ǧ���Ƥ���������'
			exit 1
		end
		if hfdef_flg[1] != 2
			$stderr.puts '���إե�����Υإå��Υ᥸�㡼�С������2�ʳ��Ǥ���'
			$stderr.puts '���Υ�����ץȤϡ��᥸�㡼�С������2�ʳ��ˤ��б����Ƥ��ޤ���'
			exit 1
		end
		case	hfdef_flg[3]
		when	0
			# ̤�����̤�����ʸ�������ɤǤ���
			$stderr.puts '�إå���ʸ�������ɤ����ꤵ��Ƥ��ޤ���'
			$stderr.puts '�إå���ʸ�������ɤ򵭽Ҥ��Ƥ��顢�¹Ԥ��Ƥ���������'
			exit 1
		when	1
			# ���ϳ��إե������EUC-JP������
			case	out_word_code
			when	'1'
				# �Ѵ����ʸ�������ɤ�EUC������
				h_data0 = hierarcy_data
#				h_data0 = Kconv::toeuc(hierarcy_data)
			when	'2'
				# �Ѵ����ʸ�������ɤϥ��ե�JIS������
				h_data0 = Kconv::tosjis(hierarcy_data)
				# ���ԥ����ɤ��Ѵ�
				while	h_data0 =~ /[^\x0d]\x0a/
					h_data0.gsub! /([^\x0d])\x0a/, '\1'+"\x0d\x0a"
				end
			end
		when	2
			# ���ϳ��إե������Shift_JIS������
			case	out_word_code
			when	'1'
				# �Ѵ����ʸ�������ɤ�EUC������
				h_data0 = Kconv::toeuc(hierarcy_data)
				# ���ԥ����ɤ��Ѵ� (windows�Ǥ����פ�����debian¦�Ǥ�ɬ��)
				while	h_data0 =~ /\x0d\x0a/
					h_data0.gsub! /\x0d\x0a/, "\x0a"
				end
			when	'2'
				# �Ѵ����ʸ�������ɤϥ��ե�JIS������
				h_data0 = hierarcy_data
#				h_data0 = Kconv::tosjis(hierarcy_data)
			end
		when	3
			# ���ϳ��إե������UTF-8������
			case	out_word_code
			when	'1'
				# �Ѵ����ʸ�������ɤ�EUC������
				h_data0 = Uconv.u8toeuc(hierarcy_data)
			when	'2'
				# �Ѵ����ʸ�������ɤϥ��ե�JIS������
				h_data0 = Kconv::tosjis(Uconv.u8toeuc(hierarcy_data))
				# ���ԥ����ɤ��Ѵ�
				while	h_data0 =~ /[^\x0d]\x0a/
					h_data0.gsub! /([^\x0d])\x0a/, '\1'+"\x0d\x0a"
				end
			end
		when	4
			# ���ϳ��إե������JIS������
			case	out_word_code
			when	'1'
				# �Ѵ����ʸ�������ɤ�EUC������
				h_data0 = Kconv::toeuc(hierarcy_data)
			when	'2'
				# �Ѵ����ʸ�������ɤϥ��ե�JIS������
				h_data0 = Kconv::tosjis(hierarcy_data)
				# ���ԥ����ɤ��Ѵ�
				while	h_data0 =~ /[^\x0d]\x0a/
					h_data0.gsub! /([^\x0d])\x0a/, '\1'+"\x0d\x0a"
				end
			end
		end


		# �����Ȥȹ�����ζ�������
		h_data1 = record_del_comment_space(h_data0)

		# ��=�פ�����ζ�������
		while  /\=[^\S\n]+?/ =~ h_data1
			h_data1.gsub! /\=[^\S\n]+?/ , '='
		end
		while  /\s+?\=/ =~ h_data1
			h_data1.gsub! /\s+?\=/ , '='
		end

		# LineTarget����
		h_data1 = exec_LineTargetDefinition(h_data1)
		if h_data1 == nil
			# �ؿ���ǥ��顼��ȯ������
			raise '��exec_LineTargetDefinition�״ؿ��ǡ����顼��ȯ�����ޤ�����(exchange_hierarcy_to_harray)'
			# �㳰��ȯ���Ǥ��ʤ������θ����
			return	nil
		end


		# ���إǡ����ꥹ�ȥ��饹������
		ret_data = HierarcydataList.new


		# ���ؾ�����إǡ����ꥹ�ȷ������Ѵ�����

		# ����ñ�̤�����
		h_data2 = h_data1.split("\n")

		w_idx1 = 0
		seq_point1 = 0	# �������֤ν����

		h_data2.each do |h_data_ex1|

			if h_data_ex1 =~ /^\s*?(\S+?)\s*?\=\s*?(\S+?)\s*?$/
				# $1 = XML�ѥ�
				# $2 = �Х��ȿ�
				xml_path1 = $1
				seq_byte1 = $2
#				seq_byte1.gsub /^([0-9]*?)[^0-9]??\S*?$/, '\1'	# �񼰤��Ѥ�äƤ�����פʤ褦�����򤦤�
				seq_byte1.gsub! /^\s*?([0-9]+?)\s*?,[\S\s]*?$/, '\1'	# �񼰤��Ѥ�äƤ�����פʤ褦�����򤦤�
				seq_byte2 = Integer(seq_byte1)

				# ������ܤξ��ϡ����إǡ����ꥹ�Ȥˤϲ��⥻�åȤ��ʤ�
				if xml_path1 != $nodata_word
					# ���إǡ����ꥹ�Ȥγ���XML�ѥ��˵������֤ȥХ��ȿ��Υ��å�
					ret_data.put_data_from_xmlpatharray(ret_data.convert_xml_path_to_patharray(xml_path1), seq_point1, seq_byte2, w_idx1)
				end
				# ����Х��ȿ��������֤򤺤餹
				seq_point1 += seq_byte2
			else
				raise '���ؾ������ˡ������ʵ��Ҥ�ȯ�����ޤ�����(�ۤ�' + String(w_idx1 + 1) + '�����ն�)' + "\n" + '[' + h_data_ex1 + ']'
				# �㳰��ȯ���Ǥ��ʤ������θ����
				return	nil
			end
			w_idx1 += 1
		end
		return	[ret_data, w_idx1, seq_point1]
	end


	# ------------------------------------------------------------

	# XML�ǡ����Υ������󥷥�����ؤ��Ѵ�(���إꥹ�ȥǡ�������)
	#    ����
	# hierarcy_list - ���إꥹ�ȥǡ��� [IN / ����(���֥�������)��]
	# output_mode   - ���ϥ⡼�� [IN / Integer��]
	#                    0 - �������󥷥��ե��������
	#                    1 - CSV�ե��������
	# out_word_code - ����ʸ�������� [IN / String��]
	#                    '1' - EUC������
	#                    '2' - ���ե�JIS������
	#    �����
	# �������󥷥��ǡ��� [OUT / ����(String)��] ([]�ξ�硢���顼)
	#    [0] - ��Ǽ�ǡ��� (����˶�����դ����ꡢ���դ줿��ʬ���ڤ�ΤƤϤ��Ƥ��ޤ���)
	#    [1] - ��������
	#    [2] - ��Ǽ�Х��ȿ�
	#    ����
	# �¹����ˡ����ؾ���ǡ������إꥹ�ȥǡ������Ѵ����Ƥ���������
	# ���δؿ��μ¹����ˡ�xmldata_setup�ץ��С��ؿ���¹Ԥ��Ƥ���������
	# �¹Ը�ΰ��������Ƥ��ݾڤ���ޤ���
	# ��®���Τ��ᡢ�������󥷥��ǡ��������󷿤ˤ��Ƥ��ޤ���
	# �ե�����񤭹��ߤκݤˡ����֤˽񤭹���Ǥ�館���̾��ʸ�����񤭹���Τ�Ʊ���ˤʤ�ޤ�
	# ������������ξ���nil�����뤳�Ȥ�����ޤ��Τǡ����դ��Ƥ���������
	#
	def convert_xmldata_to_sequenthdata_main(hierarcy_list, output_mode, out_word_code)


		hirlist_stack = []			# ���ؾ���ꥹ�ȥ����å�
									# �ҳ��ؤ����뤿�Ӥ˥����å����Ѥ�
		hirlist_index_stack = []	# ���ؾ���ꥹ�ȤΥ���ǥå����ͤΥ����å�
									# �ҳ��ؤ����뤿�Ӥ˥����å����Ѥ�
		xmllist_stack = []			# XML�ꥹ�ȥ����å�
									# �ҳ��ؤ����뤿�Ӥ˥����å����Ѥ�
		xmllist_index_stack = []	# XML�ꥹ�ȤΥ���ǥå����ͤΥ����å�

		now_hirlist = nil			# ���ߤ��оݳ��ؾ���ꥹ��
		now_hirlist_index = 0		# ���ߤ��оݳ��ؾ���ꥹ�ȤΥ���ǥå�����
		now_xmllist = nil			# ���ߤ��о�XML�ꥹ��
		now_xmllist_index = 0		# ���ߤ��о�XML�ꥹ�ȤΥ���ǥå�����

		seq_data = []				# ��������ǡ���
									# [0] = ��Ǽ�ǡ���, [1] = ��������, [2] = �Х��ȿ�


		# ��xmldata_setup�פμ¹ԥ����å�
		if @xmllist == nil
			raise '��xmllist�ץǡ������С���nil�Ǥ�����xmldata_setup�פ��¹Ԥ���ʤ��ä��������顼��ȯ�����Ƥ���Ȼפ��ޤ���'
			# �㳰��ȯ���Ǥ��ʤ������θ����
			return	[]
		end


		now_xmllist = @xmllist			# XML�ꥹ�ȤΥ��å�
		now_hirlist = hierarcy_list		# ���إꥹ�ȤΥ��å�


		loop do
			# XML����ǥå����ͤ����ξ�硢���ߤ�XML�ꥹ�Ȥ˥ǡ������Ƥ�����С�
			# ���ؾ���ꥹ�Ȥ򻲾Ȥ��ơ������ΰ�˥ǡ�����񤭹���
			if now_xmllist_index == 0
				case	now_xmllist.data
				when	nil, ''
					# ���򤫤�nil�ʤ顢���⤷�ʤ�
				else
					if now_hirlist.byte > 0
						# ���ؾ���ǻ��ꤵ�줿����ǥå���������ˡ�
						# ���ơ��������֡��Х��ȿ��򥻥åȤ���
						seq_data[now_hirlist.seq_num] = [now_xmllist.data, now_hirlist.position, now_hirlist.byte]
					end
				end
			end

# Xml_baselist�˥ǡ���������С�HierarcydataList�λ��ꤵ�줿�ΰ�˥ǡ����򥻥å�
#    [��Ǽ�ǡ���], [��������], [�Х��ȿ�]
# ����ν����ϡ�����ǥå��������ξ��˼¹Ԥ��롣
#
#
# Xml_baselist�λҳ��ؤ˥ǡ���������С�HierarcydataList���鳺�����륿���򸡺�����
# �ҳ��ؤ򸽺ߤΥ��֥������ȤȤ��ƥ롼�פ��롣
#
# Xml_basellist�ҳ��ؤ˥ǡ������ʤ���С������å��˥��֥������Ȥ�����С�
# �����å�����ݥåפ�����Τ򸽺ߤ�XML�γ��ؤˤ��롣(����ǥå����ϼ��ˤ���)
# �����ơ��롼��


			if (now_xmllist.list_value - now_xmllist_index) > 0
				# �ҳ��ؤ�����
				# ���ؾ���ꥹ�Ȥ��顢�������륿���򸡺�����

				if now_hirlist.hirlist_value > 0
					now_hirlist_index = 0
					for now_hirlist_index in 0..(now_hirlist.hirlist_value - 1) do
						if now_xmllist.xmllist[now_xmllist_index].name == now_hirlist.hirlist[now_hirlist_index].name
							# ����̾�����פ���
							# ���ߤ�XML�ꥹ�ȡ����إꥹ�Ȥ�����
							xmllist_stack.push now_xmllist
							xmllist_index_stack.push now_xmllist_index
							hirlist_stack.push now_hirlist
							hirlist_index_stack.push now_hirlist_index
							# ���ߤ��о�XML�ꥹ�ȡ����إꥹ�Ȥ��ѹ�
							now_xmllist = now_xmllist.xmllist[now_xmllist_index]
							now_xmllist_index = 0	# ����ǥå����ν����
							now_hirlist = now_hirlist.hirlist[now_hirlist_index]
							# forʸ����ȴ����
							break
						else
							if now_hirlist_index == (now_hirlist.hirlist_value - 1)
								# �Ǹ�ʤΤǡ����Ĥ���ʤ��ä�
								now_xmllist_index += 1	# ����XML�ꥹ�Ȥ˰ܤ�
							end
						end
					end

				else
					# �ҳ��ؼ��Τ��ʤ��Τǡ��������ʤ�
					now_xmllist_index = now_xmllist.list_value	# �Ǹ���ͤˤ��Ƥ���
				end
			else
				# �ҳ��ؤ��ʤ�
				now_xmllist = xmllist_stack.pop
				if now_xmllist == nil
					# �Ƴ��ؤ�ʤ��Τǡ��롼�פ���ȴ����
					break
				end
				now_xmllist_index = xmllist_index_stack.pop
				now_hirlist = hirlist_stack.pop
#				now_hirlist_index = hirlist_index_stack.pop
				hirlist_index_stack.pop
				now_hirlist_index = 0

				# XML�ꥹ�ȤΥ���ǥå����򼡤ˤ���
				now_xmllist_index += 1

			end

		end		# loop do end

		return	seq_data
	end




	# ------------------------------------------------------------

end


# =============================================================================
# =============================================================================
# �ؿ���




# =============================================================================
# =============================================================================
# �ᥤ�������
if __FILE__ == $0

	# ���������
	hir_data1 = '' ; hir_data2 = ''
	xml_data1 = ''
	seq_data1 = ''
	hir_list1 = []


	# ��������ե�������ɤ߹���
	open($hir_file, 'r') do |fp_h|
		hir_data1 = fp_h.read
	end


	# XML�ե�������ɤ߹���
	open($xml_file, 'r') do |fp_xml|
		xml_data1 = fp_xml.read
	end


	# XML�ꥹ�ȥ��֥������Ȥγ���
	xml_list = Xml_listdata.new


	# XML�ǡ����μ�����
	case	$lang_conf
	when	'1'
#		$stderr.puts '���ߡ�EUCʸ�������ɤǤμ¹ԤǤ�'
	when	'2'
#		$stderr.puts '���ߡ����ե�JISʸ�������ɤǤμ¹ԤǤ�'
	else
		raise 'ͽ�����̥��顼�Ǥ�'
	end
	xml_list.xmldata_setup(xml_data1, $lang_conf)

	case	$hir_match_mode
	when	0
		# ñ����������ǡ���
		# XML�ǡ����򥷡����󥷥��ǡ������Ѵ�
		seq_data1 = xml_list.convert_xmldata_to_sequenthdata(hir_data1, $output_mode, $lang_conf)
	when	1
		# ���إǡ�����ꥹ�ȷ����ˤ������
		# ���ؾ����ꥹ�ȹ�¤���Ѵ�
		
		hir_list1 = xml_list.exchange_hierarcy_to_hierarcylist(hir_data1, $lang_conf)
		#    �����
		# nil��[]�ξ�� - ���ؾ�����Ѵ��˼��Ԥ���
		# nil��[]�Ǥʤ����
		#  [0] - ���ؾ���ꥹ�ȥǡ��� [OUT / ���֥������ȷ�]
		#  [1] - ���Ϸ�� [OUT / Numeric��]
		#  [2] - ���ΤΥ������󥷥��ǡ����ΥХ��ȿ� [OUT / Numeric��]
		# XML�ǡ����򥷡����󥷥��ǡ������Ѵ�

		seq_data2 = []
		seq_data2 = xml_list.convert_xmldata_to_sequenthdata_main(hir_list1[0], $output_mode, $lang_conf)

		# ����Ū�ˡ�ʸ����Ȥ��ơ���Ǽ����
		seq_data1 = []
		seq_data1[0] = ''
		seq_data3 = ''
		seq_data3 = ' ' * hir_list1[2]
		seq_data2.each do |w_ary1|
			# nil�ι��ܤ�ȤФ�������(�Х���)
			if w_ary1 != nil
				seq_data3[w_ary1[1], w_ary1[2]] = w_ary1[0]
			end
		end
		seq_data1[0] = seq_data3
	else
		raise 'ͽ�����̥��顼�Ǥ�'
	end


	# �������󥷥��ե�����ؤν񤭹���
	open($seq_file, 'w') do |fp_sw|
		seq_data1.each do |w_str1|
			fp_sw.print w_str1
		end
	end


end
# =============================================================================
# =============================================================================

