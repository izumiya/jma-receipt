#!/usr/bin/env ruby


# �ǥ����ɽ��� ver0.11
#
#
# ver0.11 - ʣ����ñ�ȥ����ؤ��б�
#
#
# 2001/10/15 - �ִ���ˡ���ѹ����ơ��礭��ñ�̤��ִ����б�����
# 2001/10/18 - ����ʸ�������ɤ�EUC-JP/Shift_JIS����ˤ���


# =============================================================================
# ������Ƚ��


if __FILE__ == $0


	$h_char = '~'	# ���ؤζ��ڤ�ʸ��
	$a_char = '!'	# °���ζ��ڤ�ʸ��
	$out_word_code = '1'		# ����ʸ�������ɤ�EUC-JP������
#	$out_word_code = '2'		# ����ʸ�������ɤ�Shift_JIS������

	$in_xml = 'test_a.xml'					# �ƥ��ȼ¹Ի����ɤ߹���ե�����̾
	$layout_fname = 'orca_layout_decode.def'	# �쥤����������ե�����̾
	$link_fname = 'decode.def'				# �ѥ�᡼����󥯥ե�����̾
	$seq_fname = 'seq.txt'					# ���ϥ������󥷥��ե�����̾
	$hirarcy_fname = 'hieracy.txt'			# �������ؾ�����ϥե�����̾

	$exec_mode = '0'	# �¹ԥ⡼��
	argv_size = 0	# ARGV���礭��
	argv_size = ARGV.size

	help_flg = 0	# �إ��ɽ�������뤫�Υե饰


	if argv_size != 0
		$exec_mode = String(ARGV.shift)
		case	$exec_mode
		when	'--help', '-?', '/?'
			# �إ��ɽ��
			help_flg = 1
		when	'-execute', '-e', '1'
			# �ǥ����ɼ¹�
			if argv_size < 5
				$stderr.puts '�ǥ����ɤμ¹Ԥ�ɬ�פʰ�������­���Ƥ��ޤ�'
				exit 2
			end
			$exec_mode= '1'
			$in_xml = String(ARGV.shift)
			$layout_fname = String(ARGV.shift)	# �쥤����������ե�����̾
			$link_fname = String(ARGV.shift)	# �ѥ�᡼����󥯥ե�����̾
			$seq_fname = String(ARGV.shift)		# ���ϥ������󥷥��ե�����̾
			$hirarcy_fname = ''					# �������ؾ�����ϥե�����̾
		when	'-show', '-s', '2'
			# �ǥ����ɷ�̤�ɽ��
			if argv_size < 4
				$stderr.puts '�ǥ����ɷ�̤�ɽ����ɬ�פʰ�������­���Ƥ��ޤ�'
				exit 2
			end
			$exec_mode= '2'
			$in_xml = String(ARGV.shift)
			$layout_fname = String(ARGV.shift)	# �쥤����������ե�����̾
			$link_fname = String(ARGV.shift)	# �ѥ�᡼����󥯥ե�����̾
			$hirarcy_fname = ''					# �������ؾ�����ϥե�����̾
		when	'-print', '-p', '-p1', '11'
			# ɽ���⡼�� Type1
			if argv_size < 2
				$stderr.puts '���ؾ����ɽ����ɬ�פʰ�������­���Ƥ��ޤ�'
				exit 2
			end
			$exec_mode= '11'
			$in_xml = String(ARGV.shift)
		when	'-p2', '12'
			# ɽ���⡼�� Type2
			if argv_size < 2
				$stderr.puts '���ؾ����ɽ����ɬ�פʰ�������­���Ƥ��ޤ�'
				exit 2
			end
			$exec_mode= '12'
			$in_xml = String(ARGV.shift)
		when	'-p3', '13'
			# ɽ���⡼�� Type3
			if argv_size < 2
				$stderr.puts '���ؾ����ɽ����ɬ�פʰ�������­���Ƥ��ޤ�'
				exit 2
			end
			$exec_mode= '13'
			$in_xml = String(ARGV.shift)
		else
			# �����ʰ����Ǥ���
			$stderr.puts '���ꤵ�줿�����ϡ������Ǥ�'
			exit 2
		end
	else
		help_flg = 1
	end



	if help_flg == 1
		mes = ''
		mes = mes + 'decode_xml.rb [mode] [...]' + "\n"
		mes = mes + "\n"
		mes = mes + "\n"
		mes = mes + 'mode = -e or -execute' + "\n"
		mes = mes + "  �ǥ����ɽ����μ¹�\n"
		mes = mes + "[-e] [in_file] [layout_file] [link_file] [out_file]\n"
		mes = mes + "    in_file     = XML�ե�����̾\n"
		mes = mes + "    layout_file = �쥤����������ե�����̾\n"
		mes = mes + "    link_file   = �ѥ�᡼����󥯥ե�����̾\n"
		mes = mes + "    out_file    = ���ϥե�����̾\n"
		mes = mes + "\n"
		mes = mes + "\n"
		mes = mes + 'mode = -s or -show' + "\n"
		mes = mes + "  �ǥ����ɽ����μ¹Ը塢ɸ����Ϥ�ɽ��\n"
		mes = mes + "[-s] [in_file] [layout_file] [link_file]\n"
		mes = mes + "    in_file     = XML�ե�����̾\n"
		mes = mes + "    layout_file = �쥤����������ե�����̾\n"
		mes = mes + "    link_file   = �ѥ�᡼����󥯥ե�����̾\n"
		mes = mes + "\n"
		mes = mes + "\n"
		mes = mes + 'mode = -p or -p1 or -print' + "\n"
		mes = mes + "  ���ؾ����ɸ����Ϥؤ�ɽ��\n"
		mes = mes + "[-p1] [in_file]\n"
		mes = mes + "    in_file     = ����ɽ��XML�ե�����̾\n"
		mes = mes + "\n"
		mes = mes + "\n"
		mes = mes + 'mode = -p2' + "\n"
		mes = mes + "  �ǥХå��ѳ��ؾ����ɸ����Ϥؤ�ɽ��\n"
		mes = mes + "[-p2] [in_file]\n"
		mes = mes + "    in_file     = ����ɽ��XML�ե�����̾\n"
		mes = mes + "\n"
		mes = mes + "\n"

		$stdout.print	mes

		exit 0

	end

end




# =============================================================================


require 'xmlparser'
require 'kconv'
require 'uconv'
include Uconv

require 'orca_lib.rb'




# hierarchy - ����

# UTF-8���顢ľ��Shift_JIS���Ѵ�����ȡ��������¹Ԥ���ʤ��Τǡ�
# EUC���Ѵ����Ƥ��顢Shift_JIS���Ѵ����Ƥ���




# =============================================================================


# XML�γ��ؾ���μ���
#    ����
# in_xml        - ����XMLʸ�� [IN / String��]
# section_code  - ���ؤ��ʬ���륳���� [IN / String��]
# atr_sec_code  - ����̾��°�����ʬ���륳���� [IN / String��]
# out_word_code - ����ʸ�������� [IN / String��]
#                    '1' - EUC������
#                    '2' - ���ե�JIS������
#    �����
# [x][0] - ����̾���� [OUT / String������]
# [x][1] - �����;��� [OUT / String������]
#    ����
# XML�Υѡ����˼��Ԥ��ޤ����顢�㳰��ȯ�����ޤ��Τǡ���դ��Ƥ�������
#  �⤷�⡢�㳰Ƚ���Ԥ��������ϡ�
#  ���δؿ��γ�¦�ǡ��㳰��ª�������ɲä��Ƥ�������
# ���ؤ�°�����ʬ���륳���ɤ˻��Ѥ���ʸ���ϡ�Ⱦ�ѵ���ǡ���_�פ��-�פʤɡ�
#  �������̾�˽ФƤ���褦�ʵ���ϡ������Ƥ�������
def get_xml_hierarcyinfo(in_xml, section_code, atr_sec_code, out_word_code)

	h_stack = []	# ���ߤγ��إ����å�
	now_hiera = ''	# ���ߤγ���

	h_stack_o = []	# ���إ����å�
	v_stack_o = []	# �ͥ����å�
	wd_h = ''	# ����̾�Ρ�����ΰ�
	wd_ha = ''	# °���γ���̾�Ρ�����ΰ�
	wd_v = ''	# �͡�����ΰ�
	wd_name = ''	# ̾�ΤΥ���ΰ�
	wd_an1 = ''	# ̾�Τ����󲽤ǻ��Ѥ������ΰ�
	wd_an2 = ''
	wd_an3 = ''
	wd_an4 = ''
	wd_an5 = ''
	idx1 = 0
	idx2 = 0
	flg1 = 0
	flg2 = 0
	idx3 = 0
	encoding = ''	# ʸ����δ���������
	o_encoding = ''	# ����ʸ����δ���������
	xml = ''	# XMLʸ����γ�Ǽ���


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
	idx3 = in_xml.index("\n")
	xml = in_xml[0, idx3]

	# �إå����Ѵ�
	# ����� : UTF��Ƚ�����˹Ԥ�����()
	# encoding�ε��Ҥ��ʤ���С�UTF-8�Ȥ��ư���
	if xml =~ /^.*?(<\?xml\sversion\s*?=\s*?\"[\.\d]+?\")\s*?(\?\>)/i 
#	if xml =~ /^.*?(<\?xml\sversion\s*?=\s*?\".+?\")\s*?(\?\>)/i 
		xml = $1 + " encoding=\"UTF-8\"" + $2 + "\n"
	end
	if xml =~ /^.*?(<\?xml\sversion\s*?=.+\sencoding\s*?=\s*?.UTF-8.*?)/i
		xml.gsub!(/UTF-8/i, "UTF-8")
		encoding = "UTF-8"
	end
	if xml =~ /^<\?xml\sversion\s*?=.+\sencoding\s*?=\s*?.EUC-JP./i
		xml.gsub!(/EUC-JP/i, "UTF-8")
		encoding = "EUC-JP"
	end
#	if xml =~ /^<\?xml\sversion=.+\sencoding=.Shift_JIS./i
	if xml =~ /^<\?xml\s*?version\s*?=.+\sencoding\s*?=\s*?.Shift_JIS./i
		xml.gsub!(/Shift_JIS/i, "UTF-8")
		encoding = "Shift_JIS"
	end
	if xml =~ /^<\?xml\s*?version\s*?=.+\sencoding\s*?=\s*?.JIS./i
		xml.gsub!(/JIS/i, "UTF-8")
		encoding = "JIS"
	end

	# �Ĥ��ʸ�����������ơ��Ѵ�
	xml += in_xml[idx3 .. -1]
	case encoding
	when "EUC-JP"
		xml = euctou8(xml)
	when "Shift_JIS"
		xml = Uconv.euctou8(Kconv::toeuc(xml))
#		xml = sjistou8(xml)
	when "JIS"
		xml = Uconv::euctou8(Kconv::toeuc(xml))
	when "UTF-8"
#		xml = xml
		xml = euctou8(Uconv.u8toeuc(xml))
#		if xml[0] != '<'
#			xml = xml[3 .. -1]		# ���ߤ������äƤ����顢������
#		end
	else
		# ̤�б���ʸ�������ɤʤΤǡ��㳰��ȯ��������
		raise '���Ϥ��줿ʸ�������ɤ��б����Ƥ��ޤ��� [' + String(encoding) + ']'
	end


	parser = XMLParser.new
	def parser.default
	end


	parser.parse(xml) do |type, name, data|
		case type
		when XMLParser::START_ELEM
			case o_encoding
			when "EUC-JP"
				wd_name = Uconv.u8toeuc(name)
			when "Shift_JIS"
				wd_name = Kconv::tosjis(Uconv.u8toeuc(name))
#			when "UTF-8"
#				wd_name = name
			end
			# ���ߤγ�����������
			wd_an1 = now_hiera
			wd_an1 = wd_an1 + section_code		if wd_an1 != ''
			idx1 = 1
			flg1 = 0
			# Ʊ�����󤬸��Ĥ���֡������֤�
			while flg1 == 0
				wd_an2 = wd_name + '(' + String(idx1) + ')'
				wd_an3 = wd_an1 + wd_an2
				if h_stack_o.include?(wd_an3) == false
					flg1 = 1
					break	# whileʸ����ȴ����
				end
				idx1 += 1
			end

			# ñ�ȥ�������������ؤ��б�
			# �ʤ������󤬤ʤ���С�̵�뤹��
			if data.size > 0
				idx2 = idx1
				flg2 = 0
				while flg2 == 0
					wd_an4 = wd_name + '(' + String(idx2) + ')'
					data.each do |key, value|
						case o_encoding
						when "EUC-JP"
							wd_ha = Uconv.u8toeuc(key)
						when "Shift_JIS"
							wd_ha = Kconv::tosjis(Uconv.u8toeuc(key))
#						when "UTF-8"
#							wd_ha = key
						end
						wd_an5 = wd_an1 + wd_an4 + atr_sec_code + wd_ha
						if h_stack_o.include?(wd_an5) == false
							wd_an2 = wd_an4		# �������֤������̾���򥻥å�
							flg2 = 1
							break	# eachʸ����ȴ����
						end
					end
					if flg2 != 0
						break	# whileʸ����ȴ����
					end
					idx2 += 1
				end
				wd_ha = ''
				wd_v = ''
			end

			h_stack.push wd_an2		# ���ߤγ��ؤ򥹥��å����Ѥ�

			now_hiera = h_stack.join section_code	# ���ߤγ��ؤμ���
			# °����̾����ɽ��
			data.each do |key, value|
				case o_encoding
				when "EUC-JP"
#					puts now_hiera + atr_sec_code + Uconv.u8toeuc(key) + ' = [' + Uconv.u8toeuc(value) + ']'
					wd_ha = now_hiera + atr_sec_code + Uconv.u8toeuc(key)
					wd_v = Uconv.u8toeuc(value)
				when "Shift_JIS"
#					puts now_hiera + atr_sec_code + Kconv::tosjis(Uconv.u8toeuc(key)) + ' = [' + Kconv::tosjis(Uconv.u8toeuc(value)) + ']'
					wd_ha = now_hiera + atr_sec_code + Kconv::tosjis(Uconv.u8toeuc(key))
					wd_v = Kconv::tosjis(Uconv.u8toeuc(value))
#				when "UTF-8"
#					wd_ha = now_hiera + atr_sec_code + key
#					wd_v = value
				end
				h_stack_o.push wd_ha
				v_stack_o.push wd_v
			end
		when XMLParser::END_ELEM
			h_stack.pop		# ���γ��ؤ��᤹
			now_hiera = h_stack.join section_code	# ���ߤγ��ؤμ���
		when XMLParser::CDATA
			case o_encoding
			when "EUC-JP"
				c_data = Uconv.u8toeuc data
			when "Shift_JIS"
#				c_data = Uconv.u8toeuc data
#				c_data = Kconv::tosjis c_data
				c_data = Kconv::tosjis(Uconv.u8toeuc(data))
#				c_data = Uconv.u8tosjis data
#			when "UTF-8"
#				c_data = data
			end
			c_data = ''		if c_data == nil
			c_data.strip!
#			if c_data.strip != ''
#				puts now_hiera + ' = [' + c_data + ']'
				h_stack_o.push now_hiera
				v_stack_o.push c_data
#			end
			# ���򥹥��å���ޤ�ʤ��ȡ����󲽤˻پ㤬����Τǡ�����⥻�å�
		when XMLParser::PI
#		when XMLParser::START_DOCTYPE_DECL
#		when XMLParser::END_DOCTYPE_DECL
		when XMLParser::DEFAULT
		else
		end
	end
	del_idx = []	# �������ǥå����γ�Ǽ���
	h_stack_o.each_index do |e_i|
		wd_v = v_stack_o[e_i]
		if wd_v == ''
			del_idx.unshift(e_i)	# �Ǹ�Υǡ����������饻�åȤ��Ƥ���
		end
	end
	del_idx.each do |e_i|
		h_stack_o.delete_at(e_i)
		v_stack_o.delete_at(e_i)
	end

	ret = []
	h_stack_o.each_index do |e_i|
		ret.push [h_stack_o[e_i], v_stack_o[e_i]]
	end

	return ret
end



# =============================================================================


# ���ؾ���ɽ������
#    ����
# in_xml        - ����XMLʸ�� [IN / String��]
# section_code  - ���ؤ��ʬ���륳���� [IN / String��]
# atr_sec_code  - ����̾��°�����ʬ���륳���� [IN / String��]
# out_word_code - ����ʸ�������� [IN / String]
#                    '1' - EUC������
#                    '2' - ���ե�JIS������
# show_mode     - ɽ���⡼�� [IN / Numeric�� / �ǥե������ = 0]
#                   1 - �ǡ�����������ɽ���⡼��
#                   2 - ����̾�Τߤ�ɽ���⡼��
#                  10 - �ǥХå�������ɽ���⡼��
#    �����
# �ʤ�
#    ����
#
def print_hierarcy(in_xml, section_code, atr_sec_code, out_word_code, show_mode = 10)

	hiera = []

	# ���ؾ���μ���
	hiera = get_xml_hierarcyinfo(in_xml, section_code, atr_sec_code, out_word_code)

	# ���ؾ����ɽ��
	case	show_mode
	when	1
		hiera.each do |e|
			# ̾�Ρ��ͥ����å��ΰ��ɽ��
			$stdout.puts e[0] + " = " + e[1] + "\n"
		end
		$stderr.puts 'ɽ����������λ���ޤ���'
	when	2
		hiera.each do |e|
			# ̾�Υ����å��ΰ��ɽ��
			$stdout.puts e[0] + "\n"
		end
		$stderr.puts 'ɽ����������λ���ޤ���'
	when	10
		hiera.each do |e|
			$stdout.puts '-' * 78
			# ̾�Ρ��ͥ����å��ΰ��ɽ��
			$stdout.puts "[" + e[0] + "]\n" + e[1] + "\n"
		end
		$stderr.puts 'ɽ����������λ���ޤ���'
	end


	nil
end



# =============================================================================


# �������ؤΥѥ�᡼���μ���
#    ����
# param_data - �ѥ�᡼�����ǡ��� [IN / String��]
# hierarcy   - ����̾ [IN / String��]
#    �����
# �ѥ�᡼��̾(nil�ξ�硢���Ĥ���ʤ��ä�)
#    ����
#
def get_hierarcy_parameter(param_data, hierarcy)
	# ��(�פ��)�פ�����ɽ��Ƚ�꤬���ޤ������ʤ��Τǡ��Ѵ�
	w_h = hierarcy.clone
	d_gsub = [['\(', '\\('], ['\)', '\\)']]
	d_gsub.each do |e1|
		w_h.gsub! /#{e1[0]}/, e1[1]
	end
#	w_h = hierarcy.gsub /\(/, '\\('
#	w_h = w_h.gsub /\)/, '\\)'
	ret = ''
#	if param_data =~ /#{w_h}/
	if param_data =~ /^\s*?#{w_h}\s*?=\s*?(\S*?)\s*?$/
		# $1�˥ѥ�᡼��̾�����äƤ���
		ret = $1
		ret = ''		if ret == nil
		ret.strip!
		ret = nil		if ret == ''
	else
		# ����̾�����Ĥ���ʤ��ä��Τǡ��ѥ�᡼���Ϥʤ�
		ret = nil
	end
	return ret
end



# =============================================================================


# ��ݷ������γ���̾������ɽ�������ˤ���
#    ����
# mode    - 1���� [IN]
#   0 - ñ�������ɽ���˻Ȥ�������ˤ���
#   1 - ��ݷ����γ���̾������ɽ�������ˤ���
# in_name - ����̾ [IN]
#    �����
# ����ɽ�������γ���̾
def exchange_hierarcy_name(mode, in_name)
	case	mode
	when	0, 1
		# ��(�פ��)�פ�����ɽ��Ƚ�꤬���ޤ������ʤ��Τǡ��Ѵ�
		h_name = in_name.clone
		d_gsub = [['\(', '\\('], ['\)', '\\)']]
		d_gsub.each do |e1|
			h_name.gsub! /#{e1[0]}/, e1[1]
		end

		if mode == 1
			# ��������̾����������ε�����ʬ������ɽ���η������Ѵ�
			h_name.gsub! /\\\([xX]\\\)/, '\\([0-9]+?\\)'
		end
	end

	return h_name
end



# =============================================================================


# ���곬�ؤγ��ؾ��󤫤�θ���
#    ����
# h_name   - ��������̾ [IN / String��]
# hierarcy - ���ؾ��� [IN / 2��������(String)��]
#    �����
# �ʲ�������
# [0] - ����̾
# [1] - ���ؤ���
# nil�ξ�硢���Ĥ���ʤ��ä�
#    ����
# ��h_name�פϡ����Ū��̾�������Ѳ�ǽ�Ǥ���
def search_hierarcy_data(h_name, hierarcy)
	ret = []

	# ����̾����ݳ���̾���顢����ɽ����������̾���Ѵ�
	find_hierarcy = exchange_hierarcy_name(1, h_name)

#	# ��(�פ��)�פ�����ɽ��Ƚ�꤬���ޤ������ʤ��Τǡ��Ѵ�
#	find_hierarcy = h_name.clone
#	d_gsub = [['\(', '\\('], ['\)', '\\)']]
#	d_gsub.each do |e1|
#		find_hierarcy.gsub! /#{e1[0]}/, e1[1]
#	end
#
#	# ��������̾����������ε�����ʬ������ɽ���η������Ѵ�
#	find_hierarcy.gsub! /\\\([x]\\\)/, '\\([0-9]+?\\)'


	# ������Ф��ơ�each����Ѥ��ơ�����ɽ�������򷫤��֤�
	hierarcy.each do |e1|
		if e1[0] =~/^#{find_hierarcy}$/
			ret.push e1
		end
	end

	ret = nil		if ret.size == 0

	return ret
end



# =============================================================================


# ����ɽ���˻Ȥ���ʸ����ؤ��Ѵ��ؿ�
#    ����
# mode - �Ѵ��⡼�� [IN]
#  ���ߡ�mode��ͽ��ǡ�0����Ȥ���
# word - �Ѵ��о�ʸ���� [IN / String��]
#    �����
# �Ѵ���ʸ����
def exchange_regular_expression(mode, word)
	w_d = word.clone

	d_gsub = [['\(', '\\('], ['\)', '\\)'], ['\$', '\\$'], ['\=', '\\=']]
	d_gsub.each do |e1|
		w_d.gsub! /#{e1[0]}/, e1[1]
	end


	return w_d
end



# =============================================================================


# �ѥ�᡼���ǡ�����LineTarget�ޥ������
#    ����
# param_data - �ѥ�᡼���ǡ��� [IN / String��]
# h_data - ���ؾ���ǡ��� [IN / 2��������(String)��]
#    �����
# nil - ���顼��ȯ������
# �Ѵ���Υѥ�᡼���ǡ���
def exec_param_LineTagetMacro(param_data, h_data)


# 	# ****************************************
# 	$stderr.puts	'*' * 78
# 	$stderr.puts	'�ǥХå�����(exec_param_LineTagetMacro)'
# 	dbg_ary = search_hierarcy_data('Mml(1)~MmlHeader(1)~toc(1)~tocItem(x)', h_data)
# 	if dbg_ary != nil
# 		dbg_ary.each do |dbg_e|
# 			$stderr.puts '[' + dbg_e[0] + ']=[' + dbg_e[1] + ']'
# 		end
# 	else
# 		$stderr.puts	'nil���֤äƤ��ޤ���'
# 	end
# 	$stderr.puts	'�ǥХå���λ(exec_param_LineTagetMacro)'
# 	$stderr.puts	'*' * 78
# 	# ****************************************
# 
# 
# 	return param_data



# �嵭�ϡ��ƥ���Ū�˼¹Ԥ��Ƥ�����
# ���������顢�嵭�Υ����ɤ������Ƥ���������

	# LineTarget�ޥ���μ¹�
	p_word = param_data.clone

	while p_word =~ /^(\s*?\$LineTargetStart\s*?,\s*?(\S+?)\s*?,(.+?)\n)/i or p_word =~ /^(\s*?\$LTStart\s*?,\s*?(\S+?)\s*?,(.+?)\n)/i

		# �ޥ���̿�᤬�ѥ�᡼���ǡ�����¸�ߤ���
		# $1 = �ޥ���̿�᤬�и�������
		# $2 = �ޥ���Υ���ܥ�̾
		# $3 = ����ܥ�̾�����Υѥ�᡼������
		macro_startline = $1
		macro_symbol = $2
		macro_param = $3


		# �ޥ���̿��ν�λ���֤μ���
		if p_word =~ /^(\s*?\$LineTargetEnd\s*?,\s*?#{macro_symbol}\s*?\n)/i or p_word =~ /^(\s*?\$LTEnd\s*?,\s*?#{macro_symbol}\s*?\n)/i
			# $1 = ��λ�ޥ���̿�᤬�и�������
			macro_endline = $1
		else
			$stderr.puts '��λ�ޥ���¸�ߤ��ޤ��� (����ܥ�̾ = ' + String(macro_symbol) + ')'
			exit 2
		end

		# �����������Ƥ�����ɽ���ǻȤ���褦���Ѵ�
		macro_startline = exchange_regular_expression(0, macro_startline)
		macro_endline = exchange_regular_expression(0, macro_endline)

		r1_word = ''
		r2_word = ''
		if p_word =~ /(#{macro_startline}([\s\S]*?)#{macro_endline})/
			# $1 - �ִ����о�
			# $2 - �ִ����о�
			r1_word = $1.clone			# �ִ������Ƥγ�Ǽ�ΰ�
			r2_word = $2.clone		# �ִ������Ƥγ�Ǽ�ΰ�
		end

		# �ѥ�᡼������μ���
		arg1 = []
		arg1 = macro_param.split ','
		arg1.each_index do |ei1|
			# ������ζ���������
			arg1[ei1].strip!
		end

		base_hierarcy = arg1[0]			# ��೬�ؤμ���
		mean_decision = arg1[1]			# Ƚ����ˡ
		decision_hierarcy = arg1[2]		# Ƚ�곬��
		decision_value = arg1[3]		# Ƚ������
		arg_num = Integer(arg1[4])		# �о������ֹ�


		# +++++
		find_flg1 = 0	# �����ե饰
		# +++++


		# Ƚ�곬�ؤ�̾�Τ����Ƥ����
		arg2 = []
		arg2 = search_hierarcy_data(decision_hierarcy, h_data)
		if arg2 == nil or arg2.size == 0
			# +++++
			find_flg1 = 1	# �����ե饰�Υ��å�
			# +++++
#			$stderr.puts '���إǡ�����¸�ߤ��ޤ��� (Ƚ�곬�� = ' + String(decision_hierarcy) + ')'
#			exit 2
		end
		# �����������Ƥ�Ƚ���������
		# +++++
		if find_flg1 == 0
		# +++++
			h_name_arg1 = []
			h_value_arg1 = []
			arg2.each do |e1|
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
#				$stderr.puts '���ꤵ�줿���ؤ˳��������Ƥ�¸�ߤ��ޤ��� (Ƚ�곬�� = ' + String(decision_hierarcy) + ') = (' + decision_value + ')'
#				exit 2
			end
		# +++++
		end
		# +++++


		# �������å���������
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
#				$stderr.puts '���ꤵ�줿����γ��ؤ˳��������Ƥ�¸�ߤ��ޤ��� (Ƚ�곬�� = ' + String(decision_hierarcy) + ') = (' + decision_value + ') (���� = ' + String(arg_num) + ')'
#				exit 2
			end
		# +++++
		end
		# +++++


		# +++++
		if find_flg1 == 0
		# +++++
			# ���ؤ����Ĥ��ä�
			# ���λ����ǡ�h_name1�˳������볬�ؾ������äƤ���
			h_name2 = exchange_hierarcy_name(1, base_hierarcy)		# ��೬�ؤ��Ѵ�����
			# Ʊ����Τ����Ĥ��ä���硢��೬�ؤ����ɽ���򡢼�ɽ�����Ѵ�����
			if h_name1 =~ /(#{h_name2})/
				# ��೬�ؤ�����ɽ����Ƚ�곬�ؤ����פ���
				h_name3 = $1	# ��೬�ؤμ³���̾�μ���
				# ��೬��̾�ȼ³���̾���ִ�����
				r2_word.gsub! /#{exchange_regular_expression(0, base_hierarcy)}/, h_name3
				# ���Τ��Ф��Ƥ��ִ�����(�����������ִ������Ǥ���)
#				p_word.gsub! /#{exchange_regular_expression(0, r1_word)}/, r2_word
#				p_word.gsub! Regexp.quote(r1_word), r2_word

				idx1 = 0
				idx1 = p_word.index r1_word		# �Ѵ���ʸ����򸡺�����
				idx2 = r1_word.size
				idx3 = p_word.size
				p_word = p_word[0 .. (idx1 - 1)] + r2_word + p_word[(idx1 + idx2) ... idx3]


			else
				# ��೬�ؤ�����ɽ����Ƚ�곬�ؤ����פ��ʤ��ä�
				$stderr.puts 'Ƚ�곬�ؤϡ���೬�ؤ���ˤʤ���Фʤ�ޤ��� (Ƚ�곬�� = ' + String(decision_hierarcy) + '), (��೬�� = ' + base_hierarcy + ')'
				exit 2
			end
		# +++++
		else
			# ���ؤ����Ĥ���ʤ��ä�
			# �����������Ƥ������Ƥ��ޤ�(���ؤ��ʤ��Ȥߤʤ�)
#			p_word.gsub! /#{exchange_regular_expression(0, r1_word)}/, ''
			# �嵭gsub��Ȥ��ȡ�Ĺ�����ư۾ｪλ����Τǡ��ʲ�����ˡ������

			idx1 = 0
			idx1 = p_word.index r1_word		# �Ѵ���ʸ����򸡺�����
			idx2 = r1_word.size
			idx3 = p_word.size
			p_word = p_word[0 .. (idx1 - 1)] + p_word[(idx1 + idx2) ... idx3]

		end
		# +++++
	end


	# ��ݳ��ؾ����³��ؾ�����Ѵ�
	while p_word =~ /^\s*?(.*?\([xX]\).*?)\s*?=.*?\n/
		# $1 = ����̾
		w_hname = $1.strip
		r1_data = []
		r1_data = search_hierarcy_data(w_hname, h_data)
		if r1_data == nil or r1_data.size == 0
			# ���ꤵ�줿���إǡ����ϡ�¸�ߤ��ʤ�
			$stderr.puts '���ꤵ�줿���إǡ�����¸�ߤ��ޤ��� {' + String(w_hname) + ']'
			exit 2
		end
		r2_data = r1_data[0]
		w_hname = exchange_hierarcy_name(0, w_hname)	# ����ɽ���˻Ȥ���������Ѵ�
		p_word.gsub! /#{w_hname}/, r2_data[0]	# �ִ�����
	end


	return p_word
end



# =============================================================================


# �����ѥ�᡼���Υǡ������֤μ���
#    ����
# layout_data - �쥤����������ǡ��� [IN / String��]
# param_name  - �ѥ�᡼���ǡ��� [IN / String��]
#    �����
# [x][0] - �������� (Integer��)
# [x][1] - ��Ǽ�Х��ȿ� (Integer��)
# nil�ξ�硢�����ѥ�᡼����¸�ߤ��ʤ�
#    ����
# �쥤����������ǡ����Υ��������ϡ��Ϥ����Ƥ���¹Ԥ��Ƥ�������
# �ޤ���equal�ޥ���⡢�Ѵ�������äƤ���¹Ԥ��Ƥ�������
def get_parameter_seqpoint(layout_data, param_name)
	return nil		if param_name == nil

	ret = []
	layout_data.scan /^\s*?#{param_name}\s*?.*?$/ do |s|
		w_param1 = s.strip.split /\s*,\s*/	# ��,�פ�ʬ����
		case w_param1[1]
		when	nil		# �ǡ��������äƤ��ʤ�
		when	'nowdate1'		# ���������դΥ��å�(�ѥ�����1)
		when	'nowdate2'		# ���������դΥ��å�(�ѥ�����2)
		when	'const'			# ����ǡ����Υ��å�
#		else			# ��ľ�˹��ܤΥ��å�
		when	'0'		# ��ľ�˹��ܤΥ��å�
#		when	'0', 'attribute'		# ��ľ�˹��ܤΥ��å�
			# ���־�����ɤ߹���
			sp = w_param1[2]	# ���ϰ���(StartPoint)
#			if sp.class.name!='Fixnum'
				# ­�����䤫�������������硢������н�
				sp = eval(sp)
#			end
			wbyte = Integer(w_param1[3])		# ʸ���Х��ȿ�
			sp = Integer(sp)
			if wbyte != 0
				# �Х��ȿ������Х��ȤǤʤ���С�����ͤ��ΰ�˥��åȤ���
				ret.push [sp, wbyte]
			end
		end
	end


	ret = nil		if ret.size == 0

	return ret
end



# =============================================================================


# �����쥤�����ȥǡ����κ���ǡ���Ĺ�μ���
#    ����
# layout_data - ���󷿥쥤�����ȥǡ���
#    �����
# ʸ��������(nil�ξ�硢�����˼���)
#    ����
# ¾�Υ⥸�塼��Ǥ���Ѥ��Ƥ���Τǡ��Ǥ���С����̥饤�֥�����Ͽ������
# �쥤����������ǡ����ϡ���������������Ƥ��顢�¹Ԥ��Ƥ�������
def get_layout_maxrecordlength(layout_data)
	maxpoint = -1	# �Ǹ������ܰ���
	maxsize = 0		# �Ǹ������ܤ�Ĺ��
	ret = nil
#	w_layout = []
	w_layout = layout_data.split '\n'
#	wd1 = ''	# ʸ����ǡ����Ȥ��ƽ����
	wd1 = []	# ����ǡ����Ȥ��ƽ����
#	wbyte = 0
#	sp = 0
	w_layout.each do |e|
		wd1 = e.split /\s*,\s*/	# �����ʬ����
		case	wd1[1]
		when	nil		# �ǡ��������äƤ��ʤ�
		when	'nowdate1'		# ���������դΥ��å�(�ѥ�����1)
		when	'nowdate2'		# ���������դΥ��å�(�ѥ�����2)
		when	'const'			# ����ǡ����Υ��å�
		when	'ifdef'
		when	'endif'
#		else			# ��ľ�˹��ܤΥ��å�
		when	'0'		# ��ľ�˹��ܤΥ��å�
#		when	'0', 'attribute'		# ��ľ�˹��ܤΥ��å�
			if wd1[0]==nil or wd1[2]==nil or wd1[3]==nil
				ret = nil
#				break
				return
			else
				sp = wd1[2]	# ���ϰ���(StartPoint)
#				if sp.class.name!='Fixnum'
					# ­�����䤫�������������硢������н�
					sp = eval(sp)
#				end
				wbyte = Integer(wd1[3])		# ʸ���Х��ȿ�
				sp = Integer(sp)
				if maxpoint < sp
					maxpoint = sp
					maxsize = wbyte
					ret = maxpoint + maxsize
				end
			end
		end
	end
	ret
end



# =============================================================================


# �������ؤΥѥ�᡼���Υ��å�
#    ����
# in_xml        - ����XMLʸ�� [IN / String��]
# section_code  - ���ؤ��ʬ���륳���� [IN / String��]
# atr_sec_code  - ����̾��°�����ʬ���륳���� [IN / String��]
# param_data    - �ѥ�᡼�����ǡ��� [IN / String��]
# layout_data   - �쥤����������ǡ��� [IN / String��]
# h_file        - ���ؾ���ν��ϥե�����̾ [IN / String��]
# out_word_code - ����ʸ�������� [IN / String��]
#                    '1' - EUC������
#                    '2' - ���ե�JIS������
#    �����
# �������󥷥��ǡ���
#    ����
# h_file���������ξ�硢���ؾ���Ͻ��Ϥ��ޤ���
def set_sequenth_xmldata(in_xml, section_code, atr_sec_code, param_data, layout_data, h_file, out_word_code)

	h_data = []		# ���ؾ����ΰ�
	seq_maxsize = 0	# �������󥷥��ե�����κ��祵�����μ���
	seq_data = ''	# �������󥷥��ǡ����ΰ�
	w_data1 = ''	# ʸ����ΰ��Ū���ΰ�


	# �쥤����������ǡ����ν����
	$stderr.puts	'equal�ޥ����Ÿ����Ǥ�'
	# �쥤�����ȥǡ����Τ�����equal�ޥ�������Ÿ��
	macro_execute_equal(layout_data)
	$stderr.puts	'equal�ޥ����Ÿ������λ���ޤ���'


	# �������󥷥��ե�����Υǡ���Ĺ�μ���
	seq_maxsize = get_layout_maxrecordlength(layout_data)
	if seq_maxsize == nil
		$stderr.puts '�쥤����������ǡ����μ����˼��Ԥ��ޤ���'
		exit 2
	end
#	$stderr.puts	'�ǡ���Ĺ = [' + String(seq_maxsize) + ']'

	seq_data = ' ' * seq_maxsize	# �������󥷥��ǡ����ΰ�γ���


	# ���ؾ���μ���
	$stderr.puts	'���ؾ���μ�����Ǥ�'
	h_data = get_xml_hierarcyinfo(in_xml, section_code, atr_sec_code, out_word_code)
	$stderr.puts	'���ؾ����������ޤ���'


	if h_file != '' and h_file != nil
		# ���ؾ���ν���
		open(h_file, 'w') do |fp|
			h_data.each do |e_h|
				fp.puts '<' + e_h[0] + '> = [' + e_h[1] + ']'
			end
		end
	end


	# �ѥ�᡼������Υ����Ȥ����Ԥκ��
	record_del_comment_space(param_data)


	$stderr.puts	'LineTarget�ޥ���ν�����Ǥ�'
	# �ѥ�᡼���ǡ�����LineTarget�ޥ������
	param_data = exec_param_LineTagetMacro(param_data, h_data)
	$stderr.puts	'LineTarget�ޥ���ν�������λ���ޤ���'


	$stderr.puts	'���ؾ����������򸵤ˡ��������󥷥��ǡ����κ�����Ǥ�'
	# ���ؾ���ʬ�������֤�
	h_data.each do |e1|
		# [0] = ����̾, [1] = ��
		# �������ؤΥѥ�᡼��̾�μ���
		p_name = get_hierarcy_parameter(param_data, e1[0])
		if p_name != nil
			# �����ѥ�᡼����¸�ߤ���
			seq_pnt = []
			# �������֤��Ǽ�Х��ȿ��ʤɤμ���
			seq_pnt = get_parameter_seqpoint(layout_data, p_name)
			if seq_pnt != nil
				seq_pnt.each do |e2|
					# �������󥷥���ΰ�ؤΥ��å�
#					seq_data[e2[0], e2[1]] = e1[1]
					w_data1 = e1[1]
					if w_data1.size < e2[1]
						w_data1 = w_data1 + (' ' * (e2[1] - w_data1.size))
					else
						if w_data1.size > e2[1]
							$stderr.puts '����ʸ����Ĺ������Τǡ��ͤ��ڤ�ΤƤޤ�' + "\n" + '�ڤ�Τ���[' + w_data1 + ']'
							w_data1 = w_data1[0, e2[1]]
							$stderr.puts '�ڤ�ΤƸ�[' + w_data1 + ']'
						end
					end
					seq_data[e2[0], e2[1]] = w_data1
					# ���λ����ǡ�����ǥ������󥷥��ǡ����˽��Ϥ���Х��ȿ���
					# ���ʤ��ʤäƤ����ǽ��������Τǡ����������䴰����
					if seq_data.size < seq_maxsize
						seq_data = seq_data + (' ' * (seq_maxsize - seq_data.size))
					end
				end
			end
		end
	end
	$stderr.puts	'�������󥷥��ǡ����κ������Ǥ��ޤ���'


	return seq_data
end



# =============================================================================




# ======================================================================

# ======================================================================


# =============================================================================
# main������


if __FILE__ == $0

	xml_data = ''

# 	xml_data = $stdin.read		# ɸ�����Ϥ��顢xml�ǡ������ɤ߹���

	if $in_xml != '' and $in_xml != nil
		open($in_xml, 'r') do |fp_x|
			xml_data = fp_x.read
		end
	end


	# -------------------------------------------------------------------------
	case	$exec_mode
	when	'1'
		# �ǥ����ɼ¹�
		layout_data = ''	# �쥤�����ȥǡ���
		link_data = ''		# ��󥯥ǡ���
		seq_data = ''		# �������󥷥��ǡ���

		# �ե�����ǡ������ɤ߹���
		open($layout_fname, 'r') do |fp_l|
			layout_data = fp_l.read
		end
		open($link_fname, 'r') do |fp_k|
			link_data = fp_k.read
		end


		# �������ؤΥѥ�᡼���Υ��å�
		seq_data = set_sequenth_xmldata(xml_data, '~', '!', link_data, layout_data, $hirarcy_fname, $out_word_code)

		# ɸ����Ϥ����Ƥ����
		open($seq_fname, 'w') do |fp_o|
			fp_o.write seq_data
		end


	# -------------------------------------------------------------------------
	when	'2'
		# �ǥ����ɷ�̤�ɽ��
		layout_data = ''	# �쥤�����ȥǡ���
		link_data = ''		# ��󥯥ǡ���
		seq_data = ''		# �������󥷥��ǡ���

		# �ե�����ǡ������ɤ߹���
		open($layout_fname, 'r') do |fp_l|
			layout_data = fp_l.read
		end
		open($link_fname, 'r') do |fp_k|
			link_data = fp_k.read
		end


		# �������ؤΥѥ�᡼���Υ��å�
		seq_data = set_sequenth_xmldata(xml_data, '~', '!', link_data, layout_data, $hirarcy_fname, $out_word_code)

		# ɸ����Ϥ����Ƥ����
		$stdout.print seq_data
	# -------------------------------------------------------------------------
	when	'11'
		# ɽ���⡼�� Type1
		print_hierarcy(xml_data, '~', '!', $out_word_code, 1)
# 		print_hierarcy(xml_data, '+', '@', $out_word_code, 1)
	# -------------------------------------------------------------------------
	when	'12'
		# ɽ���⡼�� Type2
		print_hierarcy(xml_data, '~', '!', $out_word_code, 2)
# 		print_hierarcy(xml_data, '+', '@', $out_word_code, 2)
	# -------------------------------------------------------------------------
	when	'13'
		# ɽ���⡼�� Type3
		print_hierarcy(xml_data, '~', '!', $out_word_code, 10)
# 		print_hierarcy(xml_data, '+', '@', $out_word_code, 10)
	# -------------------------------------------------------------------------
	else
		$stderr.puts '��$exe_mode�פ��ͤ������Ǥ�'
		exit 2
	end


	exit 0	# �����ǽ�λ������

end


