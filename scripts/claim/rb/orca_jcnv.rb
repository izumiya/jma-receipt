#!/usr/bin/env ruby

# XMLfile j-code convert scirpt
#     args 0:in_file
#          1:out_file
#          2:out_code
#          3:mode

require 'kconv'
require 'uconv'


# @jcnv_debug = 1	# DebugMode�Ǥ���

if __FILE__ == $0
	# ���Υ�����ץȤ�ñ�Ȥǵ�ư����

	in_file, out_file, out_code, mode = ARGV

end


# ------------------------------------------------------------


# XML�ե������ʸ���������Ѵ�����
#    ����
# in_word       - �Ѵ��о�ʸ���� [IN / String��]
# after_code    - �Ѵ���ʸ���󥳡��� [IN / String��]
#   1 - EUC
#   2 - Shift_JIS
#   3 - UTF-8
#   4 - JIS������
# linefeed_code - �Ѵ���β��ԥ�����
#   1 - UNIX�ϲ��ԥ�����(LF(0xa)�Τ�)
#   2 - Machintosh�ϲ��ԥ�����(CR(0xd)�Τ�)
#   3 - Windows�ϲ��ԥ�����(CR(0xd)+LF(0xa))
# exec_mode     - �Ѵ��¹ԥ⡼�� [IN / String��]
#   1 - �̾�¹�
#   2 - ���顼�����äƤ⡢�¹�
#    �����
# �������
# [0] = ��λ���ơ�����
#       0 - ���ｪλ
#      -1 - �Ѵ���ʸ�������ɤλ��꤬����(���δؿ������Υ��顼)
#      -2 - �Ѵ���ʸ�������ɤλ��꤬����(�����λ��꤬����)
#      -3 - �Ѵ������ԥ����ɤλ��꤬����(�ؿ������Υ��顼)
#      -4 - �Ѵ�����ԥ����ɤλ��꤬����(�����λ��꤬����)
#      -5 - �Ѵ����о�ʸ����˲��ԥ����ɤ��ʤ��������Ǥ�(�����λ��꤬����)
#      -6 - �Ѵ���ʸ�������ɤλ��꤬����(�����λ��꤬����)
#      -7 - �ǽ�ιԤ�ʸ�������ɻ��꤬�ʤ��������Ǥ�(�����λ��꤬����)
#      -8 - ��Kconv::guess�פǤ�Ƚ���̡�ʸ�������ɤ��б����Ƥ��ʤ�������
#      -9 - ��Kconv::guess�פ��������ͤ�����(ȯ��������硢����Ĵ����ɬ��)
#     -10 - XML��encode��Kconv��Ƚ���̤��ۤʤ�(����ʸ���Υ����ɤ�����)
# [1] = �Ѵ���ʸ����
# [2] = ���顼��å�����(���顼���ʤ���硢nil)
#    ����
# ���������ա����������ա����������ա�����������
# �ե�����ؽ񤭹���ݤϡ�open�ؿ��ΰ����ˤ��ʤ餺��"wb"�פȡ�
#   �Х��ʥ����Ǥν񤭹��ߤ�ԤäƤ���������
def xml_code_conversion(in_word, after_code, linefeed_code, exec_mode)
	ret = [0, '']

# ****************************************
$stderr.puts '[xml_code_conversion] Start'			if @jcnv_debug == 1
# ****************************************

	# 1���ܤ��������ʸ��Ƚ��
	# ���ԥ����ɤμ���
	lf_code = get_linefeed_code(in_word)
	if lf_code == nil
		ret = [-5, nil, '���ԥ����ɤ�¸�ߤ��ʤ����������Ǥ�']
		return ret
	end
	# �ǽ�β��ԤޤǤ�ʸ�������
	first_word = in_word[0, in_word.index(lf_code)]


	# �Ѵ���ʸ�������ɤμ���μ���
	case	after_code
	when	1	# EUC
		after_word = 'EUC-JP'
	when	2	# Shift_JIS
		after_word = 'Shift_JIS'
	when	3	# UTF-8
		after_word = 'UTF-8'
	when	4	# JIS������
		after_word = 'JIS'
	else
		ret = [-6, nil, '�Ѵ���ʸ�������ɤλ��꤬�����Ǥ�(after_code = [' + String(after_code) + '])']
		return ret
	end


	# �Ѵ���ʸ�������ɤμ���
	case	first_word
	when /euc/i
		before_code = 1
		before_word = 'EUC-JP'
	when /shift-jis/i
		before_code = 2
		before_word = 'Shift-JIS'
	when /shift_jis/i
		before_code = 2
		before_word = 'Shift_JIS'
	when /utf-8/i
		before_code = 3
		before_word = 'UTF-8'
	when /jis/i
		before_code = 4
		before_word = 'JIS'
	else
		ret = [-7, nil, '�ǽ�ιԤ�ʸ�������ɻ��꤬�ʤ��������Ǥ�']
		return ret
	end

	# ʸ��������Ƚ��
	kconv_code = Kconv::guess(in_word)
	case	kconv_code
	when	Kconv::UNKNOWN		# ʸ�������ɤϡ�����
		# �����ܤ�ʸ�������ɤ򡢤��Τޤ޺��Ѥ���
	when	Kconv::EUC			# ʸ�������ɤ�EUC������
		if before_code != 1
			# �����ܤ�ʸ����ȼºݤ�ʸ�������ɤ��ۤʤ�
			if exec_mode == 1
				# �۾ｪλ������
				ret = [-10, nil, 'XML��encode��Kconv��Ƚ���̤��ۤʤ�ޤ�(Kconv=EUC, before_code=' + String(before_code) + ')']
				return ret
			else
				# ����³��
				# ���顼��å������ν���
      			$stderr.print " orca_jcnv Warrning : XML��encode��Kconv��Ƚ���̤��ۤʤ�ޤ�\n"
			end
		end
	when	Kconv::SJIS			# ʸ�������ɤϡ����ե�JIS�����ɤ���UTF-8������
		if before_code != 2 and before_code != 3
			# �����ܤ�ʸ����ȼºݤ�ʸ�������ɤ��ۤʤ�
			if exec_mode == 1
				# �۾ｪλ������
				ret = [-10, nil, 'XML��encode��Kconv��Ƚ���̤��ۤʤ�ޤ�(Kconv=SJIS, before_code=' + String(before_code) + ')']
				return ret
			else
				# ����³��
				# ���顼��å������ν���
      			$stderr.print " orca_jcnv Warrning : XML��encode��Kconv��Ƚ���̤��ۤʤ�ޤ�\n"
			end
		end
	when	Kconv::JIS			# ʸ�������ɤ�JIS������
		if before_code != 4
			# �����ܤ�ʸ����ȼºݤ�ʸ�������ɤ��ۤʤ�
			if exec_mode == 1
				# �۾ｪλ������
				ret = [-10, nil, 'XML��encode��Kconv��Ƚ���̤��ۤʤ�ޤ�(Kconv=JIS, before_code=' + String(before_code) + ')']
				return ret
			else
				# ����³��
				# ���顼��å������ν���
      			$stderr.print " orca_jcnv Warrning : XML��encode��Kconv��Ƚ���̤��ۤʤ�ޤ�\n"
			end
		end
	when	Kconv::BINARY		# ʸ�������ɤϡ�������
		ret = [-8, nil, '��Kconv::guess�פǤ�Ƚ���̡�ʸ�������ɤ��б����Ƥ��ʤ��������Ǥ�(ret = [' + String(kconv_code) + '])']
		return ret
	else
		ret = [-9, nil, '��Kconv::guess�פ��������ͤ������Ǥ�(ret = [' + String(kconv_code) + '])']
		return ret
	end


	# �����ܤ�encode���Ѵ�
	word2 = in_word.sub	/#{before_word}/i, after_word


	# ʸ�������ɤ��Ѵ��ᥤ�����
	ret = word_code_conversion(word2, before_code, after_code, linefeed_code)

# ****************************************
$stderr.puts '[xml_code_conversion] End'			if @jcnv_debug == 1
# ****************************************

	return ret
end



# ------------------------------------------------------------


# ���ԥ����ɤ�Ƚ�ꡦ��������
#    ����
# in_word - Ƚ�ꤷ����ʸ����
#    �����
# ����ʸ��������
#  nil = ���ԥ����ɤ�¸�ߤ��ʤ�������
#  \xa = UNIX�ϲ��ԥ�����
#  \xd = Macintosh�ϲ��ԥ�����
#  \xd\xa = Windows�ϲ��ԥ�����
def get_linefeed_code(in_word)
	ret = nil

	if in_word =~ /\xd\xa/
		# Windows�Ϥβ��ԥ����ɤǤ���
		ret = "\xd\xa"
	else
		if in_word =~ /\xa/
			# UNIX�Ϥβ��ԥ����ɤǤ���
			ret = "\xa"
		else
			if in_word =~ /\xd/
				# Machintosh�Ϥβ��ԥ����ɤǤ���
				ret = "\xd"
			else
			end
		end
	end

	return ret
end



# ------------------------------------------------------------


# ʸ���������Ѵ�����
#    ����
# in_word       - �Ѵ��о�ʸ���� [IN / String��]
# before_code   - �Ѵ���ʸ���󥳡��� [IN / String��]
#   1 - EUC
#   2 - Shift_JIS
#   3 - UTF-8
#   4 - JIS������
# after_code    - �Ѵ���ʸ���󥳡��� [IN / String��]
#   1 - EUC
#   2 - Shift_JIS
#   3 - UTF-8
#   4 - JIS������
# linefeed_code - �Ѵ���β��ԥ�����
#   1 - UNIX�ϲ��ԥ�����(LF(0xa)�Τ�)
#   2 - Machintosh�ϲ��ԥ�����(CR(0xd)�Τ�)
#   3 - Windows�ϲ��ԥ�����(CR(0xd)+LF(0xa))
#    �����
# �������
# [0] = ��λ���ơ�����
#       0 - ���ｪλ
#      -1 - �Ѵ���ʸ�������ɤλ��꤬����(�����λ��꤬����)
#      -2 - �Ѵ���ʸ�������ɤλ��꤬����(�����λ��꤬����)
#      -3 - �Ѵ������ԥ����ɤλ��꤬����(���δؿ������Υ��顼)
#      -4 - �Ѵ�����ԥ����ɤλ��꤬����
# [1] = �Ѵ���ʸ����
# [2] = ���顼��å�����(���顼���ʤ���硢nil)
#    ����
# ���������ա����������ա����������ա�����������
# �ե�����ؽ񤭹���ݤϡ�open�ؿ��ΰ����ˤ��ʤ餺��"wb"�פȡ�
#   �Х��ʥ����Ǥν񤭹��ߤ�ԤäƤ���������
def word_code_conversion(in_word, before_code, after_code, linefeed_code)
	ret = [0, '']
	word1 = ''	# ���Ū��ʸ�����ΰ�

# ****************************************
$stderr.puts '[word_code_conversion] Start'			if @jcnv_debug == 1
# ****************************************

	# �Ѵ������ԥ����ɤμ���
	case	get_linefeed_code(in_word)
	when	nil	# ���ԥ����ɤ��ʤ�������
		# ���ԥ����ɤϡ���Ȥ�Ʊ���Ȥ���
		before_lfcode = linefeed_code
	when	"\xd\xa"	# Windows�Ϥβ��ԥ����ɤǤ���
		before_lfcode = 3
	when	"\xa"		# UNIX�Ϥβ��ԥ����ɤǤ���
		before_lfcode = 1
	when	"\xd"		# Machintosh�Ϥβ��ԥ����ɤǤ���
		before_lfcode = 2
	end


	if before_code == after_code and before_lfcode == linefeed_code
		# ���ϤȽ��ϤΥ����ɤ�Ʊ���ʤΤǡ����Ƥ�ž�����ƽ�λ����
		ret = [0, in_word.clone]
		return ret
	end


	case	before_code
	when	1	# �Ѵ����ϡ�EUC������
		case	after_code
		when	1	# �Ѵ���ϡ�EUC������
			# ���⤷�ʤ�
			word1 = in_word
		when	2	# �Ѵ���ϡ�Shift_JIS������
			word1 = Kconv::tosjis(in_word)
		when	3	# �Ѵ���ϡ�UTF-8������
			word1 = Uconv::euctou8(in_word)
		when	4	# �Ѵ���ϡ�JIS������
			word1 = Kconv::tojis(in_word)
		else
			ret = [-2, nil, '�Ѵ���ʸ�������ɤλ��꤬�����Ǥ�(after_code==[' + String(after_code) + '])']
			return ret
		end
	when	2	# �Ѵ����ϡ�Shift_JIS������
		case	after_code
		when	1	# �Ѵ���ϡ�EUC������
			word1 = Kconv::toeuc(in_word)
		when	2	# �Ѵ���ϡ�Shift_JIS������
			# ���⤷�ʤ�
			word1 = in_word
		when	3	# �Ѵ���ϡ�UTF-8������
			word1 = Uconv::sjistou8(in_word)
		when	4	# �Ѵ���ϡ�JIS������
			word1 = Kconv::tojis(in_word)
		else
			ret = [-2, nil, '�Ѵ���ʸ�������ɤλ��꤬�����Ǥ�(after_code==[' + String(after_code) + '])']
			return ret
		end
	when	3	# �Ѵ����ϡ�UTF-8������
		case	after_code
		when	1	# �Ѵ���ϡ�EUC������
			word1 = Uconv::u8toeuc(in_word)
		when	2	# �Ѵ���ϡ�Shift_JIS������
			word1 = Uconv::u8tosjis(in_word)
		when	3	# �Ѵ���ϡ�UTF-8������
			# ���⤷�ʤ�
			word1 = in_word
		when	4	# �Ѵ���ϡ�JIS������
			word1 = Uconv::u8toeuc(Kconv::tojis(in_word))
		else
			ret = [-2, nil, '�Ѵ���ʸ�������ɤλ��꤬�����Ǥ�(after_code==[' + String(after_code) + '])']
			return ret
		end
	when	4	# �Ѵ����ϡ�JIS������
		case	after_code
		when	1	# �Ѵ���ϡ�EUC������
			word1 = Kconv::toeuc(in_word)
		when	2	# �Ѵ���ϡ�Shift_JIS������
			word1 = Kconv::tosjis(in_word)
		when	3	# �Ѵ���ϡ�UTF-8������
			word1 = Uconv::euctou8(Kconv::toeuc(in_word))
		when	4	# �Ѵ���ϡ�JIS������
			# ���⤷�ʤ�
			word1 = in_word
		else
			ret = [-2, nil, '�Ѵ���ʸ�������ɤλ��꤬�����Ǥ�(after_code = [' + String(after_code) + '])']
			return ret
		end
	else
		ret = [-1, nil, '�Ѵ���ʸ�������ɤλ��꤬�����Ǥ�(before_code = [' + String(before_code) + '])']
		return ret
	end


	# ���ԥ����ɤ��Ѵ�
	case	before_lfcode
	when	1	# �������Υ����ɤϡ�UNIX�ϲ��ԥ�����(LF(0xa)�Τ�)
		case	linefeed_code
		when	1	# ���Ը�Υ����ɤϡ�UNIX�ϲ��ԥ�����(LF(0xa)�Τ�)
			# ���⤷�ʤ�
			word2 = word1
		when	2	# ���Ը�Υ����ɤϡ�Machintosh�ϲ��ԥ�����(CR(0xd)�Τ�)
			word2 = word1.gsub /\xa/, "\xd"
		when	3	# ���Ը�Υ����ɤϡ�Windows�ϲ��ԥ�����(CR(0xd)+LF(0xa))
			word2 = word1.gsub /\xa/, "\xd\xa"
		else
			ret = [-4, nil, '�Ѵ�����ԥ����ɤλ��꤬�����Ǥ�(linefeed_code = [' + String(linefeed_code) + '])']
			return ret
		end
	when	2	# �������Υ����ɤϡ�Machintosh�ϲ��ԥ�����(CR(0xd)�Τ�)
		case	linefeed_code
		when	1	# ���Ը�Υ����ɤϡ�UNIX�ϲ��ԥ�����(LF(0xa)�Τ�)
			word2 = word1.gsub /\xd/, "\xa"
		when	2	# ���Ը�Υ����ɤϡ�Machintosh�ϲ��ԥ�����(CR(0xd)�Τ�)
			# ���⤷�ʤ�
			word2 = word1
		when	3	# ���Ը�Υ����ɤϡ�Windows�ϲ��ԥ�����(CR(0xd)+LF(0xa))
			word2 = word1.gsub /\xd/, "\xd\xa"
		else
			ret = [-4, nil, '�Ѵ�����ԥ����ɤλ��꤬�����Ǥ�(linefeed_code = [' + String(linefeed_code) + '])']
			return ret
		end
	when	3	# �������Υ����ɤϡ�Windows�ϲ��ԥ�����(CR(0xd)+LF(0xa))
		case	linefeed_code
		when	1	# ���Ը�Υ����ɤϡ�UNIX�ϲ��ԥ�����(LF(0xa)�Τ�)
			word2 = word1.gsub /\xd\xa/, "\xa"
		when	2	# ���Ը�Υ����ɤϡ�Machintosh�ϲ��ԥ�����(CR(0xd)�Τ�)
			word2 = word1.gsub /\xd\xa/, "\xd"
		when	3	# ���Ը�Υ����ɤϡ�Windows�ϲ��ԥ�����(CR(0xd)+LF(0xa))
			# ���⤷�ʤ�
			word2 = word1
		else
			ret = [-4, nil, '�Ѵ�����ԥ����ɤλ��꤬�����Ǥ�(linefeed_code = [' + String(linefeed_code) + '])']
			return ret
		end
	else
		ret = [-3, nil, '�Ѵ������ԥ����ɤλ��꤬�����Ǥ�(before_lfcode = [' + String(before_lfcode) + '])']
		return ret
	end

# ****************************************
$stderr.puts '[word_code_conversion] End'			if @jcnv_debug == 1
# $stderr.puts 'word2 = [' + word2 + ']'
# ****************************************

	ret = [0, word2]
	return ret
end


# ------------------------------------------------------------

#----- Main --------------------------------------------------

if __FILE__ == $0
	# ���Υ�����ץȤ�ñ�Ȥǵ�ư����

	# �����ܤ˥إ��ɽ���򤷤Ƥۤ������꤬����С��إ��ɽ��
	case	in_file
	when	nil, '', /--help/i, /-\?/i, /\/\?/
		$stderr.puts '���Υ�����ץȤϡ�XML�Υ����ɤ��Ѵ����륹����ץȤǤ�'
		$stderr.puts ''
		$stderr.puts 'orca_jcnv.rb [in_file] [out_code] [mode]'
		$stderr.puts ''
		$stderr.puts '  in_file  - �Ѵ��оݥե�����'
		$stderr.puts ''
		$stderr.puts '  out_code - �Ѵ�������ʸ��������'
		$stderr.puts '    EUC-JP / Shift_JIS / UTF-8 / JIS'
		$stderr.puts ''
		$stderr.puts '  mode     - �Ѵ��¹ԥ⡼��'
		$stderr.puts '     -n  normal mode / -f  force mode'
		$stderr.puts ''
		$stderr.puts ''
		$stderr.puts '������ϡ�ɸ����ϤǤ�'
		exit
	end


	# �ե�������ɤ߹���
	word = ''
	open(in_file, 'rb') do |fp_i|
		word = fp_i.read
	end
	if word == nil or word == ''
		$stderr.puts 'XML�ե�������ɤ߹��ߤ˼��Ԥ��ޤ���'
		exit 2
	end


	# ʸ�������ɤο��ͤؤ��Ѵ�
	case	out_code
	when	/euc/i, /euc-jp/i
		out_codenum = 1
		ret_codenum = 1
	when	/shift-jis/i, /shift_jis/i, /sjis/i, /s-jis/i
		out_codenum = 2
		ret_codenum = 3		# Machintosh�ϲ��ԥ����ɤˤϡ�̤�б�
	when	/utf-8/i, /utf8/i
		out_codenum = 3
		ret_codenum = 1
	when	/jis/i
		out_codenum = 4
		ret_codenum = 3
	end


	# �¹ԥ⡼�ɤ��Ѵ�
	case	mode
	when	nil, '', /-n/i, /-normal/i
		# �̾�¹ԥ⡼��
		exec_mode = 1
	when	/-f/i, /-force/i
		# ���顼�����äƤ⶯���¹�
		exec_mode = 2
	end


	ret = [-1, '', nil]
	# XML�ǡ�����ʸ���������Ѵ�����
	ret = xml_code_conversion(word, out_codenum, ret_codenum, exec_mode)
	if ret[0] != 0
		# ���顼��ȯ������
		$stderr.puts '����С��Ȥ˼��Ԥ��ޤ���'
		$stderr.puts ''
		$stderr.puts ret[2]
		exit 2
	end


#	# ɸ����Ϥˡ���̤����
#	$stdout.print String(ret[1])


	open(out_file, 'wb') do |fp_i|
		fp_i.print ret[1]
	end

end