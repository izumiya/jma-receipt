#!/usr/bin/env ruby


# ======================================================================
# �������󥷥��ե������XML�ե������Ѵ��������å�����������ץ�

# ����ϡ��ƥ⥸�塼���ƤӽФ������Υ�����ץȤǤ�


#orca_send.rb [mode] [in_seq] [tmpl] [layout] [temp_file] [out_code] [out_lf] [host] [port] [option]
#��mode      - �¹ԥ⡼�� [���ߡ�1�פǸ���]
#��in_seq    - ���ϥ������󥷥��ե�����̾
#��tmpl      - �ƥ�ץ졼�ȥե�����̾
#��layout    - �쥤����������ե�����̾
#��temp_file - ��֥ե�����̾(�����å��̿����ˡ���ȥ饤������˻���)
#��out_code  - �Ѵ�ʸ��������
#��out_lf    - �Ѵ����ԥ�����
#��host      - �����ۥ���̾
#��port      - �����ݡ����ֹ�
#��option    - ���ץ����(���ߡ�̤��)


# ======================================================================



if __FILE__ == $0

	debug = 1	# �ǥХå��⡼�ɤǤ���

	# ��ư����μ�����
	@arg = []
	@arg = ARGV

end



# ======================================================================


require 'orca_make_xml.rb'
require 'orca_jcnv.rb'
require 'orca_send_xml.rb'


# ======================================================================


# ���ޥ�ɰ�����ʸ��������Ƚ��
#    ����
# word - Ƚ���оݥ����� [IN / String��]
#    �����
# ������ʸ�������ɤ���ɤ�����̤Υ�����(nil�ξ�硢ʸ�������ɤ�Ƚ��˼��Ԥ���)
#  1 - EUC-JP
#  2 - Shift_JIS
#  3 - UTF-8
#  4 - JIS
def decision_word_code(word)
	ret = nil

	case	word
	when	/euc/i, /euc-jp/i, /unix/i, /linux/i, '1'
		ret = 1
	when	/shift_jis/i, /shift-jis/i, /sjis/i, /s_jis/i, /s-jis/i, /mac/i, /machintosh/i, /win/i, /windows/i, /dos/i, '2'
		ret = 2
	when	/utf-8/i, /utf8/i, '3'
		ret = 3
	when	/jis/i, '4'
		ret = 4
	end

	return ret
end



# ----------------------------------------------------------------------


# ���ޥ�ɰ����β��ԥ�����Ƚ��
#    ����
# word - Ƚ���оݥ����� [IN / String��]
#    �����
# �����β��ԥ����ɤ���ɤ�����̤Υ�����(nil�ξ�硢���ԥ����ɤ�Ƚ��˼��Ԥ���)
#   1 - UNIX�ϲ��ԥ�����(LF(0xa)�Τ�)
#   2 - Machintosh�ϲ��ԥ�����(CR(0xd)�Τ�)
#   3 - Windows�ϲ��ԥ�����(CR(0xd)+LF(0xa))
def decision_linefeed_code(word)
	ret = nil

	case	word
	when	/unix/i, /linux/i, /0xa/i, '1'
		ret = 1
	when	/mac/i, /machintosh/i, /0xd/i, '2'
		ret = 2
	when	/windows/i, /win/i, /dos/i, /0xd0xa/i, '3'
		ret = 3
	end

	return ret
end



# ======================================================================


if __FILE__ == $0

	if @arg[0] =~ /^@(\S+?)/
		# �쥹�ݥ󥹥ե������ͳ�Υ��ץ�������
		resp_file = $1
		resp_data = ''
		open(resp_file, "r") do |fp|
			resp_data = fp.read
		end
		resp_data.gsub! /^\s*?\#.*?\n/, ''	# �����ȹԤκ��
		resp_data.gsub! /^\s*?\n/, ''	# ���ԤΤߤιԤκ��
		resp_data.gsub! /^.*?\#.*?\n/, ''	# �����Ȥκ��

		# �ѥ�᡼��̾�ˤ�����ϡ��ʲ����ѹ����뤳��
		@arg = resp_data.split("\n")	# ���Ԥ�ʬ����


		# ****************************************
		# �쥹�ݥ󥹥ե�����Υե����ޥåȤˤĤ���
		#
		# �ʲ��η��������ޤ���
		#   ��#�׹ԤΥ����Ƚ�����ä���
		#    ����Ԥ�̵��
		#   ��<�ѥ�᡼��̾>=<����>�פ������ˤ���
		# ****************************************
	end


	# ============================================================
	case	@arg[0]
	when	'1'
		# �⡼��1�Ǥ���
		arg_mode, arg_in_seq, arg_tmpl, arg_layout, arg_temp_file, arg_out_code, arg_out_lf, arg_host, arg_port, arg_option = @arg


$stderr.puts '-' * 78 + "\n" + "����������\n"				if (debug & 1) == 1
$stderr.puts "mode = [" + String(arg_mode) + "]"			if (debug & 1) == 1
$stderr.puts "in_seq = [" + String(arg_in_seq) + "]"		if (debug & 1) == 1
$stderr.puts "tmpl = [" + String(arg_tmpl) + "]"			if (debug & 1) == 1
$stderr.puts "layout = [" + String(arg_layout) + "]"		if (debug & 1) == 1
$stderr.puts "temp_file = [" + String(arg_temp_file) + "]"	if (debug & 1) == 1
$stderr.puts "out_code = [" + String(arg_out_code) + "]"	if (debug & 1) == 1
$stderr.puts "out_lf = [" + String(arg_out_lf) + "]"		if (debug & 1) == 1
$stderr.puts "host = [" + String(arg_host) + "]"			if (debug & 1) == 1
$stderr.puts "port = [" + String(arg_port) + "]"			if (debug & 1) == 1
$stderr.puts "option = [" + String(arg_option) + "]"		if (debug & 1) == 1
$stderr.puts '-' * 78										if (debug & 1) == 1


		# �ǡ����ΰ�ؤγ�Ǽ

		# �������󥷥��ե�������ɤ߹���
		seq_data = ''
		open(arg_in_seq, 'r') do |fp|
			seq_data = fp.read
		end

		# �ƥ�ץ졼��XML�ե�������ɤ߹���
		tmpl_data = ''
		open(arg_tmpl, 'r') do |fp|
			tmpl_data = fp.read
		end

		# �쥤����������ե�������ɤ߹���
		layout_data = ''
		open(arg_layout, 'r') do |fp|
			layout_data = fp.read
		end

		# ����ʸ��������
		out_code = ''
		out_code = decision_word_code(arg_out_code)
		if out_code == nil
			if arg_out_code == nil or arg_out_code == ''
				$stderr.puts '����ʸ�������ɤ����ꤵ��Ƥ��ޤ���'
			else
				$stderr.puts '���ꤵ�줿����ʸ�������ɤ������Ǥ�(' + String(arg_out_code) + ')'
			end
			exit 2
		end


		# ���ϲ��ԥ�����
		out_lf = ''
		out_lf = decision_linefeed_code(arg_out_lf)
		if out_lf == nil
			if arg_out_lf == nil or arg_out_lf == ''
				$stderr.puts '���ϲ��ԥ����ɤ����ꤵ��Ƥ��ޤ���'
			else
				$stderr.puts '���ꤵ�줿���ϲ��ԥ����ɤ������Ǥ�(' + String(arg_out_lf) + ')'
			end
			exit 2
		end


		# XML����С��Ƚ���

		ret_cd1 = nil
#		ret_cd1 = conversion_seq_to_xml(tmpl_data, seq_data, layout_data, 0)	# authentic
		ret_cd1 = conversion_seq_to_xml(tmpl_data, seq_data, layout_data, 1)	# debug
		if ret_cd1 == nil
			$stderr.puts '��conversion_seq_to_xml�פ���nil��������ޤ���'
			exit 2
		end
		if ret_cd1[0] != 0
			$stderr.puts '��conversion_seq_to_xml�פǥ��顼��ȯ�����ޤ���'
			exit 2
		end

# ****************************************
$stderr.puts 'ʸ�������ɤ��Ѵ���Ǥ�'		if (debug & 1) == 1
# ****************************************

		# ʸ�������ɤ��Ѵ�
		ret_cd2 = nil
		ret_cd2 = xml_code_conversion(ret_cd1[1], out_code, out_lf, 1)
#		ret_cd2 = xml_code_conversion(ret_cd1[1], out_code, out_lf, 2)
		if ret_cd2 == nil
			$stderr.puts '��xml_code_conversion�פ���nil��������ޤ���'
			exit 2
		end
		if ret_cd2[0] != 0
			# xml_code_conversion�ǥ��顼
			$stderr.puts String(ret_cd2[2])
			exit 2
		end

		# ��֥ե�����ؤν񤭹���
		open(arg_temp_file, 'wb') do |fp|
			fp.print ret_cd2[1]
		end

# ****************************************
$stderr.puts 'ʸ�������ɤ��Ѵ��������ޤ���'		if (debug & 1) == 1
# ****************************************

# ****************************************
$stderr.puts '�����å��̿���Ǥ�'		if (debug & 1) == 1
# ****************************************

		# �����å��̿�����
		ret_cd3 = 0
		ret_cd3 = xml_send(arg_temp_file, arg_host, arg_port)
		case	ret_cd3
		when	0
			# ���ｪλ
		when	-1
			# ��³���顼
			$stderr.puts '�����å��̿����ˡ���³���顼��ȯ�����ޤ���'
			exit 2
		when	-2
			# �ǡ����������顼
			$stderr.puts '�����å��̿����ˡ��ǡ����������顼��ȯ�����ޤ���'
			exit 2
		else
			$stderr.puts '��xml_send�פ��顢ͽ�������ͤ�������ޤ���'
			exit 2
		end

# ****************************************
$stderr.puts '�����å��̿��������ޤ���'		if (debug & 1) == 1
# ****************************************


# ****************************************
$stderr.puts 'XML�Ѵ��������������������ޤ���'		if (debug & 1) == 1
# ****************************************


	# ============================================================

	when	'2'		# XML�Ѵ�����
		# �⡼��2�Ǥ���
		arg_mode, arg_in_seq, arg_tmpl, arg_layout, arg_out_file, arg_option = @arg


		# �ǡ����ΰ�ؤγ�Ǽ

		# �������󥷥��ե�������ɤ߹���
		seq_data = ''
		open(arg_in_seq, 'r') do |fp|
			seq_data = fp.read
		end

		# �ƥ�ץ졼��XML�ե�������ɤ߹���
		tmpl_data = ''
		open(arg_tmpl, 'r') do |fp|
			tmpl_data = fp.read
		end

		# �쥤����������ե�������ɤ߹���
		layout_data = ''
		open(arg_layout, 'r') do |fp|
			layout_data = fp.read
		end


		# XML����С��Ƚ���

		ret_cd1 = nil
#		ret_cd1 = conversion_seq_to_xml(tmpl_data, seq_data, layout_data, 0)	# authentic
		ret_cd1 = conversion_seq_to_xml(tmpl_data, seq_data, layout_data, 1)	# debug
		if ret_cd1 == nil
			$stderr.puts '��conversion_seq_to_xml�פ���nil��������ޤ���'
			exit 2
		end
		if ret_cd1[0] != 0
			$stderr.puts '��conversion_seq_to_xml�פǥ��顼��ȯ�����ޤ���'
			exit 2
		end


		# ���ϥե�����ؤν񤭹���
		open(arg_out_file, 'wb') do |fp|
			fp.print ret_cd1[1]
		end


# ****************************************
$stderr.puts 'XML�Ѵ��������������ޤ���'		if (debug & 1) == 1
# ****************************************


	# ============================================================

	when	'3'		# ʸ���������Ѵ�����
		# �⡼��3�Ǥ���
		arg_mode, arg_in_file, arg_out_file, arg_out_code, arg_out_lf, arg_option = @arg


		# �ǡ����ΰ�ؤγ�Ǽ

		# ���ϥե�������ɤ߹���
		in_file = ''
		open(arg_in_file, 'r') do |fp|
			in_file = fp.read
		end

		# ����ʸ��������
		out_code = ''
		out_code = decision_word_code(arg_out_code)
		if out_code == nil
			if arg_out_code == nil or arg_out_code == ''
				$stderr.puts '����ʸ�������ɤ����ꤵ��Ƥ��ޤ���'
			else
				$stderr.puts '���ꤵ�줿����ʸ�������ɤ������Ǥ�(' + String(arg_out_code) + ')'
			end
			exit 2
		end


		# ���ϲ��ԥ�����
		out_lf = ''
		out_lf = decision_linefeed_code(arg_out_lf)
		if out_lf == nil
			if arg_out_lf == nil or arg_out_lf == ''
				$stderr.puts '���ϲ��ԥ����ɤ����ꤵ��Ƥ��ޤ���'
			else
				$stderr.puts '���ꤵ�줿���ϲ��ԥ����ɤ������Ǥ�(' + String(arg_out_lf) + ')'
			end
			exit 2
		end


		# ʸ�������ɤ��Ѵ�
		ret_cd2 = nil
		ret_cd2 = xml_code_conversion(in_file, out_code, out_lf, 1)
#		ret_cd2 = xml_code_conversion(in_file, out_code, out_lf, 2)
		if ret_cd2 == nil
			$stderr.puts '��xml_code_conversion�פ���nil��������ޤ���'
			exit 2
		end
		if ret_cd2[0] != 0
			# xml_code_conversion�ǥ��顼
			$stderr.puts String(ret_cd2[2])
			exit 2
		end

		# ���ϥե�����ؤν񤭹���
		open(arg_out_file, 'wb') do |fp|
			fp.print ret_cd2[1]
		end



	# ============================================================

	when	'4'		# �����å��̿�����
		# �⡼��4�Ǥ���
		arg_mode, arg_send_file, arg_host, arg_port, arg_option = @arg


		# �����å��̿�����
		ret_cd3 = 0
		ret_cd3 = xml_send(arg_send_file, arg_host, arg_port)
		case	ret_cd3
		when	0
			# ���ｪλ
		when	-1
			# ��³���顼
			$stderr.puts '�����å��̿����ˡ���³���顼��ȯ�����ޤ���'
			exit 2
		when	-2
			# �ǡ����������顼
			$stderr.puts '�����å��̿����ˡ��ǡ����������顼��ȯ�����ޤ���'
			exit 2
		else
			$stderr.puts '��xml_send�פ��顢ͽ�������ͤ�������ޤ���'
			exit 2
		end



	# ============================================================

	when	'5'		# ʸ���������Ѵ��Τ�
		# �⡼��5�Ǥ���
		arg_mode, arg_in_file, arg_out_file, arg_in_code, arg_out_code, arg_out_lf, arg_option = @arg


		# �ǡ����ΰ�ؤγ�Ǽ

		# ���ϥե�������ɤ߹���
		in_file = ''
		open(arg_in_file, 'r') do |fp|
			in_file = fp.read
		end


		# ����ʸ��������
		in_code = ''
		in_code = decision_word_code(arg_in_code)
		if in_code == nil
			if arg_in_code == nil or arg_in_code == ''
				$stderr.puts '����ʸ�������ɤ����ꤵ��Ƥ��ޤ���'
			else
				$stderr.puts '���ꤵ�줿����ʸ�������ɤ������Ǥ�(' + String(arg_in_code) + ')'
			end
			exit 2
		end


		# ����ʸ��������
		out_code = ''
		out_code = decision_word_code(arg_out_code)
		if out_code == nil
			if arg_out_code == nil or arg_out_code == ''
				$stderr.puts '����ʸ�������ɤ����ꤵ��Ƥ��ޤ���'
			else
				$stderr.puts '���ꤵ�줿����ʸ�������ɤ������Ǥ�(' + String(arg_out_code) + ')'
			end
			exit 2
		end


		# ���ϲ��ԥ�����
		out_lf = ''
		out_lf = decision_linefeed_code(arg_out_lf)
		if out_lf == nil
			if arg_out_lf == nil or arg_out_lf == ''
				$stderr.puts '���ϲ��ԥ����ɤ����ꤵ��Ƥ��ޤ���'
			else
				$stderr.puts '���ꤵ�줿���ϲ��ԥ����ɤ������Ǥ�(' + String(arg_out_lf) + ')'
			end
			exit 2
		end


		# ʸ�������ɤ��Ѵ�
		ret_cd2 = nil
		ret_cd2 = word_code_conversion(in_file, in_code, out_code, out_lf)
		if ret_cd2 == nil
			$stderr.puts '��word_code_conversion�פ���nil��������ޤ���'
			exit 2
		end
		if ret_cd2[0] != 0
			# xml_code_conversion�ǥ��顼
			$stderr.puts String(ret_cd2[2])
			exit 2
		end

		# ���ϥե�����ؤν񤭹���
		open(arg_out_file, 'wb') do |fp|
			fp.print ret_cd2[1]
		end



	# ============================================================

	when	nil, '', '--help', '-?', '/?'
		mes = ''
		mes += "���Υ�����ץȤϡ��������󥷥��ե������XML���Ѵ����������å��̿����륹����ץȤǤ�\n"
		mes += "\n"
		mes += "\n"
		mes += "XML�Ѵ���ʸ���������Ѵ��������å��̿�����[�⡼��1]\n"
		mes += "\n"
		mes += "orca_send.rb [mode] [in_seq] [tmpl] [layout] [temp_file] [out_code] [out_lf] [host] [port] [option]\n"
		mes += "   [mode]      - �¹ԥ⡼�� [��1�פ򥻥å�]\n"
		mes += "   [in_seq]    - ���ϥ������󥷥��ե�����̾\n"
		mes += "   [tmpl]      - �ƥ�ץ졼�ȥե�����̾\n"
		mes += "   [layout]    - �쥤����������ե�����̾\n"
		mes += "   [temp_file] - ��֥ե�����̾(�����å��̿����ˡ���ȥ饤������˻���)\n"
		mes += "   [out_code]  - �Ѵ�ʸ��������\n"
		mes += "   [out_lf]    - �Ѵ����ԥ�����\n"
		mes += "   [host]      - �����ۥ���̾\n"
		mes += "   [port]      - �����ݡ����ֹ�\n"
		mes += "   [option]    - ���ץ����(���ߡ�̤��)\n"
		mes += "\n"
		mes += "\n"
		mes += "\n"
		mes += "XML�Ѵ�����[�⡼��2]\n"
		mes += "\n"
		mes += "orca_send.rb [mode] [in_seq] [tmpl] [layout] [out_file] [option]\n"
		mes += "   [mode]      - �¹ԥ⡼�� [��2�פ򥻥å�]\n"
		mes += "   [in_seq]    - ���ϥ������󥷥��ե�����̾\n"
		mes += "   [tmpl]      - �ƥ�ץ졼�ȥե�����̾\n"
		mes += "   [layout]    - �쥤����������ե�����̾\n"
		mes += "   [out_file]  - ���ϥե�����̾\n"
		mes += "   [option]    - ���ץ����(���ߡ�̤��)\n"
		mes += "\n"
		mes += "\n"
		mes += "\n"
		mes += "XMLʸ���������Ѵ�����[�⡼��3]\n"
		mes += "\n"
		mes += "orca_send.rb [mode] [in_file] [out_file] [out_code] [out_lf] [option]\n"
		mes += "   [mode]      - �¹ԥ⡼�� [��3�פ򥻥å�]\n"
		mes += "   [in_file]   - ���ϥե�����̾\n"
		mes += "   [out_file]  - ���ϥե�����̾\n"
		mes += "   [out_code]  - �Ѵ�ʸ��������\n"
		mes += "   [out_lf]    - �Ѵ����ԥ�����\n"
		mes += "   [option]    - ���ץ����(���ߡ�̤��)\n"
		mes += "\n"
		mes += "\n"
		mes += "\n"
		mes += "�����å��̿�����[�⡼��4]\n"
		mes += "\n"
		mes += "orca_send.rb [mode] [send_file] [host] [port] [option]\n"
		mes += "   [mode]      - �¹ԥ⡼�� [��4�פ򥻥å�]\n"
		mes += "   [send_file] - �����ե�����̾\n"
		mes += "   [host]      - �����ۥ���̾\n"
		mes += "   [port]      - �����ݡ����ֹ�\n"
		mes += "   [option]    - ���ץ����(���ߡ�̤��)\n"
		mes += "\n"
		mes += "\n"
		mes += "\n"
		mes += "ʸ���������Ѵ�����[�⡼��5]\n"
		mes += "\n"
		mes += "orca_send.rb [mode] [in_file] [out_file] [in_code] [out_code] [out_lf] [option]\n"
		mes += "   [mode]      - �¹ԥ⡼�� [��5�פ򥻥å�]\n"
		mes += "   [in_file]   - ���ϥե�����̾\n"
		mes += "   [out_file]  - ���ϥե�����̾\n"
		mes += "   [in_code]   - �Ѵ���ʸ��������\n"
		mes += "   [out_code]  - �Ѵ���ʸ��������\n"
		mes += "   [out_lf]    - �Ѵ����ԥ�����\n"
		mes += "   [option]    - ���ץ����(���ߡ�̤��)\n"
		mes += "\n"
		mes += "\n"
		mes += "\n"


		$stdout.puts	mes
		exit


	# ============================================================

	else
		$stderr.puts	'���������Ƥ������Ǥ� mode = [' + String(@arg[0]) + ']'
		exit 2
	end


end



# ======================================================================



