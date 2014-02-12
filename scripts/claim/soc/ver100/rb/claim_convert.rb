#!/usr/bin/ruby
# coding : euc-jp
Encoding.default_external = "euc-jp" unless RUBY_VERSION == "1.8.7"



# Claim����ե����륳��С���(2003ǯ�ƥС������)
# ������ǡ����θߴ����Τ���˺���

# Repeat��ʸ���б��������


# ��¸������ե���������ؤ��Ѵ����ΰ�������

# claim_convert.rb -D [����def�ե�����] [����def�ե�����]
# claim_convert.rb -X [����xml�ե�����] [����xml�ե�����]
# claim_convert.rb -H [���ϳ��إǡ����ե�����] [����def�ե�����] [���ϳ��إѥ�᡼����󥯥ե�����]


# �إ�ץ�å�����
# claim_convert.rb --help
#
#
# ���ߺ����
# �ʤ�
#
# ��������
# 2003/06/27��def�ե������°��������¤Ӥ�狼��䤹�����롣��COBOL��INC�ե�����ˤ��䤹�������
# 2003/07/11 - ������Υե����뤫�鿷�����Υե�����ؤΰܹԤ��θ�����إå�������ɲä���
# 2003/07/11 - ���ؾ���ε�����ؤ�2�ե�����ʬΥ�������®��(1���֤�20�ä�û��(180�ܤι�®��))
#


# �����
# "��"��'��'����ˡ�,�פ����äƤ�����硢�������������Ǥ��ʤ��Τǡ���դ��뤳��
# $BaseStart��$Repeat�Ǥϡ�$Repeat��ͥ�褷�Ƥ��ޤ��Τǡ���դ��뤳��
# XML�ξ�硢EUC�����ɸ���Ȥ��Ƥ��롣UTF-8��XML�����Ѵ�������ϡ��Ѵ����Ƥ���¹Ԥ��뤳��
# $define����ϡ�ʸ���������ϤǤ��ޤ���Τǡ���դ��Ƥ������������ͤ�����ϤǤ��ޤ���



if __FILE__ == $0


# =============================================================================
# �ǥХå��б�

	# �ǥХå��Ѱ�������
	debug_argv_set = 0	# �ǥХå��ǤϤʤ�
#	debug_argv_set = 1	# def�ե�����δ�¸�ؤ��Ѵ��ǤΥǥХå�
#	debug_argv_set = 2	# xml�ƥ�ץ졼�ȥե�����δ�¸�ؤ��Ѵ��ǤΥǥХå�
#	debug_argv_set = 3	# ���إǡ����ե�����δ�¸�ؤ��Ѵ��ǤΥǥХå�
#	debug_argv_set = 13	# ���إǡ����ե������ver2�����ؤ��Ѵ��ǤΥǥХå�



	DEBUG = 0		# �ǥХå��ǤϤʤ�
#	DEBUG = 1		# �ǥХå���å�������ͭ���ˤ���




# =============================================================================
# ����λ���

# def�ե�����δ�¸�����ؤ��Ѵ��Ǥ���
CONV_MODE_EXISTING_DEF = 1
# XML�ƥ�ץ졼�ȥե�����δ�¸�����ؤ��Ѵ��Ǥ���
CONV_MODE_EXISTING_XML = 2
# ���إǡ����ե�����δ�¸�����ؤ��Ѵ��Ǥ���
CONV_MODE_EXISTING_HIR = 3
# Repeat������Ѵ��Ǥ���
CONV_MODE_REPEAT = 4

# def�ե������ver2�����ؤ��Ѵ��Ǥ���
# CONV_MODE_VER200_DEF = 11
# XML�ƥ�ץ졼�ȥե������ver2�����ؤ��Ѵ��Ǥ���
# CONV_MODE_VER200_XML = 12
# ���إǡ����ե������VER2�����ؤ��Ѵ��Ǥ���
CONV_MODE_VER200_HIR = 13


# =============================================================================
# ������Ƚ��


	argv_size = 0	# ARGV���礭��
	argv_size = ARGV.size

	help_flg = 0	# �إ��ɽ�������뤫�Υե饰


	argv_f = []		# �����ǻ��ꤷ���ե�����̾���Ǽ�����ΰ�
	conv_mode = 0	# �Ѵ��⡼��(1 = def�ե�����, 2 = XML�ƥ�ץ졼�ȥե�����, 3 = ���إǡ����ե�����)


	case	debug_argv_set
	when	1
		# def�ե�����δ�¸�ؤ��Ѵ��ǤΥǥХå�
		argv_f.push 'test001.def'			# �ƥ��ȼ¹Ի����ɤ߹���ե�����̾
		argv_f.push 'test001out.def'		# �ƥ��ȼ¹Ի��ν��ϥե�����̾
		conv_mode = CONV_MODE_EXISTING_DEF
	when	2
		# xml�ƥ�ץ졼�ȥե�����δ�¸�ؤ��Ѵ��ǤΥǥХå�
		argv_f.push 'test002.xml'			# �ƥ��Ȼ����ɤ߹���XML�ƥ�ץ졼�ȥե�����̾
		argv_f.push 'test002out.xml'		# �ƥ��Ȼ��˽��Ϥ���XML�ƥ�ץ졼�ȥե�����̾
		conv_mode = CONV_MODE_EXISTING_XML
	when	3
		# ���إǡ����ե�����δ�¸�ؤ��Ѵ��ǤΥǥХå�
		argv_f.push 'test003.hir'			# �ƥ��Ȼ����ɤ߹��೬�إǡ����ե�����̾
		argv_f.push 'test003out.def'		# �ƥ��Ȼ��˽��Ϥ���def�ե�����̾
		argv_f.push 'test003out.link'		# �ƥ��Ȼ��˽��Ϥ��볬�إѥ�᡼����󥯥ե�����̾
		conv_mode = CONV_MODE_EXISTING_HIR
	when	13
		# ���إǡ����ե������ver2�����ؤ��Ѵ��ǤΥǥХå�
		argv_f.push 'testa013.hir'			# �ƥ��Ȼ����ɤ߹��೬�إǡ����ե�����̾
		argv_f.push 'testa013exe.hir'		# �ƥ��Ȼ��˽��Ϥ���¹Ի����إǡ����ե�����̾
		conv_mode = CONV_MODE_VER200_HIR
	end



	if argv_size != 0
		ARGV.each do |x|
			case	x
			when	'--help'
				# �إ�ץ�å�������ɽ��������
				help_flg = 1
			when	'-D'
				# def�ե�����δ�¸�����ؤ��Ѵ��Ǥ���
				conv_mode = CONV_MODE_EXISTING_DEF
			when	'-X'
				# XML�ƥ�ץ졼�ȥե�����δ�¸�����ؤ��Ѵ��Ǥ���
				conv_mode = CONV_MODE_EXISTING_XML
			when	'-H'
				# ���إǡ����ե�����δ�¸�����ؤ��Ѵ��Ǥ���
				conv_mode = CONV_MODE_EXISTING_HIR
			when	'-h'
				# ���إǡ����ե������ver2�����ؤ��Ѵ��Ǥ���
				conv_mode = CONV_MODE_VER200_HIR
			when	'-r'
				# Repeat������Ѵ��Ǥ���
				conv_mode = CONV_MODE_REPEAT
			else
				# �ե�����λ���
				argv_f.push x	# �ե�����ꥹ�Ȥ�push����
			end
		end
	else
		# ���������ꤵ��Ƥ��ʤ�
		if debug_argv_set == 0
			help_flg = 1
		end
	end



	if help_flg == 1
		# �إ�ץ�å�������ɽ��
		mes = ''
		mes += __FILE__ + ' [opt] [�ե�����̾...]' + "\n"
		mes += '    -D [����def�ե�����] [����def�ե�����]' + "\n"
		mes += '        def�ե�����δ�¸�����ؤ��Ѵ�' + "\n"
		mes += '    -X [����xml�ե�����] [����xml�ե�����]' + "\n"
		mes += '        xml�ƥ�ץ졼�ȥե�����δ�¸�����ؤ��Ѵ�' + "\n"
		mes += '    -H [���ϳ��إǡ����ե�����] [����def�ե�����] [���ϳ��إѥ�᡼����󥯥ե�����]' + "\n"
		mes += '        ���إǡ����ե�����δ�¸�����ؤ��Ѵ�' + "\n"
		mes += '    -h [���ϳ��إǡ����ե�����] [���ϳ��إǡ����ե�����]' + "\n"
		mes += '        ���إǡ����ե������ver2�����ؤ��Ѵ�' + "\n"
		mes += "\n"
		mes += __FILE__ + ' --help' + "\n"
		mes += '    �إ�פ�ɽ��' + "\n"
		mes += "\n"
		mes += "\n"

		$stdout.print	mes
		exit 0
	end



	case	conv_mode
	when	0
		puts '�������Ѵ��Τ����������ꤵ��Ƥ��ޤ���'
		exit 10
	when	CONV_MODE_EXISTING_DEF
		# def�ե�������Ѵ��Ǥ���
		if argv_f.size != 2
			puts '�����ˡ�����def�ե�����Ƚ���def�ե�����Σ��Ĥ���ꤷ�Ƥ�������'
			exit 10
		end
	when	CONV_MODE_EXISTING_XML
		# XML�ƥ�ץ졼�ȥե�������Ѵ��Ǥ���
		if argv_f.size != 2
			puts '�����ˡ�����xml�ƥ�ץ졼�ȥե�����Ƚ���xml�ƥ�ץ졼�ȥե�����Σ��Ĥ���ꤷ�Ƥ�������'
			exit 10
		end
	when	CONV_MODE_EXISTING_HIR
		# ���إǡ����ե�����Ǥ���
		if argv_f.size != 3
			puts '�����ˡ����ϳ��إǡ����ե�����Ƚ���def�ե�����Ƚ��ϳ��إѥ�᡼����󥯥ե�����Σ��Ĥ���ꤷ�Ƥ�������'
			exit 10
		end
	when	CONV_MODE_REPEAT
		# Repeat������Ѵ��Ǥ���
		if argv_f.size != 2
			puts '�����ˡ����ϥե�����Ƚ��ϥե�����Σ��Ĥ���ꤷ�Ƥ�������'
			exit 10
		end
	when	CONV_MODE_VER200_HIR
		# ���إǡ����ե������ver2�����ؤ��Ѵ��Ǥ���
		if argv_f.size != 2
			puts '�����ˡ����ϳ��إǡ����ե�����Ƚ��ϳ��إǡ����ե�����Σ��Ĥ���ꤷ�Ƥ�������'
			exit 10
		end
	else
		puts '�¹Ի����顼�Ǥ�'
		exit 10
	end



end



# =============================================================================


# Repeat�������
#    ����
# in_data - Repeat�����¸�ߤ���Ȼפ���ʸ���� (String��/IN)
#    �����
# RepeatŸ������ʸ���� (String��/OUT)
#
def repeat_extend_proc(in_data)
	# �ͥ��Ȥ���Repeat������б��Ǥ���褦�ˡ�������
	# �����ʳ��ǡ�����Repeat����ν����򤹤뤫����ꤹ�롣�ʤ���ǡ�����whileʸ��forʸ���ѹ������
	# �ͥ��Ȥ��б�����ݤˡ�Repeat�����̾�����ѹ�����Τǡ�
	#   �ѹ�����̾���򥹥��å����ơ����Ȥǥ��եȤǼ��Ф���褦�ˤ��뤳��
	# ���̻Ҥ�ä���ȡ���$Repeat�פ����$REPEAT_1�פˤʤ�褦�ˤ���
	# �ޤ������󲽤������θ�˥ޡ��������б������concat�����ϽŤ���ʹ���Ƥ��뤬��������
	w_data1 = []	# ���󲽥ǡ����ΰ�
	w_data2 = ''	# ���Ū��ʸ�����ΰ�
	w_data3 = []

	repeat_indexname1 = []	# Repeat����μ���̾�Τߤ��Ǽ������ʤ��Ȥ�forʸ�ǻ��ѡ�
	repeat_indexname2 = []	# Repeat����μ���̾�Τߤ��Ǽ���륹���å��ΰ�ʥ���ѡ�
	repeat_indexcount = 0	# repeat����˥��åȤ��뼱�̻ҤΥ��������

	# ���̻Ҥϡ���_�פ��դ���������Ĺ�����򥻥åȤ���

	w_data1 = in_data.split("\n")	# Repeat������б��Ǥ���褦������
	w_data1.each_index do |x|
		w_str1 = ''
		w_str2 = ''
		if w_data1[x] =~ /^\s*?(\$Repeat)\s*?,/i
			repeat_indexcount += 1		# ��������ͤ򤢤餫����ܣ�����
			w_str1 = ''
			w_str1 = '_' + String(repeat_indexcount)
			repeat_indexname2.push w_str1
			repeat_indexname1.push w_str1
			w_data1[x].gsub!(/^(\s*?\$Repeat)(\s*?,)/i, '\1' + w_str1 + '\2')
		end


		if w_data1[x] =~ /^\s*?(\$EndRepeat)\s*?/i
			if repeat_indexname2.size == 0
				# $Repeat���ʤ�$EndRepeat������
				puts '��Ϣ����$Repeat�Τʤ���$EndRepeat���������ޤ�'
			end
			w_str2 = repeat_indexname2.pop	# �б����뼱�̻Ҥ����
			w_data1[x].gsub!(/^(\s*?\$EndRepeat)(\s*?)/i, '\1' + w_str2 + '\2')
		end

	end


	if repeat_indexname2.size > 0
		# $EndRepeat���ʤ�$Repeat������
		puts '$EndRepeat������ʤ�$Repeat�����¸�ߤ��ޤ�'
	end


	w_data2 = w_data1.join("\n")
	w_data1 = []


	# Repeat�������(1) [�ͥ����б���]
	repeat_indexname1.each do |rname|
		repeat_alldata = ''		# Repeat�����ޤ᤿���Τ�ʸ����
		repeat_count = 0		# �����֤����
		repeat_string = ''		# �ִ��о�ʸ����
		repeat_figures = 0		# �������(0�ξ�硢��ͳ���)
		repeat_target = ''		# �����֤��������å��ΰ�
		repeat_figformat = ''	# �����sprintf��ɽ������ݤ�ɽ�����Ǽ�����ΰ�

		if w_data2 =~ /^(\s*?\$Repeat#{rname}\s*?,[\s\S]*?)\n/
			w_data3 = $1.split(',')
			if (w_data3.size != 4)
				puts 'Repeat�������ǡ������λ��꤬3�ĤǤϤʤ���Τ�����ޤ�'
			end
		end


		while w_data2 =~ /^(\s*?\$Repeat#{rname}\s*?,\s*?([0-9]+?)\s*?,\s*?(\S+?)\s*?,\s*?([0-9]+?)[\s\S]*?\n([\s\S]*?)^\s*?\$EndRepeat#{rname}[\s\S]*?)\n/i
			# $1 = $Repeat����γ��ϹԤ��齪λ�ԤޤǤ�����
			# $2 = �����֤����
			# $3 = �ִ��о�ʸ����
			# $4 = �������(0�ξ�硢��ͳ���)
			# $5 = �����֤����ִ��������å��ΰ�
			repeat_alldata = $1
			repeat_count = $2.to_i	# �����֤��������ͤ��Ѵ����ƥ��å�
			repeat_string = $3
			repeat_figures = $4.to_i	# �������ͤ��Ѵ�
			repeat_target = $5

			if repeat_figures == 0
				repeat_figformat = '%d'
			else
				repeat_figformat = '%0' + String(repeat_figures) + 'd'
			end

			put_data = ''	# ���åȥǡ����γ�Ǽ�ΰ�
			for x in 1..repeat_count do
				put_data += repeat_target.gsub(/#{Regexp.quote(repeat_string)}/, sprintf(repeat_figformat, x))
			end

			# Repeat������ִ�����
			w_data2.gsub!(/#{Regexp.quote(repeat_alldata)}/, put_data)
			put_data = ''
		end

	end

	return w_data2
end


# =============================================================================


# def�ǡ����Υ���С��Ƚ���
#    ����
# in_data - ����С�������def�ǡ��� (String��)
#    �����
# ����С��ȸ��def�ǡ��� ((String����)����)
#    ����
# ���Υ���С��Ƚ����ϡ���¸�η������Ѵ������ΤǤ���
# ��˹�®���б��Τ��ᡢ�̤Υ���ץ�ʷ������Ѵ������Τ��ѹ�����
def def_conversion_proc_a(in_data)
	out_data1 = []
	if_stack1 = []		# ifʸ�ѤΥ����å��ΰ�(��IF_�����ֹ�פη���)
	if_count1 = 0		# ifʸ�˼��̻Ҥ������Ƥ뤿��Υ�������ΰ�
	offset_count1 = 0	# �ѥ�᡼������Υ��ե��å��ͤΥ�������ΰ�


	data2 = in_data.split("\n")		# ���ϥǡ���������


	data2.each do |x|
		w_str1 = ''			# ʸ�����ѥ���ΰ�
		w_str2 = ''

		x1 = x
		if x1 =~ /^\s*?\#/
			# �����ȹԤϡ�����ˤ��롣
			x1 = ''
			# ���ѹ����ϡ�#$ type=figure-define version=2.0 encoding=EUC-JP�פ��ФƤ������Τ��Ȥ�ͤ��뤳��
		end
		if x1 == nil or x1 == ''
			x1 == ''
		else
#			out_data1.push x
			w_data1 = []	# ��,�פǶ��ڤä������ΰ�ν����
			w_data1 = x.split(',')
			# ����������
			w_data1.each_index do |i|
				w_data1[i].strip!
			end
			w_data2 = []
			# ----------------------------------------
			case	(w_data1[0].gsub /\s*?\#[\S\s]*?$/, '')
#			case	w_data1[0]
			when	'$define'
				#defineʸ�Ǥ���
				w_data2.push w_data1[1]
				w_data2.push 'equal'
				w_str1 = w_data1[2].gsub /\s*?\#[\S\s]*?$/, ''
				w_data2.push w_str1
#				w_data2.push w_data1[2]
			# --------------------
			when	'$const'
				# constʸ�Ǥ���
				w_data2.push w_data1[1]
				w_data2.push 'const'
				w_str1 = w_data1[2].gsub /\s*?\#[\S\s]*?$/, ''
				w_data2.push w_str1
#				w_data2.push w_data1[2]
			# --------------------
			when	'$nowdate'
				# nowdateʸ�Ǥ���
				w_str1 = w_data1[2].gsub /\s*?\#[\S\s]*?$/, ''
				case	w_str1
#				case	w_data1[2]
				when	'%Y-%m-%d'
					w_data2.push w_data1[1]
					w_data2.push 'nowdate1'
				when	'%Y-%m-%dT%H:%M:%S'
					w_data2.push w_data1[1]
					w_data2.push 'nowdate2'
				else
					# ¾�Υե����ޥåȡʸ���̤�б���
					puts '���ߡ�nowdate�ΰ����˻���Ǥ���Τϡ�%Y-%m-%d�פȡ�%Y-%m-%dT%H:%M:%S�פǤ�'
				end
			# --------------------
			when	'$ifdef'
				# ifdefʸ�Ǥ���
				if_count1 += 1		# ifʸ�����å���û�
				w_str1 = 'IF_' + String(if_count1)
				if_stack1.push w_str1
				w_data2.push w_str1
				w_data2.push 'ifdef'
				# �����ǡ�����2�Υ����å����Ǥ���
				w_data2.push w_data1[1]
				# �����ǡ�����3�ʹ߰��������뤫�Υ����å����Ǥ���
				for lcnt1 in 2..(w_data1.size - 1) do
					if lcnt1 != (w_data1.size - 1)
						# ���ֺǸ�ΰ����ǤϤʤ�
						w_data2.push w_data1[lcnt1]
					else
						# ���ֺǸ�ΰ����Ǥ���
						w_data2.push((w_data1[lcnt1].gsub /\s*?\#[\S\s]*?$/, ''))
					end
				end
			# --------------------
			when	'$endif'
				# endifʸ�Ǥ���
				# �����ǡ�if_stack1�ο��򸫤ơ�ifdefʸ���ʤ��Τ�endifʸ�����뤫�Υ����å����Ǥ���
				w_data2.push if_stack1.pop
				w_data2.push 'endif'
			# --------------------
			else
				# �̾�Υѥ�᡼���Ǥ���
				case	w_data1.size
				when	2
					# �̾�Υѥ�᡼��
					w_data2.push w_data1[0]
					w_data2.push '0'
					w_data2.push String(offset_count1)
					w_str1 = w_data1[1].gsub /\s*?\#[\S\s]*?$/, ''
					w_data2.push w_str1
					offset_count1 += Integer(w_str1)
#					w_data2.push w_data1[1]
#					offset_count1 += Integer(w_data1[1])
				when	3
					# °���Υѥ�᡼��
					w_data2.push w_data1[0]
					w_data2.push '0'
					w_data2.push String(offset_count1)
					w_str1 = w_data1[1].gsub /\s*?\#[\S\s]*?$/, ''
					w_data2.push w_str1
					w_data2.push ''
					w_str2 = w_data1[2].gsub /\s*?\#[\S\s]*?$/, ''
					w_data2.push w_str2
#					w_data2.push w_data1[2]
					offset_count1 += Integer(w_str1)
#					w_data2.push w_data1[1]
#					w_data2.push ''
#					w_data2.push w_data1[2]
#					offset_count1 += Integer(w_data1[1])
				else
					# ̵���ʥѥ�᡼��
					puts '�ѥ�᡼���ͤλ��̵꤬���Ǥ�'
					w_data1.each do |y1|
						print '[' + y1 + ']  '
					end
					puts ''
				end
			end
			# ----------------------------------------
			out_data1.push w_data2.join(',')
			w_data2 = []
		end
	end



	return out_data1
end


# =============================================================================


# ���Хѥ������Хѥ����Ѵ�
#    ����
# soc_path  - ����оݥѥ� (String��/IN)
# base_path - ���Хѥ����Υ١����ѥ�(String��/IN)
#    �����
# ���Хѥ� (String��/OUT)
#    ����
# �����Ȥ�����ΤϤ���Ǥ����#�ס�$�פǻϤޤ��ΤˤĤ��Ƥϡ����δؿ��Ǥν����Ϥ��ޤ���
def exchange_absolute_path(soc_path, base_path)

	if soc_path =~ /^\s*?\#/
		# �����ȤǤ���
		w_path = soc_path
	elsif soc_path =~ /^\s*?[^\$\#]/
		# �̾�ι�
		if soc_path =~ /^(\s*?)\.(\/[\s\S]*?)$/
			# ���Х��ɥ쥹����Ǥ���(./���ꤢ��)
			w_path = $1 + base_path + $2
		elsif soc_path =~ /^(\s*?)([^\/\s][\s\S]*?)$/
			# ���Х��ɥ쥹����Ǥ���(./����ʤ�)
			w_path = $1 + base_path + '/' + $2
		else
			# ���Х��ɥ쥹����Ǥ���
			w_path = soc_path
		end
	else
		# ����Ǥ���
		w_path = soc_path
	end

	return w_path

end


# =============================================================================


# ���إǡ�����base�������
#    ����
# in_data - base��������о�ʸ���� (String��/IN)
#    �����
# base����������ʸ���� (String��/OUT)
#    ����
def basehierarchy_extend_proc(in_data)

# �ͥ��Ȥ��б��Ǥ���褦�ˤ��뤳��
# ���Τ��ᡢBaseStart��BaseEnd�μ��̻Ҥ򤢤餫����ä��뤳�ȡ�
# �ޤ����١������ؤν�����ǥ����å����򤹤�
# gsub�Ǥΰ쵤���Ѵ��Ϥ�������ñ�̤ǳ�������١������ؤ��ִ�������Ԥ��褦�ˤ��롣
#   �ʤ������ʤ��ȡ��ͥ��Ȥؤ��б����Ǥ��ʤ���


	# ���̻��������
	# �ͥ��Ȥ���Base������б��Ǥ���褦�ˡ����̻Ҥ����ꤹ��������
	w_data1 = []	# ���󲽥ǡ����ΰ�
	w_data2 = ''	# ���Ū��ʸ�����ΰ�
	w_data3 = []	# LineTarget̿��ǻ��Ѥ�������ǡ����ΰ�

#	base_indexname1 = []	# Base����μ���̾�Τߤ��Ǽ���륹���å��ΰ��Base���Ÿ�����ѡ�
	base_indexname2 = []	# Base����μ���̾�Τߤ��Ǽ���륹���å��ΰ�ʼ���̾���åȻ��ѡ�
	base_indexcount = 0		# Base����˥��åȤ��뼱�̻ҤΥ��������

	# ���̻Ҥϡ���_�פ��դ���������Ĺ�����򥻥åȤ���

	w_data1 = in_data.split("\n")	# Base������б��Ǥ���褦������
	w_data1.each_index do |x|
		w_str1 = ''
		w_str2 = ''
		if w_data1[x] =~ /^\s*?(\$BaseStart)\s*?,/i
			base_indexcount += 1		# ��������ͤ򤢤餫����ܣ�����
			w_str1 = ''
			w_str1 = '_' + String(base_indexcount)
			base_indexname2.push w_str1
			w_data1[x].gsub!(/^(\s*?\$BaseStart)(\s*?,)/i, '\1' + w_str1 + '\2')
		end

		# �����ǥ��顼�����å����Ǥ����$BaseStart���ʤ�$BaseEnd�������

		if w_data1[x] =~ /^\s*?(\$BaseEnd)\s*?/i
			w_str2 = base_indexname2.pop	# �б����뼱�̻Ҥ����
			w_data1[x].gsub!(/^(\s*?\$BaseEnd)(\s*?)/i, '\1' + w_str2 + '\2')
		end

	end


	# �����ǥ��顼�����å����Ǥ��� ($BaseEnd���ʤ�$BaseStart������)


	base_indexname2 = []
	hierarchy_stack = []
	now_hierarchy = ''		# ���ߤΥǥ��쥯�ȥ�
	w_str1 = ''

#	base_indexname1.push w_str1

	# base����¹Խ���
	w_data1.each_index do |x|
		if w_data1[x] =~ /^\s*?\$BaseStart(\_[0-9]+?)\s*?,\s*?([\s\S]*?)\s*?$/i
			# BaseStart���
#			base_indexname1.push $1
			w_str1 = $2
			hierarchy_stack.push now_hierarchy
			if w_str1 =~ /^\s*?\//
				now_hierarchy = w_str1.strip
			elsif w_str1 =~ /^\s*?\.\/([\s\S]*?)$/
				now_hierarchy += '/' + $1.strip
			elsif w_str1 =~ /^\s*?([^\/][\s\S]*?)$/
				now_hierarchy += '/' + $1.strip
			end
			now_hierarchy.gsub! /\/$/, ''		# �����ˡ�/�פ��դ��Ƥ����顢�Ϥ���
		elsif w_data1[x] =~ /^\s*?\$BaseEnd(\_[0-9]+?)\s*?$/i
			# BaseEnd���
#			base_indexname1.pop
			now_hierarchy = hierarchy_stack.pop
# base_indexname1��Ȥ��Ȼפä������ºݤϻȤ�ɬ�פ��ʤ��Ȼפ���
		elsif w_data1[x] =~ /^\s*?\$LineTargetStart/i
			# $LineTarget̿��
			# $LineTargetStart [����оݳ���] = [����], [�����ꥸ��γ��ϰ���], [�ִ��оݳ���]
			# ����оݳ��ء��ִ��оݳ��ؤ����ؾ��������
			w_data3 = []
			# ��,��ñ�̤����󲽤���
			w_data3 = w_data1[x].split(',')
			# ����оݳ��ؤ����Хѥ��������Хѥ����Ѵ�
			w_data3[1] = exchange_absolute_path(w_data3[1], now_hierarchy)
			# �ִ��оݳ��ؤ����Хѥ��������Хѥ����Ѵ�
			w_data3[3] = exchange_absolute_path(w_data3[3], now_hierarchy)
			# ��,��ñ�̤����󲽤�����Τ򡢸����᤹
			w_data1[x] = w_data3.join(',')

		else
			# �̾�ι�
			# ���Хѥ��ιԤ����Хѥ����Ѵ�����
			w_data1[x] = exchange_absolute_path(w_data1[x], now_hierarchy)
		end
	end


	# base����κ������
	w_data1.each_index do |x|
		if w_data1[x] =~ /^\s*?\$BaseStart_[0-9]+?\s*?/i
			# BaseStartʸ�ιԤǤ���
			w_data1[x] = ''
#			w_data1[x] = "\n"
		elsif w_data1[x] =~ /^\s*?\$BaseEnd_[0-9]+?\s*?/i
			# BaseEndʸ�ιԤǤ���
			w_data1[x] = ''
#			w_data1[x] = "\n"
		end
	end


	w_data2 = w_data1.join("\n")
	w_data1 = []

	return w_data2

end



# =============================================================================


# ���إǡ�����LineTarget̿��ؤμ��̻ҥ��åȽ���
#    ����
# in_data - LineTarget̿��μ��̻ҥ��åȽ����о�ʸ���� (String��/IN)
#    �����
# LineTarget̿��ؤμ��̻ҥ��åȽ������ʸ���� (String��/OUT)
#    ����
# �����������γ��إǡ����ե����륳��С��ȸ�η������Ѵ�����Τǡ�
#   ��¸�ξ��ˤ��֤���������դ��뤳��
def linetarget_setmark_proc(in_data)

	marknum = 0		# ���̻Ҥ��դ����ֹ�ν����
	out_data = ''	# ���̻ҥ��åȸ�γ�Ǽ�ΰ�ν����
	w_data1 = []	# ���ԤǶ��ڤä�ʸ����γ�Ǽ�����ΰ�ν����
	w_data2 = ''	# ���̻Ҥΰ����Ǽ�ΰ�
	marknum_stack = []	# �ͥ��Ȥ��б����뤿��Υ����å��ΰ�


	w_data1 = in_data.split("\n")

	w_data1.each_index do |i|
		w_data2 = ''	# ���ԤΥǡ����ΰ����Ǽ�ΰ�
		if w_data1[i] =~ /^(\s*?\$LineTargetStart\s*?)(,[\s\S]+?)$/i
			# LineTargetStartʸ�Ǥ���
			marknum += 1
			w_data2 = 'LT_' + String(marknum)
			w_data1[i] = $1 + ', ' + w_data2 + $2
			marknum_stack.push w_data2
		elsif w_data1[i] =~ /^(\s*?\$LineTargetEnd\s*?)$/i
			# LineTargetEndʸ�Ǥ���
			w_data1[i] = $1 + ',  ' + marknum_stack.pop
#		else
#			# ����¾�ιԤǤ���
		end
	end


	out_data = w_data1.join("\n")

	return out_data

end



# =============================================================================


# ���إǡ����Υ���С��Ƚ��� (ver2����)
#    ����
# in_data - ����С������γ��إǡ��� (String��)
#    �����
# ����С��ȸ�γ��إǡ��� ((String����)����)
#    ����
# Repeat����μ¹Ԥ򡢤��餫���ᴰλ���Ƥ��뤳��
def hierarchy_conversion_proc_ver200(in_data)


	w_data1 = ''
	w_data2 = []

	w_str1 = []
	w_str2 = []
	w_str3 = ''


	# ���إǡ�����base�������
	w_data1 = basehierarchy_extend_proc(in_data)


	# LineTargetStart��LineTargetEnd�μ��̻������б�
	w_data1 = linetarget_setmark_proc(w_data1)



# ������


































































	return w_data1
end



# =============================================================================


# ���إǡ�����¸���� (ver2����)
#    ����
# in_data - ����С��ȸ�γ��إǡ��� (String��)
# hir_filename - ���إǡ����ե�����̾
#    ����
def hierarchy_save_proc_ver200(in_data, hir_filename)

	def_data = []		# def�ե�����˽��Ϥ���ǡ���
	hir_data = []		# ���إѥ�᡼����󥯥ե�����˽��Ϥ���ǡ���
	# ����®���Τ��ᡢ�嵭������(1���֤ν�����16�ä�û��...)
	param_num = 0		# �ѥ�᡼��̾�ǻ��Ѥ����ֹ�ν����
	offset_count = 0	# �Х��ȿ����饪�ե��åȤ�׻�����
	w_data1 = []
	w_data2 = []
	w_data3 = []
	w_str1 = ''
	w_str2 = ''


	w_data1 = in_data.split("\n")


	# ��Ƭ�Ԥˡ�#$ type=hierarchy version=2.0�פ�����С�
	# �¹Ի���ɽ����#$ type=hierarchy-execute version=2.0�פ��Ѵ�����
	if w_data1[0] =~ /\A\s*?\#\$([\s\S]+?)$/
		# ��Ƭ�Ԥ�ɽ�������Ĥ��ä�
		w_str1 = $1
		w_str1.gsub! /(\S)\s+?(\=)\s+?(\S)/, '\1\2\3'
		if w_str1 =~ /type=hierarchy/i
			w_data1[0] = '#$ type=hierarchy-execute version=2.0 encoding=EUC-JP'
		else
			puts '�ե����뼱�̾���ˡ�type=hierarchy�פ����Ĥ���ޤ���Ǥ�����'
		end
	else
		# ��Ƭ�Ԥ�ɽ�������Ĥ���ʤ��ä�
		puts '�ե����뼱�̾��󤬸��Ĥ���ޤ���Ǥ�����'
		puts '��Ƭ�Ԥˡ�#$ type=hierarchy ...�פ򵭽Ҥ��Ƥ�������'
		exit 10
	end


	# �̾�Υѥ�᡼���ǡ�*** = 999, NAMAE�פȤ��ä�ɽ�������ä��顢�ǽ�Ρ�,�װʹߤ�̵���ˤ���
	# ����ϡ�����COBOL��COPY�缫ư������������Ƥ�پ㤬�ʤ��褦�ˤ��뤿������֤Ǥ���
	

	w_data1.each_index do |i|
		if w_data1[i] =~ /^[\s]*?\$LineTargetStart/i
			# $LineTargetStart����Ǥ���
			# ��Ƭ�ζ����ʤ���
			w_data1[i].gsub! /^\s*?([^\s])/, '\1'
			# ��,������ζ����ʤ���
			w_data1[i].gsub! /([^\s])\s*?(\,)\s*?([^\s])/, '\1\2\3'
			# ��=�פ�����ζ����ʤ���
			w_data1[i].gsub! /([^\s])\s*?(\=)\s*?([^\s])/, '\1\2\3'
		elsif w_data1[i] =~ /^(\s*?\$LineTargetEnd)/i
			# $LineTargetEnd����Ǥ���
			# ��Ƭ�ζ����ʤ���
			w_data1[i].gsub! /^\s*?([^\s])/, '\1'
			# ��,������ζ����ʤ���
			w_data1[i].gsub! /([^\s])\s*?(\,)\s*?([^\s])/, '\1\2\3'
		elsif w_data1[i] =~ /^\s*?\$/
			# $����Ǥ���
			# ��Ƭ�ζ����ʤ���
			w_data1[i].gsub! /^\s*?([^\s])/, '\1'
		elsif w_data1[i] =~ /^\s*?\#/
			# ��#�ץ����ȤǤ���
			if i > 0
				# �����ȹԤ�ʤ���
				w_data1[i] = ''
			end
#			# ��Ƭ�ζ����ʤ���
#			w_data1[i].gsub! /^\s*?([^\s])/, '\1'
		elsif w_data1[i] =~ /^\s*?$/
			# ����ԤǤ���
		else
			# �ѥ�᡼���Ǥ���
			# ��Ƭ�ζ����ʤ���
			w_data1[i].gsub! /^\s*?([^\s])/, '\1'
			# ��=�פ�����ζ����ʤ���
			w_data1[i].gsub! /([^\s])\s*?(\=)\s*?([^\s])/, '\1\2\3'
			# ���դΡ�,�װʹߤ�ʤ���
			w_data1[i].gsub! /^([^\,]+?)\,[\s\S]*?$/, '\1'
			w_data1[i].gsub! /([^\S])\s$/, '\1'
		end
	end


	# ����Ԥ�ʤ���
	w_data2 = w_data1
	w_data1 = []
	w_data2.each do |d|
		if d =~ /^[\s]*?\$LineTargetStart/i
			# $LineTargetStart����Ǥ���
			w_data1.push d
		elsif d =~ /^(\s*?\$LineTargetEnd)/i
			# $LineTargetEnd����Ǥ���
			w_data1.push d
		elsif d =~ /^\s*?\$/
			# $����Ǥ���
			w_data1.push d
		elsif d =~ /^\s*?\#/
			# ��#�ץ����ȤǤ���
			if d =~ /^\s*?\#\$/
				# �����ܤΥ����ȤΤ߻Ĥ�
				w_data1.push d
			end
		elsif d =~ /^\s*?$/
			# ����ԤǤ���
		else
			# �ѥ�᡼���Ǥ���
			w_data1.push d
		end
	end













	hir_data = w_data1



	# �ե�����ؤν񤭹���
# ****************************************
$stderr.puts '  ���إǡ����ե�����ؽ񤭹�����Ǥ�'		if (DEBUG & 1) == 1
# ****************************************
	open(hir_filename, "w") do |fp2|
		hir_data.each do |x2|
			fp2.puts x2
		end
	end
# ****************************************
$stderr.puts '  ���إѥ�᡼����󥯥ե�����ؤν񤭹��ߤ������ޤ���'		if (DEBUG & 1) == 1
# ****************************************


end



# =============================================================================


# ���إǡ����Υ���С��Ƚ��� (��¸����)
#    ����
# in_data - ����С������γ��إǡ��� (String��)
#    �����
# ����С��ȸ�γ��إǡ��� ((String����)����)
#    ����
# ���Υ���С��Ƚ����ϡ���¸�η������Ѵ������ΤǤ���
# ��˹�®���б��Τ��ᡢ�̤Υ���ץ�ʷ������Ѵ������Τ��ѹ�����
# ��®���б��κݤˤϡ����ǡ������Ф��ơ�������ѹ���ɬ�פˤʤ�(def�ե�����Ȥ�����Τ��ᡢ�Х��ȿ������Ƥ������ɬ�פ����뤿��Ǥ���)
# Repeat����μ¹Ԥ򡢤��餫���ᴰλ���Ƥ��뤳��
def hierarchy_conversion_proc_a(in_data)

	w_data1 = ''
	w_data2 = []

	w_str1 = []
	w_str2 = []
	w_str3 = ''


	# ���إǡ�����base�������
	w_data1 = basehierarchy_extend_proc(in_data)


	# LineTargetStart��LineTargetEnd�μ��̻������б�
	w_data1 = linetarget_setmark_proc(w_data1)


	# LineTargetStart�δ�¸�����ؤ��Ѵ�
	w_data2 = w_data1.split("\n")

	w_data2.each_index do |i|
		if w_data2[i] =~ /^[\s]*?\$LineTargetStart/i
			# $LineTargetStart����Ǥ���
#		if w_data2[i] =~ /^[^\s]*?$/ and w_data2[i] =~ /^\s*?[^\#]+/
			w_str1 = w_data2[i].split(',')
			# w_str1[0] = $LineTargetStart
			# w_str1[1] = ���̻�
			# w_str1[2] = ����оݳ��� = ����
			# w_str1[3] = �����ꥸ��γ��ϰ���
			# w_str1[4] = �ִ��оݳ���
			w_str2 = w_str1[2].split('=')		# ���̻Ҥμ�����Ӥ�ʬ�򤹤�
			# �������ˤ���Ƭ�ˡ�/�פ����뤬����¸��������Ƭ�ˡ�~�פ��ʤ��Τǡ��������
			if w_str1[4] =~ /^(\s*?)\/([\s\S]*?)$/
				w_str1[4] = $1 + $2
			end
			if w_str2[0] =~ /^(\s*?)\/([\s\S]*?)$/
				w_str2[0] = $1 + $2
			end
			w_str3 = w_str1[0] + ', ' + w_str1[1] + ', ' + w_str1[4] + ', =, ' + w_str2[0] + ', ' + w_str2[1] + ', ' + String(Integer(w_str1[3]) - 1)
			w_data2[i] = w_str3

			w_str1 = []
			w_str2 = []
			w_str3 = ''
		elsif w_data2[i] =~ /^(\s*?\$LineTargetEnd)/i
			# $LineTargetEnd����Ǥ���
		elsif w_data2[i] =~ /^\s*?\$/
			# $����Ǥ���
		elsif w_data2[i] =~ /^\s*?\#/
			# ��#�ץ����ȤǤ���
		elsif w_data2[i] =~ /^\s*?$/
			# ����ԤǤ���
		else
			# �ѥ�᡼���Ǥ���
			# �������ˤ���Ƭ�ˡ�/�פ����뤬����¸��������Ƭ�ˡ�~�פ��ʤ��Τǡ��������
			if w_data2[i] =~ /^(\s*?)\/([\s\S]*?)$/
				w_data2[i] = $1 + $2
			end
		end
	end

	# ��¸�����ϡ���Ƭ�ˡ�~�פ��Ĥ��ʤ��Τǡ����λ����ǡ���Ƭ�Ρ�/�פ�ʤ�����
	w_data2.each_index do |i|
		if w_data2[i] =~ /^[\s]*?\$LineTargetStart/i
			# $LineTargetStart����Ǥ���
		elsif w_data2[i] =~ /^(\s*?\$LineTargetEnd)/i
			# $LineTargetEnd����Ǥ���
		elsif w_data2[i] =~ /^\s*?\$/
			# $����Ǥ���
		elsif w_data2[i] =~ /^\s*?\#/
			# ��#�ץ����ȤǤ���
		elsif w_data2[i] =~ /^\s*?$/
			# ����ԤǤ���
		else
			# �ѥ�᡼���Ǥ���
		end
	end


	# ��Ƭ�˶��򤬤���С������������
	w_data2.each_index do |i|
		w_data2[i].gsub! /^\s+?([^\s])/, '\1'
	end


	# ���Хѥ��ˤʤä���Τ��Ф��ơ���¸����
	w_data1 = w_data2.join("\n")



	# �̾�Υǡ������/����̾[�����ֹ�]/����̾[�����ֹ�]@°��̾�פ���
	#   ��~����̾(�����ֹ�)~����̾(�����ֹ�)!°��̾�פ��Ѵ�����
	w_data1.gsub! /\//, '~'
	w_data1.gsub! /\@/, '!'
	w_data1.gsub! /\[/, '('
	w_data1.gsub! /\]/, ')'


	return w_data1

end



# =============================================================================

# ���إǡ�����¸���� (��¸����)
#    ����
# in_data - ����С��ȸ�γ��إǡ��� (String��)
# hir_filename - ���إǡ����ե�����̾
# def_filename - ����def�ե�����̾
#    ����
# ���γ��إǡ�����¸�����ϡ���¸�η������Ѵ������ΤǤ���
# �ҤȤĤˤޤȤ�Ƥ���ե�����򡢴�¸�η�����ʬ�䡦�Ѵ�������¸����
def hierarchy_save_proc_a(in_data, hir_filename, def_filename)

	def_data = []		# def�ե�����˽��Ϥ���ǡ���
	hir_data = []		# ���إѥ�᡼����󥯥ե�����˽��Ϥ���ǡ���
	# ����®���Τ��ᡢ�嵭������(1���֤ν�����16�ä�û��...)
	param_num = 0		# �ѥ�᡼��̾�ǻ��Ѥ����ֹ�ν����
	offset_count = 0	# �Х��ȿ����饪�ե��åȤ�׻�����
	w_data1 = []
	w_data2 = []
	w_data3 = []
	w_str1 = ''
	w_str2 = ''


	w_data1 = in_data.split("\n")


	# ��Ƭ�ԤΡ�#$ type=hierarchy version=2.0 encoding=EUC-JP�פ�������
	if w_data1[0] =~ /\A\s*?\#\$[\s\S]+?$/
		w_data1.shift
	end



# ****************************************
$stderr.puts '  ���إǡ�����def�ǡ����ȳ��إ�󥯥ǡ�����ʬΥ���ޤ�'		if (DEBUG & 1) == 1
# ****************************************

	w_data1.each do |x|
		if x =~ /^\s*?$/
			# ���ԤǤ���
			def_data.push "\n"
			hir_data.push "\n"
		elsif x =~ /^\s*?\#[\s\S]*?$/
			# �����ȹԤǤ���
			def_data.push x + "\n"
			hir_data.push x + "\n"
		elsif x =~ /^\s*?\$LineTargetStart\s*?,\s*?(\S*?)/i
			def_data.push '# LineTargetStart [' + $1 + "]\n"
			hir_data.push x + "\n"
		elsif x =~ /^\s*?\$LineTargetEnd\s*?,\s*?(\S*?)/i
			def_data.push '# LineTargetEnd   [' + $1 + "]\n"
			hir_data.push x + "\n"
		else
			# ���ؾ���ԤǤ���
			param_num += 1		# �ѥ�᡼���ֹ�βû�
			w_str1 = '%' + sprintf("%06d", param_num) + '%'		# �ѥ�᡼��̾�Υ��å�(max��99���ޤ�)
			if x !~ /=/
				puts '���ؾ���ˡ�=�פ��ޤޤ�Ƥ��ʤ��Ԥ�����ޤ�'
				puts x
				puts ''
			end
			w_data2 = x.split('=')
			w_data3 = w_data2[1].split(',')		# �Х��ȿ���COBOL¦̾�Τ�ξ������������θ
			def_data.push w_str1 + ', 0, ' + String(offset_count) + ', ' + w_data3[0]
			if w_data2[0] =~ /^[\S\s]+?\!([\S]+?)\s*?$/
				# °���Ǥ���
				def_data.push ', , ' + $1
			end
			def_data.push "\n"
			hir_data.push w_data2[0] + ' = ' + w_str1 + "\n"
			offset_count += Integer(w_data3[0])
		end

	end

# ****************************************
$stderr.puts '  ʬΥ�����������ޤ���'		if (DEBUG & 1) == 1
# ****************************************

	# �ե�����ؤν񤭹���
# ****************************************
$stderr.puts '  ���إѥ�᡼����󥯥ե�����ؽ񤭹�����Ǥ�'		if (DEBUG & 1) == 1
# ****************************************
	open(hir_filename, "w") do |fp2|
		hir_data.each do |x2|
			fp2.print x2
		end
	end
# ****************************************
$stderr.puts '  ���إѥ�᡼����󥯥ե�����ؤν񤭹��ߤ������ޤ���'		if (DEBUG & 1) == 1
# ****************************************
# ****************************************
$stderr.puts '  def�ե�����ؽ񤭹�����Ǥ�'		if (DEBUG & 1) == 1
# ****************************************
	open(def_filename, "w") do |fp3|
		def_data.each do |x3|
			fp3.print x3
		end
	end
# ****************************************
$stderr.puts '  def�ե�����ؤν񤭹��ߤ������ޤ���'		if (DEBUG & 1) == 1
# ****************************************


	return
end



# =============================================================================



# =============================================================================



case	conv_mode
when	CONV_MODE_EXISTING_DEF
	# def�ե�����δ�¸�����ؤ��Ѵ��Ǥ���

	in_data1 = ''		# ���ϥե����������
	data2 = []		# ���ϥե���������Ƥ����󲽤������
	out_data1 = []		# ��������(�Ǹ�ޤ�����ˤ���(1.6�ޤǤϡ�ʸ����ǥޡ�������ȡ�®�٤��٤��ʤ�))

	# �ե�������ɤ߹���
	open(argv_f[0], "r") do |fp1|
		in_data1 = fp1.read
	end

	# Repeat�������
	in_data1 = repeat_extend_proc(in_data1)

	# def�ǡ����Υ���С��Ƚ���
	out_data1 = def_conversion_proc_a(in_data1)

	# �ե�����ν񤭹���
	open(argv_f[1], "w") do |fp2|
		out_data1.each do |x|
			fp2.puts x
		end
	end


# ------------------------------------------------------------
when	CONV_MODE_EXISTING_XML
	# XML�ƥ�ץ졼�ȥե�����δ�¸�����ؤ��Ѵ��Ǥ���

	in_data1 = ''		# ���ϥե����������
#	out_data1 = []		# ��������(�Ǹ�ޤ�����ˤ���(1.6�ޤǤϡ�ʸ����ǥޡ�������ȡ�®�٤��٤��ʤ�))
	out_data1 = ''		# ��������

	# �ե�������ɤ߹���
	open(argv_f[0], "r") do |fp1|
		in_data1 = fp1.read
	end


	# Repeat�������
	out_data1 = repeat_extend_proc(in_data1)

	# �ե�����ν񤭹���
	open(argv_f[1], "w") do |fp2|
		fp2.print out_data1
	end


# ------------------------------------------------------------
when	CONV_MODE_EXISTING_HIR
	# ���إǡ����ե�����δ�¸�����ؤ��Ѵ��Ǥ���

	in_data1 = ''		# ���ϥե����������
#	out_data1 = []		# ��������(�Ǹ�ޤ�����ˤ���(1.6�ޤǤϡ�ʸ����ǥޡ�������ȡ�®�٤��٤��ʤ�))
	out_data1 = ''		# ��������

	# �ե�������ɤ߹���
	open(argv_f[0], "r") do |fp1|
		in_data1 = fp1.read
	end

	# Repeat�������
# ****************************************
$stderr.puts 'Repeat��������򳫻Ϥ��ޤ�'		if (DEBUG & 1) == 1
# ****************************************
	in_data1 = repeat_extend_proc(in_data1)
# ****************************************
$stderr.puts 'Repeat��������������ޤ���'		if (DEBUG & 1) == 1
# ****************************************

	# ���إǡ����Υ���С��Ƚ���
# ****************************************
$stderr.puts '���إǡ����Υ���С��Ƚ����򳫻Ϥ��ޤ�'		if (DEBUG & 1) == 1
# ****************************************
	out_data1 = hierarchy_conversion_proc_a(in_data1)
# ****************************************
$stderr.puts '���إǡ����Υ���С��Ƚ����������ޤ���'		if (DEBUG & 1) == 1
# ****************************************

	# ���إǡ�����¸����
# ****************************************
$stderr.puts '���إǡ�����¸�����򳫻Ϥ��ޤ�'		if (DEBUG & 1) == 1
# ****************************************
	hierarchy_save_proc_a(out_data1, argv_f[2], argv_f[1])
# ****************************************
$stderr.puts '���إǡ�����¸�����������ޤ���'		if (DEBUG & 1) == 1
# ****************************************


# ------------------------------------------------------------
when	CONV_MODE_REPEAT
	# Repeat������Ѵ��Ǥ���

	in_data1 = ''		# ���ϥե����������
#	out_data1 = []		# ��������(�Ǹ�ޤ�����ˤ���(1.6�ޤǤϡ�ʸ����ǥޡ�������ȡ�®�٤��٤��ʤ�))
	out_data1 = ''		# ��������

	# �ե�������ɤ߹���
	open(argv_f[0], "r") do |fp1|
		in_data1 = fp1.read
	end


	# Repeat�������
	out_data1 = repeat_extend_proc(in_data1)

	# �ե�����ν񤭹���
	open(argv_f[1], "w") do |fp2|
		fp2.print out_data1
	end


# ------------------------------------------------------------
when	CONV_MODE_VER200_HIR
	# ���إǡ����ե������ver2�����ؤ��Ѵ��Ǥ���
	in_data1 = ''		# ���ϥե����������
#	out_data1 = []		# ��������(�Ǹ�ޤ�����ˤ���(1.6�ޤǤϡ�ʸ����ǥޡ�������ȡ�®�٤��٤��ʤ�))
	out_data1 = ''		# ��������

	# �ե�������ɤ߹���
	open(argv_f[0], "r") do |fp1|
		in_data1 = fp1.read
	end

	# Repeat�������
# ****************************************
$stderr.puts 'Repeat��������򳫻Ϥ��ޤ�'		if (DEBUG & 1) == 1
# ****************************************
	in_data1 = repeat_extend_proc(in_data1)
# ****************************************
$stderr.puts 'Repeat��������������ޤ���'		if (DEBUG & 1) == 1
# ****************************************
	# ���إǡ����Υ���С��Ƚ��� (ver2����)
# ****************************************
$stderr.puts '���إǡ����Υ���С��Ƚ����򳫻Ϥ��ޤ�'		if (DEBUG & 1) == 1
# ****************************************
	out_data1 = hierarchy_conversion_proc_ver200(in_data1)
# ****************************************
$stderr.puts '���إǡ����Υ���С��Ƚ����������ޤ���'		if (DEBUG & 1) == 1
# ****************************************

	# ���إǡ�����¸���� (ver2����)
# ****************************************
$stderr.puts '���إǡ�����¸�����򳫻Ϥ��ޤ�'		if (DEBUG & 1) == 1
# ****************************************
	hierarchy_save_proc_ver200(out_data1, argv_f[1])
# ****************************************
$stderr.puts '���إǡ�����¸�����������ޤ���'		if (DEBUG & 1) == 1
# ****************************************




# ------------------------------------------------------------
end
