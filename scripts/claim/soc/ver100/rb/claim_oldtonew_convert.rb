#!/usr/bin/ruby
# coding : euc-jp
Encoding.default_external = "euc-jp"



# Claim��¸�������������Ѵ�����С���

# ���
# $Repeat����ϼ��Ȥǽ������Ƥ���������
# xml�ƥ�ץ졼�ȥե�����ϡ��Ѵ���ɬ�פ�����ޤ��󡣡�$Repeat����ν����ϼ��ȤǤ���
# equal����Τ�����ʸ���������򤷤Ƥ����Τϡ����餫������Ȥǽ������Ƥ���ԤäƤ���������
# equal����Ρ�a, equal, b \n b, equal, c \n c, equal, a�פΤ褦�ʤ�Τˤ��б����Ƥ��ޤ��󡣡ʺǰ���̵�¥롼�פ��ޤ���
#
#
# ���ߺ����
# �ʤ�
#
# ��������
# 2003/06/26 - ��-H�׻���ǡ�def�ե������link�ե�����λ��꤬�դˤʤäƤ����Τ���
# 2003/06/27 - def�ե������°��������¤Ӥ�狼��䤹�����롣��COBOL��INC�ե�����ˤ��䤹�������
# 2003/07/11- - ������Υե����뤫�鿷�����Υե�����ؤΰܹԤ��θ�����إå�������ɲä���




if __FILE__ == $0

# =============================================================================
# �ǥХå��б�

	# �ǥХå��Ѱ�������
	debug_argv_set = 0	# �ǥХå��ǤϤʤ�
#	debug_argv_set = 1	# def�ե�����δ�¸������Ѵ��ǤΥǥХå�
#	debug_argv_set = 2	# xml�ƥ�ץ졼�ȥե�����δ�¸������Ѵ��ǤΥǥХå�
#	debug_argv_set = 3	# ���إǡ����ե�����δ�¸������Ѵ��ǤΥǥХå�



# =============================================================================
# ����λ���

# def�ե�����δ�¸����������Ѵ��Ǥ���
OLDCONV_MODE_DEF = 1
# XML�ƥ�ץ졼�ȥե�����δ�¸����������Ѵ��Ǥ���
OLDCONV_MODE_XML = 2
# ���إǡ����ե�����δ�¸����������Ѵ��Ǥ���
OLDCONV_MODE_HIR = 3


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
		argv_f.push 'test011old.def'		# �ƥ��ȼ¹Ի����ɤ߹���ե�����̾
		argv_f.push 'test011new.def'		# �ƥ��ȼ¹Ի��ν��ϥե�����̾
		conv_mode = OLDCONV_MODE_DEF
	when	2
		# xml�ƥ�ץ졼�ȥե�����δ�¸�ؤ��Ѵ��ǤΥǥХå�
		argv_f.push 'test012old.xml'		# �ƥ��Ȼ����ɤ߹���XML�ƥ�ץ졼�ȥե�����̾
		argv_f.push 'test012new.xml'		# �ƥ��Ȼ��˽��Ϥ���XML�ƥ�ץ졼�ȥե�����̾
		conv_mode = OLDCONV_MODE_XML
	when	3
		# ���إǡ����ե�����δ�¸�ؤ��Ѵ��ǤΥǥХå�
		argv_f.push 'test013old.def'		# �ƥ��Ȼ����ɤ߹���def�ե�����̾
		argv_f.push 'test013old.link'		# �ƥ��Ȼ����ɤ߹��೬�إѥ�᡼����󥯥ե�����̾
		argv_f.push 'test013new.hir'		# �ƥ��Ȼ��˽��Ϥ��볬�إǡ����ե�����̾
		conv_mode = OLDCONV_MODE_HIR
	end



	if argv_size != 0
		ARGV.each do |x|
			case	x
			when	'--help'
				# �إ�ץ�å�������ɽ��������
				help_flg = 1
			when	'-D'
				# def�ե�����δ�¸�����ؤ��Ѵ��Ǥ���
				conv_mode = OLDCONV_MODE_DEF
			when	'-X'
				# XML�ƥ�ץ졼�ȥե�����δ�¸�����ؤ��Ѵ��Ǥ���
				conv_mode = OLDCONV_MODE_XML
			when	'-H'
				# ���إǡ����ե�����δ�¸�����ؤ��Ѵ��Ǥ���
				conv_mode = OLDCONV_MODE_HIR
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
		mes += '        def�ե�����δ�¸����������Ѵ�' + "\n"
#		mes += '    -X [����xml�ե�����] [����xml�ե�����]' + "\n"
#		mes += '        xml�ƥ�ץ졼�ȥե�����δ�¸����������Ѵ�' + "\n"
		mes += '    -H [����def�ե�����] [���ϳ��إѥ�᡼����󥯥ե�����] [���ϳ��إǡ����ե�����]' + "\n"
		mes += '        ���إǡ����ե�����δ�¸����������Ѵ�' + "\n"
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
	when	OLDCONV_MODE_DEF
		# def�ե�������Ѵ��Ǥ���
		if argv_f.size != 2
			puts '�����ˡ�����def�ե�����Ƚ���def�ե�����Σ��Ĥ���ꤷ�Ƥ�������'
			exit 10
		end
#	when	OLDCONV_MODE_XML
#		# XML�ƥ�ץ졼�ȥե�������Ѵ��Ǥ���
#		if argv_f.size != 2
#			puts '�����ˡ�����xml�ƥ�ץ졼�ȥե�����Ƚ���xml�ƥ�ץ졼�ȥե�����Σ��Ĥ���ꤷ�Ƥ�������'
#			exit 10
#		end
	when	OLDCONV_MODE_HIR
		# ���إǡ����ե�����Ǥ���
		if argv_f.size != 3
			puts '�����ˡ�����def�ե���������ϳ��إѥ�᡼����󥯥ե�����Ƚ��ϳ��إǡ����ե�����Σ��Ĥ���ꤷ�Ƥ�������'
			exit 10
		end
	else
		puts '�¹Ի����顼�Ǥ�'
		exit 10
	end



end



# =============================================================================


# def�ե�����ǡ������Ѵ�
#    ����
# in_data - �Ѵ��о�def�ե�����ǡ��� [String��/IN]
#    �����
# �Ѵ����def�ե�����ǡ��� [String��/OUT]
def	def_convert_proc(in_data)

	w_line1 = []	# ��ñ�̤������ʬ������Τ�������ΰ�
	w_line2 = []	# ����ñ�̤������ʬ������Τ�������ΰ�
	w_line3 = ''	# ���������Ȥ������ʸ����γ�Ǽ�ΰ�
	w_str1 = ''
	w_str2 = ''
	w_str3 = ''
	w_str4 = ''

	w_comment1 = ''		# ���������Ȥγ�Ǽ�ΰ�

	w_idx1 = 0
	w_idx2 = 0


	w_line1 = in_data.split("\n")		# ���Ԥ������ʬ����


	w_line1.each_index do |w_idx1|

		# ���������ȤΤߡ����ΰ����¸�ʤ������������ȤΤߤιԤϽ�����
		if w_line1[w_idx1] =~ /^([\s\S]*?\S+)(\s*?\#[\s\S]*?)$/
			w_line3 = $1
			w_comment1 = $2
		else
			w_line3 = w_line1[w_idx1]
			w_comment1 = ''
		end


		# equal�����ʸ���󤬤��ä��ꡢ���󤬤���С��ٹ��å�������Ф���
		if w_line3 =~ /^(\s*?\S+?\s*?,\s*?equal\s*?,([\S\s]+?))$/i
			w_str2 = $1
			w_str3 = $2
			if w_str3 =~ /^\s*?$/
				puts 'equalʸ����ˡ�����򥻥åȤ��Ƥ����Τ�����ޤ�'
				puts w_str2
				puts '������claim�̿��ϡ�ʸ�����equal����򥵥ݡ��Ȥ��Ƥ��ޤ���Τǡ���դ��Ƥ�������'
				puts ''
			elsif w_str3 !~ /^\s*?[0-9\+\*\-\/\(\)\s]+?\s*?$/
				puts 'equalʸ����ˡ������ʳ��򥻥åȤ��Ƥ����Τ�����ޤ�'
				puts w_str2
				puts '������claim�̿��ϡ�ʸ�����equal����򥵥ݡ��Ȥ��Ƥ��ޤ���Τǡ���դ��Ƥ�������'
				puts ''
			end
		end


		if w_line3 =~ /^\s*?\#/i
			# �����ȤǤ���
		elsif w_line3 =~ /^\s*?$/i
			# ����ιԤǤ���
		elsif w_line3 =~ /^(\s*?)(\S+?)\s*?,\s*?equal\s*?,(\s*?)([\S\s]+?)\s*?$/i
			# equal��ʸ���Ѵ�
				# $1 = ���ζ���
				# $2 = ���̾
				# $3 = �ͤ����ζ���
				# $4 = ��
			w_line1[w_idx1] = $1 + '$define, ' + $2 + ',' + $3 + $4 + w_comment1
		elsif w_line3 =~ /^(\s*?)(\S+?)\s*?,\s*?const\s*?,(\s*?)([\S\s]+?)\s*?$/i
			# const��ʸ���Ѵ�
				# $1 = ���ζ���
				# $2 = �ѥ�᡼��̾
				# $3 = ���Ƥ����ζ���
				# $4 = ����
			w_line1[w_idx1] = $1 + '$const, ' + $2 + ',' + $3 + $4 + w_comment1
		elsif w_line3 =~ /^(\s*?)(\S+?)\s*?,\s*?nowdate1\s*?$/i
			# nowdate1��ʸ���Ѵ�
				# $1 = ���ζ���
				# $2 = �ѥ�᡼��̾
			w_line1[w_idx1] = $1 + '$nowdate, ' + $2 + ', %Y-%m-%d' + w_comment1
		elsif w_line3 =~ /^(\s*?)(\S+?)\s*?,\s*?nowdate2\s*?$/i
			# nowdate2��ʸ���Ѵ�
				# $1 = ���ζ���
				# $2 = �ѥ�᡼��̾
			w_line1[w_idx1] = $1 + '$nowdate, ' + $2 + ', %Y-%m-%dT%H:%M:%S' + w_comment1
		elsif w_line3 =~ /^(\s*?)(\S+?)\s*?,\s*?ifdef\s*?,([\S\s]+?)\s*?$/i
			# ifdef��ʸ���Ѵ�
				# $1 = ���ζ���
				# $2 = ����̾
				# $3 = [and �ޤ��� or]�ʹߤ�����
			w_line1[w_idx1] = $1 + '$ifdef, ' + $3 + w_comment1
		elsif w_line3 =~ /^(\s*?)(\S+?)\s*?,\s*?endif\s*?$/i
			# endif��ʸ���Ѵ�
				# $1 = ���ζ���
				# $2 = ����̾
			w_line1[w_idx1] = $1 + '$endif' + w_comment1
		else
			# �ѥ�᡼���ιԤȻפ���
			w_line2 = w_line3.split(',')
			# ����ζ���������
			w_line2.each_index do |w_idx2|
				w_line2[w_idx2].strip!
			end
			if (w_line2[5] == nil) or (w_line2[5] == '')
				# �����ܤι��ܤ�����ξ�����ϡ��̾�Υѥ�᡼���Ǥ���
				w_line3 =~ /^(\s*?)(\S+?)\s*?,\s*?0,\s*?([^,]+?)\s*?,(\s*?[\S\s]+?)$/
					# $1 = ���ζ���
					# $2 = �ѥ�᡼��̾
					# $3 = ���ե��å�
					# $4 = �Х��ȿ�(���ζ����ޤ�)
				w_str2 = $1 + $2 + ','
				w_str3 = $4
				w_str3.gsub! /^([^,]*?),/, '\1'	# ���Υ���޺���ϡ����󷫤��֤�(����ξ�礬����Τ�)
				w_str3.gsub! /^([^,]*?),/, '\1'
				w_line1[w_idx1] = w_str2 + w_str3 + w_comment1
			else
				# �����ܤι��ܤ˲������äƤ��������ϡ�°���Υѥ�᡼���Ǥ���
				w_line3 =~ /^(\s*?)(\S+?)\s*?,\s*?0,\s*?([^,]+?)\s*?,(\s*?[^,]+?)\s*?,[^,]*?,(\s*?[\S\s]+?)$/
					# $1 = ���ζ���
					# $2 = �ѥ�᡼��̾
					# $3 = ���ե��å�
					# $4 = �Х��ȿ�(���ζ����ޤ�)
					# $5 = °��̾(���ζ����ޤ�)
				w_str2 = $1 + $2 + ','
				w_str3 = $4 + ','
				w_str4 = $5
				w_str4.gsub! /^([^,]*?),/, '\1'	# ���Υ���޺���ϡ����󷫤��֤�(����ξ�礬����Τ�)
				w_str4.gsub! /^([^,]*?),/, '\1'
				w_line1[w_idx1] = w_str2 + w_str3 + w_str4 + w_comment1
#				w_line1[w_idx1] = $1 + $2 + ',' + $4 + ',' + $5 + w_comment1
			end
		end
	end


	# ���̾������Ƭ���ɲä���
	w_line1.unshift('#$ type="figure-define" version="2.0" encoding="EUC-JP"')


	# �����ʬ������Τ򸵤��᤹
	out_data = w_line1.join("\n")




	return out_data

end



# =============================================================================



# =============================================================================


# ��def�ե�����ǡ�����equal���Ÿ������
#    ����
# in_def - ��def�ե�����ǡ�����equal���Ÿ�����ǡ��� (String��/IN)
#    �����
# def�ե�����ǡ�����equal���Ÿ����Υǡ��� (String��/OUT)
def defdata_expand_equal(in_def)

	w_def1 = []

	out_def = ''

	w_idx1 = 0
	w_idx2 = 0
	w_idx3 = 0
	w_line1 = ''
	w_line2 = []
	w_line3 = []
	w_line4 = ''

	w_ary1 = []

	equal_stack = []		# equal�ǡ��������å� 1�����[�ѥ�᡼��̾, ������������]������

	chg_flg1 = 0		# �Ѵ��ե饰(0 = �Ѵ����ʤ��ä�, 1 = �Ѵ�����)
	chg_flg2 = 0		# ���Ԥ��Ѵ��ե饰�ν����(0 = �ѹ����ʤ��ä�, 1 = �ѹ�����)
	chg_flg3 = 0		# ���Ԥ��Ѵ��ե饰�ν����(0 = �ѹ����ʤ��ä�, 1 = �ѹ�����)


	# ��ñ�̤�����ˤ���
	w_def1 = in_def.split("\n")


	# equal�ǡ����򥹥��å��ˤ���ơ������ΰ褫����
	w_def1.each_index do |w_idx1|
		w_line1 = w_def1[w_idx1]
		if w_line1 =~ /^\s*?$/
			# ����ԤǤ���
		elsif w_line1 =~ /^\s*?\#[\s\S]*?$/
			# �����ȹԤǤ���
		else
			# ����Ԥ䥳���ȹԤǤϤʤ�
			w_line2 = w_line1.split(',')
			w_line2.each_index do |w_idx2|
				w_line2[w_idx2].strip!
			end
			if w_line2[1] == 'equal'
				# �����Ԥϡ�equal����ιԤǤ��롣
				equal_stack.push [w_line2[0], w_line2[2]]
				# equal����ιԤʤΤǡ������Ԥ����ˤ���
				w_def1[w_idx1] = ''
			end
		end
	end


	# equal�����å��������ǡ������equal�����Ÿ����Ԥ����ʤ��������롼�����Ÿ�����٤⤷�ʤ���С���̿Ū���Ѵ����顼��
	chg_flg1 = 1		# �Ѵ��ե饰(0 = �Ѵ����ʤ��ä�, 1 = �Ѵ�����)
	notnum_flg = 1	# �����ʳ�¸�ߥե饰(0 = �����ʳ���¸�ߤ��ʤ��ä�, 1 = �����ʳ���¸�ߤ���)
	while notnum_flg == 1
		chg_flg1 = 0		# �Ѵ��ե饰�ν����(0 = �Ѵ����ʤ��ä�)
		notnum_flg = 0	# �����ʳ�¸�ߥե饰�ν����(0 = �����ʳ���¸�ߤ��ʤ��ä�)

		equal_stack.each_index do |w_idx3|
			w_line3 = equal_stack[w_idx3]

			chg_flg2 = 0		# ���Ԥ��Ѵ��ե饰�ν����(0 = �ѹ����ʤ��ä�, 1 = �ѹ�����)



			# �����ʳ�����Ƚ��
			if w_line3[1] !~ /^\s*?[0-9]+?\s*?$/
				# �����ʳ��Ǥ���
				notnum_flg = 1	# �����ʳ�¸�ߥե饰�Υ��å�

				# ��+-*/()�פ�ʬ����ʸ����Ȥ��������å���Υѥ�᡼���򸡺�����
				# �������������Τߡ���+-*/()�פΤߤ����
				# ���Ĥ��ä��顢�ѥ�᡼����Ÿ���򤷡��Ѵ��ե饰�򥻥åȤ���
				w_len1 = 0
				w_idx4 = 0
				w_idx5 = 0
				w_idx6 = 0
				w_idx7 = 0
				w_item1 = []
				w_str1 = ''
				w_str2 = ''
				w_str3 = ''

				w_str1 = w_line3[1]
				w_len1 = w_str1.length	# ʸ����Ĺ�������

				for w_idx4 in 0..(w_len1 - 1) do
					w_str3 = w_str1[w_idx4, 1]	# �оݤΣ�ʸ�������
					case	w_str3
					when	'+', '-', '*', '/', '(', ')'
						# ���ڤ�Ǥ���
						w_item1.push w_str2.strip		# ����̾�����å��˥��å�
						w_item1.push w_str3		# ���ڤ국��������
						w_str2 = ''		# ����̾�ν����
					else
						# ���ڤ�ʳ��Ǥ���
						w_str2 += w_str3
					end
				end
				if w_str2 != ''
					w_item1.push w_str2.strip
					w_str2 = ''		# ����̾�ν����
				end


				# �ѥ�᡼����Ÿ���򤷡��Ѵ��ե饰�򥻥å�
				# �⤷�⡢���դȺ��դ�Ʊ�����ϡ���̿Ū���顼�ʤΤǡ�������λ������
				w_item1.each_index do |w_idx5|
					# ���դ�Ʊ��̾�������դˤ���С���̿Ū���顼�Ȥ���
					if w_line3[1] == w_item1[w_idx5]
						puts 'equalʸ�ˡ�����Ÿ���Ǥ��ʤ���Τ�����ޤ�'
						puts '[' + w_line3[1] + ']'
						exit 10
					end
					case	w_item1[w_idx5]
					when	'+' , '-', '*', '/', '(', ')'
						# ���ڤ�ʸ���Ǥ���
					else
						# �����ʳ������äƤ���С�Ÿ������
						if w_item1[w_idx5] =~ /[^0-9]/
							# �����ʳ��Ǥ���
							if w_item1[w_idx5] =~ /^\s*?$/
								# ����Ǥ���
								# 0�ˤ���
								w_item1[w_idx5] = '0'
							else
								# ����ʳ��Ǥ���
								# �ѥ�᡼����equal�ǡ��������å����鸡������Ÿ����Ȥ򤹤�
								# ������������褬�����ΤߤǤ��뤳�ȡ��ʤ���ʳ��ϡ��ǽ��Ÿ�����ʤ�����ͳ�ϡ���+�פ���*�פ�ͥ�褷�Ƽ���ɾ������Ȥ��������ؾ���������������Ȥˤʤ�Τǡ����Τ褦�ˤ���ȡ��տޤ���ư���ˤʤ�Ȼפ����
								equal_stack.each_index do |w_idx6|
									if equal_stack[w_idx6][0] == w_item1[w_idx5]

										# �ѥ�᡼�������Ĥ��ä�
										if equal_stack[w_idx6][1] !~ /^\s*?[0-9]+?\s*?$/
											# �����ʳ����ޤޤ�Ƥ���
										else
											# �����ʳ����ޤޤ�Ƥ��ʤ��Τǡ����åȤ���
											w_item1[w_idx5] = equal_stack[w_idx6][1]
											chg_flg1 = 1		# �Ѵ��ե饰�Υ��å�
											chg_flg2 = 1		# ���Ԥ��Ѵ��ե饰�Υ��å�
										end
										break	# each_index����ȴ����
									end
								end
							end
						end
					end
				end


				# ���󷿤��顢ʸ���󷿤��᤹
#				w_line4 = w_item1.join('')
				w_line4 = w_item1.to_s

				if w_line4 =~ /[^0-9\+\-\*\/\(\)]+?/
					# �����ʳ��Ǥ���
					# ���Τޤޤˤ���
				else
					# �����β�ǽ��������
					if w_line4 !~ /[\+\-\*\/\(\)]+?/
						# �����ΤߤǤ���
					else
						# �����Ǥ���
						# ����Ÿ������
						w_line4 = String(eval(w_line4))
						chg_flg1 = 1		# �Ѵ��ե饰�Υ��å�
						chg_flg2 = 1		# ���Ԥ��Ѵ��ե饰�Υ��å�
					end
				end

				if chg_flg2 == 1
					# �ѹ��ե饰�����åȤ���Ƥ���Τǡ��ѥ�᡼�������Ƥ��ѹ����줿
					equal_stack[w_idx3][1] = w_line4
					chg_flg1 = 1
					chg_flg2 = 0
				end
			end
			w_line3 = []
		end

		if (chg_flg1 == 0) and (notnum_flg == 1)
			# �����ʳ���¸�ߤ���Τˡ��Ѵ����ʤ��ä�
			puts 'equal����ǡ��������Ѵ��Ǥ��ʤ���Τ�����ޤ�'
			# �����ǡ��Ǥ���С�equal����ǿ����ʳ��Υѥ�᡼��̾��ɽ��

			exit 10
		end

	end


# ---------------------------------------------------------

# equal�����å������٤ƿ��ͤ��Ѵ������С������ѥ�᡼�����ִ����롣
# ���������ǽ���̾�ѥ�᡼���ΤߤȤ������θ�ǡ�̵���gsub��Ԥ���

# equal_stack

	equal_stack.each_index do |w_idx3|
		w_line3 = equal_stack[w_idx3]
		w_def1.each_index do |w_idx1|
			chg_flg3 = 0		# ���Ԥ��Ѵ��ե饰�ν����(0 = �ѹ����ʤ��ä�, 1 = �ѹ�����)
			w_line1 = w_def1[w_idx1]
			if w_line1 =~ /^\s*?$/
				# ����ԤǤ���
			elsif w_line1 =~ /^\s*?\#[\s\S]*?$/
				# �����ȹԤǤ���
			else
				# ����Ԥ䥳���ȹԤǤϤʤ�
				w_line2 = w_line1.split(',')
				w_line2.each_index do |w_idx2|
					w_line2[w_idx2].strip!
				end
				if w_line2[1] == '0'
					# �����Ԥϡ��̾�Υѥ�᡼������ιԤǤ���
#					if w_line2[2] =~ /^\s*?[^0-9]+?\s*?$/
					if w_line2[2] !~ /^\s*?[0-9]+?\s*?$/
						# �����ʳ������äƤ���
#						w_line2[2].gsub! /#{w_line3[0]}/, w_line3[1]
						w_line2[2].gsub! /#{Regexp.quote(w_line3[0])}/, w_line3[1]
						chg_flg3 = 1	# �ѹ��ե饰�Υ��å�
#						if w_line2[2] =~ /^\s*?[^0-9\+\-\*\/\(\)]+?\s*?$/
						if w_line2[2] !~ /^\s*?[0-9\+\-\*\/\(\)]+?\s*?$/
							# �ޤ��ѥ�᡼�����ĤäƤ���Τǡ�����Ÿ���Ϥ��ʤ�
						else
							# ���Ǥ˱黻�Ҥȿ����ΤߤʤΤǡ�����Ÿ���򤹤�
							w_line2[2] = String(eval(w_line2[2]))
						end
					end
					if w_line2[3] !~ /^\s*?[0-9]+?\s*?$/
#					if w_line2[3] =~ /^\s*?[^0-9]+?\s*?$/
						# �����ʳ������äƤ���
						w_line2[3].gsub! /#{Regexp.quote(w_line3[0])}/, w_line3[1]
#						w_line2[3].gsub! /#{w_line3[0]}/, w_line3[1]
						chg_flg3 = 1	# �ѹ��ե饰�Υ��å�
#						if w_line2[3] =~ /^\s*?[^0-9\+\-\*\/\(\)]+?\s*?$/
						if w_line2[3] !~ /^\s*?[0-9\+\-\*\/\(\)]+?\s*?$/
							# �ޤ��ѥ�᡼�����ĤäƤ���Τǡ�����Ÿ���Ϥ��ʤ�
						else
							# ���Ǥ˱黻�Ҥȿ����ΤߤʤΤǡ�����Ÿ���򤹤�
							w_line2[3] = String(eval(w_line2[3]))
						end
					end
					if chg_flg3 != 0
						# �ѹ������ä�
						# w_line2���������Ƥ�ޡ���������Ǽ����
						w_def1[w_idx1] = w_line2.join(',')
					end


				end
			end
		end

	end


	# ���������def�ǡ�����String�����᤹
	out_def = w_def1.join("\n")


	return	out_def

end



# =============================================================================


# �����ѥ�᡼���ε�def�ե�����ǡ�����������
#    ����
# in_def - ��def�ե�����ǡ��� (String��/IN)
# param_name - ���Ƥ��������ѥ�᡼��̾ (String��/IN)
#    �����
# �ѥ�᡼������[���ե��å���, �Х��ȿ�] (����/OUT)
#      nil�ξ�硢���Ĥ���ʤ��ä���
#    ���
# equal�������դ�ʧ������
def get_param_from_defdata(in_def, param_name)

	w_str1 = ''

	# def�ǡ�����equalŸ������


	if in_def =~ /^(\s*?#{Regexp.quote(param_name)}[\s\S]*?)$/
		# �����ѥ�᡼�������Ĥ��ä�
		w_str1 = $1

		w_param1 = []
		out_param = []

		w_param1 = w_str1.split(',')
		# �ѥ�᡼���������å�
		if w_param1.size < 4
			puts 'def�ե�����Υѥ�᡼���Τ���������Υѥ�᡼��������̤���Ǥ�'
			puts w_str1
		end
		# ����ζ���������
		w_param1.each_index do |i|
			w_param1[i].strip!
		end

		# (0���ꥸ���)2���ܤ����ե��å�
		out_param.push w_param1[2]
		# (0���ꥸ���)3���ܤ��Х��ȿ�
		out_param.push w_param1[3]
	else
#		puts '���ꤵ�줿�ѥ�᡼������def�ե����뤫�鸫�Ĥ���ޤ���Ǥ��� [' + param_name + ']'
		out_param = nil
	end


	return out_param
end


# =============================================================================

# ���إѥ�᡼����󥯥ե������def�ե����뤫��γ��إǡ����ե�����ؤ��Ѵ�
#    ����
# in_def    - �Ѵ��о�def�ե�����ǡ��� (String��/IN)
# in_hirlnk - �Ѵ��оݳ��إѥ�᡼����󥯥ǡ��� (String��/IN)
#    �����
# �Ѵ���γ��إǡ��� (����/OUT)
def	hierarchy_convert_proc(in_def, in_hirlnk)

	w_hirlnk1 = ''		# ���إѥ�᡼����󥯥ǡ�������ΰ�
	w_hirlnk2 = []

	w_def1 = ''			# def�ǡ����ΰ���ΰ�

	w_hirline = []		# ���Ԥ������ʬ�������ΰ���ΰ�

	w_hirout = []		# ���Ϥ��볬�إǡ���

	w_str1 = ''
	w_str2 = ''



	# �ȼ�ɽ�������Τ�Τ�XML�ѥ������ؤ��Ѵ�
	w_hirlnk1 = in_hirlnk
	w_hirlnk1.gsub! /~/, '/'
	w_hirlnk1.gsub! /!/, '@'
	w_hirlnk1.gsub! /\(/, '['
	w_hirlnk1.gsub! /\)/, ']'

	# def�ǡ�����equal�����Ÿ������
	w_def1 = defdata_expand_equal(in_def)


	# ����ˤ���
	w_hirlnk2 = w_hirlnk1.split("\n")


	# LineTarget��ʸ�δ�¸�������鿷�����ؤ��Ѵ�
	w_hirlnk2.each_index do |i|
		w_str1 = ''
		w_str2 = ''
		w_hirline = []
		w_str1 = w_hirlnk2[i]
		if w_str1 =~ /^(\s*?)\$LineTargetStart/i
			# $LineTargetStart����Ǥ���
			w_str2 = $1
			w_hirline = w_str1.split(',')		# ��,�פǶ��ڤäơ�����ˤ���
			if w_hirline.size != 7
				puts '$LineTargetStartʸ�ΰ����ο����ۤʤ�ޤ�'
				puts w_str1
			end
			# ����ζ������
			w_hirline.each_index do |j|
				w_hirline[j].strip!
			end
			# ���ϰ��֤�0���ꥸ�󤫤�1���ꥸ��ˤ���
			w_hirline[6] = String(Integer(w_hirline[6]) + 1)

			# ��¸��������Ƭ�ˡ�~�פ��ʤ������������ˤ���Ƭ�ˡ�/�פ�����Τǡ��ɲä���
			# ����оݳ��ؤ��ִ�(w_hirline[4])
			if w_hirline[4] =~ /^(\s*?)([^\/\s][\s\S]*?)$/
				# ��Ƭ���Ѵ����XML�ѥ���/�פ�¸�ߤ��ʤ��Τǡ��ɲ�
				w_hirline[4] = $1 + '/' + $2
			end
			# �ִ��оݳ��ؤ��ִ�(w_hirline[2])
			if w_hirline[2] =~ /^(\s*?)([^\/\s][\s\S]*?)$/
				# ��Ƭ���Ѵ����XML�ѥ���/�פ�¸�ߤ��ʤ��Τǡ��ɲ�
				w_hirline[2] = $1 + '/' + $2
			end

			# �¤��ؤ���Ԥ�
			w_str2 = w_str2 + w_hirline[0] + ', ' + w_hirline[4] + ' = ' + w_hirline[5] + ', ' + w_hirline[6] + ', ' + w_hirline[2]
			# ����˥��å�
			w_hirlnk2[i] = w_str2
		elsif w_str1 =~ /^(\s*?\$LineTargetEnd)/i
			# $LineTargetEnd����Ǥ���
			w_hirlnk2[i] = $1
		elsif w_str1 =~ /^\s*?\$/
			# $����Ǥ���
		elsif w_str1 =~ /^\s*?\#/
			# ��#�ץ����ȤǤ���
		elsif w_str1 =~ /^\s*?$/
			# ����ԤǤ���
		else
			# �ѥ�᡼���Ǥ���

			# ��¸��������Ƭ�ˡ�~�פ��ʤ������������ˤ���Ƭ�ˡ�/�פ�����Τǡ��ɲä���
			if w_str1 =~ /^(\s*?)([^\/\s][\s\S]*?)$/
				# ��Ƭ���Ѵ����XML�ѥ���/�פ�¸�ߤ��ʤ��Τǡ��ɲ�
				w_hirlnk2[i] = $1 + '/' + $2
			end


		end

	end


# $LineTargetStart,  LT_1,  ~Mml(1)~Body(2)~MemoB(x), =,  ~Mml(1)~Body(2)~MemoB(x)~code(1)!atr, 'char', 0
#   ��
# $LineTargetStart, /Mml[1]/Body[2]/MemoB[x]/code[1]@atr='char', 1, /Mml[1]/Body[2]/MemoB[x]



	# def�ե�����ȳ��إѥ�᡼�����������
	w_hirlnk2.each_index do |i|
		w_str1 = ''
		w_str2 = ''
#		w_hirline = []
		w_str1 = w_hirlnk2[i]
		if w_str1 =~ /^(\s*?)\$LineTargetStart/i
			# $LineTargetStart�Ǥ���
#			w_str2 = $1
#			w_hirline = w_str1.split(',')		# ��,�פǶ��ڤäơ�����ˤ���

		elsif w_str1 =~ /^(\s*?\$LineTargetEnd)/i
			# $LineTargetEnd�Ǥ���
		elsif w_str1 =~ /^\s*?\$/
			# $����Ǥ���
		elsif w_str1 =~ /^\s*?\#/
			# ��#�ץ����ȤǤ���
		else
			# �ѥ�᡼���Ǥ���
			w_str1.scan(/^(\s*?\S+?\s*?\=\s*?)(\S+?)\s*?$/) do |x, y|
				w_param1 = get_param_from_defdata(w_def1, y)
				w_hirlnk2[i] = x + w_param1[1]
			end
		end


	end


	w_hirout = w_hirlnk2

	# ���̾������Ƭ���ɲä���
	w_hirout.unshift('#$ type="hierarchy" version="2.0" encoding="EUC-JP"' + "\n")



	return	w_hirout

end



# =============================================================================


case	conv_mode
when	OLDCONV_MODE_DEF
	# def�ե�����δ�¸����������Ѵ��Ǥ���

	in_def = ''
	out_def = ''

	# def�ե�������ɤ߹���
	open(argv_f[0], "r") do |fp1|
		in_def = fp1.read
	end


	# def�ե�����ǡ������Ѵ�
	out_def = def_convert_proc(in_def)


	# �ե�����ν񤭹���
	open(argv_f[1], "w") do |fp2|
		fp2.print out_def
	end


# ------------------------------------------------------------
when	OLDCONV_MODE_XML
	# XML�ƥ�ץ졼�ȥե�����δ�¸����������Ѵ��Ǥ���

	puts 'XML�ƥ�ץ졼�ȥե�����ϡ���¸�η��������Τޤ޻Ȥ��ޤ�'
	puts '��������$Repeat����ˤĤ��Ƥϡ����ȤǤν�����ɬ�פǤ�'


# ------------------------------------------------------------
when	OLDCONV_MODE_HIR
	# ���إǡ����ե�����δ�¸����������Ѵ��Ǥ���

	in_hirlnk = ''
	in_def = ''

	# def�ե�������ɤ߹���
	open(argv_f[0], "r") do |fp1|
		in_def = fp1.read
	end

	# ���إѥ�᡼����󥯥ե�������ɤ߹���
	open(argv_f[1], "r") do |fp2|
		in_hirlnk = fp2.read
	end


	# ���إѥ�᡼����󥯥ե������def�ե����뤫��γ��إǡ����ե�����ؤ��Ѵ�
	out_hir = hierarchy_convert_proc(in_def, in_hirlnk)


	# �ե�����ν񤭹���
	open(argv_f[2], "w") do |fp3|
		out_hir.each do |x|
			fp3.puts x
		end
	end




end
