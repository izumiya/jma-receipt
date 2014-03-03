#!/usr/bin/ruby


# Claim�С������2�饤�֥��





# =============================================================================



# ���ܤ�����ˤ������ν������
#    ����
# word - ���������ʸ����
#    �����
# ���������ʸ����
#    ����
# �������Ф��Ƥ����ν��������Ԥ��Τǡ�
# ���Ƥ���¸���������ϡ����ΰ�˥ǡ��������򤷤Ƥ���¹Ԥ��Ƥ�������
def item_trim(word)
	# ��Ƭ�����������ܴ֤ζ������
	while  /^\s+?/ =~ word
		word.gsub! /^\s+?/ , ''
	end
	while  /\s+?\n/ =~ word
		word.gsub! /\s+?\n/ , "\n"
	end
	while  /\s+?$/ =~ word
		word.gsub! /\s+?$/ , ''
	end
	while  /,[^\S\n]+?/ =~ word
		word.gsub! /,[^\S\n]+?/ , ','
	end
	while  /\s+?,/ =~ word
		word.gsub! /\s+?,/ , ","
	end
	return word
end


# =========================================================

# �����Ȥ����ιԡ���Ƭ�����������ܴ֤ζ���κ��
#    ����
# word - �������κ���򤹤�ʸ����
#    �����
# ������������ʸ����
#    ����
# �������Ф��Ƥ����ν��������Ԥ��Τǡ�
# ���Ƥ���¸���������ϡ����ΰ�˥ǡ��������򤷤Ƥ���¹Ԥ��Ƥ�������
def record_del_comment_space(word)
	# �����Ȥκ��
	while  /#[^\n]+?\n/ =~ word
#	while  /#[\s\S]*?\n/ =~ word
		word.gsub! /#[^\n]+?\n/ , "\n"
#		word.gsub! /#[\s\S]*?\n/ , "\n"
#		word.gsub! /#[\s\S]*?\n/ , ''
	end
	# �����ȤΤߤκ�� (2003/12/5�ɲ�)
	while  /^\s*?#[^\n]*?\n/ =~ word
		word.gsub! /^\s*?#[^\n]*?\n/ , "\n"
	end
	# �ե������������Ȥκ��
	word.gsub! /#[^\n]+?$/, ''
	# ����Ԥκ������
	while  /^\s*?\n/ =~ word
		word.gsub! /^\s*?\n/ , ''
	end
	# ��Ƭ�����������ܴ֤ζ������
	item_trim word
	return word
end


# =============================================================================


# Repeat�������
#    ����
# in_data - Repeat�����¸�ߤ���Ȼפ���ʸ���� (String��/IN)
#    �����
# RepeatŸ������ʸ���� (String��/OUT)
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
	w_data4 = ''

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
	# ����join����ľ��ϡ��Ǹ�ιԤθ���˲��Ԥ��ʤ��Τ�����

	# �Ǹ夬���ԤǤʤ����ϡ������ǲ��Ԥ��ɲ�
	if w_data2[-1, 1] != "\n"
		w_data2 += "\n"
	end


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


		while w_data2 =~ /^(\s*?\$Repeat#{rname}\s*?,\s*?([0-9]+?)\s*?,\s*?(\S+?)\s*?,\s*?([0-9]+?)[\s\S]*?\n([\s\S]*?)^\s*?\$EndRepeat#{rname}[\s\S]*?\n)/i
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



# =========================================================


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



# =========================================================


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
		# ����޶��ڤ�˺��ؤΡ���θ��;�פ��⤷��ʤ������б��Ϥ����
		if w_data1[x] =~ /^\s*?(\$BaseStart)\s*?[^,\s]+?/i
			w_data1[x].gsub!(/^(\s*?\$BaseStart)(\s*?[^,\s]+?)/i, '\1' + ',' + '\2')
		end
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
	# �����Ǥϡ�ñ���$BaseStart,$BaseEnd�ιԤ����ˤ��Ƥ�������ʤΤǡ�
	# ���Ū�˶���Ԥ��Ǥ��Ƥ��ޤ���

	# ����Ԥκ������
	w_idx1 = w_data1.size
	while w_idx1 >= 0 do
		w_idx1 -= 1
		case	w_data1[w_idx1]
		when	nil, ''
			w_data1.delete_at(w_idx1)
		end
	end
	# ñ��ˡ�gsub��Ȥä���ˡ��������ä��Τǡ����������ˡ��Ȥ�


	w_data2 = w_data1.join("\n")
	w_data1 = []
	# ����join����ľ��ϡ��Ǹ�ιԤθ���˲��Ԥ��ʤ��Τ�����

	# �Ǹ夬���ԤǤʤ����ϡ������ǲ��Ԥ��ɲ�
	if w_data2[-1, 1] != "\n"
		w_data2 += "\n"
	end

	return w_data2

end



# =========================================================


# ���إǡ�����LineTarget̿��ؤμ��̻ҥ��åȽ���
#    ����
# in_data - LineTarget̿��μ��̻ҥ��åȽ����о�ʸ���� (String��/IN)
#    �����
# LineTarget̿��ؤμ��̻ҥ��åȽ������ʸ���� (String��/OUT)
#    ����
# �����������γ��إǡ����ե����륳��С��ȸ�η������Ѵ�����Τǡ�
#   ��¸�ξ��ˤ��֤����������դ��뤳��
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
	# ����join����ľ��ϡ��Ǹ�ιԤθ���˲��Ԥ��ʤ��Τ�����

	# �Ǹ夬���ԤǤʤ����ϡ������ǲ��Ԥ��ɲ�
	if out_data[-1, 1] != "\n"
		out_data += "\n"
	end

	return out_data

end



# =========================================================


# LineTarget�ι԰ʳ��γ��ؾ���˵������֤��ɲä������
#    ����
# in_data - �������֤��ɲä��볬�ؾ���
#    �����
# �������֤��ɲä������ؾ���(nil�ξ�硢�������֤��ɲäǤ��ʤ��ä�)
#    ����
# ��/***[*]/***[*]=99�פ��/***[*]/***[*]=99,99�פ��Ѵ�����
def hierarchy_add_startpoint_proc(in_data)
	ret_data = nil

	h_data1 = []
	w_idx1 = 0
	w_idx2 = 0
	w_str1 = ''	# �оݹԤ�����ζ���������ʸ����
	w_str2 = ''	# �嵭�ѿ�����Τ�������ʸ���ܤ�ʸ��
	w_str3 = ''	# XML�ѥ���ʬ��ʸ����
	w_str4 = ''	# �Х��ȿ���ʬ��ʸ����
	w_str5 = ''	# �Х��ȿ��ʹߤ�ʸ����
	w_num1 = 0


	h_data1 = in_data.split "\n"


	w_idx1 = 0		# �������֤Υ���ǥå���
	h_data1.each_index do |w_idx2|
		w_str1 = h_data1[w_idx2].strip
		if w_str1 != ''
			# ����ԤǤϤʤ�
			w_str2 = w_str1[0, 1]	# 1ʸ���ܤμ���
			if w_str2 != '#' and w_str2 != '$'
				# ����Ԥ䥳���ȹԤǤϤʤ�
				h_data1[w_idx2] =~ /^(\s*?\S+?\s*?\=\s*?)(\S[\S\s]*?\s*?)$/
				# $1 = XML�ѥ���ʬ
				# $2 = �Х��ȿ��ʹߤ���ʬ
				w_str3 = $1
				w_str4 = $2
				if w_str4 =~ /^([0-9]+?)(\s*?,[\S\s]*?$)/
					# �Х��ȿ��ʹߤˤ�ʸ���󤬤���
					# $1 = �Х��ȿ�����ʬ
					# $2 = �Х��ȿ����������ʬ
					w_str5 = $2
					w_str4 = $1
				else
					# �Х��ȿ��ʹߤˤ�ʸ���󤬤ʤ�
					w_str5 = ''
				end
				w_num1 = Integer(w_str4)	# �������Ѵ�
				h_data1[w_idx2] = w_str3 + w_str4 + ',' + String(w_idx1) + w_str5
				w_idx1 += w_num1	# �������֤Υ���ǥå����ˡ��Х��ȿ���û�
			end
		end
	end


	ret_data = ''
	ret_data = h_data1.join "\n"
	# �Ǹ夬���ԤǤʤ����ϡ������ǲ��Ԥ��ɲ�
	if ret_data[-1, 1] != "\n"
		ret_data += "\n"
	end


	return ret_data
end



# =========================================================


# ��Ƭǧ������
#    ����
# soc_line - ���Ϥ��룱���ܤξ���
#    �����
# [0] - ����
#        0 = ���������Ƥ����С������2������Τ��
#        1 = XML�ƥ�ץ졼�ȥե�����
#        2 = def�������ե�����
#        3 = ���ؾ��󥽡����ե�����
#        4 = def�¹Ի��ե�����
#        5 = ���ؼ¹Ի��ե�����
# [1] - �᥸�㡼�С������
#        0 = ���������Ƥ����С������1�Ǥ���
#        2 = �С������2�Ǥ���
# [2] - �ޥ��ʡ��С������(ͽ��)
# [3] - ʸ��������
#        0 = ̤�����̤�����ʸ�������ɤǤ���
#        1 = EUC-JP������
#        2 = Shift_JIS������ (ͽ��)
#        3 = UTF-8������ (ͽ��)
#        4 = JIS������ (ͽ��)
#    ����
# ���δؿ��ϡ��С������ߴ����θ���뤿��Τ�ΤǤ���
def realize_topline(soc_line)
	ret = []

# puts soc_line

	file_format = 0		# �ե�����η��� [0]
	major_version = 0	# �᥸�㡼�С������ [1]
	minor_version = 0	# �ޥ��ʡ��С������(ͽ��) [2]
	character_code = 0	# ʸ�������� [3]
	w_line1 = ''
	w_line2 = ''
	w_line3 = ''
	w_line4 = ''

	w_line1 = soc_line.clone	# ����ΰ�˥��ԡ�
	# UTF-8�Υإå��������(��ȥ륨��ǥ�����ˤ��б����ʤ�)
	if w_line1 =~ /^\xEF\xBB\xBF([\s\S]*?)$/
		w_line1 = $1
	end

	if w_line1 =~ /^\#\$/
		# ��Ƭ�˼��̾��󤬳�Ǽ����Ƥ���

		# =������ζ���������
		w_line1.gsub! /\s+?\=\s+?/, '='

		# ������˥��֥륯�����ơ�����󤬤ʤ���硢���֥륯�����ơ�������ä���
		w_line1.gsub! /(\=)([^\"\s]+?)(\s)/, '\1"\2"\3'	# �̾�Υ��֥륯�����ơ�����󥻥å��б�
		w_line1.gsub! /(\=)([^\"\s]+?)$/, '\1"\2"'		# �����Υ��֥륯�����ơ�����󥻥å��б�

		# type����μ���
		if w_line1 =~ /\s+?type\=\"([^\"]*?)\"/i
			w_line2 = $1.strip

			case	w_line2.downcase
			when	'figure-define'
				# def�������ե�����
				file_format = 2
			when	'hierarchy'
				# ���ؾ��󥽡����ե�����
				file_format = 3
			when	'figure-define-execute'
				# def�¹Ի��ե�����
				file_format = 4
			when	'hierarchy-execute'
				# ���ؼ¹Ի��ե�����
				file_format = 5
			else
				# ����������
				file_format = 0
			end
		end


		# version����μ���
		if w_line1 =~ /\s+?version\=\"([^\"]*?)\"/i
			w_line2 = $1.strip
			if w_line2 =~ /^(\S+?)\.(\S+?)/
				w_line3 = $1
				w_line4 = $2
			else
				w_line3 = w_line2
				w_line4 = '0'
			end

			# �С���������ͤ��Ѵ�����
			major_version = w_line3.to_i	# �᥸�㡼�С������ [1]
			minor_version = w_line4.to_i	# �ޥ��ʡ��С������(ͽ��) [2]
		end


		# encoding����μ���
		if w_line1 =~ /\s+?encoding\=\"([^\"]*?)\"/i
			w_line2 = $1.strip
			case	w_line2.downcase
			when	'euc-jp', 'euc_jp'
				# EUC-JP������
				character_code = 1
			when	'shift_jis', 'shift-jis', 'sjis'
				# Shift_JIS������
				character_code = 2
			when	'utf8', 'utf-8'
				# UTF-8������
				character_code = 3
			when	'jis'
				# JIS������
				character_code = 4
			end
		end


	elsif w_line1 =~ /^\s*?\<\?xml\s/
		# XML�ե�����ʤΤǡ�xml�ƥ�ץ졼�ȥե������Ƚ��

		# XML�ƥ�ץ졼�ȥե�����
		file_format = 1		# �ե�����η��� [0]
		major_version = 0	# �᥸�㡼�С������ [1]
		minor_version = 0	# �ޥ��ʡ��С������(ͽ��) [2]
		character_code = 0	# ʸ�������� [3]

		# =������ζ���������
		w_line1.gsub! /\s+?\=\s+?/, '='

		# ������˥��֥륯�����ơ�����󤬤ʤ���硢���֥륯�����ơ�������ä���
		w_line1.gsub! /(\=)([^\"\s]+?)(\s)/, '\1"\2"\3'	# �̾�Υ��֥륯�����ơ�����󥻥å��б�
		w_line1.gsub! /(\=)([^\"\s]+?)$/, '\1"\2"'		# �����Υ��֥륯�����ơ�����󥻥å��б�

		# encoding����μ���
		if w_line1 =~ /\s+?encoding\=\"([^\"]*?)\"/i
			w_line2 = $1.strip
			case	w_line2.downcase
			when	'euc-jp', 'euc_jp'
				# EUC-JP������
				character_code = 1
			when	'shift-jis', 'sjis', 'shift_jis'
				# Shift_JIS������
				character_code = 2
			when	'utf8', 'utf-8'
				# UTF-8������
				character_code = 3
			when	'jis'
				# JIS������
				character_code = 4
			end
		else
			# ʸ��������
			character_code = 3	# XML���ʤ�encoding̤����Υǥե���Ȥ�UTF-8
		end
	end


	w_line1 = nil
	w_line2 = nil
	w_line3 = nil
	w_line4 = nil

	ret.push(file_format)		# �ե�����η��� [0]
	ret.push(major_version)		# �᥸�㡼�С������ [1]
	ret.push(minor_version)		# �ޥ��ʡ��С������(ͽ��) [2]
	ret.push(character_code)	# ʸ�������� [3]

	return	ret
end



# =========================================================


# �ǥХå��ѹ�Ƭǧ��������̥�å�������������
#    ����
# output_mode - ���ϥ�å������⡼��(���ߡ�1�Τ�ͭ��)
# info_ary - realize_topline�ؿ��������
def debug_realize_topline_mes(output_mode, info_ary)

	output_mes = ''

	case	output_mode
	# ----------------------------------------
	when	1
		output_mes += '-' * 40 + "\n"
		output_mes += '�ե�����η���' + "\n"
		case	info_ary[0]
		when	0
			output_mes += '0 = ���������Ƥ����С������2������Τ��' + "\n"
		when	1
			output_mes += '1 = XML�ƥ�ץ졼�ȥե�����' + "\n"
		when	2
			output_mes += '2 = def�������ե�����' + "\n"
		when	3
			output_mes += '3 = ���ؾ��󥽡����ե�����' + "\n"
		when	4
			output_mes += '4 = def�¹Ի��ե�����' + "\n"
		when	5
			output_mes += '5 = ���ؼ¹Ի��ե�����' + "\n"
		else
			output_mes += String(info_ary[0]) + ' = ' + 'realize_topline�ؿ��Υե�����������ͤ������Ǥ�' + "\n"
		end

		output_mes += '-' * 40 + "\n"
		output_mes += '�᥸�㡼�С������ = ' + String(info_ary[1]) + "\n"
		output_mes += '�ޥ��ʡ��С������ = ' + String(info_ary[2]) + "\n"

		output_mes += '-' * 40 + "\n"
		output_mes += 'ʸ��������' + "\n"
		case	info_ary[3]
		when	0
			output_mes += '0 = ̤�����̤�����ʸ�������ɤǤ���' + "\n"
		when	1
			output_mes += '1 = EUC-JP������' + "\n"
		when	2
			output_mes += '2 = Shift_JIS������' + "\n"
		when	3
			output_mes += '3 = UTF-8������' + "\n"
		when	4
			output_mes += '4 = JIS������' + "\n"
		else
			output_mes += String(info_ary[0]) + ' = ' + 'realize_topline�ؿ���ʸ�������ɤ��ͤ������Ǥ�' + "\n"
		end
	# ----------------------------------------
	else
		raise '��realize_topline�פΰ�����output_mode�פ�̵�����ͤ����åȤ���ޤ�����'
	end


	return output_mes
end
# soc_line - ���Ϥ��룱���ܤξ���


# =============================================================================
# =============================================================================


if __FILE__ == $0
	# ľ�ܼ¹Ԥ����Ȥ��ν���

	# �ƥ��ȥ�å�����
	mes = '#$ type=figure-define version=2.0 encoding=EUC-JP'
#	mes = '#$ type=figure-define version=2.1.2 encoding=EUC-JP'
#	mes = '#$ type = hierarchy version = 2.0 encoding = EUC-JP'
#	mes = '#$ type="figure-define-execute" version="2.0" encoding="EUC-JP"'
#	mes = '#$ type = "hierarchy-execute" version = "2.0" encoding = "EUC-JP"'
#	mes = "\xEF\xBB\xBF" + '#$ type = "hierarchy" version = "2.0" encoding = "UTF-8"'
#	mes = '<?xml version="1.0" encoding="Shift_JIS" ?>'
#	mes = '<?xml version="1.0" encoding="EUC-JP" ?>'
#	mes = '<?xml version="1.0" encoding="JIS" ?>'
#	mes = "\xEF\xBB\xBF" + '<?xml version="1.0" encoding="UTF-8" ?>'
#	mes = "\xEF\xBB\xBF" + '<?xml version="1.0" ?>'


	w_ary1 = []

	# �ƥ��ȼ¹�
	w_ary1 = realize_topline(mes)

	# �ǥХå��ѤΥ�å�����ɽ��
	# puts debug_realize_topline_mes 1, w_ary1
	puts debug_realize_topline_mes(1, w_ary1)

end


# =============================================================================

