#!/usr/bin/env ruby

# ======================================================================
# ���� ������ץ�

# 2001/9/19 - debian linux�ǡ�class�ץ᥽�åɤ�ư��ʤ��ä����ᡢ
#             ������ʬ����

# 2001/10/11 - ifdef�ν����ǡ����ĤΤߤλ��������Ƚ��򤷤Ƥ��ʤ��ä�
#              �����ϡ�����ǥå����λ�����ˡ�ˤ��ꡢ�����ܤ�����ꤹ�٤���Τ�
#              �����ܤ�����ꤷ�Ƥ�������ˡ��Զ�礬ȯ�����Ƥ���

# 2001/10/12 - equal�ޥ������®������(10�ܰʾ���ǧ)


# ======================================================================
# �ؿ������


# ���ܤ�����ˤ������ν������
#    ����
# word - ���������ʸ����
#    ����
# �������Ф��ƶ���ν��������Ԥ��Τǡ�
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
end


# =========================================================

# �����Ȥ����ιԡ���Ƭ�����������ܴ֤ζ���κ��
#    ����
# word - �������κ���򤹤�ʸ����
def record_del_comment_space(word)
	# �����Ȥκ��
	while  /#[\s\S]*?\n/ =~ word
		word.gsub! /#[\s\S]*?\n/ , "\n"
#		word.gsub! /#[\s\S]*?\n/ , ''
	end
	# ����Ԥκ������
	while  /^\s*?\n/ =~ word
		word.gsub! /^\s*?\n/ , ''
	end
	# ��Ƭ�����������ܴ֤ζ������
	item_trim word
end


# ======================================================================


# �쥳���ɥ쥤�����Ⱦ���Զ��ڤ����
#    ����
# layout_data - �Զ��ڤ�򤹤�쥤�����ȥǡ���
#    �����
# �쥳���ɥ쥤�����Ⱦ�����ڤ����������(nil�ξ�硢�����˼��Ԥ���)
#    ����
# ���ν�������ǡ������Ȥκ���䡢����Ԥκ�����򤷤ޤ�
def get_record_layout_linediv(layout_data)
	wd = layout_data
	# �����Ȥȶ���Ԥȹ�Ƭ�����������ܴ֤ζ������
	record_del_comment_space(wd)
#	# �����Ȥκ��
#	begin
#		wd.gsub! /#[\s\S]*?\n/ , "\n"
##		wd.gsub! /#[\s\S]*?\n/ , ''
#	end  while  /#[\s\S]*?\n/ =~ wd
#	# ����Ԥκ������
#	begin
#		wd.gsub! /^\s*?\n/ , ''
#	end  while  /^\s*?\n/ =~ wd
#	# ��Ƭ�����������ܴ֤ζ������
#	item_trim wd

#	$stderr.puts wd
#	wd
	ad = wd.split("\n")
#	ad.each do |e|
#		$stderr.puts "["+e+"]"
#	end
	ad
end


# =========================================================

# �ޥ���̿�����μ¹�
#    ����
# �ޥ���̿��ϡ��쥤����������ե����뼫�Ȥ����Ƥ��ѹ������ΤǤ�
# ��equal�פϡ�����򥻥åȤ����ΤǤ�
# ��typestart�פϡ���¤������򥻥åȤ����ΤǤ�


# ����equal�פ򡢤��٤Ƽ¹Ԥ��Ƥ��顢typestart��¹Ԥ��Ƥ�������
# ���ޥ�����¹Ԥ��ޤ����顢�ޥ����ιԤ������Ƥ�������
# ����baserepeat�ץޥ����ϡ����δؿ��Ȥ��̤δؿ��Ǽ¹Ԥ��Ƥ�������


# =====================================


# ��equal�ץޥ����������������μ���
#    ����
# layout - �쥤����������ǡ��� [IN/OUT]
#    �����
# equal�ޥ��������������(nil�ξ�硢����ޥ����ʤ�)
#   [�ѥ�᡼��],[equal],[��]
#    ����
# ���δؿ��ϡ�equal�ޥ�����������������������뤿��δؿ��Ǥ�
# ����Ū�ˡ�equal�ޥ�����Ȥ�ʤ��ʤä����䡢�ǥХå����ˡ�
# equal�ޥ�������������ͤ�������ơ��������˽��ϤǤ���褦���Ѱդ�����ΤǤ�
# ���顼��ȯ��������硢�㳰��ȯ�����ޤ�
# �ʤ����ø������η׻��ߥ��ˤĤ��Ƥϡ�������ץ�¦���㳰��ȯ�����ơ�
# ������λ���Ƥ��ޤ��ޤ��Τǡ��㳰����ª�ϤǤ��ޤ���
# ���ܤ�����ˤ������䥳���ȡ�����Ԥ������Ƥ��顢�¹Ԥ��Ƥ�������
def macro_get_equal_data(layout)
	ret = nil
	wd1 = ''
	wd1 = layout.clone	# �����ΰ�̣�Ǥ����Ƥ�ʣ��
	                  	# (clone�Ȥ��ʤ��ȡ��ݥ��󥿤Τߤ�ʣ��)
	# �����Ȥȶ���Ԥȹ�Ƭ�����������ܴ֤ζ������
	record_del_comment_space(wd1)
	wd1 = wd1+"\n"
	# �ޥ����ʳ��κ��
	begin
		wd1.gsub! /(?!^.*?,equal,)^.*?\n/ , ''
	end  while  /(?!^.*?,equal,)^.*?\n/ =~ wd1

# [ʸ����][,][equal][,][����][#ʸ����][\n]

	if wd1.to_s !=''
		# ����ޥ�������
		# ��ʣ�ѥ�᡼���θ���
		wd2 = wd1.split "\n"	# ��ñ�̤�����ˤ���
		wd2.each_with_index do |x, i|
			wd2.each_with_index do |y, j|
				if i!=j
					wx1=x.split ','
					wy1=y.split ','
					# �̤Υ쥳���ɤǤ���
					if wx1[0]==wy1[0]
						raise '�������ޥ���(equal)�ˤ����ơ���ʣ����ѥ�᡼��������ޤ�('+wx1[0]+")\n"
					end
				end
			end
		end
		# �������������åȤ���Ƥ������ؤ���θ
		flg1=1	# ��λȽ��ե饰
		while flg1==1
			# ���٤ƿ��������åȤ��줿�����ǧ
			flg2=1
			flg1=0
			wd2.each_with_index do |x, i|
				wx1=x.split ','
				wx1[2]=''		if wx1[2]==nil
				wx1[2].strip!	# �������
				# ��������ʸ���Υ����å�
#				if /[^0-9+*\/\-()]/ =~ wx1[2]
#				if /[^0-9+*\/\-()]|^.*?\".*?\"\s*?$/ =~ wx1[2]
#				if /#{'\"'}[^\"]*?#{'\"'}/ =~ wx1[2]
# 				if /\"[^\"]*?\"/ =~ wx1[2]
# $stderr.puts '�о�...'
# 				else
# $stderr.puts '�оݳ�!!!'
# 				end

				if /[^0-9+*\/\-()]/ =~ wx1[2] or /\"[^\"]*?\"/ !~ wx1[2]
# 				if /[^0-9+*\/\-()]/ =~ wx1[2] and /\"[^\"]*?\"/ !~ wx1[2]
					# �����ʳ��Ǥ���
					flg2=0	# �ޤ��Τ�Τ�����Τǡ��ե饰�Υ��ꥢ
					flg1=1
				else
					# ���٤ƿ��Ͳ����������ǡ��黻����(eval�����)
					# �黻���˥��顼�����ä���硢eval��ʸˡ���顼�Ȥ������ǡ�
					# �۾ｪλ����
					num1=(eval(wx1[2]))
#					num1=Integer(wx1[2])
					wx1[2]=String(num1)
					x=wx1.join ','
					wd2[i]=x
				end
			end
			# �����ʳ������äƤ���С��ִ�������Ԥ�
			if flg2==0
				wd2.each do |x|
					wx1=x.split ','
					wx2=wx1[0]
					wd2.each do |e|
						wy1=e.split ','
						wy2=wy1[2]
						while /#{wx2}/ =~ wy2
							e.gsub! wx2, wx1[2]
							wy1=e.split ','
							wy2=wy1[2]
							flg2=1
						end
					end
					wx1=nil
				end
			end

# 2001/08/13 ʸ�����������б����뤿�ᡢ���顼�����ˤ��ʤ�
			if flg2==0
				flg1=0
			end

#			# �Ѳ����ʤ���С����顼��������
#			if flg2==0
#				raise "�������ޥ���(equal)�ˤ����ơ����������򥻥åȤ��Ƥ����Τ������ʻ���򤷤Ƥ����Τ�����ޤ�\n"
#			end
#			flg1=0
		end
		ret=wd2
	else
		# ����ޥ����ʤ�
		ret=nil
	end
	ret
end


# =====================================


# ��equal�ץޥ���̿��μ¹�
#    ����
# layout - �쥤����������ǡ��� [IN/OUT]
#    �����
# �ʤ�
#    ����
# ���ܤ�����ˤ������䥳���ȡ�����Ԥ������Ƥ��顢�¹Ԥ��Ƥ�������
def macro_execute_equal(layout)
#	# �����Ȥȶ���Ԥȹ�Ƭ�����������ܴ֤ζ������
#	record_del_comment_space(layout)


	# **********
	$stderr.print '  equal�ޥ�������μ�����Ǥ�  ... '
	# **********
	wd2 = macro_get_equal_data(layout)
	# **********
	$stderr.puts 'complete'
	# **********
	if wd2!=nil
		# ����ޥ������������

		# **********
		$stderr.print '  ���פʹԤκ����Ǥ��������� ... '
		# **********

		# ����ޥ����ιԤκ��
#		wd2 = wd1.split "\n"	# ��ñ�̤�����ˤ���
#		reg1 = Regexp.new ""	# ��®���Τ��ᡢ����ɽ����������
#		wd2.each do |x|
#			wx1=x.split ','
#			reg1 = "^.*?#{wx1[0]}.*?,\s*?equal.*?\n"
#			while layout =~ reg1
#				layout.gsub! reg1, ''
#			end
# #			while  /^.*?#{wx1[0]}.*?,\s*?equal.*?\n/ =~ layout
# #				layout.gsub! /^.*?#{wx1[0]}.*?,\s*?equal.*?\n/, ''
# #			end
#			wx1=[]
#		end
		layout.gsub! /^.+?equal.+?\n/, ''

		# **********
		$stderr.puts 'complete'
		# **********




		# **********
		$stderr.print '  �쥤�����Ⱦ�����ִ���Ǥ��� ... '
		# **********
		# �쥤�����Ⱦ�����ִ�����
		wd2.each do |x|
			wx1=x.split ','
			wx1[2]=''		if wx1[2]==nil
# **********
#$stderr.print '[+'
# **********
			layout.gsub! wx1[0], wx1[2]
# 			layout.gsub! /(^.+?,.*?)#{wx1[0]}(.*?\n)/, '\1'+wx1[2]+'\2'
# **********
#$stderr.print '-] '
# **********
# 			while  /(^.+?,.*?)#{wx1[0]}(.*?\n)/ =~ layout
# # #				layout.gsub! /(^.+?,.*?)#{wx1[0]}(.*?\n)/, $1+wx1[2]+$2
# #				layout.gsub! /^.+?,.*?#{wx1[0]}.*?\n/, $1+wx1[2]+$2
# #				# ���Хå���ե���󥹤���Ѥ��Ƥ���
# 				layout.gsub! /(^.+?,.*?)#{wx1[0]}(.*?\n)/, '\1'+wx1[2]+'\2'
# 			end
			wx1=[]
		end
		# **********
		$stderr.puts 'complete'
		# **********
	else
		# ����ޥ����ʤ�
		$stderr.puts '����ޥ����ʤ�'
	end

	wd2=nil

	nil
end


# =========================================================


# �������ܤι������Ƽ����ؿ�(�ޥ�����������)
#    ����
# layout - �쥤����������ǡ��� [IN]
# rdata  - ���쥳���ɤΥ������󥷥��ǡ��� [IN]
# item_l - �����оݹ������� [IN]
#    �����
# �����Ρ�item_l�פΥ���ǥå�������¤�������Ƥ�����
#    ����
# �����Υ쥤����������ǡ�����layout�פϡ�����ζ������������Ƥ��뤳��
# ���դΥѥ�᡼������ꤷ����硢�����ΰ�϶��򰷤�����ޤ�
#
def get_layout_seqitem(layout, rdata, item_l)
	item_size = 0 ; idx1 = 0 ; ret = []
	param1 = '' ; param2a = ''
	wd1 = [] ; sp = 0 ; wbyte = 0 ; ins = '' ; wd2 = ''
	item_size = item_l.size	# ����������礭���μ���
	for idx1 in 0..(item_size-1) do
		# ���Ƥν����
		ret[idx1] = ''
		if layout =~ /^#{item_l[idx1]},(.+?),(.+?)$/
			# �쥤�����ȥǡ��������Ĥ��ä�
			param1 = $1
			param2a = $2
			param1.strip!
			param2a.strip!
			# �ѥ�᡼���ΰ褫�顢���Ƥμ���
			case	param1
			when	nil		# �ǡ��������äƤ��ʤ�
			when	'nowdate1'		# ���������դΥ��å�(�ѥ�����1)
			when	'nowdate2'		# ���������դΥ��å�(�ѥ�����2)
			when	'const'			# ����ǡ����Υ��å�
				if param2a!=nil
					param2a.gsub! /^\"/, ''
					param2a.gsub! /\"$/, ''
					ret[idx1] = param2a
				end
#			else			# ��ľ�˹��ܤΥ��å�
			when	'0'		# ��ľ�˹��ܤΥ��å�
#			when	'0', 'attribute'		# ��ľ�˹��ܤΥ��å�
				wd1 = param2a.split(',')	# ���ܤ������ʬ����
				sp = wd1[0]		# ���ϰ���
#				sp = Integer(wd1[0])	# ���ϰ���(StartPoint)
# 				if sp.class.name!='Fixnum'
#					$stderr.puts '���Ͱʳ��Ǥ�'+sp.class.name
					sp = eval(sp)
#					sp = sp.to_i
# 				end
				wbyte = Integer(wd1[1])		# ʸ���Х��ȿ�
				sp = Integer(sp)
				ins = wd1[2]		# ��������
				wd2 = rdata[sp, wbyte]	# ���ꤵ�줿ʸ�������
				wd2=''		if wd2==nil		# ��Ф���ʸ����nil�ʤ顢����ʸ���ˤ���
				# ����ζ���κ��
				item_trim wd2
				if sp==0 and wbyte==0
					# ���ϰ��֡��Х��ȿ��Ȥ⣰�ʤΤǡ���������򤽤Τޤޥ��å�
					ins.gsub! /^\"/, ''
					ins.gsub! /\"$/, ''
					ret[idx1] = ins
				else
					if ins==nil or ins==''
						# ��������ʤ��Ǥ���
						ret[idx1] = wd2
					else
#						$stderr.puts "["+ins+"]"
						ins.gsub! /^\"/, ''
						ins.gsub! /\"$/, ''
#						$stderr.puts "["+ins+"]"
						if nil == ins.index('%s')
							# %s���ޤޤ�Ƥ��ʤ�����������������äƤ���(���˥��å�)
							ret[idx1] = ins+wd2
						else
							# %s���ޤޤ�Ƥ���
							if wd2==''
								ret[idx1] = ''
#								ret[idx1] = sprintf(ins, ' ')
							else
								ret[idx1] = sprintf(ins, wd2)
							end
						end
					end
				end
			end
		end
	end
	ret
end


# =========================================================


# ifdef�ޥ���̿��μ¹�
#    ����
# layout - �쥤����������ǡ��� [IN/OUT]
# rdata  - ���쥳���ɤΥ������󥷥��ǡ��� [IN]
#    �����
# �ʤ�
#    ����
# �¹����ˡ������Ȥȶ���Ԥȹ�Ƭ�����������ܴ֤ζ�������ԤäƤ�������
# ���ߡ�������б��Ϥ��Ƥ��ޤ���
def macro_execute_ifdefl(layout, rdata)
	tag1 = ''
	wd1 = [] ; wd2 = [] ; wd3 = [] ; wd4 = [] ; wd5 = ''
	wsize1 = 0 ; idx1 = 0 ; wflg1 = 0
	rec_layout = layout
#	rec_layout = layout.clone
	# �����Ȥȶ���Ԥȹ�Ƭ�����������ܴ֤ζ������
#	record_del_comment_space(rec_layout)


	# ifdef�ޥ��������Ĥ��ä���硢����endif�ޤǤ�Ƚ�ꤹ��
	while /^(.+?),ifdef,.+?/ =~ rec_layout
		tag1 = $1
#		if /(^(.+?),ifdef,.+?$)([\s\S]*?)(^.+?,endif.*?$)/ =~ rec_layout
#		if /((^(.+?),ifdef,.+?$)([\s\S]*?)(^\3,endif.*?$))/ =~ rec_layout
		if /((^(.+?),ifdef,.+?\n)([\s\S]*?)(^\3,endif.*?$))/ =~ rec_layout
			# �оݤ����Ĥ��ä�
# 			$stderr.puts '�оݤ����Ĥ���ޤ��� $1=['+$1+'],  $2=['+$2+'],  $3=['+$3+'],  $4=['+$4+'],  $5=['+$5+']'
# 			exit
			wd1 = [$1, $2, $3, $4, $5]	# �嵭IFʸ�γ�̤�°�������Ƥμ���
			# wd1[0], $1 <= ���Τ�ʸ����
			# wd1[1], $2 <= ���ϹԤ�ʸ����(�Ǹ����β��Ԥ�ޤ�)
			# wd1[2], $3 <= ifdef�ޥ����μ��̻�
			# wd1[3], $4 <= �ޥ���������
			# wd1[4], $5 <= ��λ�Ԥ�ʸ����(��Ƭ�β��Ԥ�ޤޤʤ�)
			wd2 = $2.strip.split(',')
			wd3 = []
			wsize1 = wd2.size - 3	# Ƚ����ܿ��ϡ�ifdef�Ԥι��ܿ�-3
			idx1 = 0
			wflg1 = 0	# ͭ��Ƚ��ե饰(0 = ���Ƥ�̵���ˤ���, 1 = ���Ƥ�ͭ���ˤ���)
			case	wd2[2]
			when	'or'
				# �����ܰʹߤι��ܤΤɤ줫������ʳ��ʤ顢��Ȥ�ͭ���ˤ���
				# �դˡ������ܰʹߤι��ܤΤ��٤Ƥ�����ʤ顢��Ȥ�̵���ˤ���
				# �оݹ��ܤ����
				for idx1 in 0..(wsize1 - 1) do
					wd3.concat [String(wd2[3+idx1])]
				end
				# ���ܤ��ͤμ���
				wd4 = get_layout_seqitem(rec_layout, rdata, wd3)
				# orȽ�����
				wflg1 = 0	# ¸�ߤ��ʤ��Ȥ��ƽ����
				wd4.each do |e|
					if e!=nil and e!=''
						# ���Ƥ�¸�ߤ����Τ��ǤƤ���
						wflg1 = 1
					end
				end
			when	'and'
				# �����ܰʹߤι��ܤΤɤ줫������ʤ顢��Ȥ�̵���ˤ���
				# �դˡ������ܰʹߤι��ܤΤ��٤Ƥ�����ʳ��ʤ顢��Ȥ�ͭ���ˤ���
				# �оݹ��ܤ����
				for idx1 in 0..(wsize1 - 1) do
					wd3.concat [wd2[3+idx1]]
				end
				# ���ܤ��ͤμ���
				wd4 = get_layout_seqitem(rec_layout, rdata, wd3)
				# andȽ�����
				wflg1 = 1	# ¸�ߤ���Ȥ��ƽ����
				wd4.each do |e|
					if e==nil or e==''
						# ���Ƥ�¸�ߤ��ʤ���Τ��ǤƤ���
						wflg1 = 0
					end
				end
			else
				# �裳���ܤˡ�and�ס�or�װʳ��λ���򤷤Ƥ���
				$stderr.puts 'ifdef�Σ����ܤι��ܤˡ�and�ס�or�װʳ�����ꤷ�Ƥ��ޤ� (���̻�)> ['+tag1+']'
				exit 2
			end
# 			# �ִ���������ɽ���ǻپ�Τʤ�����ľ��
 			w_arg1 = [['\(', '\\('], ['\)', '\\)'], ['\+', '\\\+'], ['\*', '\\\*'], ['\/', '\\/'], ['\"', '\\"']]
 			w_arg1.each do |e|
 				wd1[0].gsub! /#{e[0]}/, e[1]
 			end
			# �ºݤι������Ƥ��ִ�����
			if wflg1 == 1
				# ���Ƥ�ͭ���ˤ���
#				rec_layout.gsub! wd1[0], wd1[3]
				rec_layout.gsub! /#{wd1[0]}/, wd1[3]
			else
				# ���Ƥ�̵���ˤ���
				wd5 = ''
				# �ΰ��ʣ������������ִ�������Ƥ��������
				wd5 = wd1[3].clone
				wd5.gsub! /^([^\s,]*?),.*?$/, '\1,const,'
				# �ºݤ��ִ�����
				rec_layout.gsub! /#{wd1[0]}/, wd5
# 				rec_layout.gsub! /#{wd1[0]}/, ''
				wd5 = ''
			end
		else
			# ��λ�Ԥ����Ĥ���ʤ��ä��Τǡ����顼�����ˤ���
			$stderr.puts 'ifdef���Ф��롢endifʸ�����Ĥ���ޤ��� (���̻�)> ['+tag1+']'
			exit 2
		end
	end



# # ++++++++++++++++++++
# dbg_lst1 = []
# dbg_lst1 = ['%M_RA1%', '%M_RX002%', '%D_C2%']
# dbg_mes1 = get_layout_seqitem(rec_layout, rdata, dbg_lst1)
# dbg_lst1.each_index do |dbg_idx1|
# 	$stderr.puts '['+dbg_lst1[dbg_idx1]+'] = ['+dbg_mes1[dbg_idx1]+']'
# end
# # ++++++++++++++++++++

	nil
end


# =========================================================

# �����ԤΥ쥤�����Ⱦ���μ���
#    ����
# line_ldata - �Զ��ڤ��쥤�����ȥǡ���
# rdata      - ���쥳���ɤΥ������󥷥��ǡ���
#    �����
# �쥳���ɥǡ�������(��������nil�ξ�硢��������)
# ret[0] : �ѥ�᡼��̾
# ret[1] : �ѥ�᡼���˥��åȤ�������
#    ����
# �������Ȱ��֤ˤĤ��Ƥϡ��黻�����ε��Ҥ򤷤���ΤˤĤ��Ƥ⡢
# �������Ԥ��뤳�Ȥ��ǧ
def get_record_data(line_ldata, rdata)
	wd1 = line_ldata.split(',')	# ���ܤ������ʬ����
	case	wd1[1]
	when	nil		# �ǡ��������äƤ��ʤ�
		ret = nil
	when	'nowdate1'		# ���������դΥ��å�(�ѥ�����1)
		ret = [wd1[0], Time.now.strftime("%Y-%m-%d")]
	when	'nowdate2'		# ���������դΥ��å�(�ѥ�����2)
		ret = [wd1[0], Time.now.strftime("%Y-%m-%dT%H:%M:%S")]
	when	'const'			# ����ǡ����Υ��å�
		if wd1[2]==nil
			ret = [wd1[0], '']
		else
			wd2 = wd1[2]
			wd2.gsub! /^\"/, ''
			wd2.gsub! /\"$/, ''
			ret = [wd1[0], wd2]
		end
#	else			# ��ľ�˹��ܤΥ��å�
	when	'0'		# ��ľ�˹��ܤΥ��å�
#	when	'0', 'attribute'		# ��ľ�˹��ܤΥ��å�
		# [�ѥ�᡼��], [�ǡ������μ��̻�], [���ϰ���], [�Х��ȿ�], [��������]
		# [��������]�ϡ���ά�����ꤹ�뤳�Ȥ�����
		# [�ǡ������μ��̻�]�ϡ����ߤ�ͽ����֤�����
		# Ruby¦�ǥǡ������Ѵ��򤫤���ɬ�פ��ǤƤ����Ȥ��ˡ�
		# �쥤�����ȥǡ������ѹ������פˤ��뤿��ˤ⤦����
		if wd1[0]==nil or wd1[2]==nil or wd1[3]==nil
			ret = nil
		else
			param = String(wd1[0])	# �ѥ�᡼��̾
			sp = wd1[2]	# ���ϰ���(StartPoint)
#			sp = Integer(wd1[2])	# ���ϰ���(StartPoint)
# 			if sp.class.name!='Fixnum'
#				$stderr.puts '���Ͱʳ��Ǥ�'+sp.class.name
				sp = eval(sp)
#				sp = sp.to_i
# 			end
			wbyte = Integer(wd1[3])	# ʸ���Х��ȿ�(WordBytevalue)
			ins = String(wd1[4])	# ��������
			atr = String(wd1[5])	# °��̾
			wd2 = rdata[sp, wbyte]	# ���ꤵ�줿ʸ�������
			wd2=''		if wd2==nil
			ins=''		if ins==nil
			atr=''		if atr==nil
			# ����ζ���κ��
			item_trim wd2
			if sp==0 and wbyte==0
				# ���ϰ��֡��Х��ȿ��Ȥ⣰�ʤΤǡ���������򤽤Τޤޥ��å�
				ins.gsub! /^\"/, ''
				ins.gsub! /\"$/, ''
				ret = [param, ins]
			else
				if ins==nil or ins==''
					# ��������ʤ��Ǥ���
					ret = [param, wd2]
				else
#					$stderr.puts "["+ins+"]"
					ins.gsub! /^\"/, ''
					ins.gsub! /\"$/, ''
#					$stderr.puts "["+ins+"]"
					if nil == ins.index('%s')
						# %s���ޤޤ�Ƥ��ʤ�����������������äƤ���(���˥��å�)
						ret = [param, ins+wd2]
					else
						# %s���ޤޤ�Ƥ���
						if wd2==''
							ret = [param, '']
#							ret = [param, sprintf(ins, ' ')]
						else
							ret = [param, sprintf(ins, wd2)]
						end
					end
				end
			end
			# °�����󤬤���С�°��̾���ɲ�
			if atr!=nil and atr!='' and ret[1]!=nil and ret[1]!=''
				ret[1]=' '+atr+'="'+(ret[1])+'"'
			end
		end
	end
	ret
end


# ======================================================================

if __FILE__ == $0
	# ���Υ�����ץȤ�ñ�Ȥǵ�ư����

	puts '���Υ�����ץȤϡ�ñ�Ȥ�ư����륹����ץȤǤϤ���ޤ���'
	puts '    ¾�Υ�����ץȤ���ƤӽФ�������ץȷ��Υե�����Ǥ�'

end
