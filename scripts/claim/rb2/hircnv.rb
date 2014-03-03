#!/usr/bin/ruby


# ����С��Ƚ���


# =============================================================================
# =============================================================================
# ľ�ܼ¹Ԥ����Ȥ��ν���
if __FILE__ == $0


	help_flg = 0		# �إ��ɽ���ե饰 [0 = �إ�פ�ɽ�����ʤ�, 1 = �إ�פ�ɽ������]

	output_mode = 1		# ���ϥ⡼�� [1 = ����ץ�⡼��, 2 = ���������ղå⡼��]
	w_file_name = []		# �ե�����̾�ΰ��Ū�ʳ�Ǽ�ΰ�


	# �����Υ��åȽ���
	ARGV.each do |w_a1|
		case	w_a1.strip
		when	'--mode1'
			# ���Ϥϥ���ץ�⡼��
			output_mode = 1
		when	'--mode2'
			# ���Ϥϵ��������ղå⡼��
			output_mode = 2
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
	when	1
		# �ե�����ο������ʤ�
		puts '���ޥ�ɥ饤��ΰ����˥ե�����̾�����ꤵ��Ƥ��ޤ���'
		puts ''
		help_flg = 1
	else
		# �ե�����̾�λ��꤬���İʾ�ʤΤǡ����åȤ��줿���Ƥ��Ϥ�
		$soc_file = w_file_name[0]
		$dest_file = w_file_name[1]
	end

	if help_flg == 1
		puts 'hircnv.rb [option] [input file] [output file]'
		puts ''
		puts '  [input file]   ����С�������������ե�����'
		puts '  [output file]  ����С��ȸ峬������ե�����'
		puts ''
		puts '  [option]'
		puts '    --help �إ�פ�ɽ��'
		exit 0
	end


	# �ѿ���ǧ�Ѥ�ɽ������
	puts '����С�������������ե����� = [' + $soc_file + ']'
	puts '����С��ȸ峬������ե����� = [' + $dest_file + ']'



# =============================================================================
	# �̥⥸�塼������߽���


	require 'claim2_lib'

end



# =============================================================================
# =============================================================================
# �ؿ���



# ======================================================================


# =============================================================================
# =============================================================================
# �ᥤ�������


	# ���������
	hir_data1 = '' ; hir_data2 = '' ; hir_data3 = '' ; hir_data4 = '' ; hir_data5 = ''
	hir_head1 = ''
	sh_type = []


	# ��������ե�������ɤ߹���
	open($soc_file, 'r') do |fp_sh|
		hir_data1 = fp_sh.read
	end

	hir_head1 = hir_data1.sub /\A([\s\S]*?)\n[\s\S]*?\Z/, '\1'
#	puts hir_head1

	# ��������ե����������Ƚ��
	sh_type = realize_topline(hir_head1)

	# �ǥХå��ѤΥ�å�����ɽ��
#	puts '=' * 60
#	puts debug_realize_topline_mes 1, sh_type
#	puts '=' * 60

	# ʸ�������ɤΤߥ����å�
	case	sh_type[3]
	when	0
		puts 'ʸ�������ɤ�̤����Τ褦�Ǥ�����³�Ԥ��ޤ�'
	when	1
		# EUC-JP������
		puts 'EUC-JP�ʤΤǡ�³�Ԥ��ޤ�'
#	when	2
#		# Shift_JIS������
#		puts 'Shift_JIS�ʤΤǡ�³�Ԥ��ޤ�'
#	when	3
#		# UTF-8������
#	when	4
		# JIS������
	else
		raise '��������ե����뤬�б��Ǥ��ʤ�ʸ�������ɤʤΤǡ��۾ｪλ�����ޤ�'
	end

	# ��������ե�����Υ����ȥ����Ƚ���
	hir_data2 = record_del_comment_space hir_data1


	# Repeat�������
	hir_data3 = repeat_extend_proc(hir_data2)


	# ���إǡ�����base�������
	hir_data4 = basehierarchy_extend_proc(hir_data3)


	# ���إǡ�����LineTarget̿��ؤμ��̻ҥ��åȽ���
	hir_data5 = linetarget_setmark_proc(hir_data4)


	if output_mode == 2
		# ���������ղå⡼�ɤʤΤǡ���LineTarget�׹Ԥ����XML�ѥ�����θ���˵������֤��ղä���
		hir_data5 = hierarchy_add_startpoint_proc(hir_data5)
	end


	# ���ߡ����ե�JIS��ᤦ���ʤΤǡ��ʲ��Τ褦����Ƭ�˥إå����դ���
	hir_data5 = '#$ type=hierarchy-execute version=2.0 encoding=EUC-JP' + "\n" + hir_data5
#	hir_data5 = '#$ type=hierarchy-execute version=2.0 encoding=Shift_JIS' + "\n" + hir_data5


	# �ƥ���Ū�ˡ���������ե�����ν񤭹���
	open($dest_file, 'w') do |fp_dh|
		fp_dh.print hir_data5
	end


# =============================================================================
# =============================================================================

