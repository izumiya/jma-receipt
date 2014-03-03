#!/usr/bin/env ruby

# ======================================================================
# �������󥷥��ե����� => XML�ե����� ����������ץ�

# �С���������
#
# ��ver001 2001/8/3
# ���ƥ�ץ졼��XML�ե�������Ф��ơ��ƥ�ץ졼��XML�ե������
#   �����ѥ�᡼������ȥ쥤�����ȥե�����������Ѥ���
#   �������󥷥��ե�����򸵤��ִ���������κ���
#
# ��ver002 2001/8/?
# ���ƥ�ץ졼��XML�ե�����������ѥ�᡼���������Ѥ���������ʤ�����
#   �ִ�����ϥ쥤����������ե�����ǰ���������褦�˽���


# Thanks!!
#
# ������ɽ���ϡ�coco��ν����ˤ�äƼ¸��Ǥ��ޤ���

# ======================================================================


# ���̥饤�֥��μ�����
require 'orca_lib.rb'



if __FILE__ == $0
	# ���Υ�����ץȤ�ñ�Ȥǵ�ư����

	# �ե�����̾�Υ��å�
#	lay_fname = 'orca_layout_front.def'	# �쥤����������ե�����̾
#	seq_fname = 'claim0000000000.txt'	# �������󥷥��ǡ����Υե�����̾
#	in_fname = 'base_yoyaku.xml'	# �ƥ�ץ졼��XML�ե�����̾
#	out_fname = 'kekka.xml'	# ����XML�ե�����̾
	lay_fname = ARGV.shift	# �쥤����������ե�����̾
	seq_fname = ARGV.shift	# �������󥷥��ǡ����Υե�����̾
	in_fname = ARGV.shift	# �ƥ�ץ졼��XML�ե�����̾
#	out_fname = ARGV.shift	# ����XML�ե�����̾

#	$stderr.puts lay_fname
#	$stderr.puts seq_fname




	# �裱��������--help�פ���/?�פξ�硢�إ��ɽ��
	if lay_fname==nil or lay_fname=='--help' or lay_fname=='/?'
		puts	'�������󥷥��ե����� => XML�ե����������ߥ�����ץ�'
		puts	''
		puts	'make_xml.rb [file1] [file2] [file3] >[file4]'
		puts	'   [file1] <= �쥤����������ե�����'
		puts	'   [file2] <= �������󥷥��ե�����̾'
		puts	'   [file3] <= �ƥ�ץ졼��XML�ե�����̾'
		puts	'   [file4] <= ����XML�ե�����̾'
		puts	''
		exit
	end

end



# ======================================================================
# �ƤӽФ��ؿ���


# XML���Ѵ��ؿ�
#    ����
# template   - �ƥ�ץ졼��XML�ǡ��� [IN / String��]
# seq        - �Ѵ����������󥷥��ǡ��� [IN / String��]
# layout     - �쥤����������ǡ��� [IN / String��]
# debug_mode - �ǥХå��ѥ��顼��å���������(0 = ���Ϥ��ʤ�, 1 = ���Ϥ���) [IN / String��]
#    �����
# ����(2���ܸ���)
# [0] = ���ơ�����������(0 = ���ｪλ)
# [1] = �Ѵ����XML�ǡ���
def conversion_seq_to_xml(template, seq, layout, debug_mode)
	# ���������ƤΥ���������äơ����Ƥ��ѹ����Ƥ⤤���褦�ˤ���
	xml = template.clone
	seq_data = seq.clone
	rec_layout = layout.clone

	# **********
	deb_counter = 0		# �ǥХå��ѥ������ΰ�
	# **********

	# �������󥷥��ǡ����β��Ԥ���(ʣ���쥳���ɤˤʤä����ϡ����̤˲��Ժ��)
	begin
	  seq_data.gsub! "\n" , ''
	end  while  /#{"\n"}/ =~ seq_data

	# ----------
	$stderr.puts	'equal�ޥ�����Ÿ����Ǥ�'		if debug_mode!=0
	# �쥤����������ǡ����Τ�����equal�ޥ��������Ÿ��
	macro_execute_equal(rec_layout)
	$stderr.puts	'equal�ޥ�����Ÿ������λ���ޤ���'		if debug_mode!=0
	# ----------
	# �쥤�������������Υ����Ȥȶ���Ԥȹ�Ƭ�����������ܴ֤ζ������
	record_del_comment_space(rec_layout)
	# ----------
	$stderr.puts	'ifdef�ޥ����β�����Ǥ�'		if debug_mode!=0
	# IFDEF�ޥ����μ¹�
	macro_execute_ifdefl(rec_layout, seq_data)
	$stderr.puts	'ifdef�ޥ����β��Ϥ���λ���ޤ���'		if debug_mode!=0
	# ----------


	# ----------
	$stderr.puts	'�ᥤ�����(1)�μ¹���Ǥ�'		if debug_mode!=0
	# ----------

	# �쥤����������ǡ������ñ�̤�����ˤ���
	rec_layout2 = get_record_layout_linediv(rec_layout)
	rec_layout = nil
	# ----------
	$stderr.puts	'�ᥤ�����(2)�μ¹���Ǥ�'		if debug_mode!=0
	# ----------
#	rec_layout2.each do |e|
#		$stderr.puts '['+e+']'
#	end
	# �Կ�ʬ�ִ�������Ԥ�
	rec_layout2.each do |e|
		# �������󥷥��ǡ����ȥ쥤����������ǡ������顢
		# �ѥ�᡼��̾���ִ����Ƥμ���
		cdata = get_record_data(e, seq_data)

		if cdata!=nil
#			while  /#{cdata[0]}/ =~ xml
			if cdata[1] != ''
				xml.gsub! /#{cdata[0]}/, cdata[1]
				# **********
				deb_counter += 1	# �����󥿤Υ�����ȥ��å�
				# **********
			end
#			end
			cdata=nil
		end
		e = nil
	end
	# **********
	$stderr.puts 'gsub��� = [' + String(deb_counter) + ']'
	# **********


	# ----------
	$stderr.puts	'�ᥤ�����(3)�μ¹���Ǥ�'		if debug_mode!=0
	# ----------

	# �����Ȥκ������
	while  /^\s*<!--[\s\S]*?-->\s*\n/ =~ xml
		xml.gsub! /^\s*<!--[\s\S]*?-->\s*\n/ , ''
	end


	# ����Ԥκ������
	while  /^\s*?\n/ =~ xml
		xml.gsub! /^\s*?\n/ , ''
	end

	while  /\n\s*?\n/ =~ xml
		xml.gsub! /\n\s*?\n/ , "\n"
	end


	# ���֤�Ⱦ�Ѷ���4ʸ�����Ѵ�
	while  /#{"\t"}/ =~ xml
		xml.gsub! /#{"\t"}/ , '    '
	end

	# del %______%

	xml.gsub! /%(.)+?%/ , ''

	# ����Υ�����Ȥκ������
	while  /^\s*<([\w:]+)[^>]*>\s*<\/\1>\s*\n/ =~ xml
		xml.gsub! /^\s*<([\w:]+)[^>]*>\s*<\/\1>\s*\n/ , ''
	end


	# ������ȺǸ��;ʬ�ʶ���κ������
	# ----------
	$stderr.puts	'�ᥤ���������λ���ޤ���'		if debug_mode!=0
	# ----------


	return [0, xml]
end


# ======================================================================
# main��


if __FILE__ == $0
	# ���Υ�����ץȤ�ñ�Ȥǵ�ư����

#	xml = ARGF.read
#	if xml==nil
#		$stderr.puts 'ɸ�����Ϥ����ɤ߹���ޤ���Ǥ���'
#		exit 2
#	end

	xml=''
	open(in_fname, 'r') do |fp_i|
		xml = fp_i.read
	end
	if xml==nil or xml==''
		$stderr.puts '�ƥ�ץ졼��XML�ե����뤬�ɤ߹���ޤ���Ǥ���'
		exit 2
	end




	# �쥤�������������򸵤ˡ��ִ�����

	# �������󥷥��ǡ������ɤ߹���
	rec_layout = ''
	seq_data = ''
	open(seq_fname, 'r') do |fp_s|
		seq_data = fp_s.read
	end
	# �쥤�����ȥǡ������ɤ߹���
	open(lay_fname, 'r') do |fp_l|
		rec_layout = fp_l.read
	end



	xml_o = ''
	ret = []
	# XML�ؤ��Ѵ�����
	ret = conversion_seq_to_xml(xml, seq_data, rec_layout, 1)
#	ret = conversion_seq_to_xml(xml, seq_data, rec_layout, 0)

	ret_cd = ret[0]
	xml_o = ret[1]



#	open (out_fname, 'w') do |fp_o|
#		fp_o.print xml_o
#	end
	print xml_o


end
