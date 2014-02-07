#!/usr/bin/env ruby
# coding : euc-jp

# ʸ����β��Զ��ڤ������ˤ��ơ����������Υץ������Ϥ�����
# (2002/03/07 ���Ļ��ɲð���)

# ���ߤΰ���������ˡ
# ����1 = ����ե�����̾
# ����2 = ��ư�ץ����
# ����3 = dia�ǻȤ�XML����Ģɼ����ե�����
# ����4 = def�ե�����
# ����5 = data�ե�����
# ����6 = LP̾(��ά��)

# ��ʣ���Υץ����μ¹ԤϤǤ��ʤ�


# ���ۤϡ��ʲ��ǤϤ��뤬������
# ����1 = ����ե�����̾
# ����2 = ���ǡ��������ֹ�
# ����3 = Ʊ����ư�ץ����� (Ʊ���¹Ԥ��ǽ�ˤ���)
# ����4 = ��ư�ץ����
# ����5 = dia�ǻȤ�XML����Ģɼ����ե�����
# ����6 = def�ե�����
# ����7 = data�ե�����
# ����8 = LP̾(��ά��)
#  �嵭�ΰ�������ˤ���ȡ�ʣ���ץ����μ¹Ԥ��ǽ�ˤʤ�



# ============================================================

# �إ��ɽ��

help_flg = 0

case	ARGV[0]
when	nil
	help_flg = 1
when	'-?', '/?', '-h', '/h', '--help'
	help_flg = 1
else
	help_flg = 0
end

if (help_flg != 0)
	puts '���Υ�����ץȤϡ����ԤǶ��ڤäơ������Υץ������Ϥ��ץ����Ǥ�'
	puts $0 + ' <temp> <exec> <dia xml> <def> <data> <lp>'
	puts '<temp>    = ����ե�����̾'
	puts '<exec>    = ��ư�ץ����'
	puts '<dia xml> = dia�ǻȤ�XML����Ģɼ����ե�����'
	puts '<def>     = def�ե�����'
	puts '<data>    = data�ե�����'
	puts '<lp>      = ����LP̾(��ά��ǽ)'
	exit 0
end
# ����1 = ����ե�����̾
# ����2 = ��ư�ץ����
# ����3 = dia�ǻȤ�XML����Ģɼ����ե�����
# ����4 = def�ե�����
# ����5 = data�ե�����
# ����6 = LP̾(��ά��)



# ============================================================
# �ΰ�ν����

# �ե�����̾�ΰ�
temp_file = ''; exec_file = ''; dia_file = ''; def_file = ''; data_file = ''; lp_name = ''
temp_file2 = ''; exec_file2 = ''; dia_file2 = ''; def_file2 = ''; data_file2 = ''; lp_name2 = ''
# ʸ�����ΰ�
word1 = ''; word2 = []
# exec��ʸ�����ΰ�
w_exec = ''
# fork���ΰ�
pid = []
# ���������ΰ�
li_cnt1 = 0


# ============================================================
# �����Υ��å�

# �����ο������å�
for li_cnt1 in 1..4 do
	if ARGV[li_cnt1] == nil
		puts '�����ο���­��ޤ���'
		exit 1
	end
end


# �������ΰ褫�顢ʸ����μ���
temp_file = String(ARGV[0])		# ����ե�����
exec_file = String(ARGV[1])		# ��ư�ץ����ե�����
dia_file = String(ARGV[2])		# dia�ǻȤ�XML����Ģɼ����ե�����
def_file = String(ARGV[3])		# def�ե�����̾
data_file = String(ARGV[4])		# data�ե�����̾
lp_name = String(ARGV[5])		# lp̾ (2002/03/07 ���Ļ��ɲð���)


# ============================================================
# �ؿ������


# �ե�����̾������Ρ�'�ס�"�פ����������
def filename_change(name)
	filename_change = name.gsub /\A[\'\"]([\s\S]*?)[\'\"][\s]*?\z/, '\1'
end


# �ե�����̾�˶��򤬤����硢����ˡ�"�פ򥻥åȤ������
def filename_unchange(name)
	if name =~ / /
		filename_unchange = "\"" + name + "\""
	else
		filename_unchange = name
	end
end



# ============================================================
# �����βù�����

# �ؿ���ƤӽФ��ơ�����Ρ�'�ס�"�פ���
temp_file = filename_change(temp_file)
exec_file = filename_change(exec_file)
dia_file = filename_change(dia_file)
def_file = filename_change(def_file)
data_file = filename_change(data_file)
lp_name = filename_change(lp_name)	# (2002/03/07 ���Ļ��ɲð���)


# �դˡ��֤˶��򤬤���ʸ����ˡ���"�פ��ɲä�����Τ��Ѱ�
temp_file2 = filename_unchange(temp_file)
exec_file2 = filename_unchange(exec_file)
dia_file2 = filename_unchange(dia_file)
def_file2 = filename_unchange(def_file)
data_file2 = filename_unchange(data_file)
lp_name2 = filename_unchange(lp_name)	# (2002/03/07 ���Ļ��ɲð���)


# �ǥХå��Ѥ�ɽ��
#puts "����ե�����   = [" + temp_file + "]"
#puts "��ư�ץ���� = [" + exec_file + "]"
#puts "dia�ѥե�����  = [" + dia_file + "]"
#puts "def�ե�����    = [" + def_file + "]"
#puts "data�ե�����   = [" + data_file + "]"
#puts "LP̾   = [" + lp_name + "]"	# (2002/03/07 ���Ļ��ɲð���)


# ============================================================

# ����ե�����¸�ߥ����å�
begin
	if nil != File.size(temp_file)
		puts '����ե����뤬¸�ߤ���ΤǺ�����ޤ�'
		puts '�ե�����̾ = [' + temp_file + ']'
		# ����ե�����κ��
		File.delete(temp_file)
		puts '������ޤ���'
#		puts '����ե����뤬¸�ߤ��뤿�ᡢ�񤭹��ߤǤ��ޤ���'
#		puts '�ե�����̾ = [' + temp_file + ']'
#		exit 2
	end
rescue
# �㳰��ȯ������С�����ե����뤬¸�ߤ��ʤ��Τǡ��ϣ�
end


# data�ե������¸�ߥ����å�
begin
	if nil == File.size(data_file)
		puts 'data�ե����뤬¸�ߤ��ޤ���'
		puts '�ե�����̾ = [' + data_file + ']'
		exit 3
	end
rescue
# �㳰��ȯ������С�data�ե����뤬¸�ߤ��ʤ�
	puts 'data�ե����뤬¸�ߤ��ޤ���'
	puts '�ե�����̾ = [' + data_file + ']'
	exit 3
end


# ============================================================


# data�ե�������ɤ߹���
# ���ϥե�������ɤ߹��߽���
open(data_file, "r") do |fp|
	word1 = fp.read
end


# -----------------------------------

# ���Զ��ڤ����
#word2 = word1.split(/\s*\n\s*/)
word2 = word1.split(/\n/)


# -----------------------------------


li_cnt1 = 0

# �¹Խ���
word2.each do |d2|
# �ǥХå��ѥ�����
#	puts '[' + d2 + ']'
	li_cnt1 = li_cnt1 + 1
	# ����ե�����ؤν񤭹���
	open(temp_file, "w") do |fp|
		fp.print d2
		fp.print ' ' * 5000		# 2002/03/07 ���Ļ��б�����
	end
# �ǥХå��Ѥ�ɽ��
	case	d2
	when	nil
		puts	'���ԤΤߤǤ�' + '[' + String(li_cnt1) + ']'
	when	''
		puts	'���ԤΤߤǤ�' + '[' + String(li_cnt1) + ']'
	else
		if d2 =~ /\A\s*\z/
			puts	'���򡦲��ԤǤ�' + '[' + String(li_cnt1) + ']'
#		else
#			puts	'OK [' + String(li_cnt1) + ']'
		end
	end
	w_exec = exec_file + ' ' + dia_file + ' ' + def_file + ' ' + temp_file
#	w_exec = exec_file + ' ' + dia_file2 + ' ' + def_file2 + ' ' + temp_file2
	# LP̾�����ꤵ��Ƥ����顢�������ɲ�(2002/03/07 ���Ļ��������)
	if lp_name != ''
		w_exec = w_exec + ' ' + lp_name
	end
# �ǥХå��Ѥ�ɽ��
#	puts w_exec
# **
	# �¹�����å���������(2002/03/07 ���Ļ��������)
	puts	'Print Start [' + String(li_cnt1) + ']'
	# �ץ����μ¹�
	pid = fork do
		exec w_exec
	end
	sleep 0.01	# ͽ�����̥��顼�β���Τ��ᡢ�Ԥ�(�����Ԥ�ʤ��ȡ�ruby ver1.4��ǸƤӽФ��줿������ץȤ˥��顼��ȯ������)
	# �¹Ԥ����ץ���ब�����ޤ��Ԥ�(�����Σ����ܤϡ�1.4�ǤΥ��顼����Τ���)
	Process.waitpid(pid, 0)
end



# ============================================================

# ����ե�������
begin
	if nil != File.size(temp_file)
		# ����ե����뤬¸�ߤ���
		# ����ե�����κ��
		File.delete(temp_file)
	end
rescue
# �㳰��ȯ������С�����ե����뤬¸�ߤ��ʤ�
end



# ============================================================
