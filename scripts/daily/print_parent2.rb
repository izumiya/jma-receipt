#!/usr/bin/env ruby

# ʸ����β��Զ��ڤ������ˤ��ơ����������Υץ������Ϥ�����
# (2002/03/07 ���Ļ��ɲð���)
# ��������Τ�Τ򡢰���data�ե�����˴ޤ��褦���ѹ�(2002/04/16 :���Ļ���� :�����б�)

# ��ʣ���Υץ����μ¹ԤϤǤ��ʤ�


# ����������ˡ
# ����1 = ����ե�����̾
# ����2 = ��ư�ץ����
# ����3 = data�ե�����

# data�ե�����ե����ޥå�
# +------------------+------------+--------------------------+
# |�ե�����̾(30Byte)|LP̾(20Byte)|Ģɼ�ǡ���(Max10000�Х���)|
# +------------------+------------+--------------------------+
# �ե�����̾�ˡ���ĥ�Ҥȥǥ��쥯�ȥ�̾���ղä�����Τ�ҥץ����ΰ������Ϥ�
#
# ��LP̾�϶���ξ�硢�ҥץ����ΰ������ά����

# ����3 = dia�ǻȤ�XML����Ģɼ����ե�����
# ����4 = def�ե�����
# ����6 = LP̾(��ά��)





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
	puts $0 + ' <temp> <exec> <data>'
	puts '<temp>    = ����ե�����̾'
	puts '<exec>    = ��ư�ץ����'
	puts '<data>    = data�ե�����'
	exit 0
end


# ����λ���(�Ǹ�ˡ����ʤ餺��/�פ��դ������ˤ��Ƥ�������)
DIA_DIR = '/usr/local/orca/form/'
DEF_DIR = '/usr/local/orca/record/'
# �ɲó��� (2002/09/17)
RED_DIR = '/usr/local/orca/form/'

# .red����ps�ե�������Ѵ�����ݤ˻��Ѥ���ץ����
RED_EXEC = 'red2ps'
# .red�ե�����ǡ�LP̾���ά���줿�ݤ˻��Ѥ���ץ��̾
DEFAULT_LP = 'lp1'
# �ɲý�λ (2002/09/17)



# ============================================================
# �ΰ�ν����

# �ե�����̾�ΰ�
temp_file = ''; exec_file = ''; dia_file = ''; def_file = ''; data_file = ''; lp_name = ''
temp_file2 = ''; exec_file2 = ''; dia_file2 = ''; def_file2 = ''; data_file2 = ''; lp_name2 = ''
# ʸ�����ΰ�
word1 = ''; word2 = []; word3 = ''
# exec��ʸ�����ΰ�
w_exec = ''
# fork���ΰ�
pid = []
# ���������ΰ�
li_cnt1 = 0
# ʸ����Ĺ���ΰ�
d2_len = 0


# ============================================================
# �����Υ��å�

# �����ο������å�
for li_cnt1 in 1..2 do
	if ARGV[li_cnt1] == nil
		puts '�����ο���­��ޤ���'
		exit 1
	end
end


# �������ΰ褫�顢ʸ����μ���
temp_file = String(ARGV[0])		# ����ե�����
exec_file = String(ARGV[1])		# ��ư�ץ����ե�����
data_file = String(ARGV[2])		# data�ե�����̾


# ============================================================
# �ؿ������


# �ե�����̾������Ρ�'�ס�"�פ����������
def filename_change(name)
	filename_change = name.gsub /\A[\'\"]([\s\S]*?)[\'\"][\s]*?\z/, '\1'
end


# �ե�����̾�˶��򤬤����硢����ˡ�"�פ򥻥åȤ������
def filename_unchange(name)
	if name =~ ' '
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
data_file = filename_change(data_file)


# �դˡ��֤˶��򤬤���ʸ����ˡ���"�פ��ɲä�����Τ��Ѱ�
temp_file2 = filename_unchange(temp_file)
exec_file2 = filename_unchange(exec_file)
data_file2 = filename_unchange(data_file)


# �ǥХå��Ѥ�ɽ��
#puts "����ե�����   = [" + temp_file + "]"
#puts "��ư�ץ���� = [" + exec_file + "]"
#puts "data�ե�����   = [" + data_file + "]"


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
	d2_len = d2.size	# ʸ�����Ĺ���μ���










	# �ե�����̾����н���







	# ��Ƭ30�Х��Ȥϡ�.dia��.def�ե�����Υե�����̾��ʬ
	# ���뤤�ϡ�.red�ե�����γ�ĥ�Ҥ�ޤ�ե�����̾ (2002/09/17�ɲû���)
	dia_file = ''
	def_file = ''
# �ɲó��� (2002/09/17)
	red_file = ''
# �ɲý�λ (2002/09/17)
	ls_w1 = ''	# �����ʸ�����ΰ�
	ls_w1 = d2[0, 30].strip	# �ե�����̾����ζ��������Ƽ���
# �ɲó��� (2002/09/17)
	if ls_w1 =~ /.red$/
		# .red�ե�����Ǥ���
		red_file = RED_DIR + ls_w1		# Red�ե�����̾�Υ��å�
		lp_name = d2[30, 20].strip	# LP̾�Υ��å�
		word3 = d2[50, (d2_len - 50)]	# ����ե�����ؽ��Ϥ������ƤΥ��å�

		# ����ե�����ؤν񤭹���
		open(temp_file, "w") do |fp|
			fp.print word3
			fp.print ' ' * 10000		# 2002/03/07 ���Ļ��б�����
		end

# �ǥХå��Ѥ�ɽ��
		case	word3
		when	nil
			puts	'���ԤΤߤǤ�' + '[' + String(li_cnt1) + ']'
		when	''
			puts	'���ԤΤߤǤ�' + '[' + String(li_cnt1) + ']'
		else
			if d2 =~ /\A\s*\z/
				puts	'���򡦲��ԤǤ�' + '[' + String(li_cnt1) + ']'
#			else
#				puts	'OK [' + String(li_cnt1) + ']'
			end
		end
		# LP̾�����ꤵ��Ƥ��ʤ��ä��顢LP̾��lp1�ˤ���
		if lp_name == ''
			lp_name = DEFAULT_LP
		end

		w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -p ' + lp_name

# �ǥХå��Ѥ�ɽ��
#		puts w_exec
# **
		# �¹�����å���������
		puts	'Print Start [' + String(li_cnt1) + ']'
		# �ץ����μ¹�
		pid = fork do
			exec w_exec
		end
		sleep 0.01	# ͽ�����̥��顼�β���Τ��ᡢ�Ԥ�(�����Ԥ�ʤ��ȡ�ruby ver1.4��ǸƤӽФ��줿������ץȤ˥��顼��ȯ������)
		# �¹Ԥ����ץ���ब�����ޤ��Ԥ�(�����Σ����ܤϡ�1.4�ǤΥ��顼����Τ���)
		Process.waitpid(pid, 0)


	else
		# .red�ե�����ʳ��Ǥ���
# �ɲý�λ(�б�����end�ޤǡ�����ǥ�Ȥ��ɲä��Ƥ���) (2002/09/17)
		dia_file = DIA_DIR + ls_w1 + '.dia'		# Dia�ե�����̾�Υ��å�
		def_file = DEF_DIR + ls_w1 + '.def'		# Def�ե�����̾�Υ��å�
		lp_name = d2[30, 20].strip	# LP̾�Υ��å�
		word3 = d2[50, (d2_len - 50)]	# ����ե�����ؽ��Ϥ������ƤΥ��å�








		# ����ե�����ؤν񤭹���
		open(temp_file, "w") do |fp|
			fp.print word3
			fp.print ' ' * 10000		# 2002/03/07 ���Ļ��б�����
		end
# �ǥХå��Ѥ�ɽ��
		case	word3
		when	nil
			puts	'���ԤΤߤǤ�' + '[' + String(li_cnt1) + ']'
		when	''
			puts	'���ԤΤߤǤ�' + '[' + String(li_cnt1) + ']'
		else
			if d2 =~ /\A\s*\z/
				puts	'���򡦲��ԤǤ�' + '[' + String(li_cnt1) + ']'
#			else
#				puts	'OK [' + String(li_cnt1) + ']'
			end
		end
		w_exec = exec_file + ' ' + dia_file + ' ' + def_file + ' ' + temp_file
#		w_exec = exec_file + ' ' + dia_file2 + ' ' + def_file2 + ' ' + temp_file2
		# LP̾�����ꤵ��Ƥ����顢�������ɲ�(2002/03/07 ���Ļ��������)
		if lp_name != ''
			w_exec = w_exec + ' ' + lp_name
		end
# �ǥХå��Ѥ�ɽ��
#		puts w_exec
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

# �ɲó��� (2002/09/17)
	end
# �ɲý�λ (2002/09/17)
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


