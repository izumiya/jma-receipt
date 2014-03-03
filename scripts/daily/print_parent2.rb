#!/usr/bin/ruby

# ʸ����β��Զ��ڤ������ˤ��ơ����������Υץ��������Ϥ�����
# (2002/03/07 ���Ļ��ɲð���)
# ��������Τ�Τ򡢰���data�ե�����˴ޤ��褦���ѹ�(2002/04/16 :���Ļ���� :�����б�)
#
# red2ps�б��Ѥ�
# site-lib��ʬ�б� (2002/10/16)
#   woody�ǤǤϡ�site-lib��dia/red�ե�����⸫�ʤ���Фʤ�ʤ��Τǡ�����˴ؤ����б�
#   ���ǥ��쥯�ȥ���ɲä��뤳�Ȥ��б�
#   data�ե�����ե����ޥåȤ���(LP̾�Τ��Ȥˡ�site��ʬ���ɲ�)
# �ǥ��쥯�ȥ�����б� (2002/11/13)
#   ���ޥ�ɥ饤�����3��6�Υǥ��쥯�ȥ���־������Ѥ���褦�˽���
# ʣ���ץ������б� (2003/05/22)
#   ���ޥ�ɥ饤�����1�ǻ��ꤷ�Ƥ�������ե�����̾��������λ���ǤϤʤ���
#     ��������������褦�ˤ���
#   ����ե�����̾�˻��ꤹ��Τϡ�����7��data�ե�����ǡ�
#     �Ǹ�γ�ĥ��(.dat)��.tmp���Ѵ�����褦�ˤ���
#   �����ȼ��������1�ϥ��ߡ������Ȥ���
#   �����б��ǡ�ʣ���ץ������μ¹Ԥ��Ǥ���褦�ˤʤ�
# ��ĥ�����¸�б� (2003/05/24)
#   data�ե�����γ�ĥ�Ҥ�(.dat)�Ǥʤ��Ȥ����ʤ��ä�����
#   ��ĥ�Ҥ˰�¸���ʤ��褦�˽�������
# (2007/12/27 ) patch-lib �б�


# ����������ˡ
# ����1 = [���ߡ�]
# ����2 = ��ư�ץ������
# ����3 = ����ɸ��쥻�ץ�form�ǥ��쥯�ȥ�
# ����4 = ����ɸ��쥻�ץ�record�ǥ��쥯�ȥ�
# ����5 = ����ɸ��쥻�ץ�site��ͭform�ǥ��쥯�ȥ�
# ����6 = ����ɸ��쥻�ץ�site��ͭrecord�ǥ��쥯�ȥ�
# ����7 = data�ե�����

# data�ե�����ե����ޥå�
# +------------------+------------+---------------+---------------+---------------+--------------------------+
# |�ե�����̾(30Byte)|LP̾(20Byte)|offset-x(5byte)|offset-y(5byte)|site��ʬ(1Byte)|Ģɼ�ǡ���(Max20000�Х���)|
# +------------------+------------+---------------+---------------+---------------+--------------------------+
# �ե�����̾�ˡ���ĥ�Ҥȥǥ��쥯�ȥ�̾���ղä�����Τ�ҥץ������ΰ������Ϥ�
#    site��ʬ
#      1 = orcaɸ��
#      2 = site-lib�Υ������ޥ���Ģɼ
#
# ��LP̾�϶���ξ�硢�ҥץ������ΰ������ά����


require "tempfile"


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
	puts '���Υ�����ץȤϡ����ԤǶ��ڤäơ������Υץ��������Ϥ��ץ������Ǥ�'
	puts $0 + ' <dummy> <exec> <form> <record> <site-form> <site-record> <data>'
	puts '<dummy>       = ���ߡ�ʸ����'
	puts '<exec>        = ��ư�ץ������'
	puts '<form>        = ����ɸ��쥻�ץ�form�ǥ��쥯�ȥ�'
	puts '<record>      = ����ɸ��쥻�ץ�record�ǥ��쥯�ȥ�'
	puts '<site-form>   = ����ɸ��쥻�ץ�site��ͭform�ǥ��쥯�ȥ�'
	puts '<site-record> = ����ɸ��쥻�ץ�site��ͭrecord�ǥ��쥯�ȥ�'
	puts '<data>        = data�ե�����'
	exit 0
end


std_form = ''; std_record = ''; site_form = ''; site_record = ''; patch_form = ''

# ����λ���(�Ǹ�ˡ����ʤ餺��/�פ��դ������ˤ��Ƥ�������)
std_form = ARGV[2] + '/'   		# ���쥻ɸ��form�ǥ��쥯�ȥ�
std_record = ARGV[3] + '/' 		# ���쥻ɸ��record�ǥ��쥯�ȥ�
site_form = ARGV[4] + '/'  		# ���쥻site��ͭform�ǥ��쥯�ȥ�
site_record = ARGV[5] + '/'		# ���쥻site��ͭrecord�ǥ��쥯�ȥ�
patch_form = ARGV[7] + '/'  		# ���쥻patch��ͭform�ǥ��쥯�ȥ�


# .red����ps�ե�������Ѵ�����ݤ˻��Ѥ���ץ������
#RED_EXEC = 'red2ps'
RED2EMBED = 'red2embed'
MONPE = 'monpe'
# .red�ե�����ǡ�LP̾���ά���줿�ݤ˻��Ѥ���ץ��̾
DEFAULT_LP = 'lp1'



# ============================================================
# �ΰ�ν����

# �ե�����̾�ΰ�
temp_file = ''; exec_file = ''; dia_file = ''; def_file = ''; data_file = ''; lp_name = ''; offset_x = ''; offset_y = ''
temp_file2 = ''; exec_file2 = ''; dia_file2 = ''; def_file2 = ''; data_file2 = ''; lp_name2 = ''
site_flag = ''; base_dir = ''
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
for li_cnt1 in 1..6 do
	if ARGV[li_cnt1] == nil
		puts '�����ο���­��ޤ���'
		exit 1
	end
end


# �������ΰ褫�顢ʸ����μ���
#temp_file = String(ARGV[0])		# ����ե�����
exec_file = String(ARGV[1])		# ��ư�ץ������ե�����
data_file = String(ARGV[6])		# data�ե�����̾


# ����ե�����Υե�����̾������
temp_file = data_file + '.tmp'



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


# slash���դ���
def add_slash(str)
	if str =~ /[^\/]$/
		return (str += '/')
	else
		return str
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


# ���ޥ�ɰ�����form��record�Υǥ��쥯�ȥ�̾�ˡ�/�פ��ʤ���硢��/�פ��դ������
std_form = add_slash(std_form)
std_record = add_slash(std_record)
site_form = add_slash(site_form)
site_record = add_slash(site_record)


# �ǥХå��Ѥ�ɽ��
#puts "����ե�����   = [" + temp_file + "]"
#puts "��ư�ץ������ = [" + exec_file + "]"
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

	fred=Tempfile.new(['tmpred','.red'])
	fred.close
	fpdf=Tempfile.new(['tmppdf','.pdf'])
	fpdf.close


# �ǥХå��ѥ�����
#	puts '[' + d2 + ']'
	li_cnt1 = li_cnt1 + 1
	d2_len = d2.size	# ʸ�����Ĺ���μ���

	layeroption = d2.scan(/MonpeLayerIn(.*)MonpeLayerOut/).join.gsub(/ *-L */,",").sub(/,/,"-H ")

	# ��Ƭ30�Х��Ȥϡ�.dia��.def�ե�����Υե�����̾��ʬ
	# ���뤤�ϡ�.red�ե�����γ�ĥ�Ҥ�ޤ�ե�����̾
	dia_file = ''
	def_file = ''
	red_file = ''
	ls_w1 = ''	# �����ʸ�����ΰ�
	ls_w1 = d2[0, 30].strip	# �ե�����̾�����ζ��������Ƽ���
	site_flag = d2[60, 1].strip		# site��ͭȽ��ե饰�μ���
	if ls_w1 =~ /.red$/
		# .red�ե�����Ǥ���

		# Red�ե�����̾�Υ��å�
		case	site_flag
		when	'1'
			red_file = std_form + ls_w1
		when	'2'
			red_file = site_form + ls_w1
		else
			red_file = std_form + ls_w1
		end

		lp_name = d2[30, 20].strip	# LP̾�Υ��å�
		offset_x = d2[50, 5].strip				# offset-x�Υ��å�
		offset_y = d2[55, 5].strip				# offset-y�Υ��å�
		word3 = d2[61, (d2_len - 61)]	# ����ե�����ؽ��Ϥ������ƤΥ��å�

		# ����ե�����ؤν񤭹���
		open(temp_file, "w") do |fp|
			fp.print word3
			fp.print ' ' * 20000
		end

# take patch-lib ��form��¸�ߤ�����ϡ�path�����ؤ���
site_red_file = site_form + ls_w1
patch_red_file = patch_form + ls_w1
     puts 'Site  file ' + ls_w1 + ' ' + site_form
     puts 'Patch file ' + ls_w1 + ' ' + patch_form
if File.exist?(site_red_file)
     puts 'Site  Hit!!' + ls_w1 + ' ' + site_form
else
  if File.exist?(patch_red_file)
     red_file = patch_red_file
     puts 'Patch Hit!!' + ls_w1 + ' ' + patch_form
  else
     puts 'Normal Hit!!' + ls_w1
  end
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

		# offset-x�����ꤵ��Ƥ��ʤ��ä��顢0�ˤ���
		if offset_x == ''
			offset_x = '0'
		end
		# offset-y�����ꤵ��Ƥ��ʤ��ä��顢0�ˤ���
		if offset_y == ''
			offset_y = '0'
		end

#		w_exec = RED_EXEC + ' ' + red_file + ' ' + temp_file + ' -x ' + offset_x + ' -y ' + offset_y + ' -p ' + lp_name

		w_exec = RED2EMBED + ' '   + red_file + ' ' + temp_file + ' -o ' + fred.path  + ' ; ' \
               + MONPE + ' ' +  fred.path + ' -x ' + offset_x + ' -y ' + offset_y + ' -e ' + fpdf.path + ' ' + layeroption  + ' ; ' \
               + 'cat ' + fpdf.path + ' |  lpr -P ' + lp_name


# �ǥХå��Ѥ�ɽ��
#		puts w_exec
# **
		# �¹�����å���������
		puts	'Print Start [' + String(li_cnt1) + ']'
		# �ץ������μ¹�
		pid = fork do
			exec w_exec
		end
		sleep 0.01	# ͽ�����̥��顼�β���Τ��ᡢ�Ԥ�(�����Ԥ�ʤ��ȡ�ruby ver1.4��ǸƤӽФ��줿������ץȤ˥��顼��ȯ������)
		# �¹Ԥ����ץ�����ब�����ޤ��Ԥ�(�����Σ����ܤϡ�1.4�ǤΥ��顼����Τ���)
		Process.waitpid(pid, 0)


	else
#	monpe�ʳ��Υե�����ϥ��顼�Ȥ��롣
		exit 4
	end

	fred.unlink
	fpdf.unlink

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

