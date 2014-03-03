#!/usr/bin/ruby
require "jcode"

# Ⱦ�ѱѿ������� -> �����Ѵ�(EUC) script for ORCA
# 
#       �оݹ��ܤ�Ⱦ�Ѥ����ѤΥ������ʤ�¸�ߤ����硢���ߤ���Ⱦ�ѥ���
#       ���ʡ�Ⱦ�ѱѿ�������ӵ�������Ѥ��Ѵ�����
#
#     �ѥ�᡼���ե����� /home/orca/ORCADC.PARA [ $file_path_name ]
#              ( csv�ե�����ϸ��ܤ� �ե�����̾_org �Ȥ�����¸���ޤ� )
#     �оݹ����ֹ� 2 [ $tgt_csv_no ] ( ���ʻ�̾ )
# 02/01/03 by ��ƣ

$KCODE = "euc"

$file_path_name = ARGV[0]
$tgt_csv_no = [2]

def h2z(buf)
  # Ⱦ��->���� �Ѵ��ơ��֥� ============================================
  # (Ⱦ�ѥ��ڡ����ϥǥ�ߥ��Ȥ��ƻ��Ѥ���Τ���Ͽ�Ǥ��ޤ���)
  str1 = "" ; str2 = ""
  str1 = str1 + '���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� ���� �� �Î� �Ď� �ʎ� �ˎ� �̎� �͎� �Ύ� ���� '
  str2 = str2 + '�����������������������¥ťǥɥХӥ֥٥ܥ�'
  str1 = str1 + '�ʎ� �ˎ� �̎� �͎� �Ύ� �� �� �� �� �� �� �� �� �� �� �� �� '
  str2 = str2 + '�ѥԥץڥݥ�������������á�����'
  str1 = str1 + '�� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� '
  str2 = str2 + '�����������������������������������ĥƥȥʥ˥̥ͥ�'
  str1 = str1 + '�� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� �� '
  str2 = str2 + '�ϥҥեإۥޥߥ���������������'
  str1 = str1 + 'a b c d e f g h i j k l m n o p q r s t u v w x y z '
  str2 = str2 + '����������������������������������'
  str1 = str1 + 'A B C D E F G H I J K L M N O P Q R S T U V W X Y Z '
  str2 = str2 + '���£ãģţƣǣȣɣʣˣ̣ͣΣϣУѣңӣԣգ֣ףأ٣�'
  str1 = str1 + '0 1 2 3 4 5 6 7 8 9 '
  str2 = str2 + '��������������������'
  str1 = str1 + '\! " # \$ % & \' \( \) \* \+ , - \. / : ; < = > \? @ \[ \\\\ ] \^ _ ` \{ \| } ~ '
  str2 = str2 + '���ɡ������ǡʡˡ��ܡ��ݡ����������䡩���Ρ�ϡ����ơСáѡ�'

  frm = str1.split(" ")
  to  = str2.split("")

  i = 0
  frm.each{|s|
#   print s ; print ',' ; puts to[i]
    buf.gsub!(s,to[i])
    i = i + 1
  }
  return buf
end

# �ѥ�᡼���ե����뤫�����ϥե�����̾�򥻥åȤ���
def set_in_f(idct)
  # �ѥ�᡼���ե������¸�߳�ǧ
  if !File.exists?($file_path_name)
    print "ERR: no file [ " + $file_path_name + " ]\n"
    exit
  end
  open($file_path_name,"r") do |f|
    while buf = f.gets
      if /^#{idct}/ =~ buf.chop!
        buf.gsub!(idct,"")
        buf.gsub!(/(,T)$/,"")
        return buf
      end
    end
  end
end

#----- Main -------------------------------------------------
cnt = 0 ; lcnt = 0
# ���ϥե�����̾�Υ��å�
in_file = set_in_f('@01-1:')
# �ե������¸�߳�ǧ
if !File.exists?(in_file)
  print "ERR: no file [ " + in_file + " ]\n"
  exit
end

# �ե����븶�ܤ���̾����¸(copy)�����ϥե�����Ȥ���
out_file = in_file
in_file = in_file + "_org"
`cp #{out_file} #{in_file}`

# ���ϡ����ϥե�����򤽤줾�쥪���ץ�
open(in_file,"r") do |f|
  open(out_file,"w") do |f2|

    # ���ϥե����뤫��1���ɤ߹���
    while buf = f.gets
      lcnt = lcnt + 1
      ary = Array.new
      # �����β��ԥ����ɤ���
      buf.chop!
      # �ǥ�ߥ��򥫥�ޤȤ�����˥��åȡʣ����ꥸ���
#     print "[IN-ARY] => " + buf + "\n"
      ary = buf.split(",",-1)

      # �оݹ��ܤˤĤ��ƤΤ߽���
      flg = false
      $tgt_csv_no.each{|s|
        # Ⱦ�Ѥ����ѤΥ������ʤ�¸�ߤ���Х᥽�å� h2z(String) ���Ѵ�
        if /[��-�ߥ�-��������]/e =~ ary[s-1]
          ary[s-1] = h2z(String(ary[s-1])) ; flg = true
          ary[s-1] = ary[s-1].gsub(/��/,'"')
        end
      }
      cnt = cnt + 1 if flg

      # ����򥫥�ޥǥ�ߥ���ʸ����˷��
      buf = ary.join(",")
#     print "[OUT-ARY]=> " + buf + "\n"
#     puts "-----------------------------------------------------------------------"
      # ʸ�������ϥե�����˽񤭹���
      f2.puts buf
    end
  end
end
printf("**(orcvt_euc_kananame.rb)**  convert CSV file [ %s (%d lines) ]\n",out_file,lcnt)
#----- Script end -------------------------------------------