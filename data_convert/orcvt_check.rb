#!/usr/bin/ruby
require "jcode"

# Ⱦ�ѱѿ������� -> �����Ѵ�(EUC) �Х��б�
#     orcvt_euc.rb �ΥХ��ˤ���Ѵ��ߥ��򤷤��ǡ�����ɽ�� 
#
#     �ѥ�᡼���ե����� /home/orca/ORCADC.PARA [ $file_path_name ]
#     �оݹ����ֹ� 16,17,18,19,20 [ $tgt_csv_no ] ( ���ϥ����ɣ����� )
# 01/11/05 by ����

$KCODE = "euc"

$file_path_name = "/home/orca/ORCADC.PARA"
$tgt_csv_no = [16,17,18,19,20]

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

#----- Main --------------------------------------------------
cnt = 0 ; lcnt = 0
# ���ϥե�����̾�Υ��å�
in_file = set_in_f('@01-5:')
# �ե������¸�߳�ǧ
if !File.exists?(in_file)
  print "ERR: no file [ " + in_file + " ]\n"
  exit
end

# ���ϥե����륪���ץ�
open(in_file,"r") do |f|
  # ���ϥե����뤫��1���ɤ߹���
  while buf = f.gets
    lcnt = lcnt + 1
    ary = Array.new
    # �����β��ԥ����ɤ���
    buf.chop!
    # �ǥ�ߥ��򥫥�ޤȤ�����˥��åȡʣ����ꥸ���
    ary = buf.split(",",-1)

    # �оݹ��ܤˤĤ��ƤΤߥ����å�
    flg = false
    $tgt_csv_no.each{|s|
      # '��','-'���ޤޤ�뤫?
      if /[��-]/e =~ ary[s-1]
        flg = true
      end
    }

    if flg
      cnt = cnt + 1
      printf("[line:%d] -------------- \n",lcnt)
      puts buf
    end
  end
end
printf("\n**(orcvt_chek.rb)**  checked CSV file [ %s (%d lines) ] miss data %d\n\n",in_file,lcnt,cnt)
#----- Script end -------------------------------------------
