#!/usr/bin/env ruby


# XML�����⥸�塼��
require "orca_send_xml.rb"



# ����ץ�Υ��å�
send_file_name = "test.xml"
remort_host = "localhost"
remort_port = 8888


# ------------------------------------------------------------

# XML�ؤ��Ѵ�����




# ------------------------------------------------------------

# �����������Ѵ�����






# ------------------------------------------------------------


# XML����������
#   send_file_name - �����ե�����̾
#   remort_host    - �����ۥ���̾
#   remort_port    - �����ݡ����ֹ�
ret_cd = xml_send(send_file_name, remort_host, remort_port)

puts ret_cd		# ���顼�����ɤν���



# ------------------------------------------------------------

