--                                    --
-- ���׳Ф��񤭥ǡ�������             --
-- ����������ץץ����           --
--                                    --
-- Create Date : 2008/07/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBNYUALL' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBNYUALL	0000	000     	00000000	99999999									����ǯ��������������оݤȤ������ǯ���������Ϥ��ޤ���\n������ʬ���������������ƤΥǡ������оݤȤ��ޤ���\n������������������������Ϣ�ȡʣạ̃��ɣ����ˤΥǡ����Τ��оݤȤ��ޤ���\n�ꥹ�ȶ�ʬ�������������ʬ�����顼ʬ���Ƥ�����оݤȤ��ޤ�\n�����������������������顼ʬ�ʷٹ��ޤ�)�ΤߤΥǡ���������оݤȤ��ޤ���\n\n\n			20081203	20081203	160343	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
