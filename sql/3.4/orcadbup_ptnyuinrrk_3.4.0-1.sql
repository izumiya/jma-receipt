\set ON_ERROR_STOP
--
-- ������������ơ��֥빽¤�ѹ�
--
-- �������������ֹ�(MAXEDANUM)�ɲ�
-- ����������ʬ(SKJKBN)�ɲ�
-- 
-- Create Date : 2005/02/17           --
--
--  �����ɲ�                        --
alter table TBL_PTNYUINRRK
   add column MAXEDANUM  numeric(03);
alter table TBL_PTNYUINRRK
   add column SKJKBN  character(1);


--  ��������                       --
alter table TBL_SYUNOU
   alter GRP_HAKHOUFLG set default 0;


--  ������ֹ楻�å�
update tbl_ptnyuinrrk set maxedanum = (select max(b.rrkedanum) from tbl_ptnyuinrrk b where tbl_ptnyuinrrk.hospid = b.hospid and tbl_ptnyuinrrk.ptid = b.ptid and tbl_ptnyuinrrk.rrknum = b.rrknum group by b.hospid,b.ptid,b.rrknum);

--  ������ʬ���å�
update tbl_ptnyuinrrk set skjkbn = '1' where (jtikbn <> '5') and (jtikbn <> '6');
