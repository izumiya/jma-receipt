--                                      
-- �����ơ��֥�
--                                    
-- �󿯽�Ū���ư�֥�˥���󥰲û��ɲ�
-- �����¸Ʊ���ȿ��û��ɲ�
-- ����ɽ��ʬ�ֹ��ɲ�
--                                   
-- Create Date : 2016/03/01      

\set ON_ERROR_STOP

alter table tbl_tensu add column hisinsyumonitorksn smallint default 0;
alter table tbl_tensu add column touketuhozonksn smallint default 0;
alter table tbl_tensu add column kubunbangou character varying(30);
update tbl_tensu set kubunbangou = '';
