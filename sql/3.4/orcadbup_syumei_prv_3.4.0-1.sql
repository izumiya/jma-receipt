\set ON_ERROR_STOP
--
-- �ݸ�����(HKNTEN)�ɲ�
-- �ݸ������(HKNMONEY)�ɲ�
-- ���������(JIHIMONEY)�ɲ�
-- ���������(SKJMONEY)�ɲ�
-- ���������(LIFEMONEY)�ɲ�
-- Ĵ����(CHOSEI)�ɲ�
-- ���ȶ��(DISCOUNTMONEY)�ɲ�
-- ��������(JIHI_1��JIHI_10)�ɲ�
-- ��������(RMSAGAKU)�ɲ�
-- 
-- Create Date : 2006/12/19           --
--
--  �����ɲ�                        --
alter table TBL_SYUMEI_PRV
   add column HKNTEN		numeric(08);
alter table TBL_SYUMEI_PRV
   add column HKNMONEY		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHIMONEY		numeric(08);
alter table TBL_SYUMEI_PRV
   add column SKJMONEY		numeric(08);
alter table TBL_SYUMEI_PRV
   add column LIFEMONEY		numeric(08);
alter table TBL_SYUMEI_PRV
   add column CHOSEI		numeric(08);
alter table TBL_SYUMEI_PRV
   add column DISCOUNTMONEY	numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_1		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_2		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_3		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_4		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_5		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_6		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_7		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_8		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_9		numeric(08);
alter table TBL_SYUMEI_PRV
   add column JIHI_10		numeric(08);
alter table TBL_SYUMEI_PRV
   add column RMSAGAKU		numeric(08);


--  ��������                       --
alter table TBL_SYUMEI_PRV
   alter HKNTEN		set default 0;
alter table TBL_SYUMEI_PRV
   alter HKNMONEY	set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHIMONEY	set default 0;
alter table TBL_SYUMEI_PRV
   alter SKJMONEY	set default 0;
alter table TBL_SYUMEI_PRV
   alter LIFEMONEY	set default 0;
alter table TBL_SYUMEI_PRV
   alter CHOSEI		set default 0;
alter table TBL_SYUMEI_PRV
   alter DISCOUNTMONEY	set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_1		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_2		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_3		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_4		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_5		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_6		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_7		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_8		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_9		set default 0;
alter table TBL_SYUMEI_PRV
   alter JIHI_10	set default 0;
alter table TBL_SYUMEI_PRV
   alter RMSAGAKU	set default 0;
