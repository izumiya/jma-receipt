--                                    --
-- �����ƥ�����ǡ�������             --
-- ����������ץץ����           --
-- �ѥ�᥿���󹹿�SQL                --
--                                    --
-- Create Date : 2008/07/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0043' and kbncd = '1010' and styukymd = '00000000' and  edyukymd = '99999999';

COPY tbl_syskanri (kanricd, kbncd, styukymd, edyukymd, kanritbl, termid, opid, creymd, upymd, uphms, hospnum) FROM stdin;
0043	1010    	00000000	99999999	��Ǽ����                                                                                                                                                        ORCBD010                ������              YMD       ��λ������          YMD       ������ʬ            PSN1      ���׶�ʬ            PSN1      ���ٶ�ʬ            PSN1      ���ɽ������ʬ      PSN1                                                                                                                              121111    1			20070209	20080710	160049	1
\.

		update tbl_syskanri set kanritbl = encode(substr(decode(replace(a.kanritbl,'\\','\\\\'),'escape')||decode(repeat(' ',500),'escape'),1,494)||(substr(decode(replace(tbl_syskanri.kanritbl,'\\','\\\\'),'escape')||decode(repeat(' ',500),'escape'),495,1)),'escape')
		from tbl_syskanri a
		where a.kanricd in ('0043','0044')
		and  tbl_syskanri.kanricd in ('3001','3002')
		and  substr(decode(replace(a.kanritbl,'\\','\\\\'),'escape'),161,24)
		  =  substr(decode(replace(tbl_syskanri.kanritbl,'\\','\\\\'),'escape'),161,24);
		;

