--                                              --
-- ʿ������ǯ�ٲ����������ܥǡ����ɲ�           --
--                                              --
-- ͭ�����Ž����������������֤�������           --
-- ͭ�����Ž����������������֤�������           --
-- ͭ�����Ž����������������ɲá�               --
--                                              --
-- Create Date : 2010/03/02                     --
--                                              --
\set ON_ERROR_STOP

delete from tbl_nyuinkhn where khn_srycd = '099999950' and yukostymd = '20060401';
delete from tbl_nyuinkhn where khn_srycd = '099999951' and yukostymd = '20060401';
delete from tbl_nyuinkhn where khn_srycd = '099999952';

COPY "tbl_nyuinkhn" FROM stdin;
099999950	20060401	20100331	1	 	  	  	  	  	  	0	190097010	190097110	190119510	190119610	190119610	190119610	190119610			        	        	      
099999951	20060401	20100331	1	 	  	  	  	  	  	0	190097210	190097310	190119710	190119810	190119810	190119810	190119810			        	        	      
099999950	20100401	99999999	1	 	  	  	  	  	  	0	190097010	190119510	190119610	190119610	190119610	190119610	190119610			        	        	      
099999951	20100401	99999999	1	 	  	  	  	  	  	0	190097210	190119710	190119810	190119810	190119810	190119810	190119810			        	        	      
099999952	20100401	99999999	1	 	  	  	  	  	  	0	190135110	190135210	190135310	190135310	190135310	190135310	190135310			        	        	      
\.
