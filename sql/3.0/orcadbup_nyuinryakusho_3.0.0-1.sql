\set ON_ERROR_STOP
delete from tbl_nyuinryakusho where srycd = '190121310' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121410' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121510' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121610' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121710' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121810' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122010' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122110' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122210' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122310' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122410' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122510' and yukostymd = '20060701' and yukoedymd = '99999999';
COPY "tbl_nyuinryakusho" FROM stdin;
190121310	20060401	99999999	�ţ�						
190121410	20060401	99999999	�ţ�						
190121510	20060401	99999999	�ţ�						
190121610	20060401	99999999	�ţ�						
190121710	20060401	99999999	�ţ�						
190121810	20060401	99999999	����						
190122010	20060401	99999999	���ţ�						
190122110	20060401	99999999	���ţ�						
190122210	20060401	99999999	���ţ�						
190122310	20060401	99999999	���ţ�						
190122410	20060401	99999999	���ţ�						
190122510	20060401	99999999	���ţ���						
\.
