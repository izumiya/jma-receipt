-- tbl_henrei_body --

\set ON_ERROR_STOP

alter table tbl_henrei_body add column disaster char(1);
alter table tbl_henrei_body add column s_disaster_1 char(1);
alter table tbl_henrei_body add column s_disaster_2 char(1);
alter table tbl_henrei_body add column s_disaster_3 char(1);
alter table tbl_henrei_body add column s_disaster_4 char(1);
alter table tbl_henrei_body add column s_disaster_5 char(1);
alter table tbl_henrei_body add column s_disaster_6 char(1);
alter table tbl_henrei_body add column s_disaster_7 char(1);
alter table tbl_henrei_body add column s_disaster_8 char(1);
alter table tbl_henrei_body add column s_disaster_9 char(1);
alter table tbl_henrei_body add column s_disaster_10 char(1);
update tbl_henrei_body set disaster='0',s_disaster_1='',s_disaster_2='',s_disaster_3='',s_disaster_4='',s_disaster_5='',s_disaster_6='',s_disaster_7='',s_disaster_8='',s_disaster_9='',s_disaster_10='';
