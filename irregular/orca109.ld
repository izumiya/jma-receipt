# 1 "orca109.ld.in"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "orca109.ld.in"
name orca109;

arraysize 20;
textsize 400;

bind "" "OpenCOBOL" "ORCGN41";
bind "N41" "OpenCOBOL" "ORCGN41";
bind "N41ERR" "OpenCOBOL" "ORCGN41ERR";
bind "N41ID1" "OpenCOBOL" "ORCGN41ID1";
bind "M98P" "OpenCOBOL" "ORCGM98P";
bind "M99P" "OpenCOBOL" "ORCGM99P";
bind "MUIDP" "OpenCOBOL" "ORCGMUIDP";
bind "MDAS" "OpenCOBOL" "ORCGMDAS";
bind "MINF" "OpenCOBOL" "ORCGMINF";
bind "MID2P" "OpenCOBOL" "ORCGMID2P";

multiplex_group orca109;


data {
 spa orcaspa;
 window {
  N41;
  N41ERR;
  N41ID1;
  M98P;
  M99P;
  MUIDP;
  MDAS;
  MINF;
  MID2P;
 };
};

db {
 tbl_help;
 tbl_syskanri;
 tbl_jobkanri;
 tbl_joblog;
 tbl_adrs;
 tbl_infection;
 tbl_ptinf;
 tbl_byomei;
};

db "shell" {
 shell;
};
#db "system" {
# metadb;
#};
#db "blob_body" {
#         blob;
#};
