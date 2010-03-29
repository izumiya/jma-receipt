db_group {
 type "PostgreSQL";
 port "";
 name "orca";
 redirect "log";
};
db_group "log" {
 priority 100;
 type "PostgreSQL";
 port "sub-jma-receipt";
 name "orca";
 file "/var/lib/jma-receipt/dbredirector/orca.log";
 redirect_port "localhost";
};
db_group "shell" {
 type "Shell";
 priority 1000;
};
db_group "system" {
 type "System";
};
db_group    "blob_body" {
 type    "NativeBLOB";
};
#db_group "xml" {
# type    "XMLIO";
#};

