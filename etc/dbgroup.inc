db_group {
 type "PostgreSQL";
 port "";
 name "orca_4.9_haori";
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
