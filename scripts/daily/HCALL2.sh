#!/usr/bin/env sh
#----------------------------------------------------------------------#
#       ����	
#       1:DATA�ե�����̾��
#----------------------------------------------------------------------#

	echo	`date`
	echo	$1

ruby /usr/local/orca/scripts/daily/print_parent2.rb "temp.tmp" "ruby /usr/local/orca/scripts/daily/print-data.rb " $1 

	rm -f $1
	echo	`date`
