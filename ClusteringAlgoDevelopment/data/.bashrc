# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

function submitJob ()
{
	if [ "$1" = "-h" ]
	then
		echo "enter: time cores memory script"
	else
		qsub -l mem=$3gb -l walltime=$1 -l nodes=1:ppn=$2 $4 
	fi
}
