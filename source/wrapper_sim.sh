# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# wrapper script for left_fig_simulation.sh and right_fig_simulation.sh
#!/bin/bash

case "$#" in
    [1-3])
        echo "modules already loaded in environment"
        module list
        echo "loading R module"
        module load R
        echo ""
        echo "Final module list"
        module list
        echo "running on"
        hostname
        echo ""
        echo ""
        export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER
        Rscript $1 $2 $3
        ;;
    *)
        echo "Usage: $0 <code_file>"
        echo "       Submits job and"
        echo "       joins stderr and stdout, sends mail notification,"
        echo "       names job for easy identification in qstat"
        exit 1
        ;;
esac
exit 0