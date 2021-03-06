# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# sh script to execute all simulation scripts for the left figure 
# written on 06/09/2022

for j in {1..6}; do
for i in {1..1000}; do
  command="sbatch -n 1 -N 1 --mem=8G -c 1 -p janson,janson_cascade,janson_bigmem -e ./logfiles/log"$j"_"$i".err -o ./logfiles/log"$j"_"$i".out --time=70:00:00 wrapper_sim.sh ./R_file_script.R $i $j"
  $command
done
done