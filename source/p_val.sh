for i in {1..400}; do
  command="sbatch -n 1 -N 1 --mem=50000  -c 1 -p janson,janson_cascade,janson_bigmem -e ./logfiles/log"$i".err -o ./logfiles/log"$i".out --time=70:00:00  wrapper_pval.sh ./R_file_script.R $i"
  $command
done