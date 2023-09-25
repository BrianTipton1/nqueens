#!/bin/bash

# Initialize output file
output_file="average_times.txt"
echo "Average Times for nqueens.cabal" > $output_file
echo "===============================" >> $output_file

# List of queen problems to solve
n_values=(4 7 8 9)

# Run the command for each n_value
for n in "${n_values[@]}"
do
  total_time=0

  # Run the command 5 times
  for i in {1..5}
  do
    # Show progress
    echo "Running for n=$n, iteration $i..."

    # Run the command and capture the real time elapsed
    time_output=$( (time cabal run nqueens.cabal -- -d -s=$n) 2>&1 1>/dev/null )
    real_time=$(echo "$time_output" | grep 'real' | awk '{print $2}' | sed -e 's/s//')

    # Convert the real_time to milliseconds
    if [[ $real_time == *m* ]]; then
      minutes=$(echo $real_time | awk -F'm' '{print $1}')
      seconds=$(echo $real_time | awk -F'm' '{print $2}')
      real_time=$(echo "($minutes * 60 + $seconds) * 1000" | bc)
    else
      real_time=$(echo "$real_time * 1000" | bc)
    fi

    # Add real_time to total_time
    total_time=$(echo "$total_time + $real_time" | bc)
  done

  # Calculate average time
  average_time=$(echo "scale=2; $total_time / 5" | bc)

  # Output average time to file
  echo "Average time for n = $n: ${average_time}ms" >> $output_file
done
