timedcat
========

Output STDIN to a file after every N milliseconds of no output.

Feed in lines on standard input and `timedcat` will output the input consumed so far to the provided file, STDOUT, or a temporary file.

Examples:
  ```
  > tail /var/log/syslog | grep 'NetworkManager' | timedcat -s 3000 -o syslog_snapshot.log &
  > ls syslog_snapshot.log | entr echo "$(wc -l syslog_snapshot.log) lines of output in the last 3 seconds."
  ```

  ```
  while true; do
    echo "$(tail /var/log/syslog | grep 'NetworkManager' | timedcat -s 3000 -o - | wc -l) lines of syslog output in the last 3 seconds.";
  done
  ```
