#!/usr/local/bin/bash

function readrss() {

   declare -a rss
   OIFS="$IFS"
   IFS=$'\n'
   set -f   # cf. help set
   rss=( $(/usr/local/bin/curl -L -s --max-time 10 "$@" | /usr/local/bin/xml sel -t -m '//item' -v 'title' -o ' ' -n -v  'link' -n) )
   set +f
   IFS="$OIFS"

   declare n=0

   for ((i=0; i < "${#rss[@]}"; i++)); do 
      if [[ $(($i%2)) -ne 0 ]]; then
         #printf "      %s\n" "${rss[${i}]}"
	 echo "" >/dev/null
      else
         let n++
         printf "%s\n" "${rss[${i}]}"
      fi
   done

   return 0
}
readrss $1
