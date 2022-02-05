#!/usr/bin/env bash

today=$(date "+%Y-%m-%d")
yesterday=$(date -v-1d "+%Y-%m-%d")
now=$(date "+%Y-%m-%d %H:%M:%S")
log_dir="/tmp"
stale_log="${log_dir}/${yesterday}_brew.log"
current_log="${log_dir}/${today}_brew.log"

cat << EOF >> "${current_log}"
 ===================
|$now|
 ===================
EOF

if [ -f "${stale_log}" ]; then
  rm -v "${stale_log}" >> "${current_log}" 2>&1
fi

/usr/local/bin/brew update >> "${current_log}" 2>&1
/usr/local/bin/brew upgrade >> "${current_log}" 2>&1
/usr/local/bin/brew cleanup >> "${current_log}" 2>&1
