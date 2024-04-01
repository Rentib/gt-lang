#!/bin/sh

make 2> /dev/null

[ $? -ne 0 ] && echo "Compilation failed" && exit 1

INTERPRETER="./interpreter"
GOOD_EXAMPLES_DIR="./lang/examples/good/"
BAD_EXAMPLES_DIR="./lang/examples/bad/"

OK_CNT=0
WA_CNT=0

printf "\e[32mRunning good examples\e[0m\n"
for file in $GOOD_EXAMPLES_DIR*.gt; do
    printf "%s: " "$(basename $file)"
    $INTERPRETER $file > /dev/null 2>&1
    [ $? -eq 0 ] && { printf "\033[32mOK\033[0m\n"; OK_CNT=$((OK_CNT+1)); } \
                 || { printf "\033[31mWA\033[0m\n"; WA_CNT=$((WA_CNT+1)); }
done

echo -e "\e[31mRunning bad examples\e[0m\n"
for file in $BAD_EXAMPLES_DIR*.gt; do
    printf "%s: " "$(basename $file)"
    $INTERPRETER $file > /dev/null 2>&1
    [ $? -ne 0 ] && { printf "\033[32mOK\033[0m\n"; OK_CNT=$((OK_CNT+1)); } \
                 || { printf "\033[31mWA\033[0m\n"; WA_CNT=$((WA_CNT+1)); }
done

printf "\e[32mOK: $OK_CNT\e[0m\n"
printf "\e[31mWA: $WA_CNT\e[0m\n"
