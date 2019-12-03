# Directory changing
# alias ..="cd .." # defined elsewhere
abbr -a ... cd ../..

# Enforce use of gradle wrapper wrapper
# Uses an available wrapper if present; otherwise system gradle
alias gradle="gw"

# Pass for second password store
abbr -a zhlpass env PASSWORD_STORE_DIR=/home/j_dage01/.password-store-zhldigital pass
