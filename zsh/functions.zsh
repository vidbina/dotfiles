man () {
  LESS_TERMCAP_mb=$'\e'"[1;31m" \
  LESS_TERMCAP_md=$'\e'"[1;31m" \
  LESS_TERMCAP_me=$'\e'"[0m" \
  LESS_TERMCAP_se=$'\e'"[0m" \
  LESS_TERMCAP_so=$'\e'"[1;44;33m" \
  LESS_TERMCAP_ue=$'\e'"[0m" \
  LESS_TERMCAP_us=$'\e'"[1;32m" \
  command man "$@"
}

grep () {
  command grep --color=always "$@"
}

egrep () {
  command egrep --color=always "$@"
}

tree () {
  command tree -C "$@"
}

dict () {
  command dict "$@" | colorit
}

# Run a command with secrets pulled from 1Password secret references.
#
# Reads KEY=op://... lines from an env file (default ./.env.1password) and
# injects resolved values into the child process only — nothing touches disk
# and nothing leaks into the parent shell.
#
# OP_ACCOUNT must pin the account: a bare vault name like "Employee" exists in
# every 1Password account, so an unscoped lookup resolves against whichever
# happens to be unlocked. Set it in the project's .envrc.
#
#   openv -- python app.py
#   openv -f prod.env -- terraform plan
#   openv --no-masking -- printenv TYPESAFE_API_KEY
openv () {
  local env_file=".env.1password"
  local -a op_args

  while [ $# -gt 0 ]; do
    case "$1" in
      -f|--env-file) env_file="$2"; shift 2 ;;
      --) shift; break ;;
      *) op_args+=("$1"); shift ;;
    esac
  done

  if [ ! -f "$env_file" ]; then
    print -u2 "openv: no such env file: $env_file"
    return 66
  fi

  if [ $# -eq 0 ]; then
    print -u2 "openv: no command given (openv [-f FILE] [op flags] -- CMD ...)"
    return 64
  fi

  if [ -z "$OP_ACCOUNT" ]; then
    print -u2 "openv: OP_ACCOUNT unset — refs resolve against any unlocked account"
  fi

  command op run --env-file="$env_file" "${op_args[@]}" -- "$@"
}
