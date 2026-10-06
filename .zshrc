# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time Oh My Zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git macos dotenv)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='nvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch $(uname -m)"

# Set personal aliases, overriding those provided by Oh My Zsh libs,
# plugins, and themes. Aliases can be placed here, though Oh My Zsh
# users are encouraged to define aliases within a top-level file in
# the $ZSH_CUSTOM folder, with .zsh extension. Examples:
# - $ZSH_CUSTOM/aliases.zsh
# - $ZSH_CUSTOM/macos.zsh
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias snowsql=/Applications/SnowSQL.app/Contents/MacOS/snowsql

# Created by `pipx` on 2025-04-16 13:19:49
export PATH="$PATH:/Users/anselm/.local/bin"

aws-login() {
  if [ -z "${1}" ]; then
    echo "Usage: aws-login AWS_PROFILE" 1>&2
    return
  fi

  if ! command -V jq > /dev/null; then
    echo "The jq command is required for this function to work."
    return
  fi

  SSO_START_URL=$(aws configure get "profile.${1}.sso_start_url")

  if [ -z "${SSO_START_URL}" ]; then
    echo "profile (${1}) not configured for sso; Missing sso_start_url" 1>&2
    return
  fi

  ACCESS_TOKEN=$(cat ~/.aws/sso/cache/*.json | jq -r --arg url "${SSO_START_URL}" 'select(.startUrl==$url).accessToken')
  SSO_ACCOUNT_ID=$(aws configure get "profile.${1}.sso_account_id")
  SSO_ROLE_NAME=$(aws configure get "profile.${1}.sso_role_name")
  CREDS=$(aws sso get-role-credentials --account-id "${SSO_ACCOUNT_ID}" --role-name "${SSO_ROLE_NAME}" --access-token "${ACCESS_TOKEN}" --profile "${1}" 2>/dev/null)

  if [ "$?" != "0" ]; then
    aws sso login --profile "${1}"
    ACCESS_TOKEN=$(cat ~/.aws/sso/cache/*.json | jq -r --arg url "${SSO_START_URL}" 'select(.startUrl==$url).accessToken')
    CREDS=$(aws sso get-role-credentials --account-id "${SSO_ACCOUNT_ID}" --role-name "${SSO_ROLE_NAME}" --access-token "${ACCESS_TOKEN}" --profile "${1}")
  fi

  export AWS_ACCESS_KEY_ID=$(jq -r '.roleCredentials.accessKeyId' <<< "${CREDS}")
  export AWS_SECRET_ACCESS_KEY=$(jq -r '.roleCredentials.secretAccessKey' <<< "${CREDS}")
  export AWS_SESSION_TOKEN=$(jq -r '.roleCredentials.sessionToken' <<< "${CREDS}")
  export AWS_PROFILE="${1}"
}


# pnpm
export PNPM_HOME="/Users/anselm/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"
eval "$(rbenv init -)"

# bun completions
[ -s "/Users/anselm/.bun/_bun" ] && source "/Users/anselm/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

. "$HOME/.local/bin/env"

# direnv: per-project envs (.envrc); pairs with Emacs envrc package
eval "$(direnv hook zsh)"
