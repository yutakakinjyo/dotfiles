# ref http://qiita.com/yu-ichiro/items/6441453321c06484bb22

function loadlib() {
    lib=${1:?"You have to specify a library file"}
    if [ -f "$lib" ];then #ファイルの存在を確認
	. "$lib"
    fi
}

loadlib ./dotfiles/p/zshalias

# history search
bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward

HISTFILE=$HOME/.zsh-history           # 履歴をファイルに保存する
HISTSIZE=100000                       # メモリ内の履歴の数
SAVEHIST=100000                       # 保存される履歴の数
setopt extended_history               # 履歴ファイルに時刻を記録
function history-all { history -E 1 } # 全履歴の一覧を出力する

setopt share_history

## ls colors

# ref http://mkit2009.hatenablog.com/entry/2013/01/28/001213

autoload -U compinit
compinit

export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

alias ls="ls -GF"
alias gls="gls --color"

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'


# gopath
export GOPATH="$HOME/go"

# Added by the Heroku Toolbelt
export PATH="/usr/local/bin:/usr/local/heroku/bin:$PATH"

### git branch

# vcs_infoロード    
autoload -Uz vcs_info    
# PROMPT変数内で変数参照する    
setopt prompt_subst    

# vcsの表示    
zstyle ':vcs_info:*' formats '%F{green}%b%f'    
zstyle ':vcs_info:*' actionformats '%F{green}%b%f(%F{red}%a%f)'    
# プロンプト表示直前にvcs_info呼び出し    
precmd() { vcs_info }    
# プロンプト表示    
PROMPT='%~/%f:[${vcs_info_msg_0_}]: '   

# added by travis gem
[ -f /Users/yutaka/.travis/travis.sh ] && source /Users/yutaka/.travis/travis.sh

