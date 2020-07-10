function prompt_char {
	if [ $UID -eq 0 ]; then echo "~"; else echo "➜"; fi
}

PROMPT='%(!.%{$fg_bold[green]%}.%{$fg_bold[green]%}%n@)%m $(prompt_char) %{$fg_bold[cyan]%}%(!.%2~.%2~) '
PROMPT+='$(git_prompt_info)%{$reset_color%} '
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
